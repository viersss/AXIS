library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(nortest)
library(car)
library(lmtest)
library(moments)
library(knitr)
library(rmarkdown)
library(officer)
library(flextable)
library(forecast)
library(broom)
library(leaflet)

# Helper functions
safe_notification <- function(message, type = "info") {
  tryCatch({
    if (type == "success") {
      showNotification(message, type = "success", duration = 4)
    } else if (type == "error") {
      showNotification(message, type = "error", duration = 6)
    } else if (type == "warning") {
      showNotification(message, type = "warning", duration = 5)
    } else {
      showNotification(message, type = "default", duration = 3)
    }
  }, error = function(e) {
    # Fallback if notification fails
    cat(paste("[", toupper(type), "]:", message, "\n"))
  })
}

format_pvalue <- function(p_value) {
  if (p_value < 0.001) {
    return("< 0.001")
  } else if (p_value < 0.01) {
    return(sprintf("%.3f", p_value))
  } else {
    return(sprintf("%.3f", p_value))
  }
}


# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    processed_data = NULL,
    distance_data = NULL,
    data_loaded = FALSE,
    transformation_done = FALSE,
    categorization_done = FALSE,
    normality_done = FALSE,
    homogeneity_done = FALSE,
    mean_test_done = FALSE,
    prop_test_done = FALSE,
    var_test_done = FALSE,
    anova_done = FALSE,
    regression_done = FALSE,
    map_generated = FALSE,
    spatial_analysis_done = FALSE,
    regression_model = NULL,
    current_test_result = NULL,
    current_cohens_d = NULL,
    current_n = NULL,
    prop_test_result = NULL,
    var_test_result = NULL,
    anova_result = NULL,
    posthoc_result = NULL,
    effect_size_result = NULL,
    spatial_weights = NULL,
    morans_i_result = NULL
  )
  
  # Data loading
  observeEvent(input$load_data, {
    tryCatch({
      temp_data <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")
      distance_data <- read.csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")
      
      values$data <- temp_data
      values$processed_data <- values$data
      values$distance_data <- distance_data
      values$data_loaded <- TRUE
      
      # Update variable choices
      numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
      categorical_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) | is.factor(x))]
      all_vars <- names(values$data)
      
      # Update untuk semua selectInput
      updateSelectInput(session, "transform_variable", choices = numeric_vars)
      updateSelectInput(session, "var_to_categorize", choices = numeric_vars)
      updateSelectInput(session, "explore_variable", choices = all_vars)
      updateSelectInput(session, "normality_variable", choices = numeric_vars)
      updateSelectInput(session, "homogeneity_variable", choices = numeric_vars)
      updateSelectInput(session, "homogeneity_group", choices = categorical_vars)
      updateSelectInput(session, "test_variable", choices = numeric_vars)
      updateSelectInput(session, "group_variable", choices = categorical_vars)
      updateSelectInput(session, "paired_variable", choices = numeric_vars)
      updateSelectInput(session, "prop_variable", choices = categorical_vars)
      updateSelectInput(session, "prop_group_variable", choices = categorical_vars)
      updateSelectInput(session, "var_variable", choices = numeric_vars)
      updateSelectInput(session, "var_group_variable", choices = categorical_vars)
      updateSelectInput(session, "anova_dependent", choices = numeric_vars)
      updateSelectInput(session, "anova_factor1", choices = categorical_vars)
      updateSelectInput(session, "anova_factor2", choices = categorical_vars)
      updateSelectInput(session, "reg_dependent", choices = numeric_vars)
      updateSelectInput(session, "reg_independent", choices = numeric_vars)
      updateSelectInput(session, "map_variable", choices = numeric_vars)
      updateSelectInput(session, "spatial_variable", choices = numeric_vars)
      
      safe_notification("Dataset berhasil dimuat!", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error memuat data:", e$message), "error")
    })
  })
  
  # Data loaded output
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Data summary
  output$data_summary <- renderPrint({
    if (values$data_loaded) {
      cat("Informasi Dataset:\n")
      cat("==================\n")
      cat("Baris:", nrow(values$processed_data), "\n")
      cat("Kolom:", ncol(values$processed_data), "\n")
      cat("Missing Values:", sum(is.na(values$processed_data)), "\n\n")
      
      cat("Tipe Variabel:\n")
      cat("===============\n")
      print(sapply(values$processed_data, class))
    }
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    if (values$data_loaded) {
      DT::datatable(values$processed_data, 
                    options = list(scrollX = TRUE, pageLength = 10),
                    class = 'cell-border stripe')
    }
  })
  
  # =================================================================== #
  # LOGIKA TRANSFORMASI DATA
  # =================================================================== #
  observeEvent(input$apply_transformation, {
    req(values$processed_data, input$transform_variable, input$transform_method)
    
    tryCatch({
      var_name <- input$transform_variable
      method <- input$transform_method
      original_data <- values$processed_data[[var_name]]
      
      if (!is.numeric(original_data)) {
        safe_notification("Transformasi hanya bisa diterapkan pada variabel numerik.", "warning")
        return()
      }
      
      transformed_data <- NULL
      
      if (method == "log") {
        if (any(original_data <= 0, na.rm = TRUE)) {
          safe_notification("Logaritma natural tidak bisa diterapkan pada nilai non-positif (<= 0).", "error")
          return()
        }
        transformed_data <- log(original_data)
      } else if (method == "log10") {
        if (any(original_data <= 0, na.rm = TRUE)) {
          safe_notification("Log10 tidak bisa diterapkan pada nilai non-positif (<= 0).", "error")
          return()
        }
        transformed_data <- log10(original_data)
      } else if (method == "sqrt") {
        if (any(original_data < 0, na.rm = TRUE)) {
          safe_notification("Akar kuadrat tidak bisa diterapkan pada nilai negatif.", "error")
          return()
        }
        transformed_data <- sqrt(original_data)
      } else if (method == "square") {
        transformed_data <- original_data^2
      } else if (method == "boxcox") {
        if (any(original_data <= 0, na.rm = TRUE)) {
          safe_notification("Transformasi Box-Cox memerlukan semua nilai data positif (> 0).", "error")
          return()
        }
        lambda <- BoxCox.lambda(original_data, method = "guerrero")
        transformed_data <- BoxCox(original_data, lambda = lambda)
      }
      
      new_col_name <- paste0(var_name, "_", method)
      values$processed_data[[new_col_name]] <- transformed_data
      values$transformation_done <- TRUE
      
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      all_vars <- names(values$processed_data)
      
      updateSelectInput(session, "transform_variable", choices = numeric_vars)
      updateSelectInput(session, "var_to_categorize", choices = numeric_vars)
      updateSelectInput(session, "explore_variable", choices = all_vars, selected = new_col_name)
      updateSelectInput(session, "normality_variable", choices = numeric_vars, selected = new_col_name)
      updateSelectInput(session, "homogeneity_variable", choices = numeric_vars)
      updateSelectInput(session, "test_variable", choices = numeric_vars)
      updateSelectInput(session, "anova_dependent", choices = numeric_vars)
      updateSelectInput(session, "reg_dependent", choices = numeric_vars)
      updateSelectInput(session, "reg_independent", choices = numeric_vars)
      updateSelectInput(session, "map_variable", choices = numeric_vars)
      updateSelectInput(session, "spatial_variable", choices = numeric_vars)
      
      safe_notification(paste("Variabel", new_col_name, "berhasil ditambahkan!"), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error saat transformasi:", e$message), "error")
    })
  })
  
  output$transformation_done <- reactive({
    values$transformation_done
  })
  outputOptions(output, "transformation_done", suspendWhenHidden = FALSE)
  
  output$transformation_result <- renderPrint({
    if (values$transformation_done) {
      new_col_name <- paste0(input$transform_variable, "_", input$transform_method)
      if (new_col_name %in% names(values$processed_data)) {
        cat("Ringkasan variabel baru:", new_col_name, "\n")
        cat("======================================\n")
        print(summary(values$processed_data[[new_col_name]]))
      }
    }
  })
  
  observeEvent(input$apply_categorization, {
    req(values$processed_data, input$var_to_categorize)
    
    tryCatch({
      var_name <- input$var_to_categorize
      var_data <- values$processed_data[[var_name]]
      
      if (input$categorize_method == "quantile") {
        breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        labels <- c("Low", "Medium-Low", "Medium-High", "High")
      } else if (input$categorize_method == "equal_width") {
        breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = 5)
        labels <- c("Low", "Medium-Low", "Medium-High", "High")
      } else if (input$categorize_method == "custom") {
        custom_breaks <- as.numeric(unlist(strsplit(input$custom_thresholds, ",")))
        breaks <- c(min(var_data, na.rm = TRUE), custom_breaks, max(var_data, na.rm = TRUE))
        labels <- paste0("Cat", 1:(length(breaks)-1))
      }
      
      categorized_var <- cut(var_data, breaks = unique(breaks), labels = labels, include.lowest = TRUE)
      new_col_name <- paste0(var_name, "_cat")
      values$processed_data[[new_col_name]] <- categorized_var
      values$categorization_done <- TRUE
      
      categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) | is.factor(x))]
      all_vars <- names(values$processed_data)
      
      updateSelectInput(session, "explore_variable", choices = all_vars, selected = new_col_name)
      updateSelectInput(session, "homogeneity_group", choices = categorical_vars)
      updateSelectInput(session, "group_variable", choices = categorical_vars)
      updateSelectInput(session, "prop_variable", choices = categorical_vars)
      updateSelectInput(session, "prop_group_variable", choices = categorical_vars)
      updateSelectInput(session, "var_group_variable", choices = categorical_vars)
      updateSelectInput(session, "anova_factor1", choices = categorical_vars)
      updateSelectInput(session, "anova_factor2", choices = categorical_vars)
      
      safe_notification(paste("Variabel", new_col_name, "berhasil ditambahkan!"), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error saat kategorisasi:", e$message), "error")
    })
  })
  
  output$categorization_done <- reactive({
    values$categorization_done
  })
  outputOptions(output, "categorization_done", suspendWhenHidden = FALSE)
  
  output$categorization_result <- renderPrint({
    if (values$categorization_done) {
      var_name <- paste0(input$var_to_categorize, "_cat")
      if (var_name %in% names(values$processed_data)) {
        cat("Hasil Kategorisasi:\n")
        cat("======================\n")
        print(table(values$processed_data[[var_name]], useNA = "ifany"))
      }
    }
  })
  
  observeEvent(input$explore_variable, {
    req(values$data_loaded, input$explore_variable)
    
    var_is_numeric <- is.numeric(values$processed_data[[input$explore_variable]])
    
    if (var_is_numeric) {
      updateSelectInput(session, "chart_type",
                        label = "Tipe Visualisasi (Numerik):",
                        choices = list("Histogram" = "histogram",
                                       "Boxplot" = "boxplot",
                                       "Density Plot" = "density",
                                       "Q-Q Plot" = "qq"))
    } else {
      updateSelectInput(session, "chart_type",
                        label = "Tipe Visualisasi (Kategorik):",
                        choices = list("Bar Chart" = "barchart"))
    }
  }, ignoreNULL = TRUE)
  
  output$exploration_plot <- renderPlotly({
    req(values$data_loaded, input$explore_variable, input$chart_type)
    
    var_data <- values$processed_data[[input$explore_variable]]
    
    if (is.numeric(var_data)) {
      p <- if (input$chart_type == "histogram") {
        ggplot(values$processed_data, aes_string(x = input$explore_variable)) +
          geom_histogram(bins = input$bins, fill = "#3c8dbc", alpha = 0.7) +
          labs(title = paste("Histogram", input$explore_variable))
      } else if (input$chart_type == "boxplot") {
        ggplot(values$processed_data, aes_string(y = input$explore_variable)) +
          geom_boxplot(fill = "#3c8dbc", alpha = 0.7) +
          labs(title = paste("Boxplot", input$explore_variable))
      } else if (input$chart_type == "density") {
        ggplot(values$processed_data, aes_string(x = input$explore_variable)) +
          geom_density(fill = "#3c8dbc", alpha = 0.7) +
          labs(title = paste("Density Plot", input$explore_variable))
      } else if (input$chart_type == "qq") {
        ggplot(values$processed_data, aes_string(sample = input$explore_variable)) +
          stat_qq() + stat_qq_line() +
          labs(title = paste("Q-Q Plot", input$explore_variable))
      }
      ggplotly(p)
    } else {
      df <- as.data.frame(table(var_data))
      colnames(df) <- c("Category", "Frequency")
      p <- ggplot(df, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Bar Chart", input$explore_variable), x = input$explore_variable) +
        theme(legend.position = "none")
      ggplotly(p)
    }
  })
  
  output$descriptive_stats <- renderPrint({
    req(values$data_loaded, input$explore_variable)
    
    var_data <- values$processed_data[[input$explore_variable]]
    
    cat("Statistik Deskriptif:\n")
    cat("======================\n")
    
    if(is.numeric(var_data)){
      cat("Mean:", round(mean(var_data, na.rm = TRUE), 4), "\n")
      cat("Median:", round(median(var_data, na.rm = TRUE), 4), "\n")
      cat("Standard Deviation:", round(sd(var_data, na.rm = TRUE), 4), "\n")
      cat("Variance:", round(var(var_data, na.rm = TRUE), 4), "\n")
      cat("Minimum:", round(min(var_data, na.rm = TRUE), 4), "\n")
      cat("Maximum:", round(max(var_data, na.rm = TRUE), 4), "\n")
      cat("Range:", round(max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE), 4), "\n")
      cat("IQR:", round(IQR(var_data, na.rm = TRUE), 4), "\n")
      cat("Skewness:", round(moments::skewness(var_data, na.rm = TRUE), 4), "\n")
      cat("Kurtosis:", round(moments::kurtosis(var_data, na.rm = TRUE), 4), "\n")
      cat("Missing Values:", sum(is.na(var_data)), "\n")
    } else {
      print(table(var_data, useNA = "ifany"))
    }
  })
  
  output$stats_interpretation <- renderPrint({
    req(values$data_loaded, input$explore_variable)
    
    var_data <- values$processed_data[[input$explore_variable]]
    
    if(!is.numeric(var_data)) {
      cat("INTERPRETASI STATISTIK KATEGORIK:\n")
      cat("=====================================\n")
      tbl <- table(var_data, useNA = "ifany")
      total <- sum(tbl)
      cat("Total observasi:", total, "\n")
      cat("Jumlah kategori:", length(tbl), "\n")
      
      # Kategori dominan
      dominant_cat <- names(which.max(tbl))
      dominant_pct <- round(max(tbl)/total * 100, 2)
      cat("Kategori dominan:", dominant_cat, "(", dominant_pct, "%)\n")
      
      # Distribusi keseimbangan
      if (max(tbl)/min(tbl) > 3) {
        cat("Distribusi: TIDAK SEIMBANG - ada kategori yang sangat dominan\n")
      } else {
        cat("Distribusi: RELATIF SEIMBANG antar kategori\n")
      }
      return()
    }
    
    mean_val <- mean(var_data, na.rm = TRUE)
    median_val <- median(var_data, na.rm = TRUE)
    sd_val <- sd(var_data, na.rm = TRUE)
    skew_val <- moments::skewness(var_data, na.rm = TRUE)
    kurt_val <- moments::kurtosis(var_data, na.rm = TRUE)
    cv <- sd_val / abs(mean_val) * 100
    
    cat("INTERPRETASI STATISTIK KOMPREHENSIF:\n")
    cat("=====================================\n\n")
    
    # Analisis Tendensi Sentral
    cat("1. ANALISIS TENDENSI SENTRAL:\n")
    cat("   Mean vs Median Analysis:\n")
    if (abs(mean_val - median_val) / sd_val < 0.1) {
      cat("   - Distribusi SIMETRIS (mean ≈ median)\n")
      cat("   - Data terpusat dengan baik di sekitar nilai tengah\n")
    } else if (mean_val > median_val) {
      cat("   - Distribusi CONDONG KANAN (positive skew)\n")
      cat("   - Terdapat nilai-nilai ekstrem tinggi yang menarik mean ke atas\n")
    } else {
      cat("   - Distribusi CONDONG KIRI (negative skew)\n")
      cat("   - Terdapat nilai-nilai ekstrem rendah yang menarik mean ke bawah\n")
    }
    
    # Analisis Variabilitas
    cat("\n2. ANALISIS VARIABILITAS:\n")
    cat("   Coefficient of Variation (CV):", round(cv, 2), "%\n")
    if (cv < 15) {
      cat("   - Variabilitas RENDAH: Data homogen dan konsisten\n")
    } else if (cv < 30) {
      cat("   - Variabilitas SEDANG: Data cukup bervariasi\n")
    } else {
      cat("   - Variabilitas TINGGI: Data sangat heterogen\n")
    }
    
    # Analisis Bentuk Distribusi
    cat("\n3. ANALISIS BENTUK DISTRIBUSI:\n")
    cat("   Skewness:", round(skew_val, 3), "\n")
    if (abs(skew_val) < 0.5) {
      cat("   - Distribusi HAMPIR SIMETRIS\n")
    } else if (abs(skew_val) < 1) {
      cat("   - Distribusi MODERATELY SKEWED\n")
    } else {
      cat("   - Distribusi HIGHLY SKEWED\n")
    }
    
    cat("   Kurtosis:", round(kurt_val, 3), "\n")
    if (kurt_val < 3) {
      cat("   - Distribusi PLATYKURTIC (lebih datar dari normal)\n")
    } else if (kurt_val > 3) {
      cat("   - Distribusi LEPTOKURTIC (lebih runcing dari normal)\n")
    } else {
      cat("   - Distribusi MESOKURTIC (mirip distribusi normal)\n")
    }
    
    # Deteksi Outlier
    Q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower_fence <- Q1 - 1.5 * IQR_val
    upper_fence <- Q3 + 1.5 * IQR_val
    outliers <- sum(var_data < lower_fence | var_data > upper_fence, na.rm = TRUE)
    
    cat("\n4. DETEKSI OUTLIER:\n")
    cat("   Jumlah outlier potensial:", outliers, "\n")
    if (outliers > 0) {
      cat("   - PERHATIAN: Terdapat", outliers, "nilai ekstrem\n")
      cat("   - Pertimbangkan investigasi lebih lanjut atau transformasi data\n")
    } else {
      cat("   - Tidak ada outlier yang terdeteksi\n")
    }
    
    # Rekomendasi Analisis
    cat("\n5. REKOMENDASI ANALISIS LANJUTAN:\n")
    if (abs(skew_val) > 1) {
      cat("   - Pertimbangkan transformasi data (log, sqrt, Box-Cox)\n")
    }
    if (cv > 30) {
      cat("   - Gunakan median dan IQR sebagai ukuran yang lebih robust\n")
    }
    if (outliers > nrow(values$processed_data) * 0.05) {
      cat("   - Investigasi outlier sebelum analisis inferensial\n")
    }
  })
  
  # =================================================================== #
  # UJI ASUMSI
  # =================================================================== #
  
  observeEvent(input$run_normality_test, {
    req(values$data_loaded, input$normality_variable)
    values$normality_done <- TRUE
  })
  
  output$normality_done <- reactive({
    values$normality_done
  })
  outputOptions(output, "normality_done", suspendWhenHidden = FALSE)
  
  output$normality_result <- renderPrint({
    if (values$normality_done) {
      var_data <- values$processed_data[[input$normality_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      cat("HASIL UJI NORMALITAS - ANALISIS KOMPREHENSIF\n")
      cat("=============================================\n\n")
      
      # Informasi Uji
      cat("Variabel yang diuji:", input$normality_variable, "\n")
      cat("Jumlah observasi:", length(var_data), "\n")
      cat("Metode uji:", switch(input$normality_test,
                                "shapiro" = "Shapiro-Wilk Test",
                                "ks" = "Kolmogorov-Smirnov Test", 
                                "ad" = "Anderson-Darling Test"), "\n\n")
      
      # Hipotesis
      cat("HIPOTESIS:\n")
      cat("H₀: Data mengikuti distribusi normal\n")
      cat("H₁: Data tidak mengikuti distribusi normal\n")
      cat("α = 0.05\n\n")
      
      # Hasil Uji
      if (input$normality_test == "shapiro") {
        if (length(var_data) > 5000) {
          cat("PERINGATAN: Shapiro-Wilk test tidak direkomendasikan untuk n > 5000\n")
          cat("Gunakan Kolmogorov-Smirnov atau Anderson-Darling test\n\n")
        }
        test_result <- shapiro.test(var_data)
      } else if (input$normality_test == "ks") {
        test_result <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
      } else {
        test_result <- ad.test(var_data)
      }
      
      cat("HASIL UJI STATISTIK:\n")
      print(test_result)
      
      # Interpretasi Statistik
      p_value <- test_result$p.value
      cat("\n\nINTERPRETASI STATISTIK:\n")
      cat("========================\n")
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (p_value > 0.05) {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Pada tingkat signifikansi 5%, tidak ada bukti yang cukup\n")
        cat("           untuk menyatakan bahwa data tidak berdistribusi normal.\n")
        cat("           Data dapat dianggap berdistribusi normal.\n\n")
        cat("IMPLIKASI PRAKTIS:\n")
        cat("- Asumsi normalitas TERPENUHI\n")
        cat("- Dapat menggunakan uji parametrik (t-test, ANOVA, dll.)\n")
        cat("- Interval kepercayaan dan uji hipotesis berbasis distribusi normal valid\n")
      } else {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Pada tingkat signifikansi 5%, terdapat bukti yang cukup\n")
        cat("           untuk menyatakan bahwa data tidak berdistribusi normal.\n\n")
        cat("IMPLIKASI PRAKTIS:\n")
        cat("- Asumsi normalitas TIDAK TERPENUHI\n")
        cat("- Pertimbangkan transformasi data atau uji non-parametrik\n")
        cat("- Gunakan bootstrap atau metode robust untuk inferensi\n")
      }
      
      # Statistik Deskriptif Tambahan
      skew_val <- moments::skewness(var_data, na.rm = TRUE)
      kurt_val <- moments::kurtosis(var_data, na.rm = TRUE)
      
      cat("\nSTATISTIK DESKRIPTIF PENDUKUNG:\n")
      cat("Skewness:", round(skew_val, 4), 
          ifelse(abs(skew_val) < 0.5, " (hampir simetris)", 
                 ifelse(abs(skew_val) < 1, " (moderately skewed)", " (highly skewed)")), "\n")
      cat("Kurtosis:", round(kurt_val, 4),
          ifelse(kurt_val < 3, " (platykurtic)", 
                 ifelse(kurt_val > 3, " (leptokurtic)", " (mesokurtic)")), "\n")
      
      # Rekomendasi
      cat("\nREKOMENDASI:\n")
      if (p_value <= 0.05) {
        cat("1. Coba transformasi data (log, sqrt, Box-Cox)\n")
        cat("2. Gunakan uji non-parametrik sebagai alternatif\n")
        cat("3. Pertimbangkan metode bootstrap untuk inferensi\n")
        if (abs(skew_val) > 1) {
          cat("4. Data sangat skewed - transformasi sangat direkomendasikan\n")
        }
      } else {
        cat("1. Lanjutkan dengan analisis parametrik\n")
        cat("2. Asumsi normalitas dapat diandalkan\n")
      }
    }
  })
  
  observeEvent(input$run_homogeneity_test, {
    req(values$data_loaded, input$homogeneity_variable, input$homogeneity_group)
    values$homogeneity_done <- TRUE
  })
  
  output$homogeneity_done <- reactive({
    values$homogeneity_done
  })
  outputOptions(output, "homogeneity_done", suspendWhenHidden = FALSE)
  
  output$homogeneity_result <- renderPrint({
    if (values$homogeneity_done) {
      formula <- as.formula(paste(input$homogeneity_variable, "~", input$homogeneity_group))
      
      cat("HASIL UJI HOMOGENITAS VARIANS - ANALISIS KOMPREHENSIF\n")
      cat("======================================================\n\n")
      
      # Informasi Uji
      cat("Variabel numerik:", input$homogeneity_variable, "\n")
      cat("Variabel grup:", input$homogeneity_group, "\n")
      cat("Metode uji:", switch(input$homogeneity_test,
                                "levene" = "Levene's Test",
                                "bartlett" = "Bartlett's Test",
                                "fligner" = "Fligner-Killeen Test"), "\n\n")
      
      # Statistik Deskriptif per Grup
      group_stats <- values$processed_data %>%
        group_by(!!sym(input$homogeneity_group)) %>%
        summarise(
          n = n(),
          mean = round(mean(!!sym(input$homogeneity_variable), na.rm = TRUE), 4),
          sd = round(sd(!!sym(input$homogeneity_variable), na.rm = TRUE), 4),
          variance = round(var(!!sym(input$homogeneity_variable), na.rm = TRUE), 4),
          .groups = 'drop'
        )
      
      cat("STATISTIK DESKRIPTIF PER GRUP:\n")
      print(group_stats)
      
      # Rasio Varians
      max_var <- max(group_stats$variance, na.rm = TRUE)
      min_var <- min(group_stats$variance, na.rm = TRUE)
      var_ratio <- max_var / min_var
      
      cat("\nRASIO VARIANS (max/min):", round(var_ratio, 4), "\n")
      if (var_ratio > 4) {
        cat("PERINGATAN: Rasio varians > 4 menunjukkan heterogenitas yang substansial\n")
      }
      
      # Hipotesis
      cat("\nHIPOTESIS:\n")
      cat("H₀: Varians antar grup adalah homogen (σ₁² = σ₂² = ... = σₖ²)\n")
      cat("H₁: Setidaknya ada satu varians grup yang berbeda\n")
      cat("α = 0.05\n\n")
      
      # Hasil Uji
      if (input$homogeneity_test == "levene") {
        test_result <- leveneTest(formula, data = values$processed_data)
        p_value <- test_result$`Pr(>F)`[1]
        f_stat <- test_result$`F value`[1]
        cat("HASIL LEVENE'S TEST:\n")
        print(test_result)
      } else if (input$homogeneity_test == "bartlett") {
        test_result <- bartlett.test(formula, data = values$processed_data)
        p_value <- test_result$p.value
        cat("HASIL BARTLETT'S TEST:\n")
        print(test_result)
      } else {
        test_result <- fligner.test(formula, data = values$processed_data)
        p_value <- test_result$p.value
        cat("HASIL FLIGNER-KILLEEN TEST:\n")
        print(test_result)
      }
      
      # Interpretasi Statistik
      cat("\n\nINTERPRETASI STATISTIK:\n")
      cat("========================\n")
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (p_value > 0.05) {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Pada tingkat signifikansi 5%, tidak ada bukti yang cukup\n")
        cat("           untuk menyatakan bahwa varians antar grup berbeda.\n")
        cat("           Asumsi homogenitas varians dapat diterima.\n\n")
        cat("IMPLIKASI PRAKTIS:\n")
        cat("- Asumsi homogenitas varians TERPENUHI\n")
        cat("- Dapat menggunakan ANOVA klasik dan pooled variance t-test\n")
        cat("- Uji parametrik standar dapat diandalkan\n")
      } else {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Pada tingkat signifikansi 5%, terdapat bukti yang cukup\n")
        cat("           untuk menyatakan bahwa varians antar grup tidak homogen.\n\n")
        cat("IMPLIKASI PRAKTIS:\n")
        cat("- Asumsi homogenitas varians TIDAK TERPENUHI\n")
        cat("- Gunakan Welch's t-test atau Welch's ANOVA\n")
        cat("- Pertimbangkan transformasi data atau uji non-parametrik\n")
      }
      
      # Analisis Tambahan
      cat("\nANALISIS TAMBAHAN:\n")
      cat("Coefficient of Variation per grup:\n")
      group_stats$cv <- round((group_stats$sd / group_stats$mean) * 100, 2)
      for(i in 1:nrow(group_stats)) {
        cat("  ", group_stats[[input$homogeneity_group]][i], ": ", group_stats$cv[i], "%\n")
      }
      
      # Rekomendasi
      cat("\nREKOMENDASI:\n")
      if (p_value <= 0.05) {
        cat("1. Gunakan Welch's correction untuk t-test dan ANOVA\n")
        cat("2. Pertimbangkan transformasi data (log, sqrt)\n")
        cat("3. Gunakan uji non-parametrik sebagai alternatif\n")
        if (var_ratio > 10) {
          cat("4. Varians sangat heterogen - transformasi sangat direkomendasikan\n")
        }
      } else {
        cat("1. Lanjutkan dengan uji parametrik standar\n")
        cat("2. Asumsi homogenitas dapat diandalkan\n")
      }
    }
  })
  
  output$assumptions_interpretation <- renderPrint({
    if (values$normality_done || values$homogeneity_done) {
      cat("RINGKASAN EVALUASI ASUMSI STATISTIK\n")
      cat("====================================\n\n")
      
      if (values$normality_done && values$homogeneity_done) {
        cat("Kedua uji asumsi telah dilakukan. Gunakan hasil ini untuk menentukan\n")
        cat("metode analisis yang tepat:\n\n")
        cat("- Jika KEDUA asumsi terpenuhi: Gunakan uji parametrik standar\n")
        cat("- Jika hanya NORMALITAS terpenuhi: Gunakan Welch's correction\n")
        cat("- Jika hanya HOMOGENITAS terpenuhi: Pertimbangkan transformasi\n")
        cat("- Jika KEDUA asumsi dilanggar: Gunakan uji non-parametrik\n")
      } else if (values$normality_done) {
        cat("Uji normalitas telah dilakukan. Lanjutkan dengan uji homogenitas\n")
        cat("untuk evaluasi asumsi yang lengkap.\n")
      } else if (values$homogeneity_done) {
        cat("Uji homogenitas telah dilakukan. Lanjutkan dengan uji normalitas\n")
        cat("untuk evaluasi asumsi yang lengkap.\n")
      }
    }
  })
  
  # =================================================================== #
  # UJI BEDA RATA-RATA
  # =================================================================== #
  
  observeEvent(input$run_mean_test, {
    req(values$data_loaded, input$test_variable)
    values$mean_test_done <- TRUE
  })
  
  output$mean_test_done <- reactive({
    values$mean_test_done
  })
  outputOptions(output, "mean_test_done", suspendWhenHidden = FALSE)
  
  output$mean_test_result <- renderPrint({
    if (values$mean_test_done) {
      var_data <- values$processed_data[[input$test_variable]]
      
      cat("HASIL UJI BEDA RATA-RATA - ANALISIS KOMPREHENSIF\n")
      cat("================================================\n\n")
      
      # Informasi Uji
      cat("Tipe uji:", switch(input$test_type,
                              "one_sample" = "One Sample t-test",
                              "two_sample" = "Two Sample t-test",
                              "paired" = "Paired t-test"), "\n")
      cat("Variabel:", input$test_variable, "\n")
      cat("Alternative hypothesis:", input$alternative, "\n")
      cat("Confidence level:", input$confidence_level * 100, "%\n\n")
      
      # Hipotesis berdasarkan tipe uji
      if (input$test_type == "one_sample") {
        cat("HIPOTESIS:\n")
        if (input$alternative == "two.sided") {
          cat("H₀: μ = ", input$test_value, "\n")
          cat("H₁: μ ≠ ", input$test_value, "\n")
        } else if (input$alternative == "greater") {
          cat("H₀: μ ≤ ", input$test_value, "\n")
          cat("H₁: μ > ", input$test_value, "\n")
        } else {
          cat("H₀: μ ≥ ", input$test_value, "\n")
          cat("H₁: μ < ", input$test_value, "\n")
        }
        
        test_result <- t.test(var_data, mu = input$test_value, 
                              alternative = input$alternative, 
                              conf.level = input$confidence_level)
        
        # Effect size (Cohen's d)
        cohens_d <- (mean(var_data, na.rm = TRUE) - input$test_value) / sd(var_data, na.rm = TRUE)
        values$current_cohens_d <- cohens_d
        
      } else if (input$test_type == "two_sample") {
        cat("HIPOTESIS:\n")
        if (input$alternative == "two.sided") {
          cat("H₀: μ₁ = μ₂\n")
          cat("H₁: μ₁ ≠ μ₂\n")
        } else if (input$alternative == "greater") {
          cat("H₀: μ₁ ≤ μ₂\n")
          cat("H₁: μ₁ > μ₂\n")
        } else {
          cat("H₀: μ₁ ≥ μ₂\n")
          cat("H₁: μ₁ < μ₂\n")
        }
        
        formula <- as.formula(paste(input$test_variable, "~", input$group_variable))
        test_result <- t.test(formula, data = values$processed_data, 
                              var.equal = input$equal_var,
                              alternative = input$alternative, 
                              conf.level = input$confidence_level)
        
        # Effect size untuk two sample
        group_data <- split(var_data, values$processed_data[[input$group_variable]])
        if (length(group_data) == 2) {
          pooled_sd <- sqrt(((length(group_data[[1]])-1)*var(group_data[[1]], na.rm=TRUE) + 
                               (length(group_data[[2]])-1)*var(group_data[[2]], na.rm=TRUE)) / 
                              (length(group_data[[1]]) + length(group_data[[2]]) - 2))
          cohens_d <- (mean(group_data[[1]], na.rm=TRUE) - mean(group_data[[2]], na.rm=TRUE)) / pooled_sd
          values$current_cohens_d <- cohens_d
        }
        
      } else { # Paired
        cat("HIPOTESIS:\n")
        if (input$alternative == "two.sided") {
          cat("H₀: μd = 0 (tidak ada perbedaan)\n")
          cat("H₁: μd ≠ 0 (ada perbedaan)\n")
        } else if (input$alternative == "greater") {
          cat("H₀: μd ≤ 0\n")
          cat("H₁: μd > 0\n")
        } else {
          cat("H₀: μd ≥ 0\n")
          cat("H₁: μd < 0\n")
        }
        
        var2_data <- values$processed_data[[input$paired_variable]]
        test_result <- t.test(var_data, var2_data, paired = TRUE, 
                              alternative = input$alternative, 
                              conf.level = input$confidence_level)
        
        # Effect size untuk paired
        differences <- var_data - var2_data
        cohens_d <- mean(differences, na.rm = TRUE) / sd(differences, na.rm = TRUE)
        values$current_cohens_d <- cohens_d
      }
      
      values$current_test_result <- test_result
      
      cat("\nHASIL UJI STATISTIK:\n")
      print(test_result)
      
      # Interpretasi Statistik
      p_value <- test_result$p.value
      cat("\n\nINTERPRETASI STATISTIK:\n")
      cat("========================\n")
      cat("P-value:", format_pvalue(p_value), "\n")
      cat("t-statistic:", round(test_result$statistic, 4), "\n")
      cat("Degrees of freedom:", test_result$parameter, "\n")
      
      if (p_value <= 0.05) {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Pada tingkat signifikansi 5%, terdapat bukti yang cukup\n")
        cat("           untuk menyatakan bahwa terdapat perbedaan yang signifikan.\n")
      } else {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Pada tingkat signifikansi 5%, tidak ada bukti yang cukup\n")
        cat("           untuk menyatakan bahwa terdapat perbedaan yang signifikan.\n")
      }
      
      # Confidence Interval
      cat("\nCONFIDENCE INTERVAL:\n")
      ci <- test_result$conf.int
      cat("95% CI: [", round(ci[1], 4), ", ", round(ci[2], 4), "]\n")
      
      # Effect Size
      cat("\nEFFECT SIZE (Cohen's d):\n")
      cat("Cohen's d:", round(values$current_cohens_d, 4), "\n")
      if (abs(values$current_cohens_d) < 0.2) {
        cat("Interpretasi: SMALL effect size\n")
      } else if (abs(values$current_cohens_d) < 0.5) {
        cat("Interpretasi: SMALL effect size\n")
      } else if (abs(values$current_cohens_d) < 0.8) {
        cat("Interpretasi: MEDIUM effect size\n")
      } else {
        cat("Interpretasi: LARGE effect size\n")
      }
      
      # Practical Significance
      cat("\nSIGNIFIKANSI PRAKTIS vs STATISTIK:\n")
      if (p_value <= 0.05 && abs(values$current_cohens_d) >= 0.5) {
        cat("- Signifikan secara STATISTIK dan PRAKTIS\n")
        cat("- Perbedaan bermakna dalam konteks praktis\n")
      } else if (p_value <= 0.05 && abs(values$current_cohens_d) < 0.5) {
        cat("- Signifikan secara STATISTIK tetapi effect size kecil\n")
        cat("- Pertimbangkan relevansi praktis dari perbedaan ini\n")
      } else if (p_value > 0.05 && abs(values$current_cohens_d) >= 0.5) {
        cat("- Tidak signifikan secara STATISTIK tetapi effect size besar\n")
        cat("- Mungkin perlu sampel yang lebih besar\n")
      } else {
        cat("- Tidak signifikan secara STATISTIK dan effect size kecil\n")
      }
    }
  })
  
  output$mean_test_plot <- renderPlotly({
    req(values$mean_test_done, values$current_test_result)
    
    if (input$test_type == "one_sample") {
      # Histogram dengan garis referensi
      p <- ggplot(values$processed_data, aes_string(x = input$test_variable)) +
        geom_histogram(bins = 30, fill = "#3c8dbc", alpha = 0.7) +
        geom_vline(xintercept = input$test_value, color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = mean(values$processed_data[[input$test_variable]], na.rm = TRUE), 
                   color = "blue", linetype = "solid", size = 1) +
        labs(title = "Distribution with Test Value",
             subtitle = "Red line: Test value, Blue line: Sample mean") +
        theme_minimal()
      
    } else if (input$test_type == "two_sample") {
      # Boxplot perbandingan grup
      p <- ggplot(values$processed_data, aes_string(x = input$group_variable, y = input$test_variable, 
                                                    fill = input$group_variable)) +
        geom_boxplot(alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
        labs(title = "Comparison Between Groups") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else { # Paired
      # Scatter plot dengan garis diagonal
      df_paired <- data.frame(
        var1 = values$processed_data[[input$test_variable]],
        var2 = values$processed_data[[input$paired_variable]]
      )
      p <- ggplot(df_paired, aes(x = var1, y = var2)) +
        geom_point(alpha = 0.6, color = "#3c8dbc") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Paired Data Comparison",
             x = input$test_variable,
             y = input$paired_variable,
             subtitle = "Red line: y = x (no difference)") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  output$mean_test_interpretation <- renderPrint({
    if (values$mean_test_done) {
      cat("INTERPRETASI KOMPREHENSIF HASIL UJI\n")
      cat("===================================\n\n")
      
      p_value <- values$current_test_result$p.value
      cohens_d <- values$current_cohens_d
      
      cat("RINGKASAN TEMUAN:\n")
      if (p_value <= 0.001) {
        cat("- Perbedaan SANGAT SIGNIFIKAN (p < 0.001)\n")
      } else if (p_value <= 0.01) {
        cat("- Perbedaan SIGNIFIKAN (p < 0.01)\n")
      } else if (p_value <= 0.05) {
        cat("- Perbedaan SIGNIFIKAN (p < 0.05)\n")
      } else if (p_value <= 0.1) {
        cat("- Perbedaan MARGINALLY SIGNIFICANT (p < 0.1)\n")
      } else {
        cat("- Perbedaan TIDAK SIGNIFIKAN (p > 0.05)\n")
      }
      
      cat("- Effect size:", ifelse(abs(cohens_d) < 0.2, "NEGLIGIBLE",
                                   ifelse(abs(cohens_d) < 0.5, "SMALL",
                                          ifelse(abs(cohens_d) < 0.8, "MEDIUM", "LARGE"))), "\n\n")
      
      cat("REKOMENDASI TINDAK LANJUT:\n")
      if (p_value <= 0.05) {
        cat("1. Hasil dapat dipublikasikan sebagai temuan signifikan\n")
        cat("2. Lakukan replikasi untuk konfirmasi\n")
        if (abs(cohens_d) >= 0.5) {
          cat("3. Perbedaan memiliki relevansi praktis yang tinggi\n")
        }
      } else {
        cat("1. Tidak ada bukti perbedaan yang signifikan\n")
        cat("2. Pertimbangkan power analysis untuk ukuran sampel\n")
        cat("3. Evaluasi kemungkinan Type II error\n")
      }
    }
  })
  
  # =================================================================== #
  # UJI PROPORSI & VARIANCE - IMPLEMENTASI LENGKAP
  # =================================================================== #
  
  observeEvent(input$run_prop_test, {
    req(values$data_loaded, input$prop_variable)
    
    tryCatch({
      if (input$prop_test_type == "one_prop") {
        # One sample proportion test
        var_data <- values$processed_data[[input$prop_variable]]
        
        # Hitung proporsi sukses (asumsi kategori pertama adalah "sukses")
        tbl <- table(var_data)
        success_count <- tbl[1]
        total_count <- sum(tbl)
        
        test_result <- prop.test(success_count, total_count, 
                                 p = input$prop_test_value,
                                 alternative = input$prop_alternative)
        
        values$prop_test_result <- list(
          test = test_result,
          type = "one_prop",
          success_count = success_count,
          total_count = total_count,
          observed_prop = success_count / total_count,
          test_prop = input$prop_test_value
        )
        
      } else {
        # Two sample proportion test
        formula <- as.formula(paste(input$prop_variable, "~", input$prop_group_variable))
        
        # Buat tabel kontingensi
        cont_table <- table(values$processed_data[[input$prop_variable]], 
                            values$processed_data[[input$prop_group_variable]])
        
        # Ambil baris pertama sebagai "sukses"
        success_counts <- cont_table[1, ]
        total_counts <- colSums(cont_table)
        
        test_result <- prop.test(success_counts, total_counts,
                                 alternative = input$prop_alternative)
        
        values$prop_test_result <- list(
          test = test_result,
          type = "two_prop",
          success_counts = success_counts,
          total_counts = total_counts,
          observed_props = success_counts / total_counts,
          cont_table = cont_table
        )
      }
      
      values$prop_test_done <- TRUE
      
    }, error = function(e) {
      safe_notification(paste("Error dalam uji proporsi:", e$message), "error")
    })
  })
  
  output$prop_test_done <- reactive({
    values$prop_test_done
  })
  outputOptions(output, "prop_test_done", suspendWhenHidden = FALSE)
  
  output$prop_test_result <- renderPrint({
    if (values$prop_test_done && !is.null(values$prop_test_result)) {
      result <- values$prop_test_result
      
      cat("HASIL UJI PROPORSI - ANALISIS KOMPREHENSIF\n")
      cat("==========================================\n\n")
      
      if (result$type == "one_prop") {
        cat("TIPE UJI: One Sample Proportion Test\n")
        cat("Variabel:", input$prop_variable, "\n")
        cat("Proporsi uji (p₀):", result$test_prop, "\n")
        cat("Alternative hypothesis:", input$prop_alternative, "\n\n")
        
        cat("DATA OBSERVASI:\n")
        cat("Jumlah sukses:", result$success_count, "\n")
        cat("Total observasi:", result$total_count, "\n")
        cat("Proporsi observasi:", round(result$observed_prop, 4), "\n\n")
        
        cat("HIPOTESIS:\n")
        if (input$prop_alternative == "two.sided") {
          cat("H₀: p = ", result$test_prop, "\n")
          cat("H₁: p ≠ ", result$test_prop, "\n")
        } else if (input$prop_alternative == "greater") {
          cat("H₀: p ≤ ", result$test_prop, "\n")
          cat("H₁: p > ", result$test_prop, "\n")
        } else {
          cat("H₀: p ≥ ", result$test_prop, "\n")
          cat("H₁: p < ", result$test_prop, "\n")
        }
        
      } else {
        cat("TIPE UJI: Two Sample Proportion Test\n")
        cat("Variabel proporsi:", input$prop_variable, "\n")
        cat("Variabel grup:", input$prop_group_variable, "\n")
        cat("Alternative hypothesis:", input$prop_alternative, "\n\n")
        
        cat("DATA OBSERVASI:\n")
        cat("Tabel Kontingensi:\n")
        print(result$cont_table)
        cat("\nProporsi per grup:\n")
        for (i in 1:length(result$observed_props)) {
          cat("Grup", names(result$observed_props)[i], ":", 
              round(result$observed_props[i], 4), "\n")
        }
        
        cat("\nHIPOTESIS:\n")
        if (input$prop_alternative == "two.sided") {
          cat("H₀: p₁ = p₂\n")
          cat("H₁: p₁ ≠ p₂\n")
        } else if (input$prop_alternative == "greater") {
          cat("H₀: p₁ ≤ p₂\n")
          cat("H₁: p₁ > p₂\n")
        } else {
          cat("H₀: p₁ ≥ p₂\n")
          cat("H₁: p₁ < p₂\n")
        }
      }
      
      cat("\nHASIL UJI STATISTIK:\n")
      print(result$test)
      
      # Interpretasi
      p_value <- result$test$p.value
      cat("\n\nINTERPRETASI STATISTIK:\n")
      cat("========================\n")
      cat("P-value:", format_pvalue(p_value), "\n")
      cat("Chi-squared statistic:", round(result$test$statistic, 4), "\n")
      
      if (p_value <= 0.05) {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Terdapat perbedaan proporsi yang signifikan.\n")
      } else {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Tidak ada bukti perbedaan proporsi yang signifikan.\n")
      }
      
      # Confidence Interval
      if (!is.null(result$test$conf.int)) {
        cat("\nCONFIDENCE INTERVAL:\n")
        ci <- result$test$conf.int
        cat("95% CI: [", round(ci[1], 4), ", ", round(ci[2], 4), "]\n")
      }
    }
  })
  
  observeEvent(input$run_var_test, {
    req(values$data_loaded, input$var_variable)
    
    tryCatch({
      var_data <- values$processed_data[[input$var_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$var_test_type == "one_var") {
        # One sample variance test (Chi-square test)
        n <- length(var_data)
        sample_var <- var(var_data)
        test_var <- input$var_test_value
        
        chi_sq_stat <- (n - 1) * sample_var / test_var
        
        if (input$var_alternative == "two.sided") {
          p_value <- 2 * min(pchisq(chi_sq_stat, n-1), 1 - pchisq(chi_sq_stat, n-1))
        } else if (input$var_alternative == "greater") {
          p_value <- 1 - pchisq(chi_sq_stat, n-1)
        } else {
          p_value <- pchisq(chi_sq_stat, n-1)
        }
        
        values$var_test_result <- list(
          type = "one_var",
          statistic = chi_sq_stat,
          p_value = p_value,
          df = n-1,
          sample_var = sample_var,
          test_var = test_var,
          n = n
        )
        
      } else {
        # Two sample variance test (F-test)
        formula <- as.formula(paste(input$var_variable, "~", input$var_group_variable))
        test_result <- var.test(formula, data = values$processed_data,
                                alternative = input$var_alternative)
        
        values$var_test_result <- list(
          type = "two_var",
          test = test_result
        )
      }
      
      values$var_test_done <- TRUE
      
    }, error = function(e) {
      safe_notification(paste("Error dalam uji varians:", e$message), "error")
    })
  })
  
  output$var_test_done <- reactive({
    values$var_test_done
  })
  outputOptions(output, "var_test_done", suspendWhenHidden = FALSE)
  
  output$var_test_result <- renderPrint({
    if (values$var_test_done && !is.null(values$var_test_result)) {
      result <- values$var_test_result
      
      cat("HASIL UJI VARIANS - ANALISIS KOMPREHENSIF\n")
      cat("=========================================\n\n")
      
      if (result$type == "one_var") {
        cat("TIPE UJI: One Sample Variance Test\n")
        cat("Variabel:", input$var_variable, "\n")
        cat("Varians uji (σ²₀):", result$test_var, "\n")
        cat("Alternative hypothesis:", input$var_alternative, "\n\n")
        
        cat("DATA OBSERVASI:\n")
        cat("Ukuran sampel (n):", result$n, "\n")
        cat("Varians sampel (s²):", round(result$sample_var, 4), "\n")
        cat("Derajat bebas:", result$df, "\n\n")
        
        cat("HIPOTESIS:\n")
        if (input$var_alternative == "two.sided") {
          cat("H₀: σ² = ", result$test_var, "\n")
          cat("H₁: σ² ≠ ", result$test_var, "\n")
        } else if (input$var_alternative == "greater") {
          cat("H₀: σ² ≤ ", result$test_var, "\n")
          cat("H₁: σ² > ", result$test_var, "\n")
        } else {
          cat("H₀: σ² ≥ ", result$test_var, "\n")
          cat("H₁: σ² < ", result$test_var, "\n")
        }
        
        cat("\nHASIL UJI STATISTIK:\n")
        cat("Chi-square statistic:", round(result$statistic, 4), "\n")
        cat("Degrees of freedom:", result$df, "\n")
        cat("P-value:", format_pvalue(result$p_value), "\n")
        
      } else {
        cat("TIPE UJI: Two Sample Variance Test (F-test)\n")
        cat("Variabel:", input$var_variable, "\n")
        cat("Variabel grup:", input$var_group_variable, "\n")
        cat("Alternative hypothesis:", input$var_alternative, "\n\n")
        
        cat("HIPOTESIS:\n")
        if (input$var_alternative == "two.sided") {
          cat("H₀: σ₁² = σ₂²\n")
          cat("H₁: σ₁² ≠ σ₂²\n")
        } else if (input$var_alternative == "greater") {
          cat("H₀: σ₁² ≤ σ₂²\n")
          cat("H₁: σ₁² > σ₂²\n")
        } else {
          cat("H₀: σ₁² ≥ σ₂²\n")
          cat("H₁: σ₁² < σ₂²\n")
        }
        
        cat("\nHASIL UJI STATISTIK:\n")
        print(result$test)
      }
      
      # Interpretasi
      p_value <- ifelse(result$type == "one_var", result$p_value, result$test$p.value)
      
      cat("\n\nINTERPRETASI STATISTIK:\n")
      cat("========================\n")
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (p_value <= 0.05) {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Terdapat perbedaan varians yang signifikan.\n")
      } else {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Tidak ada bukti perbedaan varians yang signifikan.\n")
      }
    }
  })
  
  output$prop_test_plot <- renderPlotly({
    if (values$prop_test_done && !is.null(values$prop_test_result)) {
      result <- values$prop_test_result
      
      if (result$type == "one_prop") {
        # Bar chart untuk one sample proportion
        df <- data.frame(
          Category = c("Success", "Failure"),
          Count = c(result$success_count, result$total_count - result$success_count),
          Proportion = c(result$observed_prop, 1 - result$observed_prop)
        )
        
        p <- ggplot(df, aes(x = Category, y = Count, fill = Category)) +
          geom_bar(stat = "identity", alpha = 0.7) +
          geom_text(aes(label = paste0(round(Proportion*100, 1), "%")), 
                    vjust = -0.5) +
          labs(title = "Observed Proportions",
               y = "Count") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else {
        # Grouped bar chart untuk two sample proportion
        df <- data.frame(
          Group = rep(names(result$observed_props), each = 2),
          Category = rep(c("Success", "Failure"), length(result$observed_props)),
          Count = as.vector(rbind(result$success_counts, 
                                  result$total_counts - result$success_counts))
        )
        
        p <- ggplot(df, aes(x = Group, y = Count, fill = Category)) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
          labs(title = "Proportion Comparison Between Groups") +
          theme_minimal()
      }
      
      ggplotly(p)
    }
  })
  
  output$var_test_plot <- renderPlotly({
    if (values$var_test_done && !is.null(values$var_test_result)) {
      result <- values$var_test_result
      
      if (result$type == "one_var") {
        # Histogram dengan garis varians
        var_data <- values$processed_data[[input$var_variable]]
        p <- ggplot(values$processed_data, aes_string(x = input$var_variable)) +
          geom_histogram(bins = 30, fill = "#3c8dbc", alpha = 0.7) +
          labs(title = paste("Distribution (Sample Variance =", round(result$sample_var, 4), ")"),
               subtitle = paste("Test Variance =", result$test_var)) +
          theme_minimal()
        
      } else {
        # Boxplot untuk two sample variance
        p <- ggplot(values$processed_data, aes_string(x = input$var_group_variable, 
                                                      y = input$var_variable,
                                                      fill = input$var_group_variable)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = "Variance Comparison Between Groups") +
          theme_minimal() +
          theme(legend.position = "none")
      }
      
      ggplotly(p)
    }
  })
  
  output$prop_var_interpretation <- renderPrint({
    if (values$prop_test_done || values$var_test_done) {
      cat("RINGKASAN HASIL UJI PROPORSI & VARIANS\n")
      cat("======================================\n\n")
      
      if (values$prop_test_done && !is.null(values$prop_test_result)) {
        result <- values$prop_test_result
        p_value <- ifelse(result$type == "one_prop", result$test$p.value, result$test$p.value)
        
        cat("UJI PROPORSI:\n")
        if (p_value <= 0.05) {
          cat("- Terdapat perbedaan proporsi yang SIGNIFIKAN\n")
        } else {
          cat("- Tidak ada perbedaan proporsi yang signifikan\n")
        }
      }
      
      if (values$var_test_done && !is.null(values$var_test_result)) {
        result <- values$var_test_result
        p_value <- ifelse(result$type == "one_var", result$p_value, result$test$p.value)
        
        cat("UJI VARIANS:\n")
        if (p_value <= 0.05) {
          cat("- Terdapat perbedaan varians yang SIGNIFIKAN\n")
        } else {
          cat("- Tidak ada perbedaan varians yang signifikan\n")
        }
      }
    }
  })
  
  # =================================================================== #
  # ANOVA - IMPLEMENTASI LENGKAP
  # =================================================================== #
  
  observeEvent(input$run_anova, {
    req(values$data_loaded, input$anova_dependent, input$anova_factor1)
    
    tryCatch({
      if (input$anova_type == "one_way") {
        formula <- as.formula(paste(input$anova_dependent, "~", input$anova_factor1))
        anova_model <- aov(formula, data = values$processed_data)
        anova_summary <- summary(anova_model)
        
        # Post-hoc test (Tukey HSD)
        posthoc <- TukeyHSD(anova_model)
        
        # Effect size (eta squared)
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        ss_between <- anova_summary[[1]]$`Sum Sq`[1]
        eta_squared <- ss_between / ss_total
        
        values$anova_result <- list(
          model = anova_model,
          summary = anova_summary,
          type = "one_way",
          eta_squared = eta_squared
        )
        
        values$posthoc_result <- posthoc
        values$effect_size_result <- eta_squared
        
      } else {
        # Two-way ANOVA
        if (input$anova_interaction) {
          formula <- as.formula(paste(input$anova_dependent, "~", 
                                      input$anova_factor1, "*", input$anova_factor2))
        } else {
          formula <- as.formula(paste(input$anova_dependent, "~", 
                                      input$anova_factor1, "+", input$anova_factor2))
        }
        
        anova_model <- aov(formula, data = values$processed_data)
        anova_summary <- summary(anova_model)
        
        # Effect sizes
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        eta_squared_factor1 <- anova_summary[[1]]$`Sum Sq`[1] / ss_total
        eta_squared_factor2 <- anova_summary[[1]]$`Sum Sq`[2] / ss_total
        
        if (input$anova_interaction && nrow(anova_summary[[1]]) > 3) {
          eta_squared_interaction <- anova_summary[[1]]$`Sum Sq`[3] / ss_total
        } else {
          eta_squared_interaction <- NULL
        }
        
        values$anova_result <- list(
          model = anova_model,
          summary = anova_summary,
          type = "two_way",
          eta_squared_factor1 = eta_squared_factor1,
          eta_squared_factor2 = eta_squared_factor2,
          eta_squared_interaction = eta_squared_interaction
        )
        
        # Post-hoc untuk two-way lebih kompleks
        values$posthoc_result <- "Post-hoc analysis untuk two-way ANOVA memerlukan analisis terpisah per faktor"
        values$effect_size_result <- list(
          factor1 = eta_squared_factor1,
          factor2 = eta_squared_factor2,
          interaction = eta_squared_interaction
        )
      }
      
      values$anova_done <- TRUE
      
    }, error = function(e) {
      safe_notification(paste("Error dalam ANOVA:", e$message), "error")
    })
  })
  
  output$anova_done <- reactive({
    values$anova_done
  })
  outputOptions(output, "anova_done", suspendWhenHidden = FALSE)
  
  output$anova_result <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      cat("HASIL ANOVA - ANALISIS KOMPREHENSIF\n")
      cat("===================================\n\n")
      
      if (result$type == "one_way") {
        cat("TIPE ANALISIS: One-Way ANOVA\n")
        cat("Variabel dependen:", input$anova_dependent, "\n")
        cat("Faktor:", input$anova_factor1, "\n")
        cat("Alpha level:", input$anova_alpha, "\n\n")
        
        cat("HIPOTESIS:\n")
        cat("H₀: μ₁ = μ₂ = μ₃ = ... = μₖ (semua rata-rata grup sama)\n")
        cat("H₁: Setidaknya ada satu rata-rata grup yang berbeda\n\n")
        
      } else {
        cat("TIPE ANALISIS: Two-Way ANOVA\n")
        cat("Variabel dependen:", input$anova_dependent, "\n")
        cat("Faktor 1:", input$anova_factor1, "\n")
        cat("Faktor 2:", input$anova_factor2, "\n")
        cat("Interaksi:", ifelse(input$anova_interaction, "Ya", "Tidak"), "\n")
        cat("Alpha level:", input$anova_alpha, "\n\n")
      }
      
      cat("HASIL ANOVA TABLE:\n")
      print(result$summary)
      
      # Interpretasi hasil
      cat("\n\nINTERPRETASI STATISTIK:\n")
      cat("========================\n")
      
      anova_table <- result$summary[[1]]
      
      if (result$type == "one_way") {
        f_stat <- anova_table$`F value`[1]
        p_value <- anova_table$`Pr(>F)`[1]
        
        cat("F-statistic:", round(f_stat, 4), "\n")
        cat("P-value:", format_pvalue(p_value), "\n")
        
        if (p_value <= input$anova_alpha) {
          cat("KEPUTUSAN: Tolak H₀\n")
          cat("KESIMPULAN: Terdapat perbedaan rata-rata yang signifikan antar grup.\n")
        } else {
          cat("KEPUTUSAN: Gagal menolak H₀\n")
          cat("KESIMPULAN: Tidak ada bukti perbedaan rata-rata yang signifikan antar grup.\n")
        }
        
      } else {
        # Two-way ANOVA interpretation
        for (i in 1:(nrow(anova_table)-1)) {
          effect_name <- rownames(anova_table)[i]
          f_stat <- anova_table$`F value`[i]
          p_value <- anova_table$`Pr(>F)`[i]
          
          cat("\nEFEK", effect_name, ":\n")
          cat("F-statistic:", round(f_stat, 4), "\n")
          cat("P-value:", format_pvalue(p_value), "\n")
          
          if (p_value <= input$anova_alpha) {
            cat("SIGNIFIKAN - Ada efek dari", effect_name, "\n")
          } else {
            cat("TIDAK SIGNIFIKAN - Tidak ada efek dari", effect_name, "\n")
          }
        }
      }
      
      # Statistik deskriptif per grup
      cat("\n\nSTATISTIK DESKRIPTIF PER GRUP:\n")
      if (result$type == "one_way") {
        group_stats <- values$processed_data %>%
          group_by(!!sym(input$anova_factor1)) %>%
          summarise(
            n = n(),
            mean = round(mean(!!sym(input$anova_dependent), na.rm = TRUE), 4),
            sd = round(sd(!!sym(input$anova_dependent), na.rm = TRUE), 4),
            .groups = 'drop'
          )
        print(group_stats)
      } else {
        group_stats <- values$processed_data %>%
          group_by(!!sym(input$anova_factor1), !!sym(input$anova_factor2)) %>%
          summarise(
            n = n(),
            mean = round(mean(!!sym(input$anova_dependent), na.rm = TRUE), 4),
            sd = round(sd(!!sym(input$anova_dependent), na.rm = TRUE), 4),
            .groups = 'drop'
          )
        print(group_stats)
      }
    }
  })
  
  output$posthoc_result <- renderPrint({
    if (values$anova_done && !is.null(values$posthoc_result)) {
      cat("POST-HOC ANALYSIS\n")
      cat("=================\n\n")
      
      if (is.character(values$posthoc_result)) {
        cat(values$posthoc_result)
      } else {
        cat("TUKEY HSD POST-HOC TEST:\n")
        cat("(Hanya dilakukan jika ANOVA signifikan)\n\n")
        print(values$posthoc_result)
        
        cat("\n\nINTERPRETASI POST-HOC:\n")
        cat("Pasangan grup dengan p adj < 0.05 menunjukkan perbedaan yang signifikan.\n")
      }
    }
  })
  
  output$effect_size_result <- renderPrint({
    if (values$anova_done && !is.null(values$effect_size_result)) {
      cat("EFFECT SIZE ANALYSIS\n")
      cat("====================\n\n")
      
      if (is.numeric(values$effect_size_result)) {
        # One-way ANOVA
        eta_sq <- values$effect_size_result
        cat("Eta Squared (η²):", round(eta_sq, 4), "\n")
        cat("Interpretasi:", interpret_effect_size(eta_sq), "effect size\n\n")
        
        cat("PANDUAN INTERPRETASI ETA SQUARED:\n")
        cat("η² < 0.01: Negligible effect\n")
        cat("η² 0.01-0.06: Small effect\n")
        cat("η² 0.06-0.14: Medium effect\n")
        cat("η² > 0.14: Large effect\n")
        
      } else {
        # Two-way ANOVA
        cat("EFFECT SIZES PER FAKTOR:\n")
        cat("Faktor 1 (", input$anova_factor1, "): η² =", 
            round(values$effect_size_result$factor1, 4), 
            "(", interpret_effect_size(values$effect_size_result$factor1), ")\n")
        cat("Faktor 2 (", input$anova_factor2, "): η² =", 
            round(values$effect_size_result$factor2, 4),
            "(", interpret_effect_size(values$effect_size_result$factor2), ")\n")
        
        if (!is.null(values$effect_size_result$interaction)) {
          cat("Interaksi: η² =", 
              round(values$effect_size_result$interaction, 4),
              "(", interpret_effect_size(values$effect_size_result$interaction), ")\n")
        }
      }
    }
  })
  
  output$anova_plot <- renderPlotly({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      if (result$type == "one_way") {
        # Boxplot untuk one-way ANOVA
        p <- ggplot(values$processed_data, aes_string(x = input$anova_factor1, 
                                                      y = input$anova_dependent,
                                                      fill = input$anova_factor1)) +
          geom_boxplot(alpha = 0.7) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
          labs(title = "One-Way ANOVA: Group Comparison",
               x = input$anova_factor1,
               y = input$anova_dependent) +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else {
        # Interaction plot untuk two-way ANOVA
        group_means <- values$processed_data %>%
          group_by(!!sym(input$anova_factor1), !!sym(input$anova_factor2)) %>%
          summarise(mean_val = mean(!!sym(input$anova_dependent), na.rm = TRUE),
                    .groups = 'drop')
        
        p <- ggplot(group_means, aes_string(x = input$anova_factor1, 
                                            y = "mean_val",
                                            color = input$anova_factor2,
                                            group = input$anova_factor2)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          labs(title = "Two-Way ANOVA: Interaction Plot",
               x = input$anova_factor1,
               y = paste("Mean", input$anova_dependent),
               color = input$anova_factor2) +
          theme_minimal()
      }
      
      ggplotly(p)
    }
  })
  
  output$anova_interpretation <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      cat("INTERPRETASI KOMPREHENSIF HASIL ANOVA\n")
      cat("====================================\n\n")
      
      result <- values$anova_result
      anova_table <- result$summary[[1]]
      
      if (result$type == "one_way") {
        p_value <- anova_table$`Pr(>F)`[1]
        eta_sq <- result$eta_squared
        
        cat("RINGKASAN TEMUAN:\n")
        if (p_value <= 0.001) {
          cat("- Perbedaan antar grup SANGAT SIGNIFIKAN (p < 0.001)\n")
        } else if (p_value <= 0.01) {
          cat("- Perbedaan antar grup SIGNIFIKAN (p < 0.01)\n")
        } else if (p_value <= 0.05) {
          cat("- Perbedaan antar grup SIGNIFIKAN (p < 0.05)\n")
        } else {
          cat("- Perbedaan antar grup TIDAK SIGNIFIKAN (p > 0.05)\n")
        }
        
        cat("- Effect size:", interpret_effect_size(eta_sq), "\n")
        cat("- Proporsi varians dijelaskan:", round(eta_sq * 100, 2), "%\n\n")
        
        cat("REKOMENDASI:\n")
        if (p_value <= 0.05) {
          cat("1. Lakukan post-hoc test untuk identifikasi grup yang berbeda\n")
          cat("2. Periksa asumsi ANOVA (normalitas, homogenitas)\n")
          if (eta_sq >= 0.14) {
            cat("3. Effect size besar - temuan memiliki relevansi praktis tinggi\n")
          }
        } else {
          cat("1. Tidak ada bukti perbedaan antar grup\n")
          cat("2. Pertimbangkan power analysis\n")
          cat("3. Evaluasi kemungkinan Type II error\n")
        }
        
      } else {
        cat("RINGKASAN TEMUAN TWO-WAY ANOVA:\n")
        
        for (i in 1:(nrow(anova_table)-1)) {
          effect_name <- rownames(anova_table)[i]
          p_value <- anova_table$`Pr(>F)`[i]
          
          cat("\n", effect_name, ":\n")
          if (p_value <= 0.05) {
            cat("- SIGNIFIKAN: Ada efek dari", effect_name, "\n")
          } else {
            cat("- TIDAK SIGNIFIKAN: Tidak ada efek dari", effect_name, "\n")
          }
        }
        
        cat("\nREKOMENDASI:\n")
        cat("1. Interpretasi efek utama harus mempertimbangkan interaksi\n")
        cat("2. Jika interaksi signifikan, fokus pada simple effects\n")
        cat("3. Gunakan plot interaksi untuk interpretasi visual\n")
      }
    }
  })
  
  # =================================================================== #
  # REGRESI LINEAR BERGANDA - IMPLEMENTASI LENGKAP
  # =================================================================== #
  
  observeEvent(input$run_regression, {
    req(values$data_loaded, input$reg_dependent, input$reg_independent)
    
    tryCatch({
      if (input$reg_intercept) {
        formula <- as.formula(paste(input$reg_dependent, "~", 
                                    paste(input$reg_independent, collapse = "+")))
      } else {
        formula <- as.formula(paste(input$reg_dependent, "~ -1 +", 
                                    paste(input$reg_independent, collapse = "+")))
      }
      
      lm_model <- lm(formula, data = values$processed_data)
      values$regression_model <- lm_model
      values$regression_done <- TRUE
      
    }, error = function(e) {
      safe_notification(paste("Error dalam regresi:", e$message), "error")
    })
  })
  
  output$regression_done <- reactive({
    values$regression_done
  })
  outputOptions(output, "regression_done", suspendWhenHidden = FALSE)
  
  output$regression_result <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("HASIL REGRESI LINEAR BERGANDA - ANALISIS KOMPREHENSIF\n")
      cat("=====================================================\n\n")
      
      cat("MODEL SPECIFICATION:\n")
      cat("Variabel dependen:", input$reg_dependent, "\n")
      cat("Variabel independen:", paste(input$reg_independent, collapse = ", "), "\n")
      cat("Intercept:", ifelse(input$reg_intercept, "Included", "Excluded"), "\n\n")
      
      cat("HASIL REGRESI:\n")
      print(summary(model))
      
      # Model fit statistics
      r_squared <- summary(model)$r.squared
      adj_r_squared <- summary(model)$adj.r.squared
      f_stat <- summary(model)$fstatistic
      
      cat("\n\nINTERPRETASI MODEL FIT:\n")
      cat("=======================\n")
      cat("R-squared:", round(r_squared, 4), 
          "(", round(r_squared * 100, 2), "% varians dijelaskan)\n")
      cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")
      cat("F-statistic:", round(f_stat[1], 4), 
          "pada", f_stat[2], "dan", f_stat[3], "df\n")
      
      # Interpretasi R-squared
      if (r_squared >= 0.7) {
        cat("Model fit: EXCELLENT (R² ≥ 0.7)\n")
      } else if (r_squared >= 0.5) {
        cat("Model fit: GOOD (R² ≥ 0.5)\n")
      } else if (r_squared >= 0.3) {
        cat("Model fit: MODERATE (R² ≥ 0.3)\n")
      } else {
        cat("Model fit: POOR (R² < 0.3)\n")
      }
      
      # Interpretasi koefisien
      cat("\nINTERPRETASI KOEFISIEN:\n")
      coeffs <- summary(model)$coefficients
      
      for (i in 1:nrow(coeffs)) {
        var_name <- rownames(coeffs)[i]
        coeff <- coeffs[i, 1]
        p_value <- coeffs[i, 4]
        
        cat("\n", var_name, ":\n")
        cat("  Koefisien:", round(coeff, 4), "\n")
        cat("  P-value:", format_pvalue(p_value), "\n")
        
        if (p_value <= 0.05) {
          cat("  Status: SIGNIFIKAN\n")
          if (var_name != "(Intercept)") {
            cat("  Interpretasi: Setiap peningkatan 1 unit", var_name, 
                "akan mengubah", input$reg_dependent, "sebesar", round(coeff, 4), "unit\n")
          }
        } else {
          cat("  Status: TIDAK SIGNIFIKAN\n")
        }
      }
    }
  })
  
  output$model_fit_stats <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("STATISTIK KESESUAIAN MODEL\n")
      cat("==========================\n\n")
      
      # Basic fit statistics
      summary_stats <- summary(model)
      cat("R-squared:", round(summary_stats$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(summary_stats$adj.r.squared, 4), "\n")
      cat("Residual Standard Error:", round(summary_stats$sigma, 4), "\n")
      cat("F-statistic:", round(summary_stats$fstatistic[1], 4), "\n")
      cat("P-value (F-test):", format_pvalue(pf(summary_stats$fstatistic[1], 
                                                summary_stats$fstatistic[2], 
                                                summary_stats$fstatistic[3], 
                                                lower.tail = FALSE)), "\n\n")
      
      # Additional statistics
      cat("STATISTIK TAMBAHAN:\n")
      cat("AIC:", round(AIC(model), 4), "\n")
      cat("BIC:", round(BIC(model), 4), "\n")
      cat("Log-likelihood:", round(logLik(model), 4), "\n")
      cat("Degrees of freedom:", model$df.residual, "\n")
      cat("Observations:", nobs(model), "\n")
    }
  })
  
  output$coefficients_table <- DT::renderDataTable({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      # Create coefficients table
      coeffs <- summary(model)$coefficients
      coeffs_df <- data.frame(
        Variable = rownames(coeffs),
        Estimate = round(coeffs[, 1], 4),
        Std_Error = round(coeffs[, 2], 4),
        t_value = round(coeffs[, 3], 4),
        p_value = round(coeffs[, 4], 4),
        Significance = ifelse(coeffs[, 4] <= 0.001, "***",
                              ifelse(coeffs[, 4] <= 0.01, "**",
                                     ifelse(coeffs[, 4] <= 0.05, "*",
                                            ifelse(coeffs[, 4] <= 0.1, ".", ""))))
      )
      
      DT::datatable(coeffs_df, 
                    options = list(pageLength = 10, scrollX = TRUE),
                    class = 'cell-border stripe') %>%
        DT::formatStyle('p_value',
                        backgroundColor = DT::styleInterval(c(0.05), c('lightcoral', 'lightgreen')))
    }
  })
  
  # Diagnostic plots
  output$residual_plot <- renderPlotly({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      df_diag <- data.frame(
        fitted = fitted(model),
        residuals = residuals(model)
      )
      
      p <- ggplot(df_diag, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = "#3c8dbc") +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_smooth(se = FALSE, color = "blue") +
        labs(title = "Residuals vs Fitted",
             x = "Fitted Values",
             y = "Residuals") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  output$qq_plot <- renderPlotly({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      df_qq <- data.frame(
        sample = sort(residuals(model)),
        theoretical = qnorm(ppoints(length(residuals(model))))
      )
      
      p <- ggplot(df_qq, aes(x = theoretical, y = sample)) +
        geom_point(alpha = 0.6, color = "#3c8dbc") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Normal Q-Q Plot",
             x = "Theoretical Quantiles",
             y = "Sample Quantiles") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  output$scale_location_plot <- renderPlotly({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      df_scale <- data.frame(
        fitted = fitted(model),
        sqrt_abs_resid = sqrt(abs(residuals(model)))
      )
      
      p <- ggplot(df_scale, aes(x = fitted, y = sqrt_abs_resid)) +
        geom_point(alpha = 0.6, color = "#3c8dbc") +
        geom_smooth(se = FALSE, color = "blue") +
        labs(title = "Scale-Location Plot",
             x = "Fitted Values",
             y = "√|Residuals|") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  output$leverage_plot <- renderPlotly({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      df_lev <- data.frame(
        leverage = hatvalues(model),
        std_residuals = rstandard(model),
        obs = 1:length(fitted(model))
      )
      
      p <- ggplot(df_lev, aes(x = leverage, y = std_residuals)) +
        geom_point(alpha = 0.6, color = "#3c8dbc") +
        geom_hline(yintercept = c(-2, 2), color = "red", linetype = "dashed") +
        labs(title = "Residuals vs Leverage",
             x = "Leverage",
             y = "Standardized Residuals") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Assumption tests
  output$regression_normality <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      residuals_data <- residuals(model)
      
      cat("UJI NORMALITAS RESIDUAL\n")
      cat("=======================\n\n")
      
      # Shapiro-Wilk test
      if (length(residuals_data) <= 5000) {
        shapiro_test <- shapiro.test(residuals_data)
        cat("SHAPIRO-WILK TEST:\n")
        print(shapiro_test)
        
        cat("\nInterpretasi:\n")
        if (shapiro_test$p.value > 0.05) {
          cat("Residual berdistribusi NORMAL (asumsi terpenuhi)\n")
        } else {
          cat("Residual TIDAK berdistribusi normal (asumsi dilanggar)\n")
        }
      } else {
        cat("Sampel terlalu besar untuk Shapiro-Wilk test\n")
        cat("Gunakan Q-Q plot untuk evaluasi visual\n")
      }
      
      # Jarque-Bera test
      jb_stat <- (length(residuals_data)/6) * (moments::skewness(residuals_data)^2 + 
                                                 (moments::kurtosis(residuals_data) - 3)^2/4)
      jb_p <- 1 - pchisq(jb_stat, 2)
      
      cat("\nJARQUE-BERA TEST:\n")
      cat("Statistic:", round(jb_stat, 4), "\n")
      cat("P-value:", format_pvalue(jb_p), "\n")
      
      if (jb_p > 0.05) {
        cat("Residual berdistribusi NORMAL (asumsi terpenuhi)\n")
      } else {
        cat("Residual TIDAK berdistribusi normal (asumsi dilanggar)\n")
      }
    }
  })
  
  output$regression_homoscedasticity <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI HOMOSKEDASTISITAS\n")
      cat("=====================\n\n")
      
      # Breusch-Pagan test
      bp_test <- bptest(model)
      cat("BREUSCH-PAGAN TEST:\n")
      print(bp_test)
      
      cat("\nInterpretasi:\n")
      if (bp_test$p.value > 0.05) {
        cat("Varians residual HOMOGEN (asumsi terpenuhi)\n")
      } else {
        cat("Varians residual HETEROGEN (asumsi dilanggar)\n")
        cat("Pertimbangkan transformasi atau robust standard errors\n")
      }
      
      # White test (jika tersedia)
      tryCatch({
        white_test <- bptest(model, ~ fitted(model) + I(fitted(model)^2))
        cat("\nWHITE TEST:\n")
        print(white_test)
      }, error = function(e) {
        cat("\nWhite test tidak dapat dilakukan\n")
      })
    }
  })
  
  output$regression_vif <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI MULTIKOLINEARITAS (VIF)\n")
      cat("===========================\n\n")
      
      if (length(input$reg_independent) > 1) {
        vif_values <- vif(model)
        
        cat("VARIANCE INFLATION FACTORS:\n")
        for (i in 1:length(vif_values)) {
          cat(names(vif_values)[i], ":", round(vif_values[i], 4), "\n")
        }
        
        cat("\nINTERPRETASI:\n")
        cat("VIF < 5: Tidak ada masalah multikolinearitas\n")
        cat("VIF 5-10: Multikolinearitas moderat\n")
        cat("VIF > 10: Multikolinearitas serius\n\n")
        
        max_vif <- max(vif_values)
        if (max_vif < 5) {
          cat("STATUS: Tidak ada masalah multikolinearitas\n")
        } else if (max_vif < 10) {
          cat("STATUS: Multikolinearitas moderat terdeteksi\n")
        } else {
          cat("STATUS: Multikolinearitas serius terdeteksi\n")
          cat("REKOMENDASI: Pertimbangkan menghapus variabel dengan VIF tinggi\n")
        }
      } else {
        cat("VIF tidak dapat dihitung (hanya satu variabel independen)\n")
      }
    }
  })
  
  output$regression_dw <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI AUTOKORELASI (DURBIN-WATSON)\n")
      cat("================================\n\n")
      
      dw_test <- dwtest(model)
      cat("DURBIN-WATSON TEST:\n")
      print(dw_test)
      
      cat("\nINTERPRETASI:\n")
      dw_stat <- dw_test$statistic
      
      if (dw_stat >= 1.5 && dw_stat <= 2.5) {
        cat("Tidak ada autokorelasi (asumsi terpenuhi)\n")
      } else if (dw_stat < 1.5) {
        cat("Autokorelasi POSITIF terdeteksi (asumsi dilanggar)\n")
      } else {
        cat("Autokorelasi NEGATIF terdeteksi (asumsi dilanggar)\n")
      }
      
      cat("\nPANDUAN:\n")
      cat("DW ≈ 2: Tidak ada autokorelasi\n")
      cat("DW < 2: Autokorelasi positif\n")
      cat("DW > 2: Autokorelasi negatif\n")
    }
  })
  
  output$regression_interpretation <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      summary_stats <- summary(model)
      
      cat("INTERPRETASI KOMPREHENSIF HASIL REGRESI\n")
      cat("======================================\n\n")
      
      # Overall model significance
      f_p_value <- pf(summary_stats$fstatistic[1], 
                      summary_stats$fstatistic[2], 
                      summary_stats$fstatistic[3], 
                      lower.tail = FALSE)
      
      cat("SIGNIFIKANSI MODEL KESELURUHAN:\n")
      if (f_p_value <= 0.05) {
        cat("Model secara keseluruhan SIGNIFIKAN (F-test p < 0.05)\n")
        cat("Setidaknya satu variabel independen berpengaruh signifikan\n")
      } else {
        cat("Model secara keseluruhan TIDAK SIGNIFIKAN (F-test p > 0.05)\n")
        cat("Tidak ada variabel independen yang berpengaruh signifikan\n")
      }
      
      # R-squared interpretation
      r_sq <- summary_stats$r.squared
      cat("\nKEKUATAN PREDIKSI MODEL:\n")
      cat("R-squared:", round(r_sq * 100, 2), "% varians dalam", input$reg_dependent, 
          "dijelaskan oleh model\n")
      
      if (r_sq >= 0.7) {
        cat("Model memiliki kekuatan prediksi SANGAT BAIK\n")
      } else if (r_sq >= 0.5) {
        cat("Model memiliki kekuatan prediksi BAIK\n")
      } else if (r_sq >= 0.3) {
        cat("Model memiliki kekuatan prediksi MODERAT\n")
      } else {
        cat("Model memiliki kekuatan prediksi LEMAH\n")
      }
      
      # Significant predictors
      coeffs <- summary_stats$coefficients
      sig_vars <- rownames(coeffs)[coeffs[, 4] <= 0.05 & rownames(coeffs) != "(Intercept)"]
      
      cat("\nVARIABEL PREDIKTOR SIGNIFIKAN:\n")
      if (length(sig_vars) > 0) {
        for (var in sig_vars) {
          coeff <- coeffs[var, 1]
          cat("- ", var, ": koefisien =", round(coeff, 4), 
              ifelse(coeff > 0, "(pengaruh positif)", "(pengaruh negatif)"), "\n")
        }
      } else {
        cat("Tidak ada variabel prediktor yang signifikan\n")
      }
      
      cat("\nREKOMENDASI:\n")
      if (f_p_value <= 0.05 && r_sq >= 0.3) {
        cat("1. Model dapat digunakan untuk prediksi\n")
        cat("2. Periksa asumsi regresi untuk validitas\n")
        cat("3. Pertimbangkan validasi silang\n")
      } else if (f_p_value <= 0.05 && r_sq < 0.3) {
        cat("1. Model signifikan tetapi kekuatan prediksi lemah\n")
        cat("2. Pertimbangkan menambah variabel prediktor\n")
        cat("3. Evaluasi transformasi variabel\n")
      } else {
        cat("1. Model tidak signifikan - tidak cocok untuk prediksi\n")
        cat("2. Pertimbangkan model alternatif\n")
        cat("3. Evaluasi kembali pemilihan variabel\n")
      }
    }
  })
  
  # =================================================================== #
  # SPATIAL MAPPING - NEW IMPLEMENTATION
  # =================================================================== #
  
  observeEvent(input$generate_map, {
    req(values$data_loaded, input$map_variable)
    
    tryCatch({
      # Generate synthetic coordinates for demonstration
      # In real implementation, you would load actual geographic data
      n_obs <- nrow(values$processed_data)
      
      # Create synthetic lat/lon coordinates
      set.seed(123)
      lat_range <- c(-8, -6)  # Example: Indonesia latitude range
      lon_range <- c(106, 108)  # Example: Indonesia longitude range
      
      map_data <- data.frame(
        lat = runif(n_obs, lat_range[1], lat_range[2]),
        lon = runif(n_obs, lon_range[1], lon_range[2]),
        value = values$processed_data[[input$map_variable]],
        district = values$processed_data$DISTRICTCODE
      )
      
      values$map_data <- map_data
      values$map_generated <- TRUE
      
      safe_notification("Peta berhasil dibuat!", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error membuat peta:", e$message), "error")
    })
  })
  
  output$map_generated <- reactive({
    values$map_generated
  })
  outputOptions(output, "map_generated", suspendWhenHidden = FALSE)
  
  output$spatial_map <- renderLeaflet({
    if (values$map_generated && !is.null(values$map_data)) {
      map_data <- values$map_data
      
      # Create color palette
      if (input$color_scheme %in% c("viridis", "plasma")) {
        pal <- colorNumeric(palette = input$color_scheme, domain = map_data$value)
      } else {
        pal <- colorNumeric(palette = input$color_scheme, domain = map_data$value)
      }
      
      # Create leaflet map
      m <- leaflet(map_data) %>%
        addTiles() %>%
        setView(lng = mean(map_data$lon), lat = mean(map_data$lat), zoom = 10)
      
      if (input$map_type == "point") {
        m <- m %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            color = ~pal(value),
            radius = 8,
            fillOpacity = 0.8,
            popup = ~paste("District:", district, "<br>",
                           input$map_variable, ":", round(value, 2))
          )
      } else if (input$map_type == "heat") {
        # For heat map, we'll use circle markers with varying sizes
        m <- m %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            color = ~pal(value),
            radius = ~sqrt(value) * 2,
            fillOpacity = 0.6,
            popup = ~paste("District:", district, "<br>",
                           input$map_variable, ":", round(value, 2))
          )
      } else {
        # Choropleth-style with circles
        m <- m %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            fillColor = ~pal(value),
            color = "white",
            weight = 1,
            radius = 10,
            fillOpacity = 0.8,
            popup = ~paste("District:", district, "<br>",
                           input$map_variable, ":", round(value, 2))
          )
      }
      
      # Add legend
      m <- m %>%
        addLegend(
          pal = pal,
          values = ~value,
          title = input$map_variable,
          position = "bottomright"
        )
      
      return(m)
    }
  })
  
  observeEvent(input$run_spatial_analysis, {
    req(values$data_loaded, input$spatial_variable, values$distance_data)
    
    tryCatch({
      # Load distance matrix
      distance_matrix <- as.matrix(values$distance_data[, -1])  # Remove first column if it's ID
      
      # Create spatial weights matrix
      if (input$weight_type == "distance_inverse") {
        # Inverse distance weights
        weights_matrix <- 1 / (distance_matrix + diag(nrow(distance_matrix)))
        diag(weights_matrix) <- 0
      } else if (input$weight_type == "distance_exp") {
        # Exponential decay weights
        weights_matrix <- exp(-distance_matrix)
        diag(weights_matrix) <- 0
      } else {
        # K-nearest neighbors
        k <- input$k_neighbors
        weights_matrix <- matrix(0, nrow(distance_matrix), ncol(distance_matrix))
        
        for (i in 1:nrow(distance_matrix)) {
          neighbors <- order(distance_matrix[i, ])[2:(k+1)]  # Exclude self
          weights_matrix[i, neighbors] <- 1
        }
      }
      
      # Calculate Moran's I
      var_data <- values$processed_data[[input$spatial_variable]]
      morans_i <- calculate_morans_i(var_data, weights_matrix)
      
      # Calculate expected value and variance for Moran's I
      n <- length(var_data)
      expected_i <- -1 / (n - 1)
      
      # Simplified variance calculation
      variance_i <- (n^2 - 3*n + 3) / ((n-1)^2 * (n-2))
      
      # Z-score and p-value
      z_score <- (morans_i - expected_i) / sqrt(variance_i)
      p_value <- 2 * (1 - pnorm(abs(z_score)))
      
      values$morans_i_result <- list(
        morans_i = morans_i,
        expected_i = expected_i,
        variance_i = variance_i,
        z_score = z_score,
        p_value = p_value,
        weights_matrix = weights_matrix
      )
      
      values$spatial_weights <- weights_matrix
      values$spatial_analysis_done <- TRUE
      
      safe_notification("Analisis spasial berhasil!", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error dalam analisis spasial:", e$message), "error")
    })
  })
  
  output$spatial_analysis_done <- reactive({
    values$spatial_analysis_done
  })
  outputOptions(output, "spatial_analysis_done", suspendWhenHidden = FALSE)
  
  output$spatial_autocorr_result <- renderPrint({
    if (values$spatial_analysis_done && !is.null(values$morans_i_result)) {
      result <- values$morans_i_result
      
      cat("HASIL ANALISIS AUTOKORELASI SPASIAL\n")
      cat("===================================\n\n")
      
      cat("MORAN'S I STATISTIC:\n")
      cat("Variabel:", input$spatial_variable, "\n")
      cat("Tipe matriks penimbang:", switch(input$weight_type,
                                            "distance_inverse" = "Inverse Distance",
                                            "distance_exp" = "Exponential Distance",
                                            "knn" = paste("K-Nearest Neighbors (k =", input$k_neighbors, ")")), "\n\n")
      
      cat("HASIL STATISTIK:\n")
      cat("Moran's I:", round(result$morans_i, 4), "\n")
      cat("Expected I:", round(result$expected_i, 4), "\n")
      cat("Variance:", round(result$variance_i, 6), "\n")
      cat("Z-score:", round(result$z_score, 4), "\n")
      cat("P-value:", format_pvalue(result$p_value), "\n\n")
      
      cat("INTERPRETASI:\n")
      if (result$p_value <= 0.05) {
        if (result$morans_i > result$expected_i) {
          cat("AUTOKORELASI SPASIAL POSITIF SIGNIFIKAN\n")
          cat("- Nilai-nilai yang mirip cenderung berkelompok secara spasial\n")
          cat("- Terdapat pola klaster dalam distribusi spasial\n")
        } else {
          cat("AUTOKORELASI SPASIAL NEGATIF SIGNIFIKAN\n")
          cat("- Nilai-nilai yang berbeda cenderung berdekatan secara spasial\n")
          cat("- Terdapat pola dispersi dalam distribusi spasial\n")
        }
      } else {
        cat("TIDAK ADA AUTOKORELASI SPASIAL SIGNIFIKAN\n")
        cat("- Distribusi spasial bersifat acak\n")
        cat("- Tidak ada pola klaster atau dispersi yang signifikan\n")
      }
      
      # Interpretasi kekuatan
      abs_i <- abs(result$morans_i)
      cat("\nKEKUATAN AUTOKORELASI:\n")
      if (abs_i < 0.1) {
        cat("- Autokorelasi LEMAH\n")
      } else if (abs_i < 0.3) {
        cat("- Autokorelasi SEDANG\n")
      } else {
        cat("- Autokorelasi KUAT\n")
      }
    }
  })
  
  output$distance_matrix_stats <- renderPrint({
    if (values$spatial_analysis_done && !is.null(values$distance_data)) {
      distance_matrix <- as.matrix(values$distance_data[, -1])
      
      # Remove diagonal (self-distances)
      distance_values <- distance_matrix[upper.tri(distance_matrix)]
      
      cat("STATISTIK MATRIKS JARAK\n")
      cat("=======================\n\n")
      
      cat("INFORMASI UMUM:\n")
      cat("Jumlah lokasi:", nrow(distance_matrix), "\n")
      cat("Jumlah pasangan jarak:", length(distance_values), "\n\n")
      
      cat("STATISTIK DESKRIPTIF JARAK:\n")
      cat("Mean distance:", round(mean(distance_values, na.rm = TRUE), 4), "\n")
      cat("Median distance:", round(median(distance_values, na.rm = TRUE), 4), "\n")
      cat("Min distance:", round(min(distance_values, na.rm = TRUE), 4), "\n")
      cat("Max distance:", round(max(distance_values, na.rm = TRUE), 4), "\n")
      cat("Standard deviation:", round(sd(distance_values, na.rm = TRUE), 4), "\n")
      cat("Coefficient of variation:", round(sd(distance_values, na.rm = TRUE) / mean(distance_values, na.rm = TRUE) * 100, 2), "%\n\n")
      
      cat("INTERPRETASI:\n")
      cv <- sd(distance_values, na.rm = TRUE) / mean(distance_values, na.rm = TRUE) * 100
      if (cv < 30) {
        cat("- Jarak antar lokasi relatif HOMOGEN\n")
      } else if (cv < 60) {
        cat("- Jarak antar lokasi BERVARIASI SEDANG\n")
      } else {
        cat("- Jarak antar lokasi sangat HETEROGEN\n")
      }
      
      # Connectivity analysis
      if (!is.null(values$spatial_weights)) {
        weights_matrix <- values$spatial_weights
        avg_neighbors <- mean(rowSums(weights_matrix > 0))
        cat("- Rata-rata jumlah tetangga per lokasi:", round(avg_neighbors, 2), "\n")
      }
    }
  })
  
  output$spatial_interpretation <- renderPrint({
    if (values$spatial_analysis_done && !is.null(values$morans_i_result)) {
      result <- values$morans_i_result
      
      cat("INTERPRETASI KOMPREHENSIF ANALISIS SPASIAL\n")
      cat("=========================================\n\n")
      
      cat("RINGKASAN TEMUAN:\n")
      if (result$p_value <= 0.05) {
        if (result$morans_i > 0) {
          cat("- Terdapat KLASTERISASI SPASIAL yang signifikan\n")
          cat("- Wilayah dengan nilai tinggi cenderung bertetangga dengan wilayah nilai tinggi\n")
          cat("- Wilayah dengan nilai rendah cenderung bertetangga dengan wilayah nilai rendah\n")
        } else {
          cat("- Terdapat DISPERSI SPASIAL yang signifikan\n")
          cat("- Wilayah dengan nilai tinggi cenderung bertetangga dengan wilayah nilai rendah\n")
          cat("- Pola 'checkerboard' dalam distribusi spasial\n")
        }
      } else {
        cat("- Distribusi spasial ACAK (tidak ada pola signifikan)\n")
        cat("- Lokasi geografis tidak mempengaruhi nilai variabel\n")
      }
      
      cat("\nIMPLIKASI KEBIJAKAN:\n")
      if (result$p_value <= 0.05 && result$morans_i > 0) {
        cat("1. Fokus intervensi pada klaster wilayah dengan masalah serupa\n")
        cat("2. Manfaatkan efek spillover positif antar wilayah tetangga\n")
        cat("3. Pertimbangkan pendekatan regional dalam perencanaan\n")
      } else if (result$p_value <= 0.05 && result$morans_i < 0) {
        cat("1. Identifikasi faktor yang menyebabkan pola dispersi\n")
        cat("2. Evaluasi efektivitas kebijakan yang berbeda antar wilayah tetangga\n")
        cat("3. Pertimbangkan harmonisasi kebijakan regional\n")
      } else {
        cat("1. Faktor spasial tidak dominan dalam menentukan variasi\n")
        cat("2. Fokus pada faktor non-spasial dalam intervensi\n")
        cat("3. Pendekatan individual per wilayah mungkin lebih efektif\n")
      }
      
      cat("\nREKOMENDASI ANALISIS LANJUTAN:\n")
      cat("1. Analisis Local Indicators of Spatial Association (LISA)\n")
      cat("2. Spatial regression modeling\n")
      cat("3. Hotspot analysis untuk identifikasi wilayah prioritas\n")
    }
  })
  
  # =================================================================== #
  # DOWNLOAD HANDLERS - IMPLEMENTASI LENGKAP DENGAN PDF
  # =================================================================== #
  
  output$download_original_data <- downloadHandler(
    filename = function() {
      paste("data-original-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$data)) {
        write.csv(values$data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_processed_data <- downloadHandler(
    filename = function() {
      paste("data-diproses-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$processed_data)) {
        write.csv(values$processed_data, file, row.names = FALSE)
      }
    }
  )
  
  
  # 🎯 HARDCODED PATH HANDLER - PASTI SUKSES!
  
  output$download_data_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "🔥 MEMBUAT PDF DENGAN PATH FIXED...", value = 0.1)
      
      tryCatch({
        # HARDCODED PATH BERDASARKAN SCREENSHOT ANDA
        base_path <- "C:/Users/viery/OneDrive/Dokumen/College/KOMSTAT/AXIS_SECOND"
        rmd_file <- file.path(base_path, "laporan_data_management.Rmd")
        
        # CEK FILE ADA
        if (!file.exists(rmd_file)) {
          showNotification(paste("❌ FILE TIDAK ADA DI:", rmd_file), type = "error")
          return(NULL)
        }
        
        progress$set(message = "📁 FILE RMD FOUND!", value = 0.2)
        
        # Prepare data
        if (is.null(values$processed_data)) {
          showNotification("❌ Tidak ada data untuk dianalisis!", type = "error")
          return(NULL)
        }
        
        temp_data_path <- tempfile(fileext = ".csv")
        write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
        progress$inc(0.2, detail = "Data CSV siap")
        
        # Set parameters
        params <- list(
          data_path = temp_data_path,
          analysis_date = Sys.Date()
        )
        
        # Copy RMD template ke temp directory
        temp_report <- file.path(tempdir(), "laporan_data_management.Rmd")
        file.copy(rmd_file, temp_report, overwrite = TRUE)
        
        progress$inc(0.2, detail = "Template copied")
        
        # Set working directory untuk render
        old_wd <- getwd()
        setwd(tempdir())
        
        progress$inc(0.1, detail = "🚀 RENDERING PDF...")
        
        # RENDER PDF
        render_result <- tryCatch({
          rmarkdown::render(
            input = "laporan_data_management.Rmd",
            output_file = basename(file),
            params = params,
            envir = new.env(parent = globalenv()),
            quiet = FALSE  # Set FALSE untuk debug
          )
          
          # Copy hasil ke output file
          final_file <- file.path(tempdir(), basename(file))
          if (file.exists(final_file)) {
            file.copy(final_file, file, overwrite = TRUE)
            "SUCCESS"
          } else {
            "NO_OUTPUT_FILE"
          }
          
        }, error = function(e) {
          cat("RENDER ERROR:", e$message, "\n")
          return(paste("RENDER_ERROR:", e$message))
        }, finally = {
          setwd(old_wd)
        })
        
        progress$inc(0.3, detail = "✅ DONE!")
        
        # Check result
        if (render_result == "SUCCESS" && file.exists(file)) {
          showNotification("✅ PDF BERHASIL DIBUAT! REAL PDF FILE!", type = "success")
        } else {
          showNotification(paste("❌ RENDER GAGAL:", render_result), type = "error")
          cat("=== DEBUG INFO ===\n")
          cat("Render result:", render_result, "\n")
          cat("File exists:", file.exists(file), "\n")
          cat("Temp dir files:", paste(list.files(tempdir()), collapse = ", "), "\n")
        }
        
        # Cleanup
        unlink(temp_data_path)
        
      }, error = function(e) {
        showNotification(paste("❌ ERROR:", e$message), type = "error")
        cat("FATAL ERROR:", e$message, "\n")
        return(NULL)
      })
    },
    contentType = "application/pdf"
  )
  
  # 🔧 ALTERNATIF: Kalau path di atas tidak work, coba ini:
  # output$download_data_report <- downloadHandler(
  #   filename = function() {
  #     paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  #   },
  #   content = function(file) {
  #     progress <- shiny::Progress$new()
  #     on.exit(progress$close())
  #     progress$set(message = "🔥 ALTERNATIVE PATH...", value = 0.1)
  #     
  #     # ALTERNATIF PATHS
  #     possible_paths <- c(
  #       "C:/Users/viery/OneDrive/Dokumen/College/KOMSTAT/AXIS_SECOND/laporan_data_management.Rmd",
  #       "C:\\Users\\viery\\OneDrive\\Dokumen\\College\\KOMSTAT\\AXIS_SECOND\\laporan_data_management.Rmd",
  #       "./laporan_data_management.Rmd",
  #       "laporan_data_management.Rmd"
  #     )
  #     
  #     rmd_file <- NULL
  #     for (path in possible_paths) {
  #       if (file.exists(path)) {
  #         rmd_file <- path
  #         break
  #       }
  #     }
  #     
  #     if (is.null(rmd_file)) {
  #       showNotification("❌ FILE RMD TIDAK DITEMUKAN DI SEMUA PATH!", type = "error")
  #       return(NULL)
  #     }
  #     
  #     # REST OF THE CODE SAMA SEPERTI DI ATAS...
  #   },
  #   contentType = "application/pdf"
  # )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot-eksplorasi-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      if (!is.null(values$processed_data) && !is.null(input$explore_variable)) {
        var_data <- values$processed_data[[input$explore_variable]]
        
        if (is.numeric(var_data)) {
          p <- if (input$chart_type == "histogram") {
            ggplot(values$processed_data, aes_string(x = input$explore_variable)) +
              geom_histogram(bins = input$bins, fill = "#3c8dbc", alpha = 0.7) +
              labs(title = paste("Histogram", input$explore_variable))
          } else if (input$chart_type == "boxplot") {
            ggplot(values$processed_data, aes_string(y = input$explore_variable)) +
              geom_boxplot(fill = "#3c8dbc", alpha = 0.7) +
              labs(title = paste("Boxplot", input$explore_variable))
          } else if (input$chart_type == "density") {
            ggplot(values$processed_data, aes_string(x = input$explore_variable)) +
              geom_density(fill = "#3c8dbc", alpha = 0.7) +
              labs(title = paste("Density Plot", input$explore_variable))
          } else {
            ggplot(values$processed_data, aes_string(sample = input$explore_variable)) +
              stat_qq() + stat_qq_line() +
              labs(title = paste("Q-Q Plot", input$explore_variable))
          }
        } else {
          df <- as.data.frame(table(var_data))
          colnames(df) <- c("Category", "Frequency")
          p <- ggplot(df, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
            geom_bar(stat = "identity") +
            labs(title = paste("Bar Chart", input$explore_variable), x = input$explore_variable) +
            theme(legend.position = "none")
        }
        
        ggsave(file, plot = p, device = "png", width = 10, height = 6, dpi = 300)
      }
    }
  )
  
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("statistik-deskriptif-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      if (!is.null(values$processed_data) && !is.null(input$explore_variable)) {
        var_data <- values$processed_data[[input$explore_variable]]
        
        sink(file)
        cat("STATISTIK DESKRIPTIF\n")
        cat("====================\n\n")
        cat("Variabel:", input$explore_variable, "\n")
        cat("Tanggal analisis:", Sys.Date(), "\n\n")
        
        if (is.numeric(var_data)) {
          cat("STATISTIK NUMERIK:\n")
          print(summary(var_data))
          cat("\nSTATISTIK TAMBAHAN:\n")
          cat("Standard Deviation:", sd(var_data, na.rm = TRUE), "\n")
          cat("Variance:", var(var_data, na.rm = TRUE), "\n")
          cat("Skewness:", moments::skewness(var_data, na.rm = TRUE), "\n")
          cat("Kurtosis:", moments::kurtosis(var_data, na.rm = TRUE), "\n")
        } else {
          cat("TABEL FREKUENSI:\n")
          print(table(var_data, useNA = "ifany"))
        }
        sink()
      }
    }
  )
  
  output$download_exploration_report <- downloadHandler(
    filename = function() {
      paste("laporan-komprehensif-eksplorasi-", Sys.Date(), ".pdf", sep = "")
    },
    contentType = "application/pdf",
    content = function(file) {
      if (!is.null(values$processed_data) && !is.null(input$explore_variable)) {
        selected_var <- input$explore_variable
        data_for_analysis <- values$processed_data
        
        # Generate comprehensive visualizations
        temp_plots <- list()
        
        if (is.numeric(data_for_analysis[[selected_var]])) {
          # Create comprehensive histogram
          hist_plot <- create_comprehensive_histogram(data_for_analysis, selected_var)
          hist_file <- tempfile(fileext = ".png")
          ggsave(hist_file, hist_plot, width = 10, height = 6, dpi = 300)
          temp_plots$histogram <- hist_file
          
          # Create Q-Q plot
          qq_plot <- create_qq_plot(data_for_analysis, selected_var)
          qq_file <- tempfile(fileext = ".png")
          ggsave(qq_file, qq_plot, width = 8, height = 6, dpi = 300)
          temp_plots$qqplot <- qq_file
          
          # Create boxplot
          boxplot_data <- data.frame(
            Variable = selected_var,
            Value = data_for_analysis[[selected_var]]
          )
          
          boxplot_general <- ggplot(boxplot_data, aes(x = Variable, y = Value)) +
            geom_boxplot(fill = statistical_colors$primary, alpha = 0.7, outlier.color = statistical_colors$warning) +
            geom_jitter(width = 0.2, alpha = 0.5, color = "gray30") +
            stat_summary(fun = mean, geom = "point", shape = 23, 
                         size = 3, fill = statistical_colors$accent, color = "white") +
            labs(title = paste("Distribusi", selected_var),
                 subtitle = "Boxplot dengan titik data individual dan rata-rata",
                 x = "Variabel",
                 y = selected_var) +
            theme_publication()
          
          boxplot_file <- tempfile(fileext = ".png")
          ggsave(boxplot_file, boxplot_general, width = 8, height = 6, dpi = 300)
          temp_plots$boxplot <- boxplot_file
        } else {
          # Create bar chart for categorical variables
          freq_table <- table(data_for_analysis[[selected_var]])
          freq_df <- data.frame(
            Category = names(freq_table),
            Frequency = as.numeric(freq_table),
            Percentage = round(as.numeric(freq_table) / sum(freq_table) * 100, 2)
          )
          
          bar_plot <- ggplot(freq_df, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
            geom_bar(stat = "identity", alpha = 0.8) +
            geom_text(aes(label = paste0(Frequency, "\n(", Percentage, "%)")), 
                      vjust = -0.5, size = 3.5, fontface = "bold") +
            scale_fill_manual(values = publication_colors) +
            labs(title = paste("Distribusi Frekuensi", selected_var),
                 subtitle = "Jumlah dan persentase setiap kategori",
                 x = selected_var,
                 y = "Frekuensi") +
            theme_publication() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none")
          
          bar_file <- tempfile(fileext = ".png")
          ggsave(bar_file, bar_plot, width = 10, height = 6, dpi = 300)
          temp_plots$barplot <- bar_file
        }
        
        # Perform statistical tests
        statistical_tests <- list()
        
        if (is.numeric(data_for_analysis[[selected_var]])) {
          # Normality tests
          clean_data <- data_for_analysis[[selected_var]][!is.na(data_for_analysis[[selected_var]])]
          
          if (length(clean_data) >= 3 && length(clean_data) <= 5000) {
            statistical_tests$shapiro <- shapiro.test(clean_data)
          }
          
          if (length(clean_data) >= 5) {
            statistical_tests$ks <- ks.test(clean_data, "pnorm", mean(clean_data), sd(clean_data))
          }
          
          if (length(clean_data) >= 8) {
            statistical_tests$anderson <- nortest::ad.test(clean_data)
            statistical_tests$jarque_bera <- nortest::jarque.bera.test(clean_data)
          }
        }
        
        # Calculate comprehensive statistics
        if (is.numeric(data_for_analysis[[selected_var]])) {
          var_stats <- list(
            n = sum(!is.na(data_for_analysis[[selected_var]])),
            missing = sum(is.na(data_for_analysis[[selected_var]])),
            mean = mean(data_for_analysis[[selected_var]], na.rm = TRUE),
            median = median(data_for_analysis[[selected_var]], na.rm = TRUE),
            sd = sd(data_for_analysis[[selected_var]], na.rm = TRUE),
            min = min(data_for_analysis[[selected_var]], na.rm = TRUE),
            max = max(data_for_analysis[[selected_var]], na.rm = TRUE),
            q1 = quantile(data_for_analysis[[selected_var]], 0.25, na.rm = TRUE),
            q3 = quantile(data_for_analysis[[selected_var]], 0.75, na.rm = TRUE),
            skewness = moments::skewness(data_for_analysis[[selected_var]], na.rm = TRUE),
            kurtosis = moments::kurtosis(data_for_analysis[[selected_var]], na.rm = TRUE),
            cv = sd(data_for_analysis[[selected_var]], na.rm = TRUE) / mean(data_for_analysis[[selected_var]], na.rm = TRUE) * 100
          )
          
          # Outlier detection
          IQR_val <- var_stats$q3 - var_stats$q1
          lower_bound <- var_stats$q1 - 1.5 * IQR_val
          upper_bound <- var_stats$q3 + 1.5 * IQR_val
          outliers <- which(data_for_analysis[[selected_var]] < lower_bound | data_for_analysis[[selected_var]] > upper_bound)
          var_stats$outliers_count <- length(outliers)
          var_stats$outliers_percentage <- round((length(outliers) / var_stats$n) * 100, 2)
        } else {
          freq_table <- table(data_for_analysis[[selected_var]], useNA = "ifany")
          var_stats <- list(
            n = sum(!is.na(data_for_analysis[[selected_var]])),
            missing = sum(is.na(data_for_analysis[[selected_var]])),
            categories = length(unique(data_for_analysis[[selected_var]], na.rm = TRUE)),
            mode = names(freq_table)[which.max(freq_table)],
            mode_freq = max(freq_table),
            mode_percentage = round(max(freq_table) / sum(freq_table) * 100, 2)
          )
        }
        
        # Build comprehensive content
        content_list <- c(
          generate_executive_summary(data_for_analysis, paste("Eksplorasi Variabel:", selected_var)),
          generate_methodology_section("Exploratory Data Analysis"),
          
          "# Analisis Komprehensif Variabel",
          "",
          paste("## Variabel yang Dianalisis: **", selected_var, "**"),
          "",
          paste("**Tipe Data:** ", ifelse(is.numeric(data_for_analysis[[selected_var]]), "Numerik (Kuantitatif)", "Kategorikal (Kualitatif)")),
          "",
          
          if (is.numeric(data_for_analysis[[selected_var]])) {
            c(
              "# Statistik Deskriptif Lengkap",
              "",
              "## Ukuran Pemusatan",
              "",
              paste("- **Mean (Rata-rata):** ", round(var_stats$mean, 4)),
              paste("- **Median (Nilai Tengah):** ", round(var_stats$median, 4)),
              paste("- **Mode:** Tidak dihitung untuk data numerik kontinu"),
              "",
              "**Interpretasi Pemusatan:**",
              if (abs(var_stats$mean - var_stats$median) / var_stats$sd < 0.1) {
                "- Distribusi relatif simetris (mean ≈ median)"
              } else if (var_stats$mean > var_stats$median) {
                "- Distribusi cenderung positively skewed (mean > median)"
              } else {
                "- Distribusi cenderung negatively skewed (mean < median)"
              },
              "",
              "## Ukuran Penyebaran",
              "",
              paste("- **Standard Deviation:** ", round(var_stats$sd, 4)),
              paste("- **Variance:** ", round(var_stats$sd^2, 4)),
              paste("- **Range:** ", round(var_stats$max - var_stats$min, 4), " (", round(var_stats$min, 4), " - ", round(var_stats$max, 4), ")"),
              paste("- **Interquartile Range (IQR):** ", round(var_stats$q3 - var_stats$q1, 4)),
              paste("- **Coefficient of Variation:** ", round(var_stats$cv, 2), "%"),
              "",
              "**Interpretasi Penyebaran:**",
              if (var_stats$cv < 15) {
                "- Variabilitas rendah (CV < 15%)"
              } else if (var_stats$cv < 30) {
                "- Variabilitas sedang (15% ≤ CV < 30%)"
              } else {
                "- Variabilitas tinggi (CV ≥ 30%)"
              },
              "",
              "## Ukuran Bentuk Distribusi",
              "",
              paste("- **Skewness (Kemiringan):** ", round(var_stats$skewness, 4)),
              paste("- **Kurtosis (Keruncingan):** ", round(var_stats$kurtosis, 4)),
              "",
              "**Interpretasi Bentuk:**",
              paste("- **Skewness:** ", 
                    if (abs(var_stats$skewness) < 0.5) "Distribusi simetris"
                    else if (var_stats$skewness > 0.5) "Distribusi positively skewed (ekor kanan panjang)"
                    else "Distribusi negatively skewed (ekor kiri panjang)"),
              paste("- **Kurtosis:** ", 
                    if (abs(var_stats$kurtosis - 3) < 0.5) "Distribusi mesokurtic (normal)"
                    else if (var_stats$kurtosis > 3) "Distribusi leptokurtic (runcing)"
                    else "Distribusi platykurtic (datar)"),
              "",
              "## Five Number Summary",
              "",
              paste("- **Minimum:** ", round(var_stats$min, 4)),
              paste("- **Q1 (Kuartil 1):** ", round(var_stats$q1, 4)),
              paste("- **Q2 (Median):** ", round(var_stats$median, 4)),
              paste("- **Q3 (Kuartil 3):** ", round(var_stats$q3, 4)),
              paste("- **Maximum:** ", round(var_stats$max, 4)),
              "",
              "## Analisis Outliers",
              "",
              paste("- **Jumlah Outliers:** ", var_stats$outliers_count),
              paste("- **Persentase Outliers:** ", var_stats$outliers_percentage, "%"),
              paste("- **Batas Bawah (Q1 - 1.5×IQR):** ", round(lower_bound, 4)),
              paste("- **Batas Atas (Q3 + 1.5×IQR):** ", round(upper_bound, 4)),
              "",
              "**Interpretasi Outliers:**",
              if (var_stats$outliers_percentage == 0) {
                "- Tidak terdapat outliers dalam data"
              } else if (var_stats$outliers_percentage < 5) {
                "- Outliers dalam batas wajar (< 5%)"
              } else {
                "- Perlu investigasi lebih lanjut untuk outliers yang tinggi"
              }
            )
          } else {
            c(
              "# Statistik Deskriptif Kategorikal",
              "",
              "## Informasi Umum",
              "",
              paste("- **Jumlah Kategori:** ", var_stats$categories),
              paste("- **Mode (Kategori Terbanyak):** ", var_stats$mode),
              paste("- **Frekuensi Mode:** ", var_stats$mode_freq, " (", var_stats$mode_percentage, "%)", sep = ""),
              "",
              "## Distribusi Frekuensi",
              "",
              "```{r freq-table}",
              "freq_data <- data.frame(",
              paste("  Kategori = c(", paste("'", names(freq_table), "'", collapse = ", "), "),", sep = ""),
              paste("  Frekuensi = c(", paste(as.numeric(freq_table), collapse = ", "), "),"),
              paste("  Persentase = c(", paste(round(as.numeric(freq_table)/sum(freq_table)*100, 2), collapse = ", "), ")"),
              ")",
              "kable(freq_data, caption = 'Distribusi Frekuensi dan Persentase')",
              "```",
              "",
              "**Interpretasi Distribusi:**",
              if (var_stats$mode_percentage > 50) {
                "- Distribusi sangat tidak merata (mode > 50%)"
              } else if (var_stats$mode_percentage > 30) {
                "- Distribusi tidak merata (mode > 30%)"
              } else {
                "- Distribusi relatif merata"
              }
            )
          },
          "",
          "## Kualitas Data",
          "",
          paste("- **Total Observasi:** ", format(var_stats$n + var_stats$missing, big.mark = ",")),
          paste("- **Data Valid:** ", format(var_stats$n, big.mark = ","), " (", round((var_stats$n / (var_stats$n + var_stats$missing)) * 100, 2), "%)", sep = ""),
          paste("- **Missing Data:** ", format(var_stats$missing, big.mark = ","), " (", round((var_stats$missing / (var_stats$n + var_stats$missing)) * 100, 2), "%)", sep = ""),
          "",
          
          "# Visualisasi Data",
          "",
          if (is.numeric(data_for_analysis[[selected_var]])) {
            c(
              "## Histogram dengan Kurva Densitas",
              "",
              "```{r histogram, fig.cap='Distribusi Data dengan Overlay Normal'}",
              paste("knitr::include_graphics('", temp_plots$histogram, "')", sep = ""),
              "```",
              "",
              "Histogram menunjukkan distribusi empiris data dengan overlay kurva densitas dan distribusi normal teoritis untuk perbandingan.",
              "",
              "## Q-Q Plot",
              "",
              "```{r qqplot, fig.cap='Q-Q Plot untuk Uji Normalitas Visual'}",
              paste("knitr::include_graphics('", temp_plots$qqplot, "')", sep = ""),
              "```",
              "",
              "Q-Q Plot membandingkan distribusi data dengan distribusi normal teoritis. Jika titik-titik mengikuti garis diagonal, data mendekati distribusi normal.",
              "",
              "## Boxplot",
              "",
              "```{r boxplot, fig.cap='Boxplot untuk Identifikasi Outliers dan Distribusi'}",
              paste("knitr::include_graphics('", temp_plots$boxplot, "')", sep = ""),
              "```",
              "",
              "Boxplot menampilkan five-number summary dan outliers secara visual."
            )
          } else {
            c(
              "## Bar Chart Distribusi Frekuensi",
              "",
              "```{r barplot, fig.cap='Distribusi Frekuensi Kategori'}",
              paste("knitr::include_graphics('", temp_plots$barplot, "')", sep = ""),
              "```",
              "",
              "Bar chart menunjukkan distribusi frekuensi setiap kategori dengan informasi jumlah dan persentase."
            )
          },
          "",
          
          if (is.numeric(data_for_analysis[[selected_var]]) && length(statistical_tests) > 0) {
            c(
              "# Uji Normalitas",
              "",
              "Beberapa uji statistik dilakukan untuk menguji asumsi normalitas:",
              "",
              if (!is.null(statistical_tests$shapiro)) {
                c(
                  "## Shapiro-Wilk Test",
                  "",
                  paste("- **Statistik W:** ", round(statistical_tests$shapiro$statistic, 4)),
                  paste("- **P-value:** ", format_pvalue(statistical_tests$shapiro$p.value)),
                  paste("- **Interpretasi:** ", significance_interpretation(statistical_tests$shapiro$p.value)),
                  ""
                )
              } else {
                ""
              },
              
              if (!is.null(statistical_tests$ks)) {
                c(
                  "## Kolmogorov-Smirnov Test",
                  "",
                  paste("- **Statistik D:** ", round(statistical_tests$ks$statistic, 4)),
                  paste("- **P-value:** ", format_pvalue(statistical_tests$ks$p.value)),
                  paste("- **Interpretasi:** ", significance_interpretation(statistical_tests$ks$p.value)),
                  ""
                )
              } else {
                ""
              },
              
              if (!is.null(statistical_tests$anderson)) {
                c(
                  "## Anderson-Darling Test",
                  "",
                  paste("- **Statistik A:** ", round(statistical_tests$anderson$statistic, 4)),
                  paste("- **P-value:** ", format_pvalue(statistical_tests$anderson$p.value)),
                  paste("- **Interpretasi:** ", significance_interpretation(statistical_tests$anderson$p.value)),
                  ""
                )
              } else {
                ""
              },
              
              if (!is.null(statistical_tests$jarque_bera)) {
                c(
                  "## Jarque-Bera Test",
                  "",
                  paste("- **Statistik JB:** ", round(statistical_tests$jarque_bera$statistic, 4)),
                  paste("- **P-value:** ", format_pvalue(statistical_tests$jarque_bera$p.value)),
                  paste("- **Interpretasi:** ", significance_interpretation(statistical_tests$jarque_bera$p.value)),
                  ""
                )
              } else {
                ""
              },
              
              "## Kesimpulan Uji Normalitas",
              "",
              if (!is.null(statistical_tests$shapiro) && statistical_tests$shapiro$p.value > 0.05) {
                "Berdasarkan uji statistik, **data mengikuti distribusi normal** (p > 0.05)."
              } else {
                "Berdasarkan uji statistik, **data tidak mengikuti distribusi normal** (p ≤ 0.05)."
              },
              "",
              "**Implikasi untuk Analisis Lanjutan:**",
              if (!is.null(statistical_tests$shapiro) && statistical_tests$shapiro$p.value > 0.05) {
                c(
                  "- Dapat menggunakan uji parametrik",
                  "- Analisis regresi linear dapat diterapkan",
                  "- Confidence interval berbasis distribusi normal valid"
                )
              } else {
                c(
                  "- Pertimbangkan transformasi data",
                  "- Gunakan uji non-parametrik",
                  "- Bootstrap confidence interval lebih apropriado"
                )
              }
            )
          } else {
            ""
          },
          
          "# Kesimpulan dan Rekomendasi",
          "",
          "## Ringkasan Temuan",
          "",
          if (is.numeric(data_for_analysis[[selected_var]])) {
            c(
              paste("- Variabel", selected_var, "memiliki rata-rata", round(var_stats$mean, 2), "dengan standar deviasi", round(var_stats$sd, 2)),
              paste("- Koefisien variasi sebesar", round(var_stats$cv, 2), "% menunjukkan variabilitas",
                    if (var_stats$cv < 15) "rendah" else if (var_stats$cv < 30) "sedang" else "tinggi"),
              paste("- Distribusi data", 
                    if (abs(var_stats$skewness) < 0.5) "simetris" 
                    else if (var_stats$skewness > 0) "positively skewed" 
                    else "negatively skewed"),
              paste("- Terdapat", var_stats$outliers_count, "outliers (", var_stats$outliers_percentage, "%)")
            )
          } else {
            c(
              paste("- Variabel", selected_var, "memiliki", var_stats$categories, "kategori"),
              paste("- Kategori paling sering adalah", var_stats$mode, "(", var_stats$mode_percentage, "%)"),
              paste("- Distribusi kategori", 
                    if (var_stats$mode_percentage > 50) "sangat tidak merata"
                    else if (var_stats$mode_percentage > 30) "tidak merata"
                    else "relatif merata")
            )
          },
          "",
          "## Rekomendasi Analisis Lanjutan",
          "",
          if (is.numeric(data_for_analysis[[selected_var]])) {
            c(
              if (!is.null(statistical_tests$shapiro) && statistical_tests$shapiro$p.value > 0.05) {
                "1. **Analisis Parametrik:** Data normal, gunakan t-test, ANOVA, regresi linear"
              } else {
                "1. **Analisis Non-Parametrik:** Data tidak normal, gunakan Mann-Whitney, Kruskal-Wallis"
              },
              
              if (var_stats$outliers_percentage > 5) {
                "2. **Treatment Outliers:** Investigasi dan pertimbangkan windsorizing"
              } else {
                "2. **Outliers:** Dalam batas wajar, tidak perlu treatment khusus"
              },
              
              "3. **Transformasi Data:** Jika diperlukan normalisasi, pertimbangkan log, sqrt, atau Box-Cox",
              "4. **Analisis Korelasi:** Hubungan dengan variabel lain",
              "5. **Segmentasi:** Analisis berdasarkan subgroup jika relevan"
            )
          } else {
            c(
              "1. **Analisis Asosiasi:** Chi-square test untuk hubungan dengan variabel lain",
              "2. **Analisis Frekuensi:** Perbandingan proporsi antar kategori",
              "3. **Segmentasi:** Analisis cross-tabulation dengan variabel lain",
              "4. **Visualisasi:** Pie chart, stacked bar chart untuk presentasi",
              "5. **Prediksi:** Logistic regression jika sebagai variabel target"
            )
          },
          "",
          "---",
          "",
          "**Catatan:** Laporan ini dibuat secara otomatis oleh AXIS Dashboard.",
          paste("**Timestamp:** ", format(Sys.time(), "%d %B %Y, %H:%M:%S %Z")),
          "",
          "Untuk analisis statistik lebih mendalam, silakan gunakan modul uji hipotesis yang tersedia di AXIS Dashboard."
        )
        
        # Generate comprehensive PDF
        result <- create_comprehensive_pdf_report(content_list, file, paste("Laporan Komprehensif Eksplorasi Variabel:", selected_var), values$processed_data)
        
        # Clean up temporary files
        for (plot_file in temp_plots) {
          if (file.exists(plot_file)) {
            unlink(plot_file)
          }
        }
        
        if (result != "Success") {
          safe_notification(paste("Error generating PDF:", result), "error")
        } else {
          safe_notification("PDF laporan eksplorasi berhasil dibuat!", "success")
        }
      }
    }
  )
  
  # Additional download handlers for all modules with PDF support
  output$download_assumptions_results <- downloadHandler(
    filename = function() {
      paste("hasil-uji-asumsi-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("HASIL UJI ASUMSI STATISTIK\n")
      cat("===========================\n\n")
      cat("Tanggal analisis:", Sys.Date(), "\n\n")
      # Add assumption test results
      sink()
    }
  )
  
  output$download_assumptions_plots <- downloadHandler(
    filename = function() {
      paste("plot-asumsi-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create assumption plots
      png(file, width = 800, height = 600)
      par(mfrow = c(2, 2))
      # Add assumption plots here
      dev.off()
    }
  )
  
  output$download_assumptions_report <- downloadHandler(
    filename = function() {
      paste("laporan-asumsi-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      content <- paste(
        "# Laporan Uji Asumsi Statistik",
        "",
        paste("**Tanggal:**", Sys.Date()),
        "",
        "## Uji Normalitas",
        "",
        "Hasil uji normalitas menunjukkan apakah data mengikuti distribusi normal.",
        "",
        "## Uji Homogenitas",
        "",
        "Hasil uji homogenitas menunjukkan apakah varians antar grup homogen.",
        "",
        "## Kesimpulan",
        "",
        "Berdasarkan hasil uji asumsi, dapat ditentukan metode analisis yang tepat.",
        sep = "\n"
      )
      
      create_pdf_report(content, file, "Laporan Uji Asumsi Statistik")
    }
  )
  
  # Mean test downloads
  output$download_mean_test_results <- downloadHandler(
    filename = function() {
      paste("hasil-uji-rata-rata-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("HASIL UJI BEDA RATA-RATA\n")
      cat("========================\n\n")
      # Add mean test results
      sink()
    }
  )
  
  output$download_mean_test_plot <- downloadHandler(
    filename = function() {
      paste("plot-uji-rata-rata-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create mean test plot
      if (values$mean_test_done) {
        # Add plot creation code
      }
    }
  )
  
  output$download_mean_test_report <- downloadHandler(
    filename = function() {
      paste("laporan-uji-rata-rata-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      content <- paste(
        "# Laporan Uji Beda Rata-rata",
        "",
        paste("**Tanggal:**", Sys.Date()),
        "",
        "## Hasil Uji Statistik",
        "",
        "Analisis uji beda rata-rata untuk menentukan signifikansi perbedaan antar grup.",
        "",
        "## Interpretasi",
        "",
        "Hasil menunjukkan tingkat signifikansi dan effect size dari perbedaan yang diamati.",
        sep = "\n"
      )
      
      create_pdf_report(content, file, "Laporan Uji Beda Rata-rata")
    }
  )
  
  # Proportion and variance test downloads
  output$download_prop_var_results <- downloadHandler(
    filename = function() {
      paste("hasil-uji-proporsi-varians-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("HASIL UJI PROPORSI & VARIANS\n")
      cat("=============================\n\n")
      # Add results
      sink()
    }
  )
  
  output$download_prop_var_plots <- downloadHandler(
    filename = function() {
      paste("plot-proporsi-varians-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create plots
    }
  )
  
  output$download_prop_var_report <- downloadHandler(
    filename = function() {
      paste("laporan-proporsi-varians-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      content <- paste(
        "# Laporan Uji Proporsi & Varians",
        "",
        paste("**Tanggal:**", Sys.Date()),
        "",
        "## Uji Proporsi",
        "",
        "Analisis perbedaan proporsi antar grup atau terhadap nilai hipotesis.",
        "",
        "## Uji Varians",
        "",
        "Analisis perbedaan varians untuk menguji homogenitas atau perbandingan variabilitas.",
        sep = "\n"
      )
      
      create_pdf_report(content, file, "Laporan Uji Proporsi & Varians")
    }
  )
  
  # ANOVA downloads
  output$download_anova_results <- downloadHandler(
    filename = function() {
      paste("hasil-anova-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("HASIL ANOVA\n")
      cat("===========\n\n")
      # Add ANOVA results
      sink()
    }
  )
  
  output$download_anova_plots <- downloadHandler(
    filename = function() {
      paste("plot-anova-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create ANOVA plots
    }
  )
  
  output$download_anova_report <- downloadHandler(
    filename = function() {
      paste("laporan-anova-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      content <- paste(
        "# Laporan ANOVA",
        "",
        paste("**Tanggal:**", Sys.Date()),
        "",
        "## Analysis of Variance",
        "",
        "Analisis varians untuk membandingkan rata-rata dari tiga atau lebih kelompok.",
        "",
        "## Post-hoc Analysis",
        "",
        "Analisis lanjutan untuk mengidentifikasi kelompok mana yang berbeda signifikan.",
        "",
        "## Effect Size",
        "",
        "Ukuran efek untuk menentukan signifikansi praktis dari perbedaan yang ditemukan.",
        sep = "\n"
      )
      
      create_pdf_report(content, file, "Laporan ANOVA")
    }
  )
  
  # Regression downloads
  output$download_regression_results <- downloadHandler(
    filename = function() {
      paste("hasil-regresi-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("HASIL REGRESI LINEAR BERGANDA\n")
      cat("=============================\n\n")
      # Add regression results
      sink()
    }
  )
  
  output$download_regression_plots <- downloadHandler(
    filename = function() {
      paste("plot-regresi-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create regression diagnostic plots
    }
  )
  
  output$download_regression_assumptions <- downloadHandler(
    filename = function() {
      paste("asumsi-regresi-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("UJI ASUMSI REGRESI\n")
      cat("==================\n\n")
      # Add regression assumption tests
      sink()
    }
  )
  
  output$download_regression_report <- downloadHandler(
    filename = function() {
      paste("laporan-komprehensif-regresi-", Sys.Date(), ".pdf", sep = "")
    },
    contentType = "application/pdf",
    content = function(file) {
      if (!is.null(values$regression_model) && !is.null(input$reg_dependent) && !is.null(input$reg_independent)) {
        model <- values$regression_model
        dep_var <- input$reg_dependent
        indep_vars <- input$reg_independent
        data_for_analysis <- values$processed_data
        
        # Generate comprehensive visualizations
        temp_plots <- list()
        
        # 1. Residuals vs Fitted Plot
        residual_fitted_plot <- ggplot(data = NULL, aes(x = fitted(model), y = residuals(model))) +
          geom_point(alpha = 0.6, color = statistical_colors$primary) +
          geom_hline(yintercept = 0, color = statistical_colors$warning, linetype = "dashed", size = 1) +
          geom_smooth(method = "loess", se = TRUE, color = statistical_colors$accent) +
          labs(title = "Residuals vs Fitted Values",
               subtitle = "Uji asumsi linearitas dan homoskedastisitas",
               x = "Fitted Values",
               y = "Residuals") +
          theme_publication()
        
        residual_file <- tempfile(fileext = ".png")
        ggsave(residual_file, residual_fitted_plot, width = 10, height = 6, dpi = 300)
        temp_plots$residual_fitted <- residual_file
        
        # 2. Q-Q Plot of Residuals
        qq_residual_plot <- ggplot(data = NULL, aes(sample = residuals(model))) +
          stat_qq(color = statistical_colors$primary, alpha = 0.7) +
          stat_qq_line(color = statistical_colors$warning, size = 1) +
          labs(title = "Q-Q Plot of Residuals",
               subtitle = "Uji normalitas residual",
               x = "Theoretical Quantiles",
               y = "Sample Quantiles") +
          theme_publication()
        
        qq_residual_file <- tempfile(fileext = ".png")
        ggsave(qq_residual_file, qq_residual_plot, width = 8, height = 6, dpi = 300)
        temp_plots$qq_residual <- qq_residual_file
        
        # 3. Scale-Location Plot
        sqrt_abs_residuals <- sqrt(abs(residuals(model)))
        scale_location_plot <- ggplot(data = NULL, aes(x = fitted(model), y = sqrt_abs_residuals)) +
          geom_point(alpha = 0.6, color = statistical_colors$primary) +
          geom_smooth(method = "loess", se = TRUE, color = statistical_colors$accent) +
          labs(title = "Scale-Location Plot",
               subtitle = "Uji homoskedastisitas",
               x = "Fitted Values",
               y = "√|Residuals|") +
          theme_publication()
        
        scale_location_file <- tempfile(fileext = ".png")
        ggsave(scale_location_file, scale_location_plot, width = 10, height = 6, dpi = 300)
        temp_plots$scale_location <- scale_location_file
        
        # 4. Cook's Distance Plot
        cooks_dist <- cooks.distance(model)
        cook_plot <- ggplot(data = NULL, aes(x = 1:length(cooks_dist), y = cooks_dist)) +
          geom_col(fill = statistical_colors$primary, alpha = 0.7) +
          geom_hline(yintercept = 4/length(cooks_dist), color = statistical_colors$warning, 
                     linetype = "dashed", size = 1) +
          labs(title = "Cook's Distance",
               subtitle = "Identifikasi observasi berpengaruh",
               x = "Observation Number",
               y = "Cook's Distance") +
          theme_publication()
        
        cook_file <- tempfile(fileext = ".png")
        ggsave(cook_file, cook_plot, width = 10, height = 6, dpi = 300)
        temp_plots$cook_distance <- cook_file
        
        # 5. Correlation Matrix of Independent Variables
        if (length(indep_vars) > 1) {
          indep_data <- data_for_analysis[indep_vars]
          cor_matrix <- cor(indep_data, use = "complete.obs")
          
          cor_plot_file <- tempfile(fileext = ".png")
          png(cor_plot_file, width = 800, height = 800, res = 150)
          corrplot(cor_matrix, method = "color", type = "upper", 
                   order = "hclust", tl.cex = 0.8, tl.col = "black",
                   addCoef.col = "black", number.cex = 0.7,
                   col = colorRampPalette(c("#C73E1D", "white", "#2E86AB"))(100),
                   title = "Correlation Matrix - Independent Variables")
          dev.off()
          temp_plots$correlation <- cor_plot_file
        }
        
        # Perform comprehensive diagnostics
        model_summary <- summary(model)
        
        # Normality tests on residuals
        residual_tests <- list()
        clean_residuals <- residuals(model)[!is.na(residuals(model))]
        
        if (length(clean_residuals) >= 3 && length(clean_residuals) <= 5000) {
          residual_tests$shapiro <- shapiro.test(clean_residuals)
        }
        
        if (length(clean_residuals) >= 8) {
          residual_tests$jarque_bera <- nortest::jarque.bera.test(clean_residuals)
        }
        
        # Multicollinearity test (VIF)
        vif_values <- NULL
        if (length(indep_vars) > 1) {
          tryCatch({
            vif_values <- car::vif(model)
          }, error = function(e) {
            vif_values <- NULL
          })
        }
        
        # Heteroskedasticity tests
        hetero_tests <- list()
        tryCatch({
          hetero_tests$breusch_pagan <- lmtest::bptest(model)
        }, error = function(e) {
          hetero_tests$breusch_pagan <- NULL
        })
        
        # Durbin-Watson test for autocorrelation
        dw_test <- NULL
        tryCatch({
          dw_test <- lmtest::dwtest(model)
        }, error = function(e) {
          dw_test <- NULL
        })
        
        # Model performance metrics
        r_squared <- model_summary$r.squared
        adj_r_squared <- model_summary$adj.r.squared
        f_statistic <- model_summary$fstatistic
        rmse <- sqrt(mean(residuals(model)^2, na.rm = TRUE))
        mae <- mean(abs(residuals(model)), na.rm = TRUE)
        
        # AIC and BIC
        aic_value <- AIC(model)
        bic_value <- BIC(model)
        
        # Coefficients analysis
        coef_table <- model_summary$coefficients
        conf_intervals <- confint(model)
        
        # Build comprehensive content
        content_list <- c(
          generate_executive_summary(data_for_analysis, paste("Regresi Linear Berganda:", dep_var)),
          generate_methodology_section("Multiple Linear Regression"),
          
          "# Analisis Regresi Linear Berganda Komprehensif",
          "",
          paste("## Model yang Dianalisis"),
          "",
          paste("**Variabel Dependen:** ", dep_var),
          paste("**Variabel Independen:** ", paste(indep_vars, collapse = ", ")),
          "",
          paste("**Persamaan Model:**"),
          paste("", dep_var, "= β₀ +", paste("β", 1:length(indep_vars), "×", indep_vars, collapse = " + "), "+ ε"),
          "",
          
          "# Statistik Model",
          "",
          "## Goodness of Fit",
          "",
          paste("- **R-squared:** ", round(r_squared, 4), " (", round(r_squared * 100, 2), "%)", sep = ""),
          paste("- **Adjusted R-squared:** ", round(adj_r_squared, 4), " (", round(adj_r_squared * 100, 2), "%)", sep = ""),
          paste("- **F-statistic:** ", round(f_statistic[1], 4), " (df = ", f_statistic[2], ", ", f_statistic[3], ")", sep = ""),
          paste("- **P-value (F-test):** ", format_pvalue(pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE))),
          "",
          "**Interpretasi Goodness of Fit:**",
          if (r_squared >= 0.7) {
            "- Model memiliki kemampuan prediksi yang **sangat baik** (R² ≥ 0.7)"
          } else if (r_squared >= 0.5) {
            "- Model memiliki kemampuan prediksi yang **baik** (0.5 ≤ R² < 0.7)"
          } else if (r_squared >= 0.3) {
            "- Model memiliki kemampuan prediksi yang **moderat** (0.3 ≤ R² < 0.5)"
          } else {
            "- Model memiliki kemampuan prediksi yang **lemah** (R² < 0.3)"
          },
          "",
          "## Model Performance Metrics",
          "",
          paste("- **RMSE (Root Mean Square Error):** ", round(rmse, 4)),
          paste("- **MAE (Mean Absolute Error):** ", round(mae, 4)),
          paste("- **AIC (Akaike Information Criterion):** ", round(aic_value, 4)),
          paste("- **BIC (Bayesian Information Criterion):** ", round(bic_value, 4)),
          "",
          "**Interpretasi Performance:**",
          "- RMSE dan MAE mengukur tingkat kesalahan prediksi model",
          "- Nilai yang lebih kecil menunjukkan performa yang lebih baik",
          "- AIC dan BIC berguna untuk perbandingan model (nilai lebih kecil lebih baik)",
          "",
          
          "# Analisis Koefisien Regresi",
          "",
          "## Tabel Koefisien Lengkap",
          "",
          "```{r coef-table}",
          "coef_data <- data.frame(",
          paste("  Variable = c(", paste("'", rownames(coef_table), "'", collapse = ", "), "),", sep = ""),
          paste("  Estimate = c(", paste(round(coef_table[, "Estimate"], 4), collapse = ", "), "),"),
          paste("  Std_Error = c(", paste(round(coef_table[, "Std. Error"], 4), collapse = ", "), "),"),
          paste("  t_value = c(", paste(round(coef_table[, "t value"], 4), collapse = ", "), "),"),
          paste("  p_value = c(", paste(round(coef_table[, "Pr(>|t|)"], 4), collapse = ", "), "),"),
          paste("  CI_Lower = c(", paste(round(conf_intervals[, 1], 4), collapse = ", "), "),"),
          paste("  CI_Upper = c(", paste(round(conf_intervals[, 2], 4), collapse = ", "), ")"),
          ")",
          "kable(coef_data, caption = 'Tabel Koefisien Regresi dengan Confidence Interval')",
          "```",
          "",
          
          "## Interpretasi Koefisien",
          "",
          # Generate interpretation for each coefficient
          paste(sapply(1:nrow(coef_table), function(i) {
            var_name <- rownames(coef_table)[i]
            estimate <- coef_table[i, "Estimate"]
            p_value <- coef_table[i, "Pr(>|t|)"]
            ci_lower <- conf_intervals[i, 1]
            ci_upper <- conf_intervals[i, 2]
            
            if (var_name == "(Intercept)") {
              paste("**Intercept (β₀):**",
                    "- Nilai:", round(estimate, 4),
                    "- Interpretasi: Nilai prediksi", dep_var, "ketika semua variabel independen = 0",
                    "- Signifikansi:", significance_interpretation(p_value),
                    "- 95% CI: [", round(ci_lower, 4), ", ", round(ci_upper, 4), "]",
                    "", sep = "\n")
            } else {
              paste("**", var_name, " (β", i, "):**",
                    "- Koefisien:", round(estimate, 4),
                    "- Interpretasi: Setiap peningkatan 1 unit", var_name, 
                    if (estimate > 0) "meningkatkan" else "menurunkan", dep_var, "sebesar", abs(round(estimate, 4)), "unit",
                    "- Signifikansi:", significance_interpretation(p_value),
                    "- 95% CI: [", round(ci_lower, 4), ", ", round(ci_upper, 4), "]",
                    "", sep = "\n")
            }
          }), collapse = "\n"),
          "",
          
          if (!is.null(vif_values)) {
            c(
              "# Analisis Multikolinearitas",
              "",
              "## Variance Inflation Factor (VIF)",
              "",
              paste("| Variabel | VIF | Interpretasi |",
                    "|----------|-----|--------------|",
                    paste("|", names(vif_values), "|", round(vif_values, 3), "|",
                          ifelse(vif_values < 5, "Normal", 
                                 ifelse(vif_values < 10, "Moderat", "Tinggi")), "|",
                          collapse = "\n"),
                    sep = "\n"),
              "",
              "**Kriteria VIF:**",
              "- VIF < 5: Tidak ada masalah multikolinearitas",
              "- 5 ≤ VIF < 10: Multikolinearitas moderat",
              "- VIF ≥ 10: Multikolinearitas tinggi (perlu treatment)",
              "",
              "**Kesimpulan Multikolinearitas:**",
              if (max(vif_values) < 5) {
                "Tidak terdapat masalah multikolinearitas yang signifikan dalam model."
              } else if (max(vif_values) < 10) {
                "Terdapat multikolinearitas moderat yang perlu diperhatikan."
              } else {
                "Terdapat multikolinearitas tinggi yang memerlukan treatment (ridge regression, PCA, dll)."
              }
            )
          } else {
            ""
          },
          "",
          
          "# Uji Asumsi Regresi",
          "",
          "## 1. Uji Normalitas Residual",
          "",
          if (!is.null(residual_tests$shapiro)) {
            c(
              "### Shapiro-Wilk Test",
              "",
              paste("- **Statistik W:** ", round(residual_tests$shapiro$statistic, 4)),
              paste("- **P-value:** ", format_pvalue(residual_tests$shapiro$p.value)),
              paste("- **Interpretasi:** ", significance_interpretation(residual_tests$shapiro$p.value)),
              ""
            )
          } else {
            ""
          },
          
          if (!is.null(residual_tests$jarque_bera)) {
            c(
              "### Jarque-Bera Test",
              "",
              paste("- **Statistik JB:** ", round(residual_tests$jarque_bera$statistic, 4)),
              paste("- **P-value:** ", format_pvalue(residual_tests$jarque_bera$p.value)),
              paste("- **Interpretasi:** ", significance_interpretation(residual_tests$jarque_bera$p.value)),
              ""
            )
          } else {
            ""
          },
          
          "## 2. Uji Homoskedastisitas",
          "",
          if (!is.null(hetero_tests$breusch_pagan)) {
            c(
              "### Breusch-Pagan Test",
              "",
              paste("- **Statistik BP:** ", round(hetero_tests$breusch_pagan$statistic, 4)),
              paste("- **P-value:** ", format_pvalue(hetero_tests$breusch_pagan$p.value)),
              paste("- **Interpretasi:** ", significance_interpretation(hetero_tests$breusch_pagan$p.value)),
              ""
            )
          } else {
            ""
          },
          
          "## 3. Uji Autokorelasi",
          "",
          if (!is.null(dw_test)) {
            c(
              "### Durbin-Watson Test",
              "",
              paste("- **Statistik DW:** ", round(dw_test$statistic, 4)),
              paste("- **P-value:** ", format_pvalue(dw_test$p.value)),
              paste("- **Interpretasi:** ", 
                    if (dw_test$p.value > 0.05) "Tidak ada autokorelasi" else "Terdapat autokorelasi"),
              ""
            )
          } else {
            ""
          },
          
          "# Visualisasi Diagnostik Model",
          "",
          "## Residuals vs Fitted Plot",
          "",
          "```{r residual-fitted, fig.cap='Residuals vs Fitted Values'}",
          paste("knitr::include_graphics('", temp_plots$residual_fitted, "')", sep = ""),
          "```",
          "",
          "Plot ini menguji asumsi linearitas dan homoskedastisitas. Pola acak tanpa trend menunjukkan asumsi terpenuhi.",
          "",
          "## Q-Q Plot Residual",
          "",
          "```{r qq-residual, fig.cap='Q-Q Plot of Residuals'}",
          paste("knitr::include_graphics('", temp_plots$qq_residual, "')", sep = ""),
          "```",
          "",
          "Q-Q plot menguji normalitas residual. Titik-titik yang mengikuti garis diagonal menunjukkan distribusi normal.",
          "",
          "## Scale-Location Plot",
          "",
          "```{r scale-location, fig.cap='Scale-Location Plot'}",
          paste("knitr::include_graphics('", temp_plots$scale_location, "')", sep = ""),
          "```",
          "",
          "Plot ini menguji homoskedastisitas. Garis horizontal tanpa trend menunjukkan varians konstan.",
          "",
          "## Cook's Distance",
          "",
          "```{r cook-distance, fig.cap='Cook\\'s Distance'}",
          paste("knitr::include_graphics('", temp_plots$cook_distance, "')", sep = ""),
          "```",
          "",
          paste("Cook's Distance mengidentifikasi observasi berpengaruh. Threshold:", round(4/length(cooks_dist), 4)),
          "",
          
          if (!is.null(temp_plots$correlation)) {
            c(
              "## Matriks Korelasi Variabel Independen",
              "",
              "```{r correlation-matrix, fig.cap='Correlation Matrix of Independent Variables'}",
              paste("knitr::include_graphics('", temp_plots$correlation, "')", sep = ""),
              "```",
              "",
              "Matriks korelasi menunjukkan hubungan antar variabel independen untuk deteksi multikolinearitas."
            )
          } else {
            ""
          },
          "",
          
          "# Kesimpulan dan Rekomendasi",
          "",
          "## Ringkasan Model",
          "",
          paste("- **Kemampuan Prediksi:** Model menjelaskan", round(r_squared * 100, 2), "% variasi dalam", dep_var),
          paste("- **Signifikansi Model:** ", 
                if (pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE) < 0.05) 
                  "Model secara keseluruhan signifikan" 
                else 
                  "Model secara keseluruhan tidak signifikan"),
          paste("- **Jumlah Variabel Signifikan:** ", 
                sum(coef_table[-1, "Pr(>|t|)"] < 0.05), " dari ", length(indep_vars), " variabel"),
          "",
          "## Evaluasi Asumsi",
          "",
          paste("- **Normalitas Residual:** ", 
                if (!is.null(residual_tests$shapiro) && residual_tests$shapiro$p.value > 0.05) 
                  "✓ Terpenuhi" 
                else 
                  "✗ Tidak terpenuhi"),
          paste("- **Homoskedastisitas:** ", 
                if (!is.null(hetero_tests$breusch_pagan) && hetero_tests$breusch_pagan$p.value > 0.05) 
                  "✓ Terpenuhi" 
                else 
                  "✗ Tidak terpenuhi"),
          paste("- **No Autokorelasi:** ", 
                if (!is.null(dw_test) && dw_test$p.value > 0.05) 
                  "✓ Terpenuhi" 
                else 
                  "✗ Tidak terpenuhi"),
          paste("- **No Multikolinearitas:** ", 
                if (!is.null(vif_values) && max(vif_values) < 5) 
                  "✓ Terpenuhi" 
                else if (!is.null(vif_values)) 
                  "⚠ Perlu perhatian" 
                else 
                  "- Tidak dapat diuji (hanya 1 variabel)"),
          "",
          "## Rekomendasi",
          "",
          if (r_squared >= 0.7) {
            "1. **Model Performance:** Model memiliki performa yang sangat baik dan dapat digunakan untuk prediksi"
          } else if (r_squared >= 0.5) {
            "1. **Model Performance:** Model memiliki performa yang baik, pertimbangkan penambahan variabel lain"
          } else {
            "1. **Model Performance:** Model memiliki performa yang lemah, perlu re-spesifikasi model"
          },
          
          if (!is.null(residual_tests$shapiro) && residual_tests$shapiro$p.value <= 0.05) {
            "2. **Normalitas:** Pertimbangkan transformasi variabel atau robust regression"
          } else {
            "2. **Normalitas:** Asumsi normalitas terpenuhi"
          },
          
          if (!is.null(hetero_tests$breusch_pagan) && hetero_tests$breusch_pagan$p.value <= 0.05) {
            "3. **Heteroskedastisitas:** Gunakan robust standard errors atau weighted least squares"
          } else {
            "3. **Homoskedastisitas:** Asumsi varians konstan terpenuhi"
          },
          
          if (!is.null(vif_values) && max(vif_values) >= 5) {
            "4. **Multikolinearitas:** Pertimbangkan ridge regression, PCA, atau eliminasi variabel"
          } else {
            "4. **Multikolinearitas:** Tidak ada masalah multikolinearitas"
          },
          
          "5. **Validasi Model:** Lakukan cross-validation untuk menguji stabilitas model",
          "6. **Interpretasi Praktis:** Fokus pada variabel dengan koefisien signifikan dan effect size yang bermakna",
          "",
          "---",
          "",
          "**Catatan:** Laporan ini dibuat secara otomatis oleh AXIS Dashboard.",
          paste("**Timestamp:** ", format(Sys.time(), "%d %B %Y, %H:%M:%S %Z")),
          "",
          "Untuk analisis lanjutan, pertimbangkan teknik regresi alternatif seperti ridge, lasso, atau polynomial regression."
        )
        
        # Generate comprehensive PDF
        result <- create_comprehensive_pdf_report(content_list, file, "Laporan Komprehensif Regresi Linear Berganda", values$processed_data)
        
        # Clean up temporary files
        for (plot_file in temp_plots) {
          if (file.exists(plot_file)) {
            unlink(plot_file)
          }
        }
        
        if (result != "Success") {
          safe_notification(paste("Error generating PDF:", result), "error")
        } else {
          safe_notification("PDF laporan regresi berhasil dibuat!", "success")
        }
      } else {
        safe_notification("Model regresi belum dibuat. Silakan jalankan analisis regresi terlebih dahulu.", "warning")
      }
    }
  )
  
  # Spatial analysis downloads
  output$download_map <- downloadHandler(
    filename = function() {
      paste("peta-spasial-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create static map image
    }
  )
  
  output$download_spatial_results <- downloadHandler(
    filename = function() {
      paste("hasil-analisis-spasial-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      cat("HASIL ANALISIS SPASIAL\n")
      cat("======================\n\n")
      # Add spatial analysis results
      sink()
    }
  )
  
  output$download_distance_matrix <- downloadHandler(
    filename = function() {
      paste("matriks-jarak-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$distance_data)) {
        write.csv(values$distance_data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_spatial_report <- downloadHandler(
    filename = function() {
      paste("laporan-spasial-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      content <- paste(
        "# Laporan Analisis Spasial",
        "",
        paste("**Tanggal:**", Sys.Date()),
        "",
        "## Pemetaan Spasial",
        "",
        "Visualisasi distribusi geografis variabel yang dianalisis.",
        "",
        "## Autokorelasi Spasial",
        "",
        "Analisis Moran's I untuk mendeteksi pola klaster atau dispersi spasial.",
        "",
        "## Interpretasi",
        "",
        "Implikasi temuan spasial untuk kebijakan dan intervensi.",
        sep = "\n"
      )
      
      create_pdf_report(content, file, "Laporan Analisis Spasial")
    }
  )
  
  
  # =================================================================== #
  # DOWNLOAD HANDLER UNTUK LAPORAN WORD KOMPREHENSIF (VERSI STABIL)
  # =================================================================== #
  
  output$download_word_report <- downloadHandler(
    filename = function() {
      paste("Laporan_Analisis_Komprehensif_AXIS_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # Tampilkan progress bar kepada pengguna
      progress <- shiny::Progress$new(session, min = 0, max = 1)
      on.exit(progress$close())
      progress$set(message = "Membuat Laporan Word...", value = 0)
      
      # Buat file Rmd sementara di direktori temporer
      tempReport <- file.path(tempdir(), "comprehensive_report.Rmd")
      
      # --- MEMPERSIAPKAN PARAMETER UNTUK LAPORAN ---
      # Ini membekukan nilai reaktif agar aman digunakan oleh rmarkdown
      params <- list(
        data = values$processed_data,
        
        # Informasi analisis yang telah dijalankan
        flags = list(
          data_loaded = values$data_loaded,
          normality_done = values$normality_done,
          homogeneity_done = values$homogeneity_done,
          mean_test_done = values$mean_test_done,
          anova_done = values$anova_done,
          regression_done = values$regression_done
        ),
        
        # Input yang relevan
        inputs = list(
          explore_variable = input$explore_variable,
          normality_variable = input$normality_variable,
          homogeneity_variable = input$homogeneity_variable,
          homogeneity_group = input$homogeneity_group
        ),
        
        # Hasil analisis yang akan ditampilkan
        results = list(
          current_test_result = values$current_test_result,
          anova_result = values$anova_result,
          posthoc_result = values$posthoc_result,
          regression_model = values$regression_model
        )
      )
      
      progress$set(value = 0.2, detail = "Menyiapkan parameter...")
      
      # --- KONTEN R MARKDOWN ---
      report_content <- c(
        "---",
        "title: 'Laporan Hasil Analisis Statistik'",
        "author: 'Dihasilkan oleh AXIS Dashboard'",
        "date: '`r format(Sys.Date(), \"%d %B %Y\")`'",
        "output: word_document", # Menghapus referensi ke template.docx
        "params:",
        "  data: NA",
        "  flags: NA",
        "  inputs: NA",
        "  results: NA",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4.5, dpi = 300)",
        "library(ggplot2)",
        "library(flextable)",
        "library(officer)",
        "library(dplyr)",
        "library(broom)", # Penting untuk tidy() dan glance()
        "set_flextable_defaults(font.family = 'Arial', font.size = 10, table.layout = 'autofit')",
        "```",
        "",
        "# Pendahuluan",
        "Dokumen ini menyajikan ringkasan komprehensif dari analisis statistik yang dilakukan menggunakan AXIS Dashboard. Setiap bagian merangkum temuan kunci, output statistik, visualisasi, dan Interpretasi untuk mendukung pengambilan keputusan berbasis bukti.",
        "",
        
        "<!-- Bagian EDA -->",
        "```{r eda_section, eval = params$flags$data_loaded && !is.null(params$inputs$explore_variable)}",
        "cat('\\n# 1. Analisis Data Eksplorasi (EDA)\\n')",
        "cat(paste('\\nVariabel yang dianalisis:', '**', params$inputs$explore_variable, '**\\n'))",
        "```",
        "```{r eda_stats, eval = params$flags$data_loaded && !is.null(params$inputs$explore_variable)}",
        "cat('\\n## 1.1. Statistik Deskriptif\\n')",
        "var_data <- params$data[[params$inputs$explore_variable]]",
        "if(is.numeric(var_data)) {",
        "  stats <- summary(var_data)",
        "  stats_df <- data.frame(Statistik = names(stats), Nilai = as.character(round(stats, 4)))",
        "  flextable(stats_df) %>% set_caption('Statistik Deskriptif') %>% theme_booktabs()",
        "} else {",
        "  stats_df <- as.data.frame(table(var_data))",
        "  colnames(stats_df) <- c('Kategori', 'Frekuensi')",
        "  flextable(stats_df) %>% set_caption('Tabel Frekuensi') %>% theme_booktabs()",
        "}",
        "```",
        "```{r eda_interp, eval = params$flags$data_loaded && !is.null(params$inputs$explore_variable)}",
        "cat('\\n## 1.2. Interpretasi\\n')",
        "cat('Statistik deskriptif di atas memberikan gambaran awal mengenai distribusi data. Untuk variabel numerik, perbandingan antara **mean** dan **median** dapat mengindikasikan kemiringan (skewness) distribusi. Untuk data kategorik, tabel frekuensi menunjukkan kategori mana yang paling dominan.')",
        "```",
        "",
        
        "<!-- Bagian Uji Asumsi -->",
        "```{r assumptions_section, eval = params$flags$normality_done || params$flags$homogeneity_done}",
        "cat('\\n# 2. Uji Asumsi\\n')",
        "```",
        "```{r normality_test_report, eval = params$flags$normality_done}",
        "cat('\\n## 2.1. Uji Normalitas\\n')",
        "var_data <- na.omit(params$data[[params$inputs$normality_variable]])",
        "test_result <- shapiro.test(var_data)",
        "cat(paste('Uji Shapiro-Wilk pada variabel', params$inputs$normality_variable, ': W =', round(test_result$statistic, 4), ', p-value =', format_pvalue(test_result$p.value)))",
        "cat('\\n**Interpretasi:** P-value > 0.05 mengindikasikan asumsi normalitas terpenuhi.')",
        "```",
        "```{r homogeneity_test_report, eval = params$flags$homogeneity_done}",
        "cat('\\n## 2.2. Uji Homogenitas Varians\\n')",
        "formula <- as.formula(paste(params$inputs$homogeneity_variable, '~', params$inputs$homogeneity_group))",
        "test_result <- car::leveneTest(formula, data = params$data)",
        "cat(paste('Uji Levene pada variabel', params$inputs$homogeneity_variable, 'oleh grup', params$inputs$homogeneity_group, ': F =', round(test_result$`F value`[1], 4), ', p-value =', format_pvalue(test_result$`Pr(>F)`[1])))",
        "cat('\\n**Interpretasi:** P-value > 0.05 menunjukkan asumsi homogenitas varians terpenuhi.')",
        "```",
        "",
        
        "<!-- Bagian Uji Beda Rata-rata -->",
        "```{r mean_test_section, eval = params$flags$mean_test_done}",
        "cat('\\n# 3. Uji Beda Rata-rata (Uji-t)\\n')",
        "print(params$results$current_test_result)",
        "cat('\\n**Interpretasi:** P-value yang lebih kecil dari 0.05 secara konvensional dianggap signifikan. **Confidence interval** yang tidak mencakup nol juga mendukung adanya perbedaan yang signifikan.')",
        "```",
        "",
        
        "<!-- Bagian ANOVA -->",
        "```{r anova_section, eval = params$flags$anova_done}",
        "cat('\\n# 4. Analysis of Variance (ANOVA)\\n')",
        "cat('\\n## 4.1. Tabel ANOVA\\n')",
        "anova_df <- broom::tidy(params$results$anova_result$model)",
        "flextable(anova_df) %>% theme_booktabs() %>% colformat_double(digits = 4)",
        "cat('\\n**Interpretasi:** P-value (`p.value`) < 0.05 menunjukkan bahwa setidaknya ada satu kelompok yang rata-ratanya berbeda secara signifikan.')",
        "```",
        "```{r posthoc_report, eval = params$flags$anova_done && params$results$anova_result$type == 'one_way' && broom::tidy(params$results$anova_result$model)$p.value[1] < 0.05}",
        "cat('\\n## 4.2. Uji Post-Hoc (Tukey HSD)\\n')",
        "posthoc_df <- as.data.frame(params$results$posthoc_result[[1]])",
        "flextable(posthoc_df) %>% theme_booktabs() %>% colformat_double(j = 'p adj', digits = 4)",
        "cat('\\n**Interpretasi:** Pasangan dengan `p adj` < 0.05 menunjukkan perbedaan yang signifikan secara statistik.')",
        "```",
        "",
        
        "<!-- Bagian Regresi -->",
        "```{r regression_section, eval = params$flags$regression_done}",
        "cat('\\n# 5. Regresi Linear Berganda\\n')",
        "cat('\\n## 5.1. Ringkasan Model\\n')",
        "glance_df <- broom::glance(params$results$regression_model)",
        "flextable(glance_df) %>% theme_booktabs() %>% colformat_double(digits = 4)",
        "cat('\\n**Interpretasi:** **R-squared** mengukur persentase variasi yang dijelaskan model. **p.value** dari F-statistik menguji signifikansi model secara keseluruhan.')",
        "cat('\\n## 5.2. Koefisien Model\\n')",
        "coeffs_df <- broom::tidy(params$results$regression_model)",
        "flextable(coeffs_df) %>% theme_booktabs() %>% colformat_double(digits = 4)",
        "cat('\\n**Interpretasi:** `p.value` untuk setiap `term` (variabel) menunjukkan signifikansi pengaruhnya terhadap variabel dependen.')",
        "```"
      )
      
      progress$set(value = 0.5, detail = "Membuat template laporan...")
      
      # Tulis konten ke file Rmd sementara
      writeLines(report_content, tempReport, useBytes = TRUE)
      
      # Render Rmd ke file Word (.docx)
      rmarkdown::render(
        input = tempReport, 
        output_file = file, 
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      progress$set(value = 1, detail = "Selesai!")
    }
  )
  
  
  
}