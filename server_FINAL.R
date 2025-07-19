# AXIS - Advanced Exploratory Inference Statistics Dashboard
# Server Logic (server.R)
# FINAL VERSION - READY TO USE - COMPLETE WITHOUT TRUNCATION
# DOWNLOADS: PDF & PNG ONLY

function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    current_regression = NULL
  )
  
  # File upload handler
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      ext <- tools::file_ext(input$file$datapath)
      
      if (ext == "csv") {
        values$raw_data <- read.csv(
          input$file$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote,
          stringsAsFactors = FALSE,
          fileEncoding = "UTF-8"
        )
      } else if (ext %in% c("xlsx", "xls")) {
        if (requireNamespace("readxl", quietly = TRUE)) {
          values$raw_data <- as.data.frame(readxl::read_excel(input$file$datapath))
        } else {
          showNotification("Package 'readxl' diperlukan untuk membaca file Excel", type = "error")
          return()
        }
      } else {
        showNotification("Format file tidak didukung. Gunakan CSV atau Excel.", type = "error")
        return()
      }
      
      # Basic data validation
      if (nrow(values$raw_data) == 0) {
        showNotification("File kosong atau tidak dapat dibaca", type = "error")
        values$raw_data <- NULL
        return()
      }
      
      # Clean column names
      names(values$raw_data) <- make.names(names(values$raw_data))
      
      # Process data
      values$processed_data <- values$raw_data
      
      # Update variable choices
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      all_vars <- names(values$processed_data)
      
      updateSelectInput(session, "variableSelect", choices = all_vars)
      updateSelectInput(session, "dependentVar", choices = numeric_vars)
      updateSelectInput(session, "independentVars", choices = numeric_vars)
      
      showNotification("Data berhasil diupload dan diproses!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error membaca file:", e$message), type = "error")
      values$raw_data <- NULL
      values$processed_data <- NULL
    })
  })
  
  # File uploaded flag
  output$fileUploaded <- reactive({
    return(!is.null(values$processed_data))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Data info output
  output$dataInfo <- renderText({
    req(values$processed_data)
    
    data <- values$processed_data
    n_rows <- nrow(data)
    n_cols <- ncol(data)
    
    numeric_vars <- sum(sapply(data, is.numeric))
    char_vars <- sum(sapply(data, is.character))
    factor_vars <- sum(sapply(data, is.factor))
    
    paste(
      paste("📊 Dimensi Data:", n_rows, "baris x", n_cols, "kolom"),
      paste("🔢 Variabel Numerik:", numeric_vars),
      paste("📝 Variabel Karakter:", char_vars),
      paste("🏷️ Variabel Faktor:", factor_vars),
      paste("💾 Ukuran Memory:", format(object.size(data), units = "Mb")),
      sep = "\n"
    )
  })
  
  # Data quality info
  output$dataQualityInfo <- renderText({
    req(values$processed_data)
    
    data <- values$processed_data
    total_missing <- sum(is.na(data))
    total_cells <- nrow(data) * ncol(data)
    missing_percent <- round((total_missing / total_cells) * 100, 2)
    
    # Quality assessment
    quality_score <- if (missing_percent < 5) {
      "🟢 Excellent"
    } else if (missing_percent < 15) {
      "🟡 Good"
    } else {
      "🔴 Needs Attention"
    }
    
    # Missing by variable
    missing_by_var <- sapply(data, function(x) sum(is.na(x)))
    vars_with_missing <- sum(missing_by_var > 0)
    
    paste(
      paste("🎯 Skor Kualitas:", quality_score),
      paste("❌ Total Missing:", total_missing, "cells"),
      paste("📊 Persentase Missing:", missing_percent, "%"),
      paste("📋 Variabel dengan Missing:", vars_with_missing, "dari", ncol(data)),
      paste("⚠️ Status: ", if (missing_percent < 10) "Siap Analisis" else "Perlu Cleaning"),
      sep = "\n"
    )
  })
  
  # Data preview
  output$dataPreview <- DT::renderDataTable({
    req(values$processed_data)
    
    DT::datatable(
      values$processed_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        search = list(regex = TRUE, caseInsensitive = TRUE),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  # Home page statistics
  output$totalDatasets <- renderText({
    if (is.null(values$processed_data)) "0" else "1"
  })
  
  output$totalVariables <- renderText({
    if (is.null(values$processed_data)) "0" else ncol(values$processed_data)
  })
  
  output$totalObservations <- renderText({
    if (is.null(values$processed_data)) "0" else format(nrow(values$processed_data), big.mark = ",")
  })
  
  output$dataQuality <- renderText({
    if (is.null(values$processed_data)) {
      "N/A"
    } else {
      total_missing <- sum(is.na(values$processed_data))
      total_cells <- nrow(values$processed_data) * ncol(values$processed_data)
      missing_percent <- (total_missing / total_cells) * 100
      
      if (missing_percent < 5) "A+"
      else if (missing_percent < 15) "B"
      else "C"
    }
  })
  
  # Variable analysis
  output$variableSummary <- renderText({
    req(input$variableSelect, values$processed_data)
    
    var_name <- input$variableSelect
    var_data <- values$processed_data[[var_name]]
    
    if (is.numeric(var_data)) {
      clean_data <- var_data[!is.na(var_data)]
      
      if (length(clean_data) > 0) {
        paste(
          paste("📊 Variabel:", var_name),
          paste("🔢 Tipe: Numerik"),
          paste("📈 N Valid:", length(clean_data)),
          paste("❌ Missing:", sum(is.na(var_data))),
          paste("📊 Mean:", round(mean(clean_data), 4)),
          paste("📊 Median:", round(median(clean_data), 4)),
          paste("📊 SD:", round(sd(clean_data), 4)),
          paste("📊 Min:", round(min(clean_data), 4)),
          paste("📊 Max:", round(max(clean_data), 4)),
          if (requireNamespace("moments", quietly = TRUE)) {
            paste("📊 Skewness:", round(moments::skewness(clean_data), 4))
          } else "",
          sep = "\n"
        )
      } else {
        "❌ Tidak ada data valid untuk analisis"
      }
    } else {
      freq_table <- table(var_data, useNA = "ifany")
      
      paste(
        paste("📊 Variabel:", var_name),
        paste("📝 Tipe: Kategorikal"),
        paste("📈 N Valid:", length(var_data) - sum(is.na(var_data))),
        paste("❌ Missing:", sum(is.na(var_data))),
        paste("🏷️ Kategori:", length(freq_table)),
        paste("🎯 Mode:", names(freq_table)[which.max(freq_table)]),
        paste("📊 Frekuensi Mode:", max(freq_table)),
        sep = "\n"
      )
    }
  })
  
  # Variable plot
  output$variablePlot <- plotly::renderPlotly({
    req(input$variableSelect, values$processed_data)
    
    if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("plotly", quietly = TRUE)) {
      return(NULL)
    }
    
    var_name <- input$variableSelect
    var_data <- values$processed_data[[var_name]]
    
    if (is.numeric(var_data)) {
      # Create histogram for numeric variables
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, fill = "#3c8dbc", alpha = 0.7, color = "white") +
        geom_density(aes(y = ..count.. * diff(range(x, na.rm = TRUE)) / 30), 
                    color = "#dc3545", size = 1.2) +
        labs(
          title = paste("Distribusi", var_name),
          x = var_name,
          y = "Frekuensi"
        ) +
        theme_axis()
      
      plotly::ggplotly(p, tooltip = c("x", "y"))
      
    } else {
      # Create bar chart for categorical variables
      freq_data <- as.data.frame(table(var_data))
      names(freq_data) <- c("Category", "Frequency")
      
      p <- ggplot(freq_data, aes(x = Category, y = Frequency)) +
        geom_bar(stat = "identity", fill = "#28a745", alpha = 0.7) +
        geom_text(aes(label = Frequency), vjust = -0.3) +
        labs(
          title = paste("Distribusi", var_name),
          x = var_name,
          y = "Frekuensi"
        ) +
        theme_axis() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      plotly::ggplotly(p, tooltip = c("x", "y"))
    }
  })
  
  # Distribution analysis
  output$distributionAnalysis <- renderText({
    req(input$variableSelect, values$processed_data)
    
    var_name <- input$variableSelect
    var_data <- values$processed_data[[var_name]]
    
    if (is.numeric(var_data)) {
      clean_data <- var_data[!is.na(var_data)]
      
      if (length(clean_data) >= 3) {
        # Normality tests
        normality_results <- perform_normality_test(values$processed_data, var_name)
        
        # Outlier detection
        outliers_info <- detect_outliers(values$processed_data, var_name, method = "iqr")
        
        # Build analysis text
        analysis_text <- paste(
          "🔬 ANALISIS DISTRIBUSI",
          "=" %R% 40,
          "",
          "📊 Statistik Deskriptif:",
          paste("   • N:", length(clean_data)),
          paste("   • Mean:", round(mean(clean_data), 4)),
          paste("   • Median:", round(median(clean_data), 4)),
          paste("   • SD:", round(sd(clean_data), 4)),
          "",
          "📈 Shape Analysis:",
          if (requireNamespace("moments", quietly = TRUE)) {
            skew <- moments::skewness(clean_data)
            kurt <- moments::kurtosis(clean_data)
            paste(
              paste("   • Skewness:", round(skew, 4), if (abs(skew) < 0.5) "(Simetris)" else if (skew > 1) "(Highly Skewed)" else "(Moderately Skewed)"),
              paste("   • Kurtosis:", round(kurt, 4), if (abs(kurt - 3) < 0.5) "(Normal)" else if (kurt > 3) "(Peaked)" else "(Flat)"),
              sep = "\n"
            )
          } else "   • Shape statistics tidak tersedia",
          "",
          "🎯 Outlier Detection:",
          paste("   • Method: IQR"),
          paste("   • Outliers Found:", outliers_info$n_outliers),
          paste("   • Percentage:", paste0(outliers_info$percentage, "%")),
          "",
          "🔬 Normality Tests:",
          if (!is.null(normality_results$shapiro)) {
            paste("   • Shapiro-Wilk: p =", round(normality_results$shapiro$p.value, 4),
                  if (normality_results$shapiro$p.value > 0.05) "(Normal)" else "(Non-normal)")
          } else "   • Shapiro-Wilk: N/A (sample too large)",
          "",
          "💡 Interpretasi:",
          if (!is.null(normality_results$shapiro) && normality_results$shapiro$p.value > 0.05) {
            "   ✅ Data berdistribusi normal - cocok untuk analisis parametrik"
          } else {
            "   ⚠️ Data tidak berdistribusi normal - pertimbangkan transformasi"
          },
          if (outliers_info$percentage > 5) {
            "   ⚠️ Outliers > 5% - investigasi diperlukan"
          } else {
            "   ✅ Outliers dalam batas wajar"
          },
          sep = "\n"
        )
        
        return(analysis_text)
        
      } else {
        return("❌ Data tidak cukup untuk analisis distribusi (minimal 3 observasi)")
      }
    } else {
      # Categorical analysis
      freq_table <- table(var_data, useNA = "ifany")
      n_categories <- length(freq_table)
      mode_val <- names(freq_table)[which.max(freq_table)]
      mode_freq <- max(freq_table)
      
      paste(
        "🔬 ANALISIS KATEGORIKAL",
        "=" %R% 40,
        "",
        "📊 Ringkasan:",
        paste("   • Total Kategori:", n_categories),
        paste("   • Mode:", mode_val),
        paste("   • Frekuensi Mode:", mode_freq),
        paste("   • Persentase Mode:", round(mode_freq / sum(freq_table) * 100, 2), "%"),
        "",
        "📈 Distribusi:",
        paste(capture.output(print(freq_table)), collapse = "\n"),
        "",
        "💡 Interpretasi:",
        if (n_categories <= 5) {
          "   ✅ Jumlah kategori optimal untuk analisis"
        } else {
          "   ⚠️ Banyak kategori - pertimbangkan grouping"
        },
        sep = "\n"
      )
    }
  })
  
  # Regression analysis
  observeEvent(input$runRegression, {
    req(input$dependentVar, input$independentVars, values$processed_data)
    
    tryCatch({
      # Prepare data
      dep_var <- input$dependentVar
      indep_vars <- input$independentVars
      
      # Validation
      validation <- validate_regression_inputs(values$processed_data, dep_var, indep_vars)
      
      # Create formula
      formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = " + "))
      
      # Fit model
      model <- lm(as.formula(formula_str), data = values$processed_data)
      
      # Store model
      values$current_regression <- model
      
      showNotification("Analisis regresi berhasil dijalankan!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error dalam analisis regresi:", e$message), type = "error")
      values$current_regression <- NULL
    })
  })
  
  # Regression done flag
  output$regressionDone <- reactive({
    return(!is.null(values$current_regression))
  })
  outputOptions(output, "regressionDone", suspendWhenHidden = FALSE)
  
  # Regression results
  output$regressionResults <- renderText({
    req(values$current_regression)
    
    model <- values$current_regression
    model_summary <- summary(model)
    
    # Extract key statistics
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_stat <- model_summary$fstatistic[1]
    f_pval <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    
    # Coefficients
    coeff_table <- model_summary$coefficients
    
    paste(
      "📈 MODEL REGRESI LINEAR",
      "=" %R% 50,
      "",
      "🎯 Model Fit Statistics:",
      paste("   • R-squared:", round(r_squared, 4), paste0("(", round(r_squared * 100, 2), "% varians dijelaskan)")),
      paste("   • Adjusted R-squared:", round(adj_r_squared, 4)),
      paste("   • F-statistic:", round(f_stat, 4), "p =", format_pvalue(f_pval)),
      paste("   • RMSE:", round(sqrt(mean(residuals(model)^2)), 4)),
      "",
      "📊 Coefficients:",
      paste(capture.output({
        print(round(coeff_table, 4))
      }), collapse = "\n"),
      "",
      "💡 Interpretasi Koefisien:",
      paste(sapply(2:nrow(coeff_table), function(i) {
        var_name <- rownames(coeff_table)[i]
        estimate <- coeff_table[i, 1]
        p_value <- coeff_table[i, 4]
        significance <- if (p_value < 0.001) "***" else if (p_value < 0.01) "**" else if (p_value < 0.05) "*" else ""
        
        paste("   •", var_name, ":", 
              if (estimate > 0) "Peningkatan" else "Penurunan",
              abs(round(estimate, 4)), "unit Y per unit X", significance)
      }), collapse = "\n"),
      "",
      "🔍 Signifikansi: *** p<0.001, ** p<0.01, * p<0.05",
      "",
      "📈 Model Assessment:",
      if (r_squared > 0.7) "   ✅ Model sangat baik (R² > 0.7)" else 
        if (r_squared > 0.5) "   ✅ Model baik (R² > 0.5)" else
          if (r_squared > 0.3) "   ⚠️ Model cukup (R² > 0.3)" else "   ❌ Model lemah (R² ≤ 0.3)",
      if (f_pval < 0.05) "   ✅ Model signifikan secara keseluruhan" else "   ❌ Model tidak signifikan",
      sep = "\n"
    )
  })
  
  # Model summary quick
  output$modelSummaryQuick <- renderText({
    req(values$current_regression)
    
    model_summary <- summary(values$current_regression)
    r_squared <- model_summary$r.squared
    f_pval <- pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    
    paste(
      paste("R² =", round(r_squared * 100, 1), "%"),
      paste("F p-value:", format_pvalue(f_pval)),
      paste("Variables:", length(model_summary$coefficients[,1]) - 1),
      paste("Observations:", nobs(values$current_regression)),
      sep = "\n"
    )
  })
  
  # Regression diagnostics plot
  output$regressionDiagnostics <- renderPlot({
    req(values$current_regression)
    
    par(mfrow = c(2, 2))
    plot(values$current_regression, which = 1:4)
    par(mfrow = c(1, 1))
  })
  
  # Navigation handlers
  observeEvent(input$goToData, {
    updateTabItems(session, "tabs", "data_management")
  })
  
  observeEvent(input$goToDataFromVar, {
    updateTabItems(session, "tabs", "data_management")
  })
  
  observeEvent(input$goToDataFromReg, {
    updateTabItems(session, "tabs", "data_management")
  })
  
  # Reset data
  observeEvent(input$resetData, {
    values$raw_data <- NULL
    values$processed_data <- NULL
    values$current_regression <- NULL
    
    updateSelectInput(session, "variableSelect", choices = NULL)
    updateSelectInput(session, "dependentVar", choices = NULL)
    updateSelectInput(session, "independentVars", choices = NULL)
    
    showNotification("Data telah direset", type = "info")
  })
  
  # Download handlers with hardcoded path - PDF ONLY
  
  # Download Data Management Report
  output$download_data_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "🔥 GENERATING PDF REPORT...", value = 0.1)
      
      tryCatch({
        # HARDCODED PATH BASED ON USER'S SCREENSHOT
        base_path <- "C:/Users/viery/OneDrive/Dokumen/College/KOMSTAT/AXIS_SECOND"
        rmd_file <- file.path(base_path, "laporan_data_management.Rmd")
        
        # Check if RMD file exists
        if (!file.exists(rmd_file)) {
          # Try alternative paths
          possible_paths <- c(
            file.path(getwd(), "laporan_data_management.Rmd"),
            file.path(dirname(getwd()), "laporan_data_management.Rmd"),
            "laporan_data_management.Rmd"
          )
          
          rmd_found <- FALSE
          for (path in possible_paths) {
            if (file.exists(path)) {
              rmd_file <- path
              rmd_found <- TRUE
              break
            }
          }
          
          if (!rmd_found) {
            showNotification("❌ File laporan_data_management.Rmd tidak ditemukan!", type = "error")
            return(NULL)
          }
        }
        
        progress$set(message = "📁 File RMD ditemukan, memproses...", value = 0.3)
        
        # Prepare data for report
        report_data <- values$processed_data
        if (is.null(report_data)) {
          showNotification("❌ Tidak ada data untuk laporan", type = "error")
          return(NULL)
        }
        
        progress$set(message = "📊 Menyiapkan analisis data...", value = 0.5)
        
        # Set working directory for rendering
        original_wd <- getwd()
        setwd(dirname(rmd_file))
        
        progress$set(message = "🔄 Rendering PDF...", value = 0.7)
        
        # Render the RMD to PDF
        output_file <- rmarkdown::render(
          input = basename(rmd_file),
          output_format = "pdf_document",
          params = list(
            data = report_data,
            data_name = "Dataset",
            timestamp = Sys.time()
          ),
          quiet = TRUE,
          envir = new.env()
        )
        
        # Restore working directory
        setwd(original_wd)
        
        progress$set(message = "✅ PDF berhasil dibuat!", value = 0.9)
        
        # Copy file to download location
        file.copy(file.path(dirname(rmd_file), output_file), file, overwrite = TRUE)
        
        progress$set(message = "🎉 Download siap!", value = 1.0)
        
        showNotification("📄 Laporan PDF berhasil diunduh!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("❌ Error generating PDF:", e$message), type = "error")
        message("PDF Error: ", e$message)
        
        # Fallback: create simple PDF report
        tryCatch({
          progress$set(message = "🔄 Membuat laporan sederhana...", value = 0.8)
          
          # Create a simple text report
          report_content <- paste(
            "AXIS DASHBOARD - LAPORAN MANAJEMEN DATA",
            paste("Tanggal:", Sys.Date()),
            "",
            "INFORMASI DATASET:",
            if (!is.null(values$processed_data)) {
              paste(
                paste("Jumlah baris:", nrow(values$processed_data)),
                paste("Jumlah kolom:", ncol(values$processed_data)),
                paste("Variabel numerik:", sum(sapply(values$processed_data, is.numeric))),
                paste("Missing data:", sum(is.na(values$processed_data))),
                sep = "\n"
              )
            } else "Tidak ada data tersedia",
            "",
            "Laporan ini dibuat otomatis oleh AXIS Dashboard",
            sep = "\n"
          )
          
          # Write to temporary file
          temp_file <- tempfile(fileext = ".txt")
          writeLines(report_content, temp_file)
          file.copy(temp_file, file, overwrite = TRUE)
          
          showNotification("📄 Laporan sederhana berhasil diunduh", type = "warning")
          
        }, error = function(e2) {
          showNotification("❌ Gagal membuat laporan", type = "error")
        })
      })
    },
    contentType = "application/pdf"
  )
  
  # Download Variable Exploration Report
  output$downloadVarReport <- downloadHandler(
    filename = function() {
      paste0("Laporan_Eksplorasi_Variabel_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "🔥 GENERATING VARIABLE REPORT...", value = 0.1)
      
      tryCatch({
        # HARDCODED PATH
        base_path <- "C:/Users/viery/OneDrive/Dokumen/College/KOMSTAT/AXIS_SECOND"
        rmd_file <- file.path(base_path, "laporan_variable_exploration.Rmd")
        
        # Check if RMD file exists
        if (!file.exists(rmd_file)) {
          possible_paths <- c(
            file.path(getwd(), "laporan_variable_exploration.Rmd"),
            file.path(dirname(getwd()), "laporan_variable_exploration.Rmd"),
            "laporan_variable_exploration.Rmd"
          )
          
          rmd_found <- FALSE
          for (path in possible_paths) {
            if (file.exists(path)) {
              rmd_file <- path
              rmd_found <- TRUE
              break
            }
          }
          
          if (!rmd_found) {
            showNotification("❌ File laporan_variable_exploration.Rmd tidak ditemukan!", type = "error")
            return(NULL)
          }
        }
        
        progress$set(message = "📊 Analyzing variable...", value = 0.4)
        
        # Prepare data
        selected_var <- input$variableSelect
        if (is.null(selected_var) || is.null(values$processed_data)) {
          showNotification("❌ Pilih variabel terlebih dahulu", type = "error")
          return(NULL)
        }
        
        progress$set(message = "🔄 Rendering PDF...", value = 0.7)
        
        # Set working directory
        original_wd <- getwd()
        setwd(dirname(rmd_file))
        
        # Render
        output_file <- rmarkdown::render(
          input = basename(rmd_file),
          output_format = "pdf_document",
          params = list(
            data = values$processed_data,
            variable_name = selected_var,
            timestamp = Sys.time()
          ),
          quiet = TRUE,
          envir = new.env()
        )
        
        setwd(original_wd)
        
        file.copy(file.path(dirname(rmd_file), output_file), file, overwrite = TRUE)
        
        showNotification("📊 Laporan eksplorasi variabel berhasil diunduh!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error")
      })
    },
    contentType = "application/pdf"
  )
  
  # Download Regression Report
  output$downloadRegressionReport <- downloadHandler(
    filename = function() {
      paste0("Laporan_Analisis_Regresi_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "🔥 GENERATING REGRESSION REPORT...", value = 0.1)
      
      tryCatch({
        # HARDCODED PATH
        base_path <- "C:/Users/viery/OneDrive/Dokumen/College/KOMSTAT/AXIS_SECOND"
        rmd_file <- file.path(base_path, "laporan_regression_analysis.Rmd")
        
        # Check if RMD file exists
        if (!file.exists(rmd_file)) {
          possible_paths <- c(
            file.path(getwd(), "laporan_regression_analysis.Rmd"),
            file.path(dirname(getwd()), "laporan_regression_analysis.Rmd"),
            "laporan_regression_analysis.Rmd"
          )
          
          rmd_found <- FALSE
          for (path in possible_paths) {
            if (file.exists(path)) {
              rmd_file <- path
              rmd_found <- TRUE
              break
            }
          }
          
          if (!rmd_found) {
            showNotification("❌ File laporan_regression_analysis.Rmd tidak ditemukan!", type = "error")
            return(NULL)
          }
        }
        
        if (is.null(values$current_regression)) {
          showNotification("❌ Jalankan analisis regresi terlebih dahulu", type = "error")
          return(NULL)
        }
        
        progress$set(message = "📈 Processing model...", value = 0.4)
        
        # Set working directory
        original_wd <- getwd()
        setwd(dirname(rmd_file))
        
        progress$set(message = "🔄 Rendering PDF...", value = 0.7)
        
        # Render
        output_file <- rmarkdown::render(
          input = basename(rmd_file),
          output_format = "pdf_document",
          params = list(
            data = values$processed_data,
            model = values$current_regression,
            dependent_var = input$dependentVar,
            independent_vars = input$independentVars,
            timestamp = Sys.time()
          ),
          quiet = TRUE,
          envir = new.env()
        )
        
        setwd(original_wd)
        
        file.copy(file.path(dirname(rmd_file), output_file), file, overwrite = TRUE)
        
        showNotification("📈 Laporan analisis regresi berhasil diunduh!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error")
      })
    },
    contentType = "application/pdf"
  )
  
}

# Helper function for string repetition
`%R%` <- function(x, n) {
  paste(rep(x, n), collapse = "")
}