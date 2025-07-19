# AXIS - Advanced Exploratory Inference Statistics Dashboard
# Server Logic (server.R)
# FINAL VERSION - READY TO USE
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
          values$raw_data <- readxl::read_excel(input$file$datapath)
        } else {
          showNotification("Package 'readxl' tidak tersedia. Install terlebih dahulu.", type = "error")
          return()
        }
      }
      
      values$processed_data <- values$raw_data
      showNotification("Data berhasil diupload!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error uploading file:", e$message), type = "error")
    })
  })
  
  # Check if file is uploaded
  output$fileUploaded <- reactive({
    return(!is.null(values$processed_data))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # Navigation to data management
  observeEvent(input$goToData, {
    updateTabItems(session, "tabs", "data_management")
  })
  
  # Reset data
  observeEvent(input$resetData, {
    values$raw_data <- NULL
    values$processed_data <- NULL
    values$current_regression <- NULL
    showNotification("Data telah direset!", type = "info")
  })
  
  # Update variable choices
  observe({
    if (!is.null(values$processed_data)) {
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      all_vars <- names(values$processed_data)
      
      updateSelectInput(session, "variableSelect", choices = all_vars)
      updateSelectInput(session, "dependentVar", choices = numeric_vars)
      updateSelectInput(session, "independentVars", choices = numeric_vars)
    }
  })
  
  # Value boxes for home page
  output$totalDatasets <- renderValueBox({
    valueBox(
      value = if (is.null(values$processed_data)) 0 else 1,
      subtitle = "Dataset Aktif",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$totalVariables <- renderValueBox({
    valueBox(
      value = if (is.null(values$processed_data)) 0 else ncol(values$processed_data),
      subtitle = "Total Variabel",
      icon = icon("list"),
      color = "green"
    )
  })
  
  output$totalObservations <- renderValueBox({
    valueBox(
      value = if (is.null(values$processed_data)) 0 else nrow(values$processed_data),
      subtitle = "Total Observasi",
      icon = icon("table"),
      color = "yellow"
    )
  })
  
  output$dataQuality <- renderValueBox({
    if (is.null(values$processed_data)) {
      quality_score <- 0
    } else {
      missing_percent <- sum(is.na(values$processed_data)) / (nrow(values$processed_data) * ncol(values$processed_data)) * 100
      quality_score <- max(0, 100 - missing_percent)
    }
    
    valueBox(
      value = paste0(round(quality_score, 1), "%"),
      subtitle = "Kualitas Data",
      icon = icon("check-circle"),
      color = if (quality_score >= 80) "green" else if (quality_score >= 60) "yellow" else "red"
    )
  })
  
  # Data information output
  output$dataInfo <- renderText({
    if (is.null(values$processed_data)) return("Belum ada data yang diupload.")
    
    data <- values$processed_data
    paste(
      "Dimensi Data:", nrow(data), "baris x", ncol(data), "kolom\n",
      "Variabel Numerik:", sum(sapply(data, is.numeric)), "\n",
      "Variabel Kategorikal:", sum(sapply(data, function(x) is.character(x) || is.factor(x))), "\n",
      "Total Missing Values:", sum(is.na(data)), "\n",
      "Ukuran File:", format(object.size(data), units = "Kb")
    )
  })
  
  # Data quality information
  output$dataQualityInfo <- renderText({
    if (is.null(values$processed_data)) return("Belum ada data untuk dianalisis.")
    
    assessment <- assess_data_quality(values$processed_data)
    
    paste(
      "Missing Data:", assessment$missing_percent, "%\n",
      "Variabel dengan Missing > 10%:", 
      sum(assessment$missing_percent_by_var > 10), "\n",
      "Total Outliers Terdeteksi:", 
      if (length(assessment$outliers) > 0) sum(unlist(assessment$outliers)) else 0, "\n",
      "Rekomendasi:", 
      if (assessment$missing_percent < 5) "Data berkualitas baik" 
      else if (assessment$missing_percent < 15) "Perlu cleaning ringan"
      else "Perlu cleaning intensif"
    )
  })
  
  # Data preview table
  output$dataPreview <- DT::renderDataTable({
    if (is.null(values$processed_data)) return(NULL)
    
    DT::datatable(
      values$processed_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = TRUE
      ),
      class = 'cell-border stripe'
    )
  })
  
  # Variable summary
  output$variableSummary <- renderText({
    if (is.null(values$processed_data) || is.null(input$variableSelect)) return("Pilih variabel untuk analisis.")
    
    var_data <- values$processed_data[[input$variableSelect]]
    
    if (is.numeric(var_data)) {
      paste(
        "Tipe Data: Numerik\n",
        "N:", length(var_data[!is.na(var_data)]), "\n",
        "Missing:", sum(is.na(var_data)), "\n",
        "Mean:", round(mean(var_data, na.rm = TRUE), 4), "\n",
        "Median:", round(median(var_data, na.rm = TRUE), 4), "\n",
        "Std Dev:", round(sd(var_data, na.rm = TRUE), 4), "\n",
        "Min:", round(min(var_data, na.rm = TRUE), 4), "\n",
        "Max:", round(max(var_data, na.rm = TRUE), 4)
      )
    } else {
      unique_vals <- length(unique(var_data[!is.na(var_data)]))
      paste(
        "Tipe Data: Kategorikal\n",
        "N:", length(var_data[!is.na(var_data)]), "\n",
        "Missing:", sum(is.na(var_data)), "\n",
        "Unique Values:", unique_vals, "\n",
        "Mode:", names(sort(table(var_data), decreasing = TRUE))[1]
      )
    }
  })
  
  # Variable plot
  output$variablePlot <- plotly::renderPlotly({
    if (is.null(values$processed_data) || is.null(input$variableSelect)) return(NULL)
    
    var_data <- values$processed_data[[input$variableSelect]]
    var_name <- input$variableSelect
    
    if (is.numeric(var_data)) {
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, fill = statistical_colors$primary, alpha = 0.7, color = "white") +
        labs(title = paste("Distribusi", var_name), x = var_name, y = "Frekuensi") +
        theme_axis()
    } else {
      freq_table <- table(var_data)
      df_plot <- data.frame(
        category = names(freq_table),
        frequency = as.numeric(freq_table)
      )
      
      p <- ggplot(df_plot, aes(x = reorder(category, -frequency), y = frequency)) +
        geom_bar(stat = "identity", fill = statistical_colors$secondary, alpha = 0.8) +
        labs(title = paste("Distribusi", var_name), x = var_name, y = "Frekuensi") +
        theme_axis() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    plotly::ggplotly(p)
  })
  
  # Distribution analysis
  output$distributionAnalysis <- renderText({
    if (is.null(values$processed_data) || is.null(input$variableSelect)) return("Pilih variabel untuk analisis.")
    
    var_data <- values$processed_data[[input$variableSelect]]
    
    if (is.numeric(var_data)) {
      var_clean <- var_data[!is.na(var_data)]
      
      # Normality tests
      shapiro_test <- if (length(var_clean) <= 5000) shapiro.test(var_clean) else NULL
      
      skewness_val <- if (requireNamespace("moments", quietly = TRUE)) {
        moments::skewness(var_clean)
      } else NA
      
      kurtosis_val <- if (requireNamespace("moments", quietly = TRUE)) {
        moments::kurtosis(var_clean)
      } else NA
      
      paste(
        "=== ANALISIS DISTRIBUSI ===\n",
        "Skewness:", round(skewness_val, 4), 
        if (!is.na(skewness_val)) {
          if (abs(skewness_val) < 0.5) "(Simetris)" 
          else if (skewness_val > 0) "(Right-skewed)" 
          else "(Left-skewed)"
        } else "", "\n",
        "Kurtosis:", round(kurtosis_val, 4), 
        if (!is.na(kurtosis_val)) {
          if (kurtosis_val < 3) "(Platykurtic)" 
          else if (kurtosis_val > 3) "(Leptokurtic)" 
          else "(Mesokurtic)"
        } else "", "\n",
        if (!is.null(shapiro_test)) {
          paste("Shapiro-Wilk Test p-value:", format_pvalue(shapiro_test$p.value), "\n",
                "Normalitas:", if (shapiro_test$p.value > 0.05) "Terdistribusi normal" else "Tidak normal")
        } else {
          "Shapiro-Wilk Test: N terlalu besar (>5000)"
        }
      )
    } else {
      freq_table <- table(var_data, useNA = "ifany")
      mode_val <- names(freq_table)[which.max(freq_table)]
      
      paste(
        "=== ANALISIS KATEGORIKAL ===\n",
        "Kategori Terbanyak:", mode_val, "\n",
        "Frekuensi Mode:", max(freq_table), "\n",
        "Distribusi:\n",
        paste(names(freq_table), ":", freq_table, collapse = "\n")
      )
    }
  })
  
  # Regression analysis
  observeEvent(input$runRegression, {
    req(input$dependentVar, input$independentVars)
    
    tryCatch({
      formula_str <- paste(input$dependentVar, "~", paste(input$independentVars, collapse = " + "))
      model <- lm(as.formula(formula_str), data = values$processed_data)
      values$current_regression <- model
      
      showNotification("Analisis regresi berhasil dijalankan!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error dalam regresi:", e$message), type = "error")
    })
  })
  
  # Regression results
  output$regressionResults <- renderText({
    if (is.null(values$current_regression)) return("Belum ada analisis regresi yang dijalankan.")
    
    model <- values$current_regression
    model_summary <- summary(model)
    
    paste(
      "=== HASIL REGRESI LINEAR ===\n",
      "Formula:", deparse(model$call$formula), "\n\n",
      "R-squared:", round(model_summary$r.squared, 4), "\n",
      "Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n",
      "F-statistic:", round(model_summary$fstatistic[1], 4), "\n",
      "p-value:", format_pvalue(pf(model_summary$fstatistic[1], 
                                 model_summary$fstatistic[2], 
                                 model_summary$fstatistic[3], 
                                 lower.tail = FALSE)), "\n\n",
      "=== KOEFISIEN ===\n",
      capture.output(print(round(model_summary$coefficients, 4))),
      collapse = "\n"
    )
  })
  
  # Regression diagnostics plot
  output$regressionDiagnostics <- renderPlot({
    if (is.null(values$current_regression)) return(NULL)
    
    model <- values$current_regression
    
    # Create diagnostic plots
    par(mfrow = c(2, 2))
    plot(model, which = 1:4)
  })
  
  # 🎯 DOWNLOAD HANDLERS - PDF & PNG ONLY
  
  # Download Data Management Report (PDF)
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
            quiet = FALSE
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
  
  # Download Variable Exploration Report (PDF)
  output$downloadVarReport <- downloadHandler(
    filename = function() {
      var_name <- if (!is.null(input$variableSelect)) input$variableSelect else "Variable"
      paste0("Laporan_Eksplorasi_", var_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "🔥 MEMBUAT LAPORAN VARIABEL PDF...", value = 0.1)
      
      tryCatch({
        base_path <- "C:/Users/viery/OneDrive/Dokumen/College/KOMSTAT/AXIS_SECOND"
        rmd_file <- file.path(base_path, "laporan_variable_exploration.Rmd")
        
        if (!file.exists(rmd_file)) {
          showNotification(paste("❌ FILE TIDAK ADA DI:", rmd_file), type = "error")
          return(NULL)
        }
        
        progress$set(message = "📁 FILE RMD FOUND!", value = 0.2)
        
        if (is.null(values$processed_data) || is.null(input$variableSelect)) {
          showNotification("❌ Tidak ada data atau variabel yang dipilih!", type = "error")
          return(NULL)
        }
        
        temp_data_path <- tempfile(fileext = ".csv")
        write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
        progress$inc(0.2, detail = "Data CSV siap")
        
        params <- list(
          data_path = temp_data_path,
          variable_name = input$variableSelect,
          analysis_date = Sys.Date()
        )
        
        temp_report <- file.path(tempdir(), "laporan_variable_exploration.Rmd")
        file.copy(rmd_file, temp_report, overwrite = TRUE)
        progress$inc(0.2, detail = "Template copied")
        
        old_wd <- getwd()
        setwd(tempdir())
        
        progress$inc(0.1, detail = "🚀 RENDERING PDF...")
        
        render_result <- tryCatch({
          rmarkdown::render(
            input = "laporan_variable_exploration.Rmd",
            output_file = basename(file),
            params = params,
            envir = new.env(parent = globalenv()),
            quiet = FALSE
          )
          
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
        
        if (render_result == "SUCCESS" && file.exists(file)) {
          showNotification("✅ LAPORAN VARIABEL PDF BERHASIL DIBUAT!", type = "success")
        } else {
          showNotification(paste("❌ RENDER GAGAL:", render_result), type = "error")
        }
        
        unlink(temp_data_path)
        
      }, error = function(e) {
        showNotification(paste("❌ ERROR:", e$message), type = "error")
        return(NULL)
      })
    },
    contentType = "application/pdf"
  )
  
  # Download Regression Analysis Report (PDF)
  output$downloadRegressionReport <- downloadHandler(
    filename = function() {
      paste0("Laporan_Analisis_Regresi_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "🔥 MEMBUAT LAPORAN REGRESI PDF...", value = 0.1)
      
      tryCatch({
        base_path <- "C:/Users/viery/OneDrive/Dokumen/College/KOMSTAT/AXIS_SECOND"
        rmd_file <- file.path(base_path, "laporan_regression_analysis.Rmd")
        
        if (!file.exists(rmd_file)) {
          showNotification(paste("❌ FILE TIDAK ADA DI:", rmd_file), type = "error")
          return(NULL)
        }
        
        progress$set(message = "📁 FILE RMD FOUND!", value = 0.2)
        
        if (is.null(values$current_regression)) {
          showNotification("❌ Belum ada analisis regresi yang dijalankan!", type = "error")
          return(NULL)
        }
        
        temp_data_path <- tempfile(fileext = ".csv")
        write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
        
        # Save regression model
        temp_model_path <- tempfile(fileext = ".rds")
        saveRDS(values$current_regression, temp_model_path)
        progress$inc(0.2, detail = "Data dan model siap")
        
        params <- list(
          data_path = temp_data_path,
          model_path = temp_model_path,
          dependent_var = input$dependentVar,
          independent_vars = input$independentVars,
          analysis_date = Sys.Date()
        )
        
        temp_report <- file.path(tempdir(), "laporan_regression_analysis.Rmd")
        file.copy(rmd_file, temp_report, overwrite = TRUE)
        progress$inc(0.2, detail = "Template copied")
        
        old_wd <- getwd()
        setwd(tempdir())
        
        progress$inc(0.1, detail = "🚀 RENDERING PDF...")
        
        render_result <- tryCatch({
          rmarkdown::render(
            input = "laporan_regression_analysis.Rmd",
            output_file = basename(file),
            params = params,
            envir = new.env(parent = globalenv()),
            quiet = FALSE
          )
          
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
        
        if (render_result == "SUCCESS" && file.exists(file)) {
          showNotification("✅ LAPORAN REGRESI PDF BERHASIL DIBUAT!", type = "success")
        } else {
          showNotification(paste("❌ RENDER GAGAL:", render_result), type = "error")
        }
        
        unlink(temp_data_path)
        unlink(temp_model_path)
        
      }, error = function(e) {
        showNotification(paste("❌ ERROR:", e$message), type = "error")
        return(NULL)
      })
    },
    contentType = "application/pdf"
  )
  
}