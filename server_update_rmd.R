# UPDATED DOWNLOAD HANDLERS FOR SERVER.R
# Replace the existing downloadReport handlers with these improved versions

# =================================================================== #
# DATA MANAGEMENT REPORT DOWNLOAD HANDLER
# =================================================================== #
output$downloadReport <- downloadHandler(
  filename = function() {
    paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Membuat Laporan PDF Komprehensif...", value = 0.1)
    
    tryCatch({
      # Prepare data
      temp_data_path <- tempfile(fileext = ".csv")
      write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
      progress$inc(0.3, detail = "Menyiapkan data dan template")
      
      # Set parameters for the report
      params <- list(
        data_path = temp_data_path,
        analysis_date = Sys.Date()
      )
      
      # Copy template to temp directory
      temp_report <- file.path(tempdir(), "laporan_data_management.Rmd")
      file.copy("laporan_data_management.Rmd", temp_report, overwrite = TRUE)
      progress$inc(0.3, detail = "Merender laporan PDF...")
      
      # Render the report
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      progress$inc(0.4, detail = "Selesai!")
      
      # Check if file is actually PDF or HTML
      if (file.exists(file)) {
        # Read first few bytes to determine file type
        file_header <- readBin(file, "raw", n = 4)
        if (length(file_header) >= 4 && rawToChar(file_header) == "%PDF") {
          safe_notification("✅ Laporan PDF Profesional berhasil dibuat! File siap untuk dibuka.", "success")
        } else {
          safe_notification("📄 Laporan telah dibuat dalam format HTML. Gunakan Ctrl+P → 'Save as PDF' untuk mendapatkan file PDF.", "info")
        }
      } else {
        safe_notification("❌ Gagal membuat laporan.", "error")
      }
      
    }, error = function(e) {
      safe_notification(
        paste("Gagal membuat PDF. Error:", e$message),
        "error"
      )
      return(NULL)
    })
  }
)

# =================================================================== #
# VARIABLE EXPLORATION REPORT DOWNLOAD HANDLER  
# =================================================================== #
output$downloadVarReport <- downloadHandler(
  filename = function() {
    selected_var <- input$variableSelect
    if (is.null(selected_var) || selected_var == "") {
      selected_var <- "Variable"
    }
    paste0("Laporan_Eksplorasi_", gsub("[^A-Za-z0-9]", "_", selected_var), "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Membuat Laporan Eksplorasi Variabel...", value = 0.1)
    
    tryCatch({
      # Get selected variable
      selected_var <- input$variableSelect
      if (is.null(selected_var) || selected_var == "" || !selected_var %in% names(values$processed_data)) {
        # Fallback to first numeric variable
        numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
        if (length(numeric_vars) > 0) {
          selected_var <- numeric_vars[1]
        } else {
          safe_notification("❌ Tidak ada variabel yang tersedia untuk analisis.", "error")
          return(NULL)
        }
      }
      
      # Prepare data
      temp_data_path <- tempfile(fileext = ".csv")
      write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
      progress$inc(0.3, detail = "Menganalisis variabel terpilih")
      
      # Set parameters for the report
      params <- list(
        data_path = temp_data_path,
        selected_variable = selected_var,
        analysis_date = Sys.Date()
      )
      
      # Copy template to temp directory
      temp_report <- file.path(tempdir(), "laporan_variable_exploration.Rmd")
      file.copy("laporan_variable_exploration.Rmd", temp_report, overwrite = TRUE)
      progress$inc(0.3, detail = "Merender laporan komprehensif...")
      
      # Render the report
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      progress$inc(0.4, detail = "Selesai!")
      
      # Check if file is actually PDF or HTML
      if (file.exists(file)) {
        file_header <- readBin(file, "raw", n = 4)
        if (length(file_header) >= 4 && rawToChar(file_header) == "%PDF") {
          safe_notification(paste("✅ Laporan Eksplorasi untuk variabel", selected_var, "berhasil dibuat!"), "success")
        } else {
          safe_notification("📄 Laporan telah dibuat dalam format HTML. Gunakan Ctrl+P → 'Save as PDF' untuk file PDF.", "info")
        }
      } else {
        safe_notification("❌ Gagal membuat laporan eksplorasi.", "error")
      }
      
    }, error = function(e) {
      safe_notification(
        paste("Gagal membuat laporan eksplorasi. Error:", e$message),
        "error"
      )
      return(NULL)
    })
  }
)

# =================================================================== #
# REGRESSION ANALYSIS REPORT DOWNLOAD HANDLER
# =================================================================== #
output$downloadRegressionReport <- downloadHandler(
  filename = function() {
    paste0("Laporan_Analisis_Regresi_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Membuat Laporan Analisis Regresi...", value = 0.1)
    
    tryCatch({
      # Get regression variables
      dependent_var <- input$dependentVar
      independent_vars <- input$independentVars
      
      # Validate variables
      if (is.null(dependent_var) || is.null(independent_vars) || 
          length(independent_vars) == 0 || dependent_var == "") {
        # Use fallback variables
        numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
        if (length(numeric_vars) >= 2) {
          dependent_var <- numeric_vars[1]
          independent_vars <- numeric_vars[2:min(4, length(numeric_vars))]
        } else {
          safe_notification("❌ Tidak cukup variabel numerik untuk analisis regresi.", "error")
          return(NULL)
        }
      }
      
      # Prepare data
      temp_data_path <- tempfile(fileext = ".csv")
      write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
      progress$inc(0.3, detail = "Melakukan analisis regresi")
      
      # Set parameters for the report
      params <- list(
        data_path = temp_data_path,
        dependent_var = dependent_var,
        independent_vars = paste(independent_vars, collapse = ","),
        analysis_date = Sys.Date()
      )
      
      # Copy template to temp directory
      temp_report <- file.path(tempdir(), "laporan_regression_analysis.Rmd")
      file.copy("laporan_regression_analysis.Rmd", temp_report, overwrite = TRUE)
      progress$inc(0.3, detail = "Merender laporan regresi...")
      
      # Render the report
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      progress$inc(0.4, detail = "Selesai!")
      
      # Check if file is actually PDF or HTML
      if (file.exists(file)) {
        file_header <- readBin(file, "raw", n = 4)
        if (length(file_header) >= 4 && rawToChar(file_header) == "%PDF") {
          safe_notification("✅ Laporan Analisis Regresi Komprehensif berhasil dibuat!", "success")
        } else {
          safe_notification("📄 Laporan telah dibuat dalam format HTML. Gunakan Ctrl+P → 'Save as PDF' untuk file PDF.", "info")
        }
      } else {
        safe_notification("❌ Gagal membuat laporan regresi.", "error")
      }
      
    }, error = function(e) {
      safe_notification(
        paste("Gagal membuat laporan regresi. Error:", e$message),
        "error"
      )
      return(NULL)
    })
  }
)