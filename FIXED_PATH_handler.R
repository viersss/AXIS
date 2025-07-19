# 🔥 DOWNLOAD HANDLER DENGAN AUTO PATH DETECTION

output$download_data_report <- downloadHandler(
  filename = function() {
    paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "🔥 MENCARI FILE RMD...", value = 0.1)
    
    tryCatch({
      # AUTO DETECT FILE RMD LOCATION
      rmd_file <- NULL
      
      # Cek di current directory
      if (file.exists("laporan_data_management.Rmd")) {
        rmd_file <- "laporan_data_management.Rmd"
      } else {
        # Cek di parent directory
        if (file.exists("../laporan_data_management.Rmd")) {
          rmd_file <- "../laporan_data_management.Rmd"
        } else {
          # Cari di semua subdirectory
          found_files <- list.files(recursive = TRUE, pattern = "laporan_data_management.Rmd", full.names = TRUE)
          if (length(found_files) > 0) {
            rmd_file <- found_files[1]
          }
        }
      }
      
      # Kalau file tidak ditemukan
      if (is.null(rmd_file)) {
        showNotification("❌ FILE RMD TIDAK DITEMUKAN! Pastikan laporan_data_management.Rmd ada di folder project.", type = "error")
        return(NULL)
      }
      
      progress$set(message = paste("📁 FOUND:", rmd_file), value = 0.2)
      
      # Prepare data
      temp_data_path <- tempfile(fileext = ".csv")
      write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
      progress$inc(0.2, detail = "Menyiapkan data CSV")
      
      # Set parameters
      params <- list(
        data_path = temp_data_path,
        analysis_date = Sys.Date()
      )
      
      # Copy RMD template ke temp directory
      temp_report <- file.path(tempdir(), "laporan_data_management.Rmd")
      file.copy(rmd_file, temp_report, overwrite = TRUE)
      
      progress$inc(0.3, detail = "🚀 RENDERING PDF...")
      
      # RENDER PDF!
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      progress$inc(0.3, detail = "✅ DONE!")
      
      # Notification
      if (file.exists(file)) {
        showNotification("✅ PDF BERHASIL DIBUAT! REAL PDF FILE!", type = "success")
      } else {
        showNotification("❌ GAGAL BUAT PDF!", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("ERROR:", e$message), type = "error")
      return(NULL)
    })
  },
  contentType = "application/pdf"
)