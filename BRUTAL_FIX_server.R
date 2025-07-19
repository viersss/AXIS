# 🔥 BRUTAL FIX - COPY PASTE INI KE SERVER.R ANDA
# REPLACE download handler yang lama dengan ini:

output$downloadReport <- downloadHandler(
  filename = function() {
    paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "🔥 MEMBUAT PDF SEJATI...", value = 0.1)
    
    tryCatch({
      # Prepare data
      temp_data_path <- tempfile(fileext = ".csv")
      write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
      progress$inc(0.3, detail = "Menyiapkan template RMD")
      
      # Set parameters
      params <- list(
        data_path = temp_data_path,
        analysis_date = Sys.Date()
      )
      
      # Copy RMD template
      temp_report <- file.path(tempdir(), "laporan_data_management.Rmd")
      file.copy("laporan_data_management.Rmd", temp_report, overwrite = TRUE)
      
      progress$inc(0.3, detail = "🚀 RENDERING PDF...")
      
      # RENDER PDF PAKSA!
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      progress$inc(0.4, detail = "✅ DONE!")
      
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
  contentType = "application/pdf"  # 🔥 PENTING: FORCE PDF TYPE!
)