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