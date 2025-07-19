# 🛡️ SAFE DOWNLOAD HANDLER - TIDAK AKAN ERROR

output$download_data_report <- downloadHandler(
  filename = function() {
    paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "🔥 MEMBUAT PDF AMAN...", value = 0.1)
    
    tryCatch({
      # Cek apakah packages tersedia
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        showNotification("❌ Package rmarkdown tidak tersedia!", type = "error")
        return(NULL)
      }
      
      if (!requireNamespace("knitr", quietly = TRUE)) {
        showNotification("❌ Package knitr tidak tersedia!", type = "error") 
        return(NULL)
      }
      
      # AUTO DETECT FILE RMD LOCATION
      rmd_file <- NULL
      possible_locations <- c(
        "laporan_data_management.Rmd",
        "../laporan_data_management.Rmd",
        "./laporan_data_management.Rmd"
      )
      
      for (loc in possible_locations) {
        if (file.exists(loc)) {
          rmd_file <- loc
          break
        }
      }
      
      # Kalau masih tidak ketemu, cari recursive
      if (is.null(rmd_file)) {
        found_files <- list.files(recursive = TRUE, pattern = "laporan_data_management.Rmd", full.names = TRUE)
        if (length(found_files) > 0) {
          rmd_file <- found_files[1]
        }
      }
      
      # Kalau file tidak ditemukan
      if (is.null(rmd_file)) {
        showNotification("❌ FILE RMD TIDAK DITEMUKAN! Check console untuk debug info.", type = "error")
        # Debug info
        cat("=== DEBUG INFO ===\n")
        cat("Working directory:", getwd(), "\n")
        cat("Files in directory:", paste(list.files(), collapse = ", "), "\n")
        cat("Recursive .Rmd search:", paste(list.files(recursive = TRUE, pattern = "*.Rmd"), collapse = ", "), "\n")
        return(NULL)
      }
      
      progress$set(message = paste("📁 FOUND:", basename(rmd_file)), value = 0.2)
      
      # Prepare data dengan error handling
      if (is.null(values$processed_data)) {
        showNotification("❌ Tidak ada data untuk dianalisis!", type = "error")
        return(NULL)
      }
      
      temp_data_path <- tempfile(fileext = ".csv")
      write.csv(values$processed_data, temp_data_path, row.names = FALSE, fileEncoding = "UTF-8")
      progress$inc(0.2, detail = "Data CSV siap")
      
      # Set parameters yang SAFE
      params <- list(
        data_path = temp_data_path,
        analysis_date = Sys.Date()
      )
      
      # Copy RMD template ke temp directory
      temp_report <- file.path(tempdir(), "laporan_data_management.Rmd")
      copy_success <- file.copy(rmd_file, temp_report, overwrite = TRUE)
      
      if (!copy_success) {
        showNotification("❌ Gagal copy template RMD!", type = "error")
        return(NULL)
      }
      
      progress$inc(0.2, detail = "Template siap")
      
      # Set working directory untuk render
      old_wd <- getwd()
      setwd(tempdir())
      
      progress$inc(0.1, detail = "🚀 RENDERING PDF...")
      
      # RENDER PDF dengan error handling ketat
      render_result <- tryCatch({
        rmarkdown::render(
          input = basename(temp_report),
          output_file = basename(file),
          params = params,
          envir = new.env(parent = globalenv()),
          quiet = TRUE,
          clean = TRUE
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
        return(paste("RENDER_ERROR:", e$message))
      }, finally = {
        # Restore working directory
        setwd(old_wd)
      })
      
      progress$inc(0.3, detail = "✅ DONE!")
      
      # Check result
      if (render_result == "SUCCESS" && file.exists(file)) {
        showNotification("✅ PDF BERHASIL DIBUAT! REAL PDF FILE!", type = "success")
      } else {
        showNotification(paste("❌ RENDER GAGAL:", render_result), type = "error")
        # Debug output
        cat("=== RENDER DEBUG ===\n")
        cat("Render result:", render_result, "\n")
        cat("File exists:", file.exists(file), "\n")
        cat("Temp files:", paste(list.files(tempdir(), pattern = "*.pdf"), collapse = ", "), "\n")
      }
      
      # Cleanup
      unlink(temp_data_path)
      unlink(temp_report)
      
    }, error = function(e) {
      showNotification(paste("❌ ERROR FATAL:", e$message), type = "error")
      cat("=== FATAL ERROR DEBUG ===\n")
      cat("Error:", e$message, "\n")
      cat("Call:", deparse(e$call), "\n")
      return(NULL)
    })
  },
  contentType = "application/pdf"
)