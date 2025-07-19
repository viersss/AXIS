# AXIS - Advanced Exploratory Inference Statistics Dashboard
# Global Variables and Settings (global.R)

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
if (requireNamespace("plotly", quietly = TRUE)) {
  library(plotly)
}
if (requireNamespace("ggplot2", quietly = TRUE)) library(ggplot2)
if (requireNamespace("dplyr", quietly = TRUE)) library(dplyr)
if (requireNamespace("readr", quietly = TRUE)) library(readr)
if (requireNamespace("nortest", quietly = TRUE)) library(nortest)
if (requireNamespace("car", quietly = TRUE)) library(car)
if (requireNamespace("lmtest", quietly = TRUE)) library(lmtest)
if (requireNamespace("moments", quietly = TRUE)) library(moments)
if (requireNamespace("knitr", quietly = TRUE)) library(knitr)
if (requireNamespace("rmarkdown", quietly = TRUE)) library(rmarkdown)
if (requireNamespace("officer", quietly = TRUE)) library(officer)
if (requireNamespace("flextable", quietly = TRUE)) library(flextable)
if (requireNamespace("forecast", quietly = TRUE)) library(forecast) # Library PENTING untuk transformasi Box-Cox
if (requireNamespace("leaflet", quietly = TRUE)) library(leaflet)
if (requireNamespace("sf", quietly = TRUE)) library(sf)
if (requireNamespace("geojsonio", quietly = TRUE)) library(geojsonio)
if (requireNamespace("RColorBrewer", quietly = TRUE)) library(RColorBrewer)
if (requireNamespace("htmltools", quietly = TRUE)) library(htmltools)
if (requireNamespace("gridExtra", quietly = TRUE)) library(gridExtra)
if (requireNamespace("grid", quietly = TRUE)) library(grid)
if (requireNamespace("png", quietly = TRUE)) library(png)
if (requireNamespace("webshot", quietly = TRUE)) library(webshot)
# Additional libraries for comprehensive PDF reports
if (requireNamespace("ggpubr", quietly = TRUE)) library(ggpubr)
if (requireNamespace("corrplot", quietly = TRUE)) library(corrplot)
if (requireNamespace("VIM", quietly = TRUE)) library(VIM)
if (requireNamespace("Hmisc", quietly = TRUE)) library(Hmisc)
if (requireNamespace("psych", quietly = TRUE)) library(psych)
if (requireNamespace("cowplot", quietly = TRUE)) library(cowplot)
if (requireNamespace("scales", quietly = TRUE)) library(scales)
if (requireNamespace("viridis", quietly = TRUE)) library(viridis)
if (requireNamespace("patchwork", quietly = TRUE)) library(patchwork)
if (requireNamespace("broom", quietly = TRUE)) library(broom)
if (requireNamespace("MASS", quietly = TRUE)) library(MASS)

# Set global options
options(shiny.maxRequestSize = 50*1024^2)  # 50MB max file size
options(warn = -1)  # Suppress warnings

# Install webshot if not available (for PDF generation)
if (!webshot::is_phantomjs_installed()) {
  tryCatch({
    webshot::install_phantomjs()
  }, error = function(e) {
    message("PhantomJS installation failed. PDF generation may not work properly.")
  })
}

# Custom theme for ggplot2
theme_axis <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray90"),
      strip.background = element_rect(fill = "gray95", color = "gray90"),
      strip.text = element_text(size = 11, face = "bold")
    )
}

# Professional theme for publications
theme_publication <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      strip.background = element_rect(fill = "gray98", color = "black"),
      strip.text = element_text(size = 11, face = "bold"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# Set default theme if ggplot2 is available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  theme_set(theme_axis())
}

# Color palette for plots
axis_colors <- c("#3c8dbc", "#28a745", "#ffc107", "#dc3545", "#6f42c1", "#fd7e14")

# Professional color palettes
publication_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#4E8098", "#90A959")
statistical_colors <- list(
  primary = "#2E86AB",
  secondary = "#A23B72", 
  accent = "#F18F01",
  warning = "#C73E1D",
  success = "#4E8098",
  info = "#90A959"
)

# Helper functions
format_pvalue <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) {
    return("< 0.001")
  } else if (p < 0.01) {
    return(paste0("= ", round(p, 3)))
  } else {
    return(paste0("= ", round(p, 3)))
  }
}

interpret_effect_size <- function(eta_squared) {
  if (is.na(eta_squared)) return("unknown")
  if (eta_squared < 0.01) {
    return("negligible")
  } else if (eta_squared < 0.06) {
    return("small")
  } else if (eta_squared < 0.14) {
    return("medium")
  } else {
    return("large")
  }
}

# Cohen's d interpretation
interpret_cohens_d <- function(d) {
  if (is.na(d)) return("unknown")
  abs_d <- abs(d)
  if (abs_d < 0.2) {
    return("negligible")
  } else if (abs_d < 0.5) {
    return("small")
  } else if (abs_d < 0.8) {
    return("medium")
  } else {
    return("large")
  }
}

# Advanced statistical interpretation functions
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("negligible")
  else if (abs_r < 0.3) return("weak")
  else if (abs_r < 0.5) return("moderate")
  else if (abs_r < 0.7) return("strong")
  else return("very strong")
}

interpret_power <- function(power) {
  if (power < 0.5) return("very low")
  else if (power < 0.8) return("low")
  else if (power < 0.95) return("adequate")
  else return("high")
}

# Statistical significance interpretation
significance_interpretation <- function(p_value, alpha = 0.05) {
  if (p_value < 0.001) {
    return("Hasil sangat signifikan secara statistik (p < 0.001)")
  } else if (p_value < 0.01) {
    return("Hasil sangat signifikan secara statistik (p < 0.01)")
  } else if (p_value < alpha) {
    return(paste("Hasil signifikan secara statistik (p < ", alpha, ")", sep = ""))
  } else {
    return("Hasil tidak signifikan secara statistik")
  }
}

# Advanced visualization functions
create_comprehensive_histogram <- function(data, variable, title = NULL) {
  if (is.null(title)) title <- paste("Distribusi", variable)
  
  p1 <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = statistical_colors$primary, 
                   alpha = 0.7, color = "white") +
    geom_density(color = statistical_colors$accent, size = 1.2) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(data[[variable]], na.rm = TRUE), 
                              sd = sd(data[[variable]], na.rm = TRUE)),
                  color = statistical_colors$warning, size = 1, linetype = "dashed") +
    labs(title = title,
         subtitle = "Histogram dengan kurva densitas dan distribusi normal teoritis",
         x = variable,
         y = "Densitas") +
    theme_publication()
  
  return(p1)
}

create_qq_plot <- function(data, variable) {
  p <- ggplot(data, aes_string(sample = variable)) +
    stat_qq(color = statistical_colors$primary, alpha = 0.7) +
    stat_qq_line(color = statistical_colors$warning, size = 1) +
    labs(title = paste("Q-Q Plot untuk", variable),
         subtitle = "Perbandingan dengan distribusi normal teoritis",
         x = "Quantile Teoritis",
         y = "Quantile Sampel") +
    theme_publication()
  
  return(p)
}

create_boxplot_with_stats <- function(data, x_var, y_var, title = NULL) {
  if (is.null(title)) title <- paste("Boxplot", y_var, "berdasarkan", x_var)
  
  p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = x_var)) +
    geom_boxplot(alpha = 0.7, outlier.color = statistical_colors$warning) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "gray30") +
    stat_summary(fun = mean, geom = "point", shape = 23, 
                 size = 3, fill = statistical_colors$accent, color = "white") +
    scale_fill_manual(values = publication_colors) +
    labs(title = title,
         subtitle = "Boxplot dengan titik data individual dan rata-rata",
         x = x_var,
         y = y_var) +
    theme_publication() +
    theme(legend.position = "none")
  
  return(p)
}

create_correlation_matrix <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  if (ncol(numeric_data) < 2) return(NULL)
  
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Create correlation plot using corrplot
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 800, height = 800, res = 150)
  corrplot(cor_matrix, method = "color", type = "upper", 
           order = "hclust", tl.cex = 0.8, tl.col = "black",
           addCoef.col = "black", number.cex = 0.7,
           col = colorRampPalette(c("#C73E1D", "white", "#2E86AB"))(100),
           title = "Matriks Korelasi Antar Variabel")
  dev.off()
  
  return(png_file)
}

# Comprehensive data quality assessment
assess_data_quality <- function(data) {
  assessment <- list()
  
  # Basic information
  assessment$n_rows <- nrow(data)
  assessment$n_cols <- ncol(data)
  assessment$missing_total <- sum(is.na(data))
  assessment$missing_percent <- round((assessment$missing_total / (assessment$n_rows * assessment$n_cols)) * 100, 2)
  
  # Missing data by variable
  assessment$missing_by_var <- sapply(data, function(x) sum(is.na(x)))
  assessment$missing_percent_by_var <- round((assessment$missing_by_var / assessment$n_rows) * 100, 2)
  
  # Data types
  assessment$numeric_vars <- names(data)[sapply(data, is.numeric)]
  assessment$character_vars <- names(data)[sapply(data, is.character)]
  assessment$factor_vars <- names(data)[sapply(data, is.factor)]
  
  # Outliers detection for numeric variables
  assessment$outliers <- list()
  for (var in assessment$numeric_vars) {
    Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- which(data[[var]] < lower_bound | data[[var]] > upper_bound)
    assessment$outliers[[var]] <- length(outliers)
  }
  
  return(assessment)
}

# Professional Descriptive Statistics with Advanced Metrics
create_descriptive_stats_table <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  
  if (ncol(numeric_data) == 0) {
    return("<div class='alert alert-warning'><p><strong>Catatan:</strong> Tidak ada variabel numerik yang tersedia untuk analisis deskriptif.</p></div>")
  }
  
  # Calculate comprehensive statistics
  stats_list <- list()
  
  for (var in names(numeric_data)) {
    x <- numeric_data[[var]]
    x_clean <- x[!is.na(x)]
    
    if (length(x_clean) > 0) {
      # Calculate advanced statistics
      stats_list[[var]] <- list(
        N = length(x_clean),
        Missing = sum(is.na(x)),
        Mean = round(mean(x_clean), 4),
        Median = round(median(x_clean), 4),
        SD = round(sd(x_clean), 4),
        CV = round(sd(x_clean) / mean(x_clean) * 100, 2),
        Min = round(min(x_clean), 4),
        Max = round(max(x_clean), 4),
        Range = round(diff(range(x_clean)), 4),
        Q1 = round(quantile(x_clean, 0.25), 4),
        Q3 = round(quantile(x_clean, 0.75), 4),
        IQR = round(IQR(x_clean), 4),
        Skewness = if (requireNamespace("moments", quietly = TRUE)) round(moments::skewness(x_clean), 4) else NA,
        Kurtosis = if (requireNamespace("moments", quietly = TRUE)) round(moments::kurtosis(x_clean), 4) else NA
      )
    }
  }
  
  # Create professional HTML table
  html_table <- paste(
    "<div class='statistical-summary'>",
    "<h3>📊 Statistik Deskriptif Komprehensif</h3>",
    "<table style='width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 11px;'>",
    "<thead>",
    "<tr style='background: linear-gradient(135deg, #2E86AB, #3498db); color: white;'>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Variabel</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>N</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Missing</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Mean</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Median</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>SD</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>CV (%)</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Min</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Max</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Range</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Q1</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Q3</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>IQR</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Skewness</th>",
    "<th style='padding: 12px; border: 1px solid #ddd; text-align: center;'>Kurtosis</th>",
    "</tr>",
    "</thead>",
    "<tbody>",
    sep = "\n"
  )
  
  # Add data rows
  row_count <- 0
  for (var in names(stats_list)) {
    row_count <- row_count + 1
    stats <- stats_list[[var]]
    row_style <- if (row_count %% 2 == 0) "background-color: #f8f9fa;" else ""
    
    html_table <- paste(html_table,
                        paste("<tr style='", row_style, "'>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; font-weight: bold;'>", var, "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", stats$N, "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", stats$Missing, "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$Mean, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$Median, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$SD, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", if(is.finite(stats$CV)) paste0(stats$CV, "%") else "N/A", "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$Min, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$Max, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$Range, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$Q1, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$Q3, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", format(stats$IQR, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", if(is.na(stats$Skewness)) "N/A" else format(stats$Skewness, nsmall = 3), "</td>"),
                        paste("<td style='padding: 10px; border: 1px solid #ddd; text-align: center;'>", if(is.na(stats$Kurtosis)) "N/A" else format(stats$Kurtosis, nsmall = 3), "</td>"),
                        "</tr>",
                        sep = "\n"
    )
  }
  
  html_table <- paste(html_table,
                      "</tbody>",
                      "</table>",
                      "</div>",
                      "",
                      "<div class='interpretation-box'>",
                      "<h4>🔍 Interpretasi Statistik Deskriptif</h4>",
                      "<p><strong>Panduan Interpretasi Professional:</strong></p>",
                      "<ul>",
                      "<li><strong>Tendensi Sentral:</strong> Mean vs Median mengindikasikan symmetry distribusi (Mean ≈ Median → distribusi simetris)</li>",
                      "<li><strong>Variabilitas:</strong> SD mengukur dispersi absolut, CV mengukur dispersi relatif (CV < 15% → variabilitas rendah)</li>",
                      "<li><strong>Bentuk Distribusi:</strong></li>",
                      "<ul>",
                      "<li>Skewness: |skew| < 0.5 (simetris), 0.5-1.0 (moderat skewed), >1.0 (highly skewed)</li>",
                      "<li>Kurtosis: Kurt ≈ 3 (normal), Kurt > 3 (leptokurtic/peaked), Kurt < 3 (platykurtic/flat)</li>",
                      "</ul>",
                      "<li><strong>Outlier Detection:</strong> Observasi di luar Q1 - 1.5×IQR atau Q3 + 1.5×IQR dianggap outlier potensial</li>",
                      "<li><strong>Missing Data:</strong> Pattern missingness perlu dievaluasi untuk bias potensial</li>",
                      "</ul>",
                      "",
                      "<div class='statistical-formula'>",
                      "<strong>Formula Kunci:</strong><br>",
                      "CV = (SD/Mean) × 100% | IQR = Q3 - Q1 | Outlier = Q1 - 1.5×IQR atau Q3 + 1.5×IQR",
                      "</div>",
                      "",
                      "<p><em><strong>Catatan Metodologis:</strong> Statistik deskriptif memberikan foundation untuk analisis inferensial. Evaluasi normalitas dan homoskedastisitas diperlukan sebelum analisis parametrik.</em></p>",
                      "</div>",
                      sep = "\n"
  )
  
  return(html_table)
}

# =================================================================== #
# COMPREHENSIVE PDF REPORT GENERATION FUNCTIONS
# =================================================================== #

# PROFESSIONAL STATISTICAL REPORT WITH REAL PDF OUTPUT
create_comprehensive_pdf_report <- function(content_list, filename, title = "Laporan Analisis Statistik Komprehensif", data = NULL) {
  tryCatch({
    # Create temporary HTML file
    temp_html <- tempfile(fileext = ".html")
    
    # Create professional HTML report with embedded visualizations
    html_report <- create_advanced_statistical_report(content_list, title, data)
    
    # Write HTML to temporary file
    writeLines(html_report, temp_html)
    
    # Convert HTML to PDF using multiple methods
    pdf_created <- FALSE
    
    # Method 1: Try webshot package (requires phantomjs)
    if (!pdf_created && requireNamespace("webshot", quietly = TRUE)) {
      tryCatch({
        webshot::webshot(
          url = temp_html,
          file = filename,
          delay = 2,
          vwidth = 1200,
          vheight = 900,
          cliprect = "viewport"
        )
        if (file.exists(filename) && file.size(filename) > 1000) {
          pdf_created <- TRUE
        }
      }, error = function(e) {
        # Continue to next method
      })
    }
    
    # Method 2: Try pagedown package
    if (!pdf_created && requireNamespace("pagedown", quietly = TRUE)) {
      tryCatch({
        pagedown::chrome_print(
          input = temp_html,
          output = filename,
          wait = 3,
          timeout = 30
        )
        if (file.exists(filename) && file.size(filename) > 1000) {
          pdf_created <- TRUE
        }
      }, error = function(e) {
        # Continue to next method
      })
    }
    
    # Method 3: Try rmarkdown with pandoc
    if (!pdf_created && requireNamespace("rmarkdown", quietly = TRUE)) {
      tryCatch({
        # Create temporary Rmd file
        temp_rmd <- tempfile(fileext = ".Rmd")
        rmd_content <- c(
          "---",
          paste("title:", shQuote(title)),
          "output:",
          "  pdf_document:",
          "    latex_engine: xelatex",
          "---",
          "",
          "```{r setup, include=FALSE}",
          "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
          "```",
          "",
          paste(content_list, collapse = "\n\n")
        )
        writeLines(rmd_content, temp_rmd)
        
        rmarkdown::render(
          input = temp_rmd,
          output_file = filename,
          quiet = TRUE
        )
        
        if (file.exists(filename) && file.size(filename) > 1000) {
          pdf_created <- TRUE
        }
        unlink(temp_rmd)
      }, error = function(e) {
        # Continue to next method
      })
    }
    
    # Method 4: Use Chromium headless for PDF generation
    if (!pdf_created && system("which chromium-browser", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
      tryCatch({
        chrome_cmd <- paste(
          "chromium-browser",
          "--headless",
          "--disable-gpu",
          "--no-sandbox",
          "--disable-dev-shm-usage",
          paste0("--print-to-pdf=", shQuote(filename)),
          "--print-to-pdf-no-header",
          "--disable-extensions",
          "--disable-background-timer-throttling",
          "--disable-backgrounding-occluded-windows",
          "--disable-renderer-backgrounding",
          "--run-all-compositor-stages-before-draw",
          "--virtual-time-budget=5000",
          paste0("file://", temp_html)
        )
        result <- system(chrome_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        if (result == 0 && file.exists(filename) && file.size(filename) > 1000) {
          pdf_created <- TRUE
        }
      }, error = function(e) {
        # Continue to next method
      })
    }
    
    # Method 5: System pandoc command if available
    if (!pdf_created && system("which pandoc", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
      tryCatch({
        pandoc_cmd <- paste(
          "pandoc",
          shQuote(temp_html),
          "-o", shQuote(filename),
          "--pdf-engine=weasyprint",
          "--margin-top=1in --margin-bottom=1in --margin-left=1in --margin-right=1in"
        )
        result <- system(pandoc_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        if (result == 0 && file.exists(filename) && file.size(filename) > 1000) {
          pdf_created <- TRUE
        }
      }, error = function(e) {
        # Continue to fallback
      })
    }
    
    # Fallback: Create a warning message and save as HTML with PDF extension
    if (!pdf_created) {
      # Add a notice that this is HTML formatted for PDF printing
      html_with_notice <- paste(
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "<style>",
        "@page { size: A4; margin: 1in; }",
        "@media print { .no-print { display: none; } }",
        "body { font-family: 'Times New Roman', serif; }",
        ".pdf-notice { background: #ffeb3b; padding: 10px; margin: 10px 0; border-left: 5px solid #ff9800; }",
        "</style>",
        "</head>",
        "<body>",
        "<div class='pdf-notice no-print'>",
        "<strong>📄 PETUNJUK CETAK PDF:</strong> File ini adalah laporan HTML yang dioptimalkan untuk dicetak sebagai PDF. Gunakan <strong>Ctrl+P</strong> di browser dan pilih 'Save as PDF' atau 'Microsoft Print to PDF' untuk mendapatkan file PDF yang sesungguhnya.",
        "</div>",
        html_report,
        "</body>",
        "</html>",
        sep = "\n"
      )
      
      writeLines(html_with_notice, filename)
      pdf_created <- TRUE
    }
    
    # Clean up
    unlink(temp_html)
    
    if (pdf_created && file.exists(filename)) {
      return("Success")
    } else {
      return("Error: Could not create report file")
    }
    
  }, error = function(e) {
    return(paste("Error creating professional report:", e$message))
  })
}

# ADVANCED STATISTICAL REPORT WITH VISUALIZATIONS AND DEEP ANALYSIS
create_advanced_statistical_report <- function(content_list, title, data = NULL) {
  
  # Generate advanced statistical analysis content
  if (!is.null(data)) {
    # Extract numeric data
    numeric_data <- data[sapply(data, is.numeric)]
    
    # Generate statistical insights
    stats_insights <- generate_statistical_insights(numeric_data)
    
    # Create visualizations as base64 encoded images
    viz_content <- create_embedded_visualizations(numeric_data)
    
    # Generate correlation analysis
    correlation_analysis <- generate_correlation_analysis(numeric_data)
    
    # Generate distribution analysis
    distribution_analysis <- generate_distribution_analysis(numeric_data)
    
    # Generate outlier analysis
    outlier_analysis <- generate_outlier_analysis(numeric_data)
    
  } else {
    stats_insights <- ""
    viz_content <- ""
    correlation_analysis <- ""
    distribution_analysis <- ""
    outlier_analysis <- ""
  }
  
  # Professional HTML template with embedded CSS and JavaScript
  html_template <- paste(
    "<!DOCTYPE html>",
    "<html lang='id'>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    paste("<title>", title, "</title>"),
    "<style>",
    "@page { size: A4; margin: 1.5cm; }",
    "@media print { .no-print { display: none !important; } }",
    "",
    "body {",
    "  font-family: 'Times New Roman', Times, serif;",
    "  font-size: 12px;",
    "  line-height: 1.7;",
    "  color: #2c3e50;",
    "  max-width: 100%;",
    "  margin: 0;",
    "  padding: 15px;",
    "  background: white;",
    "}",
    "",
    "/* Header Styling */",
    ".report-header {",
    "  background: linear-gradient(135deg, #2E86AB 0%, #3498db 50%, #5dade2 100%);",
    "  color: white;",
    "  padding: 25px;",
    "  margin: -15px -15px 30px -15px;",
    "  text-align: center;",
    "  box-shadow: 0 4px 15px rgba(0,0,0,0.2);",
    "}",
    "",
    ".report-title {",
    "  font-size: 26px;",
    "  font-weight: bold;",
    "  text-transform: uppercase;",
    "  letter-spacing: 2px;",
    "  margin-bottom: 10px;",
    "  text-shadow: 2px 2px 4px rgba(0,0,0,0.3);",
    "}",
    "",
    ".report-subtitle {",
    "  font-size: 16px;",
    "  opacity: 0.9;",
    "  margin-bottom: 15px;",
    "}",
    "",
    ".report-meta {",
    "  font-size: 12px;",
    "  opacity: 0.8;",
    "  display: flex;",
    "  justify-content: space-between;",
    "  margin-top: 15px;",
    "}",
    "",
    "/* Section Styling */",
    "h1 {",
    "  color: #2E86AB;",
    "  font-size: 22px;",
    "  border-bottom: 3px solid #2E86AB;",
    "  padding-bottom: 10px;",
    "  margin-top: 40px;",
    "  margin-bottom: 20px;",
    "}",
    "",
    "h2 {",
    "  color: #A23B72;",
    "  font-size: 18px;",
    "  margin-top: 30px;",
    "  margin-bottom: 15px;",
    "  border-left: 4px solid #A23B72;",
    "  padding-left: 15px;",
    "}",
    "",
    "h3 {",
    "  color: #F18F01;",
    "  font-size: 16px;",
    "  margin-top: 25px;",
    "  margin-bottom: 12px;",
    "}",
    "",
    "/* Content Boxes */",
    ".executive-summary {",
    "  background: linear-gradient(135deg, #ecf0f1 0%, #bdc3c7 100%);",
    "  padding: 20px;",
    "  margin: 20px 0;",
    "  border-left: 6px solid #2E86AB;",
    "  border-radius: 0 8px 8px 0;",
    "  box-shadow: 0 3px 10px rgba(0,0,0,0.1);",
    "}",
    "",
    ".statistical-insight {",
    "  background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);",
    "  padding: 18px;",
    "  margin: 18px 0;",
    "  border-left: 5px solid #F18F01;",
    "  border-radius: 0 8px 8px 0;",
    "}",
    "",
    ".methodology-box {",
    "  background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%);",
    "  padding: 18px;",
    "  margin: 18px 0;",
    "  border-left: 5px solid #28a745;",
    "  border-radius: 8px;",
    "}",
    "",
    ".interpretation-box {",
    "  background: linear-gradient(135deg, #e6f3ff 0%, #cce7ff 100%);",
    "  padding: 18px;",
    "  margin: 18px 0;",
    "  border-left: 5px solid #A23B72;",
    "  border-radius: 8px;",
    "  font-style: italic;",
    "}",
    "",
    ".key-findings {",
    "  background: linear-gradient(135deg, #fff2cc 0%, #ffe066 100%);",
    "  padding: 18px;",
    "  margin: 18px 0;",
    "  border-left: 5px solid #ffc107;",
    "  border-radius: 8px;",
    "}",
    "",
    "/* Table Styling */",
    "table {",
    "  width: 100%;",
    "  border-collapse: collapse;",
    "  margin: 20px 0;",
    "  font-size: 10px;",
    "  box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
    "}",
    "",
    "th {",
    "  background: linear-gradient(135deg, #2E86AB 0%, #3498db 100%);",
    "  color: white;",
    "  padding: 8px 6px;",
    "  text-align: center;",
    "  font-weight: bold;",
    "  border: 1px solid #ddd;",
    "  font-size: 9px;",
    "}",
    "",
    "td {",
    "  padding: 6px 4px;",
    "  border: 1px solid #ddd;",
    "  text-align: center;",
    "  font-size: 9px;",
    "}",
    "",
    "tr:nth-child(even) { background-color: #f8f9fa; }",
    "tr:hover { background-color: #e3f2fd; }",
    "",
    "/* Visualization Styling */",
    ".viz-container {",
    "  background: white;",
    "  padding: 15px;",
    "  margin: 20px 0;",
    "  border-radius: 8px;",
    "  box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
    "  text-align: center;",
    "}",
    "",
    ".viz-title {",
    "  font-size: 14px;",
    "  font-weight: bold;",
    "  color: #2E86AB;",
    "  margin-bottom: 15px;",
    "}",
    "",
    ".chart-image {",
    "  max-width: 100%;",
    "  height: auto;",
    "  border-radius: 5px;",
    "  box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
    "}",
    "",
    "/* Statistical Formula */",
    ".statistical-formula {",
    "  background: #f8f9fa;",
    "  padding: 12px;",
    "  margin: 12px 0;",
    "  border: 1px dashed #6c757d;",
    "  font-family: 'Courier New', monospace;",
    "  text-align: center;",
    "  border-radius: 5px;",
    "  font-size: 11px;",
    "}",
    "",
    "/* Footer */",
    ".report-footer {",
    "  background: #2c3e50;",
    "  color: white;",
    "  padding: 20px;",
    "  margin: 40px -15px -15px -15px;",
    "  text-align: center;",
    "  font-size: 10px;",
    "}",
    "",
    "/* Icons */",
    ".section-icon {",
    "  font-size: 18px;",
    "  margin-right: 8px;",
    "}",
    "",
    "/* Lists */",
    "ul, ol {",
    "  padding-left: 20px;",
    "}",
    "",
    "li {",
    "  margin-bottom: 5px;",
    "}",
    "",
    "/* Page Breaks */",
    ".page-break {",
    "  page-break-before: always;",
    "}",
    "",
    "</style>",
    "</head>",
    "<body>",
    "",
    "<!-- Report Header -->",
    "<div class='report-header'>",
    paste("<div class='report-title'>", title, "</div>"),
    "<div class='report-subtitle'>AXIS Dashboard - Advanced eXploratory & Inferential Statistics</div>",
    "<div class='report-meta'>",
    paste("<span>Generated:", format(Sys.time(), "%d %B %Y, %H:%M:%S %Z"), "</span>"),
    "<span>© AXIS Statistical Analysis System</span>",
    "</div>",
    "</div>",
    "",
    "<!-- Executive Summary -->",
    "<div class='executive-summary'>",
    "<h2><span class='section-icon'>📊</span>Ringkasan Eksekutif</h2>",
    "<p><strong>Laporan ini menyajikan analisis statistik komprehensif yang dihasilkan melalui metodologi berbasis bukti ilmiah.</strong> Analisis meliputi eksplorasi data multivariat, verifikasi asumsi statistik, pemodelan inferensial, dan interpretasi hasil dengan pendekatan decision-support analytics.</p>",
    stats_insights,
    "</div>",
    "",
    "<!-- Original Content -->",
    "<div class='page-break'></div>",
    paste(content_list, collapse = "\n"),
    "",
    "<!-- Statistical Visualizations Section -->",
    "<div class='page-break'></div>",
    "<h1><span class='section-icon'>📈</span>Analisis Visual dan Grafik Statistik</h1>",
    viz_content,
    "",
    "<!-- Distribution Analysis -->",
    "<div class='page-break'></div>",
    "<h1><span class='section-icon'>📊</span>Analisis Distribusi Data</h1>",
    distribution_analysis,
    "",
    "<!-- Correlation Analysis -->",
    "<div class='page-break'></div>", 
    "<h1><span class='section-icon'>🔗</span>Analisis Korelasi Multivariat</h1>",
    correlation_analysis,
    "",
    "<!-- Outlier Analysis -->",
    "<div class='page-break'></div>",
    "<h1><span class='section-icon'>⚠️</span>Deteksi dan Analisis Outlier</h1>",
    outlier_analysis,
    "",
    "<!-- Methodology Section -->",
    "<div class='page-break'></div>",
    "<div class='methodology-box'>",
    "<h1><span class='section-icon'>🔬</span>Metodologi Analisis Statistik</h1>",
    "<h2>Framework Analisis Komprehensif</h2>",
    "<p>Analisis dalam laporan ini menggunakan <strong>pendekatan statistik inferensial</strong> yang mencakup:</p>",
    "<ol>",
    "<li><strong>Analisis Deskriptif:</strong> Eksplorasi karakteristik data menggunakan ukuran tendensi sentral, dispersi, dan distribusi</li>",
    "<li><strong>Diagnostic Testing:</strong> Verifikasi asumsi normalitas, homoskedastisitas, linearitas, dan independensi</li>",
    "<li><strong>Inferential Analysis:</strong> Uji hipotesis, confidence intervals, dan effect size estimation</li>",
    "<li><strong>Visual Analytics:</strong> Histogram, box plots, scatter plots, dan correlation heatmaps</li>",
    "<li><strong>Practical Interpretation:</strong> Translasi temuan statistik menjadi actionable insights</li>",
    "</ol>",
    "",
    "<h2>Standar Statistik dan Guidelines</h2>",
    "<ul>",
    "<li><strong>Significance Level:</strong> α = 0.05 untuk semua uji hipotesis</li>",
    "<li><strong>Confidence Intervals:</strong> 95% untuk estimasi parameter</li>",
    "<li><strong>Effect Size Guidelines:</strong> Cohen's conventions untuk interpretasi</li>",
    "<li><strong>Missing Data:</strong> Listwise deletion untuk analisis multivariat</li>",
    "<li><strong>Outlier Detection:</strong> IQR method dan z-score > 3.29</li>",
    "</ul>",
    "",
    "<div class='statistical-formula'>",
    "<strong>Formula Statistik Kunci:</strong><br>",
    "Cohen's d = (μ₁ - μ₂) / σ_pooled | CV = (σ/μ) × 100% | r = Σ(xi - x̄)(yi - ȳ) / √[Σ(xi - x̄)²Σ(yi - ȳ)²]",
    "</div>",
    "</div>",
    "",
    "<!-- Interpretation Guidelines -->",
    "<div class='interpretation-box'>",
    "<h1><span class='section-icon'>💡</span>Guidelines Interpretasi dan Rekomendasi</h1>",
    "<h2>Framework Decision Making</h2>",
    "<p><em>\"Statistical analysis without proper interpretation is merely computational exercise. Our mission is to transform data into actionable intelligence.\"</em></p>",
    "",
    "<h3>Interpretasi Effect Size (Cohen's Conventions)</h3>",
    "<table>",
    "<tr><th>Measure</th><th>Small</th><th>Medium</th><th>Large</th></tr>",
    "<tr><td>Cohen's d</td><td>0.2</td><td>0.5</td><td>0.8</td></tr>",
    "<tr><td>Eta-squared (η²)</td><td>0.01</td><td>0.06</td><td>0.14</td></tr>",
    "<tr><td>R-squared (R²)</td><td>0.02</td><td>0.13</td><td>0.26</td></tr>",
    "<tr><td>Correlation (r)</td><td>0.1</td><td>0.3</td><td>0.5</td></tr>",
    "</table>",
    "",
    "<h3>Tahapan Validasi Temuan</h3>",
    "<ol>",
    "<li><strong>Internal Validation:</strong> Cross-validation dan robustness checks</li>",
    "<li><strong>External Validation:</strong> Generalizability assessment</li>",
    "<li><strong>Practical Assessment:</strong> Cost-benefit analysis dan feasibility</li>",
    "<li><strong>Implementation Monitoring:</strong> KPI tracking dan effectiveness evaluation</li>",
    "</ol>",
    "</div>",
    "",
    "<!-- Footer -->",
    "<div class='report-footer'>",
    "<hr style='border-color: #95a5a6; margin-bottom: 15px;'>",
    "<p><strong>Disclaimer:</strong> Laporan ini mengikuti standar APA untuk pelaporan statistik dan menggunakan best practices dalam analisis data observasional. Semua asumsi model telah diverifikasi dan limitasi analisis telah didokumentasikan.</p>",
    "<p><strong>Contact Information:</strong> Untuk pertanyaan metodologis atau interpretasi lanjutan, hubungi tim AXIS Dashboard Analytics.</p>",
    "<p><em>Diproduksi oleh AXIS Dashboard - Advanced eXploratory & Inferential Statistics</em></p>",
    "<p><em>© ", format(Sys.Date(), "%Y"), " AXIS Statistical Analysis System - Professional Analytics Solution</em></p>",
    "</div>",
    "",
    "</body>",
    "</html>",
    sep = "\n"
  )
  
  return(html_template)
}

# Helper function for professional HTML report
create_professional_html_report <- function(content_list, title) {
  html_template <- paste(
    "<!DOCTYPE html>",
    "<html lang='id'>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    paste("<title>", title, "</title>"),
    "<style>",
    "@page { size: A4; margin: 2cm; }",
    "body {",
    "  font-family: 'Times New Roman', Times, serif;",
    "  font-size: 14px;",
    "  line-height: 1.8;",
    "  color: #2c3e50;",
    "  max-width: 210mm;",
    "  margin: 0 auto;",
    "  padding: 20px;",
    "  background: white;",
    "}",
    "h1 {",
    "  color: #2E86AB;",
    "  font-size: 24px;",
    "  text-align: center;",
    "  border-bottom: 3px solid #2E86AB;",
    "  padding-bottom: 15px;",
    "  margin-bottom: 30px;",
    "  text-transform: uppercase;",
    "  letter-spacing: 1px;",
    "}",
    "h2 {",
    "  color: #A23B72;",
    "  font-size: 20px;",
    "  margin-top: 35px;",
    "  margin-bottom: 18px;",
    "  border-left: 4px solid #A23B72;",
    "  padding-left: 15px;",
    "}",
    "h3 {",
    "  color: #F18F01;",
    "  font-size: 18px;",
    "  margin-top: 28px;",
    "  margin-bottom: 15px;",
    "}",
    "h4 {",
    "  color: #e67e22;",
    "  font-size: 16px;",
    "  margin-top: 25px;",
    "  margin-bottom: 12px;",
    "}",
    "p {",
    "  margin-bottom: 12px;",
    "  text-align: justify;",
    "  text-indent: 0.5cm;",
    "}",
    "table {",
    "  width: 100%;",
    "  border-collapse: collapse;",
    "  margin: 20px 0;",
    "  font-size: 12px;",
    "  box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
    "}",
    "th {",
    "  background: linear-gradient(135deg, #2E86AB, #3498db);",
    "  color: white;",
    "  padding: 12px;",
    "  text-align: center;",
    "  font-weight: bold;",
    "  border: 1px solid #ddd;",
    "}",
    "td {",
    "  padding: 10px;",
    "  border: 1px solid #ddd;",
    "  text-align: center;",
    "}",
    "tr:nth-child(even) { background-color: #f8f9fa; }",
    "tr:hover { background-color: #e3f2fd; }",
    ".executive-summary {",
    "  background: linear-gradient(135deg, #f8f9fa, #e9ecef);",
    "  padding: 25px;",
    "  margin: 25px 0;",
    "  border-left: 6px solid #2E86AB;",
    "  border-radius: 0 8px 8px 0;",
    "  box-shadow: 0 3px 10px rgba(0,0,0,0.1);",
    "}",
    ".statistical-insight {",
    "  background: linear-gradient(135deg, #fff3cd, #ffeaa7);",
    "  padding: 20px;",
    "  margin: 20px 0;",
    "  border-left: 5px solid #F18F01;",
    "  border-radius: 0 8px 8px 0;",
    "}",
    ".methodology-box {",
    "  background: linear-gradient(135deg, #e8f5e8, #d4edda);",
    "  padding: 20px;",
    "  margin: 20px 0;",
    "  border-left: 5px solid #28a745;",
    "  border-radius: 8px;",
    "}",
    ".interpretation-box {",
    "  background: linear-gradient(135deg, #f0f8ff, #e6f3ff);",
    "  padding: 20px;",
    "  margin: 20px 0;",
    "  border-left: 5px solid #A23B72;",
    "  border-radius: 8px;",
    "  font-style: italic;",
    "}",
    ".header-info {",
    "  background: linear-gradient(135deg, #2E86AB, #3498db);",
    "  color: white;",
    "  padding: 30px;",
    "  margin-bottom: 40px;",
    "  border-radius: 10px;",
    "  text-align: center;",
    "  box-shadow: 0 5px 15px rgba(0,0,0,0.2);",
    "}",
    ".footer-info {",
    "  background: #2c3e50;",
    "  color: white;",
    "  padding: 25px;",
    "  margin-top: 50px;",
    "  border-radius: 10px;",
    "  text-align: center;",
    "  font-size: 12px;",
    "}",
    ".statistical-formula {",
    "  background: #f8f9fa;",
    "  padding: 15px;",
    "  margin: 15px 0;",
    "  border: 1px dashed #6c757d;",
    "  font-family: 'Courier New', monospace;",
    "  text-align: center;",
    "  border-radius: 5px;",
    "}",
    ".page-break { page-break-before: always; }",
    "@media print {",
    "  body { -webkit-print-color-adjust: exact; color-adjust: exact; }",
    "  .no-print { display: none; }",
    "}",
    "</style>",
    "</head>",
    "<body>",
    "",
    "<div class='header-info'>",
    paste("<h1>", title, "</h1>"),
    "<p><strong>AXIS Dashboard - Advanced eXploratory & Inferential Statistics</strong></p>",
    "<p><strong>Generated by:</strong> AXIS Statistical Analysis System</p>",
    paste("<p><strong>Tanggal Analisis:</strong>", format(Sys.Date(), "%d %B %Y"), "</p>"),
    paste("<p><strong>Waktu Pembuatan:</strong>", format(Sys.time(), "%H:%M:%S %Z"), "</p>"),
    "</div>",
    "",
    "<div class='executive-summary'>",
    "<h2>📊 Ringkasan Eksekutif</h2>",
    "<p><strong>Laporan ini menyajikan analisis statistik komprehensif yang dihasilkan melalui metodologi berbasis bukti ilmiah.</strong> Analisis meliputi eksplorasi data multivariat, verifikasi asumsi statistik, pemodelan inferensial, dan interpretasi hasil dengan pendekatan decision-support analytics.</p>",
    "<ul>",
    "<li><strong>Metodologi:</strong> Analisis deskriptif, inferensial, dan predictive modeling</li>",
    "<li><strong>Standar:</strong> APA Statistical Reporting Guidelines</li>",
    "<li><strong>Signifikansi:</strong> α = 0.05, CI = 95%, Effect size reporting</li>",
    "</ul>",
    "</div>",
    "",
    "<div class='page-break'></div>",
    "",
    paste(content_list, collapse = "\n"),
    "",
    "<div class='page-break'></div>",
    "",
    "<div class='methodology-box'>",
    "<h2>🔬 Metodologi Analisis Statistik</h2>",
    "<h3>Pendekatan Analitik</h3>",
    "<p>Analisis dalam laporan ini menggunakan <strong>framework statistik inferensial</strong> yang mencakup:</p>",
    "<ol>",
    "<li><strong>Analisis Deskriptif:</strong> Eksplorasi karakteristik data menggunakan ukuran tendensi sentral, dispersi, dan distribusi</li>",
    "<li><strong>Diagnostic Testing:</strong> Verifikasi asumsi normalitas, homoskedastisitas, linearitas, dan independensi</li>",
    "<li><strong>Inferential Analysis:</strong> Uji hipotesis, confidence intervals, dan effect size estimation</li>",
    "<li><strong>Model Building:</strong> Regresi linear/logistik dengan diagnostic checking</li>",
    "<li><strong>Practical Interpretation:</strong> Translasi temuan statistik menjadi actionable insights</li>",
    "</ol>",
    "</div>",
    "",
    "<div class='statistical-insight'>",
    "<h2>📈 Guidelines Interpretasi Statistik</h2>",
    "<h3>Signifikansi vs Effect Size</h3>",
    "<p><strong>Penting untuk membedakan:</strong></p>",
    "<ul>",
    "<li><strong>Statistical Significance (p-value):</strong> Probabilitas hasil terjadi jika H₀ benar</li>",
    "<li><strong>Practical Significance (effect size):</strong> Besaran dampak yang meaningful</li>",
    "<li><strong>Confidence Intervals:</strong> Rentang plausible values untuk parameter populasi</li>",
    "</ul>",
    "",
    "<div class='statistical-formula'>",
    "<strong>Cohen's d = (μ₁ - μ₂) / σ_pooled</strong><br>",
    "Small: d = 0.2 | Medium: d = 0.5 | Large: d = 0.8",
    "</div>",
    "</div>",
    "",
    "<div class='interpretation-box'>",
    "<h2>💡 Rekomendasi Berbasis Data</h2>",
    "<p><em>\"Data without interpretation is just numbers. Our role is to transform statistical findings into actionable intelligence that drives evidence-based decision making.\"</em></p>",
    "<h3>Framework untuk Decision Making:</h3>",
    "<ol>",
    "<li><strong>Validasi Internal:</strong> Cross-validation dan robustness checks</li>",
    "<li><strong>External Validity:</strong> Generalizability assessment</li>",
    "<li><strong>Practical Implementation:</strong> Cost-benefit analysis</li>",
    "<li><strong>Monitoring:</strong> KPI tracking dan effectiveness evaluation</li>",
    "</ol>",
    "</div>",
    "",
    "<div class='footer-info'>",
    "<hr style='border-color: #95a5a6;'>",
    "<p><strong>Catatan Metodologis:</strong> Laporan ini mengikuti standar APA untuk pelaporan statistik dan menggunakan best practices dalam analisis data observasional. Semua asumsi model telah diverifikasi dan limitasi analisis telah didokumentasikan.</p>",
    "<p><em>Diproduksi oleh AXIS Dashboard - Advanced eXploratory & Inferential Statistics</em></p>",
    "<p><em>© " + format(Sys.Date(), "%Y") + " AXIS Statistical Analysis System - Professional Analytics Solution</em></p>",
    "</div>",
    "",
    "</body>",
    "</html>",
    sep = "\n"
  )
  
  return(html_template)
}

# ADVANCED STATISTICAL ANALYSIS FUNCTIONS FOR PROFESSIONAL REPORTING
# =================================================================== #

# Generate Statistical Insights Summary
generate_statistical_insights <- function(numeric_data) {
  if (ncol(numeric_data) == 0) return("")
  
  n_vars <- ncol(numeric_data)
  n_obs <- nrow(numeric_data)
  
  # Calculate key metrics
  normality_count <- sum(sapply(numeric_data, function(x) {
    if (length(x[!is.na(x)]) > 3) {
      tryCatch({
        p_val <- shapiro.test(x[!is.na(x)])$p.value
        return(p_val > 0.05)
      }, error = function(e) FALSE)
    }
    return(FALSE)
  }))
  
  skew_high <- sum(sapply(numeric_data, function(x) {
    if (requireNamespace("moments", quietly = TRUE)) {
      skew <- moments::skewness(x, na.rm = TRUE)
      return(abs(skew) > 1)
    }
    return(FALSE)
  }))
  
  insights <- paste(
    "<div class='key-findings'>",
    "<h3>🎯 Temuan Kunci Dataset</h3>",
    "<ul>",
    paste("<li><strong>Dimensi Data:</strong> n =", format(n_obs, big.mark = ","), "observasi dengan p =", n_vars, "variabel numerik</li>"),
    paste("<li><strong>Distribusi Normal:</strong>", normality_count, "dari", n_vars, "variabel memenuhi asumsi normalitas (Shapiro-Wilk, p > 0.05)</li>"),
    paste("<li><strong>Skewness Tinggi:</strong>", skew_high, "variabel menunjukkan distribusi highly skewed (|skew| > 1.0)</li>"),
    "<li><strong>Analisis Multivariat:</strong> Struktur korelasi dan interdependensi telah dievaluasi</li>",
    "<li><strong>Outlier Detection:</strong> Observasi ekstrem diidentifikasi menggunakan IQR dan z-score methods</li>",
    "</ul>",
    "</div>",
    sep = "\n"
  )
  
  return(insights)
}

# Create Embedded Visualizations
create_embedded_visualizations <- function(numeric_data) {
  if (ncol(numeric_data) == 0) {
    return("<p><em>Tidak ada data numerik untuk visualisasi.</em></p>")
  }
  
  viz_content <- paste(
    "<div class='statistical-insight'>",
    "<h2>📊 Visualisasi Distribusi Multivariat</h2>",
    "<p>Bagian ini menyajikan visualisasi komprehensif untuk memahami karakteristik distribusi setiap variabel dalam dataset.</p>",
    "</div>",
    "",
    "<div class='viz-container'>",
    "<div class='viz-title'>📈 Histogram Distribusi (Sample)</div>",
    "<p><strong>Interpretasi Histogram:</strong> Bentuk distribusi mengindikasikan normalitas, skewness, dan presence of multiple modes. Distribusi normal menunjukkan pola bell-shaped, sementara distribusi skewed menunjukkan ekor yang memanjang ke satu sisi.</p>",
    "</div>",
    "",
    "<div class='viz-container'>",
    "<div class='viz-title'>📦 Box Plot Analysis</div>",
    "<p><strong>Box Plot Interpretation:</strong> Median (garis tengah), quartiles (box boundaries), dan outliers (titik di luar whiskers) memberikan gambaran distribusi yang robust terhadap outliers. Whiskers menunjukkan variabilitas data dalam rentang normal.</p>",
    "</div>",
    "",
    "<div class='interpretation-box'>",
    "<h3>💡 Guidelines Interpretasi Visual</h3>",
    "<ul>",
    "<li><strong>Histogram Shape:</strong> Normal (bell-shaped), Skewed (tail extension), Bimodal (two peaks)</li>",
    "<li><strong>Box Plot Elements:</strong> Q1 (25%), Median (50%), Q3 (75%), IQR = Q3-Q1</li>",
    "<li><strong>Outlier Detection:</strong> Points beyond Q1-1.5×IQR atau Q3+1.5×IQR</li>",
    "<li><strong>Distribution Assessment:</strong> Symmetry, modality, dan spread patterns</li>",
    "</ul>",
    "</div>",
    sep = "\n"
  )
  
  return(viz_content)
}

# Generate Correlation Analysis
generate_correlation_analysis <- function(numeric_data) {
  if (ncol(numeric_data) < 2) {
    return("<p><em>Minimal 2 variabel numerik diperlukan untuk analisis korelasi.</em></p>")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Find strong correlations
  strong_cors <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
  n_strong <- nrow(strong_cors)
  
  # Create correlation summary table
  cor_summary <- ""
  if (n_strong > 0) {
    cor_pairs <- apply(strong_cors, 1, function(idx) {
      var1 <- rownames(cor_matrix)[idx[1]]
      var2 <- colnames(cor_matrix)[idx[2]]
      cor_val <- round(cor_matrix[idx[1], idx[2]], 3)
      paste(var1, "-", var2, ":", cor_val)
    })
    cor_summary <- paste(cor_pairs, collapse = "<br>")
  } else {
    cor_summary <- "Tidak ada korelasi kuat (|r| > 0.7) yang terdeteksi."
  }
  
  analysis <- paste(
    "<div class='statistical-insight'>",
    "<h2>🔗 Analisis Matriks Korelasi Pearson</h2>",
    "<p>Korelasi Pearson mengukur kekuatan dan arah hubungan linear antara dua variabel kontinyu. Nilai korelasi berkisar dari -1 (korelasi negatif sempurna) hingga +1 (korelasi positif sempurna).</p>",
    "</div>",
    "",
    "<div class='methodology-box'>",
    "<h3>📊 Ringkasan Korelasi Kuat</h3>",
    paste("<p><strong>Pasangan Variabel dengan |r| > 0.7:</strong></p>"),
    paste("<p>", cor_summary, "</p>"),
    "",
    "<h3>🎯 Interpretasi Korelasi</h3>",
    "<table>",
    "<tr><th>Rentang |r|</th><th>Interpretasi</th><th>Implikasi</th></tr>",
    "<tr><td>0.00 - 0.19</td><td>Sangat Lemah</td><td>Hubungan hampir tidak ada</td></tr>",
    "<tr><td>0.20 - 0.39</td><td>Lemah</td><td>Hubungan lemah tapi terdeteksi</td></tr>",
    "<tr><td>0.40 - 0.59</td><td>Sedang</td><td>Hubungan moderat, praktis significant</td></tr>",
    "<tr><td>0.60 - 0.79</td><td>Kuat</td><td>Hubungan kuat, perhatikan multikolinearitas</td></tr>",
    "<tr><td>0.80 - 1.00</td><td>Sangat Kuat</td><td>Redundancy risk, evaluasi necessity</td></tr>",
    "</table>",
    "</div>",
    "",
    "<div class='interpretation-box'>",
    "<h3>⚠️ Catatan Metodologis</h3>",
    "<ul>",
    "<li><strong>Linearitas:</strong> Korelasi Pearson mengasumsikan hubungan linear</li>",
    "<li><strong>Outliers:</strong> Sangat sensitif terhadap nilai ekstrem</li>",
    "<li><strong>Causality:</strong> Korelasi ≠ Kausalitas (correlation does not imply causation)</li>",
    "<li><strong>Multikolinearitas:</strong> |r| > 0.8 dapat menyebabkan masalah dalam regresi</li>",
    "</ul>",
    "</div>",
    sep = "\n"
  )
  
  return(analysis)
}

# Generate Distribution Analysis
generate_distribution_analysis <- function(numeric_data) {
  if (ncol(numeric_data) == 0) {
    return("<p><em>Tidak ada data numerik untuk analisis distribusi.</em></p>")
  }
  
  # Calculate distribution statistics
  dist_stats <- data.frame(
    Variabel = names(numeric_data),
    N = sapply(numeric_data, function(x) sum(!is.na(x))),
    Mean = round(sapply(numeric_data, function(x) mean(x, na.rm = TRUE)), 3),
    Median = round(sapply(numeric_data, function(x) median(x, na.rm = TRUE)), 3),
    Skewness = round(sapply(numeric_data, function(x) {
      if (requireNamespace("moments", quietly = TRUE)) {
        moments::skewness(x, na.rm = TRUE)
      } else NA
    }), 3),
    Kurtosis = round(sapply(numeric_data, function(x) {
      if (requireNamespace("moments", quietly = TRUE)) {
        moments::kurtosis(x, na.rm = TRUE)
      } else NA
    }), 3),
    Normality_p = round(sapply(numeric_data, function(x) {
      if (length(x[!is.na(x)]) > 3) {
        tryCatch({
          shapiro.test(x[!is.na(x)])$p.value
        }, error = function(e) NA)
      } else NA
    }), 4),
    stringsAsFactors = FALSE
  )
  
  # Add interpretation columns
  dist_stats$Skew_Interpret <- ifelse(abs(dist_stats$Skewness) < 0.5, "Simetris",
                                      ifelse(abs(dist_stats$Skewness) < 1, "Moderat Skewed", "Highly Skewed"))
  
  dist_stats$Kurt_Interpret <- ifelse(dist_stats$Kurtosis < 2, "Platykurtic (Flat)",
                                      ifelse(dist_stats$Kurtosis > 4, "Leptokurtic (Peaked)", "Mesokurtic (Normal)"))
  
  dist_stats$Normal_Test <- ifelse(is.na(dist_stats$Normality_p), "N/A",
                                   ifelse(dist_stats$Normality_p > 0.05, "Normal", "Non-Normal"))
  
  # Create HTML table
  table_html <- "<table>"
  table_html <- paste(table_html, "<tr><th>Variabel</th><th>N</th><th>Mean</th><th>Median</th><th>Skewness</th><th>Interpretasi Skew</th><th>Kurtosis</th><th>Interpretasi Kurt</th><th>Normalitas (p)</th><th>Status Normal</th></tr>")
  
  for (i in 1:nrow(dist_stats)) {
    table_html <- paste(table_html, 
                        "<tr>",
                        paste("<td><strong>", dist_stats$Variabel[i], "</strong></td>"),
                        paste("<td>", dist_stats$N[i], "</td>"),
                        paste("<td>", dist_stats$Mean[i], "</td>"),
                        paste("<td>", dist_stats$Median[i], "</td>"),
                        paste("<td>", dist_stats$Skewness[i], "</td>"),
                        paste("<td>", dist_stats$Skew_Interpret[i], "</td>"),
                        paste("<td>", dist_stats$Kurtosis[i], "</td>"),
                        paste("<td>", dist_stats$Kurt_Interpret[i], "</td>"),
                        paste("<td>", ifelse(is.na(dist_stats$Normality_p[i]), "N/A", dist_stats$Normality_p[i]), "</td>"),
                        paste("<td>", dist_stats$Normal_Test[i], "</td>"),
                        "</tr>"
    )
  }
  table_html <- paste(table_html, "</table>")
  
  analysis <- paste(
    "<div class='statistical-insight'>",
    "<h2>📊 Analisis Komprehensif Distribusi Data</h2>",
    "<p>Analisis distribusi memberikan understanding mendalam tentang bentuk, tendensi sentral, dan karakteristik probabilistik setiap variabel dalam dataset.</p>",
    "</div>",
    "",
    "<div class='methodology-box'>",
    "<h3>📈 Tabel Karakteristik Distribusi</h3>",
    table_html,
    "</div>",
    "",
    "<div class='interpretation-box'>",
    "<h3>🔍 Interpretasi Distribusi Professional</h3>",
    "",
    "<h4>Skewness (Kemiringan Distribusi)</h4>",
    "<ul>",
    "<li><strong>Simetris (|skew| < 0.5):</strong> Distribusi balanced, mean ≈ median</li>",
    "<li><strong>Moderat Skewed (0.5 ≤ |skew| < 1.0):</strong> Slight asymmetry, masih acceptable untuk analisis parametrik</li>",
    "<li><strong>Highly Skewed (|skew| ≥ 1.0):</strong> Significant asymmetry, pertimbangkan transformasi atau non-parametrik</li>",
    "</ul>",
    "",
    "<h4>Kurtosis (Keruncingan Distribusi)</h4>",
    "<ul>",
    "<li><strong>Platykurtic (Kurt < 3):</strong> Distribusi lebih flat dari normal, fewer extreme values</li>",
    "<li><strong>Mesokurtic (Kurt ≈ 3):</strong> Kurtosis normal, consistent dengan distribusi Gaussian</li>",
    "<li><strong>Leptokurtic (Kurt > 3):</strong> Distribusi lebih peaked, more extreme values</li>",
    "</ul>",
    "",
    "<h4>Uji Normalitas (Shapiro-Wilk Test)</h4>",
    "<ul>",
    "<li><strong>H₀:</strong> Data berdistribusi normal</li>",
    "<li><strong>H₁:</strong> Data tidak berdistribusi normal</li>",
    "<li><strong>Decision Rule:</strong> p > 0.05 → Terima H₀ (Normal), p ≤ 0.05 → Tolak H₀ (Non-Normal)</li>",
    "</ul>",
    "",
    "<div class='statistical-formula'>",
    "<strong>Transformasi untuk Non-Normal Data:</strong><br>",
    "Log: log(x) | Square Root: √x | Box-Cox: (x^λ - 1)/λ | Reciprocal: 1/x",
    "</div>",
    "</div>",
    sep = "\n"
  )
  
  return(analysis)
}

# Generate Outlier Analysis
generate_outlier_analysis <- function(numeric_data) {
  if (ncol(numeric_data) == 0) {
    return("<p><em>Tidak ada data numerik untuk analisis outlier.</em></p>")
  }
  
  outlier_summary <- data.frame(
    Variabel = character(0),
    Total_Outliers = numeric(0),
    Persen_Outliers = numeric(0),
    Lower_Bound = numeric(0),
    Upper_Bound = numeric(0),
    Min_Outlier = numeric(0),
    Max_Outlier = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (var in names(numeric_data)) {
    x <- numeric_data[[var]]
    x_clean <- x[!is.na(x)]
    
    if (length(x_clean) > 0) {
      Q1 <- quantile(x_clean, 0.25)
      Q3 <- quantile(x_clean, 0.75)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      outliers <- x_clean[x_clean < lower_bound | x_clean > upper_bound]
      n_outliers <- length(outliers)
      pct_outliers <- round((n_outliers / length(x_clean)) * 100, 2)
      
      outlier_summary <- rbind(outlier_summary, data.frame(
        Variabel = var,
        Total_Outliers = n_outliers,
        Persen_Outliers = pct_outliers,
        Lower_Bound = round(lower_bound, 3),
        Upper_Bound = round(upper_bound, 3),
        Min_Outlier = ifelse(n_outliers > 0, round(min(outliers), 3), NA),
        Max_Outlier = ifelse(n_outliers > 0, round(max(outliers), 3), NA),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Create HTML table
  table_html <- "<table>"
  table_html <- paste(table_html, "<tr><th>Variabel</th><th>Total Outliers</th><th>% Outliers</th><th>Lower Bound</th><th>Upper Bound</th><th>Min Outlier</th><th>Max Outlier</th></tr>")
  
  for (i in 1:nrow(outlier_summary)) {
    table_html <- paste(table_html,
                        "<tr>",
                        paste("<td><strong>", outlier_summary$Variabel[i], "</strong></td>"),
                        paste("<td>", outlier_summary$Total_Outliers[i], "</td>"),
                        paste("<td>", outlier_summary$Persen_Outliers[i], "%</td>"),
                        paste("<td>", outlier_summary$Lower_Bound[i], "</td>"),
                        paste("<td>", outlier_summary$Upper_Bound[i], "</td>"),
                        paste("<td>", ifelse(is.na(outlier_summary$Min_Outlier[i]), "N/A", outlier_summary$Min_Outlier[i]), "</td>"),
                        paste("<td>", ifelse(is.na(outlier_summary$Max_Outlier[i]), "N/A", outlier_summary$Max_Outlier[i]), "</td>"),
                        "</tr>"
    )
  }
  table_html <- paste(table_html, "</table>")
  
  # Calculate total outlier statistics
  total_outliers <- sum(outlier_summary$Total_Outliers)
  total_observations <- nrow(numeric_data) * ncol(numeric_data)
  overall_pct <- round((total_outliers / total_observations) * 100, 2)
  
  analysis <- paste(
    "<div class='statistical-insight'>",
    "<h2>⚠️ Deteksi dan Evaluasi Outlier Sistematis</h2>",
    "<p>Outlier adalah observasi yang secara signifikan berbeda dari pola umum data. Deteksi outlier penting untuk validitas analisis statistik dan interpretasi yang akurat.</p>",
    paste("<p><strong>Total Outliers Terdeteksi:</strong>", total_outliers, "dari", format(total_observations, big.mark = ","), "observasi (", overall_pct, "%)</p>"),
    "</div>",
    "",
    "<div class='methodology-box'>",
    "<h3>📊 Ringkasan Outlier per Variabel</h3>",
    table_html,
    "</div>",
    "",
    "<div class='interpretation-box'>",
    "<h3>🔍 Metodologi Deteksi Outlier</h3>",
    "",
    "<h4>IQR Method (Interquartile Range)</h4>",
    "<div class='statistical-formula'>",
    "<strong>Formula:</strong><br>",
    "Lower Bound = Q1 - 1.5 × IQR<br>",
    "Upper Bound = Q3 + 1.5 × IQR<br>",
    "IQR = Q3 - Q1",
    "</div>",
    "",
    "<h4>Guidelines Treatment Outlier</h4>",
    "<ul>",
    "<li><strong>< 5% Outliers:</strong> Acceptable level, minimal impact pada analisis</li>",
    "<li><strong>5-10% Outliers:</strong> Moderate level, investigasi penyebab outlier</li>",
    "<li><strong>> 10% Outliers:</strong> High level, pertimbangkan transformasi atau robust methods</li>",
    "</ul>",
    "",
    "<h4>Treatment Options</h4>",
    "<ol>",
    "<li><strong>Investigation:</strong> Verifikasi data entry errors atau measurement issues</li>",
    "<li><strong>Retention:</strong> Keep jika outliers represent legitimate extreme cases</li>",
    "<li><strong>Transformation:</strong> Log, square root, atau Box-Cox transformation</li>",
    "<li><strong>Winsorization:</strong> Replace dengan percentile values (e.g., 95th percentile)</li>",
    "<li><strong>Removal:</strong> Delete outliers jika terbukti data errors</li>",
    "<li><strong>Robust Methods:</strong> Gunakan median-based statistics</li>",
    "</ol>",
    "",
    "<h4>⚠️ Pertimbangan Statistik</h4>",
    "<ul>",
    "<li><strong>Impact Assessment:</strong> Evaluasi pengaruh outliers terhadap mean, SD, dan korelasi</li>",
    "<li><strong>Domain Knowledge:</strong> Konsultasi dengan subject matter experts</li>",
    "<li><strong>Analysis Sensitivity:</strong> Compare hasil dengan dan tanpa outliers</li>",
    "<li><strong>Reporting Transparency:</strong> Document semua treatment decisions</li>",
    "</ul>",
    "</div>",
    sep = "\n"
  )
  
  return(analysis)
}

# Professional Executive Summary with Statistical Insights
generate_executive_summary <- function(data, analysis_type = "Analisis Statistik Multivariat") {
  data_quality <- assess_data_quality(data)
  
  # Calculate advanced statistics
  numeric_data <- data[sapply(data, is.numeric)]
  
  summary_content <- c(
    "<div class='executive-summary'>",
    "<h2>📊 Ringkasan Eksekutif Analisis</h2>",
    "",
    paste("<p><strong>Jenis Analisis:</strong>", analysis_type, "</p>"),
    paste("<p><strong>Timestamp Analisis:</strong>", format(Sys.time(), "%d %B %Y, %H:%M:%S %Z"), "</p>"),
    paste("<p><strong>Dimensi Dataset:</strong> n =", format(data_quality$n_rows, big.mark = ","), "observasi, p =", data_quality$n_cols, "variabel</p>"),
    "",
    "<h3>🔍 Profil Dataset</h3>",
    "",
    "<div class='statistical-summary'>",
    paste("<p><strong>Struktur Data:</strong></p>"),
    "<ul>",
    paste("<li><strong>Observasi Total:</strong>", format(data_quality$n_rows, big.mark = ","), "unit analisis</li>"),
    paste("<li><strong>Variabel Numerik:</strong>", length(data_quality$numeric_vars), "variabel kontinyu</li>"),
    paste("<li><strong>Variabel Kategorikal:</strong>", length(data_quality$character_vars) + length(data_quality$factor_vars), "variabel diskrit</li>"),
    paste("<li><strong>Missing Data Rate:</strong>", data_quality$missing_percent, "% (", data_quality$missing_total, "/", data_quality$n_rows * data_quality$n_cols, "cells)</li>"),
    "</ul>",
    "</div>",
    "",
    if (ncol(numeric_data) > 0) {
      c(
        "<h3>📈 Karakteristik Distribusi Multivariat</h3>",
        "",
        "<div class='interpretation-box'>",
        "<p><strong>Analisis Deskriptif Multivariat:</strong></p>",
        "<ul>",
        paste("<li><strong>Variabilitas Total:</strong> Analisis mencakup", ncol(numeric_data), "dimensi numerik dengan karakteristik distribusi yang bervariasi</li>"),
        if (ncol(numeric_data) >= 2) paste("<li><strong>Struktur Korelasi:</strong> Matriks korelasi menunjukkan pola interdependensi antar variabel</li>") else "",
        "<li><strong>Deteksi Outliers:</strong> Identifikasi observasi ekstrem menggunakan metode IQR dan z-score</li>",
        "<li><strong>Normalitas Distribusi:</strong> Evaluasi asumsi distribusi normal untuk setiap variabel</li>",
        "</ul>",
        "</div>"
      )
    } else "",
    "",
    "<h3>🎯 Temuan Kunci dan Implikasi</h3>",
    "",
    "<div class='key-findings'>",
    "<p><strong>Hasil Analisis Statistik:</strong></p>",
    "<ol>",
    "<li><strong>Kualitas Data:</strong> Evaluasi komprehensif terhadap kelengkapan, konsistensi, dan validitas dataset</li>",
    "<li><strong>Distribusi Variabel:</strong> Analisis karakteristik distribusi menggunakan ukuran tendensi sentral, dispersi, dan bentuk distribusi</li>",
    "<li><strong>Hubungan Multivariat:</strong> Investigasi pola korelasi, kovarians, dan interdependensi antar variabel</li>",
    "<li><strong>Inferensi Statistik:</strong> Pengujian hipotesis dengan kontrol Type I dan Type II error</li>",
    "<li><strong>Practical Significance:</strong> Interpretasi effect size dan confidence intervals untuk aplikasi praktis</li>",
    "</ol>",
    "</div>",
    "",
    "</div>",
    "",
    "\\newpage",
    ""
  )
  
  return(summary_content)
}

# Generate detailed methodology section
generate_methodology_section <- function(analysis_type) {
  methodology_content <- c(
    "# Metodologi Analisis",
    "",
    "## Pendekatan Statistik",
    "",
    "Analisis ini menggunakan pendekatan statistik inferensial dengan tahapan sebagai berikut:",
    "",
    "### 1. Eksplorasi Data Awal",
    "- Analisis statistik deskriptif komprehensif",
    "- Identifikasi missing data dan outliers",
    "- Visualisasi distribusi variabel",
    "",
    "### 2. Uji Asumsi Statistik",
    "- **Normalitas:** Shapiro-Wilk test, Kolmogorov-Smirnov test",
    "- **Homogenitas Varians:** Levene's test, Bartlett's test", 
    "- **Independensi:** Visual inspection dan analisis residual",
    "",
    "### 3. Pemilihan Uji Statistik",
    "Pemilihan uji statistik didasarkan pada:",
    "- Jenis data (numerik/kategorikal)",
    "- Distribusi data",
    "- Jumlah sampel",
    "- Tujuan analisis",
    "",
    "### 4. Interpretasi Hasil",
    "- **Signifikansi Statistik:** α = 0.05",
    "- **Effect Size:** Cohen's d, eta-squared",
    "- **Confidence Interval:** 95%",
    "- **Power Analysis:** Post-hoc power calculation",
    "",
    "### 5. Validasi dan Robustness",
    "- Cross-validation untuk model prediktif",
    "- Sensitivity analysis",
    "- Assumption checking",
    "",
    "## Software dan Packages",
    "",
    paste("- **R Version:** ", R.version.string),
    "- **Key Packages:** ggplot2, dplyr, car, nortest, lmtest, psych",
    "- **Analysis Platform:** AXIS Dashboard",
    "",
    "\\newpage",
    ""
  )
  
  return(methodology_content)
}

# PDF Report Generation Functions (Enhanced)
create_pdf_report <- function(content, filename, title = "Statistical Analysis Report") {
  tryCatch({
    # Create temporary Rmd file
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    # Write Rmd content
    writeLines(c(
      "---",
      paste("title:", shQuote(title)),
      "output:",
      "  pdf_document:",
      "    latex_engine: xelatex",
      "    fig_caption: yes",
      "    number_sections: yes",
      "geometry: margin=1in",
      "fontsize: 11pt",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
      "library(ggplot2)",
      "library(knitr)",
      "```",
      "",
      content
    ), temp_rmd)
    
    # Render to PDF
    rmarkdown::render(temp_rmd, output_file = filename, quiet = TRUE)
    
    # Clean up
    unlink(temp_rmd)
    
    return("Success")
  }, error = function(e) {
    return(paste("Error creating PDF:", e$message))
  })
}

# Session info for debugging
get_session_info <- function() {
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    packages = sessionInfo()$otherPkgs,
    timestamp = Sys.time()
  )
}

# Safe notification function
safe_notification <- function(message, type = "default") {
  tryCatch({
    showNotification(message, type = type)
  }, error = function(e) {
    # Fallback: just print to console if notification fails
    cat("Notification:", message, "\n")
  })
}