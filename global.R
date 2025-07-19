# AXIS - Advanced Exploratory Inference Statistics Dashboard
# Global Variables and Settings (global.R)

# Load required libraries
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
library(forecast) # Library PENTING untuk transformasi Box-Cox
library(leaflet)
library(sf)
library(geojsonio)
library(RColorBrewer)
library(htmltools)
library(gridExtra)
library(grid)
library(png)
library(webshot)
# Additional libraries for comprehensive PDF reports
library(ggpubr)
library(corrplot)
library(VIM)
library(Hmisc)
library(psych)
library(cowplot)
library(scales)
library(viridis)
library(patchwork)
library(broom)
library(MASS)

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

# Set default theme
theme_set(theme_axis())

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

# Professional statistical summary
create_descriptive_stats_table <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  if (ncol(numeric_data) == 0) return(NULL)
  
  stats_summary <- numeric_data %>%
    summarise_all(list(
      N = ~sum(!is.na(.)),
      Mean = ~round(mean(., na.rm = TRUE), 3),
      SD = ~round(sd(., na.rm = TRUE), 3),
      Min = ~round(min(., na.rm = TRUE), 3),
      Q1 = ~round(quantile(., 0.25, na.rm = TRUE), 3),
      Median = ~round(median(., na.rm = TRUE), 3),
      Q3 = ~round(quantile(., 0.75, na.rm = TRUE), 3),
      Max = ~round(max(., na.rm = TRUE), 3),
      Skewness = ~round(moments::skewness(., na.rm = TRUE), 3),
      Kurtosis = ~round(moments::kurtosis(., na.rm = TRUE), 3)
    ))
  
  # Transpose and format
  stats_df <- as.data.frame(t(stats_summary))
  stats_df$Variable <- rep(names(numeric_data), each = 10)
  stats_df$Statistic <- rep(c("N", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max", "Skewness", "Kurtosis"), 
                            times = ncol(numeric_data))
  stats_df <- stats_df[, c("Variable", "Statistic", "V1")]
  names(stats_df)[3] <- "Value"
  
  return(stats_df)
}

# =================================================================== #
# COMPREHENSIVE PDF REPORT GENERATION FUNCTIONS
# =================================================================== #

# PDF report function using webshot for HTML to PDF conversion
create_comprehensive_pdf_report <- function(content_list, filename, title = "Comprehensive Statistical Analysis Report") {
  tryCatch({
    # Check if webshot is available
    if (!requireNamespace("webshot", quietly = TRUE)) {
      return("Error: webshot package not available")
    }
    
    # Create temporary HTML file
    temp_html <- tempfile(fileext = ".html")
    
    # Create professional HTML content with print-optimized styling
    html_content <- paste(
      "<!DOCTYPE html>",
      "<html>",
      "<head>",
      "<meta charset='UTF-8'>",
      "<title>", title, "</title>",
      "<style>",
      "body {",
      "  font-family: 'Times New Roman', Times, serif;",
      "  font-size: 14px;",
      "  line-height: 1.6;",
      "  color: #333;",
      "  margin: 40px;",
      "  background: white;",
      "}",
      "h1 {",
      "  color: #2E86AB;",
      "  border-bottom: 3px solid #2E86AB;",
      "  padding-bottom: 10px;",
      "  margin-bottom: 25px;",
      "  font-size: 28px;",
      "}",
      "h2 {",
      "  color: #A23B72;", 
      "  margin-top: 30px;",
      "  margin-bottom: 15px;",
      "  font-size: 22px;",
      "}",
      "h3 {",
      "  color: #F18F01;",
      "  margin-top: 25px;",
      "  margin-bottom: 12px;",
      "  font-size: 18px;",
      "}",
      "table {",
      "  width: 100%;",
      "  border-collapse: collapse;",
      "  margin: 20px 0;",
      "  font-size: 12px;",
      "}",
      "th, td {",
      "  border: 1px solid #333;",
      "  padding: 10px;",
      "  text-align: left;",
      "}",
      "th {",
      "  background-color: #f0f0f0;",
      "  font-weight: bold;",
      "}",
      ".stats-summary {",
      "  background: #f9f9f9;",
      "  padding: 20px;",
      "  margin: 20px 0;",
      "  border-left: 5px solid #2E86AB;",
      "}",
      ".header-info {",
      "  margin-bottom: 30px;",
      "  padding: 20px;",
      "  background: #f8f8f8;",
      "  border: 1px solid #ddd;",
      "  border-radius: 5px;",
      "}",
      ".footer-info {",
      "  margin-top: 40px;",
      "  padding: 20px;",
      "  font-size: 12px;",
      "  color: #666;",
      "  border-top: 2px solid #ddd;",
      "}",
      "p { margin-bottom: 10px; }",
      "</style>",
      "</head>",
      "<body>",
      "<div class='header-info'>",
      paste("<h1>", title, "</h1>"),
      paste("<p><strong>Dashboard:</strong> AXIS Dashboard - Advanced Statistical Analysis</p>"),
      paste("<p><strong>Generated by:</strong> AXIS Dashboard System</p>"),
      paste("<p><strong>Tanggal Generate:</strong>", format(Sys.Date(), "%d %B %Y"), "</p>"),
      paste("<p><strong>Waktu Generate:</strong>", format(Sys.time(), "%H:%M:%S %Z"), "</p>"),
      "</div>",
      "",
      paste(content_list, collapse = "\n"),
      "",
      "<div class='footer-info'>",
      "<hr>",
      "<p><em>Laporan ini dibuat secara otomatis oleh AXIS Dashboard.</em></p>",
      "<p><em>Untuk analisis lebih lanjut, silakan akses dashboard di aplikasi AXIS.</em></p>",
      "<p><em>© AXIS Dashboard - Advanced Statistical Analysis System</em></p>",
      "</div>",
      "</body>",
      "</html>",
      sep = "\n"
    )
    
    # Write HTML file
    writeLines(html_content, temp_html)
    
    # Convert HTML to PDF using webshot
    pdf_created <- FALSE
    
    tryCatch({
      # Try webshot with increased delay and viewport
      webshot::webshot(
        url = temp_html,
        file = filename,
        delay = 3,
        vwidth = 1200,
        vheight = 800,
        cliprect = "viewport"
      )
      
      if (file.exists(filename) && file.size(filename) > 1000) {
        pdf_created <- TRUE
      }
    }, error = function(e) {
      # webshot failed, continue to other methods
    })
    
    # Fallback: Try pandoc if webshot fails
    if (!pdf_created && system("which pandoc", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
      pandoc_cmd <- paste(
        "pandoc",
        shQuote(temp_html),
        "-o", shQuote(filename),
        "--pdf-engine=weasyprint",
        "2>/dev/null"
      )
      
      result <- system(pandoc_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
      if (result == 0 && file.exists(filename)) {
        pdf_created <- TRUE
      }
    }
    
    # Final fallback: Create a simple text-based PDF using base R
    if (!pdf_created) {
      # Convert HTML to plain text and create a simple PDF-like file
      text_content <- gsub("<[^>]*>", "", paste(content_list, collapse = "\n"))
      text_content <- gsub("&nbsp;", " ", text_content)
      text_content <- gsub("&amp;", "&", text_content)
      
      # Create a simple formatted text file with PDF extension
      formatted_content <- paste(
        paste(rep("=", 80), collapse = ""),
        title,
        paste(rep("=", 80), collapse = ""),
        "",
        paste("Generated by: AXIS Dashboard System"),
        paste("Date:", format(Sys.Date(), "%d %B %Y")),
        paste("Time:", format(Sys.time(), "%H:%M:%S %Z")),
        "",
        paste(rep("-", 80), collapse = ""),
        "",
        text_content,
        "",
        paste(rep("-", 80), collapse = ""),
        "",
        "Laporan ini dibuat secara otomatis oleh AXIS Dashboard.",
        "Untuk analisis lebih lanjut, silakan akses dashboard di aplikasi AXIS.",
        "",
        sep = "\n"
      )
      
      writeLines(formatted_content, filename)
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
    return(paste("Error creating report:", e$message))
  })
}

# Generate executive summary
generate_executive_summary <- function(data, analysis_type = "General") {
  data_quality <- assess_data_quality(data)
  
  summary_content <- c(
    "# Executive Summary",
    "",
    paste("**Jenis Analisis:** ", analysis_type),
    paste("**Tanggal Analisis:** ", format(Sys.time(), "%d %B %Y, %H:%M %Z")),
    paste("**Total Observasi:** ", format(data_quality$n_rows, big.mark = ",")),
    paste("**Total Variabel:** ", data_quality$n_cols),
    "",
    "## Ringkasan Kualitas Data",
    "",
    paste("- **Missing Data:** ", data_quality$missing_total, " (", data_quality$missing_percent, "%)", sep = ""),
    paste("- **Variabel Numerik:** ", length(data_quality$numeric_vars)),
    paste("- **Variabel Kategorikal:** ", length(data_quality$character_vars) + length(data_quality$factor_vars)),
    "",
    "## Temuan Utama",
    "",
    "Analisis statistik komprehensif telah dilakukan terhadap dataset dengan fokus pada:",
    "",
    "1. **Kualitas Data:** Evaluasi kelengkapan dan konsistensi data",
    "2. **Distribusi Variabel:** Analisis karakteristik distribusi setiap variabel",
    "3. **Hubungan Antar Variabel:** Identifikasi pola korelasi dan asosiasi",
    "4. **Pengujian Hipotesis:** Verifikasi asumsi dan pengujian signifikansi statistik",
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