# AXIS - Advanced Exploratory Inference Statistics Dashboard
# Global Variables and Settings (global.R)
# FINAL VERSION - READY TO USE

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
if (requireNamespace("forecast", quietly = TRUE)) library(forecast)
if (requireNamespace("leaflet", quietly = TRUE)) library(leaflet)
if (requireNamespace("sf", quietly = TRUE)) library(sf)
if (requireNamespace("geojsonio", quietly = TRUE)) library(geojsonio)
if (requireNamespace("RColorBrewer", quietly = TRUE)) library(RColorBrewer)
if (requireNamespace("htmltools", quietly = TRUE)) library(htmltools)
if (requireNamespace("gridExtra", quietly = TRUE)) library(gridExtra)
if (requireNamespace("grid", quietly = TRUE)) library(grid)
if (requireNamespace("png", quietly = TRUE)) library(png)
if (requireNamespace("webshot", quietly = TRUE)) library(webshot)
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
if (requireNamespace("kableExtra", quietly = TRUE)) library(kableExtra)
if (requireNamespace("tinytex", quietly = TRUE)) library(tinytex)
if (requireNamespace("readxl", quietly = TRUE)) library(readxl)

# Set global options
options(shiny.maxRequestSize = 50*1024^2)  # 50MB max file size
options(warn = -1)  # Suppress warnings

# Install webshot if not available (for PDF generation)
if (requireNamespace("webshot", quietly = TRUE)) {
  if (!webshot::is_phantomjs_installed()) {
    tryCatch({
      webshot::install_phantomjs()
    }, error = function(e) {
      message("PhantomJS installation failed. PDF generation may not work properly.")
    })
  }
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

# Data quality assessment
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
  
  return(stats_list)
}

create_correlation_matrix <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  if (ncol(numeric_data) < 2) return(NULL)
  
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Create correlation plot using corrplot
  png_file <- tempfile(fileext = ".png")
  png(png_file, width = 800, height = 800, res = 150)
  if (requireNamespace("corrplot", quietly = TRUE)) {
    corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black",
             addCoef.col = "black", number.cex = 0.7,
             col = colorRampPalette(c("#C73E1D", "white", "#2E86AB"))(100),
             title = "Matriks Korelasi Antar Variabel")
  }
  dev.off()
  
  return(png_file)
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

# Session info for debugging
get_session_info <- function() {
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    packages = sessionInfo()$otherPkgs,
    timestamp = Sys.time()
  )
}