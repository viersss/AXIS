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

# Set default theme
theme_set(theme_axis())

# Color palette for plots
axis_colors <- c("#3c8dbc", "#28a745", "#ffc107", "#dc3545", "#6f42c1", "#fd7e14")

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

# Statistical interpretation functions
interpret_normality <- function(p_value, alpha = 0.05) {
  if (is.na(p_value)) return("Cannot determine")
  if (p_value > alpha) {
    return("Data follows normal distribution")
  } else {
    return("Data does not follow normal distribution")
  }
}

interpret_homogeneity <- function(p_value, alpha = 0.05) {
  if (is.na(p_value)) return("Cannot determine")
  if (p_value > alpha) {
    return("Variances are homogeneous")
  } else {
    return("Variances are not homogeneous")
  }
}

# Power analysis helper
calculate_power <- function(effect_size, n, alpha = 0.05) {
  if (is.na(effect_size) || is.na(n) || n <= 0) return(NA)
  # Simplified power calculation for t-test
  delta <- effect_size * sqrt(n/2)
  power <- 1 - pt(qt(1 - alpha/2, n-2), n-2, delta) + pt(qt(alpha/2, n-2), n-2, delta)
  return(power)
}

# Sample size calculation
calculate_sample_size <- function(effect_size, power = 0.8, alpha = 0.05) {
  if (is.na(effect_size) || effect_size <= 0) return(NA)
  # Simplified sample size calculation
  z_alpha <- qnorm(1 - alpha/2)
  z_beta <- qnorm(power)
  n <- 2 * ((z_alpha + z_beta) / effect_size)^2
  return(ceiling(n))
}

# Data URL constants
SOVI_DATA_URL <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
DISTANCE_DATA_URL <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
METADATA_URL <- "https://www.sciencedirect.com/science/article/pii/S2352340921010180"

# Dashboard information
DASHBOARD_INFO <- list(
  name = "AXIS",
  full_name = "Advanced Exploratory Inference Statistics",
  version = "1.0.0",
  description = "Comprehensive statistical analysis platform for exploratory data analysis and statistical inference",
  author = "Statistical Analysis Team",
  contact = "axis@statistics.com"
)

# Distance matrix interpretation function
interpret_distance_matrix <- function(distance_matrix) {
  if (is.null(distance_matrix) || !is.matrix(distance_matrix)) {
    return(list(
      mean_distance = NA,
      sd_distance = NA,
      min_distance = NA,
      max_distance = NA,
      cv = NA
    ))
  }
  
  mean_dist <- mean(distance_matrix, na.rm = TRUE)
  sd_dist <- sd(distance_matrix, na.rm = TRUE)
  min_dist <- min(distance_matrix, na.rm = TRUE)
  max_dist <- max(distance_matrix, na.rm = TRUE)
  
  list(
    mean_distance = mean_dist,
    sd_distance = sd_dist,
    min_distance = min_dist,
    max_distance = max_dist,
    cv = ifelse(mean_dist != 0, (sd_dist / mean_dist) * 100, NA)
  )
}

# Spatial autocorrelation helper
calculate_morans_i <- function(values, weights_matrix) {
  if (is.null(values) || is.null(weights_matrix) || length(values) == 0) {
    return(NA)
  }
  
  n <- length(values)
  W <- sum(weights_matrix, na.rm = TRUE)
  
  if (W == 0 || is.na(W)) return(NA)
  
  mean_val <- mean(values, na.rm = TRUE)
  if (is.na(mean_val)) return(NA)
  
  numerator <- 0
  denominator <- sum((values - mean_val)^2, na.rm = TRUE)
  
  if (denominator == 0) return(NA)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (!is.na(weights_matrix[i, j]) && weights_matrix[i, j] > 0) {
        numerator <- numerator + weights_matrix[i, j] * (values[i] - mean_val) * (values[j] - mean_val)
      }
    }
  }
  
  morans_i <- (n / W) * (numerator / denominator)
  return(morans_i)
}

# Spatial weights matrix creation functions
create_distance_weights <- function(distance_matrix, method = "inverse", threshold = NULL) {
  if (is.null(distance_matrix) || !is.matrix(distance_matrix)) {
    return(matrix(0, 1, 1))
  }
  
  if (method == "inverse") {
    weights <- 1 / (distance_matrix + diag(nrow(distance_matrix)))
    diag(weights) <- 0
  } else if (method == "exponential") {
    weights <- exp(-distance_matrix)
    diag(weights) <- 0
  } else if (method == "threshold") {
    if (is.null(threshold)) threshold <- mean(distance_matrix, na.rm = TRUE)
    weights <- ifelse(distance_matrix <= threshold & distance_matrix > 0, 1, 0)
  } else {
    weights <- matrix(0, nrow(distance_matrix), ncol(distance_matrix))
  }
  
  return(weights)
}

create_knn_weights <- function(distance_matrix, k = 5) {
  if (is.null(distance_matrix) || !is.matrix(distance_matrix)) {
    return(matrix(0, 1, 1))
  }
  
  n <- nrow(distance_matrix)
  weights <- matrix(0, n, n)
  
  for (i in 1:n) {
    neighbors <- order(distance_matrix[i, ])[2:min(k+1, n)]  # Exclude self
    weights[i, neighbors] <- 1
  }
  
  return(weights)
}

# Spatial statistics helper functions
calculate_local_morans_i <- function(values, weights_matrix) {
  if (is.null(values) || is.null(weights_matrix)) {
    return(rep(NA, length(values)))
  }
  
  n <- length(values)
  mean_val <- mean(values, na.rm = TRUE)
  var_val <- var(values, na.rm = TRUE)
  
  if (is.na(mean_val) || is.na(var_val) || var_val == 0) {
    return(rep(NA, n))
  }
  
  local_i <- numeric(n)
  
  for (i in 1:n) {
    wi_sum <- sum(weights_matrix[i, ], na.rm = TRUE)
    if (wi_sum > 0) {
      local_i[i] <- (values[i] - mean_val) / var_val * 
        sum(weights_matrix[i, ] * (values - mean_val), na.rm = TRUE)
    } else {
      local_i[i] <- NA
    }
  }
  
  return(local_i)
}

# Geary's C calculation
calculate_gearys_c <- function(values, weights_matrix) {
  if (is.null(values) || is.null(weights_matrix)) {
    return(NA)
  }
  
  n <- length(values)
  W <- sum(weights_matrix, na.rm = TRUE)
  
  if (W == 0 || is.na(W)) return(NA)
  
  numerator <- 0
  denominator <- sum((values - mean(values, na.rm = TRUE))^2, na.rm = TRUE)
  
  if (denominator == 0) return(NA)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (!is.na(weights_matrix[i, j]) && weights_matrix[i, j] > 0) {
        numerator <- numerator + weights_matrix[i, j] * (values[i] - values[j])^2
      }
    }
  }
  
  gearys_c <- ((n - 1) / (2 * W)) * (numerator / denominator)
  return(gearys_c)
}

# Spatial lag calculation
calculate_spatial_lag <- function(values, weights_matrix) {
  if (is.null(values) || is.null(weights_matrix)) {
    return(rep(NA, length(values)))
  }
  
  n <- length(values)
  spatial_lag <- numeric(n)
  
  for (i in 1:n) {
    wi_sum <- sum(weights_matrix[i, ], na.rm = TRUE)
    if (wi_sum > 0) {
      spatial_lag[i] <- sum(weights_matrix[i, ] * values, na.rm = TRUE) / wi_sum
    } else {
      spatial_lag[i] <- NA
    }
  }
  
  return(spatial_lag)
}

# Color palette functions for mapping
get_color_palette <- function(scheme, n = 5) {
  tryCatch({
    if (scheme == "viridis") {
      return(viridis::viridis(n))
    } else if (scheme == "plasma") {
      return(viridis::plasma(n))
    } else {
      return(RColorBrewer::brewer.pal(min(n, 9), scheme))
    }
  }, error = function(e) {
    return(rainbow(n))
  })
}

# Map legend helper
create_map_legend <- function(values, breaks, colors) {
  if (is.null(values) || is.null(breaks) || length(breaks) < 2) {
    return(character(0))
  }
  
  legend_labels <- character(length(breaks) - 1)
  for (i in 1:(length(breaks) - 1)) {
    legend_labels[i] <- paste(round(breaks[i], 2), "-", round(breaks[i + 1], 2))
  }
  return(legend_labels)
}

# Statistical test result formatter
format_test_result <- function(test_result, test_name) {
  if (is.null(test_result)) {
    return("Test result not available")
  }
  
  result_text <- paste0(
    "Test: ", test_name, "\n",
    "Statistic: ", ifelse(is.null(test_result$statistic), "NA", round(test_result$statistic, 4)), "\n",
    "P-value: ", ifelse(is.null(test_result$p.value), "NA", format_pvalue(test_result$p.value)), "\n"
  )
  
  if (!is.null(test_result$conf.int)) {
    result_text <- paste0(result_text, 
                          "95% CI: [", round(test_result$conf.int[1], 4), 
                          ", ", round(test_result$conf.int[2], 4), "]\n")
  }
  
  return(result_text)
}

# Data validation functions
validate_numeric_data <- function(data, var_name) {
  if (is.null(data)) {
    return(paste("Error:", var_name, "is NULL"))
  }
  
  if (!is.numeric(data)) {
    return(paste("Error:", var_name, "must be numeric"))
  }
  
  if (all(is.na(data))) {
    return(paste("Error:", var_name, "contains only missing values"))
  }
  
  if (length(unique(data[!is.na(data)])) < 2) {
    return(paste("Warning:", var_name, "has insufficient variation"))
  }
  
  return("OK")
}

validate_categorical_data <- function(data, var_name) {
  if (is.null(data)) {
    return(paste("Error:", var_name, "is NULL"))
  }
  
  if (all(is.na(data))) {
    return(paste("Error:", var_name, "contains only missing values"))
  }
  
  unique_vals <- length(unique(data[!is.na(data)]))
  if (unique_vals < 2) {
    return(paste("Error:", var_name, "must have at least 2 categories"))
  }
  
  if (unique_vals > 20) {
    return(paste("Warning:", var_name, "has many categories (", unique_vals, ")"))
  }
  
  return("OK")
}

# Report generation helpers
generate_summary_stats <- function(data, var_name) {
  if (is.null(data)) {
    return(list(error = "Data is NULL"))
  }
  
  if (is.numeric(data)) {
    return(list(
      mean = mean(data, na.rm = TRUE),
      median = median(data, na.rm = TRUE),
      sd = sd(data, na.rm = TRUE),
      min = min(data, na.rm = TRUE),
      max = max(data, na.rm = TRUE),
      n = sum(!is.na(data)),
      missing = sum(is.na(data))
    ))
  } else {
    tbl <- table(data, useNA = "ifany")
    return(list(
      frequencies = tbl,
      n = sum(!is.na(data)),
      missing = sum(is.na(data)),
      categories = length(unique(data[!is.na(data)]))
    ))
  }
}

# File export helpers
export_to_csv <- function(data, filename) {
  tryCatch({
    write.csv(data, filename, row.names = FALSE)
    return("Success")
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

export_plot_to_png <- function(plot_obj, filename, width = 10, height = 6, dpi = 300) {
  tryCatch({
    ggsave(filename, plot = plot_obj, device = "png", 
           width = width, height = height, dpi = dpi)
    return("Success")
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

# PDF Report Generation Functions
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