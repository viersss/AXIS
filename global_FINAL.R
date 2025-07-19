# AXIS - Advanced Exploratory Inference Statistics Dashboard
# Global Variables and Settings (global.R)
# FINAL VERSION - READY TO USE - COMPLETE WITHOUT TRUNCATION

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

# Advanced statistical functions for analysis
perform_normality_test <- function(data, variable) {
  var_data <- data[[variable]]
  var_data <- var_data[!is.na(var_data)]
  
  results <- list()
  
  # Shapiro-Wilk test (for n <= 5000)
  if (length(var_data) <= 5000 && length(var_data) >= 3) {
    results$shapiro <- shapiro.test(var_data)
  }
  
  # Kolmogorov-Smirnov test
  if (length(var_data) >= 5) {
    results$ks <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
  }
  
  # Anderson-Darling test
  if (requireNamespace("nortest", quietly = TRUE) && length(var_data) >= 8) {
    results$anderson <- nortest::ad.test(var_data)
  }
  
  return(results)
}

perform_regression_diagnostics <- function(model) {
  diagnostics <- list()
  
  # Basic model info
  diagnostics$summary <- summary(model)
  diagnostics$coefficients <- diagnostics$summary$coefficients
  diagnostics$r_squared <- diagnostics$summary$r.squared
  diagnostics$adj_r_squared <- diagnostics$summary$adj.r.squared
  
  # Residual analysis
  diagnostics$residuals <- residuals(model)
  diagnostics$fitted <- fitted(model)
  diagnostics$standardized_residuals <- rstandard(model)
  
  # Cook's distance
  diagnostics$cooks_distance <- cooks.distance(model)
  
  # VIF (if multiple predictors)
  if (length(model$coefficients) > 2 && requireNamespace("car", quietly = TRUE)) {
    tryCatch({
      diagnostics$vif <- car::vif(model)
    }, error = function(e) {
      diagnostics$vif <- NULL
    })
  }
  
  # Durbin-Watson test
  if (requireNamespace("lmtest", quietly = TRUE)) {
    tryCatch({
      diagnostics$durbin_watson <- lmtest::dwtest(model)
    }, error = function(e) {
      diagnostics$durbin_watson <- NULL
    })
  }
  
  # Breusch-Pagan test
  if (requireNamespace("lmtest", quietly = TRUE)) {
    tryCatch({
      diagnostics$breusch_pagan <- lmtest::bptest(model)
    }, error = function(e) {
      diagnostics$breusch_pagan <- NULL
    })
  }
  
  return(diagnostics)
}

# Generate comprehensive data summary
generate_data_summary <- function(data) {
  summary_info <- list()
  
  # Basic info
  summary_info$dimensions <- dim(data)
  summary_info$variable_types <- sapply(data, class)
  summary_info$missing_summary <- sapply(data, function(x) sum(is.na(x)))
  
  # Numeric variables summary
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_vars) > 0) {
    summary_info$numeric_summary <- data.frame(
      Variable = numeric_vars,
      Mean = sapply(data[numeric_vars], function(x) round(mean(x, na.rm = TRUE), 4)),
      Median = sapply(data[numeric_vars], function(x) round(median(x, na.rm = TRUE), 4)),
      SD = sapply(data[numeric_vars], function(x) round(sd(x, na.rm = TRUE), 4)),
      Min = sapply(data[numeric_vars], function(x) round(min(x, na.rm = TRUE), 4)),
      Max = sapply(data[numeric_vars], function(x) round(max(x, na.rm = TRUE), 4))
    )
  }
  
  # Categorical variables summary
  categorical_vars <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  if (length(categorical_vars) > 0) {
    summary_info$categorical_summary <- lapply(categorical_vars, function(var) {
      tbl <- table(data[[var]], useNA = "ifany")
      list(
        variable = var,
        categories = length(tbl),
        mode = names(tbl)[which.max(tbl)],
        mode_freq = max(tbl),
        distribution = tbl
      )
    })
    names(summary_info$categorical_summary) <- categorical_vars
  }
  
  return(summary_info)
}

# Create advanced visualizations
create_advanced_histogram <- function(data, variable) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  var_data <- data[[variable]]
  
  p <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(aes(y = ..density..), bins = 30, 
                   fill = statistical_colors$primary, alpha = 0.7, color = "white") +
    geom_density(color = statistical_colors$accent, size = 1.2) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(var_data, na.rm = TRUE), 
                            sd = sd(var_data, na.rm = TRUE)),
                  color = statistical_colors$warning, size = 1, linetype = "dashed") +
    labs(title = paste("Distribusi", variable),
         subtitle = "Histogram dengan kurva densitas dan distribusi normal teoritis",
         x = variable,
         y = "Densitas") +
    theme_axis()
  
  return(p)
}

create_qq_plot <- function(data, variable) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  p <- ggplot(data, aes_string(sample = variable)) +
    stat_qq(color = statistical_colors$primary, alpha = 0.7) +
    stat_qq_line(color = statistical_colors$warning, size = 1) +
    labs(title = paste("Q-Q Plot untuk", variable),
         subtitle = "Perbandingan dengan distribusi normal teoritis",
         x = "Quantile Teoritis",
         y = "Quantile Sampel") +
    theme_axis()
  
  return(p)
}

create_boxplot_analysis <- function(data, variable) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  
  # Create a temporary grouping variable if none exists
  temp_data <- data.frame(
    value = data[[variable]],
    group = "All Data"
  )
  
  p <- ggplot(temp_data, aes(x = group, y = value)) +
    geom_boxplot(fill = statistical_colors$primary, alpha = 0.7, outlier.color = statistical_colors$warning) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "gray30") +
    stat_summary(fun = mean, geom = "point", shape = 23, 
                 size = 3, fill = statistical_colors$accent, color = "white") +
    labs(title = paste("Boxplot", variable),
         subtitle = "Distribusi dengan outliers dan rata-rata",
         x = "",
         y = variable) +
    theme_axis() +
    theme(axis.text.x = element_blank())
  
  return(p)
}

# Advanced correlation analysis
perform_correlation_analysis <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  
  if (ncol(numeric_data) < 2) {
    return(list(error = "Minimal 2 variabel numerik diperlukan untuk analisis korelasi"))
  }
  
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Find significant correlations
  n <- nrow(na.omit(numeric_data))
  cor_test_results <- list()
  
  for (i in 1:(ncol(numeric_data)-1)) {
    for (j in (i+1):ncol(numeric_data)) {
      var1 <- colnames(numeric_data)[i]
      var2 <- colnames(numeric_data)[j]
      
      test_result <- cor.test(numeric_data[[var1]], numeric_data[[var2]])
      
      cor_test_results[[paste(var1, var2, sep = "_")]] <- list(
        variables = c(var1, var2),
        correlation = test_result$estimate,
        p_value = test_result$p.value,
        confidence_interval = test_result$conf.int
      )
    }
  }
  
  return(list(
    correlation_matrix = cor_matrix,
    correlation_tests = cor_test_results,
    sample_size = n
  ))
}

# Statistical reporting functions
create_regression_report <- function(model, data) {
  diagnostics <- perform_regression_diagnostics(model)
  
  report <- list(
    model_summary = diagnostics$summary,
    coefficients_table = diagnostics$coefficients,
    model_fit = list(
      r_squared = diagnostics$r_squared,
      adj_r_squared = diagnostics$adj_r_squared,
      rmse = sqrt(mean(diagnostics$residuals^2, na.rm = TRUE)),
      mae = mean(abs(diagnostics$residuals), na.rm = TRUE)
    ),
    diagnostics = list(
      vif = diagnostics$vif,
      durbin_watson = diagnostics$durbin_watson,
      breusch_pagan = diagnostics$breusch_pagan
    ),
    residual_analysis = list(
      residuals = diagnostics$residuals,
      fitted = diagnostics$fitted,
      standardized_residuals = diagnostics$standardized_residuals,
      cooks_distance = diagnostics$cooks_distance
    )
  )
  
  return(report)
}

# Export functions for data
export_summary_table <- function(summary_data, filename) {
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    tryCatch({
      openxlsx::write.xlsx(summary_data, filename)
      return("Success")
    }, error = function(e) {
      return(paste("Error:", e$message))
    })
  } else {
    write.csv(summary_data, filename, row.names = FALSE)
    return("Success (CSV format)")
  }
}

# Utility functions for data validation
validate_numeric_variable <- function(data, variable) {
  if (!variable %in% names(data)) {
    return(list(valid = FALSE, message = "Variabel tidak ditemukan dalam data"))
  }
  
  if (!is.numeric(data[[variable]])) {
    return(list(valid = FALSE, message = "Variabel bukan tipe numerik"))
  }
  
  non_missing <- sum(!is.na(data[[variable]]))
  if (non_missing < 3) {
    return(list(valid = FALSE, message = "Terlalu sedikit data valid (minimum 3 observasi)"))
  }
  
  return(list(valid = TRUE, message = "Variabel valid untuk analisis"))
}

validate_regression_inputs <- function(data, dependent_var, independent_vars) {
  validation_results <- list()
  
  # Check dependent variable
  dep_check <- validate_numeric_variable(data, dependent_var)
  validation_results$dependent <- dep_check
  
  # Check independent variables
  validation_results$independent <- list()
  for (var in independent_vars) {
    validation_results$independent[[var]] <- validate_numeric_variable(data, var)
  }
  
  # Check for multicollinearity (basic)
  if (length(independent_vars) > 1) {
    numeric_data <- data[c(dependent_var, independent_vars)]
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Check for high correlations among independent variables
    indep_cor <- cor_matrix[independent_vars, independent_vars]
    diag(indep_cor) <- 0
    max_cor <- max(abs(indep_cor), na.rm = TRUE)
    
    validation_results$multicollinearity <- list(
      max_correlation = max_cor,
      warning = max_cor > 0.8,
      message = if (max_cor > 0.8) "Peringatan: Korelasi tinggi antar variabel independen" else "Tidak ada masalah multikolinearitas yang jelas"
    )
  }
  
  return(validation_results)
}

# Advanced data transformation functions
apply_transformation <- function(data, variable, method = "log") {
  var_data <- data[[variable]]
  
  if (!is.numeric(var_data)) {
    return(list(success = FALSE, message = "Variabel harus numerik"))
  }
  
  transformed_data <- switch(method,
    "log" = {
      if (any(var_data <= 0, na.rm = TRUE)) {
        list(success = FALSE, message = "Log transformation membutuhkan nilai positif")
      } else {
        list(success = TRUE, data = log(var_data), method = "Natural Log")
      }
    },
    "log10" = {
      if (any(var_data <= 0, na.rm = TRUE)) {
        list(success = FALSE, message = "Log10 transformation membutuhkan nilai positif")
      } else {
        list(success = TRUE, data = log10(var_data), method = "Log10")
      }
    },
    "sqrt" = {
      if (any(var_data < 0, na.rm = TRUE)) {
        list(success = FALSE, message = "Square root transformation membutuhkan nilai non-negatif")
      } else {
        list(success = TRUE, data = sqrt(var_data), method = "Square Root")
      }
    },
    "square" = {
      list(success = TRUE, data = var_data^2, method = "Square")
    },
    "inverse" = {
      if (any(var_data == 0, na.rm = TRUE)) {
        list(success = FALSE, message = "Inverse transformation tidak dapat diterapkan pada nilai nol")
      } else {
        list(success = TRUE, data = 1/var_data, method = "Inverse")
      }
    },
    {
      list(success = FALSE, message = "Metode transformasi tidak dikenal")
    }
  )
  
  return(transformed_data)
}

# Function to calculate effect sizes
calculate_effect_size <- function(group1, group2, type = "cohens_d") {
  if (type == "cohens_d") {
    mean1 <- mean(group1, na.rm = TRUE)
    mean2 <- mean(group2, na.rm = TRUE)
    sd1 <- sd(group1, na.rm = TRUE)
    sd2 <- sd(group2, na.rm = TRUE)
    n1 <- length(group1[!is.na(group1)])
    n2 <- length(group2[!is.na(group2)])
    
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    cohens_d <- (mean1 - mean2) / pooled_sd
    
    return(list(
      effect_size = cohens_d,
      interpretation = interpret_cohens_d(cohens_d),
      type = "Cohen's d"
    ))
  }
}

# Advanced outlier detection
detect_outliers <- function(data, variable, method = "iqr") {
  var_data <- data[[variable]]
  
  if (!is.numeric(var_data)) {
    return(list(error = "Variabel harus numerik"))
  }
  
  clean_data <- var_data[!is.na(var_data)]
  
  outliers_info <- switch(method,
    "iqr" = {
      Q1 <- quantile(clean_data, 0.25)
      Q3 <- quantile(clean_data, 0.75)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      outlier_indices <- which(var_data < lower_bound | var_data > upper_bound)
      
      list(
        method = "IQR Method",
        lower_bound = lower_bound,
        upper_bound = upper_bound,
        outlier_indices = outlier_indices,
        outlier_values = var_data[outlier_indices],
        n_outliers = length(outlier_indices),
        percentage = round(length(outlier_indices) / length(clean_data) * 100, 2)
      )
    },
    "zscore" = {
      z_scores <- abs(scale(clean_data))
      outlier_threshold <- 3
      outlier_indices <- which(abs(scale(var_data)) > outlier_threshold)
      
      list(
        method = "Z-Score Method",
        threshold = outlier_threshold,
        outlier_indices = outlier_indices,
        outlier_values = var_data[outlier_indices],
        z_scores = z_scores,
        n_outliers = length(outlier_indices),
        percentage = round(length(outlier_indices) / length(clean_data) * 100, 2)
      )
    }
  )
  
  return(outliers_info)
}

# Function to create comprehensive statistical summary
create_comprehensive_summary <- function(data) {
  summary_report <- list()
  
  # Basic data information
  summary_report$basic_info <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    variable_names = names(data),
    data_types = sapply(data, class)
  )
  
  # Missing data analysis
  summary_report$missing_data <- list(
    total_missing = sum(is.na(data)),
    missing_by_variable = sapply(data, function(x) sum(is.na(x))),
    missing_percentage = round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 2)
  )
  
  # Numeric variables analysis
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_vars) > 0) {
    summary_report$numeric_analysis <- create_descriptive_stats_table(data)
  }
  
  # Categorical variables analysis
  categorical_vars <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  if (length(categorical_vars) > 0) {
    summary_report$categorical_analysis <- lapply(categorical_vars, function(var) {
      freq_table <- table(data[[var]], useNA = "ifany")
      list(
        variable = var,
        n_categories = length(freq_table),
        mode = names(freq_table)[which.max(freq_table)],
        frequency_table = freq_table
      )
    })
    names(summary_report$categorical_analysis) <- categorical_vars
  }
  
  # Data quality assessment
  summary_report$quality_assessment <- assess_data_quality(data)
  
  return(summary_report)
}

# Initialize any global variables needed
.axis_initialized <- TRUE

# Print initialization message
if (interactive()) {
  cat("AXIS Dashboard Global Environment Initialized\n")
  cat("===========================================\n")
  cat("✓ Libraries loaded\n")
  cat("✓ Helper functions defined\n")
  cat("✓ Color themes set\n")
  cat("✓ Statistical functions ready\n")
  cat("✓ Ready for analysis!\n\n")
}