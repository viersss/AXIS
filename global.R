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
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_line(color = "grey95", size = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Color palette
axis_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                 "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

# Helper function: Safe statistical calculations
safe_stat <- function(x, fun, ...) {
  tryCatch({
    if (length(x) == 0 || all(is.na(x))) return(NA)
    fun(x, na.rm = TRUE, ...)
  }, error = function(e) NA)
}

# Helper function: Calculate descriptive statistics
calculate_descriptive_stats <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  if (!any(numeric_cols)) {
    return(data.frame(
      Variable = character(0),
      N = numeric(0),
      Mean = numeric(0),
      Median = numeric(0),
      SD = numeric(0),
      Min = numeric(0),
      Max = numeric(0),
      Skewness = numeric(0),
      Kurtosis = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  stats_df <- data.frame(
    Variable = names(numeric_data),
    N = sapply(numeric_data, function(x) sum(!is.na(x))),
    Mean = sapply(numeric_data, function(x) safe_stat(x, mean)),
    Median = sapply(numeric_data, function(x) safe_stat(x, median)),
    SD = sapply(numeric_data, function(x) safe_stat(x, sd)),
    Min = sapply(numeric_data, function(x) safe_stat(x, min)),
    Max = sapply(numeric_data, function(x) safe_stat(x, max)),
    Skewness = sapply(numeric_data, function(x) safe_stat(x, moments::skewness)),
    Kurtosis = sapply(numeric_data, function(x) safe_stat(x, moments::kurtosis)),
    stringsAsFactors = FALSE
  )
  
  # Round numeric columns
  numeric_cols_stats <- c("Mean", "Median", "SD", "Min", "Max", "Skewness", "Kurtosis")
  stats_df[numeric_cols_stats] <- lapply(stats_df[numeric_cols_stats], function(x) round(x, 3))
  
  return(stats_df)
}

# Helper function: Data quality assessment
assess_data_quality <- function(data) {
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(list(
      missing_summary = data.frame(Variable = character(0), Missing_Count = numeric(0), Missing_Percent = numeric(0)),
      duplicate_rows = 0,
      data_types = data.frame(Variable = character(0), Type = character(0))
    ))
  }
  
  # Missing values
  missing_count <- sapply(data, function(x) sum(is.na(x)))
  missing_percent <- round((missing_count / nrow(data)) * 100, 2)
  
  missing_summary <- data.frame(
    Variable = names(missing_count),
    Missing_Count = missing_count,
    Missing_Percent = missing_percent,
    stringsAsFactors = FALSE
  )
  
  # Duplicate rows
  duplicate_rows <- sum(duplicated(data))
  
  # Data types
  data_types <- data.frame(
    Variable = names(data),
    Type = sapply(data, class),
    stringsAsFactors = FALSE
  )
  
  return(list(
    missing_summary = missing_summary,
    duplicate_rows = duplicate_rows,
    data_types = data_types
  ))
}

# Helper function: Correlation analysis
calculate_correlation <- function(data, method = "pearson") {
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) return(NULL)
  
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Remove columns with all NA or constant values
  valid_cols <- sapply(numeric_data, function(x) {
    x_clean <- x[!is.na(x)]
    length(x_clean) > 1 && var(x_clean) > 0
  })
  
  if (sum(valid_cols) < 2) return(NULL)
  
  valid_data <- numeric_data[, valid_cols, drop = FALSE]
  
  tryCatch({
    cor_matrix <- cor(valid_data, use = "complete.obs", method = method)
    return(cor_matrix)
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function: Normality tests
test_normality <- function(x, test_type = "shapiro") {
  if (length(x) < 3 || all(is.na(x))) {
    return(list(statistic = NA, p_value = NA, method = test_type))
  }
  
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 3) {
    return(list(statistic = NA, p_value = NA, method = test_type))
  }
  
  tryCatch({
    if (test_type == "shapiro" && length(x_clean) <= 5000) {
      test_result <- shapiro.test(x_clean)
      return(list(
        statistic = test_result$statistic,
        p_value = test_result$p.value,
        method = "Shapiro-Wilk"
      ))
    } else if (test_type == "anderson" && requireNamespace("nortest", quietly = TRUE)) {
      test_result <- nortest::ad.test(x_clean)
      return(list(
        statistic = test_result$statistic,
        p_value = test_result$p.value,
        method = "Anderson-Darling"
      ))
    } else if (test_type == "kolmogorov" && requireNamespace("nortest", quietly = TRUE)) {
      test_result <- nortest::lillie.test(x_clean)
      return(list(
        statistic = test_result$statistic,
        p_value = test_result$p.value,
        method = "Kolmogorov-Smirnov"
      ))
    } else {
      # Fallback to Shapiro-Wilk for small samples
      if (length(x_clean) <= 5000) {
        test_result <- shapiro.test(x_clean)
        return(list(
          statistic = test_result$statistic,
          p_value = test_result$p.value,
          method = "Shapiro-Wilk"
        ))
      } else {
        return(list(statistic = NA, p_value = NA, method = "Sample too large"))
      }
    }
  }, error = function(e) {
    return(list(statistic = NA, p_value = NA, method = paste("Error:", e$message)))
  })
}

# Helper function: Outlier detection using IQR method
detect_outliers <- function(x, method = "iqr") {
  if (length(x) < 4 || all(is.na(x))) return(rep(FALSE, length(x)))
  
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 4) return(rep(FALSE, length(x)))
  
  if (method == "iqr") {
    Q1 <- quantile(x_clean, 0.25, na.rm = TRUE)
    Q3 <- quantile(x_clean, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    outliers <- x < lower_bound | x > upper_bound
    outliers[is.na(x)] <- FALSE
    return(outliers)
  } else if (method == "zscore") {
    z_scores <- abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
    outliers <- z_scores > 3
    outliers[is.na(x)] <- FALSE
    return(outliers)
  }
  
  return(rep(FALSE, length(x)))
}

# Helper function: Advanced regression diagnostics
calculate_regression_diagnostics <- function(model, data) {
  if (is.null(model) || class(model)[1] != "lm") return(NULL)
  
  tryCatch({
    # Basic diagnostics
    residuals <- residuals(model)
    fitted_values <- fitted(model)
    standardized_residuals <- rstandard(model)
    studentized_residuals <- rstudent(model)
    
    # Leverage values
    leverage <- hatvalues(model)
    
    # Cook's distance
    cooks_distance <- cooks.distance(model)
    
    # Durbin-Watson test for autocorrelation
    dw_test <- NULL
    if (requireNamespace("lmtest", quietly = TRUE)) {
      dw_test <- tryCatch({
        lmtest::dwtest(model)
      }, error = function(e) NULL)
    }
    
    # Breusch-Pagan test for heteroscedasticity
    bp_test <- NULL
    if (requireNamespace("lmtest", quietly = TRUE)) {
      bp_test <- tryCatch({
        lmtest::bptest(model)
      }, error = function(e) NULL)
    }
    
    # VIF for multicollinearity (if multiple predictors)
    vif_values <- NULL
    if (length(model$coefficients) > 2 && requireNamespace("car", quietly = TRUE)) {
      vif_values <- tryCatch({
        car::vif(model)
      }, error = function(e) NULL)
    }
    
    return(list(
      residuals = residuals,
      fitted_values = fitted_values,
      standardized_residuals = standardized_residuals,
      studentized_residuals = studentized_residuals,
      leverage = leverage,
      cooks_distance = cooks_distance,
      dw_test = dw_test,
      bp_test = bp_test,
      vif_values = vif_values
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function: Model comparison metrics
calculate_model_metrics <- function(model) {
  if (is.null(model) || class(model)[1] != "lm") return(NULL)
  
  tryCatch({
    # Basic metrics
    r_squared <- summary(model)$r.squared
    adj_r_squared <- summary(model)$adj.r.squared
    
    # AIC and BIC
    aic_value <- AIC(model)
    bic_value <- BIC(model)
    
    # RMSE
    residuals <- residuals(model)
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # MAE
    mae <- mean(abs(residuals), na.rm = TRUE)
    
    return(list(
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      aic = aic_value,
      bic = bic_value,
      rmse = rmse,
      mae = mae
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function: Data transformation suggestions
suggest_transformations <- function(x) {
  if (length(x) < 3 || all(is.na(x)) || !is.numeric(x)) {
    return("No transformation suggested")
  }
  
  x_clean <- x[!is.na(x) & is.finite(x)]
  if (length(x_clean) < 3) return("Insufficient data")
  
  # Check if data is positive (required for log transformation)
  has_positive <- all(x_clean > 0)
  
  # Calculate skewness
  skew <- safe_stat(x_clean, moments::skewness)
  
  suggestions <- character()
  
  if (is.na(skew)) {
    return("Unable to calculate skewness")
  }
  
  if (abs(skew) < 0.5) {
    suggestions <- c(suggestions, "Data appears normally distributed")
  } else if (skew > 1) {
    suggestions <- c(suggestions, "Right-skewed: Consider log or sqrt transformation")
    if (has_positive) {
      suggestions <- c(suggestions, "Log transformation recommended")
    }
  } else if (skew < -1) {
    suggestions <- c(suggestions, "Left-skewed: Consider square or exponential transformation")
  } else {
    suggestions <- c(suggestions, "Mild skewness: Transformation may not be necessary")
  }
  
  # Box-Cox transformation suggestion
  if (has_positive && requireNamespace("forecast", quietly = TRUE)) {
    tryCatch({
      lambda <- forecast::BoxCox.lambda(x_clean)
      if (!is.na(lambda)) {
        suggestions <- c(suggestions, paste("Box-Cox lambda:", round(lambda, 3)))
      }
    }, error = function(e) {
      # Ignore errors in Box-Cox calculation
    })
  }
  
  return(paste(suggestions, collapse = "; "))
}

# Helper function: Effect size calculations
calculate_effect_size <- function(model) {
  if (is.null(model) || class(model)[1] != "lm") return(NULL)
  
  tryCatch({
    # Cohen's f-squared
    r_squared <- summary(model)$r.squared
    f_squared <- r_squared / (1 - r_squared)
    
    # Interpretation
    if (f_squared < 0.02) {
      interpretation <- "Very small effect"
    } else if (f_squared < 0.15) {
      interpretation <- "Small effect"
    } else if (f_squared < 0.35) {
      interpretation <- "Medium effect"
    } else {
      interpretation <- "Large effect"
    }
    
    return(list(
      f_squared = f_squared,
      interpretation = interpretation
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function: Clean column names
clean_column_names <- function(data) {
  names(data) <- make.names(names(data), unique = TRUE)
  names(data) <- gsub("\\.", "_", names(data))
  return(data)
}

# Helper function: Validate data for analysis
validate_data <- function(data) {
  issues <- character()
  
  if (nrow(data) == 0) {
    issues <- c(issues, "Dataset is empty")
  }
  
  if (ncol(data) == 0) {
    issues <- c(issues, "No columns in dataset")
  }
  
  if (nrow(data) < 3) {
    issues <- c(issues, "Insufficient rows for statistical analysis (minimum 3 required)")
  }
  
  numeric_cols <- sum(sapply(data, is.numeric))
  if (numeric_cols == 0) {
    issues <- c(issues, "No numeric variables found")
  }
  
  return(issues)
}

# Helper function: Format p-values
format_p_value <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  return(sprintf("%.3f", p))
}

# Helper function: Create summary statistics table
create_summary_table <- function(data, group_var = NULL) {
  if (is.null(group_var)) {
    return(calculate_descriptive_stats(data))
  } else {
    # Grouped summary statistics
    if (!group_var %in% names(data)) {
      return(calculate_descriptive_stats(data))
    }
    
    groups <- unique(data[[group_var]])
    groups <- groups[!is.na(groups)]
    
    summary_list <- list()
    for (group in groups) {
      group_data <- data[data[[group_var]] == group & !is.na(data[[group_var]]), ]
      group_summary <- calculate_descriptive_stats(group_data)
      group_summary$Group <- as.character(group)
      summary_list[[as.character(group)]] <- group_summary
    }
    
    if (length(summary_list) > 0) {
      combined_summary <- do.call(rbind, summary_list)
      return(combined_summary)
    } else {
      return(calculate_descriptive_stats(data))
    }
  }
}

# Helper function: Generate diagnostic plots data
generate_diagnostic_plots_data <- function(model) {
  if (is.null(model) || class(model)[1] != "lm") return(NULL)
  
  tryCatch({
    # Extract residuals and fitted values
    fitted_vals <- fitted(model)
    residuals_vals <- residuals(model)
    standardized_residuals <- rstandard(model)
    
    # Q-Q plot data
    qq_data <- data.frame(
      theoretical = qnorm(ppoints(length(standardized_residuals))),
      sample = sort(standardized_residuals)
    )
    
    # Residuals vs Fitted
    rvf_data <- data.frame(
      fitted = fitted_vals,
      residuals = residuals_vals
    )
    
    # Scale-Location plot
    sl_data <- data.frame(
      fitted = fitted_vals,
      sqrt_std_residuals = sqrt(abs(standardized_residuals))
    )
    
    # Leverage plot
    leverage_vals <- hatvalues(model)
    cooks_dist <- cooks.distance(model)
    
    leverage_data <- data.frame(
      leverage = leverage_vals,
      std_residuals = standardized_residuals,
      cooks_distance = cooks_dist,
      observation = 1:length(leverage_vals)
    )
    
    return(list(
      qq_data = qq_data,
      rvf_data = rvf_data,
      sl_data = sl_data,
      leverage_data = leverage_data
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function: Safe file reading with encoding detection
safe_read_file <- function(file_path, file_ext) {
  tryCatch({
    if (file_ext %in% c("csv", "txt")) {
      # Try different encodings
      encodings <- c("UTF-8", "latin1", "UTF-8-BOM")
      
      for (enc in encodings) {
        tryCatch({
          data <- readr::read_csv(file_path, locale = readr::locale(encoding = enc), 
                                show_col_types = FALSE)
          if (ncol(data) > 0) return(data)
        }, error = function(e) NULL)
      }
      
      # Fallback to base R
      data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
      return(data)
      
    } else if (file_ext %in% c("xls", "xlsx")) {
      if (requireNamespace("readxl", quietly = TRUE)) {
        data <- readxl::read_excel(file_path)
        return(as.data.frame(data))
      } else {
        stop("readxl package required for Excel files")
      }
    }
  }, error = function(e) {
    stop(paste("Error reading file:", e$message))
  })
}

# Global variables for storing analysis results
analysis_cache <- list()

# Function to clear analysis cache
clear_analysis_cache <- function() {
  analysis_cache <<- list()
}

# Function to get cached analysis or compute new one
get_or_compute_analysis <- function(key, compute_func, ...) {
  if (key %in% names(analysis_cache)) {
    return(analysis_cache[[key]])
  }
  
  result <- compute_func(...)
  analysis_cache[[key]] <<- result
  return(result)
}