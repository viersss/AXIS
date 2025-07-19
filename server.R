# AXIS - Advanced Exploratory Inference Statistics Dashboard
# Server Logic (server.R)

function(input, output, session) {
  
  # Reactive values for storing data and models
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    current_model = NULL
  )
  
  # Helper function for string repetition
  strrep <- function(s, times) {
    paste(rep(s, times), collapse = "")
  }
  
  # File upload and data processing
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      ext <- tools::file_ext(input$file$datapath)
      
      if (ext %in% c("csv", "txt")) {
        values$raw_data <- readr::read_csv(input$file$datapath, 
                                          locale = readr::locale(encoding = "UTF-8"),
                                          show_col_types = FALSE)
      } else if (ext %in% c("xls", "xlsx")) {
        if (requireNamespace("readxl", quietly = TRUE)) {
          values$raw_data <- readxl::read_excel(input$file$datapath)
        } else {
          showNotification("readxl package required for Excel files", type = "error")
          return()
        }
      } else {
        showNotification("Unsupported file format. Please use CSV or Excel files.", type = "error")
        return()
      }
      
      # Clean column names and process data
      values$raw_data <- clean_column_names(values$raw_data)
      values$processed_data <- values$raw_data
      
      # Update variable choices
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      all_vars <- names(values$processed_data)
      
      updateSelectInput(session, "explore_variable", choices = all_vars)
      updateSelectInput(session, "dependent_var", choices = numeric_vars)
      updateSelectInput(session, "independent_vars", choices = numeric_vars)
      
      showNotification("Data uploaded successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error uploading file:", e$message), type = "error")
    })
  })
  
  # Show import options conditionally
  output$show_import_options <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, "show_import_options", suspendWhenHidden = FALSE)
  
  # Home tab statistics
  output$home_total_observations <- renderText({
    if (is.null(values$processed_data)) return("0")
    format(nrow(values$processed_data), big.mark = ",")
  })
  
  output$home_total_variables <- renderText({
    if (is.null(values$processed_data)) return("0")
    as.character(ncol(values$processed_data))
  })
  
  output$home_numeric_variables <- renderText({
    if (is.null(values$processed_data)) return("0")
    numeric_count <- sum(sapply(values$processed_data, is.numeric))
    as.character(numeric_count)
  })
  
  output$home_missing_percent <- renderText({
    if (is.null(values$processed_data)) return("0%")
    total_cells <- nrow(values$processed_data) * ncol(values$processed_data)
    missing_cells <- sum(is.na(values$processed_data))
    paste0(round((missing_cells / total_cells) * 100, 1), "%")
  })
  
  # Data information output
  output$data_info <- renderText({
    if (is.null(values$processed_data)) {
      return("No data uploaded yet.")
    }
    
    info <- paste(
      "Dataset Information:",
      paste("Rows:", nrow(values$processed_data)),
      paste("Columns:", ncol(values$processed_data)),
      paste("Numeric variables:", sum(sapply(values$processed_data, is.numeric))),
      paste("Character variables:", sum(sapply(values$processed_data, is.character))),
      paste("Factor variables:", sum(sapply(values$processed_data, is.factor))),
      sep = "\n"
    )
    
    return(info)
  })
  
  # Data quality output
  output$data_quality <- renderText({
    if (is.null(values$processed_data)) {
      return("No data uploaded yet.")
    }
    
    quality <- assess_data_quality(values$processed_data)
    
    info <- paste(
      "Data Quality Assessment:",
      paste("Total missing values:", sum(quality$missing_summary$Missing_Count)),
      paste("Duplicate rows:", quality$duplicate_rows),
      paste("Variables with missing data:", sum(quality$missing_summary$Missing_Count > 0)),
      "",
      "Missing data by variable:",
      sep = "\n"
    )
    
    # Add missing data details
    missing_details <- quality$missing_summary[quality$missing_summary$Missing_Count > 0, ]
    if (nrow(missing_details) > 0) {
      for (i in 1:nrow(missing_details)) {
        info <- paste(info, 
                     paste0("  ", missing_details$Variable[i], ": ", 
                           missing_details$Missing_Count[i], " (", 
                           missing_details$Missing_Percent[i], "%)"),
                     sep = "\n")
      }
    } else {
      info <- paste(info, "  No missing data detected", sep = "\n")
    }
    
    return(info)
  })
  
  # Data preview table
  output$data_preview <- DT::renderDataTable({
    if (is.null(values$processed_data)) {
      return(data.frame("Message" = "No data uploaded yet."))
    }
    
    DT::datatable(
      values$processed_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display nowrap compact",
      filter = "top",
      selection = "multiple"
    )
  })
  
  # Variable summary for exploration
  output$variable_summary <- renderText({
    if (is.null(values$processed_data) || is.null(input$explore_variable)) {
      return("No variable selected or data not available.")
    }
    
    var_name <- input$explore_variable
    if (!var_name %in% names(values$processed_data)) {
      return("Selected variable not found in dataset.")
    }
    
    var_data <- values$processed_data[[var_name]]
    
    if (is.numeric(var_data)) {
      stats <- list(
        N = length(var_data),
        Missing = sum(is.na(var_data)),
        Mean = round(mean(var_data, na.rm = TRUE), 3),
        Median = round(median(var_data, na.rm = TRUE), 3),
        SD = round(sd(var_data, na.rm = TRUE), 3),
        Min = round(min(var_data, na.rm = TRUE), 3),
        Max = round(max(var_data, na.rm = TRUE), 3),
        Q1 = round(quantile(var_data, 0.25, na.rm = TRUE), 3),
        Q3 = round(quantile(var_data, 0.75, na.rm = TRUE), 3)
      )
      
      summary_text <- paste(
        paste("Variable:", var_name),
        paste("Type: Numeric"),
        "",
        "Descriptive Statistics:",
        paste("N =", stats$N),
        paste("Missing =", stats$Missing),
        paste("Mean =", stats$Mean),
        paste("Median =", stats$Median),
        paste("SD =", stats$SD),
        paste("Min =", stats$Min),
        paste("Max =", stats$Max),
        paste("Q1 =", stats$Q1),
        paste("Q3 =", stats$Q3),
        sep = "\n"
      )
      
    } else {
      # For non-numeric variables
      freq_table <- table(var_data, useNA = "ifany")
      top_values <- head(sort(freq_table, decreasing = TRUE), 10)
      
      summary_text <- paste(
        paste("Variable:", var_name),
        paste("Type:", class(var_data)[1]),
        paste("N =", length(var_data)),
        paste("Missing =", sum(is.na(var_data))),
        paste("Unique values =", length(unique(var_data))),
        "",
        "Top 10 most frequent values:",
        paste(names(top_values), ":", top_values, collapse = "\n"),
        sep = "\n"
      )
    }
    
    return(summary_text)
  })
  
  # Interactive plot for variable exploration
  output$interactive_plot <- plotly::renderPlotly({
    if (is.null(values$processed_data) || is.null(input$explore_variable)) {
      return(plotly::plot_ly() %>% plotly::add_text(text = "No data or variable selected", x = 0.5, y = 0.5))
    }
    
    var_name <- input$explore_variable
    var_data <- values$processed_data[[var_name]]
    
    if (is.numeric(var_data)) {
      # Create histogram for numeric variables
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(fill = axis_colors[1], alpha = 0.7, bins = 30) +
        labs(title = paste("Distribution of", var_name),
             x = var_name,
             y = "Frequency") +
        theme_axis()
      
    } else {
      # Create bar chart for categorical variables
      freq_data <- data.frame(table(var_data))
      names(freq_data) <- c("Category", "Frequency")
      
      p <- ggplot(freq_data, aes(x = Category, y = Frequency)) +
        geom_bar(stat = "identity", fill = axis_colors[2], alpha = 0.7) +
        labs(title = paste("Frequency of", var_name),
             x = var_name,
             y = "Frequency") +
        theme_axis() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    plotly::ggplotly(p) %>%
      plotly::layout(height = 400)
  })
  
  # Descriptive statistics table
  output$descriptive_stats <- DT::renderDataTable({
    if (is.null(values$processed_data)) {
      return(data.frame("Message" = "No data available."))
    }
    
    stats_df <- calculate_descriptive_stats(values$processed_data)
    
    if (nrow(stats_df) == 0) {
      return(data.frame("Message" = "No numeric variables found for analysis."))
    }
    
    DT::datatable(
      stats_df,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      class = "display nowrap compact"
    ) %>%
      DT::formatRound(columns = c("Mean", "Median", "SD", "Min", "Max", "Skewness", "Kurtosis"), digits = 3)
  })
  
  # Distribution analysis
  output$distribution_analysis <- renderText({
    if (is.null(values$processed_data) || is.null(input$explore_variable)) {
      return("No variable selected or data not available.")
    }
    
    var_name <- input$explore_variable
    var_data <- values$processed_data[[var_name]]
    
    if (!is.numeric(var_data)) {
      return("Distribution analysis is only available for numeric variables.")
    }
    
    # Normality test
    normality_result <- test_normality(var_data, "shapiro")
    
    # Outlier detection
    outliers <- detect_outliers(var_data, "iqr")
    n_outliers <- sum(outliers, na.rm = TRUE)
    
    # Transformation suggestions
    transform_suggest <- suggest_transformations(var_data)
    
    analysis_text <- paste(
      "Distribution Analysis:",
      "",
      paste("Normality Test (", normality_result$method, "):", sep = ""),
      paste("  Statistic =", round(normality_result$statistic, 4)),
      paste("  p-value =", format_p_value(normality_result$p_value)),
      ifelse(normality_result$p_value < 0.05, 
             "  Result: Non-normal distribution", 
             "  Result: Normal distribution"),
      "",
      "Outlier Detection (IQR method):",
      paste("  Number of outliers:", n_outliers),
      paste("  Percentage of outliers:", round((n_outliers / length(var_data)) * 100, 2), "%"),
      "",
      "Transformation Suggestions:",
      paste(" ", transform_suggest),
      sep = "\n"
    )
    
    return(analysis_text)
  })
  
  # Regression analysis
  observeEvent(input$run_regression, {
    if (is.null(values$processed_data) || is.null(input$dependent_var) || 
        length(input$independent_vars) == 0) {
      showNotification("Please select dependent and independent variables.", type = "warning")
      return()
    }
    
    tryCatch({
      # Create formula
      formula_str <- paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + "))
      formula_obj <- as.formula(formula_str)
      
      # Fit model
      model_data <- values$processed_data[complete.cases(values$processed_data[c(input$dependent_var, input$independent_vars)]), ]
      values$current_model <- lm(formula_obj, data = model_data)
      
      showNotification("Regression analysis completed successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error in regression analysis:", e$message), type = "error")
    })
  })
  
  # Regression results output
  output$regression_results <- renderText({
    if (is.null(values$current_model)) {
      return("No regression model fitted yet. Please run regression analysis first.")
    }
    
    model_summary <- summary(values$current_model)
    
    # Extract key statistics
    r_squared <- round(model_summary$r.squared, 4)
    adj_r_squared <- round(model_summary$adj.r.squared, 4)
    f_statistic <- round(model_summary$fstatistic[1], 4)
    f_p_value <- format_p_value(pf(model_summary$fstatistic[1], 
                                  model_summary$fstatistic[2], 
                                  model_summary$fstatistic[3], 
                                  lower.tail = FALSE))
    
    # Model metrics
    metrics <- calculate_model_metrics(values$current_model)
    
    results_text <- paste(
      "Regression Analysis Results:",
      "",
      "Model Formula:",
      paste(" ", deparse(values$current_model$call$formula)),
      "",
      "Model Fit Statistics:",
      paste("  R-squared =", r_squared),
      paste("  Adjusted R-squared =", adj_r_squared),
      paste("  F-statistic =", f_statistic),
      paste("  p-value =", f_p_value),
      "",
      "Model Performance Metrics:",
      paste("  AIC =", round(metrics$aic, 2)),
      paste("  BIC =", round(metrics$bic, 2)),
      paste("  RMSE =", round(metrics$rmse, 4)),
      paste("  MAE =", round(metrics$mae, 4)),
      "",
      "Coefficients:",
      sep = "\n"
    )
    
    # Add coefficient details
    coef_summary <- model_summary$coefficients
    for (i in 1:nrow(coef_summary)) {
      coef_name <- rownames(coef_summary)[i]
      estimate <- round(coef_summary[i, 1], 4)
      std_error <- round(coef_summary[i, 2], 4)
      t_value <- round(coef_summary[i, 3], 4)
      p_value <- format_p_value(coef_summary[i, 4])
      
      results_text <- paste(results_text,
                           paste("  ", coef_name, ":"),
                           paste("    Estimate =", estimate),
                           paste("    Std. Error =", std_error),
                           paste("    t-value =", t_value),
                           paste("    p-value =", p_value),
                           sep = "\n")
    }
    
    return(results_text)
  })
  
  # Diagnostic plots
  output$diagnostic_plots <- renderPlot({
    if (is.null(values$current_model)) {
      plot.new()
      text(0.5, 0.5, "No regression model available.\nPlease run regression analysis first.", 
           cex = 1.2, col = "gray50")
      return()
    }
    
    # Create diagnostic plots
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    
    # Residuals vs Fitted
    plot(values$current_model, which = 1)
    
    # Q-Q plot
    plot(values$current_model, which = 2)
    
    # Scale-Location
    plot(values$current_model, which = 3)
    
    # Residuals vs Leverage
    plot(values$current_model, which = 5)
  })
  
  # Regression summary table
  output$regression_table <- DT::renderDataTable({
    if (is.null(values$current_model)) {
      return(data.frame("Message" = "No regression model available."))
    }
    
    model_summary <- summary(values$current_model)
    coef_df <- data.frame(
      Variable = rownames(model_summary$coefficients),
      Estimate = round(model_summary$coefficients[, 1], 4),
      Std_Error = round(model_summary$coefficients[, 2], 4),
      t_value = round(model_summary$coefficients[, 3], 4),
      p_value = model_summary$coefficients[, 4],
      Significance = ifelse(model_summary$coefficients[, 4] < 0.001, "***",
                           ifelse(model_summary$coefficients[, 4] < 0.01, "**",
                                 ifelse(model_summary$coefficients[, 4] < 0.05, "*",
                                       ifelse(model_summary$coefficients[, 4] < 0.1, ".", ""))))
    )
    
    DT::datatable(
      coef_df,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      class = "display nowrap compact"
    ) %>%
      DT::formatRound(columns = c("Estimate", "Std_Error", "t_value"), digits = 4) %>%
      DT::formatRound(columns = "p_value", digits = 6)
  })
  
  # Navigation buttons
  observeEvent(input$start_analysis, {
    updateTabItems(session, "sidebar", "data")
  })
  
  observeEvent(input$go_to_explore, {
    updateTabItems(session, "sidebar", "explore")
  })
  
  observeEvent(input$go_to_regression, {
    updateTabItems(session, "sidebar", "regression")
  })
  
  # Reset data functionality
  observeEvent(input$reset_data, {
    values$raw_data <- NULL
    values$processed_data <- NULL
    values$current_model <- NULL
    
    updateSelectInput(session, "explore_variable", choices = character(0))
    updateSelectInput(session, "dependent_var", choices = character(0))
    updateSelectInput(session, "independent_vars", choices = character(0))
    
    showNotification("Data has been reset successfully.", type = "info")
  })
  
  # Download handlers for PDF reports
  
  # Data Management Report
  output$download_data_report <- downloadHandler(
    filename = function() {
      paste("AXIS_Data_Management_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (is.null(values$processed_data)) {
        showNotification("No data available for report generation.", type = "warning")
        return()
      }
      
      # Show progress
      withProgress(message = 'Generating PDF report...', value = 0, {
        incProgress(0.2, detail = "Preparing data...")
        
        # Hardcoded path - update this to match your exact directory
        rmd_path <- "/home/msi/Documents/AXIS_shiny_app/data_management_report.Rmd"
        
        # Check if file exists, if not try alternative locations
        if (!file.exists(rmd_path)) {
          alternative_paths <- c(
            "/home/msi/Documents/data_management_report.Rmd",
            "/home/msi/AXIS_shiny_app/data_management_report.Rmd",
            "data_management_report.Rmd",
            "./reports/data_management_report.Rmd"
          )
          
          for (alt_path in alternative_paths) {
            if (file.exists(alt_path)) {
              rmd_path <- alt_path
              break
            }
          }
        }
        
        incProgress(0.4, detail = "Processing analysis...")
        
        tryCatch({
          # Prepare data for report
          report_data <- list(
            data = values$processed_data,
            data_info = assess_data_quality(values$processed_data),
            descriptive_stats = calculate_descriptive_stats(values$processed_data)
          )
          
          incProgress(0.6, detail = "Rendering report...")
          
          # Render the report
          rmarkdown::render(
            input = rmd_path,
            output_file = file,
            params = list(
              data = report_data$data,
              data_info = report_data$data_info,
              descriptive_stats = report_data$descriptive_stats
            ),
            envir = new.env(),
            quiet = TRUE
          )
          
          incProgress(1, detail = "Report generated successfully!")
          
        }, error = function(e) {
          showNotification(paste("Error generating report:", e$message), type = "error")
          
          # Create a simple fallback report
          cat("AXIS Data Management Report\n",
              "Generated on:", as.character(Sys.Date()), "\n",
              "Error: Could not generate full report\n",
              file = file)
        })
      })
    },
    contentType = "application/pdf"
  )
  
  # Variable Exploration Report
  output$download_explore_report <- downloadHandler(
    filename = function() {
      paste("AXIS_Variable_Exploration_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (is.null(values$processed_data)) {
        showNotification("No data available for report generation.", type = "warning")
        return()
      }
      
      withProgress(message = 'Generating exploration report...', value = 0, {
        incProgress(0.2, detail = "Preparing analysis...")
        
        # Hardcoded path
        rmd_path <- "/home/msi/Documents/AXIS_shiny_app/variable_exploration_report.Rmd"
        
        # Check alternative locations
        if (!file.exists(rmd_path)) {
          alternative_paths <- c(
            "/home/msi/Documents/variable_exploration_report.Rmd",
            "/home/msi/AXIS_shiny_app/variable_exploration_report.Rmd",
            "variable_exploration_report.Rmd",
            "./reports/variable_exploration_report.Rmd"
          )
          
          for (alt_path in alternative_paths) {
            if (file.exists(alt_path)) {
              rmd_path <- alt_path
              break
            }
          }
        }
        
        incProgress(0.6, detail = "Rendering report...")
        
        tryCatch({
          rmarkdown::render(
            input = rmd_path,
            output_file = file,
            params = list(
              data = values$processed_data,
              selected_var = input$explore_variable
            ),
            envir = new.env(),
            quiet = TRUE
          )
          
          incProgress(1, detail = "Report generated successfully!")
          
        }, error = function(e) {
          showNotification(paste("Error generating report:", e$message), type = "error")
          
          # Fallback
          cat("AXIS Variable Exploration Report\n",
              "Generated on:", as.character(Sys.Date()), "\n",
              "Error: Could not generate full report\n",
              file = file)
        })
      })
    },
    contentType = "application/pdf"
  )
  
  # Regression Analysis Report
  output$download_regression_report <- downloadHandler(
    filename = function() {
      paste("AXIS_Regression_Analysis_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (is.null(values$current_model)) {
        showNotification("No regression model available for report generation.", type = "warning")
        return()
      }
      
      withProgress(message = 'Generating regression report...', value = 0, {
        incProgress(0.2, detail = "Preparing model analysis...")
        
        # Hardcoded path
        rmd_path <- "/home/msi/Documents/AXIS_shiny_app/regression_analysis_report.Rmd"
        
        # Check alternative locations
        if (!file.exists(rmd_path)) {
          alternative_paths <- c(
            "/home/msi/Documents/regression_analysis_report.Rmd",
            "/home/msi/AXIS_shiny_app/regression_analysis_report.Rmd",
            "regression_analysis_report.Rmd",
            "./reports/regression_analysis_report.Rmd"
          )
          
          for (alt_path in alternative_paths) {
            if (file.exists(alt_path)) {
              rmd_path <- alt_path
              break
            }
          }
        }
        
        incProgress(0.6, detail = "Rendering report...")
        
        tryCatch({
          rmarkdown::render(
            input = rmd_path,
            output_file = file,
            params = list(
              model = values$current_model,
              data = values$processed_data
            ),
            envir = new.env(),
            quiet = TRUE
          )
          
          incProgress(1, detail = "Report generated successfully!")
          
        }, error = function(e) {
          showNotification(paste("Error generating report:", e$message), type = "error")
          
          # Fallback
          cat("AXIS Regression Analysis Report\n",
              "Generated on:", as.character(Sys.Date()), "\n",
              "Error: Could not generate full report\n",
              file = file)
        })
      })
    },
    contentType = "application/pdf"
  )
}