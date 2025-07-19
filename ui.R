# AXIS - Advanced Exploratory Inference Statistics Dashboard
# User Interface (ui.R)

dashboardPage(
  # Dashboard Header
  dashboardHeader(
    title = tags$span(
      icon("chart-line"), 
      "AXIS Dashboard",
      style = "font-weight: bold; color: white;"
    ),
    titleWidth = 300
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Management", tabName = "data", icon = icon("database")),
      menuItem("Variable Exploration", tabName = "explore", icon = icon("search")),
      menuItem("Regression Analysis", tabName = "regression", icon = icon("line-chart")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    ),
    tags$div(
      style = "position: absolute; bottom: 10px; width: 100%; text-align: center;",
      tags$small("Version 1.0", style = "color: #999;")
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .main-header .navbar .sidebar-toggle {
          color: white !important;
        }
        .main-header .logo {
          background-color: #34495e !important;
          color: white !important;
          border-bottom: 0 solid transparent !important;
        }
        .main-header .logo:hover {
          background-color: #2c3e50 !important;
        }
        .main-sidebar {
          background-color: #2c3e50 !important;
        }
        .sidebar-menu > li.header {
          background: #1a252f !important;
          color: #b8c7ce !important;
        }
        .sidebar-menu > li > a {
          border-left: 3px solid transparent;
          color: #b8c7ce !important;
        }
        .sidebar-menu > li:hover > a, .sidebar-menu > li.active > a {
          color: white !important;
          background: #1e282c !important;
          border-left-color: #3c8dbc !important;
        }
        .sidebar-menu .treeview-menu > li > a {
          color: #8aa4af !important;
        }
        .sidebar-menu .treeview-menu > li.active > a, .sidebar-menu .treeview-menu > li > a:hover {
          color: white !important;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .box.box-primary {
          border-top-color: #3c8dbc;
        }
        .box.box-info {
          border-top-color: #00c0ef;
        }
        .box.box-warning {
          border-top-color: #f39c12;
        }
        .box.box-success {
          border-top-color: #00a65a;
        }
        .box.box-danger {
          border-top-color: #dd4b39;
        }
        .content-wrapper {
          background-color: #f4f4f4 !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
        .btn-primary {
          background-color: #3c8dbc;
          border-color: #2e6da4;
        }
        .btn-primary:hover {
          background-color: #2e6da4;
          border-color: #204d74;
        }
        .progress-bar {
          background-color: #3c8dbc;
        }
        .small-box h3, .small-box p {
          color: white;
        }
        .small-box .icon {
          color: rgba(255,255,255,0.15);
        }
        .info-box-icon {
          color: white !important;
        }
        .bg-blue {
          background-color: #3c8dbc !important;
        }
        .bg-green {
          background-color: #00a65a !important;
        }
        .bg-yellow {
          background-color: #f39c12 !important;
        }
        .bg-red {
          background-color: #dd4b39 !important;
        }
        .bg-purple {
          background-color: #605ca8 !important;
        }
        .welcome-card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 10px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .feature-card {
          background: white;
          border-radius: 8px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          border-left: 4px solid #3c8dbc;
        }
        .stats-card {
          background: white;
          border-radius: 8px;
          padding: 15px;
          text-align: center;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .upload-area {
          border: 2px dashed #ccc;
          border-radius: 10px;
          padding: 30px;
          text-align: center;
          background-color: #fafafa;
          transition: all 0.3s ease;
        }
        .upload-area:hover {
          border-color: #3c8dbc;
          background-color: #f0f8ff;
        }
        .analysis-section {
          background: white;
          border-radius: 8px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .metric-box {
          background: linear-gradient(45deg, #f093fb 0%, #f5576c 100%);
          color: white;
          padding: 15px;
          border-radius: 8px;
          text-align: center;
          margin: 5px;
        }
        .help-section {
          background: white;
          border-radius: 8px;
          padding: 20px;
          margin: 10px 0;
          border-left: 4px solid #17a2b8;
        }
        .alert {
          border-radius: 6px;
        }
        .nav-tabs-custom > .tab-content {
          background: white;
          padding: 20px;
          border-radius: 0 0 8px 8px;
        }
        .plotly {
          width: 100% !important;
        }
        .dataTables_wrapper {
          margin-top: 20px;
        }
      "))
    ),
    
    # Tab Items
    tabItems(
      # Home Tab
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            div(
              class = "welcome-card",
              h1(icon("chart-line"), " Welcome to AXIS Dashboard", style = "margin: 0;"),
              p("Advanced Exploratory Inference Statistics Dashboard", style = "font-size: 18px; margin: 10px 0 0 0;"),
              p("Your comprehensive tool for statistical analysis and data exploration", style = "margin: 5px 0 0 0;")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 3,
            div(
              class = "stats-card",
              h3(textOutput("home_total_observations"), style = "color: #3c8dbc; margin: 0;"),
              p("Total Observations", style = "margin: 5px 0 0 0;")
            )
          ),
          column(
            width = 3,
            div(
              class = "stats-card",
              h3(textOutput("home_total_variables"), style = "color: #00a65a; margin: 0;"),
              p("Total Variables", style = "margin: 5px 0 0 0;")
            )
          ),
          column(
            width = 3,
            div(
              class = "stats-card",
              h3(textOutput("home_numeric_variables"), style = "color: #f39c12; margin: 0;"),
              p("Numeric Variables", style = "margin: 5px 0 0 0;")
            )
          ),
          column(
            width = 3,
            div(
              class = "stats-card",
              h3(textOutput("home_missing_percent"), style = "color: #dd4b39; margin: 0;"),
              p("Missing Data %", style = "margin: 5px 0 0 0;")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(
              class = "feature-card",
              h3(icon("database"), " Key Features"),
              tags$ul(
                tags$li("Upload and manage CSV/Excel files"),
                tags$li("Comprehensive descriptive statistics"),
                tags$li("Interactive data visualization"),
                tags$li("Advanced regression analysis"),
                tags$li("Professional PDF report generation"),
                tags$li("Data quality assessment"),
                tags$li("Correlation and outlier analysis")
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "feature-card",
              h3(icon("play-circle"), " Quick Start Guide"),
              tags$ol(
                tags$li("Navigate to 'Data Management' to upload your dataset"),
                tags$li("Explore variables in 'Variable Exploration'"),
                tags$li("Run regression analysis in 'Regression Analysis'"),
                tags$li("Download comprehensive PDF reports"),
                tags$li("Check 'Help' section for detailed guidance")
              ),
              br(),
              actionButton("start_analysis", "Start Analysis", 
                          class = "btn-primary btn-lg", 
                          icon = icon("play"),
                          style = "width: 100%;")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "analysis-section",
              h3(icon("info-circle"), " About AXIS Dashboard"),
              p("AXIS (Advanced Exploratory Inference Statistics) Dashboard is a comprehensive 
                statistical analysis platform designed for researchers, analysts, and data scientists. 
                Our tool provides an intuitive interface for conducting sophisticated statistical 
                analyses without requiring extensive programming knowledge."),
              p("Built with R Shiny, AXIS combines powerful statistical computing capabilities 
                with an elegant, user-friendly interface. Whether you're conducting exploratory 
                data analysis, fitting regression models, or generating publication-ready reports, 
                AXIS provides the tools you need for rigorous statistical analysis.")
            )
          )
        )
      ),
      
      # Data Management Tab
      tabItem(
        tabName = "data",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Data Upload", status = "primary", solidHeader = TRUE, width = NULL,
              div(
                class = "upload-area",
                fileInput("file", 
                         label = div(icon("upload"), " Choose CSV or Excel File"),
                         accept = c(".csv", ".xlsx", ".xls"),
                         placeholder = "No file selected"),
                p("Maximum file size: 50MB", style = "color: #666; font-size: 12px;")
              ),
              
              conditionalPanel(
                condition = "output.show_import_options",
                h4("Import Options"),
                fluidRow(
                  column(6, checkboxInput("header", "Header", TRUE)),
                  column(6, checkboxInput("stringsAsFactors", "Strings as factors", FALSE))
                ),
                fluidRow(
                  column(6, 
                    selectInput("sep", "Separator",
                               choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                               selected = ",")),
                  column(6,
                    selectInput("quote", "Quote",
                               choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                               selected = '"'))
                )
              )
            ),
            
            box(
              title = "Data Actions", status = "info", solidHeader = TRUE, width = NULL,
              fluidRow(
                column(6,
                  downloadButton("download_data_report", "Download Data Report",
                               class = "btn-info btn-block",
                               icon = icon("file-pdf"))
                ),
                column(6,
                  actionButton("reset_data", "Reset Data",
                             class = "btn-warning btn-block",
                             icon = icon("refresh"))
                )
              ),
              br(),
              fluidRow(
                column(6,
                  actionButton("go_to_explore", "Explore Variables",
                             class = "btn-success btn-block",
                             icon = icon("search"))
                ),
                column(6,
                  actionButton("go_to_regression", "Run Regression",
                             class = "btn-primary btn-block",
                             icon = icon("line-chart"))
                )
              )
            )
          ),
          
          column(
            width = 6,
            box(
              title = "Data Information", status = "success", solidHeader = TRUE, width = NULL,
              verbatimTextOutput("data_info")
            ),
            
            box(
              title = "Data Quality", status = "warning", solidHeader = TRUE, width = NULL,
              verbatimTextOutput("data_quality")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            box(
              title = "Data Preview", status = "primary", solidHeader = TRUE, width = NULL,
              DT::dataTableOutput("data_preview")
            )
          )
        )
      ),
      
      # Variable Exploration Tab
      tabItem(
        tabName = "explore",
        fluidRow(
          column(
            width = 3,
            box(
              title = "Variable Selection", status = "primary", solidHeader = TRUE, width = NULL,
              selectInput("explore_variable", "Select Variable:",
                         choices = NULL),
              br(),
              actionButton("refresh_explore", "Refresh Analysis",
                          class = "btn-primary btn-block",
                          icon = icon("refresh")),
              br(), br(),
              downloadButton("download_explore_report", "Download Exploration Report",
                           class = "btn-success btn-block",
                           icon = icon("file-pdf"))
            )
          ),
          
          column(
            width = 9,
            box(
              title = "Variable Summary", status = "info", solidHeader = TRUE, width = NULL,
              verbatimTextOutput("variable_summary")
            ),
            
            box(
              title = "Interactive Plot", status = "success", solidHeader = TRUE, width = NULL,
              plotly::plotlyOutput("interactive_plot", height = "400px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "Descriptive Statistics", status = "primary", solidHeader = TRUE, width = NULL,
              DT::dataTableOutput("descriptive_stats")
            )
          ),
          
          column(
            width = 6,
            box(
              title = "Distribution Analysis", status = "warning", solidHeader = TRUE, width = NULL,
              verbatimTextOutput("distribution_analysis")
            )
          )
        )
      ),
      
      # Regression Analysis Tab
      tabItem(
        tabName = "regression",
        fluidRow(
          column(
            width = 3,
            box(
              title = "Model Specification", status = "primary", solidHeader = TRUE, width = NULL,
              selectInput("dependent_var", "Dependent Variable:",
                         choices = NULL),
              selectInput("independent_vars", "Independent Variables:",
                         choices = NULL, multiple = TRUE),
              br(),
              actionButton("run_regression", "Run Regression",
                          class = "btn-primary btn-block",
                          icon = icon("play")),
              br(), br(),
              downloadButton("download_regression_report", "Download Regression Report",
                           class = "btn-success btn-block",
                           icon = icon("file-pdf"))
            )
          ),
          
          column(
            width = 9,
            box(
              title = "Regression Results", status = "info", solidHeader = TRUE, width = NULL,
              verbatimTextOutput("regression_results")
            ),
            
            box(
              title = "Model Diagnostics", status = "warning", solidHeader = TRUE, width = NULL,
              plotOutput("diagnostic_plots", height = "500px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            box(
              title = "Regression Summary Table", status = "success", solidHeader = TRUE, width = NULL,
              DT::dataTableOutput("regression_table")
            )
          )
        )
      ),
      
      # Help Tab
      tabItem(
        tabName = "help",
        fluidRow(
          column(
            width = 12,
            div(
              class = "help-section",
              h2(icon("question-circle"), " AXIS Dashboard User Guide"),
              p("Welcome to the comprehensive help section for AXIS Dashboard. 
                Here you'll find detailed instructions on how to use each feature of the application.")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(
              class = "help-section",
              h3(icon("database"), " Data Management"),
              h4("Supported File Formats:"),
              tags$ul(
                tags$li("CSV files (.csv)"),
                tags$li("Excel files (.xlsx, .xls)")
              ),
              h4("Upload Instructions:"),
              tags$ol(
                tags$li("Click 'Choose CSV or Excel File' button"),
                tags$li("Select your data file (max 50MB)"),
                tags$li("Adjust import options if needed"),
                tags$li("Review data preview and quality information")
              ),
              h4("Data Quality Checks:"),
              tags$ul(
                tags$li("Missing value analysis"),
                tags$li("Data type validation"),
                tags$li("Duplicate row detection"),
                tags$li("Basic descriptive statistics")
              )
            )
          ),
          
          column(
            width = 6,
            div(
              class = "help-section",
              h3(icon("search"), " Variable Exploration"),
              h4("Features Available:"),
              tags$ul(
                tags$li("Interactive visualizations with Plotly"),
                tags$li("Comprehensive descriptive statistics"),
                tags$li("Distribution analysis and normality tests"),
                tags$li("Outlier detection"),
                tags$li("Data transformation suggestions")
              ),
              h4("Visualization Types:"),
              tags$ul(
                tags$li("Histograms for continuous variables"),
                tags$li("Bar charts for categorical variables"),
                tags$li("Box plots for distribution analysis"),
                tags$li("Q-Q plots for normality assessment")
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(
              class = "help-section",
              h3(icon("line-chart"), " Regression Analysis"),
              h4("Model Types Supported:"),
              tags$ul(
                tags$li("Simple linear regression"),
                tags$li("Multiple linear regression"),
                tags$li("Polynomial regression")
              ),
              h4("Diagnostic Tests:"),
              tags$ul(
                tags$li("Residual analysis"),
                tags$li("Normality tests"),
                tags$li("Heteroscedasticity tests"),
                tags$li("Multicollinearity assessment (VIF)"),
                tags$li("Outlier and influence detection")
              ),
              h4("Model Evaluation:"),
              tags$ul(
                tags$li("R-squared and Adjusted R-squared"),
                tags$li("AIC and BIC for model comparison"),
                tags$li("RMSE and MAE"),
                tags$li("Effect size calculations")
              )
            )
          ),
          
          column(
            width = 6,
            div(
              class = "help-section",
              h3(icon("file-pdf"), " Report Generation"),
              h4("Available Reports:"),
              tags$ul(
                tags$li("Data Management Report"),
                tags$li("Variable Exploration Report"),
                tags$li("Regression Analysis Report")
              ),
              h4("Report Contents:"),
              tags$ul(
                tags$li("Executive summary"),
                tags$li("Detailed statistical analyses"),
                tags$li("High-quality visualizations"),
                tags$li("Interpretation and recommendations"),
                tags$li("Technical appendices")
              ),
              h4("Export Formats:"),
              tags$ul(
                tags$li("PDF reports for sharing"),
                tags$li("PNG images for presentations")
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "help-section",
              h3(icon("exclamation-triangle"), " Troubleshooting"),
              h4("Common Issues and Solutions:"),
              tags$dl(
                tags$dt("File upload fails:"),
                tags$dd("Check file size (max 50MB) and format (CSV/Excel only)"),
                tags$dt("Variables not showing in dropdown:"),
                tags$dd("Ensure data is uploaded successfully and contains numeric variables"),
                tags$dt("Regression analysis fails:"),
                tags$dd("Check that you have selected at least one independent variable"),
                tags$dt("PDF report generation fails:"),
                tags$dd("Ensure you have sufficient data and variables for analysis"),
                tags$dt("Plots not displaying:"),
                tags$dd("Try refreshing the analysis or check browser compatibility")
              ),
              
              h4("Data Format Requirements:"),
              tags$ul(
                tags$li("First row should contain column headers"),
                tags$li("Numeric variables should not contain text values"),
                tags$li("Date variables should be in standard format"),
                tags$li("Missing values can be left blank or marked as NA")
              ),
              
              h4("Technical Support:"),
              p("For technical support or feature requests, please contact the development team."),
              p("Visit our documentation for advanced usage examples and tutorials.")
            )
          )
        )
      )
    )
  )
)