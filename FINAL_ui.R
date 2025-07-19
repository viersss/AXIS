# AXIS - Advanced Exploratory Inference Statistics Dashboard
# User Interface (ui.R) 
# FINAL VERSION - READY TO USE

dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = "AXIS Dashboard",
    tags$li(class = "dropdown",
      tags$a(href = "#", 
             tags$span("Advanced eXploratory & Inferential Statistics", 
                      style = "color: white; font-size: 12px;"))
    )
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Beranda", tabName = "home", icon = icon("home")),
      menuItem("📊 Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem("🔍 Eksplorasi Variabel", tabName = "variable_exploration", icon = icon("search")),
      menuItem("📈 Analisis Regresi", tabName = "regression_analysis", icon = icon("chart-line")),
      menuItem("ℹ️ Bantuan", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .main-header .navbar {
          background-color: #3c8dbc !important;
        }
        .skin-blue .main-header .navbar .nav > li > a {
          color: #fff !important;
        }
        .box.box-solid.box-primary > .box-header {
          background: #3c8dbc;
          color: #fff;
        }
        .box.box-solid.box-success > .box-header {
          background: #28a745;
          color: #fff;
        }
        .box.box-solid.box-warning > .box-header {
          background: #ffc107;
          color: #333;
        }
        .box.box-solid.box-danger > .box-header {
          background: #dc3545;
          color: #fff;
        }
        .info-box-icon {
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .progress-bar {
          transition: width 0.6s ease;
        }
        .btn-file {
          position: relative;
          overflow: hidden;
        }
        .btn-file input[type=file] {
          position: absolute;
          top: 0;
          right: 0;
          min-width: 100%;
          min-height: 100%;
          font-size: 100px;
          text-align: right;
          filter: alpha(opacity=0);
          opacity: 0;
          outline: none;
          background: white;
          cursor: inherit;
          display: block;
        }
      "))
    ),
    
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
        fluidRow(
          column(12,
            box(
              title = "Selamat Datang di AXIS Dashboard", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              h3("Advanced eXploratory & Inferential Statistics"),
              p("AXIS Dashboard adalah platform analisis statistik komprehensif yang dirancang untuk membantu Anda dalam:"),
              tags$ul(
                tags$li("📊 Manajemen dan eksplorasi data"),
                tags$li("🔍 Analisis variabel mendalam"),
                tags$li("📈 Analisis regresi dan inferensial"),
                tags$li("📄 Pembuatan laporan PDF profesional")
              ),
              br(),
              h4("Fitur Utama:"),
              fluidRow(
                column(6,
                  tags$ul(
                    tags$li("Upload data format CSV/Excel"),
                    tags$li("Deteksi outlier otomatis"),
                    tags$li("Visualisasi interaktif"),
                    tags$li("Analisis korelasi multivariat")
                  )
                ),
                column(6,
                  tags$ul(
                    tags$li("Uji normalitas komprehensif"),
                    tags$li("Analisis regresi lanjutan"),
                    tags$li("Diagnostik model statistik"),
                    tags$li("Laporan PDF berkualitas publikasi")
                  )
                )
              ),
              br(),
              div(
                style = "text-align: center;",
                h4("Mulai analisis Anda dengan mengunggah data di tab Manajemen Data"),
                actionButton("goToData", "Mulai Analisis", icon = icon("play"), 
                           class = "btn btn-primary btn-lg")
              )
            )
          )
        ),
        
        fluidRow(
          # Statistics Overview
          valueBoxOutput("totalDatasets", width = 3),
          valueBoxOutput("totalVariables", width = 3),
          valueBoxOutput("totalObservations", width = 3),
          valueBoxOutput("dataQuality", width = 3)
        )
      ),
      
      # Data Management Tab
      tabItem(tabName = "data_management",
        fluidRow(
          column(12,
            box(
              title = "Upload dan Manajemen Data", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              fileInput("file", "Pilih File Data (CSV/Excel)",
                       accept = c(".csv", ".xlsx", ".xls"),
                       placeholder = "Belum ada file dipilih"),
              
              conditionalPanel(
                condition = "output.fileUploaded",
                fluidRow(
                  column(6,
                    h4("Opsi Import"),
                    checkboxInput("header", "Header", TRUE),
                    radioButtons("sep", "Separator",
                               choices = list(Comma = ",", Semicolon = ";", Tab = "\t"),
                               selected = ",", inline = TRUE),
                    radioButtons("quote", "Quote",
                               choices = list(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                               selected = '"', inline = TRUE)
                  ),
                  column(6,
                    h4("Aksi"),
                    br(),
                    downloadButton("download_data_report", "📄 Download Laporan PDF", 
                                 class = "btn btn-success"),
                    br(), br(),
                    actionButton("resetData", "🔄 Reset Data", class = "btn btn-warning")
                  )
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.fileUploaded",
          fluidRow(
            column(6,
              box(
                title = "Informasi Dataset", 
                status = "info", 
                solidHeader = TRUE,
                width = 12,
                verbatimTextOutput("dataInfo")
              )
            ),
            column(6,
              box(
                title = "Kualitas Data", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                verbatimTextOutput("dataQualityInfo")
              )
            )
          ),
          
          fluidRow(
            column(12,
              box(
                title = "Preview Data", 
                status = "success", 
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput("dataPreview")
              )
            )
          )
        )
      ),
      
      # Variable Exploration Tab
      tabItem(tabName = "variable_exploration",
        conditionalPanel(
          condition = "!output.fileUploaded",
          fluidRow(
            column(12,
              box(
                title = "Perhatian", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                h4("Silakan upload data terlebih dahulu di tab Manajemen Data")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.fileUploaded",
          fluidRow(
            column(4,
              box(
                title = "Pilihan Variabel", 
                status = "primary", 
                solidHeader = TRUE,
                width = 12,
                selectInput("variableSelect", "Pilih Variabel untuk Analisis:",
                           choices = NULL),
                br(),
                downloadButton("downloadVarReport", "📊 Download Laporan Variabel", 
                             class = "btn btn-success"),
                br(), br(),
                actionButton("refreshVar", "🔄 Refresh", class = "btn btn-info")
              ),
              
              box(
                title = "Ringkasan Statistik", 
                status = "info", 
                solidHeader = TRUE,
                width = 12,
                verbatimTextOutput("variableSummary")
              )
            ),
            
            column(8,
              box(
                title = "Visualisasi Variabel", 
                status = "success", 
                solidHeader = TRUE,
                width = 12,
                plotly::plotlyOutput("variablePlot", height = "400px")
              ),
              
              box(
                title = "Analisis Distribusi", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                verbatimTextOutput("distributionAnalysis")
              )
            )
          )
        )
      ),
      
      # Regression Analysis Tab
      tabItem(tabName = "regression_analysis",
        conditionalPanel(
          condition = "!output.fileUploaded",
          fluidRow(
            column(12,
              box(
                title = "Perhatian", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                h4("Silakan upload data terlebih dahulu di tab Manajemen Data")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.fileUploaded",
          fluidRow(
            column(4,
              box(
                title = "Pengaturan Model", 
                status = "primary", 
                solidHeader = TRUE,
                width = 12,
                selectInput("dependentVar", "Variabel Dependen:",
                           choices = NULL),
                selectInput("independentVars", "Variabel Independen:",
                           choices = NULL, multiple = TRUE),
                br(),
                actionButton("runRegression", "▶️ Jalankan Regresi", 
                           class = "btn btn-primary"),
                br(), br(),
                downloadButton("downloadRegressionReport", "📈 Download Laporan Regresi", 
                             class = "btn btn-success")
              )
            ),
            
            column(8,
              box(
                title = "Hasil Regresi", 
                status = "success", 
                solidHeader = TRUE,
                width = 12,
                verbatimTextOutput("regressionResults")
              ),
              
              box(
                title = "Diagnostik Model", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                plotOutput("regressionDiagnostics", height = "400px")
              )
            )
          )
        )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
        fluidRow(
          column(12,
            box(
              title = "Panduan Penggunaan AXIS Dashboard", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              h3("📊 Cara Menggunakan Dashboard"),
              
              h4("1. Manajemen Data"),
              p("• Upload file data dalam format CSV atau Excel"),
              p("• Atur opsi import sesuai format data Anda"),
              p("• Download laporan komprehensif tentang kualitas data"),
              
              h4("2. Eksplorasi Variabel"),
              p("• Pilih variabel yang ingin dianalisis"),
              p("• Lihat visualisasi dan statistik deskriptif"),
              p("• Download laporan detail untuk variabel tertentu"),
              
              h4("3. Analisis Regresi"),
              p("• Pilih variabel dependen dan independen"),
              p("• Jalankan analisis regresi linear"),
              p("• Evaluasi diagnostik model"),
              p("• Download laporan analisis regresi lengkap"),
              
              h4("📄 Format Data yang Didukung"),
              tags$ul(
                tags$li("CSV (Comma Separated Values)"),
                tags$li("Excel (.xlsx, .xls)"),
                tags$li("Separator: Comma, Semicolon, Tab"),
                tags$li("Encoding: UTF-8 recommended")
              ),
              
              h4("⚠️ Tips Penggunaan"),
              tags$ul(
                tags$li("Pastikan data tidak mengandung karakter khusus di nama kolom"),
                tags$li("Variabel numerik akan dianalisis secara otomatis"),
                tags$li("Missing data akan dideteksi dan ditangani secara otomatis"),
                tags$li("Gunakan nama variabel yang deskriptif untuk hasil yang lebih baik")
              ),
              
              h4("🔧 Troubleshooting"),
              tags$ul(
                tags$li("Jika upload gagal, cek format dan ukuran file"),
                tags$li("Pastikan data mengandung minimal 2 variabel numerik untuk regresi"),
                tags$li("Untuk data besar (>10MB), proses mungkin membutuhkan waktu lebih lama"),
                tags$li("Refresh browser jika mengalami masalah rendering")
              )
            )
          )
        )
      )
    )
  )
)