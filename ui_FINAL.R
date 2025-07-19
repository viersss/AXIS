# AXIS - Advanced Exploratory Inference Statistics Dashboard
# User Interface (ui.R) 
# FINAL VERSION - READY TO USE - COMPLETE WITHOUT TRUNCATION

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
        .statistics-card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 15px;
          margin-bottom: 20px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        .feature-card {
          background: white;
          padding: 20px;
          border-radius: 10px;
          margin-bottom: 15px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          transition: transform 0.3s ease;
        }
        .feature-card:hover {
          transform: translateY(-5px);
        }
        .feature-icon {
          font-size: 2.5em;
          margin-bottom: 15px;
          color: #3c8dbc;
        }
        .welcome-header {
          background: linear-gradient(135deg, #3c8dbc 0%, #2980b9 100%);
          color: white;
          padding: 30px;
          border-radius: 15px;
          margin-bottom: 30px;
          text-align: center;
        }
        .stats-value {
          font-size: 2.5em;
          font-weight: bold;
          color: #3c8dbc;
        }
        .stats-label {
          font-size: 0.9em;
          color: #666;
          text-transform: uppercase;
        }
        .action-button {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
          color: white;
          padding: 15px 30px;
          border-radius: 25px;
          font-size: 16px;
          font-weight: bold;
          transition: all 0.3s ease;
        }
        .action-button:hover {
          transform: translateY(-2px);
          box-shadow: 0 5px 15px rgba(0,0,0,0.2);
        }
        .help-section {
          background: #f8f9fa;
          padding: 20px;
          border-radius: 10px;
          margin-bottom: 20px;
          border-left: 4px solid #3c8dbc;
        }
        .tip-box {
          background: #e8f5e8;
          padding: 15px;
          border-radius: 8px;
          border-left: 4px solid #28a745;
          margin: 10px 0;
        }
        .warning-box {
          background: #fff3cd;
          padding: 15px;
          border-radius: 8px;
          border-left: 4px solid #ffc107;
          margin: 10px 0;
        }
        .upload-area {
          border: 2px dashed #3c8dbc;
          border-radius: 10px;
          padding: 30px;
          text-align: center;
          background: #f8f9fa;
          transition: all 0.3s ease;
        }
        .upload-area:hover {
          border-color: #2980b9;
          background: #e3f2fd;
        }
        .data-quality-excellent {
          color: #28a745;
          font-weight: bold;
        }
        .data-quality-good {
          color: #ffc107;
          font-weight: bold;
        }
        .data-quality-poor {
          color: #dc3545;
          font-weight: bold;
        }
        .regression-summary {
          background: #e8f4f8;
          padding: 20px;
          border-radius: 10px;
          border-left: 4px solid #3c8dbc;
          margin: 15px 0;
        }
        .coefficient-significant {
          background-color: #d4edda;
          color: #155724;
          padding: 5px;
          border-radius: 3px;
        }
        .coefficient-not-significant {
          background-color: #f8d7da;
          color: #721c24;
          padding: 5px;
          border-radius: 3px;
        }
        .loading-spinner {
          text-align: center;
          margin: 50px 0;
        }
        .loading-spinner .fa-spin {
          font-size: 3em;
          color: #3c8dbc;
        }
        .sidebar-menu > li.active > a {
          background-color: #2c3e50 !important;
          border-left-color: #3c8dbc !important;
        }
        .variable-info {
          background: #f8f9fa;
          padding: 15px;
          border-radius: 8px;
          margin: 10px 0;
        }
        .download-section {
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          padding: 20px;
          border-radius: 10px;
          margin: 20px 0;
          text-align: center;
        }
        .download-button {
          margin: 5px;
          padding: 10px 20px;
          border-radius: 20px;
          border: none;
          font-weight: bold;
          transition: all 0.3s ease;
        }
        .download-button:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        }
        .pdf-download {
          background: linear-gradient(135deg, #dc3545 0%, #c82333 100%);
          color: white;
        }
        .png-download {
          background: linear-gradient(135deg, #28a745 0%, #218838 100%);
          color: white;
        }
        .analysis-card {
          background: white;
          border-radius: 10px;
          padding: 20px;
          margin-bottom: 20px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .metric-card {
          text-align: center;
          padding: 20px;
          background: white;
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          margin-bottom: 15px;
        }
        .metric-value {
          font-size: 2em;
          font-weight: bold;
          color: #3c8dbc;
          margin-bottom: 5px;
        }
        .metric-label {
          font-size: 0.9em;
          color: #666;
          text-transform: uppercase;
          letter-spacing: 1px;
        }
      "))
    ),
    
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
        fluidRow(
          column(12,
            div(class = "welcome-header",
              h1("🎯 AXIS Dashboard", style = "margin-bottom: 10px;"),
              h3("Advanced eXploratory & Inferential Statistics", style = "font-weight: 300; margin-bottom: 20px;"),
              p("Platform Analisis Statistik Profesional untuk Penelitian dan Pengambilan Keputusan Berbasis Data", 
                style = "font-size: 16px; margin-bottom: 20px;"),
              actionButton("goToData", "🚀 Mulai Analisis", class = "action-button")
            )
          )
        ),
        
        fluidRow(
          # Statistics Overview Cards
          column(3,
            div(class = "metric-card",
              div(class = "metric-value", textOutput("totalDatasets", inline = TRUE)),
              div(class = "metric-label", "Dataset Aktif")
            )
          ),
          column(3,
            div(class = "metric-card",
              div(class = "metric-value", textOutput("totalVariables", inline = TRUE)),
              div(class = "metric-label", "Total Variabel")
            )
          ),
          column(3,
            div(class = "metric-card",
              div(class = "metric-value", textOutput("totalObservations", inline = TRUE)),
              div(class = "metric-label", "Total Observasi")
            )
          ),
          column(3,
            div(class = "metric-card",
              div(class = "metric-value", textOutput("dataQuality", inline = TRUE)),
              div(class = "metric-label", "Kualitas Data")
            )
          )
        ),
        
        fluidRow(
          column(6,
            box(
              title = "🌟 Fitur Utama AXIS Dashboard", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "feature-card",
                div(class = "feature-icon", icon("database")),
                h4("📊 Manajemen Data Cerdas"),
                p("Upload, validasi, dan eksplorasi data dengan deteksi otomatis outlier, missing data, dan kualitas dataset secara real-time.")
              ),
              
              div(class = "feature-card",
                div(class = "feature-icon", icon("chart-bar")),
                h4("🔍 Analisis Variabel Mendalam"),
                p("Eksplorasi univariat dengan visualisasi interaktif, uji normalitas, dan interpretasi statistik profesional.")
              ),
              
              div(class = "feature-card",
                div(class = "feature-icon", icon("project-diagram")),
                h4("📈 Regresi Linear Lanjutan"),
                p("Analisis regresi dengan diagnostik lengkap, uji asumsi, dan interpretasi koefisien yang komprehensif.")
              ),
              
              div(class = "feature-card",
                div(class = "feature-icon", icon("file-pdf")),
                h4("📄 Laporan PDF Profesional"),
                p("Generate laporan berkualitas publikasi dengan visualisasi dan interpretasi statistik expert-level.")
              )
            )
          ),
          
          column(6,
            box(
              title = "🎯 Mengapa Memilih AXIS?", 
              status = "success", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "statistics-card",
                h3("💪 Keunggulan Platform", style = "margin-bottom: 20px;"),
                
                div(style = "margin-bottom: 15px;",
                  tags$i(class = "fas fa-check-circle", style = "color: #00ff88; margin-right: 10px;"),
                  strong("Analisis Terintegrasi:"), " Dari eksplorasi hingga inferensi dalam satu platform"
                ),
                
                div(style = "margin-bottom: 15px;",
                  tags$i(class = "fas fa-check-circle", style = "color: #00ff88; margin-right: 10px;"),
                  strong("Visualisasi Interaktif:"), " Plot dinamis dengan teknologi plotly dan ggplot2"
                ),
                
                div(style = "margin-bottom: 15px;",
                  tags$i(class = "fas fa-check-circle", style = "color: #00ff88; margin-right: 10px;"),
                  strong("Validasi Otomatis:"), " Deteksi outlier, uji asumsi, dan quality control"
                ),
                
                div(style = "margin-bottom: 15px;",
                  tags$i(class = "fas fa-check-circle", style = "color: #00ff88; margin-right: 10px;"),
                  strong("Export Profesional:"), " PDF reports siap publikasi dengan metodologi lengkap"
                ),
                
                div(style = "margin-bottom: 0;",
                  tags$i(class = "fas fa-check-circle", style = "color: #00ff88; margin-right: 10px;"),
                  strong("User-Friendly:"), " Interface intuitif untuk semua level expertise"
                )
              )
            ),
            
            box(
              title = "🚀 Quick Start Guide", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(style = "padding: 15px;",
                h5("📋 Langkah Mudah Memulai:"),
                
                div(style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 5px;",
                  tags$strong("1. Upload Data"), br(),
                  "Pilih file CSV/Excel di tab Manajemen Data"
                ),
                
                div(style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 5px;",
                  tags$strong("2. Eksplorasi Variabel"), br(),
                  "Analisis distribusi dan karakteristik setiap variabel"
                ),
                
                div(style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 5px;",
                  tags$strong("3. Analisis Regresi"), br(),
                  "Build model prediktif dengan diagnostik lengkap"
                ),
                
                div(style = "margin: 10px 0; padding: 10px; background: #e8f5e8; border-radius: 5px; border-left: 4px solid #28a745;",
                  tags$strong("4. Download Laporan"), br(),
                  "Export hasil analisis dalam format PDF profesional"
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(12,
            box(
              title = "📊 Dashboard Statistics", 
              status = "info", 
              solidHeader = TRUE,
              width = 12,
              
              fluidRow(
                column(3,
                  div(class = "analysis-card",
                    h4("🔍 Analisis Tersedia", style = "color: #3c8dbc; margin-bottom: 15px;"),
                    tags$ul(
                      tags$li("Statistik Deskriptif Komprehensif"),
                      tags$li("Uji Normalitas Multi-metode"),
                      tags$li("Deteksi & Analisis Outlier"),
                      tags$li("Analisis Korelasi Multivariat"),
                      tags$li("Regresi Linear dengan Diagnostik"),
                      tags$li("Model Validation & Assessment")
                    )
                  )
                ),
                
                column(3,
                  div(class = "analysis-card",
                    h4("📈 Visualisasi", style = "color: #28a745; margin-bottom: 15px;"),
                    tags$ul(
                      tags$li("Histogram Interaktif"),
                      tags$li("Box Plot dengan Outliers"),
                      tags$li("Q-Q Plot Normalitas"),
                      tags$li("Scatter Plot Matrix"),
                      tags$li("Correlation Heatmap"),
                      tags$li("Regression Diagnostic Plots")
                    )
                  )
                ),
                
                column(3,
                  div(class = "analysis-card",
                    h4("🛡️ Quality Control", style = "color: #ffc107; margin-bottom: 15px;"),
                    tags$ul(
                      tags$li("Missing Data Detection"),
                      tags$li("Outlier Identification"),
                      tags$li("Data Type Validation"),
                      tags$li("Statistical Assumptions Testing"),
                      tags$li("Model Diagnostic Checking"),
                      tags$li("Multicollinearity Assessment")
                    )
                  )
                ),
                
                column(3,
                  div(class = "analysis-card",
                    h4("📄 Export Options", style = "color: #dc3545; margin-bottom: 15px;"),
                    tags$ul(
                      tags$li("PDF Professional Reports"),
                      tags$li("High-Resolution PNG Plots"),
                      tags$li("Statistical Summary Tables"),
                      tags$li("Model Coefficients Export"),
                      tags$li("Diagnostic Results"),
                      tags$li("Publication-Ready Formats")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Data Management Tab
      tabItem(tabName = "data_management",
        fluidRow(
          column(12,
            box(
              title = "📊 Upload dan Manajemen Data", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "upload-area",
                fileInput("file", 
                         label = div(
                           icon("cloud-upload-alt", style = "font-size: 2em; color: #3c8dbc; margin-bottom: 10px;"),
                           h4("Pilih File Data", style = "margin: 10px 0;"),
                           p("Drag & drop file atau klik untuk browse", style = "color: #666; margin-bottom: 5px;"),
                           p("Format: CSV, Excel (.xlsx, .xls)", style = "color: #999; font-size: 0.9em;")
                         ),
                         accept = c(".csv", ".xlsx", ".xls"),
                         placeholder = "Belum ada file dipilih")
              ),
              
              conditionalPanel(
                condition = "output.fileUploaded",
                
                br(),
                
                fluidRow(
                  column(6,
                    div(class = "analysis-card",
                      h4("⚙️ Opsi Import", style = "color: #3c8dbc; margin-bottom: 15px;"),
                      
                      checkboxInput("header", 
                                   label = tags$span(icon("check"), " File memiliki header"), 
                                   value = TRUE),
                      
                      div(style = "margin: 15px 0;",
                        h5("📝 Separator"),
                        radioButtons("sep", NULL,
                                   choices = list(
                                     "Comma (,)" = ",", 
                                     "Semicolon (;)" = ";", 
                                     "Tab" = "\t"
                                   ),
                                   selected = ",", 
                                   inline = TRUE)
                      ),
                      
                      div(style = "margin: 15px 0;",
                        h5("🔤 Quote Character"),
                        radioButtons("quote", NULL,
                                   choices = list(
                                     "None" = "", 
                                     'Double Quote (")' = '"', 
                                     "Single Quote (')" = "'"
                                   ),
                                   selected = '"', 
                                   inline = TRUE)
                      )
                    )
                  ),
                  
                  column(6,
                    div(class = "download-section",
                      h4("📥 Download Laporan", style = "margin-bottom: 20px;"),
                      
                      div(style = "margin: 15px 0;",
                        downloadButton("download_data_report", 
                                     label = div(icon("file-pdf"), " Laporan Manajemen Data"),
                                     class = "download-button pdf-download")
                      ),
                      
                      div(style = "margin: 15px 0;",
                        actionButton("resetData", 
                                   label = div(icon("refresh"), " Reset Data"),
                                   class = "btn btn-warning download-button")
                      ),
                      
                      p("📄 Format PDF dengan analisis lengkap kualitas data", 
                        style = "font-size: 0.9em; color: #666; margin-top: 15px;")
                    )
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
                title = "ℹ️ Informasi Dataset", 
                status = "info", 
                solidHeader = TRUE,
                width = 12,
                
                div(class = "variable-info",
                  verbatimTextOutput("dataInfo")
                )
              )
            ),
            
            column(6,
              box(
                title = "🔍 Analisis Kualitas Data", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                
                div(class = "variable-info",
                  verbatimTextOutput("dataQualityInfo")
                )
              )
            )
          ),
          
          fluidRow(
            column(12,
              box(
                title = "👁️ Preview Data", 
                status = "success", 
                solidHeader = TRUE,
                width = 12,
                
                p("📋 Menampilkan pratinjau dataset yang telah diupload:", 
                  style = "margin-bottom: 15px; color: #666;"),
                
                DT::dataTableOutput("dataPreview")
              )
            )
          )
        ),
        
        # Data Upload Instructions
        conditionalPanel(
          condition = "!output.fileUploaded",
          
          fluidRow(
            column(12,
              box(
                title = "📋 Panduan Upload Data", 
                status = "primary", 
                solidHeader = TRUE,
                width = 12,
                
                fluidRow(
                  column(6,
                    div(class = "tip-box",
                      h4("✅ Format File yang Didukung"),
                      tags$ul(
                        tags$li(strong("CSV"), " - Comma Separated Values"),
                        tags$li(strong("Excel"), " - .xlsx dan .xls"),
                        tags$li(strong("Encoding"), " - UTF-8 (direkomendasikan)"),
                        tags$li(strong("Ukuran"), " - Maksimal 50MB")
                      )
                    ),
                    
                    div(class = "tip-box",
                      h4("🎯 Tips untuk Hasil Terbaik"),
                      tags$ul(
                        tags$li("Gunakan nama kolom yang deskriptif"),
                        tags$li("Hindari karakter khusus di nama variabel"),
                        tags$li("Pastikan data numerik dalam format yang konsisten"),
                        tags$li("Periksa missing data sebelum upload")
                      )
                    )
                  ),
                  
                  column(6,
                    div(class = "warning-box",
                      h4("⚠️ Hal yang Perlu Diperhatikan"),
                      tags$ul(
                        tags$li("Data dengan banyak missing values memerlukan cleaning"),
                        tags$li("Outliers akan dideteksi otomatis"),
                        tags$li("Minimal 2 variabel numerik untuk analisis regresi"),
                        tags$li("File besar membutuhkan waktu proses lebih lama")
                      )
                    ),
                    
                    div(class = "analysis-card",
                      h4("🔧 Setelah Upload"),
                      p("1. Sistem akan otomatis menganalisis kualitas data"),
                      p("2. Deteksi tipe variabel (numerik/kategorikal)"),
                      p("3. Identifikasi missing data dan outliers"),
                      p("4. Siap untuk eksplorasi dan analisis lanjutan")
                    )
                  )
                )
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
                title = "⚠️ Perhatian", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                
                div(class = "warning-box",
                  h3("📋 Data Belum Diupload"),
                  p("Silakan upload data terlebih dahulu di tab Manajemen Data untuk memulai eksplorasi variabel."),
                  br(),
                  actionButton("goToDataFromVar", 
                             label = div(icon("database"), " Ke Manajemen Data"),
                             class = "action-button")
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.fileUploaded",
          
          fluidRow(
            column(4,
              box(
                title = "🎯 Pilihan Variabel", 
                status = "primary", 
                solidHeader = TRUE,
                width = 12,
                
                div(style = "margin-bottom: 20px;",
                  h5("📊 Pilih Variabel untuk Analisis:"),
                  selectInput("variableSelect", NULL,
                             choices = NULL,
                             width = "100%")
                ),
                
                div(class = "download-section",
                  h5("📥 Download Analisis"),
                  
                  downloadButton("downloadVarReport", 
                               label = div(icon("chart-bar"), " Laporan Eksplorasi"),
                               class = "download-button pdf-download",
                               style = "width: 100%; margin-bottom: 10px;"),
                  
                  actionButton("refreshVar", 
                             label = div(icon("sync-alt"), " Refresh"),
                             class = "btn btn-info download-button",
                             style = "width: 100%;"),
                  
                  p("📊 Laporan PDF dengan analisis mendalam variabel terpilih", 
                    style = "font-size: 0.8em; margin-top: 10px; color: #666;")
                )
              ),
              
              box(
                title = "📈 Ringkasan Statistik", 
                status = "info", 
                solidHeader = TRUE,
                width = 12,
                
                div(class = "variable-info",
                  verbatimTextOutput("variableSummary")
                )
              )
            ),
            
            column(8,
              box(
                title = "📊 Visualisasi Variabel", 
                status = "success", 
                solidHeader = TRUE,
                width = 12,
                
                div(style = "margin-bottom: 15px;",
                  p("🎨 Visualisasi interaktif untuk variabel terpilih:", 
                    style = "color: #666;")
                ),
                
                div(style = "min-height: 400px;",
                  plotly::plotlyOutput("variablePlot", height = "400px")
                )
              ),
              
              box(
                title = "🔬 Analisis Distribusi", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                
                div(style = "margin-bottom: 10px;",
                  h5("📋 Hasil Analisis Statistik:", style = "color: #856404;")
                ),
                
                div(class = "regression-summary",
                  verbatimTextOutput("distributionAnalysis")
                )
              )
            )
          ),
          
          # Additional Analysis Options
          fluidRow(
            column(12,
              box(
                title = "🔍 Panduan Interpretasi Eksplorasi Variabel", 
                status = "primary", 
                solidHeader = TRUE,
                width = 12,
                
                fluidRow(
                  column(4,
                    div(class = "analysis-card",
                      h4("📊 Variabel Numerik", style = "color: #3c8dbc;"),
                      tags$ul(
                        tags$li(strong("Mean vs Median:"), " Indikator simetri distribusi"),
                        tags$li(strong("Skewness:"), " < 0.5 (simetris), > 1 (highly skewed)"),
                        tags$li(strong("Kurtosis:"), " ≈ 3 (normal), > 3 (peaked), < 3 (flat)"),
                        tags$li(strong("Outliers:"), " Deteksi otomatis dengan metode IQR"),
                        tags$li(strong("Normalitas:"), " Shapiro-Wilk test (p > 0.05 = normal)")
                      )
                    )
                  ),
                  
                  column(4,
                    div(class = "analysis-card",
                      h4("📈 Visualisasi", style = "color: #28a745;"),
                      tags$ul(
                        tags$li(strong("Histogram:"), " Bentuk distribusi dan pola data"),
                        tags$li(strong("Density Curve:"), " Estimasi distribusi kontinyu"),
                        tags$li(strong("Interactive Plot:"), " Zoom, hover untuk detail"),
                        tags$li(strong("Bar Chart:"), " Untuk variabel kategorikal"),
                        tags$li(strong("Export PNG:"), " Resolusi tinggi untuk publikasi")
                      )
                    )
                  ),
                  
                  column(4,
                    div(class = "analysis-card",
                      h4("🎯 Rekomendasi", style = "color: #ffc107;"),
                      tags$ul(
                        tags$li(strong("Normal Data:"), " Lanjut ke analisis parametrik"),
                        tags$li(strong("Skewed Data:"), " Pertimbangkan transformasi"),
                        tags$li(strong("Outliers > 5%:"), " Investigasi lebih lanjut"),
                        tags$li(strong("Missing > 10%:"), " Strategi handling diperlukan"),
                        tags$li(strong("High CV:"), " Variabilitas tinggi, hati-hati interpretasi")
                      )
                    )
                  )
                )
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
                title = "⚠️ Perhatian", 
                status = "warning", 
                solidHeader = TRUE,
                width = 12,
                
                div(class = "warning-box",
                  h3("📋 Data Belum Diupload"),
                  p("Silakan upload data terlebih dahulu di tab Manajemen Data untuk memulai analisis regresi."),
                  br(),
                  actionButton("goToDataFromReg", 
                             label = div(icon("database"), " Ke Manajemen Data"),
                             class = "action-button")
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.fileUploaded",
          
          fluidRow(
            column(4,
              box(
                title = "⚙️ Pengaturan Model Regresi", 
                status = "primary", 
                solidHeader = TRUE,
                width = 12,
                
                div(style = "margin-bottom: 20px;",
                  h5("🎯 Variabel Dependen (Y):"),
                  selectInput("dependentVar", NULL,
                             choices = NULL,
                             width = "100%")
                ),
                
                div(style = "margin-bottom: 20px;",
                  h5("📊 Variabel Independen (X):"),
                  selectInput("independentVars", NULL,
                             choices = NULL, 
                             multiple = TRUE,
                             width = "100%"),
                  p("💡 Tip: Pilih minimal 1 variabel independen", 
                    style = "font-size: 0.8em; color: #666; margin-top: 5px;")
                ),
                
                div(style = "text-align: center; margin: 20px 0;",
                  actionButton("runRegression", 
                             label = div(icon("play"), " Jalankan Regresi"),
                             class = "action-button",
                             style = "width: 100%;")
                ),
                
                conditionalPanel(
                  condition = "output.regressionDone",
                  div(class = "download-section",
                    h5("📥 Download Hasil"),
                    
                    downloadButton("downloadRegressionReport", 
                                 label = div(icon("chart-line"), " Laporan Regresi"),
                                 class = "download-button pdf-download",
                                 style = "width: 100%;"),
                    
                    p("📈 Laporan PDF lengkap dengan diagnostik model", 
                      style = "font-size: 0.8em; margin-top: 10px; color: #666;")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.regressionDone",
                box(
                  title = "📋 Model Summary", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  div(class = "analysis-card",
                    h5("🎯 Quick Stats:"),
                    textOutput("modelSummaryQuick", container = div)
                  )
                )
              )
            ),
            
            column(8,
              box(
                title = "📊 Hasil Analisis Regresi", 
                status = "success", 
                solidHeader = TRUE,
                width = 12,
                
                conditionalPanel(
                  condition = "!output.regressionDone",
                  div(class = "loading-spinner",
                    p("👆 Pilih variabel dan klik 'Jalankan Regresi' untuk memulai analisis", 
                      style = "color: #666; font-size: 16px;"),
                    br(),
                    div(
                      icon("chart-line", style = "font-size: 3em; color: #ddd;"),
                      p("Model regresi akan ditampilkan di sini", style = "color: #999; margin-top: 10px;")
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "output.regressionDone",
                  div(style = "margin-bottom: 15px;",
                    h5("📈 Output Model Regresi Linear:", style = "color: #155724;")
                  ),
                  
                  div(class = "regression-summary",
                    verbatimTextOutput("regressionResults")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.regressionDone",
                box(
                  title = "🔬 Diagnostik Model", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  div(style = "margin-bottom: 15px;",
                    p("🎨 Plot diagnostik untuk validasi asumsi regresi:", 
                      style = "color: #666;")
                  ),
                  
                  div(style = "min-height: 400px;",
                    plotOutput("regressionDiagnostics", height = "400px")
                  ),
                  
                  div(style = "margin-top: 15px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                    h6("🔍 Interpretasi Plot Diagnostik:"),
                    tags$ul(
                      tags$li(strong("Residuals vs Fitted:"), " Uji linearitas dan homoskedastisitas"),
                      tags$li(strong("Q-Q Plot:"), " Uji normalitas residual"),
                      tags$li(strong("Scale-Location:"), " Uji homoskedastisitas"),
                      tags$li(strong("Residuals vs Leverage:"), " Deteksi outliers dan influential points")
                    )
                  )
                )
              )
            )
          ),
          
          # Regression Help Section
          fluidRow(
            column(12,
              box(
                title = "🎓 Panduan Analisis Regresi Linear", 
                status = "primary", 
                solidHeader = TRUE,
                width = 12,
                
                fluidRow(
                  column(3,
                    div(class = "analysis-card",
                      h4("📋 Tahap Persiapan", style = "color: #3c8dbc;"),
                      tags$ol(
                        tags$li("Pilih variabel dependen (outcome)"),
                        tags$li("Pilih 1+ variabel independen (predictors)"),
                        tags$li("Pastikan variabel numerik"),
                        tags$li("Minimal 20 observasi per variabel"),
                        tags$li("Cek missing data dan outliers")
                      )
                    )
                  ),
                  
                  column(3,
                    div(class = "analysis-card",
                      h4("📊 Interpretasi Koefisien", style = "color: #28a745;"),
                      tags$ul(
                        tags$li(strong("Estimate:"), " Perubahan Y per unit X"),
                        tags$li(strong("Std Error:"), " Ketidakpastian estimasi"),
                        tags$li(strong("t-value:"), " Statistik uji signifikansi"),
                        tags$li(strong("Pr(>|t|):"), " P-value (< 0.05 signifikan)"),
                        tags$li(strong("R-squared:"), " Proporsi varians dijelaskan")
                      )
                    )
                  ),
                  
                  column(3,
                    div(class = "analysis-card",
                      h4("🔍 Asumsi Regresi", style = "color: #ffc107;"),
                      tags$ul(
                        tags$li(strong("Linearitas:"), " Hubungan linear X-Y"),
                        tags$li(strong("Independensi:"), " Residual tidak berkorelasi"),
                        tags$li(strong("Homoskedastisitas:"), " Varians residual konstan"),
                        tags$li(strong("Normalitas:"), " Residual berdistribusi normal"),
                        tags$li(strong("No Multicollinearity:"), " X tidak saling berkorelasi tinggi")
                      )
                    )
                  ),
                  
                  column(3,
                    div(class = "analysis-card",
                      h4("⚡ Quick Tips", style = "color: #dc3545;"),
                      tags$ul(
                        tags$li(strong("R² > 0.7:"), " Model sangat baik"),
                        tags$li(strong("p < 0.05:"), " Variabel signifikan"),
                        tags$li(strong("VIF > 10:"), " Multikolineritas masalah"),
                        tags$li(strong("Cook's D > 1:"), " Outlier berpengaruh"),
                        tags$li(strong("Durbin-Watson ≈ 2:"), " Tidak ada autokorelasi")
                      )
                    )
                  )
                )
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
              title = "📚 Panduan Lengkap AXIS Dashboard", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "welcome-header",
                h2("🎯 Comprehensive User Guide"),
                p("Panduan lengkap untuk memaksimalkan penggunaan AXIS Dashboard dalam analisis statistik profesional")
              )
            )
          )
        ),
        
        fluidRow(
          column(6,
            box(
              title = "🚀 Getting Started", 
              status = "success", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "help-section",
                h4("📋 Langkah-langkah Dasar"),
                
                div(class = "tip-box",
                  h5("1️⃣ Manajemen Data"),
                  tags$ul(
                    tags$li("Upload file CSV atau Excel (maks 50MB)"),
                    tags$li("Atur opsi import: header, separator, quote"),
                    tags$li("Validasi otomatis kualitas data"),
                    tags$li("Preview data dan informasi dataset"),
                    tags$li("Download laporan kualitas data (PDF)")
                  )
                ),
                
                div(class = "tip-box",
                  h5("2️⃣ Eksplorasi Variabel"),
                  tags$ul(
                    tags$li("Pilih variabel untuk analisis mendalam"),
                    tags$li("Visualisasi interaktif (histogram, bar chart)"),
                    tags$li("Statistik deskriptif komprehensif"),
                    tags$li("Uji normalitas dan analisis distribusi"),
                    tags$li("Download laporan eksplorasi (PDF)")
                  )
                ),
                
                div(class = "tip-box",
                  h5("3️⃣ Analisis Regresi"),
                  tags$ul(
                    tags$li("Tentukan variabel dependen dan independen"),
                    tags$li("Jalankan analisis regresi linear"),
                    tags$li("Interpretasi koefisien dan R-squared"),
                    tags$li("Diagnostic plots untuk validasi asumsi"),
                    tags$li("Download laporan regresi lengkap (PDF)")
                  )
                )
              )
            )
          ),
          
          column(6,
            box(
              title = "📊 Format Data & Requirements", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "help-section",
                h4("📁 Format File yang Didukung"),
                
                div(class = "analysis-card",
                  h5("✅ CSV (Comma Separated Values)"),
                  tags$ul(
                    tags$li("Separator: comma (,), semicolon (;), tab"),
                    tags$li("Encoding: UTF-8 (direkomendasikan)"),
                    tags$li("Quote: double quote (\"), single quote ('), none"),
                    tags$li("Header: optional (dicentang jika ada)")
                  )
                ),
                
                div(class = "analysis-card",
                  h5("✅ Excel (.xlsx, .xls)"),
                  tags$ul(
                    tags$li("Microsoft Excel format"),
                    tags$li("Worksheet pertama akan dibaca"),
                    tags$li("Header otomatis terdeteksi"),
                    tags$li("Tanggal dan angka format standar")
                  )
                ),
                
                div(class = "warning-box",
                  h5("⚠️ Requirements Data"),
                  tags$ul(
                    tags$li("Minimal 10 observasi untuk analisis"),
                    tags$li("Minimal 2 variabel numerik untuk regresi"),
                    tags$li("Nama kolom tidak boleh ada spasi atau karakter khusus"),
                    tags$li("Missing data < 50% untuk analisis optimal"),
                    tags$li("Ukuran file maksimal 50MB")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(4,
            box(
              title = "🔧 Troubleshooting", 
              status = "danger", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "analysis-card",
                h4("❌ Masalah Umum"),
                
                div(style = "margin: 15px 0;",
                  h6("Upload Gagal"),
                  tags$ul(
                    tags$li("Cek format file (CSV/Excel)"),
                    tags$li("Pastikan ukuran < 50MB"),
                    tags$li("Periksa encoding file"),
                    tags$li("Tutup file di Excel sebelum upload")
                  )
                ),
                
                div(style = "margin: 15px 0;",
                  h6("Analisis Error"),
                  tags$ul(
                    tags$li("Minimal 2 variabel numerik untuk regresi"),
                    tags$li("Cek missing data yang berlebihan"),
                    tags$li("Nama variabel harus valid"),
                    tags$li("Refresh browser jika ada masalah rendering")
                  )
                ),
                
                div(style = "margin: 15px 0;",
                  h6("Download Gagal"),
                  tags$ul(
                    tags$li("Pastikan analisis sudah selesai"),
                    tags$li("Cek koneksi internet"),
                    tags$li("Tunggu hingga proses PDF selesai"),
                    tags$li("Coba refresh dan ulangi download")
                  )
                )
              )
            )
          ),
          
          column(4,
            box(
              title = "💡 Tips & Best Practices", 
              status = "info", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "analysis-card",
                h4("🎯 Optimalisasi Analisis"),
                
                div(class = "tip-box",
                  h6("📊 Persiapan Data"),
                  tags$ul(
                    tags$li("Clean data sebelum upload"),
                    tags$li("Gunakan nama variabel yang deskriptif"),
                    tags$li("Standarisasi format tanggal dan angka"),
                    tags$li("Dokumentasikan missing data")
                  )
                ),
                
                div(class = "tip-box",
                  h6("🔍 Eksplorasi Efektif"),
                  tags$ul(
                    tags$li("Mulai dengan analisis univariat"),
                    tags$li("Identifikasi outliers dan missing patterns"),
                    tags$li("Cek normalitas sebelum analisis parametrik"),
                    tags$li("Visualisasi dulu, statistik kemudian")
                  )
                ),
                
                div(class = "tip-box",
                  h6("📈 Regresi Optimal"),
                  tags$ul(
                    tags$li("Periksa asumsi regresi"),
                    tags$li("Avoid multicollinearity (VIF < 10)"),
                    tags$li("Interpretasikan koefisien dengan hati-hati"),
                    tags$li("Validasi model dengan diagnostic plots")
                  )
                )
              )
            )
          ),
          
          column(4,
            box(
              title = "📞 Support & Resources", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              div(class = "analysis-card",
                h4("🆘 Butuh Bantuan?"),
                
                div(style = "margin: 15px 0; text-align: center;",
                  p("📧 Email Support"),
                  p(strong("axis.dashboard@support.com"), style = "color: #3c8dbc;")
                ),
                
                div(style = "margin: 15px 0; text-align: center;",
                  p("📚 Documentation"),
                  p(strong("docs.axis-dashboard.com"), style = "color: #3c8dbc;")
                ),
                
                div(style = "margin: 15px 0; text-align: center;",
                  p("🎥 Video Tutorials"),
                  p(strong("youtube.com/axis-tutorials"), style = "color: #3c8dbc;")
                ),
                
                div(style = "margin: 15px 0; text-align: center;",
                  p("💬 Community Forum"),
                  p(strong("forum.axis-dashboard.com"), style = "color: #3c8dbc;")
                ),
                
                hr(),
                
                div(style = "text-align: center;",
                  h6("📊 AXIS Dashboard"),
                  p("Advanced eXploratory & Inferential Statistics", style = "font-size: 0.8em; color: #666;"),
                  p("Version 2.0.0 | © 2024", style = "font-size: 0.7em; color: #999;")
                )
              )
            )
          )
        ),
        
        fluidRow(
          column(12,
            box(
              title = "📖 Interpretasi Hasil Statistik", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              fluidRow(
                column(6,
                  div(class = "analysis-card",
                    h4("📊 Statistik Deskriptif"),
                    
                    div(class = "tip-box",
                      h6("Measures of Central Tendency"),
                      tags$ul(
                        tags$li(strong("Mean:"), " Rata-rata aritmatik"),
                        tags$li(strong("Median:"), " Nilai tengah (robust terhadap outlier)"),
                        tags$li(strong("Mode:"), " Nilai yang paling sering muncul")
                      )
                    ),
                    
                    div(class = "tip-box",
                      h6("Measures of Variability"),
                      tags$ul(
                        tags$li(strong("Standard Deviation:"), " Ukuran penyebaran data"),
                        tags$li(strong("Variance:"), " Kuadrat dari standard deviation"),
                        tags$li(strong("Range:"), " Selisih nilai maksimum dan minimum"),
                        tags$li(strong("IQR:"), " Interquartile Range (Q3 - Q1)")
                      )
                    ),
                    
                    div(class = "tip-box",
                      h6("Shape of Distribution"),
                      tags$ul(
                        tags$li(strong("Skewness < |0.5|:"), " Distribusi simetris"),
                        tags$li(strong("Skewness > 1:"), " Highly skewed"),
                        tags$li(strong("Kurtosis ≈ 3:"), " Normal distribution"),
                        tags$li(strong("Kurtosis > 3:"), " Leptokurtic (peaked)")
                      )
                    )
                  )
                ),
                
                column(6,
                  div(class = "analysis-card",
                    h4("📈 Analisis Regresi"),
                    
                    div(class = "tip-box",
                      h6("Model Fit Statistics"),
                      tags$ul(
                        tags$li(strong("R-squared:"), " Proporsi varians yang dijelaskan (0-1)"),
                        tags$li(strong("Adjusted R²:"), " R² yang disesuaikan dengan jumlah variabel"),
                        tags$li(strong("F-statistic:"), " Overall model significance"),
                        tags$li(strong("RMSE:"), " Root Mean Square Error")
                      )
                    ),
                    
                    div(class = "tip-box",
                      h6("Coefficient Interpretation"),
                      tags$ul(
                        tags$li(strong("Estimate:"), " Perubahan Y untuk setiap unit perubahan X"),
                        tags$li(strong("Std Error:"), " Standard error dari estimate"),
                        tags$li(strong("t-value:"), " Estimate / Std Error"),
                        tags$li(strong("p-value < 0.05:"), " Signifikan pada α = 5%")
                      )
                    ),
                    
                    div(class = "warning-box",
                      h6("⚠️ Assumption Violations"),
                      tags$ul(
                        tags$li(strong("Non-linearity:"), " Residuals vs Fitted curved"),
                        tags$li(strong("Heteroscedasticity:"), " Fan-shaped residual plots"),
                        tags$li(strong("Non-normality:"), " Q-Q plot deviates from line"),
                        tags$li(strong("Multicollinearity:"), " VIF > 10")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)