# AXIS - Advanced eXploratory Inference Statistics Dashboard
# User Interface (ui.R) - Final Version with Bug Fix

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)

# Define UI
ui <- dashboardPage(
  
  # Dashboard Header
  dashboardHeader(
    title = span(
      tags$i(class = "fa fa-chart-line", style = "color: #fff; margin-right: 10px;"),
      "AXIS Dashboard",
      style = "font-weight: bold; font-size: 18px;"
    ),
    titleWidth = 300
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      
      # Menu Items
      menuItem(" Beranda", tabName = "beranda", icon = icon("home")),
      menuItem(" Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem(" Eksplorasi Data", tabName = "exploration", icon = icon("search")),
      menuItem(" Uji Asumsi Data", tabName = "assumptions", icon = icon("check-circle")),
      menuItem(" Uji Beda Rata-rata", tabName = "mean_tests", icon = icon("chart-line")),
      menuItem(" Uji Proporsi & Variance", tabName = "proportion_tests", icon = icon("pie-chart")),
      menuItem(" ANOVA", tabName = "anova", icon = icon("chart-area")),
      menuItem(" Regresi Linear Berganda", tabName = "regression", icon = icon("project-diagram")),
      menuItem(" Pemetaan Spasial", tabName = "spatial_mapping", icon = icon("map"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9; /* Lighter grey background */
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
        .box.box-primary {
          border-top-color: #3c8dbc;
        }
        .btn-download {
          background-color: #28a745;
          border-color: #28a745;
          color: white;
        }
        .btn-download:hover {
          background-color: #218838;
          border-color: #1e7e34;
        }
        .stat-card {
          background: white;
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 20px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.05);
        }
        .stat-value {
          font-size: 2.5em;
          font-weight: bold;
          color: #3c8dbc;
        }
        .interpretation-box {
          background-color: #e8f4f8;
          border-left: 4px solid #3c8dbc;
          padding: 15px;
          margin-top: 15px;
          border-radius: 4px;
        }
        .download-section {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 8px;
          margin-top: 20px;
        }
        .variable-table {
          width: 100%;
          border-collapse: collapse;
          margin-top: 20px;
        }
        .variable-table th, .variable-table td {
          border: 1px solid #ddd;
          padding: 12px;
          text-align: left;
          vertical-align: top;
        }
        .variable-table th {
          background-color: #f2f2f2;
          font-weight: bold;
          color: #333;
        }
        .feature-item {
          margin-bottom: 20px;
          padding-left: 15px;
        }
        .feature-item .fa-2x {
          vertical-align: middle;
        }
        .feature-item h5 {
          font-weight: bold;
          color: #333;
          display: inline;
          margin-left: 10px;
          vertical-align: middle;
        }
        .feature-item p {
          margin-left: 45px;
          color: #555;
        }
        .leaflet-container {
          height: 500px !important;
        }
      "))
    ),
    
    tabItems(
      
      # =================================================================== #
      # BERANDA TAB (REVISED AND FIXED)
      # =================================================================== #
      tabItem(
        tabName = "beranda",
        fluidRow(
          box(
            title = tagList(icon("info-circle"), "Pendahuluan dan Metadata Referensi"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h3("Selamat datang di AXIS (Advanced eXploratory Inference Statistics)", style="font-weight:300;"),
            p("Sebuah dasbor analitik interaktif yang dikembangkan secara profesional untuk eksplorasi dan inferensi statistik pada data sosial-ekonomi. Dasbor ini menyajikan visualisasi informatif dan fitur interaktif yang memungkinkan peneliti, analis, dan pembuat kebijakan menggali pola, menguji hipotesis, serta menghasilkan insight berbasis data yang sahih dan metodologis. Dengan pendekatan ilmiah yang terstruktur, dasbor ini menjadi alat strategis untuk mendukung pengambilan keputusan berbasis bukti dalam konteks sosial-ekonomi.",style = "font-size: 16px;")
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("book-open"), "Konsep dan Definisi Variabel"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Definisi berikut menjelaskan setiap variabel yang terdapat dalam dataset untuk memastikan interpretasi statistik yang akurat."),
            
            tags$table(class = "variable-table",
                       tags$thead(
                         tags$tr(
                           tags$th("Variabel"),
                           tags$th("Konsep Statistik"),
                           tags$th("Deskripsi dan Penjelasan")
                         )
                       ),
                       tags$tbody(
                         tags$tr(
                           tags$td(tags$strong("DISTRICTCODE")),
                           tags$td("Identifier (Kategorik)"),
                           tags$td("Kode unik yang merepresentasikan suatu wilayah administratif (misalnya, kabupaten/kota atau kecamatan).")
                         ),
                         tags$tr(
                           tags$td(tags$strong("CHILDREN")),
                           tags$td("Proporsi Demografi (Numerik)"),
                           tags$td("Persentase populasi yang tergolong anak-anak (misalnya, usia 0-14 tahun) dari total populasi di wilayah tersebut.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("FEMALE")),
                           tags$td("Proporsi Demografi (Numerik)"),
                           tags$td("Persentase populasi yang berjenis kelamin perempuan dari total populasi.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("ELDERLY")),
                           tags$td("Proporsi Demografi (Numerik)"),
                           tags$td("Persentase populasi yang tergolong lansia (misalnya, usia 65 tahun ke atas) dari total populasi.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("PHHD")),
                           tags$td("Indikator Kesejahteraan (Numerik)"),
                           tags$td("Singkatan dari 'Poorly Housed Households' atau 'Person with Disability Headed Household'. Umumnya mengukur persentase rumah tangga yang kondisi huniannya tidak layak atau dikepalai oleh penyandang disabilitas.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("FAMILYSIZE")),
                           tags$td("Ukuran Demografi (Numerik)"),
                           tags$td("Jumlah rata-rata anggota keluarga dalam satu rumah tangga di wilayah tersebut.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("NOELECTRIC")),
                           tags$td("Indikator Infrastruktur (Numerik)"),
                           tags$td("Persentase rumah tangga yang tidak memiliki akses listrik dari PLN maupun non-PLN.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("LOWEDU")),
                           tags$td("Indikator Pendidikan (Numerik)"),
                           tags$td("Persentase populasi (biasanya usia dewasa) dengan tingkat pendidikan rendah (misalnya, tidak tamat SD atau SMP).")
                         ),
                         tags$tr(
                           tags$td(tags$strong("GROWTH")),
                           tags$td("Tingkat Pertumbuhan (Numerik)"),
                           tags$td("Laju pertumbuhan penduduk tahunan di suatu wilayah, biasanya disajikan dalam persen (%).")
                         ),
                         tags$tr(
                           tags$td(tags$strong("POVERTY")),
                           tags$td("Indikator Ekonomi (Numerik)"),
                           tags$td("Persentase populasi yang hidup di bawah garis kemiskinan yang ditetapkan secara nasional atau regional.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("ILLITERATE")),
                           tags$td("Indikator Pendidikan (Numerik)"),
                           tags$td("Persentase populasi (biasanya usia 15 tahun ke atas) yang tidak bisa membaca dan menulis (buta huruf).")
                         ),
                         tags$tr(
                           tags$td(tags$strong("NOTWORKING")),
                           tags$td("Indikator Ekonomi (Numerik)"),
                           tags$td("Persentase populasi usia produktif yang tidak bekerja (bukan merupakan angkatan kerja, berbeda dari pengangguran).")
                         ),
                         tags$tr(
                           tags$td(tags$strong("DPHONE")),
                           tags$td("Indikator Akses Informasi (Numerik)"),
                           tags$td("Persentase rumah tangga yang tidak memiliki akses telepon, baik telepon rumah maupun seluler.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("RENTED")),
                           tags$td("Indikator Perumahan (Numerik)"),
                           tags$td("Persentase rumah tangga yang status kepemilikan rumahnya adalah sewa/kontrak.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("NOSEWER")),
                           tags$td("Indikator Sanitasi (Numerik)"),
                           tags$td("Persentase rumah tangga yang tidak memiliki akses ke sistem pembuangan limbah atau sanitasi yang layak.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("TAPWATER")),
                           tags$td("Indikator Sanitasi (Numerik)"),
                           tags$td("Persentase rumah tangga yang menggunakan air ledeng (PAM) sebagai sumber air minum utama.")
                         ),
                         tags$tr(
                           tags$td(tags$strong("POPULATION")),
                           tags$td("Jumlah Absolut (Numerik)"),
                           tags$td("Total populasi atau jumlah penduduk di wilayah yang bersangkutan.")
                         )
                       )
            )
          )
        ),
        
        fluidRow(
          box(
            title = tagList(icon("cogs"), "Fitur Utama Dashboard"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(6,
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-database fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Manajemen Data"),
                              p("Menyediakan alat untuk memuat dan melakukan pra-pemrosesan data, termasuk kategorisasi variabel numerik menjadi kategorik menggunakan metode kuantil, lebar setara, atau ambang batas kustom.")
                     ),
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-search fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Eksplorasi Data"),
                              p("Fasilitas untuk analisis statistik deskriptif dan pembuatan visualisasi data interaktif (Histogram, Boxplot, Q-Q Plot) untuk memahami distribusi dan karakteristik setiap variabel.")
                     ),
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-check-circle fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Uji Asumsi Statistik"),
                              p("Modul untuk validasi asumsi statistik fundamental sebelum melakukan analisis inferensial, mencakup Uji Normalitas (misalnya, Shapiro-Wilk) dan Uji Homogenitas Varians (misalnya, Levene).")
                     ),
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-chart-line fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Statistik Inferensial"),
                              p("Serangkaian alat uji hipotesis untuk menarik kesimpulan tentang populasi, meliputi Uji Beda Rata-rata (Uji-t), Uji Proporsi, dan Uji Varians.")
                     )
              ),
              column(6,
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-chart-area fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Analysis of Variance (ANOVA)"),
                              p("Fitur untuk membandingkan rata-rata dari tiga atau lebih kelompok melalui ANOVA Satu Arah dan Dua Arah, lengkap dengan analisis post-hoc.")
                     ),
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-project-diagram fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Regresi Linear Berganda"),
                              p("Alat untuk memodelkan hubungan antara satu variabel dependen dengan beberapa variabel independen, dilengkapi dengan diagnostik model dan uji asumsi regresi.")
                     ),
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-map fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Pemetaan Spasial"),
                              p("Visualisasi data spasial dengan peta interaktif untuk analisis distribusi geografis dan autokorelasi spasial menggunakan matriks penimbang jarak.")
                     ),
                     tags$div(class="feature-item",
                              tags$i(class="fas fa-file-alt fa-2x", style="color: #28a745; vertical-align: middle;"),
                              tags$h5("Interpretasi & Ekspor Laporan"),
                              p("Setiap hasil analisis dilengkapi dengan interpretasi statistik otomatis. Semua tabel, plot, dan laporan dapat diunduh dalam format PNG, PDF, dan Word.")
                     )
              )
            )
          )
        )
      ),
      # =================================================================== #
      # END OF REVISED BERANDA TAB
      # =================================================================== #
      
      # DATA MANAGEMENT TAB
      tabItem(
        tabName = "data_management",
        fluidRow(
          box(
            title = "Data Management", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            fluidRow(
              # Kolom 1: Load Data (Ukuran diubah ke 4)
              column(4,
                     tags$h4("1. Load Data", style = "color: #3c8dbc; margin-bottom: 20px;"),
                     
                     actionButton("load_data", "Load SoVI Dataset", 
                                  class = "btn btn-primary btn-lg", 
                                  style = "width: 100%; margin-bottom: 20px;"),
                     
                     conditionalPanel(
                       condition = "output.data_loaded == true",
                       tags$div(
                         class = "stat-card",
                         tags$h5("Data Information", style = "margin-bottom: 15px;"),
                         verbatimTextOutput("data_summary")
                       )
                     )
              ),
              
              # Kolom 2: Transformasi Data (FITUR BARU)
              column(4,
                     tags$h4("2. Transformasi Data", style = "color: #3c8dbc; margin-bottom: 20px;"),
                     
                     conditionalPanel(
                       condition = "output.data_loaded == true",
                       
                       selectInput("transform_variable", "Pilih Variabel:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("transform_method", "Metode Transformasi:",
                                   choices = list(
                                     "Log (Natural)" = "log",
                                     "Log10" = "log10",
                                     "Akar Kuadrat (Sqrt)" = "sqrt",
                                     "Kuadrat (Square)" = "square",
                                     "Box-Cox" = "boxcox"
                                   ),
                                   width = "100%"),
                       
                       actionButton("apply_transformation", "Terapkan Transformasi",
                                    class = "btn btn-success",
                                    style = "width: 100%; margin-bottom: 20px;"),
                       
                       conditionalPanel(
                         condition = "output.transformation_done == true",
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Hasil Transformasi"),
                           verbatimTextOutput("transformation_result")
                         )
                       )
                     )
              ),
              
              # Kolom 3: Kategorisasi Data (Ukuran diubah ke 4)
              column(4,
                     tags$h4("3. Kategorisasi Data", style = "color: #3c8dbc; margin-bottom: 20px;"),
                     
                     conditionalPanel(
                       condition = "output.data_loaded == true",
                       
                       selectInput("var_to_categorize", "Pilih Variabel:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("categorize_method", "Metode Kategorisasi:",
                                   choices = list(
                                     "Quantile (Quartile)" = "quantile",
                                     "Equal Width" = "equal_width",
                                     "Custom Thresholds" = "custom"
                                   ),
                                   selected = "quantile",
                                   width = "100%"),
                       
                       conditionalPanel(
                         condition = "input.categorize_method == 'custom'",
                         textInput("custom_thresholds", "Custom Thresholds (pisahkan dengan koma):",
                                   placeholder = "25, 50, 75",
                                   width = "100%")
                       ),
                       
                       actionButton("apply_categorization", "Apply Categorization",
                                    class = "btn btn-success",
                                    style = "width: 100%; margin-bottom: 20px;"),
                       
                       conditionalPanel(
                         condition = "output.categorization_done == true",
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Hasil Kategorisasi", style = "margin-bottom: 10px;"),
                           verbatimTextOutput("categorization_result")
                         )
                       )
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              tags$hr(),
              
              fluidRow(
                column(12,
                       tags$h4("Preview Data", style = "color: #3c8dbc; margin-bottom: 20px;"),
                       
                       withSpinner(
                         DT::dataTableOutput("data_preview"),
                         color = "#3c8dbc"
                       )
                )
              ),
              
              tags$div(
                class = "download-section",
                tags$h5("Download Options", style = "margin-bottom: 15px;"),
                
                fluidRow(
                  column(4,
                         downloadButton("download_original_data", "Download Original Data (CSV)",
                                        class = "btn btn-success btn-download",
                                        style = "width: 100%;")
                  ),
                  column(4,
                         downloadButton("download_processed_data", "Download Processed Data (CSV)",
                                        class = "btn btn-success btn-download",
                                        style = "width: 100%;")
                  ),
                  column(4,
                         downloadButton("download_data_report", "Download Data Report (PDF)",
                                        class = "btn btn-success btn-download",
                                        style = "width: 100%;")
                  )
                )
              )
            )
          )
        )
      ),
      
      # EXPLORATION TAB
      tabItem(
        tabName = "exploration",
        fluidRow(
          box(
            title = "Eksplorasi Data", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(4,
                       selectInput("explore_variable", "Pilih Variabel:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("chart_type", "Tipe Visualisasi:",
                                   choices = list(
                                     "Histogram" = "histogram",
                                     "Boxplot" = "boxplot",
                                     "Density Plot" = "density",
                                     "Q-Q Plot" = "qq"
                                   ),
                                   width = "100%"),
                       
                       conditionalPanel(
                         condition = "input.chart_type == 'histogram'",
                         numericInput("bins", "Jumlah Bins:", 
                                      value = 30, min = 10, max = 100, width = "100%")
                       ),
                       
                       actionButton("generate_plot", "Generate Plot",
                                    class = "btn btn-primary",
                                    style = "width: 100%; margin-bottom: 20px;")
                ),
                
                column(8,
                       withSpinner(
                         plotlyOutput("exploration_plot", height = "400px"),
                         color = "#3c8dbc"
                       )
                )
              ),
              
              tags$hr(),
              
              fluidRow(
                column(6,
                       tags$div(
                         class = "stat-card",
                         tags$h5("Statistik Deskriptif", style = "margin-bottom: 15px;"),
                         verbatimTextOutput("descriptive_stats")
                       )
                ),
                
                column(6,
                       tags$div(
                         class = "interpretation-box",
                         tags$h5("Interpretasi Statistik", style = "margin-bottom: 10px;"),
                         verbatimTextOutput("stats_interpretation")
                       )
                )
              ),
              
              tags$div(
                class = "download-section",
                tags$h5("Download Options", style = "margin-bottom: 15px;"),
                
                fluidRow(
                  column(4,
                         downloadButton("download_plot", "Download Plot (PNG)",
                                        class = "btn btn-success btn-download",
                                        style = "width: 100%;")
                  ),
                  column(4,
                         downloadButton("download_stats", "Download Stats (TXT)",
                                        class = "btn btn-success btn-download",
                                        style = "width: 100%;")
                  ),
                  column(4,
                         downloadButton("download_exploration_report", "Download Report (PDF)",
                                        class = "btn btn-success btn-download",
                                        style = "width: 100%;")
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h4("Silakan load data terlebih dahulu di menu Manajemen Data", 
                        style = "color: #999;")
              )
            )
          )
        )
      ),
      
      # ASSUMPTIONS TAB
      tabItem(
        tabName = "assumptions",
        fluidRow(
          box(
            title = "Uji Asumsi Data", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(6,
                       tags$h4("Uji Normalitas", style = "color: #3c8dbc; margin-bottom: 20px;"),
                       
                       selectInput("normality_variable", "Pilih Variabel:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("normality_test", "Metode Uji:",
                                   choices = list(
                                     "Shapiro-Wilk Test" = "shapiro",
                                     "Kolmogorov-Smirnov Test" = "ks",
                                     "Anderson-Darling Test" = "ad"
                                   ),
                                   width = "100%"),
                       
                       actionButton("run_normality_test", "Run Test",
                                    class = "btn btn-primary",
                                    style = "width: 100%; margin-bottom: 20px;"),
                       
                       conditionalPanel(
                         condition = "output.normality_done == true",
                         tags$div(
                           class = "stat-card",
                           tags$h5("Hasil Uji Normalitas", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("normality_result")
                         )
                       )
                ),
                
                column(6,
                       tags$h4("Uji Homogenitas", style = "color: #3c8dbc; margin-bottom: 20px;"),
                       
                       selectInput("homogeneity_variable", "Pilih Variabel Numerik:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("homogeneity_group", "Pilih Variabel Grup:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("homogeneity_test", "Metode Uji:",
                                   choices = list(
                                     "Levene Test" = "levene",
                                     "Bartlett Test" = "bartlett",
                                     "Fligner-Killeen Test" = "fligner"
                                   ),
                                   width = "100%"),
                       
                       actionButton("run_homogeneity_test", "Run Test",
                                    class = "btn btn-primary",
                                    style = "width: 100%; margin-bottom: 20px;"),
                       
                       conditionalPanel(
                         condition = "output.homogeneity_done == true",
                         tags$div(
                           class = "stat-card",
                           tags$h5("Hasil Uji Homogenitas", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("homogeneity_result")
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.normality_done == true || output.homogeneity_done == true",
                
                tags$hr(),
                
                fluidRow(
                  column(12,
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Interpretasi Uji Asumsi", style = "margin-bottom: 10px;"),
                           verbatimTextOutput("assumptions_interpretation")
                         )
                  )
                ),
                
                tags$div(
                  class = "download-section",
                  tags$h5("Download Options", style = "margin-bottom: 15px;"),
                  
                  fluidRow(
                    column(4,
                           downloadButton("download_assumptions_results", "Download Results (TXT)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_assumptions_plots", "Download Plots (PNG)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_assumptions_report", "Download Full Report (PDF)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h4("Silakan load data terlebih dahulu di menu Manajemen Data", 
                        style = "color: #999;")
              )
            )
          )
        )
      ),
      
      # MEAN TESTS TAB
      tabItem(
        tabName = "mean_tests",
        fluidRow(
          box(
            title = "Uji Beda Rata-rata", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(4,
                       selectInput("test_type", "Tipe Uji:",
                                   choices = list(
                                     "One Sample t-test" = "one_sample",
                                     "Two Sample t-test" = "two_sample",
                                     "Paired t-test" = "paired"
                                   ),
                                   width = "100%"),
                       
                       selectInput("test_variable", "Pilih Variabel:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       conditionalPanel(
                         condition = "input.test_type == 'one_sample'",
                         numericInput("test_value", "Nilai Uji (μ₀):", 
                                      value = 0, width = "100%")
                       ),
                       
                       conditionalPanel(
                         condition = "input.test_type == 'two_sample'",
                         selectInput("group_variable", "Pilih Variabel Grup:",
                                     choices = NULL,
                                     width = "100%"),
                         
                         checkboxInput("equal_var", "Assume Equal Variances", 
                                       value = TRUE)
                       ),
                       
                       conditionalPanel(
                         condition = "input.test_type == 'paired'",
                         selectInput("paired_variable", "Pilih Variabel Kedua:",
                                     choices = NULL,
                                     width = "100%")
                       ),
                       
                       selectInput("alternative", "Alternative Hypothesis:",
                                   choices = list(
                                     "Two-sided" = "two.sided",
                                     "Greater than" = "greater",
                                     "Less than" = "less"
                                   ),
                                   width = "100%"),
                       
                       numericInput("confidence_level", "Confidence Level:", 
                                    value = 0.95, min = 0.5, max = 0.99, step = 0.01,
                                    width = "100%"),
                       
                       actionButton("run_mean_test", "Run Test",
                                    class = "btn btn-primary",
                                    style = "width: 100%; margin-bottom: 20px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.mean_test_done == true",
                         
                         tags$div(
                           class = "stat-card",
                           tags$h5("Hasil Uji Beda Rata-rata", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("mean_test_result")
                         ),
                         
                         withSpinner(
                           plotlyOutput("mean_test_plot", height = "300px"),
                           color = "#3c8dbc"
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.mean_test_done == true",
                
                tags$hr(),
                
                fluidRow(
                  column(12,
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Interpretasi Hasil", style = "margin-bottom: 10px;"),
                           verbatimTextOutput("mean_test_interpretation")
                         )
                  )
                ),
                
                tags$div(
                  class = "download-section",
                  tags$h5("Download Options", style = "margin-bottom: 15px;"),
                  
                  fluidRow(
                    column(4,
                           downloadButton("download_mean_test_results", "Download Results (TXT)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_mean_test_plot", "Download Plot (PNG)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_mean_test_report", "Download Full Report (PDF)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h4("Silakan load data terlebih dahulu di menu Manajemen Data", 
                        style = "color: #999;")
              )
            )
          )
        )
      ),
      
      # PROPORTION TESTS TAB
      tabItem(
        tabName = "proportion_tests",
        fluidRow(
          box(
            title = "Uji Proporsi & Variance", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              tabsetPanel(
                
                # Proportion Tests
                tabPanel("Uji Proporsi",
                         
                         fluidRow(
                           column(4,
                                  tags$h4("Uji Proporsi", style = "color: #3c8dbc; margin: 20px 0;"),
                                  
                                  selectInput("prop_test_type", "Tipe Uji:",
                                              choices = list(
                                                "One Sample Proportion" = "one_prop",
                                                "Two Sample Proportion" = "two_prop"
                                              ),
                                              width = "100%"),
                                  
                                  selectInput("prop_variable", "Pilih Variabel:",
                                              choices = NULL,
                                              width = "100%"),
                                  
                                  conditionalPanel(
                                    condition = "input.prop_test_type == 'one_prop'",
                                    numericInput("prop_test_value", "Proporsi Uji (p₀):", 
                                                 value = 0.5, min = 0, max = 1, step = 0.01,
                                                 width = "100%")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.prop_test_type == 'two_prop'",
                                    selectInput("prop_group_variable", "Pilih Variabel Grup:",
                                                choices = NULL,
                                                width = "100%")
                                  ),
                                  
                                  selectInput("prop_alternative", "Alternative Hypothesis:",
                                              choices = list(
                                                "Two-sided" = "two.sided",
                                                "Greater than" = "greater",
                                                "Less than" = "less"
                                              ),
                                              width = "100%"),
                                  
                                  actionButton("run_prop_test", "Run Test",
                                               class = "btn btn-primary",
                                               style = "width: 100%; margin-bottom: 20px;")
                           ),
                           
                           column(8,
                                  conditionalPanel(
                                    condition = "output.prop_test_done == true",
                                    
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Hasil Uji Proporsi", style = "margin-bottom: 15px;"),
                                      verbatimTextOutput("prop_test_result")
                                    ),
                                    
                                    withSpinner(
                                      plotlyOutput("prop_test_plot", height = "300px"),
                                      color = "#3c8dbc"
                                    )
                                  )
                           )
                         )
                ),
                
                # Variance Tests
                tabPanel("Uji Variance",
                         
                         fluidRow(
                           column(4,
                                  tags$h4("Uji Variance", style = "color: #3c8dbc; margin: 20px 0;"),
                                  
                                  selectInput("var_test_type", "Tipe Uji:",
                                              choices = list(
                                                "One Sample Variance" = "one_var",
                                                "Two Sample Variance (F-test)" = "two_var"
                                              ),
                                              width = "100%"),
                                  
                                  selectInput("var_variable", "Pilih Variabel:",
                                              choices = NULL,
                                              width = "100%"),
                                  
                                  conditionalPanel(
                                    condition = "input.var_test_type == 'one_var'",
                                    numericInput("var_test_value", "Variance Uji (σ²₀):", 
                                                 value = 1, min = 0, step = 0.01,
                                                 width = "100%")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.var_test_type == 'two_var'",
                                    selectInput("var_group_variable", "Pilih Variabel Grup:",
                                                choices = NULL,
                                                width = "100%")
                                  ),
                                  
                                  selectInput("var_alternative", "Alternative Hypothesis:",
                                              choices = list(
                                                "Two-sided" = "two.sided",
                                                "Greater than" = "greater",
                                                "Less than" = "less"
                                              ),
                                              width = "100%"),
                                  
                                  actionButton("run_var_test", "Run Test",
                                               class = "btn btn-primary",
                                               style = "width: 100%; margin-bottom: 20px;")
                           ),
                           
                           column(8,
                                  conditionalPanel(
                                    condition = "output.var_test_done == true",
                                    
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Hasil Uji Variance", style = "margin-bottom: 15px;"),
                                      verbatimTextOutput("var_test_result")
                                    ),
                                    
                                    withSpinner(
                                      plotlyOutput("var_test_plot", height = "300px"),
                                      color = "#3c8dbc"
                                    )
                                  )
                           )
                         )
                )
              ),
              
              conditionalPanel(
                condition = "output.prop_test_done == true || output.var_test_done == true",
                
                tags$hr(),
                
                fluidRow(
                  column(12,
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Interpretasi Hasil", style = "margin-bottom: 10px;"),
                           verbatimTextOutput("prop_var_interpretation")
                         )
                  )
                ),
                
                tags$div(
                  class = "download-section",
                  tags$h5("Download Options", style = "margin-bottom: 15px;"),
                  
                  fluidRow(
                    column(4,
                           downloadButton("download_prop_var_results", "Download Results (TXT)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_prop_var_plots", "Download Plots (PNG)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_prop_var_report", "Download Full Report (PDF)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h4("Silakan load data terlebih dahulu di menu Manajemen Data", 
                        style = "color: #999;")
              )
            )
          )
        )
      ),
      
      # ANOVA TAB
      tabItem(
        tabName = "anova",
        fluidRow(
          box(
            title = "ANOVA (Analysis of Variance)", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(4,
                       selectInput("anova_type", "Tipe ANOVA:",
                                   choices = list(
                                     "One-Way ANOVA" = "one_way",
                                     "Two-Way ANOVA" = "two_way"
                                   ),
                                   width = "100%"),
                       
                       selectInput("anova_dependent", "Variabel Dependen:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("anova_factor1", "Faktor 1:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       conditionalPanel(
                         condition = "input.anova_type == 'two_way'",
                         selectInput("anova_factor2", "Faktor 2:",
                                     choices = NULL,
                                     width = "100%"),
                         
                         checkboxInput("anova_interaction", "Include Interaction", 
                                       value = TRUE)
                       ),
                       
                       numericInput("anova_alpha", "Alpha Level:", 
                                    value = 0.05, min = 0.01, max = 0.1, step = 0.01,
                                    width = "100%"),
                       
                       actionButton("run_anova", "Run ANOVA",
                                    class = "btn btn-primary",
                                    style = "width: 100%; margin-bottom: 20px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.anova_done == true",
                         
                         tags$div(
                           class = "stat-card",
                           tags$h5("ANOVA Results", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("anova_result")
                         ),
                         
                         withSpinner(
                           plotlyOutput("anova_plot", height = "400px"),
                           color = "#3c8dbc"
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.anova_done == true",
                
                tags$hr(),
                
                fluidRow(
                  column(6,
                         tags$div(
                           class = "stat-card",
                           tags$h5("Post-hoc Tests", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("posthoc_result")
                         )
                  ),
                  
                  column(6,
                         tags$div(
                           class = "stat-card",
                           tags$h5("Effect Size", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("effect_size_result")
                         )
                  )
                ),
                
                fluidRow(
                  column(12,
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Interpretasi ANOVA", style = "margin-bottom: 10px;"),
                           verbatimTextOutput("anova_interpretation")
                         )
                  )
                ),
                
                tags$div(
                  class = "download-section",
                  tags$h5("Download Options", style = "margin-bottom: 15px;"),
                  
                  fluidRow(
                    column(4,
                           downloadButton("download_anova_results", "Download Results (TXT)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_anova_plots", "Download Plots (PNG)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_anova_report", "Download Full Report (PDF)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h4("Silakan load data terlebih dahulu di menu Manajemen Data", 
                        style = "color: #999;")
              )
            )
          )
        )
      ),
      
      # REGRESSION TAB
      tabItem(
        tabName = "regression",
        fluidRow(
          box(
            title = "Regresi Linear Berganda", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(4,
                       tags$h4("Model Specification", style = "color: #3c8dbc; margin-bottom: 20px;"),
                       
                       selectInput("reg_dependent", "Variabel Dependen (Y):",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("reg_independent", "Variabel Independen (X):",
                                   choices = NULL,
                                   multiple = TRUE,
                                   width = "100%"),
                       
                       checkboxInput("reg_intercept", "Include Intercept", 
                                     value = TRUE),
                       
                       actionButton("run_regression", "Run Regression",
                                    class = "btn btn-primary",
                                    style = "width: 100%; margin-bottom: 20px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.regression_done == true",
                         
                         tags$div(
                           class = "stat-card",
                           tags$h5("Regression Results", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("regression_result")
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.regression_done == true",
                
                tags$hr(),
                
                tabsetPanel(
                  
                  # Model Summary
                  tabPanel("Model Summary",
                           fluidRow(
                             column(6,
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Model Fit Statistics", style = "margin-bottom: 15px;"),
                                      verbatimTextOutput("model_fit_stats")
                                    )
                             ),
                             
                             column(6,
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Coefficients", style = "margin-bottom: 15px;"),
                                      DT::dataTableOutput("coefficients_table")
                                    )
                             )
                           )
                  ),
                  
                  # Diagnostics
                  tabPanel("Diagnostic Plots",
                           fluidRow(
                             column(6,
                                    withSpinner(
                                      plotlyOutput("residual_plot", height = "350px"),
                                      color = "#3c8dbc"
                                    )
                             ),
                             
                             column(6,
                                    withSpinner(
                                      plotlyOutput("qq_plot", height = "350px"),
                                      color = "#3c8dbc"
                                    )
                             )
                           ),
                           
                           fluidRow(
                             column(6,
                                    withSpinner(
                                      plotlyOutput("scale_location_plot", height = "350px"),
                                      color = "#3c8dbc"
                                    )
                             ),
                             
                             column(6,
                                    withSpinner(
                                      plotlyOutput("leverage_plot", height = "350px"),
                                      color = "#3c8dbc"
                                    )
                             )
                           )
                  ),
                  
                  # Assumption Tests
                  tabPanel("Assumption Tests",
                           fluidRow(
                             column(6,
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Normality Tests", style = "margin-bottom: 15px;"),
                                      verbatimTextOutput("regression_normality")
                                    )
                             ),
                             
                             column(6,
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Homoscedasticity Tests", style = "margin-bottom: 15px;"),
                                      verbatimTextOutput("regression_homoscedasticity")
                                    )
                             )
                           ),
                           
                           fluidRow(
                             column(6,
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Multicollinearity (VIF)", style = "margin-bottom: 15px;"),
                                      verbatimTextOutput("regression_vif")
                                    )
                             ),
                             
                             column(6,
                                    tags$div(
                                      class = "stat-card",
                                      tags$h5("Durbin-Watson Test", style = "margin-bottom: 15px;"),
                                      verbatimTextOutput("regression_dw")
                                    )
                             )
                           )
                  )
                ),
                
                fluidRow(
                  column(12,
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Interpretasi Regresi", style = "margin-bottom: 10px;"),
                           verbatimTextOutput("regression_interpretation")
                         )
                  )
                ),
                
                tags$div(
                  class = "download-section",
                  tags$h5("Download Options", style = "margin-bottom: 15px;"),
                  
                  fluidRow(
                    column(3,
                           downloadButton("download_regression_results", "Download Results (TXT)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(3,
                           downloadButton("download_regression_plots", "Download Plots (PNG)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(3,
                           downloadButton("download_regression_assumptions", "Download Assumptions (TXT)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(3,
                           downloadButton("download_regression_report", "Download Full Report (PDF)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h4("Silakan load data terlebih dahulu di menu Manajemen Data", 
                        style = "color: #999;")
              )
            )
          )
        )
      ),
      
      # SPATIAL MAPPING TAB (NEW)
      tabItem(
        tabName = "spatial_mapping",
        fluidRow(
          box(
            title = "Pemetaan Spasial dan Analisis Autokorelasi", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(4,
                       tags$h4("Pengaturan Peta", style = "color: #3c8dbc; margin-bottom: 20px;"),
                       
                       selectInput("map_variable", "Pilih Variabel untuk Dipetakan:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("map_type", "Tipe Peta:",
                                   choices = list(
                                     "Choropleth Map" = "choropleth",
                                     "Point Map" = "point",
                                     "Heat Map" = "heat"
                                   ),
                                   width = "100%"),
                       
                       selectInput("color_scheme", "Skema Warna:",
                                   choices = list(
                                     "Blues" = "Blues",
                                     "Reds" = "Reds",
                                     "Greens" = "Greens",
                                     "Viridis" = "viridis",
                                     "Plasma" = "plasma"
                                   ),
                                   width = "100%"),
                       
                       numericInput("map_bins", "Jumlah Kelas:", 
                                    value = 5, min = 3, max = 10, width = "100%"),
                       
                       actionButton("generate_map", "Generate Map",
                                    class = "btn btn-primary",
                                    style = "width: 100%; margin-bottom: 20px;"),
                       
                       tags$hr(),
                       
                       tags$h4("Analisis Autokorelasi Spasial", style = "color: #3c8dbc; margin-bottom: 20px;"),
                       
                       selectInput("spatial_variable", "Variabel untuk Analisis Spasial:",
                                   choices = NULL,
                                   width = "100%"),
                       
                       selectInput("weight_type", "Tipe Matriks Penimbang:",
                                   choices = list(
                                     "Distance-based (Inverse)" = "distance_inverse",
                                     "Distance-based (Exponential)" = "distance_exp",
                                     "K-Nearest Neighbors" = "knn"
                                   ),
                                   width = "100%"),
                       
                       conditionalPanel(
                         condition = "input.weight_type == 'knn'",
                         numericInput("k_neighbors", "Jumlah Tetangga (k):", 
                                      value = 5, min = 3, max = 20, width = "100%")
                       ),
                       
                       actionButton("run_spatial_analysis", "Run Spatial Analysis",
                                    class = "btn btn-success",
                                    style = "width: 100%; margin-bottom: 20px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.map_generated == true",
                         
                         tags$div(
                           class = "stat-card",
                           tags$h5("Peta Interaktif", style = "margin-bottom: 15px;"),
                           withSpinner(
                             leafletOutput("spatial_map", height = "500px"),
                             color = "#3c8dbc"
                           )
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.spatial_analysis_done == true",
                
                tags$hr(),
                
                fluidRow(
                  column(6,
                         tags$div(
                           class = "stat-card",
                           tags$h5("Hasil Analisis Autokorelasi Spasial", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("spatial_autocorr_result")
                         )
                  ),
                  
                  column(6,
                         tags$div(
                           class = "stat-card",
                           tags$h5("Statistik Matriks Jarak", style = "margin-bottom: 15px;"),
                           verbatimTextOutput("distance_matrix_stats")
                         )
                  )
                ),
                
                fluidRow(
                  column(12,
                         tags$div(
                           class = "interpretation-box",
                           tags$h5("Interpretasi Analisis Spasial", style = "margin-bottom: 10px;"),
                           verbatimTextOutput("spatial_interpretation")
                         )
                  )
                ),
                
                tags$div(
                  class = "download-section",
                  tags$h5("Download Options", style = "margin-bottom: 15px;"),
                  
                  fluidRow(
                    column(3,
                           downloadButton("download_map", "Download Map (PNG)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(3,
                           downloadButton("download_spatial_results", "Download Results (TXT)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(3,
                           downloadButton("download_distance_matrix", "Download Distance Matrix (CSV)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    ),
                    column(3,
                           downloadButton("download_spatial_report", "Download Full Report (PDF)",
                                          class = "btn btn-success btn-download",
                                          style = "width: 100%;")
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              tags$div(
                style = "text-align: center; padding: 50px;",
                tags$h4("Silakan load data terlebih dahulu di menu Manajemen Data", 
                        style = "color: #999;")
              )
            )
          )
        )
      )
    )
  )
)