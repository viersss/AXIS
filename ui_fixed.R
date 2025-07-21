library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = span(
      tags$i(class = "fas fa-chart-line", style = "color: #fff; margin-right: 8px; font-size: 18px;"),
      "AXIS Dashboard",
      style = "font-weight: 600; font-size: 18px; letter-spacing: 0.3px;"
    ),
    titleWidth = 280
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 280,
    tags$head(
      tags$style(HTML("
        .skin-blue .main-sidebar {
          background-color: #00809D !important;
        }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li:hover > a {
          background-color: rgba(255, 255, 255, 0.15) !important;
          border-radius: 8px !important;
          margin: 2px 8px !important;
          color: #ffffff !important;
        }
        .skin-blue .sidebar-menu > li > a {
          color: rgba(255, 255, 255, 0.9) !important;
          padding: 12px 16px !important;
          margin: 1px 8px !important;
          border-radius: 8px !important;
          transition: all 0.3s ease !important;
          font-weight: 500 !important;
          font-size: 14px !important;
        }
        .skin-blue .sidebar-menu > li > a > .fa,
        .skin-blue .sidebar-menu > li > a > .fas {
          margin-right: 10px !important;
          font-size: 14px !important;
        }
      "))
    ),
    
    div(style = "padding: 20px 15px 15px 15px; border-bottom: 1px solid rgba(255,255,255,0.1);",
        h4("Statistical Analysis", 
           style = "color: rgba(255,255,255,0.9); margin: 0; font-weight: 300; font-size: 15px; text-align: center;")
    ),
    
    sidebarMenu(
      id = "tabs",
      
      # Menu Items with modern styling
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "exploration", icon = icon("search")),
      menuItem("Uji Asumsi Data", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("Uji Beda Rata-rata", tabName = "mean_tests", icon = icon("chart-line")),
      menuItem("Uji Proporsi & Variance", tabName = "proportion_tests", icon = icon("pie-chart")),
      menuItem("ANOVA", tabName = "anova", icon = icon("chart-area")),
      menuItem("Regresi Linear Berganda", tabName = "regression", icon = icon("project-diagram")),
      menuItem("Pemetaan Spasial", tabName = "spatial_mapping", icon = icon("map")),
      menuItem("Laporan Komprehensif", tabName = "laporan_komprehensif", icon = icon("file-alt"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    
    # Modern Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"),
      tags$style(HTML("
        /* Global Styles */
        body, .content-wrapper, .right-side {
          background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%) !important;
          font-family: 'Inter', sans-serif !important;
          color: #334155 !important;
          font-size: 14px !important;
          line-height: 1.5 !important;
        }
        
        /* Header Styling */
        .main-header .navbar {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          border: none !important;
          box-shadow: 0 4px 16px rgba(0, 128, 157, 0.15) !important;
        }
        
        /* Content Area */
        .content {
          padding: 20px !important;
          min-height: calc(100vh - 50px) !important;
        }
        
        /* Modern Box Styling */
        .box {
          border: none !important;
          border-radius: 16px !important;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08) !important;
          background: #ffffff !important;
          margin-bottom: 20px !important;
          overflow: hidden !important;
          transition: all 0.3s ease !important;
        }
        
        .box:hover {
          box-shadow: 0 8px 30px rgba(0, 0, 0, 0.12) !important;
          transform: translateY(-2px) !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: #ffffff !important;
          padding: 20px 24px !important;
          border: none !important;
          font-weight: 600 !important;
          font-size: 18px !important;
        }
        
        .box-header .box-title {
          font-size: 18px !important;
          font-weight: 600 !important;
          letter-spacing: 0.3px !important;
        }
        
        .box-body {
          padding: 24px !important;
          background: #ffffff !important;
        }
        
        /* Hero Section */
        .hero-section {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          border-radius: 20px !important;
          padding: 40px 30px !important;
          margin: 15px 0 30px 0 !important;
          text-align: center !important;
          color: white !important;
          position: relative !important;
          box-shadow: 0 15px 35px rgba(0, 128, 157, 0.25) !important;
          overflow: hidden !important;
        }
        
        .hero-section h1 {
          font-size: 3.5rem !important;
          font-weight: 800 !important;
          margin: 15px 0 !important;
          letter-spacing: -1px !important;
        }
        
        .hero-section h2 {
          font-size: 1.4rem !important;
          font-weight: 400 !important;
          margin-bottom: 20px !important;
          opacity: 0.95 !important;
        }
        
        .hero-section p {
          font-size: 15px !important;
          line-height: 1.6 !important;
          max-width: 600px !important;
          margin: 0 auto 25px auto !important;
          opacity: 0.9 !important;
        }
        
        /* Analysis Cards */
        .analysis-card {
          background: white !important; 
          border-radius: 16px !important; 
          padding: 24px !important; 
          margin-bottom: 20px !important; 
          box-shadow: 0 4px 20px rgba(0,0,0,0.08) !important; 
          border: 2px solid transparent !important; 
          transition: all 0.3s ease !important; 
          cursor: pointer !important; 
          text-align: center !important;
          height: 160px !important;
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
        }
        
        .analysis-card:hover {
          border-color: #00809D !important;
          box-shadow: 0 8px 30px rgba(0, 128, 157, 0.15) !important;
          transform: translateY(-4px) !important;
        }
        
        .analysis-card .icon-container {
          width: 60px !important;
          height: 60px !important;
          background: linear-gradient(135deg, #00809D, #006B85) !important;
          border-radius: 12px !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          margin: 0 auto 15px auto !important;
          box-shadow: 0 4px 15px rgba(0, 128, 157, 0.3) !important;
        }
        
        .analysis-card h4 {
          font-size: 1.1rem !important; 
          font-weight: 600 !important; 
          color: #2D3748 !important; 
          margin-bottom: 8px !important;
        }
        
        .analysis-card p {
          color: #718096 !important; 
          font-size: 0.9rem !important; 
          line-height: 1.4 !important; 
          margin: 0 !important;
        }
        
        /* Overview Cards */
        .overview-card {
          background: white !important;
          border-radius: 16px !important;
          padding: 24px !important;
          margin-bottom: 20px !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.08) !important;
          border: 2px solid transparent !important;
          transition: all 0.3s ease !important;
          text-align: center !important;
          height: 200px !important;
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
        }
        
        .overview-card:hover {
          transform: translateY(-4px) !important;
          border-color: rgba(0, 128, 157, 0.2) !important;
        }
        
        .overview-icon {
          width: 60px !important;
          height: 60px !important;
          background: linear-gradient(135deg, #00809D, #006B85) !important;
          border-radius: 12px !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          margin: 0 auto 15px auto !important;
          box-shadow: 0 4px 15px rgba(0, 128, 157, 0.3) !important;
        }
        
        .overview-icon i {
          color: white !important;
          font-size: 24px !important;
        }
        
        .overview-number {
          font-size: 2.5rem !important;
          font-weight: 700 !important;
          color: #00809D !important;
          margin-bottom: 8px !important;
        }
        
        .overview-title {
          font-size: 1rem !important;
          font-weight: 600 !important;
          color: #2D3748 !important;
          margin-bottom: 5px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }
        
        .overview-subtitle {
          font-size: 0.85rem !important;
          color: #718096 !important;
          line-height: 1.3 !important;
          margin: 0 !important;
        }
        
        /* Metadata Cards */
        .metadata-card {
          background: white !important; 
          border-radius: 16px !important; 
          padding: 20px !important; 
          margin-bottom: 20px !important; 
          box-shadow: 0 4px 20px rgba(0,0,0,0.08) !important; 
          border-left: 4px solid #00809D !important;
          transition: all 0.3s ease !important;
          height: auto !important;
          min-height: 180px !important;
        }
        
        .metadata-card:hover {
          box-shadow: 0 8px 30px rgba(0, 128, 157, 0.12) !important;
          transform: translateY(-2px) !important;
        }
        
        .metadata-card h4 {
          font-size: 1.1rem !important; 
          font-weight: 600 !important; 
          color: #2D3748 !important; 
          margin-bottom: 15px !important;
        }
        
        .metadata-card p {
          margin-bottom: 8px !important; 
          color: #4A5568 !important;
          font-size: 13px !important;
          line-height: 1.4 !important;
        }
        
        /* Province Button */
        .province-btn {
          background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%) !important;
          border: 2px solid #cbd5e1 !important;
          border-radius: 8px !important;
          padding: 8px 16px !important;
          font-size: 12px !important;
          font-weight: 600 !important;
          color: #475569 !important;
          transition: all 0.3s ease !important;
          margin-top: 10px !important;
          width: 100% !important;
        }
        
        .province-btn:hover {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          border-color: #00809D !important;
          color: white !important;
          transform: translateY(-1px) !important;
        }
        
        /* Section Headers */
        h2 {
          font-size: 1.8rem !important;
          font-weight: 600 !important;
          color: #2D3748 !important;
          margin-bottom: 15px !important;
        }
        
        h3 {
          font-size: 1.5rem !important;
          font-weight: 500 !important;
          color: #00809D !important;
          margin-bottom: 15px !important;
        }
        
        h4 {
          font-size: 1.2rem !important;
          color: #00809D !important;
          margin-bottom: 15px !important;
        }
        
        h5 {
          font-size: 1rem !important;
          font-weight: 600 !important;
          margin-bottom: 10px !important;
        }
        
        p {
          font-size: 14px !important;
          line-height: 1.5 !important;
          margin-bottom: 10px !important;
        }
        
        /* Modern Buttons */
        .btn {
          border-radius: 8px !important;
          padding: 10px 20px !important;
          font-weight: 500 !important;
          font-size: 14px !important;
          transition: all 0.3s ease !important;
          border: none !important;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: #ffffff !important;
          box-shadow: 0 2px 8px rgba(0, 128, 157, 0.3) !important;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #006B85 0%, #005A73 100%) !important;
          box-shadow: 0 4px 12px rgba(0, 128, 157, 0.4) !important;
          transform: translateY(-1px) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #10b981 0%, #059669 100%) !important;
          color: #ffffff !important;
          box-shadow: 0 2px 8px rgba(16, 185, 129, 0.3) !important;
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #059669 0%, #047857 100%) !important;
          box-shadow: 0 4px 12px rgba(16, 185, 129, 0.4) !important;
          transform: translateY(-1px) !important;
        }
        
        .btn-lg {
          padding: 12px 24px !important;
          font-size: 16px !important;
          font-weight: 600 !important;
        }
        
        /* Form Controls */
        .form-control, .selectize-input {
          border-radius: 8px !important;
          border: 2px solid #e2e8f0 !important;
          padding: 10px 12px !important;
          font-size: 14px !important;
          transition: all 0.3s ease !important;
          background: #ffffff !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #00809D !important;
          box-shadow: 0 0 0 3px rgba(0, 128, 157, 0.1) !important;
          outline: none !important;
        }
        
        /* Footer Styling */
        .footer-section {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          border-radius: 16px !important;
          padding: 30px !important;
          margin-top: 30px !important;
          color: white !important;
        }
        
        .footer-section h5 {
          font-weight: 600 !important;
          margin-bottom: 12px !important;
          font-size: 1rem !important;
        }
        
        .footer-section p {
          margin-bottom: 6px !important;
          opacity: 0.9 !important;
          font-size: 13px !important;
        }
        
        /* Interpretation Box */
        .interpretation-box {
          background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%) !important;
          border: none !important;
          border-left: 4px solid #00809D !important;
          padding: 20px !important;
          margin-top: 20px !important;
          border-radius: 12px !important;
          box-shadow: 0 2px 10px rgba(0, 128, 157, 0.1) !important;
        }
        
        .interpretation-box h5 {
          color: #1e40af !important;
          font-weight: 600 !important;
          margin-bottom: 10px !important;
        }
        
        /* Download Section */
        .download-section {
          background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%) !important;
          padding: 20px !important;
          border-radius: 12px !important;
          margin-top: 20px !important;
          border: 1px solid #e2e8f0 !important;
        }
        
        .download-section h5 {
          color: #475569 !important;
          font-weight: 600 !important;
          margin-bottom: 15px !important;
        }
        
        /* Stat Card */
        .stat-card {
          background: #ffffff !important;
          border-radius: 12px !important;
          padding: 20px !important;
          margin-bottom: 15px !important;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.08) !important;
          border: 1px solid #e2e8f0 !important;
          transition: all 0.3s ease !important;
        }
        
        .stat-card:hover {
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.12) !important;
          transform: translateY(-2px) !important;
        }
        
        .stat-card h5 {
          color: #1e293b !important;
          font-weight: 600 !important;
          margin-bottom: 12px !important;
        }
        
        /* Empty State */
        .empty-state {
          text-align: center !important;
          padding: 60px 20px !important;
          color: #94a3b8 !important;
        }
        
        .empty-state h4 {
          color: #94a3b8 !important;
          font-weight: 400 !important;
          font-size: 18px !important;
        }
        
        /* Responsive Design */
        @media (max-width: 768px) {
          .content {
            padding: 15px !important;
          }
          
          .hero-section {
            padding: 30px 20px !important;
          }
          
          .hero-section h1 {
            font-size: 2.5rem !important;
          }
          
          .hero-section h2 {
            font-size: 1.2rem !important;
          }
          
          .analysis-card {
            height: auto !important;
            min-height: 140px !important;
          }
          
          .overview-card {
            height: auto !important;
            min-height: 160px !important;
          }
          
          .metadata-card {
            min-height: 150px !important;
          }
        }
        
        /* Loading Spinner */
        .shiny-spinner-output-container {
          background: rgba(255, 255, 255, 0.95) !important;
          border-radius: 12px !important;
        }
        
        /* Modal Styling */
        .modal-content {
          border-radius: 16px !important;
          border: none !important;
          box-shadow: 0 20px 60px rgba(0,0,0,0.15) !important;
        }
        
        .modal-header {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: white !important;
          border-radius: 16px 16px 0 0 !important;
          padding: 20px 25px !important;
        }
        
        .modal-body {
          padding: 25px !important;
        }
        
        .modal-footer {
          border: none !important;
          padding: 15px 25px 25px 25px !important;
        }
      "))
    ),
    
    tabItems(
      
      # BERANDA TAB
      tabItem(
        tabName = "beranda",
        
        # Hero Section
        fluidRow(
          column(12,
                 tags$div(
                   class = "hero-section",
                   
                   # Content
                   tags$div(
                     tags$div(
                       style = "display: inline-flex; align-items: center; 
                               background: rgba(255,255,255,0.15); 
                               padding: 12px 24px; 
                               border-radius: 50px; 
                               margin-bottom: 25px;",
                       tags$i(class = "fas fa-chart-line", style = "margin-right: 10px; font-size: 16px;"),
                       tags$span("UAS Komputasi Statistik 2025", style = "font-weight: 600; font-size: 14px;")
                     ),
                     
                     tags$h1("AXIS"),
                     
                     tags$h2("Advanced eXploratory Inference Statistics"),
                     
                     tags$p("Platform analisis modern untuk penelitian kerentanan sosial dengan tools statistik komprehensif, visualisasi interaktif, dan analisis spasial yang terintegrasi.")
                   )
                 )
          )
        ),
        
        # Menu Analisis Section
        fluidRow(
          column(12,
                 tags$div(
                   style = "text-align: center; margin-bottom: 30px;",
                   tags$h2(
                     tags$i(class = "fas fa-rocket", style = "margin-right: 12px; color: #00809D;"),
                     "Menu Analisis"
                   ),
                   tags$p("Pilihan menu analisis untuk eksplorasi data kerentanan sosial",
                          style = "color: #718096; max-width: 500px; margin: 0 auto;")
                 )
          )
        ),
        
        # Analysis Menu Cards
        fluidRow(
          # Row 1
          column(4,
                 tags$div(
                   class = "analysis-card",
                   onclick = "Shiny.setInputValue('tabs', 'data_management', {priority: 'event'});",
                   
                   tags$div(
                     class = "icon-container",
                     tags$i(class = "fas fa-database", style = "color: white; font-size: 24px;")
                   ),
                   
                   tags$h4("Kelola Data"),
                   tags$p("Preprocessing dan transformasi dataset")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "analysis-card",
                   onclick = "Shiny.setInputValue('tabs', 'exploration', {priority: 'event'});",
                   
                   tags$div(
                     class = "icon-container",
                     tags$i(class = "fas fa-chart-bar", style = "color: white; font-size: 24px;")
                   ),
                   
                   tags$h4("Eksplorasi"),
                   tags$p("Analisis deskriptif dan visualisasi")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "analysis-card",
                   onclick = "Shiny.setInputValue('tabs', 'assumptions', {priority: 'event'});",
                   
                   tags$div(
                     class = "icon-container",
                     tags$i(class = "fas fa-check-circle", style = "color: white; font-size: 24px;")
                   ),
                   
                   tags$h4("Uji Asumsi"),
                   tags$p("Validasi normalitas dan homogenitas")
                 )
          )
        ),
        
        # Row 2
        fluidRow(
          column(4,
                 tags$div(
                   class = "analysis-card",
                   onclick = "Shiny.setInputValue('tabs', 'mean_tests', {priority: 'event'});",
                   
                   tags$div(
                     class = "icon-container",
                     tags$i(class = "fas fa-calculator", style = "color: white; font-size: 24px;")
                   ),
                   
                   tags$h4("Inferensia"),
                   tags$p("Uji hipotesis dan estimasi")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "analysis-card",
                   onclick = "Shiny.setInputValue('tabs', 'regression', {priority: 'event'});",
                   
                   tags$div(
                     class = "icon-container",
                     tags$i(class = "fas fa-chart-line", style = "color: white; font-size: 24px;")
                   ),
                   
                   tags$h4("Regresi"),
                   tags$p("Analisis hubungan antar variabel")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "analysis-card",
                   onclick = "Shiny.setInputValue('tabs', 'spatial_mapping', {priority: 'event'});",
                   
                   tags$div(
                     class = "icon-container",
                     tags$i(class = "fas fa-globe", style = "color: white; font-size: 24px;")
                   ),
                   
                   tags$h4("Spasial"),
                   tags$p("Analisis geografis dan autokorelasi")
                 )
          )
        ),
        
        # Overview Dataset Section
        fluidRow(
          column(12,
                 tags$div(
                   style = "text-align: center; margin: 40px 0 30px 0;",
                   tags$h2(
                     tags$i(class = "fas fa-chart-line", style = "margin-right: 12px; color: #00809D;"),
                     "Overview Dataset"
                   ),
                   tags$p("Ringkasan komprehensif dari dataset Social Vulnerability Index",
                          style = "color: #718096; max-width: 500px; margin: 0 auto;")
                 )
          )
        ),
        
        # Dataset Statistics Cards
        fluidRow(
          column(4,
                 tags$div(
                   class = "overview-card",
                   
                   tags$div(
                     class = "overview-icon",
                     tags$i(class = "fas fa-table")
                   ),
                   
                   tags$div(class = "overview-number", "511"),
                   tags$div(class = "overview-title", "Total Observasi"),
                   tags$div(class = "overview-subtitle", "Kabupaten/Kota dalam dataset")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "overview-card",
                   
                   tags$div(
                     class = "overview-icon",
                     tags$i(class = "fas fa-list")
                   ),
                   
                   tags$div(class = "overview-number", "17"),
                   tags$div(class = "overview-title", "Indikator SoVI"),
                   tags$div(class = "overview-subtitle", "Variabel kerentanan sosial")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "overview-card",
                   
                   tags$div(
                     class = "overview-icon",
                     tags$i(class = "fas fa-map-marked-alt")
                   ),
                   
                   tags$div(class = "overview-number", "38"),
                   tags$div(class = "overview-title", "Provinsi Indonesia"),
                   tags$div(class = "overview-subtitle", "Cakupan wilayah dalam data"),
                   
                   # Button to show province list
                   actionButton("show_provinces", "Lihat Daftar Provinsi",
                                class = "province-btn",
                                `data-toggle` = "modal",
                                `data-target` = "#provinceModal")
                 )
          )
        ),
        
        # Province Modal (Simplified)
        tags$div(
          class = "modal fade",
          id = "provinceModal",
          tabindex = "-1",
          role = "dialog",
          
          tags$div(
            class = "modal-dialog modal-lg",
            role = "document",
            
            tags$div(
              class = "modal-content",
              
              tags$div(
                class = "modal-header",
                tags$h4(
                  class = "modal-title",
                  tags$i(class = "fas fa-map-marked-alt", style = "margin-right: 10px;"),
                  "Daftar 38 Provinsi Indonesia"
                ),
                tags$button(
                  type = "button",
                  class = "close",
                  `data-dismiss` = "modal",
                  style = "color: white; opacity: 0.8;",
                  tags$span("×")
                )
              ),
              
              tags$div(
                class = "modal-body",
                tags$p("Dataset mencakup seluruh 38 provinsi di Indonesia dari Aceh hingga Papua, 
                       dengan total 511 kabupaten/kota dan 17 indikator kerentanan sosial.")
              ),
              
              tags$div(
                class = "modal-footer",
                tags$button(
                  type = "button",
                  class = "btn btn-primary",
                  `data-dismiss` = "modal",
                  "Tutup"
                )
              )
            )
          )
        ),
        
        # Metadata Section
        fluidRow(
          column(12,
                 tags$div(
                   style = "text-align: center; margin: 40px 0 30px 0;",
                   tags$h3(
                     tags$i(class = "fas fa-info-circle", style = "margin-right: 10px; color: #00809D;"),
                     "Metadata Ilmiah"
                   ),
                   tags$p("Informasi dataset berdasarkan publikasi ilmiah",
                          style = "color: #718096; max-width: 500px; margin: 0 auto;")
                 )
          )
        ),
        
        # Metadata Cards
        fluidRow(
          # Publikasi Ilmiah
          column(4,
                 tags$div(
                   class = "metadata-card",
                   
                   tags$div(
                     style = "display: flex; align-items: center; margin-bottom: 15px;",
                     tags$div(
                       style = "width: 40px; height: 40px; background: #00809D; 
                               border-radius: 8px; display: flex; align-items: center; 
                               justify-content: center; margin-right: 12px;",
                       tags$i(class = "fas fa-book", style = "color: white; font-size: 16px;")
                     ),
                     tags$h4("Publikasi Ilmiah")
                   ),
                   
                   tags$div(
                     tags$p(tags$strong("Journal:"), " Data in Brief"),
                     tags$p(tags$strong("Publisher:"), " Elsevier"),
                     tags$p(tags$strong("DOI:"), " 10.1016/j.dib.2021.107618")
                   )
                 )
          ),
          
          # Indikator Demografis
          column(4,
                 tags$div(
                   class = "metadata-card",
                   
                   tags$div(
                     style = "display: flex; align-items: center; margin-bottom: 15px;",
                     tags$div(
                       style = "width: 40px; height: 40px; background: #00809D; 
                               border-radius: 8px; display: flex; align-items: center; 
                               justify-content: center; margin-right: 12px;",
                       tags$i(class = "fas fa-users", style = "color: white; font-size: 16px;")
                     ),
                     tags$h4("Indikator Demografis")
                   ),
                   
                   tags$div(
                     tags$p(tags$strong("CHILDREN:"), " Proporsi Anak < 5 tahun"),
                     tags$p(tags$strong("FEMALE:"), " Proporsi Perempuan"),
                     tags$p(tags$strong("ELDERLY:"), " Proporsi Lansia 65+"),
                     tags$p(tags$strong("FHEAD:"), " KRT Perempuan")
                   )
                 )
          ),
          
          # Indikator Sosial-Ekonomi
          column(4,
                 tags$div(
                   class = "metadata-card",
                   
                   tags$div(
                     style = "display: flex; align-items: center; margin-bottom: 15px;",
                     tags$div(
                       style = "width: 40px; height: 40px; background: #00809D; 
                               border-radius: 8px; display: flex; align-items: center; 
                               justify-content: center; margin-right: 12px;",
                       tags$i(class = "fas fa-graduation-cap", style = "color: white; font-size: 16px;")
                     ),
                     tags$h4("Indikator Sosial-Ekonomi")
                   ),
                   
                   tags$div(
                     tags$p(tags$strong("LOWEDU:"), " Pendidikan Rendah"),
                     tags$p(tags$strong("POVERTY:"), " Tingkat Kemiskinan"),
                     tags$p(tags$strong("ILLITERATE:"), " Tingkat Buta Huruf"),
                     tags$p(tags$strong("NOTRAINING:"), " Tanpa Pelatihan Vokasi")
                   )
                 )
          )
        ),
        
        # Footer Information
        fluidRow(
          column(12,
                 tags$div(
                   class = "footer-section",
                   
                   fluidRow(
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-university", style = "margin-right: 8px;"),
                                "Informasi Akademis"
                              ),
                              tags$p("Politeknik Statistika STIS"),
                              tags$p("Program Studi Komputasi Statistik D-IV"),
                              tags$p("Mata Kuliah: Komputasi Statistik"),
                              tags$p("Semester Genap TA. 2024/2025")
                            )
                     ),
                     
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-calendar-alt", style = "margin-right: 8px;"),
                                "Jadwal UAS"
                              ),
                              tags$p("Tanggal: 23 Juli 2025"),
                              tags$p("Waktu: 10:30 - 12:30 WIB"),
                              tags$p("Durasi: 120 menit"),
                              tags$p("Sifat: Tidak Terstruktur")
                            )
                     ),
                     
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-database", style = "margin-right: 8px;"),
                                "Data Source"
                              ),
                              tags$p("Dataset: Social Vulnerability Index"),
                              tags$p("Format: CSV Files"),
                              tags$p("Metadata: ScienceDirect Publication")
                            )
                     ),
                     
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-book-open", style = "margin-right: 8px;"),
                                "Referensi Ilmiah"
                              ),
                              tags$p("Journal: Data in Brief"),
                              tags$p("Publisher: Elsevier"),
                              tags$p("DOI: 10.1016/j.dib.2021.107618")
                            )
                     )
                   ),
                   
                   tags$hr(style = "border-color: rgba(255,255,255,0.2); margin: 20px 0 15px 0;"),
                   
                   tags$div(
                     style = "text-align: center;",
                     tags$p("© 2025 Dashboard AXIS - Final Project using SoVI Data",
                            style = "margin: 0; opacity: 0.8; font-size: 12px;")
                   )
                 )
          )
        )
      ),
      
      # OTHER TABS (Simplified placeholders)
      tabItem(
        tabName = "data_management",
        fluidRow(
          box(
            title = tagList(icon("database"), "Manajemen Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur Manajemen Data")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "exploration",
        fluidRow(
          box(
            title = tagList(icon("search"), "Eksplorasi Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-chart-bar fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur Eksplorasi Data")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "assumptions",
        fluidRow(
          box(
            title = tagList(icon("check-circle"), "Uji Asumsi Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-check-circle fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur Uji Asumsi Data")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "mean_tests",
        fluidRow(
          box(
            title = tagList(icon("chart-line"), "Uji Beda Rata-rata"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-calculator fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur Uji Beda Rata-rata")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "proportion_tests",
        fluidRow(
          box(
            title = tagList(icon("pie-chart"), "Uji Proporsi & Variance"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-pie-chart fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur Uji Proporsi & Variance")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "anova",
        fluidRow(
          box(
            title = tagList(icon("chart-area"), "ANOVA"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-chart-area fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur ANOVA")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "regression",
        fluidRow(
          box(
            title = tagList(icon("project-diagram"), "Regresi Linear Berganda"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-project-diagram fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur Regresi Linear Berganda")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "spatial_mapping",
        fluidRow(
          box(
            title = tagList(icon("map"), "Pemetaan Spasial"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(class = "empty-state",
                tags$i(class = "fas fa-map fa-3x", style = "color: #cbd5e1; margin-bottom: 18px;"),
                h4("Fitur Pemetaan Spasial")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "laporan_komprehensif",
        fluidRow(
          box(
            title = tagList(icon("file-alt"), "Laporan Komprehensif"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(style = "text-align: center; padding: 40px 20px;",
                tags$i(class = "fas fa-file-alt fa-4x", style = "color: #00809D; margin-bottom: 20px;"),
                
                h3("Laporan Komprehensif", style = "margin-bottom: 15px; color: #1e293b;"),
                
                p("Unduh laporan analisis lengkap dalam format Microsoft Word (.docx)", 
                  style = "color: #64748b; margin-bottom: 25px; max-width: 600px; margin-left: auto; margin-right: auto;"),
                
                downloadButton("download_word_report", 
                               "Unduh Laporan Lengkap (.docx)", 
                               class = "btn btn-success btn-lg")
            ),
            
            div(class = "interpretation-box",
                style = "margin-top: 25px;",
                h5("Catatan"),
                p("Proses pembuatan laporan memerlukan waktu beberapa saat tergantung kompleksitas analisis.")
            )
          )
        )
      )
    )
  )
)

# Define server logic  
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data_loaded = FALSE
  )
  
  # Output data loaded status
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Event handlers
  observeEvent(input$load_data, {
    values$data_loaded <- TRUE
    showNotification("Data loaded successfully!", type = "success")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)