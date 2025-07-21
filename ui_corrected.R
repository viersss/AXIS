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
      tags$i(class = "fas fa-chart-line", style = "color: #fff; margin-right: 12px; font-size: 22px;"),
      "AXIS Dashboard",
      style = "font-weight: 600; font-size: 22px; letter-spacing: 0.5px;"
    ),
    titleWidth = 320
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 320,
    tags$head(
      tags$style(HTML("
        .skin-blue .main-sidebar {
          background-color: #00809D !important;
        }
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li:hover > a {
          background-color: rgba(255, 255, 255, 0.15) !important;
          border-radius: 12px !important;
          margin: 4px 12px !important;
          color: #ffffff !important;
        }
        .skin-blue .sidebar-menu > li > a {
          color: rgba(255, 255, 255, 0.9) !important;
          padding: 18px 22px !important;
          margin: 2px 12px !important;
          border-radius: 12px !important;
          transition: all 0.3s ease !important;
          font-weight: 500 !important;
          font-size: 15px !important;
        }
        .skin-blue .sidebar-menu > li > a > .fa,
        .skin-blue .sidebar-menu > li > a > .fas,
        .skin-blue .sidebar-menu > li > a > .glyphicon,
        .skin-blue .sidebar-menu > li > a > .ion {
          margin-right: 14px !important;
          font-size: 17px !important;
        }
      "))
    ),
    
    div(style = "padding: 28px 20px 24px 20px; border-bottom: 1px solid rgba(255,255,255,0.1);",
        h4("Statistical Analysis", 
           style = "color: rgba(255,255,255,0.9); margin: 0; font-weight: 300; font-size: 17px; text-align: center;")
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
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif !important;
          color: #334155 !important;
          font-size: 15px !important;
        }
        
        /* Header Styling */
        .main-header .navbar {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          border: none !important;
          box-shadow: 0 6px 24px rgba(0, 128, 157, 0.15) !important;
        }
        
        /* Content Area */
        .content {
          padding: 36px !important;
        }
        
        /* Modern Box Styling */
        .box {
          border: none !important;
          border-radius: 24px !important;
          box-shadow: 0 10px 40px rgba(0, 0, 0, 0.08) !important;
          background: #ffffff !important;
          margin-bottom: 36px !important;
          overflow: hidden !important;
          transition: all 0.4s ease !important;
        }
        
        .box:hover {
          box-shadow: 0 20px 60px rgba(0, 0, 0, 0.15) !important;
          transform: translateY(-4px) !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: #ffffff !important;
          padding: 28px 36px !important;
          border: none !important;
          font-weight: 600 !important;
          font-size: 20px !important;
        }
        
        .box-header .box-title {
          font-size: 20px !important;
          font-weight: 600 !important;
          letter-spacing: 0.3px !important;
        }
        
        .box-body {
          padding: 36px !important;
          background: #ffffff !important;
        }
        
        /* Modern Cards with Enhanced Hover */
        .stat-card {
          background: #ffffff !important;
          border-radius: 20px !important;
          padding: 32px !important;
          margin-bottom: 28px !important;
          box-shadow: 0 6px 25px rgba(0, 0, 0, 0.08) !important;
          border: 1px solid #e2e8f0 !important;
          transition: all 0.4s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;
          position: relative !important;
          overflow: hidden !important;
        }
        
        .stat-card::before {
          content: '' !important;
          position: absolute !important;
          top: 0 !important;
          left: -100% !important;
          width: 100% !important;
          height: 100% !important;
          background: linear-gradient(90deg, transparent, rgba(0, 128, 157, 0.1), transparent) !important;
          transition: left 0.6s ease !important;
        }
        
        .stat-card:hover::before {
          left: 100% !important;
        }
        
        .stat-card:hover {
          box-shadow: 0 15px 50px rgba(0, 0, 0, 0.12) !important;
          transform: translateY(-6px) scale(1.02) !important;
          border-color: #00809D !important;
        }
        
        .stat-card h5 {
          color: #1e293b !important;
          font-weight: 600 !important;
          font-size: 18px !important;
          margin-bottom: 18px !important;
        }
        
        .stat-value {
          font-size: 3.2em !important;
          font-weight: 700 !important;
          color: #00809D !important;
          line-height: 1.1 !important;
        }
        
        /* Analysis Cards with Professional Hover Effects */
        .analysis-card {
          background: white !important; 
          border-radius: 24px !important; 
          padding: 36px !important; 
          margin-bottom: 28px !important; 
          box-shadow: 0 10px 35px rgba(0,0,0,0.08) !important; 
          border: 2px solid transparent !important; 
          transition: all 0.4s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important; 
          cursor: pointer !important; 
          text-align: center !important;
          position: relative !important;
          overflow: hidden !important;
        }
        
        .analysis-card::after {
          content: '' !important;
          position: absolute !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          bottom: 0 !important;
          background: linear-gradient(135deg, rgba(0, 128, 157, 0.05) 0%, rgba(0, 107, 133, 0.05) 100%) !important;
          opacity: 0 !important;
          transition: opacity 0.4s ease !important;
        }
        
        .analysis-card:hover::after {
          opacity: 1 !important;
        }
        
        .analysis-card:hover {
          border-color: #00809D !important;
          box-shadow: 0 20px 60px rgba(0, 128, 157, 0.15) !important;
          transform: translateY(-8px) scale(1.03) !important;
        }
        
        .analysis-card:active {
          transform: translateY(-6px) scale(1.01) !important;
        }
        
        .analysis-card .fa, .analysis-card .fas {
          transition: all 0.4s ease !important;
        }
        
        .analysis-card:hover .fa, .analysis-card:hover .fas {
          transform: scale(1.1) rotate(5deg) !important;
        }
        
        .analysis-card h4 {
          font-size: 1.5rem !important; 
          font-weight: 700 !important; 
          color: #2D3748 !important; 
          margin-bottom: 14px !important;
          transition: color 0.3s ease !important;
        }
        
        .analysis-card:hover h4 {
          color: #00809D !important;
        }
        
        .analysis-card p {
          color: #718096 !important; 
          font-size: 1.05rem !important; 
          line-height: 1.6 !important; 
          margin: 0 !important;
          transition: color 0.3s ease !important;
        }
        
        .analysis-card:hover p {
          color: #4A5568 !important;
        }
        
        /* Metadata Cards with Enhanced Hover */
        .metadata-card {
          background: white !important; 
          border-radius: 24px !important; 
          padding: 32px !important; 
          margin-bottom: 28px !important; 
          box-shadow: 0 8px 30px rgba(0,0,0,0.08) !important; 
          border-left: 5px solid #00809D !important;
          transition: all 0.4s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;
          position: relative !important;
        }
        
        .metadata-card:hover {
          box-shadow: 0 20px 60px rgba(0, 128, 157, 0.12) !important;
          transform: translateY(-6px) translateX(8px) !important;
          border-left-width: 8px !important;
        }
        
        .metadata-card h4 {
          font-size: 1.4rem !important; 
          font-weight: 700 !important; 
          color: #2D3748 !important; 
          margin: 0 !important;
        }
        
        .metadata-card p {
          margin-bottom: 10px !important; 
          color: #4A5568 !important;
          font-size: 15px !important;
          line-height: 1.6 !important;
        }
        
        /* Overview Card Styling */
        .overview-card {
          background: #ffffff !important;
          border-radius: 20px !important;
          padding: 40px 30px !important; 
          margin-bottom: 30px !important; 
          box-shadow: 0 10px 30px rgba(0,0,0,0.08) !important; 
          border: 2px solid transparent !important;
          transition: all 0.4s cubic-bezier(0.4, 0.0, 0.2, 1) !important;
          text-align: center !important;
          position: relative !important;
          overflow: hidden !important;
        }

        .overview-card::before {
          content: '' !important;
          position: absolute !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          bottom: 0 !important;
          background: linear-gradient(135deg, rgba(0, 128, 157, 0.02) 0%, rgba(0, 107, 133, 0.02) 100%) !important;
          opacity: 0 !important;
          transition: opacity 0.4s ease !important;
        }

        .overview-card:hover::before {
          opacity: 1 !important;
        }

        .overview-card:hover {
          transform: translateY(-10px) scale(1.02) !important;
          box-shadow: 0 25px 50px rgba(0, 128, 157, 0.15) !important;
          border-color: rgba(0, 128, 157, 0.2) !important;
        }

        .overview-icon {
          width: 80px !important; 
          height: 80px !important; 
          background: linear-gradient(135deg, #00809D, #006B85) !important; 
          border-radius: 20px !important; 
          display: flex !important; 
          align-items: center !important; 
          justify-content: center !important; 
          margin: 0 auto 24px auto !important; 
          box-shadow: 0 10px 25px rgba(0, 128, 157, 0.3) !important;
          transition: all 0.4s ease !important;
        }
        
        .overview-card:hover .overview-icon {
          transform: scale(1.1) rotate(5deg) !important;
          box-shadow: 0 15px 35px rgba(0, 128, 157, 0.4) !important;
        }
        
        .overview-icon i {
          color: white !important; 
          font-size: 32px !important;
        }

        .overview-number {
          font-size: 3.5rem !important;
          font-weight: 800 !important;
          color: #00809D !important;
          line-height: 1 !important;
          margin-bottom: 12px !important;
          transition: all 0.3s ease !important;
        }

        .overview-card:hover .overview-number {
          transform: scale(1.05) !important;
        }

        .overview-title {
          font-size: 1.3rem !important;
          font-weight: 700 !important;
          color: #2D3748 !important;
          margin-bottom: 8px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }

        .overview-subtitle {
          font-size: 1.1rem !important;
          color: #718096 !important;
          line-height: 1.5 !important;
          margin: 0 !important;
        }

        /* Province List Modal Styles */
        .province-btn {
          background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%) !important;
          border: 2px solid #cbd5e1 !important;
          border-radius: 12px !important;
          padding: 12px 20px !important;
          font-size: 14px !important;
          font-weight: 600 !important;
          color: #475569 !important;
          transition: all 0.3s ease !important;
          margin-top: 16px !important;
          width: 100% !important;
        }

        .province-btn:hover {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          border-color: #00809D !important;
          color: white !important;
          transform: translateY(-2px) !important;
          box-shadow: 0 4px 12px rgba(0, 128, 157, 0.3) !important;
        }
        
        /* Interpretation Box */
        .interpretation-box {
          background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%) !important;
          border: none !important;
          border-left: 6px solid #00809D !important;
          padding: 28px !important;
          margin-top: 28px !important;
          border-radius: 16px !important;
          box-shadow: 0 4px 20px rgba(0, 128, 157, 0.1) !important;
          transition: all 0.3s ease !important;
        }
        
        .interpretation-box:hover {
          box-shadow: 0 8px 30px rgba(0, 128, 157, 0.15) !important;
          transform: translateY(-2px) !important;
        }
        
        .interpretation-box h5 {
          color: #1e40af !important;
          font-weight: 600 !important;
          margin-bottom: 14px !important;
          font-size: 18px !important;
        }
        
        /* Download Section */
        .download-section {
          background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%) !important;
          padding: 28px !important;
          border-radius: 20px !important;
          margin-top: 36px !important;
          border: 1px solid #e2e8f0 !important;
          transition: all 0.3s ease !important;
        }
        
        .download-section:hover {
          box-shadow: 0 8px 25px rgba(0, 0, 0, 0.08) !important;
          transform: translateY(-2px) !important;
        }
        
        .download-section h5 {
          color: #475569 !important;
          font-weight: 600 !important;
          margin-bottom: 24px !important;
          font-size: 18px !important;
        }
        
        /* Modern Buttons */
        .btn {
          border-radius: 14px !important;
          padding: 14px 28px !important;
          font-weight: 600 !important;
          font-size: 15px !important;
          transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;
          border: none !important;
          letter-spacing: 0.3px !important;
          position: relative !important;
          overflow: hidden !important;
        }
        
        .btn::before {
          content: '' !important;
          position: absolute !important;
          top: 50% !important;
          left: 50% !important;
          width: 0 !important;
          height: 0 !important;
          background: rgba(255, 255, 255, 0.3) !important;
          border-radius: 50% !important;
          transform: translate(-50%, -50%) !important;
          transition: width 0.3s ease, height 0.3s ease !important;
        }
        
        .btn:hover::before {
          width: 100% !important;
          height: 100% !important;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: #ffffff !important;
          box-shadow: 0 6px 20px rgba(0, 128, 157, 0.3) !important;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #006B85 0%, #005A73 100%) !important;
          box-shadow: 0 10px 30px rgba(0, 128, 157, 0.4) !important;
          transform: translateY(-3px) scale(1.05) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #10b981 0%, #059669 100%) !important;
          color: #ffffff !important;
          box-shadow: 0 6px 20px rgba(16, 185, 129, 0.3) !important;
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #059669 0%, #047857 100%) !important;
          box-shadow: 0 10px 30px rgba(16, 185, 129, 0.4) !important;
          transform: translateY(-3px) scale(1.05) !important;
        }
        
        .btn-lg {
          padding: 18px 36px !important;
          font-size: 17px !important;
          font-weight: 600 !important;
        }
        
        /* Form Controls */
        .form-control, .selectize-input {
          border-radius: 14px !important;
          border: 2px solid #e2e8f0 !important;
          padding: 14px 18px !important;
          font-size: 15px !important;
          transition: all 0.3s ease !important;
          background: #ffffff !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #00809D !important;
          box-shadow: 0 0 0 4px rgba(0, 128, 157, 0.1) !important;
          outline: none !important;
          transform: scale(1.02) !important;
        }
        
        /* Feature Items */
        .feature-item {
          margin-bottom: 36px !important;
          padding: 28px !important;
          background: #ffffff !important;
          border-radius: 20px !important;
          box-shadow: 0 6px 25px rgba(0, 0, 0, 0.08) !important;
          border: 1px solid #e2e8f0 !important;
          transition: all 0.4s cubic-bezier(0.25, 0.46, 0.45, 0.94) !important;
        }
        
        .feature-item:hover {
          box-shadow: 0 15px 50px rgba(0, 0, 0, 0.12) !important;
          transform: translateY(-6px) !important;
        }
        
        .feature-item .fas {
          color: #00809D !important;
          margin-right: 18px !important;
          vertical-align: middle !important;
          transition: transform 0.3s ease !important;
        }
        
        .feature-item:hover .fas {
          transform: scale(1.2) !important;
        }
        
        .feature-item h5 {
          font-weight: 600 !important;
          color: #1e293b !important;
          display: inline !important;
          margin-left: 18px !important;
          vertical-align: middle !important;
          font-size: 20px !important;
        }
        
        .feature-item p {
          margin: 18px 0 0 64px !important;
          color: #64748b !important;
          line-height: 1.6 !important;
          font-size: 15px !important;
        }
        
        /* Section Headers */
        h3, h4 {
          color: #1e293b !important;
          font-weight: 600 !important;
          margin-bottom: 28px !important;
        }
        
        h3 {
          font-size: 32px !important;
          font-weight: 300 !important;
          color: #00809D !important;
        }
        
        h4 {
          font-size: 22px !important;
          color: #00809D !important;
        }
        
        h2 {
          font-size: 28px !important;
          font-weight: 700 !important;
          color: #2D3748 !important;
        }
        
        h5 {
          font-size: 18px !important;
          font-weight: 600 !important;
        }
        
        p {
          font-size: 15px !important;
          line-height: 1.7 !important;
        }
        
        /* Empty State */
        .empty-state {
          text-align: center !important;
          padding: 80px 24px !important;
          color: #94a3b8 !important;
        }
        
        .empty-state h4 {
          color: #94a3b8 !important;
          font-weight: 400 !important;
          font-size: 20px !important;
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
                   style = "background: linear-gradient(135deg, #00809D 0%, #006B85 100%); 
                           border-radius: 28px; 
                           padding: 70px 50px; 
                           margin: 24px 0 50px 0; 
                           text-align: center; 
                           color: white; 
                           position: relative; 
                           box-shadow: 0 25px 50px rgba(0, 128, 157, 0.3);",
                   
                   # Background decoration
                   tags$div(
                     style = "position: absolute; top: -60px; right: -60px; 
                             width: 240px; height: 240px; 
                             background: rgba(255,255,255,0.12); 
                             border-radius: 50%; 
                             opacity: 0.6;"
                   ),
                   tags$div(
                     style = "position: absolute; bottom: -40px; left: -40px; 
                             width: 180px; height: 180px; 
                             background: rgba(255,255,255,0.08); 
                             border-radius: 50%;"
                   ),
                   
                   # Content
                   tags$div(
                     style = "position: relative; z-index: 2;",
                     
                     tags$div(
                       style = "display: inline-flex; align-items: center; 
                               background: rgba(255,255,255,0.15); 
                               padding: 16px 32px; 
                               border-radius: 60px; 
                               margin-bottom: 40px; 
                               backdrop-filter: blur(10px);",
                       tags$i(class = "fas fa-chart-line", style = "margin-right: 14px; font-size: 20px;"),
                       tags$span("UAS Komputasi Statistik 2025", style = "font-weight: 600; font-size: 17px;")
                     ),
                     
                     tags$h1("AXIS", 
                             style = "font-size: 5rem; 
                                     font-weight: 800; 
                                     margin: 25px 0; 
                                     letter-spacing: -2px; 
                                     text-shadow: 0 6px 25px rgba(0,0,0,0.2);"),
                     
                     tags$h2("Advanced eXploratory Inference Statistics", 
                             style = "font-size: 2rem; 
                                     font-weight: 400; 
                                     margin-bottom: 30px; 
                                     opacity: 0.95; 
                                     letter-spacing: 0.5px;"),
                     
                     tags$p("Platform analisis modern untuk penelitian kerentanan sosial dengan tools statistik komprehensif, visualisasi interaktif, dan analisis spasial yang terintegrasi menggunakan data Social Vulnerability Index dari penelitian ilmiah.",
                            style = "font-size: 18px; 
                                    line-height: 1.8; 
                                    max-width: 850px; 
                                    margin: 0 auto 35px auto; 
                                    opacity: 0.9;")
                   )
                 )
          )
        ),
        
        # Menu Analisis Section
        fluidRow(
          column(12,
                 tags$div(
                   style = "text-align: center; margin-bottom: 45px;",
                   tags$h2(
                     tags$i(class = "fas fa-rocket", style = "margin-right: 18px; color: #00809D;"),
                     "Menu Analisis",
                     style = "font-size: 2.5rem; 
                             font-weight: 700; 
                             color: #2D3748; 
                             margin-bottom: 18px;"
                   ),
                   tags$p("Pilihan menu analisis yang bisa Anda gunakan untuk memulai eksplorasi data kerentanan sosial berdasarkan penelitian ilmiah",
                          style = "font-size: 1.2rem; 
                                  color: #718096; 
                                  max-width: 650px; 
                                  margin: 0 auto; 
                                  line-height: 1.7;")
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
                     style = "width: 80px; 
                             height: 80px; 
                             background: linear-gradient(135deg, #00809D, #006B85); 
                             border-radius: 20px; 
                             display: flex; 
                             align-items: center; 
                             justify-content: center; 
                             margin: 0 auto 24px auto; 
                             box-shadow: 0 10px 25px rgba(0, 128, 157, 0.3);",
                     tags$i(class = "fas fa-database", style = "color: white; font-size: 32px;")
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
                     style = "width: 80px; 
                             height: 80px; 
                             background: linear-gradient(135deg, #00809D, #006B85); 
                             border-radius: 20px; 
                             display: flex; 
                             align-items: center; 
                             justify-content: center; 
                             margin: 0 auto 24px auto; 
                             box-shadow: 0 10px 25px rgba(0, 128, 157, 0.3);",
                     tags$i(class = "fas fa-chart-bar", style = "color: white; font-size: 32px;")
                   ),
                   
                   tags$h4("Eksplorasi"),
                   
                   tags$p("Analisis deskriptif dan visualisasi data")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "analysis-card",
                   onclick = "Shiny.setInputValue('tabs', 'assumptions', {priority: 'event'});",
                   
                   tags$div(
                     style = "width: 80px; 
                             height: 80px; 
                             background: linear-gradient(135deg, #00809D, #006B85); 
                             border-radius: 20px; 
                             display: flex; 
                             align-items: center; 
                             justify-content: center; 
                             margin: 0 auto 24px auto; 
                             box-shadow: 0 10px 25px rgba(0, 128, 157, 0.4);",
                     tags$i(class = "fas fa-check-circle", style = "color: white; font-size: 32px;")
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
                     style = "width: 80px; 
                             height: 80px; 
                             background: linear-gradient(135deg, #00809D, #006B85); 
                             border-radius: 20px; 
                             display: flex; 
                             align-items: center; 
                             justify-content: center; 
                             margin: 0 auto 24px auto; 
                             box-shadow: 0 10px 25px rgba(0, 128, 157, 0.3);",
                     tags$i(class = "fas fa-calculator", style = "color: white; font-size: 32px;")
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
                     style = "width: 80px; 
                             height: 80px; 
                             background: linear-gradient(135deg, #00809D, #006B85); 
                             border-radius: 20px; 
                             display: flex; 
                             align-items: center; 
                             justify-content: center; 
                             margin: 0 auto 24px auto; 
                             box-shadow: 0 10px 25px rgba(0, 128, 157, 0.3);",
                     tags$i(class = "fas fa-chart-line", style = "color: white; font-size: 32px;")
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
                     style = "width: 80px; 
                             height: 80px; 
                             background: linear-gradient(135deg, #00809D, #006B85); 
                             border-radius: 20px; 
                             display: flex; 
                             align-items: center; 
                             justify-content: center; 
                             margin: 0 auto 24px auto; 
                             box-shadow: 0 10px 25px rgba(0, 128, 157, 0.3);",
                     tags$i(class = "fas fa-globe", style = "color: white; font-size: 32px;")
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
                   style = "text-align: center; margin: 70px 0 45px 0;",
                   tags$h2(
                     tags$i(class = "fas fa-chart-line", style = "margin-right: 18px; color: #00809D;"),
                     "Overview Dataset",
                     style = "font-size: 2.5rem; 
                             font-weight: 700; 
                             color: #2D3748; 
                             margin-bottom: 18px;"
                   ),
                   tags$p("Ringkasan komprehensif dari dataset Social Vulnerability Index yang digunakan untuk analisis berdasarkan penelitian ilmiah",
                          style = "font-size: 1.2rem; color: #718096; max-width: 700px; margin: 0 auto; line-height: 1.7;")
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
        
        # Metadata Section
        fluidRow(
          column(12,
                 tags$div(
                   style = "text-align: center; margin: 70px 0 45px 0;",
                   tags$h3(
                     tags$i(class = "fas fa-info-circle", style = "margin-right: 14px; color: #00809D;"),
                     "Metadata Ilmiah",
                     style = "font-size: 2rem; 
                             font-weight: 600; 
                             color: #2D3748; 
                             margin-bottom: 18px;"
                   ),
                   tags$p("Informasi lengkap mengenai dataset Social Vulnerability Index berdasarkan publikasi ilmiah di ScienceDirect",
                          style = "font-size: 1.2rem; 
                                  color: #718096; 
                                  max-width: 750px; 
                                  margin: 0 auto; 
                                  line-height: 1.7;")
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
                     style = "display: flex; align-items: center; margin-bottom: 24px;",
                     tags$div(
                       style = "width: 56px; 
                               height: 56px; 
                               background: #00809D; 
                               border-radius: 14px; 
                               display: flex; 
                               align-items: center; 
                               justify-content: center; 
                               margin-right: 18px;",
                       tags$i(class = "fas fa-book", style = "color: white; font-size: 22px;")
                     ),
                     tags$h4("Publikasi Ilmiah")
                   ),
                   
                   tags$div(
                     tags$p(tags$strong("Journal:"), " Data in Brief"),
                     tags$p(tags$strong("Publisher:"), " Elsevier"),
                     tags$p(tags$strong("DOI:"), " 10.1016/j.dib.2021.107618"),
                     tags$p(tags$strong("URL:"), tags$a("ScienceDirect Link", 
                                                        href = "#", 
                                                        style = "color: #00809D; text-decoration: none;"))
                   )
                 )
          ),
          
          # Indikator Demografis
          column(4,
                 tags$div(
                   class = "metadata-card",
                   
                   tags$div(
                     style = "display: flex; align-items: center; margin-bottom: 24px;",
                     tags$div(
                       style = "width: 56px; 
                               height: 56px; 
                               background: #00809D; 
                               border-radius: 14px; 
                               display: flex; 
                               align-items: center; 
                               justify-content: center; 
                               margin-right: 18px;",
                       tags$i(class = "fas fa-users", style = "color: white; font-size: 22px;")
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
                     style = "display: flex; align-items: center; margin-bottom: 24px;",
                     tags$div(
                       style = "width: 56px; 
                               height: 56px; 
                               background: #00809D; 
                               border-radius: 14px; 
                               display: flex; 
                               align-items: center; 
                               justify-content: center; 
                               margin-right: 18px;",
                       tags$i(class = "fas fa-graduation-cap", style = "color: white; font-size: 22px;")
                     ),
                     tags$h4("Indikator Sosial-Ekonomi")
                   ),
                   
                   tags$div(
                     tags$p(tags$strong("LOWEDU:"), " Pendidikan Rendah"),
                     tags$p(tags$strong("POVERTY:"), " Tingkat Kemiskinan"),
                     tags$p(tags$strong("ILLITERATE:"), " Tingkat Buta Huruf"),
                     tags$p(tags$strong("NOTRAINING:"), " Tanpa Pelatihan Vokasi"),
                     tags$p(tags$strong("GROWTH:"), " Tingkat Pertumbuhan")
                   )
                 )
          )
        ),
        
        # Indikator Infrastruktur
        fluidRow(
          column(12,
                 tags$div(
                   class = "metadata-card",
                   
                   tags$div(
                     style = "display: flex; align-items: center; margin-bottom: 24px;",
                     tags$div(
                       style = "width: 56px; 
                               height: 56px; 
                               background: #00809D; 
                               border-radius: 14px; 
                               display: flex; 
                               align-items: center; 
                               justify-content: center; 
                               margin-right: 18px;",
                       tags$i(class = "fas fa-home", style = "color: white; font-size: 22px;")
                     ),
                     tags$h4("Indikator Infrastruktur")
                   ),
                   
                   fluidRow(
                     column(3,
                            tags$p(tags$strong("NOELECTRIC:"), " Tanpa Akses Listrik"),
                            tags$p(tags$strong("RENTED:"), " Rumah Sewa/Kontrak")
                     ),
                     column(3,
                            tags$p(tags$strong("NOSEWER:"), " Tanpa Sistem Sanitasi"),
                            tags$p(tags$strong("TAPWATER:"), " Akses Air Bersih")
                     ),
                     column(6,
                            tags$p(tags$strong("DPHONE:"), " Rawan Bencana Alam")
                     )
                   )
                 )
          )
        ),
        
        # Footer Information
        fluidRow(
          column(12,
                 tags$div(
                   style = "background: linear-gradient(135deg, #00809D 0%, #006B85 100%); 
                           border-radius: 24px; 
                           padding: 45px; 
                           margin-top: 45px; 
                           color: white;",
                   
                   fluidRow(
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-university", style = "margin-right: 12px;"),
                                "Informasi Akademis",
                                style = "font-weight: 700; margin-bottom: 18px; font-size: 1.2rem;"
                              ),
                              tags$p("Politeknik Statistika STIS", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Program Studi Komputasi Statistik D-IV", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Mata Kuliah: Komputasi Statistik", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Semester Genap TA. 2024/2025", style = "margin: 0; opacity: 0.9; font-size: 15px;")
                            )
                     ),
                     
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-calendar-alt", style = "margin-right: 12px;"),
                                "Jadwal UAS",
                                style = "font-weight: 700; margin-bottom: 18px; font-size: 1.2rem;"
                              ),
                              tags$p("Tanggal: 23 Juli 2025", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Waktu: 10:30 - 12:30 WIB", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Durasi: 120 menit", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Sifat: Tidak Terstruktur", style = "margin: 0; opacity: 0.9; font-size: 15px;")
                            )
                     ),
                     
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-database", style = "margin-right: 12px;"),
                                "Data Source",
                                style = "font-weight: 700; margin-bottom: 18px; font-size: 1.2rem;"
                              ),
                              tags$p("Dataset: Social Vulnerability Index", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Lokasi: D:/Perkuliahan.../WASKITA/data", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Format: CSV Files", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Metadata: ScienceDirect Publication", style = "margin: 0; opacity: 0.9; font-size: 15px;")
                            )
                     ),
                     
                     column(3,
                            tags$div(
                              tags$h5(
                                tags$i(class = "fas fa-book-open", style = "margin-right: 12px;"),
                                "Referensi Ilmiah",
                                style = "font-weight: 700; margin-bottom: 18px; font-size: 1.2rem;"
                              ),
                              tags$p("Journal: Data in Brief", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("Publisher: Elsevier", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$p("DOI: 10.1016/j.dib.2021.107618", style = "margin-bottom: 8px; opacity: 0.9; font-size: 15px;"),
                              tags$a("View Publication", 
                                     href = "#", 
                                     style = "color: #B8E6FF; text-decoration: none; font-weight: 600; font-size: 15px;")
                            )
                     )
                   ),
                   
                   tags$hr(style = "border-color: rgba(255,255,255,0.2); margin: 35px 0 25px 0;"),
                   
                   tags$div(
                     style = "text-align: center;",
                     tags$p("© 2025 Dashboard WASKITA - Final Project using SoVI Data with Scientific Metadata",
                            style = "margin: 0; opacity: 0.8; font-size: 15px;")
                   )
                 )
          )
        )
      ),
      
      # DATA MANAGEMENT TAB (Placeholder - would continue with other tabs)
      tabItem(
        tabName = "data_management",
        fluidRow(
          box(
            title = tagList(icon("database"), "Manajemen Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Data Management functionality would go here...")
          )
        )
      ),
      
      # EXPLORATION TAB (Placeholder)
      tabItem(
        tabName = "exploration",
        fluidRow(
          box(
            title = tagList(icon("search"), "Eksplorasi Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Data exploration functionality would go here...")
          )
        )
      ),
      
      # ASSUMPTIONS TAB (Placeholder)
      tabItem(
        tabName = "assumptions",
        fluidRow(
          box(
            title = tagList(icon("check-circle"), "Uji Asumsi Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Assumption testing functionality would go here...")
          )
        )
      ),
      
      # MEAN TESTS TAB (Placeholder)
      tabItem(
        tabName = "mean_tests",
        fluidRow(
          box(
            title = tagList(icon("chart-line"), "Uji Beda Rata-rata"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Mean difference testing functionality would go here...")
          )
        )
      ),
      
      # PROPORTION TESTS TAB (Placeholder)
      tabItem(
        tabName = "proportion_tests",
        fluidRow(
          box(
            title = tagList(icon("pie-chart"), "Uji Proporsi & Variance"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Proportion and variance testing functionality would go here...")
          )
        )
      ),
      
      # ANOVA TAB (Placeholder)
      tabItem(
        tabName = "anova",
        fluidRow(
          box(
            title = tagList(icon("chart-area"), "ANOVA"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("ANOVA functionality would go here...")
          )
        )
      ),
      
      # REGRESSION TAB (Placeholder)
      tabItem(
        tabName = "regression",
        fluidRow(
          box(
            title = tagList(icon("project-diagram"), "Regresi Linear Berganda"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Multiple linear regression functionality would go here...")
          )
        )
      ),
      
      # SPATIAL MAPPING TAB (Placeholder)
      tabItem(
        tabName = "spatial_mapping",
        fluidRow(
          box(
            title = tagList(icon("map"), "Pemetaan Spasial"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Spatial mapping functionality would go here...")
          )
        )
      ),
      
      # LAPORAN KOMPREHENSIF TAB (Placeholder)
      tabItem(
        tabName = "laporan_komprehensif",
        fluidRow(
          box(
            title = tagList(icon("file-alt"), "Laporan Komprehensif"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            h4("Comprehensive report functionality would go here...")
          )
        )
      )
    )
  )
)