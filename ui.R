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
      tags$i(class = "fas fa-chart-line", style = "color: #fff; margin-right: 12px; font-size: 20px;"),
      "AXIS Dashboard",
      style = "font-weight: 600; font-size: 20px; letter-spacing: 0.5px;"
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
          padding: 16px 20px !important;
          margin: 2px 12px !important;
          border-radius: 12px !important;
          transition: all 0.3s ease !important;
          font-weight: 500 !important;
        }
        .skin-blue .sidebar-menu > li > a > .fa,
        .skin-blue .sidebar-menu > li > a > .fas,
        .skin-blue .sidebar-menu > li > a > .glyphicon,
        .skin-blue .sidebar-menu > li > a > .ion {
          margin-right: 12px !important;
          font-size: 16px !important;
        }
      "))
    ),
    
    # Removed the extra div that was causing spacing issues
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
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
      tags$style(HTML("
        /* Global Styles */
        body, .content-wrapper, .right-side {
          background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%) !important;
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif !important;
          color: #334155 !important;
        }
        
        /* Header Styling */
        .main-header .navbar {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          border: none !important;
          box-shadow: 0 4px 20px rgba(0, 128, 157, 0.15) !important;
        }
        
        /* Content Area */
        .content {
          padding: 32px !important;
        }
        
        /* Modern Box Styling */
        .box {
          border: none !important;
          border-radius: 20px !important;
          box-shadow: 0 8px 32px rgba(0, 0, 0, 0.08) !important;
          background: #ffffff !important;
          margin-bottom: 32px !important;
          overflow: hidden !important;
          transition: all 0.3s ease !important;
        }
        
        .box:hover {
          box-shadow: 0 12px 40px rgba(0, 0, 0, 0.12) !important;
          transform: translateY(-2px) !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: #ffffff !important;
          padding: 24px 32px !important;
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
          padding: 32px !important;
          background: #ffffff !important;
        }
        
        /* Modern Cards */
        .stat-card {
          background: #ffffff !important;
          border-radius: 16px !important;
          padding: 28px !important;
          margin-bottom: 24px !important;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.06) !important;
          border: 1px solid #e2e8f0 !important;
          transition: all 0.3s ease !important;
        }
        
        .stat-card:hover {
          box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1) !important;
          transform: translateY(-1px) !important;
        }
        
        .stat-card h5 {
          color: #1e293b !important;
          font-weight: 600 !important;
          font-size: 20px !important;
          margin-bottom: 16px !important;
        }
        
        .stat-value {
          font-size: 3.2em !important;
          font-weight: 700 !important;
          color: #00809D !important;
          line-height: 1.1 !important;
        }
        
        /* Overview Cards */
        .overview-card {
          background: #ffffff !important;
          border-radius: 20px !important;
          padding: 40px 30px !important;
          margin-bottom: 30px !important;
          box-shadow: 0 8px 25px rgba(0,0,0,0.08) !important;
          border: 1px solid #e2e8f0 !important;
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
          box-shadow: 0 15px 40px rgba(0, 128, 157, 0.12) !important;
          transform: translateY(-8px) scale(1.02) !important;
        }
        
        .overview-card .icon-container {
          width: 80px !important;
          height: 80px !important;
          border-radius: 18px !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          margin: 0 auto 20px auto !important;
          transition: all 0.4s ease !important;
          position: relative !important;
          z-index: 2 !important;
        }
        
        .overview-card:hover .icon-container {
          transform: scale(1.1) rotate(5deg) !important;
        }
        
        .overview-card .icon-container i {
          font-size: 32px !important;
          color: white !important;
        }
        
        .overview-card h3 {
          font-size: 3.5rem !important;
          font-weight: 800 !important;
          margin: 15px 0 !important;
          line-height: 1 !important;
          color: #00809D !important;
          position: relative !important;
          z-index: 2 !important;
        }
        
        .overview-card h5 {
          font-size: 1.3rem !important;
          font-weight: 700 !important;
          color: #2D3748 !important;
          margin-bottom: 8px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
          position: relative !important;
          z-index: 2 !important;
        }
        
        .overview-card p {
          font-size: 1.1rem !important;
          color: #64748b !important;
          margin: 0 !important;
          line-height: 1.4 !important;
          position: relative !important;
          z-index: 2 !important;
        }
        
        /* Province Button */
        .province-btn {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: white !important;
          border: none !important;
          border-radius: 12px !important;
          padding: 12px 24px !important;
          font-size: 14px !important;
          font-weight: 600 !important;
          margin-top: 15px !important;
          transition: all 0.3s ease !important;
          box-shadow: 0 4px 15px rgba(0, 128, 157, 0.3) !important;
          position: relative !important;
          z-index: 2 !important;
        }
        
        .province-btn:hover {
          background: linear-gradient(135deg, #006B85 0%, #005A73 100%) !important;
          transform: translateY(-2px) !important;
          box-shadow: 0 6px 20px rgba(0, 128, 157, 0.4) !important;
          color: white !important;
        }
        
        /* Modal Styling */
        .modal-content {
          border-radius: 20px !important;
          border: none !important;
          box-shadow: 0 20px 60px rgba(0, 0, 0, 0.15) !important;
        }
        
        .modal-header {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: white !important;
          border-radius: 20px 20px 0 0 !important;
          border: none !important;
          padding: 25px 30px !important;
        }
        
        .modal-title {
          font-size: 1.5rem !important;
          font-weight: 700 !important;
        }
        
        .modal-body {
          padding: 30px !important;
          background: #ffffff !important;
        }
        
        .province-list {
          display: grid !important;
          grid-template-columns: 1fr 1fr !important;
          gap: 25px !important;
        }
        
        .province-group h6 {
          color: #00809D !important;
          font-weight: 700 !important;
          font-size: 1.1rem !important;
          margin-bottom: 12px !important;
          padding-bottom: 8px !important;
          border-bottom: 2px solid #e2e8f0 !important;
        }
        
        .province-group ul {
          list-style: none !important;
          padding: 0 !important;
          margin: 0 !important;
        }
        
        .province-group li {
          padding: 6px 0 !important;
          color: #4A5568 !important;
          font-size: 0.95rem !important;
          border-bottom: 1px solid #f1f5f9 !important;
        }
        
        .province-group li:last-child {
          border-bottom: none !important;
        }
        
        /* Interpretation Box */
        .interpretation-box {
          background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%) !important;
          border: none !important;
          border-left: 5px solid #00809D !important;
          padding: 24px !important;
          margin-top: 24px !important;
          border-radius: 12px !important;
          box-shadow: 0 2px 12px rgba(0, 128, 157, 0.1) !important;
        }
        
        .interpretation-box h5 {
          color: #1e40af !important;
          font-weight: 600 !important;
          margin-bottom: 12px !important;
          font-size: 18px !important;
        }
        
        /* Download Section */
        .download-section {
          background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%) !important;
          padding: 24px !important;
          border-radius: 16px !important;
          margin-top: 32px !important;
          border: 1px solid #e2e8f0 !important;
        }
        
        .download-section h5 {
          color: #475569 !important;
          font-weight: 600 !important;
          margin-bottom: 20px !important;
          font-size: 18px !important;
        }
        
        /* Modern Buttons */
        .btn {
          border-radius: 12px !important;
          padding: 12px 24px !important;
          font-weight: 500 !important;
          font-size: 15px !important;
          transition: all 0.3s ease !important;
          border: none !important;
          letter-spacing: 0.3px !important;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: #ffffff !important;
          box-shadow: 0 4px 16px rgba(0, 128, 157, 0.3) !important;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #006B85 0%, #005A73 100%) !important;
          box-shadow: 0 6px 24px rgba(0, 128, 157, 0.4) !important;
          transform: translateY(-1px) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #10b981 0%, #059669 100%) !important;
          color: #ffffff !important;
          box-shadow: 0 4px 16px rgba(16, 185, 129, 0.3) !important;
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #059669 0%, #047857 100%) !important;
          box-shadow: 0 6px 24px rgba(16, 185, 129, 0.4) !important;
          transform: translateY(-1px) !important;
        }
        
        .btn-lg {
          padding: 16px 32px !important;
          font-size: 17px !important;
          font-weight: 600 !important;
        }
        
        /* Form Controls */
        .form-control, .selectize-input {
          border-radius: 12px !important;
          border: 2px solid #e2e8f0 !important;
          padding: 12px 16px !important;
          font-size: 15px !important;
          transition: all 0.3s ease !important;
          background: #ffffff !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #00809D !important;
          box-shadow: 0 0 0 3px rgba(0, 128, 157, 0.1) !important;
          outline: none !important;
        }
        
        /* Modern Table */
        .variable-table {
          width: 100% !important;
          border-collapse: separate !important;
          border-spacing: 0 !important;
          margin-top: 24px !important;
          border-radius: 12px !important;
          overflow: hidden !important;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.06) !important;
        }
        
        .variable-table th {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important;
          color: #ffffff !important;
          font-weight: 600 !important;
          padding: 20px !important;
          text-align: left !important;
          font-size: 15px !important;
          letter-spacing: 0.3px !important;
        }
        
        .variable-table td {
          padding: 20px !important;
          border-bottom: 1px solid #f1f5f9 !important;
          vertical-align: top !important;
          background: #ffffff !important;
          font-size: 15px !important;
          line-height: 1.6 !important;
        }
        
        .variable-table tr:hover td {
          background: #f8fafc !important;
        }
        
        /* Feature Items */
        .feature-item {
          margin-bottom: 32px !important;
          padding: 24px !important;
          background: #ffffff !important;
          border-radius: 16px !important;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.06) !important;
          border: 1px solid #e2e8f0 !important;
          transition: all 0.3s ease !important;
        }
        
        .feature-item:hover {
          box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1) !important;
          transform: translateY(-2px) !important;
        }
        
        .feature-item .fas {
          color: #00809D !important;
          margin-right: 16px !important;
          vertical-align: middle !important;
        }
        
        .feature-item h5 {
          font-weight: 600 !important;
          color: #1e293b !important;
          display: inline !important;
          margin-left: 16px !important;
          vertical-align: middle !important;
          font-size: 20px !important;
        }
        
        .feature-item p {
          margin: 16px 0 0 56px !important;
          color: #64748b !important;
          line-height: 1.6 !important;
          font-size: 16px !important;
        }
        
        /* Section Headers */
        h3, h4 {
          color: #1e293b !important;
          font-weight: 600 !important;
          margin-bottom: 24px !important;
        }
        
        h3 {
          font-size: 2.8rem !important;
          font-weight: 700 !important;
          color: #00809D !important;
        }
        
        h4 {
          font-size: 2.4rem !important;
          color: #00809D !important;
          font-weight: 600 !important;
        }
        
        /* Analysis Cards - Professional Hover Effects */
        .analysis-card {
          background: white !important; 
          border-radius: 24px !important; 
          padding: 40px !important; 
          margin-bottom: 30px !important; 
          box-shadow: 0 10px 30px rgba(0,0,0,0.08) !important; 
          border: 2px solid transparent !important; 
          transition: all 0.4s cubic-bezier(0.4, 0.0, 0.2, 1) !important; 
          cursor: pointer !important; 
          text-align: center !important;
          position: relative !important;
          overflow: hidden !important;
        }
        
        .analysis-card::before {
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
        
        .analysis-card:hover::before {
          opacity: 1 !important;
        }
        
        .analysis-card:hover {
          box-shadow: 0 20px 60px rgba(0, 128, 157, 0.15) !important;
          transform: translateY(-12px) scale(1.03) !important;
          border-color: rgba(0, 128, 157, 0.2) !important;
          background: linear-gradient(135deg, #ffffff 0%, #fafbff 100%) !important;
        }
        
        .analysis-card .icon-container {
          width: 85px !important; 
          height: 85px !important; 
          background: linear-gradient(135deg, #00809D, #006B85) !important; 
          border-radius: 20px !important; 
          display: flex !important; 
          align-items: center !important; 
          justify-content: center !important; 
          margin: 0 auto 24px auto !important; 
          box-shadow: 0 10px 25px rgba(0, 128, 157, 0.3) !important;
          transition: all 0.4s cubic-bezier(0.4, 0.0, 0.2, 1) !important;
          position: relative !important;
          overflow: hidden !important;
        }
        
        .analysis-card .icon-container::after {
          content: '' !important;
          position: absolute !important;
          top: -50% !important;
          left: -50% !important;
          width: 200% !important;
          height: 200% !important;
          background: linear-gradient(45deg, transparent, rgba(255,255,255,0.3), transparent) !important;
          transform: rotate(45deg) !important;
          transition: all 0.6s ease !important;
          opacity: 0 !important;
        }
        
        .analysis-card:hover .icon-container {
          transform: rotate(5deg) scale(1.1) !important;
          box-shadow: 0 15px 35px rgba(0, 128, 157, 0.4) !important;
          background: linear-gradient(135deg, #0091B8, #007A9E) !important;
        }
        
        .analysis-card:hover .icon-container::after {
          opacity: 1 !important;
          transform: rotate(45deg) translate(50%, 50%) !important;
        }
        
        .analysis-card .icon-container i {
          color: white !important; 
          font-size: 32px !important;
          transition: all 0.4s ease !important;
          z-index: 2 !important;
          position: relative !important;
        }
        
        .analysis-card:hover .icon-container i {
          transform: scale(1.1) !important;
          text-shadow: 0 0 20px rgba(255,255,255,0.5) !important;
        }
        
        .analysis-card h4 {
          font-size: 1.8rem !important; 
          font-weight: 700 !important; 
          color: #2D3748 !important; 
          margin-bottom: 16px !important;
          transition: all 0.3s ease !important;
        }
        
        .analysis-card:hover h4 {
          color: #00809D !important;
          transform: translateY(-2px) !important;
        }
        
        .analysis-card p {
          color: #718096 !important; 
          font-size: 1.2rem !important; 
          line-height: 1.6 !important; 
          margin: 0 !important;
          transition: all 0.3s ease !important;
        }
        
        .analysis-card:hover p {
          color: #4A5568 !important;
        }
        
        /* Hero Section Enhanced */
        .hero-section {
          background: linear-gradient(135deg, #00809D 0%, #006B85 100%) !important; 
          border-radius: 28px !important; 
          padding: 70px 50px !important; 
          margin: 20px 0 50px 0 !important; 
          text-align: center !important; 
          color: white !important; 
          position: relative !important; 
          overflow: hidden !important;
          box-shadow: 0 25px 50px rgba(0, 128, 157, 0.3) !important;
        }
        
        .hero-section h1 {
          font-size: 5rem !important; 
          font-weight: 800 !important; 
          margin: 25px 0 !important; 
          letter-spacing: -2px !important; 
          text-shadow: 0 4px 20px rgba(0,0,0,0.2) !important;
        }
        
        .hero-section h2 {
          font-size: 2.3rem !important; 
          font-weight: 400 !important; 
          margin-bottom: 30px !important; 
          opacity: 0.95 !important; 
          letter-spacing: 0.5px !important;
        }
        
        .hero-section p {
          font-size: 1.3rem !important; 
          line-height: 1.8 !important; 
          max-width: 850px !important; 
          margin: 0 auto 35px auto !important; 
          opacity: 0.9 !important;
        }
        
        /* Section Headers Enhanced */
        .section-header h2 {
          font-size: 2.8rem !important; 
          font-weight: 700 !important; 
          color: #2D3748 !important; 
          margin-bottom: 18px !important;
        }
        
        .section-header p {
          font-size: 1.3rem !important; 
          color: #718096 !important; 
          max-width: 700px !important; 
          margin: 0 auto !important; 
          line-height: 1.7 !important;
        }
        
        /* Metadata Cards Enhanced */
        .metadata-card {
          background: white !important; 
          border-radius: 24px !important; 
          padding: 35px !important; 
          margin-bottom: 30px !important; 
          box-shadow: 0 10px 30px rgba(0,0,0,0.08) !important; 
          border-left: 5px solid #00809D !important;
          transition: all 0.3s cubic-bezier(0.4, 0.0, 0.2, 1) !important;
          position: relative !important;
          overflow: hidden !important;
        }
        
        .metadata-card::before {
          content: '' !important;
          position: absolute !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          bottom: 0 !important;
          background: linear-gradient(135deg, rgba(0, 128, 157, 0.02) 0%, rgba(0, 107, 133, 0.02) 100%) !important;
          opacity: 0 !important;
          transition: opacity 0.3s ease !important;
        }
        
        .metadata-card:hover::before {
          opacity: 1 !important;
        }
        
        .metadata-card:hover {
          box-shadow: 0 15px 40px rgba(0, 128, 157, 0.12) !important;
          transform: translateY(-8px) !important;
          border-left-width: 8px !important;
        }
        
        .metadata-card h4 {
          font-size: 1.7rem !important; 
          font-weight: 700 !important; 
          color: #2D3748 !important; 
          margin: 0 !important;
        }
        
        .metadata-card p {
          margin-bottom: 10px !important; 
          color: #4A5568 !important;
          font-size: 1.1rem !important;
          line-height: 1.6 !important;
        }
        
        .metadata-card .icon-container {
          width: 60px !important; 
          height: 60px !important; 
          background: #00809D !important; 
          border-radius: 15px !important; 
          display: flex !important; 
          align-items: center !important; 
          justify-content: center !important; 
          margin-right: 18px !important;
          transition: all 0.3s ease !important;
        }
        
        .metadata-card:hover .icon-container {
          transform: scale(1.1) rotate(5deg) !important;
          box-shadow: 0 8px 20px rgba(0, 128, 157, 0.3) !important;
        }
        
        .metadata-card .icon-container i {
          color: white !important; 
          font-size: 24px !important;
        }
        
        /* Footer Enhanced */
        .footer-section h5 {
          font-weight: 700 !important;
          margin-bottom: 15px !important;
          font-size: 1.2rem !important;
        }
        
        .footer-section p {
          margin-bottom: 5px !important;
          opacity: 0.9 !important;
          font-size: 1rem !important;
        }
        
        .footer-section a {
          color: #B8E6FF !important;
          text-decoration: none !important;
          font-weight: 600 !important;
          font-size: 1rem !important;
        }
        
        /* Tabs */
        .nav-tabs-custom > .nav-tabs {
          border-bottom: 2px solid #e2e8f0 !important;
          background: transparent !important;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background: #00809D !important;
          color: #ffffff !important;
          border-radius: 12px 12px 0 0 !important;
          border: none !important;
          font-weight: 600 !important;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a {
          border-radius: 12px 12px 0 0 !important;
          border: none !important;
          color: #64748b !important;
          font-weight: 500 !important;
          padding: 16px 24px !important;
          margin-right: 4px !important;
          transition: all 0.3s ease !important;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a:hover {
          background: #f1f5f9 !important;
          color: #00809D !important;
        }
        
        /* DataTables */
        .dataTables_wrapper {
          margin-top: 24px !important;
        }
        
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter {
          margin-bottom: 20px !important;
        }
        
        .dataTables_wrapper .dataTables_filter input {
          border-radius: 12px !important;
          border: 2px solid #e2e8f0 !important;
          padding: 8px 16px !important;
        }
        
        /* Responsive Design */
        @media (max-width: 768px) {
          .content {
            padding: 16px !important;
          }
          
          .box-body {
            padding: 20px !important;
          }
          
          .stat-card {
            padding: 20px !important;
          }
          
          .feature-item {
            padding: 20px !important;
          }
          
          .analysis-card {
            padding: 25px !important;
          }
          
          .analysis-card h4 {
            font-size: 1.5rem !important;
          }
          
          .analysis-card p {
            font-size: 1rem !important;
          }
          
          .hero-section h1 {
            font-size: 3.5rem !important;
          }
          
          .hero-section h2 {
            font-size: 1.8rem !important;
          }
          
          .hero-section p {
            font-size: 1.1rem !important;
          }
          
          .section-header h2 {
            font-size: 2.2rem !important;
          }
          
          .section-header p {
            font-size: 1.1rem !important;
          }
          
          .province-list {
            grid-template-columns: 1fr !important;
          }
        }
        
        /* Loading Spinner */
        .shiny-spinner-output-container {
          background: rgba(255, 255, 255, 0.9) !important;
          border-radius: 12px !important;
        }
        
        /* Checkbox and Radio Styling */
        .checkbox input[type='checkbox']:checked + label::before,
        .radio input[type='radio']:checked + label::before {
          background-color: #00809D !important;
          border-color: #00809D !important;
        }
        
        /* Plotly Container */
        .plotly {
          border-radius: 12px !important;
          overflow: hidden !important;
        }
        
        /* Leaflet Map */
        .leaflet-container {
          height: 500px !important;
          border-radius: 12px !important;
          overflow: hidden !important;
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
          font-size: 20px !important;
        }
        
        /* Scrollbar Styling */
        ::-webkit-scrollbar {
          width: 8px !important;
        }
        
        ::-webkit-scrollbar-track {
          background: #f1f5f9 !important;
          border-radius: 4px !important;
        }
        
        ::-webkit-scrollbar-thumb {
          background: #cbd5e1 !important;
          border-radius: 4px !important;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: #94a3b8 !important;
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
                   
                   # Background decoration
                   tags$div(
                     style = "position: absolute; top: -50px; right: -50px; 
                             width: 200px; height: 200px; 
                             background: rgba(255,255,255,0.1); 
                             border-radius: 50%; 
                             opacity: 0.6;"
                   ),
                   tags$div(
                     style = "position: absolute; bottom: -30px; left: -30px; 
                             width: 150px; height: 150px; 
                             background: rgba(255,255,255,0.08); 
                             border-radius: 50%;"
                   ),
                   
                   # Content
                   tags$div(
                     style = "position: relative; z-index: 2;",
                     
                     tags$div(
                       style = "display: inline-flex; align-items: center; 
                               background: rgba(255,255,255,0.15); 
                               padding: 12px 24px; 
                               border-radius: 50px; 
                               margin-bottom: 30px; 
                               backdrop-filter: blur(10px);",
                       tags$i(class = "fas fa-chart-line", style = "margin-right: 12px; font-size: 20px;"),
                       tags$span("UAS Komputasi Statistik 2025", style = "font-weight: 600; font-size: 17px;")
                     ),
                     
                     tags$h1("AXIS"),
                     
                     tags$h2("Advanced eXploratory Inference Statistics"),
                     
                     tags$p("Platform analisis modern untuk penelitian kerentanan sosial dengan tools statistik komprehensif, visualisasi interaktif, dan analisis spasial yang terintegrasi menggunakan data Social Vulnerability Index dari penelitian ilmiah.")
                   )
                 )
          )
        ),
        
        # Menu Analisis Section
        fluidRow(
          column(12,
                 tags$div(
                   class = "section-header",
                   style = "text-align: center; margin-bottom: 50px;",
                   tags$h2(
                     tags$i(class = "fas fa-rocket", style = "margin-right: 15px; color: #00809D;"),
                     "Menu Analisis"
                   ),
                   tags$p("Pilihan menu analisis yang bisa Anda gunakan untuk memulai eksplorasi data kerentanan sosial berdasarkan penelitian ilmiah")
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
                     tags$i(class = "fas fa-database")
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
                     tags$i(class = "fas fa-chart-bar")
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
                     class = "icon-container",
                     tags$i(class = "fas fa-check-circle")
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
                     tags$i(class = "fas fa-calculator")
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
                     tags$i(class = "fas fa-chart-line")
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
                     tags$i(class = "fas fa-globe")
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
                   class = "section-header",
                   style = "text-align: center; margin: 70px 0 50px 0;",
                   tags$h2(
                     tags$i(class = "fas fa-chart-line", style = "margin-right: 15px; color: #00809D;"),
                     "Overview Dataset"
                   ),
                   tags$p("Ringkasan komprehensif dari dataset Social Vulnerability Index yang digunakan untuk analisis berdasarkan penelitian ilmiah")
                 )
          )
        ),
        
        # Dataset Statistics Cards
        fluidRow(
          column(4,
                 tags$div(
                   class = "overview-card",
                   
                   tags$div(
                     class = "icon-container",
                     style = "background: linear-gradient(135deg, #3B82F6, #1E40AF);",
                     tags$i(class = "fas fa-table")
                   ),
                   
                   tags$h3("511"),
                   tags$h5("Total Observasi"),
                   tags$p("Kabupaten/Kota dalam dataset")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "overview-card",
                   
                   tags$div(
                     class = "icon-container",
                     style = "background: linear-gradient(135deg, #10B981, #047857);",
                     tags$i(class = "fas fa-list-ul")
                   ),
                   
                   tags$h3("17"),
                   tags$h5("Indikator SoVI"),
                   tags$p("Variabel kerentanan sosial")
                 )
          ),
          
          column(4,
                 tags$div(
                   class = "overview-card",
                   
                   tags$div(
                     class = "icon-container",
                     style = "background: linear-gradient(135deg, #F59E0B, #D97706);",
                     tags$i(class = "fas fa-map-marked-alt")
                   ),
                   
                   tags$h3("38"),
                   tags$h5("Provinsi Indonesia"),
                   tags$p("Cakupan wilayah dalam data"),
                   
                 )
          )
        ),
        
        # Province Modal
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
                
                tags$div(
                  class = "province-list",
                  
                  # Column 1
                  tags$div(
                    tags$div(
                      class = "province-group",
                      tags$h6("Sumatera"),
                      tags$ul(
                        tags$li("Aceh"),
                        tags$li("Sumatera Utara"),
                        tags$li("Sumatera Barat"),
                        tags$li("Riau"),
                        tags$li("Kepulauan Riau"),
                        tags$li("Jambi"),
                        tags$li("Sumatera Selatan"),
                        tags$li("Bangka Belitung"),
                        tags$li("Bengkulu"),
                        tags$li("Lampung")
                      )
                    ),
                    
                    tags$div(
                      class = "province-group",
                      style = "margin-top: 25px;",
                      tags$h6("Jawa & Bali"),
                      tags$ul(
                        tags$li("DKI Jakarta"),
                        tags$li("Jawa Barat"),
                        tags$li("Jawa Tengah"),
                        tags$li("DI Yogyakarta"),
                        tags$li("Jawa Timur"),
                        tags$li("Banten"),
                        tags$li("Bali")
                      )
                    ),
                    
                    tags$div(
                      class = "province-group",
                      style = "margin-top: 25px;",
                      tags$h6("Kalimantan"),
                      tags$ul(
                        tags$li("Kalimantan Barat"),
                        tags$li("Kalimantan Tengah"),
                        tags$li("Kalimantan Selatan"),
                        tags$li("Kalimantan Timur"),
                        tags$li("Kalimantan Utara")
                      )
                    )
                  ),
                  
                  # Column 2
                  tags$div(
                    tags$div(
                      class = "province-group",
                      tags$h6("Sulawesi"),
                      tags$ul(
                        tags$li("Sulawesi Utara"),
                        tags$li("Sulawesi Tengah"),
                        tags$li("Sulawesi Selatan"),
                        tags$li("Sulawesi Tenggara"),
                        tags$li("Gorontalo"),
                        tags$li("Sulawesi Barat")
                      )
                    ),
                    
                    tags$div(
                      class = "province-group",
                      style = "margin-top: 25px;",
                      tags$h6("Nusa Tenggara & Maluku"),
                      tags$ul(
                        tags$li("Nusa Tenggara Barat"),
                        tags$li("Nusa Tenggara Timur"),
                        tags$li("Maluku"),
                        tags$li("Maluku Utara")
                      )
                    ),
                    
                    tags$div(
                      class = "province-group",
                      style = "margin-top: 25px;",
                      tags$h6("Papua"),
                      tags$ul(
                        tags$li("Papua"),
                        tags$li("Papua Barat"),
                        tags$li("Papua Selatan"),
                        tags$li("Papua Tengah"),
                        tags$li("Papua Pegunungan"),
                        tags$li("Papua Barat Daya")
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Metadata Section
        fluidRow(
          column(12,
                 tags$div(
                   class = "section-header",
                   style = "text-align: center; margin: 70px 0 50px 0;",
                   tags$h3(
                     tags$i(class = "fas fa-info-circle", style = "margin-right: 12px; color: #00809D;"),
                     "Metadata Ilmiah"
                   ),
                   tags$p("Informasi lengkap mengenai dataset Social Vulnerability Index berdasarkan publikasi ilmiah di ScienceDirect")
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
                     style = "display: flex; align-items: center; margin-bottom: 20px;",
                     tags$div(
                       class = "icon-container",
                       tags$i(class = "fas fa-book")
                     ),
                     tags$h4("Publikasi Ilmiah")
                   ),
                   
                   tags$div(
                     tags$p(tags$strong("Journal:"), " Data in Brief"),
                     tags$p(tags$strong("Publisher:"), " Elsevier"),
                     tags$p(tags$strong("DOI:"), " 10.1016/j.dib.2021.107618"),
                
                   )
                 )
          ),
          
          # Indikator Demografis
          column(4,
                 tags$div(
                   class = "metadata-card",
                   
                   tags$div(
                     style = "display: flex; align-items: center; margin-bottom: 20px;",
                     tags$div(
                       class = "icon-container",
                       tags$i(class = "fas fa-users")
                     ),
                     tags$h4("Indikator Demografis")
                   ),
                   
                   tags$div(
                     tags$p(tags$strong("CHILDREN:"), " Proporsi Anak < 5 tahun"),
                     tags$p(tags$strong("FEMALE:"), " Proporsi Perempuan"),
                     tags$p(tags$strong("ELDERLY:"), " Proporsi Lansia 65+"),
                     tags$p(tags$strong("FHEAD:"), " KRT Perempuan"),
                     tags$p(tags$strong("FAMILYSIZE:"), " Rata-rata Ukuran Keluarga")
                   )
                 )
          ),
          
          # Indikator Sosial-Ekonomi
          column(4,
                 tags$div(
                   class = "metadata-card",
                   
                   tags$div(
                     style = "display: flex; align-items: center; margin-bottom: 20px;",
                     tags$div(
                       class = "icon-container",
                       tags$i(class = "fas fa-graduation-cap")
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
                     style = "display: flex; align-items: center; margin-bottom: 20px;",
                     tags$div(
                       class = "icon-container",
                       tags$i(class = "fas fa-home")
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
                     column(3,
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
                           border-radius: 20px; 
                           padding: 40px; 
                           margin-top: 40px; 
                           color: white;",
                   
                   fluidRow(
                     column(3,
                            tags$div(
                              class = "footer-section",
                              tags$h5(
                                tags$i(class = "fas fa-university", style = "margin-right: 10px;"),
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
                              class = "footer-section",
                              tags$h5(
                                tags$i(class = "fas fa-calendar-alt", style = "margin-right: 10px;"),
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
                              class = "footer-section",
                              tags$h5(
                                tags$i(class = "fas fa-database", style = "margin-right: 10px;"),
                                "Data Source"
                              ),
                              tags$p("Dataset: Social Vulnerability Index"),
                              tags$p("Link SoVI: ", tags$a(href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", "sovi_data.csv", target = "_blank")),
                              tags$p("Link Distance: ", tags$a(href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv", "distance.csv", target = "_blank")),                              
                              tags$p("Format: CSV Files"),
                              tags$p("Metadata: ScienceDirect Publication")
                            )
                     ),
                     
                     column(3,
                            tags$div(
                              class = "footer-section",
                              tags$h5(
                                tags$i(class = "fas fa-book-open", style = "margin-right: 10px;"),
                                "Referensi Ilmiah"
                              ),
                              tags$p("Journal: Data in Brief"),
                              tags$p("Publisher: Elsevier"),
                              tags$p("DOI: 10.1016/j.dib.2021.107618"),
                              tags$a("View Publication", href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180")
                            )
                     )
                   ),
                   
                   tags$hr(style = "border-color: rgba(255,255,255,0.2); margin: 30px 0 20px 0;"),
                   
                   tags$div(
                     style = "text-align: center;",
                     tags$p("© 2025 Dashboard AXIS - Final Project using SoVI Data with Scientific Metadata",
                            style = "margin: 0; opacity: 0.8; font-size: 1.5rem;")
                   )
                 )
          )
        )
      ),
      
      # DATA MANAGEMENT TAB
      tabItem(
        tabName = "data_management",
        fluidRow(
          box(
            title = tagList(icon("database"), "Manajemen Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            fluidRow(
              column(4,
                     div(style = "margin-bottom: 32px;",
                         h4("1. Load Data", style = "margin-bottom: 24px;"),
                         
                         actionButton("load_data", "Load SoVI Dataset", 
                                      class = "btn btn-primary btn-lg", 
                                      style = "width: 100%; margin-bottom: 24px;"),
                         
                         conditionalPanel(
                           condition = "output.data_loaded == true",
                           div(class = "stat-card",
                               h5("Data Information"),
                               verbatimTextOutput("data_summary")
                           )
                         )
                     )
              ),
              
              column(4,
                     div(style = "margin-bottom: 32px;",
                         h4("2. Transformasi Data", style = "margin-bottom: 24px;"),
                         
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
                                        style = "width: 100%; margin-bottom: 24px;"),
                           
                           conditionalPanel(
                             condition = "output.transformation_done == true",
                             div(class = "interpretation-box",
                                 h5("Hasil Transformasi"),
                                 verbatimTextOutput("transformation_result")
                             )
                           )
                         )
                     )
              ),
              
              column(4,
                     div(style = "margin-bottom: 32px;",
                         h4("3. Kategorisasi Data", style = "margin-bottom: 24px;"),
                         
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
                                        style = "width: 100%; margin-bottom: 24px;"),
                           
                           conditionalPanel(
                             condition = "output.categorization_done == true",
                             div(class = "interpretation-box",
                                 h5("Hasil Kategorisasi"),
                                 verbatimTextOutput("categorization_result")
                             )
                           )
                         )
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
              
              fluidRow(
                column(12,
                       h4("Preview Data", style = "margin-bottom: 24px;"),
                       
                       withSpinner(
                         DT::dataTableOutput("data_preview"),
                         color = "#00809D"
                       )
                )
              ),
              
              div(class = "download-section",
                  h5("Download Options"),
                  
                  fluidRow(
                    column(6,
                           downloadButton("download_original_data", "Download Original Data (CSV)",
                                          class = "btn btn-success",
                                          style = "width: 100%;")
                    ),
                    column(6,
                           downloadButton("download_processed_data", "Download Processed Data (CSV)",
                                          class = "btn btn-success",
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
            title = tagList(icon("search"), "Eksplorasi Data"), 
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
                                    style = "width: 100%; margin-bottom: 24px;")
                ),
                
                column(8,
                       withSpinner(
                         plotlyOutput("exploration_plot", height = "400px"),
                         color = "#00809D"
                       )
                )
              ),
              
              hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
              
              fluidRow(
                column(6,
                       div(class = "stat-card",
                           h5("Statistik Deskriptif"),
                           verbatimTextOutput("descriptive_stats")
                       )
                ),
                
                column(6,
                       div(class = "interpretation-box",
                           h5("Interpretasi Statistik"),
                           verbatimTextOutput("stats_interpretation")
                       )
                )
              ),
              
              div(class = "download-section",
                  h5("Download Options"),
                  
                  fluidRow(
                    column(4,
                           downloadButton("download_plot", "Download Plot (PNG)",
                                          class = "btn btn-success",
                                          style = "width: 100%;")
                    )
                  )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              div(class = "empty-state",
                  tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 16px;"),
                  h4("Silakan load data terlebih dahulu di menu Manajemen Data")
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
            title = tagList(icon("check-circle"), "Uji Asumsi Data"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(6,
                       h4("Uji Normalitas", style = "margin-bottom: 24px;"),
                       
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
                                    style = "width: 100%; margin-bottom: 24px;"),
                       
                       conditionalPanel(
                         condition = "output.normality_done == true",
                         div(class = "stat-card",
                             h5("Hasil Uji Normalitas"),
                             verbatimTextOutput("normality_result")
                         )
                       )
                ),
                
                column(6,
                       h4("Uji Homogenitas", style = "margin-bottom: 24px;"),
                       
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
                                    style = "width: 100%; margin-bottom: 24px;"),
                       
                       conditionalPanel(
                         condition = "output.homogeneity_done == true",
                         div(class = "stat-card",
                             h5("Hasil Uji Homogenitas"),
                             verbatimTextOutput("homogeneity_result")
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.normality_done == true || output.homogeneity_done == true",
                
                hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
                
                fluidRow(
                  column(12,
                         div(class = "interpretation-box",
                             h5("Interpretasi Uji Asumsi"),
                             verbatimTextOutput("assumptions_interpretation")
                         )
                  )
                ),
                
                div(class = "download-section",
                    h5("Download Options"),
                    
                    fluidRow(
                      column(4,
                             downloadButton("download_assumptions_plots", "Download Plots (PNG)",
                                            class = "btn btn-success",
                                            style = "width: 100%;")
                      )
                    )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              div(class = "empty-state",
                  tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 16px;"),
                  h4("Silakan load data terlebih dahulu di menu Manajemen Data")
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
            title = tagList(icon("chart-line"), "Uji Beda Rata-rata"), 
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
                                    style = "width: 100%; margin-bottom: 24px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.mean_test_done == true",
                         
                         div(class = "stat-card",
                             h5("Hasil Uji Beda Rata-rata"),
                             verbatimTextOutput("mean_test_result")
                         ),
                         
                         withSpinner(
                           plotlyOutput("mean_test_plot", height = "300px"),
                           color = "#00809D"
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.mean_test_done == true",
                
                hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
                
                fluidRow(
                  column(12,
                         div(class = "interpretation-box",
                             h5("Interpretasi Hasil"),
                             verbatimTextOutput("mean_test_interpretation")
                         )
                  )
                ),
                
                div(class = "download-section",
                    h5("Download Options"),
                    
                    fluidRow(
                      column(4,
                             downloadButton("download_mean_test_plot", "Download Plot (PNG)",
                                            class = "btn btn-success",
                                            style = "width: 100%;")
                      )
                    )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              div(class = "empty-state",
                  tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 16px;"),
                  h4("Silakan load data terlebih dahulu di menu Manajemen Data")
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
            title = tagList(icon("pie-chart"), "Uji Proporsi & Variance"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              tabsetPanel(
                
                tabPanel("Uji Proporsi",
                         br(),
                         fluidRow(
                           column(4,
                                  h4("Uji Proporsi", style = "margin-bottom: 24px;"),
                                  
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
                                               style = "width: 100%; margin-bottom: 24px;")
                           ),
                           
                           column(8,
                                  conditionalPanel(
                                    condition = "output.prop_test_done == true",
                                    
                                    div(class = "stat-card",
                                        h5("Hasil Uji Proporsi"),
                                        verbatimTextOutput("prop_test_result")
                                    ),
                                    
                                    withSpinner(
                                      plotlyOutput("prop_test_plot", height = "300px"),
                                      color = "#00809D"
                                    )
                                  )
                           )
                         )
                ),
                
                tabPanel("Uji Variance",
                         br(),
                         fluidRow(
                           column(4,
                                  h4("Uji Variance", style = "margin-bottom: 24px;"),
                                  
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
                                               style = "width: 100%; margin-bottom: 24px;")
                           ),
                           
                           column(8,
                                  conditionalPanel(
                                    condition = "output.var_test_done == true",
                                    
                                    div(class = "stat-card",
                                        h5("Hasil Uji Variance"),
                                        verbatimTextOutput("var_test_result")
                                    ),
                                    
                                    withSpinner(
                                      plotlyOutput("var_test_plot", height = "300px"),
                                      color = "#00809D"
                                    )
                                  )
                           )
                         )
                )
              ),
              
              conditionalPanel(
                condition = "output.prop_test_done == true || output.var_test_done == true",
                
                hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
                
                fluidRow(
                  column(12,
                         div(class = "interpretation-box",
                             h5("Interpretasi Hasil"),
                             verbatimTextOutput("prop_var_interpretation")
                         )
                  )
                ),
                
                div(class = "download-section",
                    h5("Download Options"),
                    
                    fluidRow(
                      column(4,
                             downloadButton("download_prop_var_plots", "Download Plots (PNG)",
                                            class = "btn btn-success",
                                            style = "width: 100%;")
                      )
                    )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              div(class = "empty-state",
                  tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 16px;"),
                  h4("Silakan load data terlebih dahulu di menu Manajemen Data")
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
            title = tagList(icon("chart-area"), "ANOVA (Analysis of Variance)"), 
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
                                    style = "width: 100%; margin-bottom: 24px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.anova_done == true",
                         
                         div(class = "stat-card",
                             h5("ANOVA Results"),
                             verbatimTextOutput("anova_result")
                         ),
                         
                         withSpinner(
                           plotlyOutput("anova_plot", height = "400px"),
                           color = "#00809D"
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.anova_done == true",
                
                hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
                
                fluidRow(
                  column(6,
                         div(class = "stat-card",
                             h5("Post-hoc Tests"),
                             verbatimTextOutput("posthoc_result")
                         )
                  ),
                  
                  column(6,
                         div(class = "stat-card",
                             h5("Effect Size"),
                             verbatimTextOutput("effect_size_result")
                         )
                  )
                ),
                
                fluidRow(
                  column(12,
                         div(class = "interpretation-box",
                             h5("Interpretasi ANOVA"),
                             verbatimTextOutput("anova_interpretation")
                         )
                  )
                ),
                
                div(class = "download-section",
                    h5("Download Options"),
                    
                    fluidRow(
                      column(4,
                             downloadButton("download_anova_plots", "Download Plots (PNG)",
                                            class = "btn btn-success",
                                            style = "width: 100%;")
                      )
                    )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              div(class = "empty-state",
                  tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 16px;"),
                  h4("Silakan load data terlebih dahulu di menu Manajemen Data")
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
            title = tagList(icon("project-diagram"), "Regresi Linear Berganda"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(4,
                       h4("Model Specification", style = "margin-bottom: 24px;"),
                       
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
                                    style = "width: 100%; margin-bottom: 24px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.regression_done == true",
                         
                         div(class = "stat-card",
                             h5("Regression Results"),
                             verbatimTextOutput("regression_result")
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.regression_done == true",
                
                hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
                
                tabsetPanel(
                  
                  tabPanel("Model Summary",
                           br(),
                           fluidRow(
                             column(6,
                                    div(class = "stat-card",
                                        h5("Model Fit Statistics"),
                                        verbatimTextOutput("model_fit_stats")
                                    )
                             ),
                             
                             column(6,
                                    div(class = "stat-card",
                                        h5("Coefficients"),
                                        DT::dataTableOutput("coefficients_table")
                                    )
                             )
                           )
                  ),
                  
                  tabPanel("Diagnostic Plots",
                           br(),
                           fluidRow(
                             column(6,
                                    withSpinner(
                                      plotlyOutput("residual_plot", height = "350px"),
                                      color = "#00809D"
                                    )
                             ),
                             
                             column(6,
                                    withSpinner(
                                      plotlyOutput("qq_plot", height = "350px"),
                                      color = "#00809D"
                                    )
                             )
                           ),
                           
                           br(),
                           
                           fluidRow(
                             column(6,
                                    withSpinner(
                                      plotlyOutput("scale_location_plot", height = "350px"),
                                      color = "#00809D"
                                    )
                             ),
                             
                             column(6,
                                    withSpinner(
                                      plotlyOutput("leverage_plot", height = "350px"),
                                      color = "#00809D"
                                    )
                             )
                           )
                  ),
                  
                  tabPanel("Assumption Tests",
                           br(),
                           fluidRow(
                             column(6,
                                    div(class = "stat-card",
                                        h5("Normality Tests"),
                                        verbatimTextOutput("regression_normality")
                                    )
                             ),
                             
                             column(6,
                                    div(class = "stat-card",
                                        h5("Homoscedasticity Tests"),
                                        verbatimTextOutput("regression_homoscedasticity")
                                    )
                             )
                           ),
                           
                           fluidRow(
                             column(6,
                                    div(class = "stat-card",
                                        h5("Multicollinearity (VIF)"),
                                        verbatimTextOutput("regression_vif")
                                    )
                             ),
                             
                             column(6,
                                    div(class = "stat-card",
                                        h5("Durbin-Watson Test"),
                                        verbatimTextOutput("regression_dw")
                                    )
                             )
                           )
                  )
                ),
                
                fluidRow(
                  column(12,
                         div(class = "interpretation-box",
                             h5("Interpretasi Regresi"),
                             verbatimTextOutput("regression_interpretation")
                         )
                  )
                ),
                
                div(class = "download-section",
                    h5("Download Options"),
                    
                    fluidRow(
                      column(4,
                             downloadButton("download_regression_plots", "Download Plots (PNG)",
                                            class = "btn btn-success",
                                            style = "width: 100%;")
                      )
                    )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              div(class = "empty-state",
                  tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 16px;"),
                  h4("Silakan load data terlebih dahulu di menu Manajemen Data")
              )
            )
          )
        )
      ),
      
      # SPATIAL MAPPING TAB
      tabItem(
        tabName = "spatial_mapping",
        fluidRow(
          box(
            title = tagList(icon("map"), "Pemetaan Spasial dan Analisis Autokorelasi"), 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            conditionalPanel(
              condition = "output.data_loaded == true",
              
              fluidRow(
                column(4,
                       h4("Pengaturan Peta", style = "margin-bottom: 24px;"),
                       
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
                                    style = "width: 100%; margin-bottom: 24px;"),
                       
                       hr(style = "margin: 32px 0; border-color: #e2e8f0;"),
                       
                       h4("Analisis Autokorelasi Spasial", style = "margin-bottom: 24px;"),
                       
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
                                    style = "width: 100%; margin-bottom: 24px;")
                ),
                
                column(8,
                       conditionalPanel(
                         condition = "output.map_generated == true",
                         
                         div(class = "stat-card",
                             h5("Peta Interaktif"),
                             withSpinner(
                               leafletOutput("spatial_map", height = "500px"),
                               color = "#00809D"
                             )
                         )
                       )
                )
              ),
              
              conditionalPanel(
                condition = "output.spatial_analysis_done == true",
                
                hr(style = "margin: 40px 0; border-color: #e2e8f0;"),
                
                fluidRow(
                  column(6,
                         div(class = "stat-card",
                             h5("Hasil Analisis Autokorelasi Spasial"),
                             verbatimTextOutput("spatial_autocorr_result")
                         )
                  ),
                  
                  column(6,
                         div(class = "stat-card",
                             h5("Statistik Matriks Jarak"),
                             verbatimTextOutput("distance_matrix_stats")
                         )
                  )
                ),
                
                fluidRow(
                  column(12,
                         div(class = "interpretation-box",
                             h5("Interpretasi Analisis Spasial"),
                             verbatimTextOutput("spatial_interpretation")
                         )
                  )
                ),
                
                div(class = "download-section",
                    h5("Download Options"),
                    
                    fluidRow(
                      column(4,
                             downloadButton("download_map", "Download Map (PNG)",
                                            class = "btn btn-success",
                                            style = "width: 100%;")
                      )
                    )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.data_loaded == false",
              div(class = "empty-state",
                  tags$i(class = "fas fa-database fa-3x", style = "color: #cbd5e1; margin-bottom: 16px;"),
                  h4("Silakan load data terlebih dahulu di menu Manajemen Data")
              )
            )
          )
        )
      ),
      
      # LAPORAN KOMPREHENSIF TAB
      tabItem(
        tabName = "laporan_komprehensif",
        fluidRow(
          box(
            title = tagList(icon("file-alt"), "Unduh Laporan Analisis Komprehensif"),
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            div(style = "text-align: center; padding: 40px 20px;",
                tags$i(class = "fas fa-file-alt fa-4x", style = "color: #00809D; margin-bottom: 24px;"),
                
                h3("Laporan Komprehensif", style = "margin-bottom: 16px; color: #1e293b;"),
                
                p("Klik tombol di bawah ini untuk mengunduh laporan analisis lengkap dalam format Microsoft Word (.docx). Laporan ini secara otomatis merangkum semua analisis yang telah Anda lakukan di dasbor, mulai dari eksplorasi data hingga regresi linear berganda.", 
                  style = "font-size: 17px; line-height: 1.7; color: #64748b; margin-bottom: 24px; max-width: 800px; margin-left: auto; margin-right: auto;"),
                
                p("Setiap bagian disertai dengan output statistik, visualisasi yang relevan, dan interpretasi mendalam dari sudut pandang seorang ahli statistik untuk mendukung pengambilan keputusan berbasis bukti.", 
                  style = "font-size: 17px; line-height: 1.7; color: #64748b; margin-bottom: 40px; max-width: 800px; margin-left: auto; margin-right: auto;"),
                
                downloadButton("download_word_report", 
                               "Unduh Laporan Lengkap (.docx)", 
                               class = "btn btn-success btn-lg", 
                               style = "font-weight: 600; padding: 20px 40px; font-size: 18px; margin-bottom: 32px;")
            ),
            
            div(class = "interpretation-box",
                style = "margin-top: 32px;",
                h5("Harap Diperhatikan", style = "margin-bottom: 16px;"),
                p(tags$i(class = "fas fa-info-circle", style = "margin-right: 8px; color: #00809D;"), 
                  "Proses pembuatan laporan mungkin memerlukan waktu beberapa saat (tergantung pada jumlah analisis yang dijalankan), karena sistem sedang menggabungkan semua hasil dan visualisasi ke dalam satu dokumen yang kohesif. Mohon bersabar.",
                  style = "margin: 0; line-height: 1.6;")
            )
          )
        )
      )
    )
  )
)