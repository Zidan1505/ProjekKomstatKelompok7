# =============================================
# DASHBOARD MONITOR IKLIM INDONESIA
# Dashboard Analisis Suhu Real-time Indonesia
# =============================================

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(sf)
library(geojsonsf)
library(jsonlite)
library(forecast)
library(lubridate)
library(readxl)
library(stringr)
library(shinyjs)

# =============================================
# DATA PROCESSING
# =============================================

setwd("C:/Tugas Zidan/Sem 4/Komstat/Projek/deploy")
temp_data <- read_excel("data_suhu_lengkap.xlsx")

cat("Data columns:", names(temp_data), "\n")
cat("Data rows:", nrow(temp_data), "\n")

if("date" %in% names(temp_data)) {
  temp_data$date <- as.Date(temp_data$date)
} else {
  stop("Column 'date' not found in data")
}

temp_data$year <- year(temp_data$date)
temp_data$month <- month(temp_data$date)

provinces_climate_data <- temp_data %>%
  group_by(province_name, province_id) %>%
  summarise(
    current_temp = round(mean(Tavg[year == max(year, na.rm = TRUE)], na.rm = TRUE), 1),
    historical_avg = round(mean(Tavg[year <= 2020], na.rm = TRUE), 1),
    data_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(current_temp) & !is.na(historical_avg)) %>%
  mutate(
    temp_anomaly = round(current_temp - historical_avg, 1),
    future_2050 = round(current_temp + (temp_anomaly * 1.5), 1),
    climate_risk_score = round(pmin(10, pmax(1, (temp_anomaly * 2) + (current_temp - 25) * 0.5 + 3)), 1),
    region = case_when(
      grepl("Jawa|Jakarta|Bali", province_name, ignore.case = TRUE) ~ "Jawa-Bali",
      grepl("Sumatra|Aceh|Riau|Jambi|Lampung", province_name, ignore.case = TRUE) ~ "Sumatera",
      grepl("Kalimantan", province_name, ignore.case = TRUE) ~ "Kalimantan",
      grepl("Sulawesi", province_name, ignore.case = TRUE) ~ "Sulawesi",
      grepl("Papua", province_name, ignore.case = TRUE) ~ "Papua",
      grepl("Maluku|Nusa Tenggara", province_name, ignore.case = TRUE) ~ "Maluku-Nusa Tenggara",
      TRUE ~ "Lainnya"
    )
  )

cat("Provinces climate data rows:", nrow(provinces_climate_data), "\n")
cat("Provinces climate data columns:", names(provinces_climate_data), "\n")

monthly_temp_trend <- temp_data %>%
  mutate(
    year_month = floor_date(date, "month"),
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year_month, year, month) %>%
  summarise(
    temperature = round(mean(Tavg, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(!is.na(temperature)) %>%
  arrange(year_month) %>%
  mutate(
    baseline_temp = mean(temperature[year <= 2000], na.rm = TRUE),
    anomaly = round(temperature - baseline_temp, 2)
  )

national_temp_trend <- monthly_temp_trend %>%
  group_by(year) %>%
  summarise(
    temperature = round(mean(temperature, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  mutate(
    baseline_temp = mean(temperature[year <= 2000], na.rm = TRUE),
    anomaly = round(temperature - baseline_temp, 2)
  )

monthly_data <- temp_data %>%
  group_by(month) %>%
  summarise(
    avg_temp = round(mean(Tavg, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_temp)) %>%
  arrange(month) %>%  # Sort by month number (1-12)
  mutate(
    month_name = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", 
                   "Jul", "Ags", "Sep", "Okt", "Nov", "Des")[month],
    seasonal_anomaly = round(avg_temp - mean(avg_temp, na.rm = TRUE), 2)
  )

geojson_path <- "indonesia-prov.geojson"
indonesia_provinces <- NULL

cat("Mencoba memuat GeoJSON dari:", geojson_path, "\n")

if(file.exists(geojson_path)) {
  tryCatch({
    indonesia_provinces <- geojson_sf(geojson_path)
    cat("GeoJSON berhasil dimuat. Kolom:", names(indonesia_provinces), "\n")
    cat("Jumlah provinsi di GeoJSON:", nrow(indonesia_provinces), "\n")
    
    if(!is.null(indonesia_provinces)) {
      # Try different possible column names for province names
      prov_name_cols <- c("NAME_1", "PROVINSI", "Province", "province", "name", "NAME", "Propinsi", "PROPINSI")
      prov_name_col <- NULL
      
      for(col in prov_name_cols) {
        if(col %in% names(indonesia_provinces)) {
          prov_name_col <- col
          cat("Menggunakan kolom provinsi:", col, "\n")
          break
        }
      }
      
      if(!is.null(prov_name_col)) {
        indonesia_provinces$province_clean <- toupper(trimws(gsub("[^A-Za-z0-9 ]", "", indonesia_provinces[[prov_name_col]])))
        provinces_climate_data$province_clean <- toupper(trimws(gsub("[^A-Za-z0-9 ]", "", provinces_climate_data$province_name)))
        
        cat("Sample GeoJSON province names:", head(indonesia_provinces$province_clean, 3), "\n")
        cat("Sample climate data province names:", head(provinces_climate_data$province_clean, 3), "\n")
        
        indonesia_provinces <- indonesia_provinces %>%
          left_join(provinces_climate_data, by = c("province_clean" = "province_clean"))
        
        cat("Setelah merge - kolom tersedia:", names(indonesia_provinces), "\n")
        cat("Jumlah provinsi dengan data iklim:", sum(!is.na(indonesia_provinces$current_temp)), "\n")
        
        if(sum(!is.na(indonesia_provinces$current_temp)) == 0) {
          cat("Merge gagal, mencoba metode alternatif...\n")
          
          province_mapping <- data.frame(
            geojson_name = c("DKI JAKARTA", "JAWA BARAT", "JAWA TENGAH", "JAWA TIMUR", 
                             "SUMATERA UTARA", "SUMATERA BARAT", "KALIMANTAN TIMUR", "SULAWESI SELATAN"),
            climate_name = c("DKI JAKARTA", "JAWA BARAT", "JAWA TENGAH", "JAWA TIMUR",
                             "SUMATERA UTARA", "SUMATERA BARAT", "KALIMANTAN TIMUR", "SULAWESI SELATAN"),
            stringsAsFactors = FALSE
          )
          
          for(i in 1:nrow(province_mapping)) {
            geojson_match <- which(grepl(province_mapping$geojson_name[i], indonesia_provinces$province_clean, ignore.case = TRUE))
            climate_match <- which(grepl(province_mapping$climate_name[i], provinces_climate_data$province_clean, ignore.case = TRUE))
            
            if(length(geojson_match) > 0 && length(climate_match) > 0) {
              climate_row <- provinces_climate_data[climate_match[1], ]
              indonesia_provinces[geojson_match[1], names(climate_row)] <- climate_row
            }
          }
          
          cat("Setelah manual mapping - provinsi dengan data:", sum(!is.na(indonesia_provinces$current_temp)), "\n")
        }
        
      } else {
        cat("Tidak dapat menemukan kolom nama provinsi di GeoJSON\n")
        cat("Kolom yang tersedia:", names(indonesia_provinces), "\n")
      }
    }
  }, error = function(e) {
    cat("Error loading GeoJSON:", e$message, "\n")
    indonesia_provinces <- NULL
  })
} else {
  cat("GeoJSON file tidak ditemukan:", geojson_path, "\n")
  cat("File yang ada di direktori:", list.files(pattern = "*.geojson"), "\n")
}

if(is.null(indonesia_provinces) || sum(!is.na(indonesia_provinces$current_temp)) == 0) {
  cat("Membuat data fallback untuk peta...\n")
  
  fallback_data <- data.frame(
    province_name = provinces_climate_data$province_name,
    current_temp = provinces_climate_data$current_temp,
    historical_avg = provinces_climate_data$historical_avg,
    temp_anomaly = provinces_climate_data$temp_anomaly,
    climate_risk_score = provinces_climate_data$climate_risk_score,
    region = provinces_climate_data$region,

    lat = c(-6.2, -6.9, -7.8, -7.5, 3.6, -0.9, 0.5, -5.1)[1:nrow(provinces_climate_data)],
    lng = c(106.8, 107.6, 110.4, 112.7, 98.7, 100.4, 117.1, 119.4)[1:nrow(provinces_climate_data)],
    stringsAsFactors = FALSE
  )
  
  missing_coords <- is.na(fallback_data$lat) | is.na(fallback_data$lng)
  if(any(missing_coords)) {
    fallback_data$lat[missing_coords] <- runif(sum(missing_coords), -10, 5)
    fallback_data$lng[missing_coords] <- runif(sum(missing_coords), 95, 140)
  }
}

# =============================================
# USER INTERFACE
# =============================================

ui <- fluidPage(
  theme = NULL,
  
  # CSS Styling
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
        font-family: 'Inter', sans-serif;
      }
      
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        color: #333;
        line-height: 1.6;
        min-height: 100vh;
      }
      
      .navbar {
        background: linear-gradient(135deg, #e63946 0%, #d62b39 100%);
        padding: 15px 0;
        color: white;
        box-shadow: 0 4px 20px rgba(230, 57, 70, 0.3);
        position: sticky;
        top: 0;
        z-index: 1000;
      }
      
      .navbar-container {
        display: flex;
        align-items: center;
        justify-content: space-between;
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 20px;
      }
      
      .logo {
        display: flex;
        align-items: center;
        gap: 12px;
        font-size: 26px;
        font-weight: 800;
        color: white;
        text-decoration: none;
        transition: transform 0.3s ease;
      }
      
      .logo:hover {
        transform: scale(1.05);
      }
      
      .logo i {
        font-size: 28px;
        background: rgba(255, 255, 255, 0.2);
        padding: 8px;
        border-radius: 50%;
      }
      
      .nav-links {
        display: flex;
        gap: 35px;
      }
      
      .nav-link {
        color: white;
        text-decoration: none;
        font-weight: 600;
        font-size: 16px;
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 8px 16px;
        border-radius: 25px;
        transition: all 0.3s ease;
        position: relative;
        overflow: hidden;
        border: none;
        background: none;
        cursor: pointer;
      }
      
      .nav-link:hover {
        background: rgba(255, 255, 255, 0.1);
        transform: translateY(-2px);
      }
      
      .nav-link i {
        font-size: 16px;
      }
      
      .container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 40px 20px;
      }
      
      .hero {
        display: flex;
        gap: 50px;
        margin-bottom: 50px;
        align-items: center;
      }
      
      .hero-content {
        flex: 1;
      }
      
      .hero-title {
        font-size: 52px;
        font-weight: 800;
        background: linear-gradient(135deg, #e63946 0%, #d62b39 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        margin-bottom: 25px;
        line-height: 1.1;
      }
      
      .hero-description {
        font-size: 19px;
        color: #555;
        margin-bottom: 15px;
        font-weight: 500;
      }
      
      .hero-buttons {
        display: flex;
        gap: 20px;
        margin-top: 35px;
      }
      
      .btn {
        padding: 15px 30px;
        border-radius: 50px;
        font-weight: 700;
        font-size: 16px;
        cursor: pointer;
        border: none;
        transition: all 0.3s ease;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        position: relative;
        overflow: hidden;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #e63946 0%, #d62b39 100%);
        color: white;
        box-shadow: 0 8px 25px rgba(230, 57, 70, 0.4);
      }
      
      .btn-primary:hover {
        transform: translateY(-3px);
        box-shadow: 0 12px 35px rgba(230, 57, 70, 0.5);
      }
      
      .btn-secondary {
        background: linear-gradient(135deg, #4895ef 0%, #3a85df 100%);
        color: white;
        box-shadow: 0 8px 25px rgba(72, 149, 239, 0.4);
      }
      
      .btn-secondary:hover {
        transform: translateY(-3px);
        box-shadow: 0 12px 35px rgba(72, 149, 239, 0.5);
      }
      
      .hero-map {
        flex: 1;
        background: white;
        border-radius: 20px;
        overflow: hidden;
        box-shadow: 0 15px 35px rgba(0, 0, 0, 0.1);
        min-height: 350px;
        border: 3px solid rgba(230, 57, 70, 0.1);
      }
      
      .stats-container {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 25px;
        margin-bottom: 60px;
      }
      
      .stat-card {
        background: white;
        border-radius: 20px;
        padding: 30px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.1);
        position: relative;
        overflow: hidden;
        transition: all 0.3s ease;
        border: 2px solid transparent;
      }
      
      .stat-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
      }
      
      .stat-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 6px;
        border-radius: 20px 20px 0 0;
      }
      
      .stat-card-red {
        border-color: rgba(230, 57, 70, 0.2);
      }
      
      .stat-card-red::before {
        background: linear-gradient(135deg, #e63946 0%, #d62b39 100%);
      }
      
      .stat-card-orange {
        border-color: rgba(249, 168, 38, 0.2);
      }
      
      .stat-card-orange::before {
        background: linear-gradient(135deg, #f9a826 0%, #e8940f 100%);
      }
      
      .stat-card-blue {
        border-color: rgba(72, 149, 239, 0.2);
      }
      
      .stat-card-blue::before {
        background: linear-gradient(135deg, #4895ef 0%, #3a85df 100%);
      }
      
      .stat-card-purple {
        border-color: rgba(157, 78, 221, 0.2);
      }
      
      .stat-card-purple::before {
        background: linear-gradient(135deg, #9d4edd 0%, #8b3dcc 100%);
      }
      
      .stat-value {
        font-size: 42px;
        font-weight: 800;
        margin-bottom: 8px;
        background: linear-gradient(135deg, #333 0%, #555 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .stat-label {
        font-size: 15px;
        font-weight: 600;
        color: #666;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .stat-icon {
        position: absolute;
        right: 25px;
        bottom: 25px;
        font-size: 50px;
        opacity: 0.1;
        color: #333;
      }
      
      .section-title {
        font-size: 32px;
        font-weight: 800;
        margin-bottom: 30px;
        color: #333;
        position: relative;
        display: inline-block;
      }
      
      .section-title::after {
        content: '';
        position: absolute;
        bottom: -12px;
        left: 0;
        width: 60px;
        height: 4px;
        background: linear-gradient(135deg, #e63946 0%, #d62b39 100%);
        border-radius: 2px;
      }
      
      .insights-container {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 30px;
        margin-bottom: 60px;
      }
      
      .insight-card {
        background: white;
        border-radius: 20px;
        overflow: hidden;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
        border: 2px solid transparent;
      }
      
      .insight-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
        border-color: rgba(230, 57, 70, 0.2);
      }
      
      .insight-header {
        padding: 25px;
        display: flex;
        align-items: center;
        gap: 15px;
        border-bottom: 2px solid #f8f9fa;
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      }
      
      .insight-icon {
        width: 45px;
        height: 45px;
        display: flex;
        align-items: center;
        justify-content: center;
        border-radius: 12px;
        color: white;
        font-size: 20px;
      }
      
      .insight-icon-red {
        background: linear-gradient(135deg, #e63946 0%, #d62b39 100%);
      }
      
      .insight-icon-blue {
        background: linear-gradient(135deg, #4895ef 0%, #3a85df 100%);
      }
      
      .insight-icon-green {
        background: linear-gradient(135deg, #2a9d8f 0%, #238a7a 100%);
      }
      
      .insight-title {
        font-size: 20px;
        font-weight: 700;
        color: #333;
      }
      
      .insight-content {
        padding: 25px;
        height: 280px;
      }
      
      .additional-content {
        margin-top: 60px;
      }
      
      .content-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 40px;
        margin-bottom: 50px;
      }
      
      .content-card {
        background: white;
        border-radius: 20px;
        padding: 40px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.1);
        border: 2px solid rgba(230, 57, 70, 0.1);
        transition: all 0.3s ease;
      }
      
      .content-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
        border-color: rgba(230, 57, 70, 0.3);
      }
      
      .content-card h3 {
        font-size: 24px;
        font-weight: 700;
        color: #e63946;
        margin-bottom: 20px;
        display: flex;
        align-items: center;
        gap: 12px;
      }
      
      .content-card p {
        font-size: 16px;
        color: #666;
        line-height: 1.7;
        margin-bottom: 15px;
      }
      
      .content-card ul {
        list-style: none;
        padding: 0;
      }
      
      .content-card li {
        padding: 8px 0;
        color: #555;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .content-card li::before {
        content: '✓';
        color: #e63946;
        font-weight: bold;
        font-size: 16px;
      }
      
      .tab-content {
        padding: 40px;
        background: white;
        border-radius: 20px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.1);
        border: 2px solid rgba(230, 57, 70, 0.1);
      }
      
      .tab-header {
        text-align: center;
        margin-bottom: 40px;
      }
      
      .tab-title {
        font-size: 42px;
        font-weight: 800;
        background: linear-gradient(135deg, #e63946 0%, #d62b39 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        margin-bottom: 15px;
      }
      
      .tab-description {
        font-size: 18px;
        color: #666;
        max-width: 600px;
        margin: 0 auto;
      }
      
      .control-panel {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 15px;
        padding: 30px;
        margin-bottom: 30px;
        border: 2px solid rgba(230, 57, 70, 0.1);
      }
      
      .control-row {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 20px;
        align-items: end;
      }
      
      .form-group label {
        display: block;
        font-weight: 600;
        color: #333;
        margin-bottom: 8px;
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .form-control,
      .form-select {
        width: 100%;
        padding: 12px 16px;
        border: 2px solid #e9ecef;
        border-radius: 10px;
        font-size: 14px;
        font-weight: 500;
        transition: all 0.3s ease;
        background: white;
      }
      
      .form-control:focus,
      .form-select:focus {
        outline: none;
        border-color: #e63946;
        box-shadow: 0 0 0 3px rgba(230, 57, 70, 0.1);
      }
      
      .chart-container {
        background: white;
        border-radius: 15px;
        padding: 30px;
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.1);
        border: 2px solid rgba(230, 57, 70, 0.1);
      }
      
      .data-source-info {
        background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%);
        border: 2px solid #2a9d8f;
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 30px;
      }
      
      @media (max-width: 992px) {
        .hero {
          flex-direction: column;
          text-align: center;
        }
        
        .stats-container {
          grid-template-columns: repeat(2, 1fr);
        }
        
        .insights-container {
          grid-template-columns: repeat(2, 1fr);
        }
        
        .content-grid {
          grid-template-columns: 1fr;
        }
        
        .nav-links {
          gap: 20px;
        }
      }
      
      @media (max-width: 576px) {
        .stats-container {
          grid-template-columns: 1fr;
        }
        
        .insights-container {
          grid-template-columns: 1fr;
        }
        
        .hero-title {
          font-size: 36px;
        }
        
        .nav-links {
          flex-wrap: wrap;
          gap: 15px;
        }
        
        .control-row {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  
  useShinyjs(),
  
  # Header Navigation
  div(class = "navbar",
      div(class = "navbar-container",
          a(href = "#", class = "logo",
            tags$i(class = "fas fa-thermometer-half"),
            "Monitor Iklim Indonesia"
          ),
          div(class = "nav-links",
              actionLink("nav_beranda", 
                         div(class = "nav-link",
                             tags$i(class = "fas fa-home"),
                             "Beranda"
                         )
              ),
              actionLink("nav_tren",
                         div(class = "nav-link",
                             tags$i(class = "fas fa-chart-line"),
                             "Tren"
                         )
              ),
              actionLink("nav_peta",
                         div(class = "nav-link",
                             tags$i(class = "fas fa-map-marked-alt"),
                             "Peta"
                         )
              ),
              actionLink("nav_prediksi",
                         div(class = "nav-link",
                             tags$i(class = "fas fa-chart-area"),
                             "Prediksi"
                         )
              ),
              actionLink("nav_tentang",
                         div(class = "nav-link",
                             tags$i(class = "fas fa-info-circle"),
                             "Tentang"
                         )
              )
          )
      )
  ),
  
  # Main Container
  div(class = "container",
      
      # Data Source Info
      div(class = "data-source-info",
          tags$strong("📊 Sumber Data: "),
          "Data suhu dimuat dari data_suhu_lengkap.xlsx | ",
          textOutput("data_info", inline = TRUE)
      ),
      
      # Tab Content
      div(id = "tab_content",
          
          # BERANDA TAB
          div(id = "beranda_content",
              
              div(class = "hero",
                  div(class = "hero-content",
                      h1(class = "hero-title", "Selamat Datang"),
                      p(class = "hero-description",
                        "Monitor Iklim Indonesia merupakan platform terpusat untuk menyajikan data iklim dan suhu yang kompleks menjadi wawasan yang jelas dan mudah dipahami."),
                      p(class = "hero-description",
                        "Jelajahi tren suhu, variasi regional, dan proyeksi iklim di seluruh provinsi Indonesia."),
                      div(class = "hero-buttons",
                          actionButton("jelajahi_data", "JELAJAHI DATA", class = "btn btn-primary"),
                          actionButton("pelajari_lebih", "PELAJARI LEBIH", class = "btn btn-secondary")
                      )
                  ),
                  div(class = "hero-map",
                      leafletOutput("preview_map", height = "350px")
                  )
              ),
              
              div(class = "stats-container",
                  div(class = "stat-card stat-card-red",
                      div(class = "stat-value", textOutput("suhu_tertinggi")),
                      div(class = "stat-label", "Suhu Tertinggi Saat Ini"),
                      tags$i(class = "fas fa-temperature-high stat-icon")
                  ),
                  div(class = "stat-card stat-card-orange",
                      div(class = "stat-value", textOutput("rata_rata_suhu")),
                      div(class = "stat-label", "Rata-rata Suhu Nasional"),
                      tags$i(class = "fas fa-globe-asia stat-icon")
                  ),
                  div(class = "stat-card stat-card-blue",
                      div(class = "stat-value", textOutput("anomali_suhu")),
                      div(class = "stat-label", "Rata-rata Anomali Suhu"),
                      tags$i(class = "fas fa-exclamation-triangle stat-icon")
                  ),
                  div(class = "stat-card stat-card-purple",
                      div(class = "stat-value", textOutput("cakupan_data")),
                      div(class = "stat-label", "Provinsi dengan Data"),
                      tags$i(class = "fas fa-chart-line stat-icon")
                  )
              ),
              
              h2(class = "section-title", "Analisis Suhu"),
              div(class = "insights-container",
                  div(class = "insight-card",
                      div(class = "insight-header",
                          div(class = "insight-icon insight-icon-red",
                              tags$i(class = "fas fa-chart-line")
                          ),
                          div(class = "insight-title", "Tren Nasional")
                      ),
                      div(class = "insight-content",
                          plotlyOutput("grafik_tren_nasional", height = "250px")
                      )
                  ),
                  div(class = "insight-card",
                      div(class = "insight-header",
                          div(class = "insight-icon insight-icon-blue",
                              tags$i(class = "fas fa-map-marked-alt")
                          ),
                          div(class = "insight-title", "Perbandingan Regional")
                      ),
                      div(class = "insight-content",
                          plotlyOutput("grafik_perbandingan_regional", height = "250px")
                      )
                  ),
                  div(class = "insight-card",
                      div(class = "insight-header",
                          div(class = "insight-icon insight-icon-green",
                              tags$i(class = "fas fa-calendar-alt")
                          ),
                          div(class = "insight-title", "Pola Musiman")
                      ),
                      div(class = "insight-content",
                          plotlyOutput("grafik_pola_musiman", height = "250px")
                      )
                  )
              ),
              
              div(class = "additional-content",
                  h2(class = "section-title", "Analisis Distribusi Suhu"),
                  div(class = "chart-container",
                      plotlyOutput("grafik_distribusi_suhu", height = "400px")
                  )
              )
          ),
          
          # TREN TAB
          div(id = "tren_content", style = "display: none;",
              div(class = "tab-header",
                  h1(class = "tab-title", "📈 Tren Suhu"),
                  p(class = "tab-description", "Analisis suhu nasional dari waktu ke waktu berdasarkan data Excel. Lacak jejak perubahan suhu dari waktu ke waktu.")
              ),
              
              div(class = "tab-content",
                  div(class = "chart-container",
                      plotlyOutput("grafik_tren_detail", height = "500px")
                  ),
                  
                  div(style = "margin-top: 30px;",
                      div(class = "content-grid",
                          div(class = "content-card",
                              h3(tags$i(class = "fas fa-chart-bar"), "Statistik Tren"),
                              htmlOutput("statistik_tren")
                          ),
                          div(class = "content-card",
                              h3(tags$i(class = "fas fa-thermometer-half"), "Ekstrem Suhu"),
                              htmlOutput("ekstrem_suhu")
                          )
                      )
                  )
              )
          ),
          
          # PETA TAB
          div(id = "peta_content", style = "display: none;",
              div(class = "tab-header",
                  h1(class = "tab-title", "🗺️ Peta Suhu"),
                  p(class = "tab-description", "Peta interaktif berisi variasi suhu antar provinsi Indonesia. Visualisasikan sebaran panas di seluruh Indonesia secara instan.")
              ),
              
              div(class = "control-panel",
                  div(class = "control-row",
                      div(class = "form-group",
                          tags$label("Parameter Tampilan"),
                          selectInput("parameter_peta", NULL,
                                      choices = c("Suhu Saat Ini" = "current_temp",
                                                  "Rata-rata Historis" = "historical_avg",
                                                  "Anomali Suhu" = "temp_anomaly",
                                                  "Skor Risiko Iklim" = "climate_risk_score"),
                                      selected = "current_temp")
                      ),
                      div(class = "form-group",
                          tags$label("Perbarui Peta"),
                          actionButton("perbarui_peta", "Perbarui Visualisasi", 
                                       class = "btn btn-primary", 
                                       style = "width: 100%; margin-top: 8px;")
                      )
                  )
              ),
              
              div(class = "chart-container",
                  leafletOutput("peta_iklim", height = "600px")
              ),
              
          ),
          
          # PREDIKSI TAB
          div(id = "prediksi_content", style = "display: none;",
              
              div(class = "tab-header",
                  h1(class = "tab-title", "🔮 Prediksi Suhu"),
                  p(class = "tab-description", "Prediksi suhu bulanan berbasis ARIMA menggunakan data time series. Lihat ke masa depan dengan prediksi suhu yang akurat.")
              ),
              
              div(class = "tab-content",
                  div(class = "control-panel",
                      div(class = "control-row",
                          div(class = "form-group",
                              tags$label("Periode Prediksi (Bulan)"),
                              selectInput("bulan_prediksi", NULL,
                                          choices = c("6 Bulan" = "6",
                                                      "12 Bulan (1 Tahun)" = "12",
                                                      "24 Bulan (2 Tahun)" = "24",
                                                      "36 Bulan (3 Tahun)" = "36",
                                                      "60 Bulan (5 Tahun)" = "60"),
                                          selected = "12")
                          ),
                          div(class = "form-group",
                              tags$label("Jalankan Prediksi"),
                              actionButton("perbarui_prediksi", "Jalankan Model ARIMA", 
                                           class = "btn btn-primary", 
                                           style = "width: 100%; margin-top: 8px;")
                          )
                      )
                  ),
                  
                  div(class = "chart-container",
                      plotlyOutput("grafik_prediksi_arima", height = "500px")
                  ),
                  
                  div(style = "margin-top: 30px;",
                      div(class = "content-grid",
                          div(class = "content-card",
                              h3(tags$i(class = "fas fa-cogs"), " Performa Model"),
                              uiOutput("performa_model")
                          ),
                          div(class = "content-card",
                              h3(tags$i(class = "fas fa-chart-line"), " Ringkasan Prediksi"),
                              uiOutput("ringkasan_prediksi")
                          )
                      )
                  )
              )
          ),
          
          # TENTANG TAB
          div(id = "tentang_content", style = "display: none;",
              div(class = "tab-header",
                  h1(class = "tab-title", "ℹ️ Tentang Dashboard Ini"),
                  p(class = "tab-description", "Informasi tentang sumber data, metodologi, dan implementasi teknis")
              ),
              
              div(class = "tab-content",
                  div(class = "content-grid",
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-database"), "Sumber Data"),
                          p("Dashboard ini menggunakan data suhu dari file data_suhu_lengkap.xlsx yang berisi:"),
                          tags$ul(
                            tags$li("Tanggal: Titik data time series"),
                            tags$li("Tavg: Pengukuran suhu rata-rata"),
                            tags$li("ID Provinsi: Pengenal unik provinsi"),
                            tags$li("Nama Provinsi: Nama provinsi Indonesia")
                          ),
                          div(id = "info_ringkasan_data")
                      ),
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-cogs"), "Detail Teknis"),
                          p("Dibangun menggunakan R Shiny dengan fitur-fitur utama:"),
                          tags$ul(
                            tags$li("Visualisasi suhu interaktif"),
                            tags$li("Prediksi time series ARIMA"),
                            tags$li("Pemetaan geospasial dengan Leaflet"),
                            tags$li("Pemrosesan data real-time"),
                            tags$li("Desain web responsif")
                          )
                      )
                  ),
                  
                  h2(class = "section-title", "Dataset Lengkap"),
                  div(class = "chart-container",
                      DT::dataTableOutput("tabel_data_lengkap")
                  )
              )
          )
      )
  )
)

# =============================================
# SERVER LOGIC
# =============================================

server <- function(input, output, session) {
  
  active_tab <- reactiveVal("beranda")
  
  observeEvent(input$nav_beranda, {
    active_tab("beranda")
    shinyjs::hide("tren_content")
    shinyjs::hide("peta_content")
    shinyjs::hide("prediksi_content")
    shinyjs::hide("tentang_content")
    shinyjs::show("beranda_content")
  })
  
  observeEvent(input$nav_tren, {
    active_tab("tren")
    shinyjs::hide("beranda_content")
    shinyjs::hide("peta_content")
    shinyjs::hide("prediksi_content")
    shinyjs::hide("tentang_content")
    shinyjs::show("tren_content")
  })
  
  observeEvent(input$nav_peta, {
    active_tab("peta")
    shinyjs::hide("beranda_content")
    shinyjs::hide("tren_content")
    shinyjs::hide("prediksi_content")
    shinyjs::hide("tentang_content")
    shinyjs::show("peta_content")
  })
  
  observeEvent(input$nav_prediksi, {
    active_tab("prediksi")
    shinyjs::hide("beranda_content")
    shinyjs::hide("tren_content")
    shinyjs::hide("peta_content")
    shinyjs::hide("tentang_content")
    shinyjs::show("prediksi_content")
  })
  
  observeEvent(input$nav_tentang, {
    active_tab("tentang")
    shinyjs::hide("beranda_content")
    shinyjs::hide("tren_content")
    shinyjs::hide("peta_content")
    shinyjs::hide("prediksi_content")
    shinyjs::show("tentang_content")
  })
  
  observeEvent(input$jelajahi_data, {
    active_tab("peta")
    shinyjs::hide("beranda_content")
    shinyjs::hide("tren_content")
    shinyjs::hide("prediksi_content")
    shinyjs::hide("tentang_content")
    shinyjs::show("peta_content")
  })
  
  observeEvent(input$pelajari_lebih, {
    active_tab("tentang")
    shinyjs::hide("beranda_content")
    shinyjs::hide("tren_content")
    shinyjs::hide("peta_content")
    shinyjs::hide("prediksi_content")
    shinyjs::show("tentang_content")
  })
  
  # Data info
  output$data_info <- renderText({
    paste("Memuat", nrow(temp_data), "rekord suhu dari",
          length(unique(temp_data$province_name)), "provinsi |",
          "Rentang tanggal:", min(temp_data$date), "hingga", max(temp_data$date))
  })
  
  output$suhu_tertinggi <- renderText({
    if(nrow(provinces_climate_data) > 0 && "current_temp" %in% names(provinces_climate_data)) {
      max_temp <- max(provinces_climate_data$current_temp, na.rm = TRUE)
      paste0(max_temp, "°C")
    } else {
      "N/A"
    }
  })
  
  output$rata_rata_suhu <- renderText({
    if(nrow(provinces_climate_data) > 0 && "current_temp" %in% names(provinces_climate_data)) {
      avg_temp <- mean(provinces_climate_data$current_temp, na.rm = TRUE)
      paste0(round(avg_temp, 1), "°C")
    } else {
      "N/A"
    }
  })
  
  output$anomali_suhu <- renderText({
    if(nrow(provinces_climate_data) > 0 && "temp_anomaly" %in% names(provinces_climate_data)) {
      avg_anomaly <- mean(provinces_climate_data$temp_anomaly, na.rm = TRUE)
      paste0(ifelse(avg_anomaly >= 0, "+", ""), round(avg_anomaly, 1), "°C")
    } else {
      "N/A"
    }
  })
  
  output$cakupan_data <- renderText({
    paste0(nrow(provinces_climate_data), "/", length(unique(temp_data$province_name)))
  })
  
  # Preview map
  output$preview_map <- renderLeaflet({
    if(is.null(indonesia_provinces) || nrow(provinces_climate_data) == 0) {
      leaflet() %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addControl("Peta Pratinjau Indonesia", position = "topright")
    } else {
      if("current_temp" %in% names(indonesia_provinces)) {
        colors <- colorNumeric("Reds", indonesia_provinces$current_temp, na.color = "#808080")
        
        leaflet(indonesia_provinces) %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor = ~colors(current_temp),
            weight = 1,
            opacity = 1,
            color = "white",
            fillOpacity = 0.6,
            popup = ~paste0("<b>", province_name, "</b><br/>", 
                            "Suhu: ", current_temp, "°C")
          ) %>%
          addControl("Peta Pratinjau Indonesia", position = "topright")
      } else {
        leaflet() %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addControl("Peta Pratinjau Indonesia", position = "topright")
      }
    }
  })
  
  # Grafik
  output$grafik_tren_nasional <- renderPlotly({
    if(nrow(national_temp_trend) == 0) {
      return(plotly_empty() %>% layout(title = "Data tren tidak tersedia"))
    }
    
    plot_ly(national_temp_trend, x = ~year, y = ~temperature, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#e63946', width = 3),
            marker = list(color = '#e63946', size = 6),
            hovertemplate = 'Tahun: %{x}<br>Suhu: %{y:.1f}°C<extra></extra>') %>%
      layout(
        xaxis = list(title = "Tahun"),
        yaxis = list(title = "Suhu (°C)"),
        margin = list(l = 50, r = 20, t = 20, b = 50),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$grafik_perbandingan_regional <- renderPlotly({
    if(nrow(provinces_climate_data) == 0) {
      return(plotly_empty() %>% layout(title = "Data regional tidak tersedia"))
    }
    
    regional_data <- provinces_climate_data %>%
      group_by(region) %>%
      summarise(
        current_avg = mean(current_temp, na.rm = TRUE),
        historical_avg = mean(historical_avg, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(regional_data, x = ~region, y = ~current_avg, 
            type = 'bar', name = 'Saat Ini',
            marker = list(color = '#e63946'),
            hovertemplate = 'Region: %{x}<br>Saat Ini: %{y:.1f}°C<extra></extra>') %>%
      add_trace(y = ~historical_avg, name = 'Historis', 
                marker = list(color = '#4895ef'),
                hovertemplate = 'Region: %{x}<br>Historis: %{y:.1f}°C<extra></extra>') %>%
      layout(
        xaxis = list(title = "Region"),
        yaxis = list(title = "Suhu (°C)"),
        barmode = 'group',
        margin = list(l = 50, r = 20, t = 20, b = 80),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Grafik musiman
  output$grafik_pola_musiman <- renderPlotly({
    if(nrow(monthly_data) == 0) {
      return(plotly_empty() %>% layout(title = "Data musiman tidak tersedia"))
    }
    
    monthly_data_ordered <- monthly_data %>%
      arrange(month) %>%
      mutate(month_name = factor(month_name, levels = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", 
                                                        "Jul", "Ags", "Sep", "Okt", "Nov", "Des")))
    
    plot_ly(monthly_data_ordered, x = ~month_name, y = ~avg_temp, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#2a9d8f', width = 3),
            marker = list(color = '#2a9d8f', size = 8),
            hovertemplate = '<b>%{x}</b><br>Suhu: %{y:.1f}°C<extra></extra>') %>%
      layout(
        xaxis = list(title = "Bulan", tickangle = -45, 
                     categoryorder = "array",
                     categoryarray = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", 
                                       "Jul", "Ags", "Sep", "Okt", "Nov", "Des")),
        yaxis = list(title = "Suhu (°C)"),
        margin = list(l = 50, r = 20, t = 20, b = 80),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$grafik_distribusi_suhu <- renderPlotly({
    if(nrow(provinces_climate_data) == 0 || !"current_temp" %in% names(provinces_climate_data)) {
      return(plotly_empty() %>% layout(title = "Data distribusi tidak tersedia"))
    }
    
    temp_values <- provinces_climate_data$current_temp[!is.na(provinces_climate_data$current_temp)]
    
    if(length(temp_values) == 0) {
      return(plotly_empty() %>% layout(title = "Data suhu tidak tersedia"))
    }
    
    plot_ly(x = temp_values, type = "histogram", nbinsx = 20,
            marker = list(color = '#e63946', opacity = 0.8),
            hovertemplate = 'Rentang Suhu: %{x:.1f}°C<br>Jumlah: %{y} provinsi<extra></extra>') %>%
      layout(
        title = "Distribusi Suhu di Provinsi Indonesia",
        xaxis = list(title = "Suhu (°C)"),
        yaxis = list(title = "Jumlah Provinsi"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Grafik Tren
  output$grafik_tren_detail <- renderPlotly({
    if(nrow(national_temp_trend) < 2) {
      return(plotly_empty() %>% layout(title = "Data tidak cukup untuk analisis tren"))
    }
    
    lm_model <- lm(temperature ~ year, data = national_temp_trend)
    trend_line <- predict(lm_model, newdata = national_temp_trend)
    
    plot_ly(national_temp_trend, x = ~year, y = ~temperature, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#e63946', width = 3), 
            marker = list(color = '#e63946', size = 8),
            name = 'Suhu Observasi', 
            hovertemplate = 'Tahun: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>') %>%
      add_trace(x = national_temp_trend$year, y = trend_line, 
                type = 'scatter', mode = 'lines',
                line = list(color = '#2a9d8f', width = 2, dash = 'dash'),
                name = 'Tren Linear', 
                hovertemplate = 'Tren: %{y:.2f}°C<extra></extra>') %>%
      layout(
        title = "Tren Suhu Nasional dari Waktu ke Waktu",
        xaxis = list(title = "Tahun"),
        yaxis = list(title = "Suhu (°C)"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Trend statistik
  output$statistik_tren <- renderUI({
    if(nrow(national_temp_trend) < 2) {
      return(p("Data tren tidak tersedia"))
    }
    
    lm_model <- lm(temperature ~ year, data = national_temp_trend)
    warming_rate <- round(coef(lm_model)[2], 4)
    r_squared <- round(summary(lm_model)$r.squared, 3)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Periode Data: "), 
          paste(min(national_temp_trend$year), "-", max(national_temp_trend$year))
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Rata-rata Suhu: "), 
          paste0(round(mean(national_temp_trend$temperature), 2), "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Laju Pemanasan: "), 
          paste0(warming_rate, "°C/tahun")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Kekuatan Tren (R²): "), r_squared
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Total Perubahan: "),
          paste0(round(warming_rate * (max(national_temp_trend$year) - min(national_temp_trend$year)), 2), "°C")
      )
    )
  })
  
  # Temperatur extremes
  output$ekstrem_suhu <- renderUI({
    if(nrow(provinces_climate_data) == 0 || !"current_temp" %in% names(provinces_climate_data)) {
      return(p("Data ekstrem tidak tersedia"))
    }
    
    valid_temps <- provinces_climate_data$current_temp[!is.na(provinces_climate_data$current_temp)]
    valid_anomalies <- provinces_climate_data$temp_anomaly[!is.na(provinces_climate_data$temp_anomaly)]
    
    if(length(valid_temps) == 0) {
      return(p("Data suhu tidak tersedia"))
    }
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Tertinggi Provinsi: "),
          paste0(max(valid_temps), "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Terendah Provinsi: "),
          paste0(min(valid_temps), "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Rentang Suhu: "),
          paste0(round(max(valid_temps) - min(valid_temps), 1), "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Anomali Terbesar: "),
          if(length(valid_anomalies) > 0) {
            paste0("+", max(valid_anomalies), "°C")
          } else {
            "N/A"
          }
      )
    )
  })
  
  # Climate map
  output$peta_iklim <- renderLeaflet({
    
    cat("Rendering peta iklim...\n")
    
    if(is.null(indonesia_provinces)) {
      cat("Menggunakan fallback data untuk peta\n")
      
      if(exists("fallback_data") && nrow(fallback_data) > 0) {
        
        param <- input$parameter_peta
        if(is.null(param)) param <- "current_temp"
        
        if(param == "current_temp") {
          values <- fallback_data$current_temp
          colors <- colorNumeric("Reds", values, na.color = "#808080")
          legend_title <- "Suhu Saat Ini (°C)"
        } else if(param == "historical_avg") {
          values <- fallback_data$historical_avg
          colors <- colorNumeric("Blues", values, na.color = "#808080")
          legend_title <- "Rata-rata Historis (°C)"
        } else if(param == "temp_anomaly") {
          values <- fallback_data$temp_anomaly
          colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
          legend_title <- "Anomali Suhu (°C)"
        } else {
          values <- fallback_data$climate_risk_score
          colors <- colorNumeric("OrRd", values, na.color = "#808080")
          legend_title <- "Skor Risiko Iklim"
        }
        
        leaflet(fallback_data) %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addCircleMarkers(
            lng = ~lng, lat = ~lat,
            radius = ~sqrt(values) * 3,
            fillColor = ~colors(values),
            color = "white",
            weight = 2,
            opacity = 1,
            fillOpacity = 0.8,
            popup = ~paste0("<b>", province_name, "</b><br/>", 
                            "Suhu Saat Ini: ", current_temp, "°C<br/>",
                            "Rata-rata Historis: ", historical_avg, "°C<br/>",
                            "Anomali: ", temp_anomaly, "°C<br/>",
                            "Skor Risiko: ", climate_risk_score, "/10<br/>",
                            "Region: ", region)
          ) %>%
          addLegend(
            pal = colors, 
            values = ~values,
            opacity = 0.7, 
            title = legend_title,
            position = "bottomright"
          ) %>%
          addControl("Peta Iklim Indonesia (Mode Titik)", position = "topright")
        
      } else {
        leaflet() %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addControl("Peta Iklim Indonesia - Data tidak tersedia", position = "topright")
      }
      
    } else {
      cat("Menggunakan data polygon untuk peta\n")
      
      param <- input$parameter_peta
      if(is.null(param)) param <- "current_temp"
      
      if(!param %in% names(indonesia_provinces)) {
        cat("Parameter", param, "tidak ditemukan. Kolom tersedia:", names(indonesia_provinces), "\n")
        
        leaflet() %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addControl(paste("Parameter", param, "tidak tersedia"), position = "topright")
      } else {
        
        if(param == "current_temp") {
          values <- indonesia_provinces$current_temp
          colors <- colorNumeric("Reds", values, na.color = "#808080")
          legend_title <- "Suhu Saat Ini (°C)"
        } else if(param == "historical_avg") {
          values <- indonesia_provinces$historical_avg
          colors <- colorNumeric("Blues", values, na.color = "#808080")
          legend_title <- "Rata-rata Historis (°C)"
        } else if(param == "temp_anomaly") {
          values <- indonesia_provinces$temp_anomaly
          colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
          legend_title <- "Anomali Suhu (°C)"
        } else {
          values <- indonesia_provinces$climate_risk_score
          colors <- colorNumeric("OrRd", values, na.color = "#808080")
          legend_title <- "Skor Risiko Iklim"
        }
        
        labels <- sprintf(
          "<strong>%s</strong><br/>
        Suhu Saat Ini: %s°C<br/>
        Rata-rata Historis: %s°C<br/>
        Anomali: %s°C<br/>
        Skor Risiko: %s/10<br/>
        Region: %s",
          indonesia_provinces$province_name %||% "N/A",
          indonesia_provinces$current_temp %||% "N/A",
          indonesia_provinces$historical_avg %||% "N/A", 
          indonesia_provinces$temp_anomaly %||% "N/A",
          indonesia_provinces$climate_risk_score %||% "N/A",
          indonesia_provinces$region %||% "N/A"
        ) %>% lapply(htmltools::HTML)
        
        leaflet(indonesia_provinces) %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor = ~colors(values),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = colors, 
            values = ~values,
            opacity = 0.7, 
            title = legend_title,
            position = "bottomright"
          ) %>%
          addControl("Peta Iklim Indonesia (Mode Polygon)", position = "topright")
      }
    }
  })
  
  observeEvent(input$perbarui_peta, {
    output$peta_iklim <- renderLeaflet({
      
      cat("Memperbarui peta dengan parameter:", input$parameter_peta, "\n")
      
      if(is.null(indonesia_provinces)) {
        if(exists("fallback_data") && nrow(fallback_data) > 0) {
          
          param <- input$parameter_peta
          if(is.null(param)) param <- "current_temp"
          
          if(param == "current_temp") {
            values <- fallback_data$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- fallback_data$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- fallback_data$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- fallback_data$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          leaflet(fallback_data) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(
              lng = ~lng, lat = ~lat,
              radius = ~sqrt(values) * 3,
              fillColor = ~colors(values),
              color = "white",
              weight = 2,
              opacity = 1,
              fillOpacity = 0.8,
              popup = ~paste0("<b>", province_name, "</b><br/>", 
                              "Parameter: ", legend_title, "<br/>",
                              "Nilai: ", values, "<br/>",
                              "Region: ", region)
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia (Mode Titik) - Diperbarui", position = "topright")
          
        } else {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl("Data tidak tersedia", position = "topright")
        }
      } else {
        param <- input$parameter_peta
        if(is.null(param)) param <- "current_temp"
        
        if(!param %in% names(indonesia_provinces)) {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl(paste("Parameter", param, "tidak tersedia"), position = "topright")
        } else {
          
          if(param == "current_temp") {
            values <- indonesia_provinces$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- indonesia_provinces$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- indonesia_provinces$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- indonesia_provinces$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          labels <- sprintf(
            "<strong>%s</strong><br/>
          %s: %s<br/>
          Region: %s",
            indonesia_provinces$province_name %||% "N/A",
            legend_title,
            values %||% "N/A",
            indonesia_provinces$region %||% "N/A"
          ) %>% lapply(htmltools::HTML)
          
          leaflet(indonesia_provinces) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
              fillColor = ~colors(values),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              )
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia - Diperbarui", position = "topright")
        }
      }
    })
  })
  
  # Data Tabel Province
  output$tabel_provinsi <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        Region = region,
        `Suhu Saat Ini` = current_temp,
        `Rata-rata Historis` = historical_avg,
        `Anomali` = temp_anomaly,
        `Skor Risiko` = climate_risk_score
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(3:6), digits = 1) %>%
      DT::formatStyle(
        columns = c(3:6),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
  
  # ARIMA forecast
  output$grafik_prediksi_arima <- renderPlotly({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(plotly_empty() %>% layout(title = "Data bulanan tidak cukup untuk prediksi ARIMA"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    historical_dates <- monthly_temp_trend$year_month
    last_date <- max(historical_dates, na.rm = TRUE)
    forecast_dates <- seq(from = last_date %m+% months(1), by = "1 month", length.out = forecast_months)
    
    # Plot
    plot_ly() %>%
      add_trace(
        x = historical_dates,
        y = as.numeric(ts_data),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#e63946', width = 2),
        name = 'Data Historis',
        hovertemplate = 'Tanggal: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>'
      ) %>%
      add_trace(
        x = forecast_dates,
        y = as.numeric(forecast_result$mean),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#2a9d8f', width = 3),
        name = 'Prediksi ARIMA',
        hovertemplate = 'Tanggal: %{x}<br>Prediksi: %{y:.2f}°C<extra></extra>'
      ) %>%
      layout(
        title = paste("Prediksi Suhu Bulanan ARIMA -", forecast_months, "Bulan"),
        xaxis = list(title = "Tanggal"),
        yaxis = list(title = "Suhu (°C)"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$performa_model <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk perhitungan performa model"))
    }
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature, 
                  start = c(start_year, start_month), 
                  frequency = 12)
    arima_model <- auto.arima(ts_data)
    
    residuals_data <- residuals(arima_model)
    mae <- round(mean(abs(residuals_data), na.rm = TRUE), 4)
    rmse <- round(sqrt(mean(residuals_data^2, na.rm = TRUE)), 4)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Tipe Model: "), 
          paste0("ARIMA(", paste(arimaorder(arima_model), collapse = ","), ")")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Frekuensi Data: "), "Bulanan"
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("MAE: "), paste0(mae, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("RMSE: "), paste0(rmse, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("AIC: "), round(AIC(arima_model), 2)
      )
    )
  })
  
  output$ringkasan_prediksi <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk ringkasan prediksi"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    final_forecast <- round(as.numeric(forecast_result$mean[forecast_months]), 2)
    current_temp <- round(tail(monthly_temp_trend$temperature, 1), 2)
    temp_change <- round(final_forecast - current_temp, 2)
    
    last_date <- max(monthly_temp_trend$year_month, na.rm = TRUE)
    forecast_end_date <- last_date %m+% months(forecast_months)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Horizon Prediksi: "), paste(forecast_months, "bulan")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Periode Prediksi: "), 
          paste("Hingga", format(forecast_end_date, "%B %Y"))
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Saat Ini: "), paste0(current_temp, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Proyeksi: "), paste0(final_forecast, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Perubahan yang Diharapkan: "), 
          paste0(ifelse(temp_change >= 0, "+", ""), temp_change, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Laju Perubahan Bulanan: "), 
          paste0(round(temp_change / forecast_months, 4), "°C/bulan")
      )
    )
  })
  
  output$tabel_data_lengkap <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        `ID Provinsi` = province_id,
        Region = region,
        `Suhu Saat Ini (°C)` = current_temp,
        `Rata-rata Historis (°C)` = historical_avg,
        `Anomali Suhu (°C)` = temp_anomaly,
        `Skor Risiko Iklim` = climate_risk_score,
        `Tahun Data` = data_years,
        `Tahun Pertama` = first_year,
        `Tahun Terakhir` = last_year
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(4:7), digits = 2) %>%
      DT::formatStyle(
        columns = c(4:7),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
  
  observeEvent(input$perbarui_peta, {
    output$peta_iklim <- renderLeaflet({
      
      cat("Memperbarui peta dengan parameter:", input$parameter_peta, "\n")
      
      if(is.null(indonesia_provinces)) {
        if(exists("fallback_data") && nrow(fallback_data) > 0) {
          
          param <- input$parameter_peta
          if(is.null(param)) param <- "current_temp"
          
          if(param == "current_temp") {
            values <- fallback_data$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- fallback_data$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- fallback_data$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- fallback_data$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          leaflet(fallback_data) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(
              lng = ~lng, lat = ~lat,
              radius = ~sqrt(values) * 3,
              fillColor = ~colors(values),
              color = "white",
              weight = 2,
              opacity = 1,
              fillOpacity = 0.8,
              popup = ~paste0("<b>", province_name, "</b><br/>", 
                              "Parameter: ", legend_title, "<br/>",
                              "Nilai: ", values, "<br/>",
                              "Region: ", region)
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia (Mode Titik) - Diperbarui", position = "topright")
          
        } else {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl("Data tidak tersedia", position = "topright")
        }
      } else {
        param <- input$parameter_peta
        if(is.null(param)) param <- "current_temp"
        
        if(!param %in% names(indonesia_provinces)) {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl(paste("Parameter", param, "tidak tersedia"), position = "topright")
        } else {
          
          if(param == "current_temp") {
            values <- indonesia_provinces$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- indonesia_provinces$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- indonesia_provinces$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- indonesia_provinces$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          labels <- sprintf(
            "<strong>%s</strong><br/>
          %s: %s<br/>
          Region: %s",
            indonesia_provinces$province_name %||% "N/A",
            legend_title,
            values %||% "N/A",
            indonesia_provinces$region %||% "N/A"
          ) %>% lapply(htmltools::HTML)
          
          leaflet(indonesia_provinces) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
              fillColor = ~colors(values),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              )
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia - Diperbarui", position = "topright")
        }
      }
    })
  })
  
  # Data Tabel Provinsi
  output$tabel_provinsi <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        Region = region,
        `Suhu Saat Ini` = current_temp,
        `Rata-rata Historis` = historical_avg,
        `Anomali` = temp_anomaly,
        `Skor Risiko` = climate_risk_score
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(3:6), digits = 1) %>%
      DT::formatStyle(
        columns = c(3:6),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
  
  # ARIMA forecast
  output$grafik_prediksi_arima <- renderPlotly({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(plotly_empty() %>% layout(title = "Data bulanan tidak cukup untuk prediksi ARIMA"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    historical_dates <- monthly_temp_trend$year_month
    last_date <- max(historical_dates, na.rm = TRUE)
    forecast_dates <- seq(from = last_date %m+% months(1), by = "1 month", length.out = forecast_months)
    
    plot_ly() %>%
      add_trace(
        x = historical_dates,
        y = as.numeric(ts_data),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#e63946', width = 2),
        name = 'Data Historis',
        hovertemplate = 'Tanggal: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>'
      ) %>%
      add_trace(
        x = forecast_dates,
        y = as.numeric(forecast_result$mean),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#2a9d8f', width = 3),
        name = 'Prediksi ARIMA',
        hovertemplate = 'Tanggal: %{x}<br>Prediksi: %{y:.2f}°C<extra></extra>'
      ) %>%
      layout(
        title = paste("Prediksi Suhu Bulanan ARIMA -", forecast_months, "Bulan"),
        xaxis = list(title = "Tanggal"),
        yaxis = list(title = "Suhu (°C)"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$performa_model <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk perhitungan performa model"))
    }
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature, 
                  start = c(start_year, start_month), 
                  frequency = 12)
    arima_model <- auto.arima(ts_data)
    
    residuals_data <- residuals(arima_model)
    mae <- round(mean(abs(residuals_data), na.rm = TRUE), 4)
    rmse <- round(sqrt(mean(residuals_data^2, na.rm = TRUE)), 4)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Tipe Model: "), 
          paste0("ARIMA(", paste(arimaorder(arima_model), collapse = ","), ")")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Frekuensi Data: "), "Bulanan"
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("MAE: "), paste0(mae, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("RMSE: "), paste0(rmse, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("AIC: "), round(AIC(arima_model), 2)
      )
    )
  })
  
  output$ringkasan_prediksi <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk ringkasan prediksi"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    final_forecast <- round(as.numeric(forecast_result$mean[forecast_months]), 2)
    current_temp <- round(tail(monthly_temp_trend$temperature, 1), 2)
    temp_change <- round(final_forecast - current_temp, 2)
    
    last_date <- max(monthly_temp_trend$year_month, na.rm = TRUE)
    forecast_end_date <- last_date %m+% months(forecast_months)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Horizon Prediksi: "), paste(forecast_months, "bulan")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Periode Prediksi: "), 
          paste("Hingga", format(forecast_end_date, "%B %Y"))
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Saat Ini: "), paste0(current_temp, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Proyeksi: "), paste0(final_forecast, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Perubahan yang Diharapkan: "), 
          paste0(ifelse(temp_change >= 0, "+", ""), temp_change, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Laju Perubahan Bulanan: "), 
          paste0(round(temp_change / forecast_months, 4), "°C/bulan")
      )
    )
  })
  
  output$tabel_data_lengkap <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        `ID Provinsi` = province_id,
        Region = region,
        `Suhu Saat Ini (°C)` = current_temp,
        `Rata-rata Historis (°C)` = historical_avg,
        `Anomali Suhu (°C)` = temp_anomaly,
        `Skor Risiko Iklim` = climate_risk_score,
        `Tahun Data` = data_years,
        `Tahun Pertama` = first_year,
        `Tahun Terakhir` = last_year
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(4:7), digits = 2) %>%
      DT::formatStyle(
        columns = c(4:7),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })

  observeEvent(input$perbarui_peta, {
    output$peta_iklim <- renderLeaflet({
      
      cat("Memperbarui peta dengan parameter:", input$parameter_peta, "\n")
      
      if(is.null(indonesia_provinces)) {
        if(exists("fallback_data") && nrow(fallback_data) > 0) {
          
          param <- input$parameter_peta
          if(is.null(param)) param <- "current_temp"
          
          if(param == "current_temp") {
            values <- fallback_data$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- fallback_data$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- fallback_data$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- fallback_data$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          leaflet(fallback_data) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(
              lng = ~lng, lat = ~lat,
              radius = ~sqrt(values) * 3,
              fillColor = ~colors(values),
              color = "white",
              weight = 2,
              opacity = 1,
              fillOpacity = 0.8,
              popup = ~paste0("<b>", province_name, "</b><br/>", 
                              "Parameter: ", legend_title, "<br/>",
                              "Nilai: ", values, "<br/>",
                              "Region: ", region)
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia (Mode Titik) - Diperbarui", position = "topright")
          
        } else {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl("Data tidak tersedia", position = "topright")
        }
      } else {
        param <- input$parameter_peta
        if(is.null(param)) param <- "current_temp"
        
        if(!param %in% names(indonesia_provinces)) {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl(paste("Parameter", param, "tidak tersedia"), position = "topright")
        } else {
          
          if(param == "current_temp") {
            values <- indonesia_provinces$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- indonesia_provinces$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- indonesia_provinces$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- indonesia_provinces$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          labels <- sprintf(
            "<strong>%s</strong><br/>
          %s: %s<br/>
          Region: %s",
            indonesia_provinces$province_name %||% "N/A",
            legend_title,
            values %||% "N/A",
            indonesia_provinces$region %||% "N/A"
          ) %>% lapply(htmltools::HTML)
          
          leaflet(indonesia_provinces) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
              fillColor = ~colors(values),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              )
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia - Diperbarui", position = "topright")
        }
      }
    })
  })
  
  output$tabel_provinsi <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        Region = region,
        `Suhu Saat Ini` = current_temp,
        `Rata-rata Historis` = historical_avg,
        `Anomali` = temp_anomaly,
        `Skor Risiko` = climate_risk_score
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(3:6), digits = 1) %>%
      DT::formatStyle(
        columns = c(3:6),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
  
  # ARIMA forecast
  output$grafik_prediksi_arima <- renderPlotly({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(plotly_empty() %>% layout(title = "Data bulanan tidak cukup untuk prediksi ARIMA"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    historical_dates <- monthly_temp_trend$year_month
    last_date <- max(historical_dates, na.rm = TRUE)
    forecast_dates <- seq(from = last_date %m+% months(1), by = "1 month", length.out = forecast_months)
    
    # Plot
    plot_ly() %>%
      add_trace(
        x = historical_dates,
        y = as.numeric(ts_data),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#e63946', width = 2),
        name = 'Data Historis',
        hovertemplate = 'Tanggal: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>'
      ) %>%
      add_trace(
        x = forecast_dates,
        y = as.numeric(forecast_result$mean),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#2a9d8f', width = 3),
        name = 'Prediksi ARIMA',
        hovertemplate = 'Tanggal: %{x}<br>Prediksi: %{y:.2f}°C<extra></extra>'
      ) %>%
      layout(
        title = paste("Prediksi Suhu Bulanan ARIMA -", forecast_months, "Bulan"),
        xaxis = list(title = "Tanggal"),
        yaxis = list(title = "Suhu (°C)"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Model performance
  output$performa_model <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk perhitungan performa model"))
    }
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature, 
                  start = c(start_year, start_month), 
                  frequency = 12)
    arima_model <- auto.arima(ts_data)
    
    residuals_data <- residuals(arima_model)
    mae <- round(mean(abs(residuals_data), na.rm = TRUE), 4)
    rmse <- round(sqrt(mean(residuals_data^2, na.rm = TRUE)), 4)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Tipe Model: "), 
          paste0("ARIMA(", paste(arimaorder(arima_model), collapse = ","), ")")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Frekuensi Data: "), "Bulanan"
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("MAE: "), paste0(mae, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("RMSE: "), paste0(rmse, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("AIC: "), round(AIC(arima_model), 2)
      )
    )
  })
  
  # Forecast summary
  output$ringkasan_prediksi <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk ringkasan prediksi"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    final_forecast <- round(as.numeric(forecast_result$mean[forecast_months]), 2)
    current_temp <- round(tail(monthly_temp_trend$temperature, 1), 2)
    temp_change <- round(final_forecast - current_temp, 2)
    
    last_date <- max(monthly_temp_trend$year_month, na.rm = TRUE)
    forecast_end_date <- last_date %m+% months(forecast_months)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Horizon Prediksi: "), paste(forecast_months, "bulan")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Periode Prediksi: "), 
          paste("Hingga", format(forecast_end_date, "%B %Y"))
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Saat Ini: "), paste0(current_temp, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Proyeksi: "), paste0(final_forecast, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Perubahan yang Diharapkan: "), 
          paste0(ifelse(temp_change >= 0, "+", ""), temp_change, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Laju Perubahan Bulanan: "), 
          paste0(round(temp_change / forecast_months, 4), "°C/bulan")
      )
    )
  })
  
  output$tabel_data_lengkap <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        `ID Provinsi` = province_id,
        Region = region,
        `Suhu Saat Ini (°C)` = current_temp,
        `Rata-rata Historis (°C)` = historical_avg,
        `Anomali Suhu (°C)` = temp_anomaly,
        `Skor Risiko Iklim` = climate_risk_score,
        `Tahun Data` = data_years,
        `Tahun Pertama` = first_year,
        `Tahun Terakhir` = last_year
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(4:7), digits = 2) %>%
      DT::formatStyle(
        columns = c(4:7),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
  
  observeEvent(input$perbarui_peta, {
    output$peta_iklim <- renderLeaflet({
      
      cat("Memperbarui peta dengan parameter:", input$parameter_peta, "\n")
      
      if(is.null(indonesia_provinces)) {
        if(exists("fallback_data") && nrow(fallback_data) > 0) {
          
          param <- input$parameter_peta
          if(is.null(param)) param <- "current_temp"
          
          if(param == "current_temp") {
            values <- fallback_data$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- fallback_data$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- fallback_data$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- fallback_data$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          leaflet(fallback_data) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(
              lng = ~lng, lat = ~lat,
              radius = ~sqrt(values) * 3,
              fillColor = ~colors(values),
              color = "white",
              weight = 2,
              opacity = 1,
              fillOpacity = 0.8,
              popup = ~paste0("<b>", province_name, "</b><br/>", 
                              "Parameter: ", legend_title, "<br/>",
                              "Nilai: ", values, "<br/>",
                              "Region: ", region)
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia (Mode Titik) - Diperbarui", position = "topright")
          
        } else {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl("Data tidak tersedia", position = "topright")
        }
      } else {
        param <- input$parameter_peta
        if(is.null(param)) param <- "current_temp"
        
        if(!param %in% names(indonesia_provinces)) {
          leaflet() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addControl(paste("Parameter", param, "tidak tersedia"), position = "topright")
        } else {
          
          if(param == "current_temp") {
            values <- indonesia_provinces$current_temp
            colors <- colorNumeric("Reds", values, na.color = "#808080")
            legend_title <- "Suhu Saat Ini (°C)"
          } else if(param == "historical_avg") {
            values <- indonesia_provinces$historical_avg
            colors <- colorNumeric("Blues", values, na.color = "#808080")
            legend_title <- "Rata-rata Historis (°C)"
          } else if(param == "temp_anomaly") {
            values <- indonesia_provinces$temp_anomaly
            colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
            legend_title <- "Anomali Suhu (°C)"
          } else {
            values <- indonesia_provinces$climate_risk_score
            colors <- colorNumeric("OrRd", values, na.color = "#808080")
            legend_title <- "Skor Risiko Iklim"
          }
          
          labels <- sprintf(
            "<strong>%s</strong><br/>
          %s: %s<br/>
          Region: %s",
            indonesia_provinces$province_name %||% "N/A",
            legend_title,
            values %||% "N/A",
            indonesia_provinces$region %||% "N/A"
          ) %>% lapply(htmltools::HTML)
          
          leaflet(indonesia_provinces) %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
              fillColor = ~colors(values),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              )
            ) %>%
            addLegend(
              pal = colors, 
              values = ~values,
              opacity = 0.7, 
              title = legend_title,
              position = "bottomright"
            ) %>%
            addControl("Peta Iklim Indonesia - Diperbarui", position = "topright")
        }
      }
    })
  })
  
  output$tabel_provinsi <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        Region = region,
        `Suhu Saat Ini` = current_temp,
        `Rata-rata Historis` = historical_avg,
        `Anomali` = temp_anomaly,
        `Skor Risiko` = climate_risk_score
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(3:6), digits = 1) %>%
      DT::formatStyle(
        columns = c(3:6),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
  
  # ARIMA forecast
  output$grafik_prediksi_arima <- renderPlotly({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(plotly_empty() %>% layout(title = "Data bulanan tidak cukup untuk prediksi ARIMA"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    # Fit ARIMA and forecast
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    historical_dates <- monthly_temp_trend$year_month
    last_date <- max(historical_dates, na.rm = TRUE)
    forecast_dates <- seq(from = last_date %m+% months(1), by = "1 month", length.out = forecast_months)
    
    # Plot
    plot_ly() %>%
      add_trace(
        x = historical_dates,
        y = as.numeric(ts_data),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#e63946', width = 2),
        name = 'Data Historis',
        hovertemplate = 'Tanggal: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>'
      ) %>%
      add_trace(
        x = forecast_dates,
        y = as.numeric(forecast_result$mean),
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#2a9d8f', width = 3),
        name = 'Prediksi ARIMA',
        hovertemplate = 'Tanggal: %{x}<br>Prediksi: %{y:.2f}°C<extra></extra>'
      ) %>%
      layout(
        title = paste("Prediksi Suhu Bulanan ARIMA -", forecast_months, "Bulan"),
        xaxis = list(title = "Tanggal"),
        yaxis = list(title = "Suhu (°C)"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$performa_model <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk perhitungan performa model"))
    }
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature, 
                  start = c(start_year, start_month), 
                  frequency = 12)
    arima_model <- auto.arima(ts_data)
    
    residuals_data <- residuals(arima_model)
    mae <- round(mean(abs(residuals_data), na.rm = TRUE), 4)
    rmse <- round(sqrt(mean(residuals_data^2, na.rm = TRUE)), 4)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Tipe Model: "), 
          paste0("ARIMA(", paste(arimaorder(arima_model), collapse = ","), ")")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Frekuensi Data: "), "Bulanan"
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("MAE: "), paste0(mae, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("RMSE: "), paste0(rmse, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("AIC: "), round(AIC(arima_model), 2)
      )
    )
  })
  
  output$ringkasan_prediksi <- renderUI({
    req(input$perbarui_prediksi)
    
    if(nrow(monthly_temp_trend) <= 12) {
      return(p("Data bulanan tidak cukup untuk ringkasan prediksi"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    final_forecast <- round(as.numeric(forecast_result$mean[forecast_months]), 2)
    current_temp <- round(tail(monthly_temp_trend$temperature, 1), 2)
    temp_change <- round(final_forecast - current_temp, 2)
    
    last_date <- max(monthly_temp_trend$year_month, na.rm = TRUE)
    forecast_end_date <- last_date %m+% months(forecast_months)
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Horizon Prediksi: "), paste(forecast_months, "bulan")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Periode Prediksi: "), 
          paste("Hingga", format(forecast_end_date, "%B %Y"))
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Saat Ini: "), paste0(current_temp, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Suhu Proyeksi: "), paste0(final_forecast, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Perubahan yang Diharapkan: "), 
          paste0(ifelse(temp_change >= 0, "+", ""), temp_change, "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Laju Perubahan Bulanan: "), 
          paste0(round(temp_change / forecast_months, 4), "°C/bulan")
      )
    )
  })
  
  output$tabel_data_lengkap <- DT::renderDataTable({
    if(nrow(provinces_climate_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data tidak tersedia")))
    }
    
    display_data <- provinces_climate_data %>%
      select(
        Provinsi = province_name,
        `ID Provinsi` = province_id,
        Region = region,
        `Suhu Saat Ini (°C)` = current_temp,
        `Rata-rata Historis (°C)` = historical_avg,
        `Anomali Suhu (°C)` = temp_anomaly,
        `Skor Risiko Iklim` = climate_risk_score,
        `Tahun Data` = data_years,
        `Tahun Pertama` = first_year,
        `Tahun Terakhir` = last_year
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          search = "Cari:",
          lengthMenu = "Tampilkan _MENU_ entri",
          info = "Menampilkan _START_ hingga _END_ dari _TOTAL_ entri",
          paginate = list(
            first = "Pertama",
            last = "Terakhir",
            `next` = "Selanjutnya",
            previous = "Sebelumnya"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatRound(columns = c(4:7), digits = 2) %>%
      DT::formatStyle(
        columns = c(4:7),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
}

# =============================================
# JALANKAN APLIKASI
# =============================================

shinyApp(ui = ui, server = server)
