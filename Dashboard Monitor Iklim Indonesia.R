# =============================================
# DASHBOARD MONITOR IKLIM INDONESIA - ENHANCED
# Dashboard Analisis Suhu Real-time Indonesia dengan Fitur Lanjutan
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
library(tidyr)
library(tibble)
library(zoo)
library(rmarkdown)
library(kableExtra)

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

# Enhanced province climate data
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

# National monthly temperature trend
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

# Per-province monthly data for advanced analysis
monthly_temp_trend_per_province <- temp_data %>%
  mutate(
    year_month = floor_date(date, "month"),
    year = year(date),
    month = month(date)
  ) %>%
  group_by(province_name, year_month, year, month) %>%
  summarise(
    temperature = round(mean(Tavg, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(!is.na(temperature)) %>%
  arrange(province_name, year_month) %>%
  group_by(province_name) %>%
  mutate(
    baseline_temp = mean(temperature[year <= 2000], na.rm = TRUE),
    anomaly = round(temperature - baseline_temp, 2)
  ) %>%
  ungroup()

cat("Monthly per-province data rows:", nrow(monthly_temp_trend_per_province), "\n")
cat("Provinces with monthly data:", length(unique(monthly_temp_trend_per_province$province_name)), "\n")

# National yearly trend
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

# Monthly seasonal patterns
monthly_data <- temp_data %>%
  group_by(month) %>%
  summarise(
    avg_temp = round(mean(Tavg, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_temp)) %>%
  arrange(month) %>%
  mutate(
    month_name = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", 
                   "Jul", "Ags", "Sep", "Okt", "Nov", "Des")[month],
    seasonal_anomaly = round(avg_temp - mean(avg_temp, na.rm = TRUE), 2)
  )

# GeoJSON processing
geojson_path <- "indonesia-prov.geojson"
indonesia_provinces <- NULL

cat("Mencoba memuat GeoJSON dari:", geojson_path, "\n")

if(file.exists(geojson_path)) {
  tryCatch({
    indonesia_provinces <- geojson_sf(geojson_path)
    cat("GeoJSON berhasil dimuat. Kolom:", names(indonesia_provinces), "\n")
    cat("Jumlah provinsi di GeoJSON:", nrow(indonesia_provinces), "\n")
    
    if(!is.null(indonesia_provinces)) {
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

# Fallback data for maps
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
# HELPER FUNCTIONS (MOVED OUTSIDE SERVER)
# =============================================

# Climate change velocity calculation
calculate_climate_velocity <- function(province_data) {
  tryCatch({
    if(nrow(province_data) < 24) return(NA)
    
    yearly_data <- province_data %>%
      group_by(year) %>%
      summarise(temperature = mean(temperature, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(temperature))
    
    if(nrow(yearly_data) < 5) return(NA)
    
    # Linear trend
    lm_model <- lm(temperature ~ year, data = yearly_data)
    velocity <- coef(lm_model)[2] * 10 # Change per decade
    
    return(round(velocity, 3))
  }, error = function(e) {
    return(NA)
  })
}

# Seasonal variability index
calculate_seasonal_variability <- function(province_data) {
  tryCatch({
    if(nrow(province_data) < 12) return(NA)
    
    seasonal_stats <- province_data %>%
      group_by(month) %>%
      summarise(avg_temp = mean(temperature, na.rm = TRUE), .groups = 'drop')
    
    if(nrow(seasonal_stats) < 12) return(NA)
    
    variability <- sd(seasonal_stats$avg_temp, na.rm = TRUE)
    return(round(variability, 2))
  }, error = function(e) {
    return(NA)
  })
}

# Temperature persistence calculation
calculate_temperature_persistence <- function(province_data) {
  tryCatch({
    if(nrow(province_data) < 36) return(NA)
    
    # Calculate autocorrelation at lag 1
    ts_data <- ts(province_data$temperature, frequency = 12)
    acf_result <- acf(ts_data, lag.max = 1, plot = FALSE)
    persistence <- acf_result$acf[2]
    
    return(round(persistence, 3))
  }, error = function(e) {
    return(NA)
  })
}

# =============================================
# USER INTERFACE - ENHANCED
# =============================================

ui <- fluidPage(
  theme = NULL,
  
  # Enhanced CSS Styling
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
      
      .metadata-table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 15px;
      }

      .metadata-table th,
      .metadata-table td {
        border: 1px solid #bbb; 
        padding: 8px;
        text-align: left;
        vertical-align: top;
      }

      .metadata-table th {
        background-color: #f2f2f2;
        font-weight: bold;
      }
      
      .team-container {
        display: flex; 
        flex-wrap: wrap; 
        justify-content: center;
        gap: 40px; 
        padding: 20px 0;
      }
      
      .team-member { 
        text-align: center; 
        width: 200px; 
      }
      
      .profile-pic {
        width: 150px; 
        height: 150px;
        border-radius: 50%;
        object-fit: cover;
        border: 4px solid #3498db;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
    
      .timeline-container {
        border-left: 4px solid #3498db;
        padding-left: 20px;
        margin: 40px 0; 
        position: relative;
        text-align: left; 
        width: 100%; 
        box-sizing: border-box; 
        max-height: 350px; 
        overflow-y: auto; 
      }

      .timeline-item {
        margin-bottom: 30px;
        position: relative;
      }

      .timeline-item::before {
        content: '';
        width: 14px;
        height: 14px;
        background-color: #3498db;
        border-radius: 50%;
        position: absolute;
        left: -31px;
        top: 5px;
      }

      .timeline-date {
        font-weight: bold;
        color: #2c3e50;
      }

      .timeline-desc {
        margin: 5px 0 0 0;
        color: #555;
      }
      
      .member-info h4 { 
        margin-top: 15px; 
        margin-bottom: 5px; 
        font-weight: 600; 
      }
      
      .member-info p { 
        margin: 0; 
        color: #555; 
        font-size: 14px; 
      }
      
      .section-two-columns {
        display: flex;
        flex-wrap: wrap; 
        justify-content: center; 
        gap: 40px; 
        margin-top: 40px;
        margin-bottom: 40px;
      }

      .column-video {
        flex: 0 0 560px; 
        max-width: 100%; 
        display: flex;
        flex-direction: column;
        align-items: center; 
        text-align: center;
      }

      .column-timeline {
        flex: 1; 
        min-width: 300px; 
        display: flex;
        flex-direction: column;
        align-items: center; 
        text-align: center;
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
      # System Status Info (appears on all tabs)
      div(class = "data-source-info",
          tags$strong("🔧 Status Sistem: "),
          textOutput("system_status", inline = TRUE),
          br(),
          tags$strong("📊 Informasi Sesi: "),
          textOutput("session_info", inline = TRUE)
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
                  ),
                  
                  h2(class = "section-title", "Penilaian Risiko Iklim"),
                  div(class = "content-grid",
                      div(class = "content-card",
                          h3("Visualisasi Risiko"),
                          plotlyOutput("grafik_penilaian_risiko", height = "400px")
                      ),
                      div(class = "content-card",
                          h3("Kecepatan Perubahan Iklim"),
                          plotlyOutput("grafik_kecepatan_perubahan", height = "400px")
                      )
                  )
              )
          ),
          
          # ENHANCED TREN TAB
          div(id = "tren_content", style = "display: none;",
              div(class = "tab-header",
                  h1(class = "tab-title", "📈 Tren Suhu Per Provinsi"),
                  p(class = "tab-description", "Analisis tren suhu per provinsi dari waktu ke waktu berdasarkan data historis. Pilih provinsi dan lacak jejak perubahan suhu spesifik wilayah.")
              ),
              
              div(class = "tab-content",
                  div(class = "control-panel",
                      div(class = "control-row",
                          div(class = "form-group",
                              tags$label("Pilih Provinsi"),
                              selectInput("provinsi_tren", NULL,
                                          choices = NULL,
                                          selected = NULL)
                          ),
                          div(class = "form-group",
                              tags$label("Tampilkan Tren"),
                              selectInput("jenis_tren", NULL,
                                          choices = c("Tren Tahunan" = "yearly",
                                                      "Tren Bulanan" = "monthly",
                                                      "Perbandingan Multi-Tahun" = "multi_year"),
                                          selected = "yearly")
                          ),
                          div(class = "form-group",
                              tags$label("Perbarui Grafik"),
                              actionButton("perbarui_tren", "Perbarui Analisis Tren",
                                           class = "btn btn-primary",
                                           style = "width: 100%; margin-top: 8px;")
                          )
                      ),
                      
                      # Province comparison controls
                      div(class = "control-row", style = "margin-top: 25px; border-top: 2px solid #e63946; padding-top: 25px;",
                          div(style = "grid-column: 1 / -1; margin-bottom: 20px;",
                              h4("📊 Perbandingan Antar Provinsi", style = "color: #e63946; margin: 0; font-size: 18px; font-weight: 700;"),
                              p("Pilih 3 provinsi untuk membandingkan tren suhu secara bersamaan", style = "color: #666; margin: 5px 0 0 0; font-size: 14px;")
                          ),
                          div(class = "form-group",
                              tags$label("Provinsi Pertama", style = "color: #e63946; font-weight: 600;"),
                              selectInput("provinsi_compare_1", NULL,
                                          choices = NULL,
                                          selected = NULL)
                          ),
                          div(class = "form-group",
                              tags$label("Provinsi Kedua", style = "color: #2a9d8f; font-weight: 600;"),
                              selectInput("provinsi_compare_2", NULL,
                                          choices = NULL,
                                          selected = NULL)
                          ),
                          div(class = "form-group",
                              tags$label("Provinsi Ketiga", style = "color: #f77f00; font-weight: 600;"),
                              selectInput("provinsi_compare_3", NULL,
                                          choices = NULL,
                                          selected = NULL)
                          )
                      )
                  ),
                  
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
                  ),
                  
                  # Province comparison chart
                  div(style = "margin-top: 30px;",
                      div(class = "chart-container",
                          h3("Perbandingan dengan Provinsi Lain", style = "margin-bottom: 20px;"),
                          plotlyOutput("grafik_perbandingan_provinsi", height = "400px")
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
              
              div(class = "form-group",
                  tags$label("Pilih Tahun"),
                  selectInput("tahun_pilihan_dropdown", NULL,
                              choices = sort(unique(temp_data$year), decreasing = TRUE),
                              selected = max(temp_data$year)
                  )
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
              )
          ),
          
          # ENHANCED PREDIKSI TAB
          div(id = "prediksi_content", style = "display: none;",
              
              div(class = "tab-header",
                  h1(class = "tab-title", "🔮 Prediksi Suhu Per Provinsi"),
                  p(class = "tab-description", "Prediksi suhu bulanan berbasis ARIMA per provinsi menggunakan data time series. Pilih provinsi dan lihat prediksi suhu yang akurat.")
              ),
              
              div(class = "tab-content",
                  div(class = "control-panel",
                      div(class = "control-row",
                          div(class = "form-group",
                              tags$label("Pilih Provinsi"),
                              selectInput("provinsi_prediksi", NULL,
                                          choices = NULL,
                                          selected = NULL)
                          ),
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
                  p(class = "tab-description", "Informasi tentang sumber data, metodologi, dan tim pengembang.")
              ),
              
              div(class = "tab-content",
                  # Data Source Info (moved to Tentang tab)
                  div(class = "data-source-info",
                      tags$strong("📊 Sumber Data: "),
                      "Data suhu bersumber dari data BMKG dimuat dari data_suhu_lengkap.xlsx ",
                      textOutput("data_info", inline = TRUE)
                  ),
                  
                  div(class = "content-grid",
                      div(class = "content-card",
                          h3("Tentang Dashboard"),
                          p("Dashboard ini menggunakan data BMKG dan pengolahan data dilakukan dengan ARIMA"),
                          tags$ul(
                            tags$li("Versi: 1.0.0"),
                            tags$li("Tanggal Rilis: Juli 2025"),
                            tags$li("Lisensi: MIT License")
                          )
                      ),
                      div(class = "content-card",
                          h3("Detail Teknis"),
                          p("Dibangun menggunakan R Shiny dengan fitur-fitur utama:"),
                          tags$ul(
                            tags$li("Visualisasi suhu interaktif per provinsi"),
                            tags$li("Pemetaan geospasial dengan Leaflet"),
                            tags$li("Prediksi ARIMA per provinsi"),
                            tags$li("Analisis tren temporal lanjutan")
                          )
                      )
                  ),
                  
                  div(class = "content-card",
                      h3("Metadata"),
                      p("Metadata berisikan detail variabel yang digunakan dalam pengolahan data. Data suhu bersumber dari data BMKG dimuat dari data_suhu_lengkap.xlsx"),
                      tags$table(class = "metadata-table",
                                 tags$thead(
                                   tags$tr(
                                     tags$th("Variabel"),
                                     tags$th("Definisi"),
                                     tags$th("Satuan/Format"),
                                     tags$th("Cakupan Spasial"),
                                     tags$th("Cakupan Temporal"),
                                     tags$th("Tipe Data")
                                   )
                                 ),
                                 tags$tbody(
                                   tags$tr(
                                     tags$td("Tavg"),
                                     tags$td("Suhu rata-rata harian dihitung dari [(Suhu Maksimum + Suhu Minimum) / 2]"),
                                     tags$td("Celsius (°C)"),
                                     tags$td("34 provinsi di Indonesia"),
                                     tags$td("Harian"),
                                     tags$td("double")
                                   ),
                                   tags$tr(
                                     tags$td("Date"),
                                     tags$td("Titik data time series"),
                                     tags$td("YYYY-MM-DD"),
                                     tags$td("-"),
                                     tags$td("01 Jan 2010 - 31 Des 2020"),
                                     tags$td("Date")
                                   ),
                                   tags$tr(
                                     tags$td("province_name"),
                                     tags$td("Nama provinsi"),
                                     tags$td("-"),
                                     tags$td("34 provinsi di Indonesia"),
                                     tags$td("-"),
                                     tags$td("character/string")
                                   ),
                                   tags$tr(
                                     tags$td("province_id"),
                                     tags$td("ID provinsi atau kode wilayah provinsi, didapatkan dari Badan Pusat Statistik (BPS) dan sistem Kode Wilayah Administrasi Pemerintahan dan Pulau."),
                                     tags$td("-"),
                                     tags$td("34 provinsi di Indonesia"),
                                     tags$td("-"),
                                     tags$td("integer/numeric")
                                   )
                                 )
                      )
                  ),
                  
                  hr(),
                  
                  div(class = "section-two-columns",
                      div(class = "column-video",
                          h3("📽️ Video Demo"),
                          div(style = "display: flex; justify-content: center;",
                              tags$iframe(
                                width = "560", height = "315",
                                src = "https://www.youtube.com/embed/I9VVNqa_gpQ?si=XNyMOgVPP2S6kiHi",
                                frameborder = "0",
                                allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                                allowfullscreen = NA
                              )
                          )
                      ),
                      
                      div(class = "column-timeline",
                          h3("🕒 Linimasa Kegiatan"), 
                          div(class = "timeline-container",
                              
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "26 April 2025"),
                                  p(class = "timeline-desc", "menentukan tema, melakukan perancangan kegiatan, dan menyusun laporan proposal.")
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke-7 Perkuliahan"),
                                  p(class = "timeline-desc", "Collecting Data")
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke-8 Perkuliahan"),
                                  p(class = "timeline-desc", "Pre-processing Data")
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke-9 Perkuliahan"),
                                  p(class = "timeline-desc", "Processing Data")
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke-10 Perkuliahan"),
                                  p(class = "timeline-desc", "Analisis Data")
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke-11 Perkuliahan"),
                                  p(class = "timeline-desc", "Visualisasi Data")
                              ),
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke-12 Perkuliahan"),
                                  p(class = "timeline-desc", "Pembuatan Dashboard")
                              ),
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke- 13 Perkuliahan"),
                                  p(class = "timeline-desc", "Peluncuran Awal Dashboard")
                              ),
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "Minggu ke-14 Perkuliahan"),
                                  p(class = "timeline-desc", "Pembuatan Video Tutorial dan Finalisasi Dashboard")
                              ),
                              div(class = "timeline-item",
                                  span(class = "timeline-date", "15 Juli 2025"),
                                  p(class = "timeline-desc", "Dokumentasi Publik, Evaluasi Dashboard, dan Peluncuran Resmi Dashboard versi 1.0")
                              )
                          )
                      )
                  ),
                  
                  h2(class = "section-title", "Dataset Lengkap"),
                  div(class = "chart-container",
                      DT::dataTableOutput("tabel_data_lengkap")
                  ),
                  
                  h2(class = "section-title", "Analisis Metrik Iklim Lanjutan"),
                  div(class = "chart-container",
                      DT::dataTableOutput("tabel_metrik_iklim")
                  ),
                  
                  h2(class = "section-title", "Kejadian Cuaca Ekstrem"),
                  div(class = "chart-container",
                      DT::dataTableOutput("tabel_kejadian_ekstrem")
                  ),
                  
                  h2(class = "section-title", "Penilaian Risiko Iklim"),
                  div(class = "chart-container",
                      DT::dataTableOutput("tabel_penilaian_risiko")
                  ),
                  
                  h2(class = "section-title", "Generate Laporan PDF"),
                  div(class = "content-grid",
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-file-pdf"), " Laporan Analisis Provinsi"),
                          p("Generate laporan komprehensif untuk analisis tren suhu per provinsi"),
                          div(class = "form-group",
                              tags$label("Pilih Provinsi untuk Laporan"),
                              selectInput("provinsi_laporan", NULL,
                                          choices = NULL,
                                          selected = NULL)
                          ),
                          div(class = "form-group",
                              tags$label("Periode Analisis"),
                              selectInput("periode_laporan", NULL,
                                          choices = c("Lengkap (Semua Data)" = "full",
                                                      "5 Tahun Terakhir" = "5_years",
                                                      "10 Tahun Terakhir" = "10_years"),
                                          selected = "full")
                          ),
                          downloadButton("download_laporan_provinsi", "Download Laporan Provinsi (PDF)", 
                                         class = "btn btn-primary",
                                         style = "width: 100%; margin-top: 15px;")
                      ),
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-chart-line"), " Laporan Perbandingan Regional"),
                          p("Generate laporan perbandingan suhu antar region di Indonesia"),
                          div(class = "form-group",
                              tags$label("Pilih Region untuk Dibandingkan"),
                              checkboxGroupInput("regions_laporan", NULL,
                                                 choices = c("Jawa-Bali", "Sumatera", "Kalimantan", 
                                                             "Sulawesi", "Papua", "Maluku-Nusa Tenggara"),
                                                 selected = c("Jawa-Bali", "Sumatera", "Kalimantan"))
                          ),
                          downloadButton("download_laporan_regional", "Download Laporan Regional (PDF)", 
                                         class = "btn btn-primary",
                                         style = "width: 100%; margin-top: 15px;")
                      )
                  ),
                  div(class = "content-grid",
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-exclamation-triangle"), " Laporan Risiko Iklim"),
                          p("Generate laporan penilaian risiko iklim dan kejadian cuaca ekstrem"),
                          div(class = "form-group",
                              tags$label("Kategori Risiko"),
                              selectInput("kategori_risiko_laporan", NULL,
                                          choices = c("Semua Kategori" = "all",
                                                      "Risiko Tinggi" = "high",
                                                      "Risiko Sedang" = "medium",
                                                      "Risiko Rendah" = "low"),
                                          selected = "all")
                          ),
                          div(class = "form-group",
                              tags$label("Jumlah Provinsi Teratas"),
                              selectInput("top_provinces_laporan", NULL,
                                          choices = c("Top 10" = "10",
                                                      "Top 15" = "15", 
                                                      "Top 20" = "20",
                                                      "Semua" = "all"),
                                          selected = "15")
                          ),
                          downloadButton("download_laporan_risiko", "Download Laporan Risiko (PDF)", 
                                         class = "btn btn-primary",
                                         style = "width: 100%; margin-top: 15px;")
                      ),
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-globe"), " Laporan Nasional Komprehensif"),
                          p("Generate laporan lengkap mencakup semua analisis untuk seluruh Indonesia"),
                          div(class = "form-group",
                              tags$label("Tingkat Detail"),
                              radioButtons("detail_level_laporan", NULL,
                                           choices = c("Ringkasan Eksekutif" = "summary",
                                                       "Laporan Standar" = "standard", 
                                                       "Laporan Lengkap" = "detailed"),
                                           selected = "standard")
                          ),
                          div(class = "form-group",
                              tags$label("Include Prediksi ARIMA"),
                              checkboxInput("include_forecast", "Sertakan analisis prediksi", value = TRUE)
                          ),
                          downloadButton("download_laporan_nasional", "Download Laporan Nasional (PDF)", 
                                         class = "btn btn-primary",
                                         style = "width: 100%; margin-top: 15px;")
                      )
                  ),
                  
                  h2(class = "section-title", "Export Data"),
                  div(class = "content-grid",
                      div(class = "content-card",
                          h3("Download Data Iklim"),
                          p("Export data iklim provinsi dalam format CSV"),
                          downloadButton("download_climate_data", "Download Data Iklim", 
                                         class = "btn btn-primary")
                      ),
                      div(class = "content-card",
                          h3("Download Data Bulanan"),
                          p("Export data time series bulanan per provinsi"),
                          downloadButton("download_monthly_data", "Download Data Bulanan", 
                                         class = "btn btn-primary")
                      )
                  ),
                  div(class = "content-card",
                      h3("Download Penilaian Risiko"),
                      p("Export analisis risiko iklim komprehensif"),
                      downloadButton("download_risk_assessment", "Download Assessment Risiko", 
                                     class = "btn btn-primary")
                  ),
                  
                  hr(),
                  h2("Tim Pengembang", style="text-align:center; margin-top:40px;"),
                  div(class = "team-container",
                      
                      div(class = "team-member",
                          tags$img(src = "evelyn.jpg", class = "profile-pic"), 
                          div(class = "member-info",
                              h4("Evelyn Tan Eldisha Nawa"),
                              p("222313067"),
                              p(" mencari dan mengunduh data suhu global dari sumber resmi seperti BMKG, kemudian membersihkan data tersebut dari missing values, outlier, serta duplikasi dan menstandardisasi formatnya, lalu menambahkan tab peta dan tab tentang untuk antarmuka pengguna dashboard ini.")
                          )
                      ),
                      
                      div(class = "team-member",
                          tags$img(src = "farhan.jpg", class = "profile-pic"),
                          div(class = "member-info",
                              h4("Farhan Kadhafi Azuansyah"),
                              p("222313079"),
                              p("mendesain dan membangun antarmuka dashboard interaktif menggunakan R Shiny, lalu mengimplementasikan visualisasi suhu global, tren suhu, dan peta interaktif, serta menambahkan fitur interaktif seperti filter waktu dan wilayah, disusul dengan pengujian dan debugging dashboard secara menyeluruh.")
                          )
                      ),
                      
                      div(class = "team-member",
                          tags$img(src = "naufal.jpg", class = "profile-pic"),
                          div(class = "member-info",
                              h4("Naufal Dzaki Zaidan"),
                              p("222313290"),
                              p("melakukan analisis statistik untuk menghitung suhu tertinggi/terendah, distribusi, dan anomali suhu, kemudian mengembangkan model prediksi suhu global dan menganalisis tren serta deviasi suhu dari rata-rata abad ke-20, dan terakhir menulis interpretasi hasil analisis untuk menyediakan data bagi visualisasi di dashboard.")
                          )
                      )
                  )
              )
          )
      )
  )
)

# =============================================
# SERVER LOGIC - ENHANCED
# =============================================

server <- function(input, output, session) {
  
  active_tab <- reactiveVal("beranda")
  
  # Navigation logic
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
  
  # =============================================
  # REACTIVE FUNCTIONS MOVED INSIDE SERVER
  # =============================================
  
  # Enhanced climate metrics calculation
  calculate_enhanced_climate_metrics <- reactive({
    req(nrow(monthly_temp_trend_per_province) > 0)
    
    enhanced_metrics <- monthly_temp_trend_per_province %>%
      group_by(province_name) %>%
      do({
        data <- .
        tibble(
          province_name = unique(data$province_name),
          climate_velocity = calculate_climate_velocity(data),
          seasonal_variability = calculate_seasonal_variability(data),
          temperature_persistence = calculate_temperature_persistence(data),
          data_completeness = round((nrow(data) / max(nrow(data), 132)) * 100, 1),
          trend_significance = {
            if(nrow(data) >= 24) {
              yearly_data <- data %>%
                group_by(year) %>%
                summarise(temperature = mean(temperature, na.rm = TRUE), .groups = 'drop')
              
              if(nrow(yearly_data) >= 5) {
                lm_model <- lm(temperature ~ year, data = yearly_data)
                p_value <- summary(lm_model)$coefficients[2, 4]
                ifelse(p_value < 0.05, "Signifikan", "Tidak Signifikan")
              } else {
                "Data Tidak Cukup"
              }
            } else {
              "Data Tidak Cukup"
            }
          }
        )
      }) %>%
      ungroup()
    
    return(enhanced_metrics)
  })
  
  # Identify extreme weather events
  identify_extreme_events <- reactive({
    req(nrow(monthly_temp_trend_per_province) > 0)
    
    extreme_events <- monthly_temp_trend_per_province %>%
      group_by(province_name) %>%
      mutate(
        temp_mean = mean(temperature, na.rm = TRUE),
        temp_sd = sd(temperature, na.rm = TRUE),
        z_score = (temperature - temp_mean) / temp_sd,
        is_extreme = abs(z_score) > 2,
        event_type = case_when(
          z_score > 2 ~ "Gelombang Panas",
          z_score < -2 ~ "Periode Dingin Ekstrem",
          TRUE ~ "Normal"
        )
      ) %>%
      filter(is_extreme) %>%
      arrange(desc(abs(z_score))) %>%
      select(province_name, year_month, year, month, temperature, temp_mean, z_score, event_type) %>%
      ungroup()
    
    return(extreme_events)
  })
  
  # Enhanced climate risk calculation
  calculate_climate_risk <- reactive({
    req(nrow(provinces_climate_data) > 0)
    
    enhanced_metrics <- calculate_enhanced_climate_metrics()
    extreme_events <- identify_extreme_events()
    
    # Count extreme events per province
    extreme_counts <- extreme_events %>%
      group_by(province_name) %>%
      summarise(
        total_extreme_events = n(),
        heat_waves = sum(event_type == "Gelombang Panas"),
        cold_periods = sum(event_type == "Periode Dingin Ekstrem"),
        .groups = 'drop'
      )
    
    # Combine all risk factors
    risk_assessment <- provinces_climate_data %>%
      left_join(enhanced_metrics, by = "province_name") %>%
      left_join(extreme_counts, by = "province_name") %>%
      replace_na(list(total_extreme_events = 0, heat_waves = 0, cold_periods = 0)) %>%
      mutate(
        # Normalize risk factors (0-10 scale)
        temp_anomaly_risk = pmin(10, pmax(0, (abs(temp_anomaly) / 2) * 10)),
        velocity_risk = pmin(10, pmax(0, (abs(coalesce(climate_velocity, 0)) / 0.5) * 10)),
        variability_risk = pmin(10, pmax(0, (coalesce(seasonal_variability, 0) / 5) * 10)),
        extreme_risk = pmin(10, (total_extreme_events / 10) * 10),
        
        # Calculate composite risk score
        composite_risk = round((temp_anomaly_risk + 
                                  velocity_risk + 
                                  variability_risk + 
                                  extreme_risk) / 4, 1),
        
        # Risk categories
        risk_category = case_when(
          composite_risk >= 7 ~ "Risiko Tinggi",
          composite_risk >= 4 ~ "Risiko Sedang",
          composite_risk >= 2 ~ "Risiko Rendah",
          TRUE ~ "Risiko Minimal"
        ),
        
        # Risk color coding
        risk_color = case_when(
          composite_risk >= 7 ~ "#d62b39",
          composite_risk >= 4 ~ "#f77f00",
          composite_risk >= 2 ~ "#4895ef",
          TRUE ~ "#2a9d8f"
        )
      ) %>%
      arrange(desc(composite_risk))
    
    return(risk_assessment)
  })
  
  # =============================================
  # OUTPUT FUNCTIONS
  # =============================================
  
  # =============================================
  # OUTPUT FUNCTIONS
  # =============================================
  
  # Data quality assessment reactive function
  assess_data_quality <- reactive({
    quality_metrics <- list(
      total_provinces = length(unique(temp_data$province_name)),
      provinces_with_climate_data = nrow(provinces_climate_data),
      provinces_with_monthly_data = length(unique(monthly_temp_trend_per_province$province_name)),
      data_completeness = round((nrow(provinces_climate_data) / length(unique(temp_data$province_name))) * 100, 1),
      date_range = paste(min(temp_data$date), "to", max(temp_data$date)),
      missing_values = sum(is.na(temp_data$Tavg)),
      total_records = nrow(temp_data)
    )
    
    return(quality_metrics)
  })
  
  # Data info (for Tentang tab only)
  output$data_info <- renderText({
    paste("| Memuat", nrow(temp_data), "rekord suhu dari",
          length(unique(temp_data$province_name)), "provinsi |",
          "Rentang tanggal:", min(temp_data$date), "hingga", max(temp_data$date))
  })
  
  # System status indicator (appears on all tabs)
  output$system_status <- renderText({
    tryCatch({
      quality <- assess_data_quality()
      status <- if(quality$data_completeness > 80) "🟢 OPTIMAL" else if(quality$data_completeness > 60) "🟡 BAIK" else "🔴 PERLU PERHATIAN"
      paste("Status Sistem:", status, "| Kelengkapan Data:", quality$data_completeness, "%")
    }, error = function(e) {
      "🔴 ERROR LOADING STATUS"
    })
  })
  
  # Session information for all tabs
  output$session_info <- renderText({
    tryCatch({
      session_data <- list(
        "Waktu Sesi" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        "Data Provinsi" = nrow(provinces_climate_data),
        "Data Bulanan" = nrow(monthly_temp_trend_per_province),
        "Tab Aktif" = active_tab()
      )
      
      paste(names(session_data), session_data, sep = ": ", collapse = " | ")
    }, error = function(e) {
      paste("ERROR:", e$message)
    })
  })
  
  # Enhanced statistics outputs
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
  
  # Enhanced preview map
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
  
  # Enhanced graphics with improved colors
  output$grafik_tren_nasional <- renderPlotly({
    if(nrow(national_temp_trend) == 0) {
      return(plotly_empty() %>% layout(title = "Data tren tidak tersedia"))
    }
    
    plot_ly(national_temp_trend, x = ~year, y = ~temperature, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#fa0000', width = 3),
            marker = list(color = '#730d0d', size = 6),
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
  
  # Enhanced seasonal patterns
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
            line = list(color = '#03fcde', width = 3),
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
  
  # Enhanced distribution histogram
  output$grafik_distribusi_suhu <- renderPlotly({
    if(nrow(provinces_climate_data) == 0 || !"current_temp" %in% names(provinces_climate_data)) {
      return(plotly_empty() %>% layout(title = "Data distribusi tidak tersedia"))
    }
    
    temp_values <- provinces_climate_data$current_temp[!is.na(provinces_climate_data$current_temp)]
    
    if(length(temp_values) == 0) {
      return(plotly_empty() %>% layout(title = "Data suhu tidak tersedia"))
    }
    
    plot_ly(x = temp_values, type = "histogram", nbinsx = 20,
            marker = list(color = '#e63946', opacity = 0.8,
                          line = list(color = '#730d0d', width = 1.5)),
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
  
  # =============================================
  # ENHANCED PROVINCE TREND ANALYSIS
  # =============================================
  
  # Populate province choices for trend analysis
  observe({
    province_choices <- sort(unique(monthly_temp_trend_per_province$province_name))
    updateSelectInput(session, "provinsi_tren",
                      choices = setNames(province_choices, province_choices),
                      selected = if(length(province_choices) > 0) province_choices[1] else NULL)
    
    # Update comparison province selectors
    updateSelectInput(session, "provinsi_compare_1",
                      choices = setNames(province_choices, province_choices),
                      selected = if(length(province_choices) >= 1) province_choices[1] else NULL)
    
    updateSelectInput(session, "provinsi_compare_2",
                      choices = setNames(province_choices, province_choices),
                      selected = if(length(province_choices) >= 2) province_choices[2] else NULL)
    
    updateSelectInput(session, "provinsi_compare_3",
                      choices = setNames(province_choices, province_choices),
                      selected = if(length(province_choices) >= 3) province_choices[3] else NULL)
  })
  
  # Enhanced trend analysis per province
  output$grafik_tren_detail <- renderPlotly({
    req(input$provinsi_tren)
    
    province_data <- monthly_temp_trend_per_province %>%
      filter(province_name == input$provinsi_tren) %>%
      arrange(year_month)
    
    if(nrow(province_data) < 2) {
      return(plotly_empty() %>% layout(title = paste("Data tidak cukup untuk analisis tren -", input$provinsi_tren)))
    }
    
    jenis_tren <- if(is.null(input$jenis_tren)) "yearly" else input$jenis_tren
    
    if(jenis_tren == "yearly") {
      yearly_data <- province_data %>%
        group_by(year) %>%
        summarise(
          temperature = round(mean(temperature, na.rm = TRUE), 2),
          .groups = 'drop'
        ) %>%
        filter(!is.na(temperature))
      
      if(nrow(yearly_data) < 2) {
        return(plotly_empty() %>% layout(title = "Data tahunan tidak cukup"))
      }
      
      lm_model <- lm(temperature ~ year, data = yearly_data)
      trend_line <- predict(lm_model, newdata = yearly_data)
      
      plot_ly(yearly_data, x = ~year, y = ~temperature,
              type = 'scatter', mode = 'lines+markers',
              line = list(color = '#e63946', width = 3),
              marker = list(color = '#730d0d', size = 8),
              name = 'Suhu Tahunan',
              hovertemplate = 'Tahun: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>') %>%
        add_trace(x = yearly_data$year, y = trend_line,
                  type = 'scatter', mode = 'lines',
                  line = list(color = '#2a9d8f', width = 2, dash = 'dash'),
                  name = 'Tren Linear',
                  hovertemplate = 'Tren: %{y:.2f}°C<extra></extra>') %>%
        layout(
          title = paste("Tren Suhu Tahunan -", input$provinsi_tren),
          xaxis = list(title = "Tahun"),
          yaxis = list(title = "Suhu (°C)"),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%
        config(displayModeBar = FALSE)
      
    } else if(jenis_tren == "monthly") {
      plot_ly(province_data, x = ~year_month, y = ~temperature,
              type = 'scatter', mode = 'lines',
              line = list(color = '#e63946', width = 2),
              name = 'Suhu Bulanan',
              hovertemplate = 'Bulan: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>') %>%
        layout(
          title = paste("Tren Suhu Bulanan -", input$provinsi_tren),
          xaxis = list(title = "Tahun"),
          yaxis = list(title = "Suhu (°C)"),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%
        config(displayModeBar = FALSE)
      
    } else {
      yearly_comparison <- province_data %>%
        mutate(year_factor = as.factor(year))
      
      plot_ly(yearly_comparison, x = ~year_factor, y = ~temperature,
              type = 'box',
              name = 'Distribusi Suhu',
              marker = list(color = '#e63946'),
              hovertemplate = 'Tahun: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>') %>%
        layout(
          title = paste("Perbandingan Multi-Tahun -", input$provinsi_tren),
          xaxis = list(title = "Tahun"),
          yaxis = list(title = "Suhu (°C)"),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Enhanced province comparison chart
  output$grafik_perbandingan_provinsi <- renderPlotly({
    req(input$provinsi_compare_1, input$provinsi_compare_2, input$provinsi_compare_3)
    
    selected_provinces <- unique(c(input$provinsi_compare_1, input$provinsi_compare_2, input$provinsi_compare_3))
    
    comparison_data <- monthly_temp_trend_per_province %>%
      filter(province_name %in% selected_provinces) %>%
      group_by(province_name, year) %>%
      summarise(
        temperature = round(mean(temperature, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      filter(!is.na(temperature))
    
    if(nrow(comparison_data) == 0) {
      return(plotly_empty() %>% layout(title = "Data tidak tersedia untuk provinsi yang dipilih"))
    }
    
    province_colors <- c('#e63946', '#2a9d8f', '#f77f00')
    names(province_colors) <- selected_provinces[1:min(length(selected_provinces), 3)]
    
    p <- plot_ly()
    
    for(i in seq_along(selected_provinces)) {
      if(i <= length(province_colors)) {
        province_data <- comparison_data %>% filter(province_name == selected_provinces[i])
        
        if(nrow(province_data) > 0) {
          p <- p %>% add_trace(
            data = province_data,
            x = ~year, 
            y = ~temperature,
            type = 'scatter', 
            mode = 'lines+markers',
            name = selected_provinces[i],
            line = list(width = 3, color = province_colors[selected_provinces[i]]),
            marker = list(size = 6, color = province_colors[selected_provinces[i]]),
            hovertemplate = paste0(selected_provinces[i], '<br>Tahun: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>')
          )
        }
      }
    }
    
    p %>% layout(
      title = "Perbandingan Tren Suhu - 3 Provinsi Pilihan",
      xaxis = list(title = "Tahun"),
      yaxis = list(title = "Suhu (°C)"),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      legend = list(
        orientation = "h", 
        x = 0, 
        y = -0.2,
        bgcolor = 'rgba(255,255,255,0.8)',
        bordercolor = 'rgba(0,0,0,0.2)',
        borderwidth = 1
      )
    ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Enhanced trend statistics per province
  output$statistik_tren <- renderUI({
    req(input$provinsi_tren)
    
    province_data <- monthly_temp_trend_per_province %>%
      filter(province_name == input$provinsi_tren) %>%
      arrange(year_month)
    
    if(nrow(province_data) < 2) {
      return(HTML("<p>Data tren tidak tersedia untuk provinsi ini</p>"))
    }
    
    yearly_data <- province_data %>%
      group_by(year) %>%
      summarise(
        temperature = round(mean(temperature, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      filter(!is.na(temperature))
    
    if(nrow(yearly_data) >= 2) {
      lm_model <- lm(temperature ~ year, data = yearly_data)
      warming_rate <- round(coef(lm_model)[2], 4)
      r_squared <- round(summary(lm_model)$r.squared, 3)
      
      temp_range <- max(province_data$temperature, na.rm = TRUE) - min(province_data$temperature, na.rm = TRUE)
      avg_temp <- mean(province_data$temperature, na.rm = TRUE)
      total_change <- warming_rate * (max(yearly_data$year) - min(yearly_data$year))
      
      html_content <- paste0(
        '<div style="margin-bottom: 15px;"><strong>Provinsi:</strong> ', input$provinsi_tren, '</div>',
        '<div style="margin-bottom: 15px;"><strong>Periode Data:</strong> ', 
        min(yearly_data$year), ' - ', max(yearly_data$year), '</div>',
        '<div style="margin-bottom: 15px;"><strong>Rata-rata Suhu:</strong> ', 
        round(avg_temp, 2), '°C</div>',
        '<div style="margin-bottom: 15px;"><strong>Laju Perubahan:</strong> ', 
        ifelse(warming_rate >= 0, "+", ""), warming_rate, '°C/tahun</div>',
        '<div style="margin-bottom: 15px;"><strong>Kekuatan Hubungan (R²):</strong> ', r_squared, '</div>',
        '<div style="margin-bottom: 15px;"><strong>Total Perubahan:</strong> ', 
        ifelse(total_change >= 0, "+", ""), round(total_change, 2), '°C</div>',
        '<div style="margin-bottom: 15px;"><strong>Rentang Variasi:</strong> ', 
        round(temp_range, 2), '°C</div>'
      )
      
      return(HTML(html_content))
    } else {
      return(HTML("<p>Data tidak cukup untuk analisis statistik</p>"))
    }
  })
  
  # Enhanced temperature extremes per province
  output$ekstrem_suhu <- renderUI({
    req(input$provinsi_tren)
    
    province_data <- monthly_temp_trend_per_province %>%
      filter(province_name == input$provinsi_tren)
    
    if(nrow(province_data) == 0) {
      return(HTML("<p>Data ekstrem tidak tersedia untuk provinsi ini</p>"))
    }
    
    valid_temps <- province_data$temperature[!is.na(province_data$temperature)]
    valid_anomalies <- province_data$anomaly[!is.na(province_data$anomaly)]
    
    if(length(valid_temps) == 0) {
      return(HTML("<p>Data suhu tidak tersedia</p>"))
    }
    
    max_temp_row <- province_data[which.max(province_data$temperature), ]
    min_temp_row <- province_data[which.min(province_data$temperature), ]
    
    seasonal_pattern <- province_data %>%
      group_by(month) %>%
      summarise(avg_temp = mean(temperature, na.rm = TRUE), .groups = 'drop') %>%
      arrange(month)
    
    hottest_month <- seasonal_pattern$month[which.max(seasonal_pattern$avg_temp)]
    coldest_month <- seasonal_pattern$month[which.min(seasonal_pattern$avg_temp)]
    
    month_names <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni",
                     "Juli", "Agustus", "September", "Oktober", "November", "Desember")
    
    html_content <- paste0(
      '<div style="margin-bottom: 15px;"><strong>Provinsi:</strong> ', input$provinsi_tren, '</div>',
      '<div style="margin-bottom: 15px;"><strong>Suhu Tertinggi:</strong> ',
      max(valid_temps), '°C (', format(max_temp_row$year_month, "%B %Y"), ')</div>',
      '<div style="margin-bottom: 15px;"><strong>Suhu Terendah:</strong> ',
      min(valid_temps), '°C (', format(min_temp_row$year_month, "%B %Y"), ')</div>',
      '<div style="margin-bottom: 15px;"><strong>Rentang Suhu:</strong> ',
      round(max(valid_temps) - min(valid_temps), 1), '°C</div>',
      '<div style="margin-bottom: 15px;"><strong>Bulan Terpanas:</strong> ', month_names[hottest_month], '</div>',
      '<div style="margin-bottom: 15px;"><strong>Bulan Terdingin:</strong> ', month_names[coldest_month], '</div>'
    )
    
    if(length(valid_anomalies) > 0) {
      html_content <- paste0(html_content,
                             '<div style="margin-bottom: 15px;"><strong>Anomali Terbesar:</strong> ',
                             ifelse(max(valid_anomalies) >= 0, "+", ""), round(max(valid_anomalies), 2), '°C</div>'
      )
    }
    
    return(HTML(html_content))
  })
  
  # =============================================
  # ENHANCED MAP FUNCTIONALITY
  # =============================================
  
  map_data_event <- eventReactive(input$perbarui_peta, {
    selected_year <- as.numeric(input$tahun_pilihan_dropdown)
    cat("Tombol diklik! Menghitung data untuk tahun:", selected_year, "\n")
    
    climate_data_dinamis <- temp_data %>%
      group_by(province_name) %>%
      summarise(
        current_temp = round(mean(Tavg[year == selected_year], na.rm = TRUE), 1),
        historical_avg = round(mean(Tavg[year <= selected_year], na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      filter(!is.na(current_temp)) %>%
      mutate(
        temp_anomaly = round(current_temp - historical_avg, 1),
        climate_risk_score = round(pmin(10, pmax(1, (temp_anomaly * 2) + (current_temp - 25) * 0.5 + 3)), 1)
      )
    
    climate_data_dinamis$province_clean <- toupper(trimws(gsub("[^A-Za-z0-9 ]", "", climate_data_dinamis$province_name)))
    
    if(!is.null(indonesia_provinces)) {
      final_map_data <- indonesia_provinces %>%
        left_join(climate_data_dinamis, by = "province_clean")
      
      if (sum(!is.na(final_map_data$current_temp)) == 0) {
        cat("Merge otomatis gagal, mencoba mapping manual...\n")
        province_mapping <- data.frame(
          nama_provinsi = c("DKI JAKARTA", "JAWA BARAT", "JAWA TENGAH", "JAWA TIMUR", 
                            "SUMATERA UTARA", "SUMATERA BARAT", "KALIMANTAN TIMUR", "SULAWESI SELATAN",
                            "BANTEN", "DI YOGYAKARTA", "BALI"),
          stringsAsFactors = FALSE
        )
        
        for (prov_name in province_mapping$nama_provinsi) {
          map_idx <- which(final_map_data$province_clean == prov_name)
          climate_idx <- which(climate_data_dinamis$province_clean == prov_name)
          if (length(map_idx) > 0 && length(climate_idx) > 0) {
            climate_row <- climate_data_dinamis[climate_idx[1], ]
            cols_to_copy <- names(climate_row)
            for (col in cols_to_copy) {
              if (col %in% names(final_map_data)) {
                final_map_data[map_idx[1], col] <- climate_row[[col]]
              }
            }
          }
        }
        cat("Setelah mapping manual, provinsi dengan data:", sum(!is.na(final_map_data$current_temp)), "\n")
      }
      return(final_map_data)
    } else {
      return(climate_data_dinamis)
    }
  })
  
  output$peta_iklim <- renderLeaflet({
    map_data <- map_data_event()
    req(input$parameter_peta)
    param <- input$parameter_peta
    
    cat("Rendering peta dengan parameter:", param, "\n")
    
    if (!is.null(map_data) && param %in% names(map_data) && sum(!is.na(map_data[[param]])) > 0) {
      
      if (param == "current_temp") {
        values <- map_data$current_temp
        colors <- colorNumeric("Reds", values, na.color = "#808080")
        legend_title <- "Suhu Saat Ini (°C)"
      } else if (param == "historical_avg") {
        values <- map_data$historical_avg
        colors <- colorNumeric("Blues", values, na.color = "#808080")
        legend_title <- "Rata-rata Historis (°C)"
      } else if (param == "temp_anomaly") {
        values <- map_data$temp_anomaly
        colors <- colorNumeric("RdYlBu", values, na.color = "#808080", reverse = TRUE)
        legend_title <- "Anomali Suhu (°C)"
      } else { 
        values <- map_data$climate_risk_score
        colors <- colorNumeric("OrRd", values, na.color = "#808080")
        legend_title <- "Skor Risiko Iklim"
      }
      
      if("geometry" %in% names(map_data)) {
        # Polygon map
        labels <- sprintf(
          "<strong>%s</strong><br/>
           Suhu %s: %s°C<br/>
           Historis s/d %s: %s°C<br/>
           Anomali: %s°C<br/>
           Skor Risiko: %s/10",
          map_data$province_name,
          input$tahun_pilihan_dropdown, map_data$current_temp %||% "N/A",
          input$tahun_pilihan_dropdown, map_data$historical_avg %||% "N/A",
          map_data$temp_anomaly %||% "N/A",
          map_data$climate_risk_score %||% "N/A"
        ) %>% lapply(htmltools::HTML)
        
        leaflet(map_data) %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor = ~colors(values), weight = 2, opacity = 1, color = "white",
            dashArray = "3", fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"
            )
          ) %>%
          addLegend(
            pal = colors, values = ~values, opacity = 0.7, title = legend_title, position = "bottomright"
          ) %>%
          addControl("Peta Iklim Indonesia", position = "topright")
      } else {
        # Fallback to point map
        leaflet() %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addControl("Data peta tidak tersedia", position = "topright")
      }
      
    } else {
      leaflet() %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addControl("Klik 'Perbarui Peta' untuk menampilkan data", position = "topright")
    }
  })
  
  # =============================================
  # ENHANCED ARIMA FORECASTING PER PROVINCE
  # =============================================
  
  # Populate province choices for ARIMA prediction
  observe({
    province_choices <- sort(unique(monthly_temp_trend_per_province$province_name))
    updateSelectInput(session, "provinsi_prediksi",
                      choices = setNames(province_choices, province_choices),
                      selected = if(length(province_choices) > 0) province_choices[1] else NULL)
  })
  
  # Enhanced ARIMA forecast per province
  output$grafik_prediksi_arima <- renderPlotly({
    req(input$perbarui_prediksi)
    req(input$provinsi_prediksi)
    
    province_data <- monthly_temp_trend_per_province %>%
      filter(province_name == input$provinsi_prediksi) %>%
      arrange(year_month)
    
    if(nrow(province_data) <= 12) {
      return(plotly_empty() %>% 
               layout(title = paste("Data bulanan tidak cukup untuk prediksi ARIMA -", input$provinsi_prediksi)))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(province_data$year, na.rm = TRUE)
    start_month <- min(province_data$month[province_data$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(province_data$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    tryCatch({
      arima_model <- auto.arima(ts_data)
      forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
      
      historical_dates <- province_data$year_month
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
        add_trace(
          x = forecast_dates,
          y = as.numeric(forecast_result$upper[,1]),
          type = 'scatter',
          mode = 'lines',
          line = list(color = '#2a9d8f', width = 1, dash = 'dash'),
          name = 'Batas Atas (95%)',
          hovertemplate = 'Tanggal: %{x}<br>Batas Atas: %{y:.2f}°C<extra></extra>'
        ) %>%
        add_trace(
          x = forecast_dates,
          y = as.numeric(forecast_result$lower[,1]),
          type = 'scatter',
          mode = 'lines',
          line = list(color = '#2a9d8f', width = 1, dash = 'dash'),
          name = 'Batas Bawah (95%)',
          hovertemplate = 'Tanggal: %{x}<br>Batas Bawah: %{y:.2f}°C<extra></extra>'
        ) %>%
        layout(
          title = paste("Prediksi Suhu ARIMA -", input$provinsi_prediksi, "-", forecast_months, "Bulan"),
          xaxis = list(title = "Tanggal"),
          yaxis = list(title = "Suhu (°C)"),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        ) %>%
        config(displayModeBar = FALSE)
      
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = paste("Error dalam prediksi ARIMA untuk", input$provinsi_prediksi, ":", e$message))
    })
  })
  
  # Enhanced model performance per province
  output$performa_model <- renderUI({
    req(input$perbarui_prediksi)
    req(input$provinsi_prediksi)
    
    province_data <- monthly_temp_trend_per_province %>%
      filter(province_name == input$provinsi_prediksi) %>%
      arrange(year_month)
    
    if(nrow(province_data) <= 12) {
      return(p("Data bulanan tidak cukup untuk perhitungan performa model"))
    }
    
    start_year <- min(province_data$year, na.rm = TRUE)
    start_month <- min(province_data$month[province_data$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(province_data$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    tryCatch({
      arima_model <- auto.arima(ts_data)
      
      residuals_data <- residuals(arima_model)
      mae <- round(mean(abs(residuals_data), na.rm = TRUE), 4)
      rmse <- round(sqrt(mean(residuals_data^2, na.rm = TRUE)), 4)
      
      tagList(
        div(style = "margin-bottom: 15px;",
            tags$strong("Provinsi: "), input$provinsi_prediksi
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Tipe Model: "),
            paste0("ARIMA(", paste(arimaorder(arima_model), collapse = ","), ")")
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Jumlah Data: "), nrow(province_data), " bulan"
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
    }, error = function(e) {
      p("Error dalam perhitungan performa model:", e$message)
    })
  })
  
  # Enhanced forecast summary per province
  output$ringkasan_prediksi <- renderUI({
    req(input$perbarui_prediksi)
    req(input$provinsi_prediksi)
    
    province_data <- monthly_temp_trend_per_province %>%
      filter(province_name == input$provinsi_prediksi) %>%
      arrange(year_month)
    
    if(nrow(province_data) <= 12) {
      return(p("Data bulanan tidak cukup untuk ringkasan prediksi"))
    }
    
    forecast_months <- as.numeric(input$bulan_prediksi)
    
    start_year <- min(province_data$year, na.rm = TRUE)
    start_month <- min(province_data$month[province_data$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(province_data$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    tryCatch({
      arima_model <- auto.arima(ts_data)
      forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
      
      final_forecast <- round(as.numeric(forecast_result$mean[forecast_months]), 2)
      current_temp <- round(tail(province_data$temperature, 1), 2)
      temp_change <- round(final_forecast - current_temp, 2)
      
      last_date <- max(province_data$year_month, na.rm = TRUE)
      forecast_end_date <- last_date %m+% months(forecast_months)
      
      tagList(
        div(style = "margin-bottom: 15px;",
            tags$strong("Provinsi: "), input$provinsi_prediksi
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Horizon Prediksi: "), paste(forecast_months, "bulan")
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Periode Prediksi: "),
            paste("Hingga", format(forecast_end_date, "%B %Y"))
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Suhu Terakhir: "), paste0(current_temp, "°C")
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
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Rata-rata Suhu Historis: "),
            paste0(round(mean(province_data$temperature, na.rm = TRUE), 2), "°C")
        )
      )
    }, error = function(e) {
      p("Error dalam ringkasan prediksi:", e$message)
    })
  })
  
  # =============================================
  # DATA TABLES AND VISUALIZATIONS
  # =============================================
  
  # Enhanced comprehensive data table
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
  
  # Enhanced climate metrics table
  output$tabel_metrik_iklim <- DT::renderDataTable({
    enhanced_metrics <- calculate_enhanced_climate_metrics()
    
    if(nrow(enhanced_metrics) == 0) {
      return(DT::datatable(data.frame(Message = "Data metrik tidak tersedia")))
    }
    
    display_data <- enhanced_metrics %>%
      select(
        Provinsi = province_name,
        `Kecepatan Perubahan Iklim (°C/dekade)` = climate_velocity,
        `Variabilitas Musiman (°C)` = seasonal_variability,
        `Persistensi Suhu` = temperature_persistence,
        `Kelengkapan Data (%)` = data_completeness,
        `Signifikansi Tren` = trend_significance
      ) %>%
      mutate(
        `Kecepatan Perubahan Iklim (°C/dekade)` = ifelse(is.na(`Kecepatan Perubahan Iklim (°C/dekade)`), 
                                                         "N/A", 
                                                         as.character(`Kecepatan Perubahan Iklim (°C/dekade)`)),
        `Variabilitas Musiman (°C)` = ifelse(is.na(`Variabilitas Musiman (°C)`), 
                                             "N/A", 
                                             as.character(`Variabilitas Musiman (°C)`)),
        `Persistensi Suhu` = ifelse(is.na(`Persistensi Suhu`), 
                                    "N/A", 
                                    as.character(`Persistensi Suhu`))
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
      DT::formatStyle(
        columns = c(2:6),
        backgroundColor = 'rgba(230, 57, 70, 0.05)',
        fontWeight = 'bold'
      )
  })
  
  # Extreme events table
  output$tabel_kejadian_ekstrem <- DT::renderDataTable({
    extreme_events <- identify_extreme_events()
    
    if(nrow(extreme_events) == 0) {
      return(DT::datatable(data.frame(Message = "Tidak ada kejadian ekstrem yang terdeteksi")))
    }
    
    display_data <- extreme_events %>%
      head(50) %>% # Limit to top 50 events
      mutate(
        Tanggal = format(year_month, "%B %Y"),
        `Suhu Tercatat` = paste0(round(temperature, 1), "°C"),
        `Suhu Normal` = paste0(round(temp_mean, 1), "°C"),
        `Deviasi` = paste0(ifelse(z_score > 0, "+", ""), round(z_score, 2), "σ")
      ) %>%
      select(
        Provinsi = province_name,
        Tanggal,
        `Jenis Kejadian` = event_type,
        `Suhu Tercatat`,
        `Suhu Normal`,
        `Deviasi`
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 20,
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
      DT::formatStyle(
        "Jenis Kejadian",
        backgroundColor = DT::styleEqual(
          c("Gelombang Panas", "Periode Dingin Ekstrem"),
          c("rgba(214, 43, 57, 0.2)", "rgba(42, 157, 143, 0.2)")
        )
      )
  })
  
  # Climate risk assessment table
  output$tabel_penilaian_risiko <- DT::renderDataTable({
    risk_data <- calculate_climate_risk()
    
    if(nrow(risk_data) == 0) {
      return(DT::datatable(data.frame(Message = "Data penilaian risiko tidak tersedia")))
    }
    
    display_data <- risk_data %>%
      select(
        Provinsi = province_name,
        Region = region,
        `Skor Risiko Komposit` = composite_risk,
        `Kategori Risiko` = risk_category,
        `Anomali Suhu (°C)` = temp_anomaly,
        `Kecepatan Perubahan` = climate_velocity,
        `Variabilitas Musiman` = seasonal_variability,
        `Kejadian Ekstrem` = total_extreme_events
      ) %>%
      mutate(
        `Kecepatan Perubahan` = ifelse(is.na(`Kecepatan Perubahan`), "N/A", 
                                       paste0(round(`Kecepatan Perubahan`, 3), "°C/dekade")),
        `Variabilitas Musiman` = ifelse(is.na(`Variabilitas Musiman`), "N/A", 
                                        paste0(round(`Variabilitas Musiman`, 2), "°C"))
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
      DT::formatRound(columns = c(3, 5), digits = 1) %>%
      DT::formatStyle(
        "Kategori Risiko",
        backgroundColor = DT::styleEqual(
          c("Risiko Tinggi", "Risiko Sedang", "Risiko Rendah", "Risiko Minimal"),
          c("rgba(214, 43, 57, 0.3)", "rgba(247, 127, 0, 0.3)", 
            "rgba(72, 149, 239, 0.3)", "rgba(42, 157, 143, 0.3)")
        )
      )
  })
  
  # Climate risk visualization
  output$grafik_penilaian_risiko <- renderPlotly({
    risk_data <- calculate_climate_risk()
    
    if(nrow(risk_data) == 0) {
      return(plotly_empty() %>% layout(title = "Data penilaian risiko tidak tersedia"))
    }
    
    plot_data <- risk_data %>%
      arrange(desc(composite_risk)) %>%
      head(20) # Top 20 provinces by risk
    
    plot_ly(plot_data, 
            x = ~reorder(province_name, composite_risk), 
            y = ~composite_risk,
            type = 'bar',
            marker = list(color = ~risk_color),
            text = ~risk_category,
            hovertemplate = '<b>%{x}</b><br>Skor Risiko: %{y:.1f}<br>Kategori: %{text}<extra></extra>') %>%
      layout(
        title = "Top 20 Provinsi Berdasarkan Risiko Iklim",
        xaxis = list(title = "Provinsi", tickangle = -45),
        yaxis = list(title = "Skor Risiko Komposit (0-10)", range = c(0, 10)),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(b = 120)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Climate change velocity visualization
  output$grafik_kecepatan_perubahan <- renderPlotly({
    enhanced_metrics <- calculate_enhanced_climate_metrics()
    
    if(nrow(enhanced_metrics) == 0 || all(is.na(enhanced_metrics$climate_velocity))) {
      return(plotly_empty() %>% layout(title = "Data kecepatan perubahan tidak tersedia"))
    }
    
    # Filter out NA values
    plot_data <- enhanced_metrics %>%
      filter(!is.na(climate_velocity)) %>%
      arrange(desc(climate_velocity))
    
    # Color coding based on velocity
    plot_data <- plot_data %>%
      mutate(
        color_category = case_when(
          climate_velocity > 0.2 ~ "Pemanasan Cepat",
          climate_velocity > 0 ~ "Pemanasan Sedang",
          climate_velocity > -0.2 ~ "Stabil",
          TRUE ~ "Pendinginan"
        ),
        color_value = case_when(
          climate_velocity > 0.2 ~ "#d62b39",
          climate_velocity > 0 ~ "#f77f00",
          climate_velocity > -0.2 ~ "#4895ef",
          TRUE ~ "#2a9d8f"
        )
      )
    
    plot_ly(plot_data, 
            x = ~reorder(province_name, climate_velocity), 
            y = ~climate_velocity,
            type = 'bar',
            marker = list(color = ~color_value),
            hovertemplate = '<b>%{x}</b><br>Kecepatan: %{y:.3f}°C/dekade<br>Kategori: %{text}<extra></extra>',
            text = ~color_category) %>%
      layout(
        title = "Kecepatan Perubahan Iklim per Provinsi",
        xaxis = list(title = "Provinsi", tickangle = -45),
        yaxis = list(title = "Kecepatan Perubahan (°C/dekade)"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(b = 100)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # =============================================
  # PDF REPORT DOWNLOAD HANDLERS
  # =============================================
  
  # Download handler for provincial analysis report
  output$download_laporan_provinsi <- downloadHandler(
    filename = function() {
      province_name_clean <- if(!is.null(input$provinsi_laporan) && input$provinsi_laporan != "") {
        gsub("[^A-Za-z0-9]", "_", input$provinsi_laporan)
      } else {
        "Unknown_Province"
      }
      paste0("Laporan_Analisis_", province_name_clean, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Validate province selection
      req(input$provinsi_laporan)
      
      # Prepare data for the selected province
      province_data <- monthly_temp_trend_per_province %>%
        filter(province_name == input$provinsi_laporan)
      
      # Check if province data exists
      if(nrow(province_data) == 0) {
        showNotification("Data tidak tersedia untuk provinsi yang dipilih", type = "error")
        return()
      }
      
      # Filter by period if needed
      if(input$periode_laporan == "5_years") {
        max_year <- max(province_data$year, na.rm = TRUE)
        province_data <- province_data %>% filter(year >= (max_year - 4))
      } else if(input$periode_laporan == "10_years") {
        max_year <- max(province_data$year, na.rm = TRUE)
        province_data <- province_data %>% filter(year >= (max_year - 9))
      }
      
      # Get climate data for selected province
      climate_data <- provinces_climate_data %>%
        filter(province_name == input$provinsi_laporan)
      
      # Check if climate data exists
      if(nrow(climate_data) == 0) {
        showNotification("Data iklim tidak tersedia untuk provinsi yang dipilih", type = "error")
        return()
      }
      
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Write Rmd content
      writeLines(c(
        "---",
        paste0("title: 'Laporan Analisis Suhu - ", input$provinsi_laporan, "'"),
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output:",
        "  pdf_document:",
        "    latex_engine: xelatex",
        "    toc: true",
        "    number_sections: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{float}",
        "  - \\usepackage{booktabs}",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.pos = 'H')",
        "library(ggplot2)",
        "library(kableExtra)",
        "library(dplyr)",
        "```",
        "",
        "# Ringkasan Eksekutif",
        "",
        paste0("Laporan ini menyajikan analisis komprehensif mengenai tren suhu di provinsi **", input$provinsi_laporan, "** berdasarkan data historis dari BMKG."),
        "",
        "## Statistik Utama",
        "",
        "```{r stats-table}",
        paste0("province_stats <- data.frame("),
        paste0("  'Indikator' = c('Suhu Saat Ini', 'Rata-rata Historis', 'Anomali Suhu', 'Skor Risiko Iklim'),"),
        paste0("  'Nilai' = c('", climate_data$current_temp[1], "°C', '", climate_data$historical_avg[1], "°C', '", 
               ifelse(climate_data$temp_anomaly[1] >= 0, "+", ""), climate_data$temp_anomaly[1], "°C', '", 
               climate_data$climate_risk_score[1], "/10')"),
        paste0(")"),
        "",
        "kable(province_stats, booktabs = TRUE, caption = paste0('Statistik Suhu - ', '", input$provinsi_laporan, "')) %>%",
        "  kable_styling(latex_options = c('striped', 'hold_position'))",
        "```",
        "",
        "# Analisis Tren Suhu",
        "",
        "```{r trend-analysis}",
        "# Data processing for trend analysis",
        paste0("province_data <- data.frame("),
        paste0("  year = c(", paste(province_data$year, collapse = ", "), "),"),
        paste0("  temperature = c(", paste(round(province_data$temperature, 2), collapse = ", "), ")"),
        paste0(")"),
        "",
        "# Calculate yearly averages",
        "yearly_data <- province_data %>%",
        "  group_by(year) %>%",
        "  summarise(temperature = mean(temperature, na.rm = TRUE), .groups = 'drop')",
        "",
        "# Create trend plot",
        "trend_plot <- ggplot(yearly_data, aes(x = year, y = temperature)) +",
        "  geom_line(color = '#e63946', size = 1.2) +",
        "  geom_point(color = '#730d0d', size = 2) +",
        "  geom_smooth(method = 'lm', se = TRUE, color = '#2a9d8f', alpha = 0.3) +",
        "  labs(title = paste0('Tren Suhu Tahunan - ', '", input$provinsi_laporan, "'),",
        "       x = 'Tahun', y = 'Suhu (°C)') +",
        "  theme_minimal() +",
        "  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'))",
        "",
        "print(trend_plot)",
        "```",
        "",
        "# Kesimpulan dan Rekomendasi",
        "",
        paste0("Berdasarkan analisis data suhu di provinsi ", input$provinsi_laporan, ":"),
        "",
        paste0("- Suhu rata-rata saat ini: **", climate_data$current_temp[1], "°C**"),
        paste0("- Anomali suhu terhadap baseline: **", ifelse(climate_data$temp_anomaly[1] >= 0, "+", ""), climate_data$temp_anomaly[1], "°C**"),
        paste0("- Tingkat risiko iklim: **", climate_data$climate_risk_score[1], "/10**"),
        "",
        "## Rekomendasi",
        "",
        if(climate_data$temp_anomaly[1] > 1) {
          "- Diperlukan mitigasi dampak pemanasan yang signifikan"
        } else if(climate_data$temp_anomaly[1] > 0.5) {
          "- Monitoring berkelanjutan terhadap tren pemanasan"
        } else {
          "- Kondisi suhu relatif stabil, tetap lakukan monitoring rutin"
        },
        "",
        "---",
        "",
        "*Laporan ini dibuat secara otomatis oleh Dashboard Monitor Iklim Indonesia*"
      ), temp_rmd)
      
      # Render to PDF
      tryCatch({
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
        showNotification("Laporan berhasil dibuat!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error membuat laporan:", e$message), type = "error")
      })
    }
  )
  
  # Download handler for regional comparison report
  output$download_laporan_regional <- downloadHandler(
    filename = function() {
      paste0("Laporan_Perbandingan_Regional_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Filter data for selected regions
      selected_regions <- input$regions_laporan
      regional_data <- provinces_climate_data %>%
        filter(region %in% selected_regions)
      
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Write Rmd content
      writeLines(c(
        "---",
        "title: 'Laporan Perbandingan Suhu Regional Indonesia'",
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output:",
        "  pdf_document:",
        "    latex_engine: xelatex",
        "    toc: true",
        "    number_sections: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{float}",
        "  - \\usepackage{booktabs}",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.pos = 'H')",
        "library(ggplot2)",
        "library(kableExtra)",
        "library(dplyr)",
        "```",
        "",
        "# Ringkasan Eksekutif",
        "",
        paste0("Laporan ini membandingkan kondisi suhu di region: **", paste(selected_regions, collapse = ", "), "**."),
        "",
        "# Perbandingan Statistik Regional",
        "",
        "```{r regional-stats}",
        "regional_summary <- data.frame(",
        paste0("  Region = c(", paste0("'", regional_data$region, "'", collapse = ", "), "),"),
        paste0("  Suhu_Rata_rata = c(", paste(regional_data$current_temp, collapse = ", "), "),"),
        paste0("  Anomali_Suhu = c(", paste(regional_data$temp_anomaly, collapse = ", "), "),"),
        paste0("  Skor_Risiko = c(", paste(regional_data$climate_risk_score, collapse = ", "), ")"),
        ")",
        "",
        "kable(regional_summary, booktabs = TRUE, ",
        "      col.names = c('Region', 'Suhu Rata-rata (°C)', 'Anomali (°C)', 'Skor Risiko'),",
        "      caption = 'Perbandingan Statistik Suhu Antar Region') %>%",
        "  kable_styling(latex_options = c('striped', 'hold_position'))",
        "```",
        "",
        "# Visualisasi Perbandingan",
        "",
        "```{r regional-plot}",
        "regional_plot <- ggplot(regional_summary, aes(x = Region, y = Suhu_Rata_rata, fill = Region)) +",
        "  geom_col(alpha = 0.8) +",
        "  geom_text(aes(label = paste0(Suhu_Rata_rata, '°C')), vjust = -0.5) +",
        "  labs(title = 'Perbandingan Suhu Rata-rata Antar Region',",
        "       x = 'Region', y = 'Suhu Rata-rata (°C)') +",
        "  theme_minimal() +",
        "  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),",
        "        axis.text.x = element_text(angle = 45, hjust = 1),",
        "        legend.position = 'none')",
        "",
        "print(regional_plot)",
        "```",
        "",
        "# Kesimpulan",
        "",
        "Berdasarkan analisis perbandingan regional:",
        "",
        paste0("- Region dengan suhu tertinggi: **", regional_data$region[which.max(regional_data$current_temp)], "** (", max(regional_data$current_temp), "°C)"),
        paste0("- Region dengan suhu terendah: **", regional_data$region[which.min(regional_data$current_temp)], "** (", min(regional_data$current_temp), "°C)"),
        paste0("- Variasi suhu antar region: **", round(max(regional_data$current_temp) - min(regional_data$current_temp), 1), "°C**"),
        "",
        "---",
        "",
        "*Laporan ini dibuat secara otomatis oleh Dashboard Monitor Iklim Indonesia*"
      ), temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  # Download handler for climate risk report
  output$download_laporan_risiko <- downloadHandler(
    filename = function() {
      paste0("Laporan_Risiko_Iklim_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Get risk assessment data
      risk_data <- calculate_climate_risk()
      
      # Filter by risk category if specified
      if(input$kategori_risiko_laporan != "all") {
        risk_categories <- list(
          "high" = "Risiko Tinggi",
          "medium" = "Risiko Sedang", 
          "low" = "Risiko Rendah"
        )
        risk_data <- risk_data %>%
          filter(risk_category == risk_categories[[input$kategori_risiko_laporan]])
      }
      
      # Limit number of provinces if specified
      if(input$top_provinces_laporan != "all") {
        top_n <- as.numeric(input$top_provinces_laporan)
        risk_data <- risk_data %>% head(top_n)
      }
      
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Write Rmd content
      writeLines(c(
        "---",
        "title: 'Laporan Penilaian Risiko Iklim Indonesia'",
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output:",
        "  pdf_document:",
        "    latex_engine: xelatex",
        "    toc: true",
        "    number_sections: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{float}",
        "  - \\usepackage{booktabs}",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.pos = 'H')",
        "library(ggplot2)",
        "library(kableExtra)",
        "library(dplyr)",
        "```",
        "",
        "# Ringkasan Eksekutif",
        "",
        "Laporan ini menyajikan penilaian risiko iklim komprehensif untuk provinsi-provinsi di Indonesia berdasarkan indikator suhu, variabilitas iklim, dan kejadian cuaca ekstrem.",
        "",
        "# Metodologi Penilaian Risiko",
        "",
        "Skor risiko iklim dihitung berdasarkan empat komponen utama:",
        "",
        "1. **Anomali Suhu**: Deviasi dari rata-rata historis",
        "2. **Kecepatan Perubahan Iklim**: Laju perubahan suhu per dekade",
        "3. **Variabilitas Musiman**: Tingkat fluktuasi suhu sepanjang tahun",
        "4. **Kejadian Cuaca Ekstrem**: Frekuensi gelombang panas dan periode dingin ekstrem",
        "",
        "# Hasil Penilaian Risiko",
        "",
        "```{r risk-table}",
        "risk_summary <- data.frame(",
        paste0("  Provinsi = c(", paste0("'", risk_data$province_name, "'", collapse = ", "), "),"),
        paste0("  Skor_Risiko = c(", paste(risk_data$composite_risk, collapse = ", "), "),"),
        paste0("  Kategori = c(", paste0("'", risk_data$risk_category, "'", collapse = ", "), "),"),
        paste0("  Anomali_Suhu = c(", paste(risk_data$temp_anomaly, collapse = ", "), ")"),
        ")",
        "",
        "kable(risk_summary, booktabs = TRUE,",
        "      col.names = c('Provinsi', 'Skor Risiko', 'Kategori Risiko', 'Anomali Suhu (°C)'),",
        "      caption = 'Penilaian Risiko Iklim per Provinsi') %>%",
        "  kable_styling(latex_options = c('striped', 'hold_position', 'scale_down'))",
        "```",
        "",
        "# Distribusi Risiko",
        "",
        "```{r risk-distribution}",
        "risk_counts <- table(risk_summary$Kategori)",
        "risk_df <- data.frame(",
        "  Kategori = names(risk_counts),",
        "  Jumlah = as.numeric(risk_counts)",
        ")",
        "",
        "dist_plot <- ggplot(risk_df, aes(x = Kategori, y = Jumlah, fill = Kategori)) +",
        "  geom_col(alpha = 0.8) +",
        "  geom_text(aes(label = Jumlah), vjust = -0.5) +",
        "  labs(title = 'Distribusi Kategori Risiko Iklim',",
        "       x = 'Kategori Risiko', y = 'Jumlah Provinsi') +",
        "  theme_minimal() +",
        "  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),",
        "        axis.text.x = element_text(angle = 45, hjust = 1),",
        "        legend.position = 'none')",
        "",
        "print(dist_plot)",
        "```",
        "",
        "# Rekomendasi Strategis",
        "",
        "## Prioritas Tinggi",
        if(nrow(risk_data[risk_data$risk_category == "Risiko Tinggi", ]) > 0) {
          paste0("- Provinsi berisiko tinggi memerlukan intervensi segera: **", 
                 paste(risk_data$province_name[risk_data$risk_category == "Risiko Tinggi"], collapse = ", "), "**")
        } else {
          "- Tidak ada provinsi dengan kategori risiko tinggi"
        },
        "",
        "## Monitoring Berkelanjutan",
        if(nrow(risk_data[risk_data$risk_category == "Risiko Sedang", ]) > 0) {
          paste0("- Provinsi berisiko sedang perlu monitoring ketat: **", 
                 paste(risk_data$province_name[risk_data$risk_category == "Risiko Sedang"], collapse = ", "), "**")
        } else {
          "- Tidak ada provinsi dengan kategori risiko sedang"
        },
        "",
        "---",
        "",
        "*Laporan ini dibuat secara otomatis oleh Dashboard Monitor Iklim Indonesia*"
      ), temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  # Download handler for comprehensive national report
  output$download_laporan_nasional <- downloadHandler(
    filename = function() {
      paste0("Laporan_Nasional_Monitor_Iklim_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Prepare comprehensive data
      all_provinces_data <- provinces_climate_data
      national_stats <- list(
        total_provinces = nrow(all_provinces_data),
        avg_temp = round(mean(all_provinces_data$current_temp, na.rm = TRUE), 2),
        max_temp = max(all_provinces_data$current_temp, na.rm = TRUE),
        min_temp = min(all_provinces_data$current_temp, na.rm = TRUE),
        avg_anomaly = round(mean(all_provinces_data$temp_anomaly, na.rm = TRUE), 2)
      )
      
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Determine content based on detail level
      detail_sections <- if(input$detail_level_laporan == "summary") {
        c("# Ringkasan Eksekutif", "# Statistik Nasional", "# Kesimpulan Utama")
      } else if(input$detail_level_laporan == "standard") {
        c("# Ringkasan Eksekutif", "# Statistik Nasional", "# Analisis Regional", "# Penilaian Risiko", "# Kesimpulan dan Rekomendasi")
      } else {
        c("# Ringkasan Eksekutif", "# Statistik Nasional", "# Analisis Regional", "# Penilaian Risiko", "# Analisis Tren", "# Prediksi dan Proyeksi", "# Kesimpulan dan Rekomendasi")
      }
      
      # Write comprehensive Rmd content
      rmd_content <- c(
        "---",
        "title: 'Laporan Monitor Iklim Indonesia - Analisis Komprehensif'",
        "subtitle: 'Dashboard Analisis Suhu Real-time Indonesia'",
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output:",
        "  pdf_document:",
        "    latex_engine: xelatex",
        "    toc: true",
        "    number_sections: true",
        "geometry: margin=1in",
        "header-includes:",
        "  - \\usepackage{float}",
        "  - \\usepackage{booktabs}",
        "  - \\usepackage{longtable}",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.pos = 'H')",
        "library(ggplot2)",
        "library(kableExtra)",
        "library(dplyr)",
        "```",
        "",
        "# Ringkasan Eksekutif",
        "",
        "Dashboard Monitor Iklim Indonesia menyajikan analisis komprehensif kondisi suhu di seluruh Indonesia berdasarkan data historis BMKG. Laporan ini mencakup analisis tren, penilaian risiko, dan proyeksi iklim untuk mendukung pengambilan keputusan strategis.",
        "",
        "## Temuan Utama",
        "",
        paste0("- **", national_stats$total_provinces, "** provinsi dianalisis dengan data suhu lengkap"),
        paste0("- Suhu rata-rata nasional: **", national_stats$avg_temp, "°C**"),
        paste0("- Rentang suhu: **", national_stats$min_temp, "°C** hingga **", national_stats$max_temp, "°C**"),
        paste0("- Anomali suhu rata-rata: **", ifelse(national_stats$avg_anomaly >= 0, "+", ""), national_stats$avg_anomaly, "°C**"),
        "",
        "# Statistik Nasional",
        "",
        "```{r national-stats}",
        "national_overview <- data.frame(",
        paste0("  Indikator = c('Total Provinsi', 'Suhu Rata-rata Nasional', 'Suhu Tertinggi', 'Suhu Terendah', 'Anomali Rata-rata'),"),
        paste0("  Nilai = c('", national_stats$total_provinces, "', '", national_stats$avg_temp, "°C', '", 
               national_stats$max_temp, "°C', '", national_stats$min_temp, "°C', '", 
               ifelse(national_stats$avg_anomaly >= 0, "+", ""), national_stats$avg_anomaly, "°C')"),
        ")",
        "",
        "kable(national_overview, booktabs = TRUE, ",
        "      col.names = c('Indikator', 'Nilai'),",
        "      caption = 'Statistik Suhu Nasional Indonesia') %>%",
        "  kable_styling(latex_options = c('striped', 'hold_position'))",
        "```"
      )
      
      # Add regional analysis if detailed
      if(input$detail_level_laporan %in% c("standard", "detailed")) {
        rmd_content <- c(rmd_content,
                         "",
                         "# Analisis Regional",
                         "",
                         "```{r regional-analysis}",
                         "regional_stats <- data.frame(",
                         paste0("  Region = c(", paste0("'", unique(all_provinces_data$region), "'", collapse = ", "), "),"),
                         "  Suhu_Rata_rata = c(",
                         paste(sapply(unique(all_provinces_data$region), function(r) {
                           round(mean(all_provinces_data$current_temp[all_provinces_data$region == r], na.rm = TRUE), 2)
                         }), collapse = ", "),
                         "),",
                         "  Jumlah_Provinsi = c(",
                         paste(sapply(unique(all_provinces_data$region), function(r) {
                           sum(all_provinces_data$region == r, na.rm = TRUE)
                         }), collapse = ", "),
                         ")",
                         ")",
                         "",
                         "kable(regional_stats, booktabs = TRUE,",
                         "      col.names = c('Region', 'Suhu Rata-rata (°C)', 'Jumlah Provinsi'),",
                         "      caption = 'Analisis Suhu per Region') %>%",
                         "  kable_styling(latex_options = c('striped', 'hold_position'))",
                         "```"
        )
      }
      
      # Add conclusion
      rmd_content <- c(rmd_content,
                       "",
                       "# Kesimpulan dan Rekomendasi",
                       "",
                       "## Kesimpulan Utama",
                       "",
                       "1. **Kondisi Suhu Nasional**: Indonesia menunjukkan variasi suhu yang signifikan antar region",
                       paste0("2. **Tren Pemanasan**: Anomali suhu rata-rata ", ifelse(national_stats$avg_anomaly >= 0, "positif", "negatif"), " sebesar ", abs(national_stats$avg_anomaly), "°C"),
                       "3. **Disparitas Regional**: Terdapat perbedaan kondisi iklim yang memerlukan pendekatan adaptasi yang berbeda",
                       "",
                       "## Rekomendasi Strategis",
                       "",
                       "1. **Monitoring Berkelanjutan**: Implementasi sistem monitoring iklim real-time",
                       "2. **Adaptasi Regional**: Strategi mitigasi yang disesuaikan dengan kondisi spesifik tiap region",
                       "3. **Early Warning System**: Pengembangan sistem peringatan dini untuk cuaca ekstrem",
                       "",
                       "---",
                       "",
                       "*Laporan ini dibuat secara otomatis oleh Dashboard Monitor Iklim Indonesia*",
                       "",
                       "**Tim Pengembang:**",
                       "",
                       "- Evelyn Tan Eldisha Nawa (222313067)",
                       "- Farhan Kadhafi Azuansyah (222313079)", 
                       "- Naufal Dzaki Zaidan (222313290)"
      )
      
      writeLines(rmd_content, temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  # =============================================
  # EXISTING DOWNLOAD HANDLERS FOR DATA
  # =============================================
  
  # Download handlers for data export
  output$download_climate_data <- downloadHandler(
    filename = function() {
      paste0("data_iklim_indonesia_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(provinces_climate_data, file, row.names = FALSE)
    }
  )
  
  output$download_monthly_data <- downloadHandler(
    filename = function() {
      paste0("data_bulanan_provinsi_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(monthly_temp_trend_per_province, file, row.names = FALSE)
    }
  )
  
  output$download_risk_assessment <- downloadHandler(
    filename = function() {
      paste0("penilaian_risiko_iklim_", Sys.Date(), ".csv")
    },
    content = function(file) {
      risk_data <- calculate_climate_risk()
      write.csv(risk_data, file, row.names = FALSE)
    }
  )
  
  # =============================================
  # ENHANCED BUTTON TRIGGERS
  # =============================================
  
  # Enhanced button trigger for trend updates
  observeEvent(input$perbarui_tren, {
    # Force update of trend outputs
    output$grafik_tren_detail <- renderPlotly({
      req(input$provinsi_tren)
      
      province_data <- monthly_temp_trend_per_province %>%
        filter(province_name == input$provinsi_tren) %>%
        arrange(year_month)
      
      if(nrow(province_data) < 2) {
        return(plotly_empty() %>% layout(title = paste("Data tidak cukup untuk analisis tren -", input$provinsi_tren)))
      }
      
      jenis_tren <- input$jenis_tren
      
      if(jenis_tren == "yearly") {
        yearly_data <- province_data %>%
          group_by(year) %>%
          summarise(temperature = round(mean(temperature, na.rm = TRUE), 2), .groups = 'drop') %>%
          filter(!is.na(temperature))
        
        if(nrow(yearly_data) < 2) {
          return(plotly_empty() %>% layout(title = "Data tahunan tidak cukup"))
        }
        
        lm_model <- lm(temperature ~ year, data = yearly_data)
        trend_line <- predict(lm_model, newdata = yearly_data)
        
        plot_ly(yearly_data, x = ~year, y = ~temperature,
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#e63946', width = 3),
                marker = list(color = '#730d0d', size = 8),
                name = 'Suhu Tahunan',
                hovertemplate = 'Tahun: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>') %>%
          add_trace(x = yearly_data$year, y = trend_line,
                    type = 'scatter', mode = 'lines',
                    line = list(color = '#2a9d8f', width = 2, dash = 'dash'),
                    name = 'Tren Linear',
                    hovertemplate = 'Tren: %{y:.2f}°C<extra></extra>') %>%
          layout(
            title = paste("Tren Suhu Tahunan -", input$provinsi_tren, "(Diperbarui)"),
            xaxis = list(title = "Tahun"),
            yaxis = list(title = "Suhu (°C)"),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%
          config(displayModeBar = FALSE)
        
      } else if(jenis_tren == "monthly") {
        plot_ly(province_data, x = ~year_month, y = ~temperature,
                type = 'scatter', mode = 'lines',
                line = list(color = '#e63946', width = 2),
                name = 'Suhu Bulanan',
                hovertemplate = 'Tanggal: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>') %>%
          layout(
            title = paste("Tren Suhu Bulanan -", input$provinsi_tren, "(Diperbarui)"),
            xaxis = list(title = "Tanggal"),
            yaxis = list(title = "Suhu (°C)"),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%
          config(displayModeBar = FALSE)
        
      } else {
        yearly_comparison <- province_data %>%
          mutate(year_factor = as.factor(year))
        
        plot_ly(yearly_comparison, x = ~year_factor, y = ~temperature,
                type = 'box',
                name = 'Distribusi Suhu',
                marker = list(color = '#e63946'),
                hovertemplate = 'Tahun: %{x}<br>Suhu: %{y:.2f}°C<extra></extra>') %>%
          layout(
            title = paste("Perbandingan Multi-Tahun -", input$provinsi_tren, "(Diperbarui)"),
            xaxis = list(title = "Tahun"),
            yaxis = list(title = "Suhu (°C)"),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          ) %>%
          config(displayModeBar = FALSE)
      }
    })
  })
  
  # Enhanced map update event handler
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
}

# =============================================
# RUN APPLICATION
# =============================================

shinyApp(ui = ui, server = server)
