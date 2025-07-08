# =============================================
# INDONESIA CLIMATE PULSE DASHBOARD - GLOBAL
# Data Processing and Global Variables
# =============================================

# Load required libraries
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

# Set working directory and load data
# setwd("C:/Tugas Zidan/Sem 4/Komstat/Projek/deploy")

# Create sample data for demonstration
# Replace this with your actual data loading
temp_data <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month"),
  Tavg = runif(48, 24, 32),
  province_name = sample(c("DKI Jakarta", "Jawa Barat", "Jawa Tengah", "Jawa Timur", 
                          "Sumatera Utara", "Sumatera Barat", "Kalimantan Timur", "Sulawesi Selatan"), 48, replace = TRUE),
  province_id = sample(1:8, 48, replace = TRUE)
)

# Uncomment and modify this line to load your actual data
# temp_data <- read_excel("data_suhu_lengkap.xlsx")

# Debug: Check data structure
cat("Data columns:", names(temp_data), "\n")
cat("Data rows:", nrow(temp_data), "\n")

# Ensure date column exists and is properly formatted
if("date" %in% names(temp_data)) {
  temp_data$date <- as.Date(temp_data$date)
} else {
  stop("Column 'date' not found in data")
}

temp_data$year <- year(temp_data$date)
temp_data$month <- month(temp_data$date)

# Process climate data with error handling
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

# Monthly temperature trends
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

# National temperature trend
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

# Monthly seasonal data
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

# Load GeoJSON Indonesia Provinces
geojson_path <- "indonesia-prov.geojson"
indonesia_provinces <- NULL

# Create fallback data for demonstration
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

# Fill missing coordinates with random values around Indonesia
missing_coords <- is.na(fallback_data$lat) | is.na(fallback_data$lng)
if(any(missing_coords)) {
  fallback_data$lat[missing_coords] <- runif(sum(missing_coords), -10, 5)
  fallback_data$lng[missing_coords] <- runif(sum(missing_coords), 95, 140)
}

cat("Global data processing completed.\n")
