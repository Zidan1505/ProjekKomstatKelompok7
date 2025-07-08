# =============================================
# INDONESIA CLIMATE PULSE DASHBOARD
# Enhanced Clean Design with Excel Data Source & Local GeoJSON
# =============================================

# Load required packages
library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(htmltools)
library(shinyjs)
library(sf)
library(geojsonsf)  # Added for reading GeoJSON files
library(jsonlite)   # Alternative for reading GeoJSON
library(forecast)
library(lubridate)
library(readxl)     # Added for reading Excel files
library(stringr)    # Added for string manipulation

# =============================================
# LOAD AND PROCESS EXCEL DATA
# =============================================

setwd("C:/Tugas Zidan/Sem 4/Komstat/Projek")
cat("Loading temperature data from Excel file...\n")
temp_data <- read_excel("data_suhu_lengkap.xlsx")

# Convert date column to Date type
temp_data$date <- as.Date(temp_data$date)
temp_data$year <- year(temp_data$date)
temp_data$month <- month(temp_data$date)

# Check data structure
cat("Data loaded successfully. Rows:", nrow(temp_data), "Columns:", ncol(temp_data), "\n")
cat("Date range:", min(temp_data$date, na.rm = TRUE), "to", max(temp_data$date, na.rm = TRUE), "\n")
cat("Unique provinces:", length(unique(temp_data$province_name)), "\n")

# =============================================
# PROCESS TEMPERATURE DATA
# =============================================

# Calculate statistics by province
provinces_climate_data <- temp_data %>%
  group_by(province_name, province_id) %>%
  summarise(
    # Current temperature (latest available data)
    current_temp = round(mean(Tavg[year == max(year, na.rm = TRUE)], na.rm = TRUE), 1),
    
    # Historical average (1990-2020 or available range)
    historical_avg = round(mean(Tavg[year <= 2020], na.rm = TRUE), 1),
    
    # Temperature anomaly
    temp_anomaly = round(current_temp - historical_avg, 1),
    
    # Data availability
    data_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  # Remove rows with missing essential data
  filter(!is.na(current_temp) & !is.na(historical_avg)) %>%
  # Calculate future projection (simple linear extrapolation)
  mutate(
    future_2050 = round(current_temp + (temp_anomaly * 1.5), 1),
    
    # Calculate climate risk score based on temperature anomaly and current temp
    climate_risk_score = round(pmin(10, pmax(1, 
                                             (temp_anomaly * 2) + (current_temp - 25) * 0.5 + 3)), 1),
    
    # Add regional groupings based on province names from Excel data
    region = case_when(
      grepl("Jawa|Jakarta|Bali", province_name, ignore.case = TRUE) ~ "Jawa-Bali",
      grepl("Sumatra|Aceh|Riau|Jambi|Lampung", province_name, ignore.case = TRUE) ~ "Sumatra",
      grepl("Kalimantan", province_name, ignore.case = TRUE) ~ "Kalimantan",
      grepl("Sulawesi", province_name, ignore.case = TRUE) ~ "Sulawesi",
      grepl("Papua", province_name, ignore.case = TRUE) ~ "Papua",
      grepl("Maluku|Nusa Tenggara", province_name, ignore.case = TRUE) ~ "Maluku-Nusa Tenggara",
      TRUE ~ "Other"
    )
  )

cat("Processed climate data for", nrow(provinces_climate_data), "provinces\n")

# =============================================
# CREATE MONTHLY TEMPERATURE TRENDS
# =============================================

# Calculate monthly temperature trends
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
  # Calculate anomaly based on historical baseline
  mutate(
    baseline_temp = mean(temperature[year <= 2000], na.rm = TRUE),
    anomaly = round(temperature - baseline_temp, 2)
  )

cat("Monthly temperature trends calculated for", nrow(monthly_temp_trend), "months\n")

# Keep yearly data for some charts
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

# =============================================
# CREATE MONTHLY SEASONAL DATA
# =============================================

# Calculate monthly seasonal patterns
monthly_data <- temp_data %>%
  group_by(month) %>%
  summarise(
    avg_temp = round(mean(Tavg, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_temp)) %>%
  mutate(
    month_name = month.name[month],
    seasonal_anomaly = round(avg_temp - mean(avg_temp, na.rm = TRUE), 2)
  ) %>%
  arrange(month)

cat("Monthly seasonal data calculated\n")

# =============================================
# LOAD INDONESIA PROVINCE GEOJSON
# =============================================

# Load GeoJSON file from local directory
cat("Loading Indonesia province polygons from local GeoJSON...\n")

# Define the path to your local GeoJSON file
# UPDATE THIS PATH TO YOUR LOCAL GEOJSON FILE
geojson_path <- "indonesia-prov.geojson"  # Change this to your actual file path

tryCatch({
  # Method 1: Using geojsonsf package (recommended)
  indonesia_provinces <- geojson_sf(geojson_path)
  
  # Alternative Method 2: Using st_read from sf package
  # indonesia_provinces <- st_read(geojson_path)
  
  # Clean and standardize province names
  # Adjust column names based on your GeoJSON structure
  # Common GeoJSON column names: NAME, name, NAME_1, PROVINSI, etc.
  
  # Check what columns exist in your GeoJSON
  cat("Available columns in GeoJSON:", paste(names(indonesia_provinces), collapse = ", "), "\n")
  
  # Function to clean and standardize province names
  clean_province_names <- function(name) {
    # Convert to character if not already
    name <- as.character(name)
    
    # Remove common prefixes and standardize
    name <- gsub("^PROVINSI\\s+", "", name, ignore.case = TRUE)
    name <- gsub("^DAERAH ISTIMEWA\\s+", "", name, ignore.case = TRUE)
    name <- gsub("^DAERAH KHUSUS IBUKOTA\\s+", "", name, ignore.case = TRUE)
    name <- gsub("^DI\\.?\\s+", "", name, ignore.case = TRUE)
    name <- gsub("^DKI\\s+", "", name, ignore.case = TRUE)
    
    # Handle specific cases to match your temperature data format
    name <- case_when(
      grepl("^ACEH", name, ignore.case = TRUE) ~ "Aceh",
      grepl("SUMATERA UTARA|SUMATRA UTARA", name, ignore.case = TRUE) ~ "Sumatera Utara",
      grepl("SUMATERA BARAT|SUMATRA BARAT", name, ignore.case = TRUE) ~ "Sumatera Barat",
      grepl("SUMATERA SELATAN|SUMATRA SELATAN", name, ignore.case = TRUE) ~ "Sumatera Selatan",
      grepl("RIAU", name, ignore.case = TRUE) & !grepl("KEPULAUAN", name, ignore.case = TRUE) ~ "Riau",
      grepl("KEPULAUAN RIAU", name, ignore.case = TRUE) ~ "Kepulauan Riau",
      grepl("JAMBI", name, ignore.case = TRUE) ~ "Jambi",
      grepl("BENGKULU", name, ignore.case = TRUE) ~ "Bengkulu",
      grepl("LAMPUNG", name, ignore.case = TRUE) ~ "Lampung",
      grepl("BANGKA BELITUNG", name, ignore.case = TRUE) ~ "Bangka Belitung",
      grepl("JAKARTA", name, ignore.case = TRUE) ~ "DKI Jakarta",
      grepl("JAWA BARAT", name, ignore.case = TRUE) ~ "Jawa Barat",
      grepl("JAWA TENGAH", name, ignore.case = TRUE) ~ "Jawa Tengah",
      grepl("JAWA TIMUR", name, ignore.case = TRUE) ~ "Jawa Timur",
      grepl("YOGYAKARTA", name, ignore.case = TRUE) ~ "Yogyakarta",
      grepl("BANTEN", name, ignore.case = TRUE) ~ "Banten",
      grepl("BALI", name, ignore.case = TRUE) ~ "Bali",
      grepl("NUSA TENGGARA BARAT|NUSATENGGARA BARAT", name, ignore.case = TRUE) ~ "Nusa Tenggara Barat",
      grepl("NUSA TENGGARA TIMUR|NUSATENGGARA TIMUR", name, ignore.case = TRUE) ~ "Nusa Tenggara Timur",
      grepl("KALIMANTAN BARAT", name, ignore.case = TRUE) ~ "Kalimantan Barat",
      grepl("KALIMANTAN TENGAH", name, ignore.case = TRUE) ~ "Kalimantan Tengah",
      grepl("KALIMANTAN SELATAN", name, ignore.case = TRUE) ~ "Kalimantan Selatan",
      grepl("KALIMANTAN TIMUR", name, ignore.case = TRUE) ~ "Kalimantan Timur",
      grepl("KALIMANTAN UTARA", name, ignore.case = TRUE) ~ "Kalimantan Utara",
      grepl("SULAWESI UTARA", name, ignore.case = TRUE) ~ "Sulawesi Utara",
      grepl("SULAWESI TENGAH", name, ignore.case = TRUE) ~ "Sulawesi Tengah",
      grepl("SULAWESI SELATAN", name, ignore.case = TRUE) ~ "Sulawesi Selatan",
      grepl("SULAWESI TENGGARA", name, ignore.case = TRUE) ~ "Sulawesi Tenggara",
      grepl("SULAWESI BARAT", name, ignore.case = TRUE) ~ "Sulawesi Barat",
      grepl("GORONTALO", name, ignore.case = TRUE) ~ "Gorontalo",
      grepl("MALUKU(?!\\s+UTARA)", name, ignore.case = TRUE, perl = TRUE) ~ "Maluku",
      grepl("MALUKU UTARA", name, ignore.case = TRUE) ~ "Maluku Utara",
      grepl("PAPUA BARAT", name, ignore.case = TRUE) ~ "Papua Barat",
      grepl("PAPUA(?!\\s+BARAT)", name, ignore.case = TRUE, perl = TRUE) ~ "Papua",
      TRUE ~ str_to_title(name)  # Default: convert to title case
    )
    
    # Final cleanup
    name <- trimws(name)
    
    return(name)
  }
  
  # Apply cleaning function based on available columns
  if("NAME_1" %in% names(indonesia_provinces)) {
    indonesia_provinces$name_clean <- clean_province_names(indonesia_provinces$NAME_1)
  } else if("NAME" %in% names(indonesia_provinces)) {
    indonesia_provinces$name_clean <- clean_province_names(indonesia_provinces$NAME)
  } else if("PROVINSI" %in% names(indonesia_provinces)) {
    indonesia_provinces$name_clean <- clean_province_names(indonesia_provinces$PROVINSI)
  } else if("name" %in% names(indonesia_provinces)) {
    indonesia_provinces$name_clean <- clean_province_names(indonesia_provinces$name)
  } else {
    # Use the first character column found
    char_cols <- sapply(indonesia_provinces, is.character)
    if(any(char_cols)) {
      first_char_col <- names(char_cols)[which(char_cols)[1]]
      indonesia_provinces$name_clean <- clean_province_names(indonesia_provinces[[first_char_col]])
      cat("Using column '", first_char_col, "' as province name\n")
    } else {
      indonesia_provinces$name_clean <- paste0("Province_", 1:nrow(indonesia_provinces))
    }
  }
  
  cat("Successfully loaded", nrow(indonesia_provinces), "province polygons from GeoJSON\n")
  
  # Print sample of cleaned names for verification
  cat("Sample of cleaned province names:\n")
  print(head(indonesia_provinces$name_clean, 10))
  
}, error = function(e) {
  cat("Error loading GeoJSON file:", e$message, "\n")
  cat("Please check:\n")
  cat("1. File path is correct:", geojson_path, "\n")
  cat("2. File exists and is readable\n")
  cat("3. File is valid GeoJSON format\n")
  cat("Creating dummy polygon data...\n")
  
  # Create dummy polygon data if loading fails
  indonesia_provinces <- data.frame(
    NAME_1 = unique(provinces_climate_data$province_name)[1:min(10, nrow(provinces_climate_data))],
    name_clean = unique(provinces_climate_data$province_name)[1:min(10, nrow(provinces_climate_data))]
  )
})

# =============================================
# VALIDATION: COMPARE GEOJSON NAMES WITH TEMPERATURE DATA
# =============================================

cat("\n=== VALIDATION: MATCHING PROVINCE NAMES ===\n")

# Get unique province names from both datasets
temp_provinces <- unique(provinces_climate_data$province_name)
geojson_provinces <- unique(indonesia_provinces$name_clean)

cat("Provinces in temperature data:", length(temp_provinces), "\n")
cat("Provinces in GeoJSON:", length(geojson_provinces), "\n")

# Find matches
matched_provinces <- intersect(temp_provinces, geojson_provinces)
cat("Matched provinces:", length(matched_provinces), "\n")

# Find unmatched provinces
unmatched_temp <- setdiff(temp_provinces, geojson_provinces)
unmatched_geojson <- setdiff(geojson_provinces, temp_provinces)

if(length(unmatched_temp) > 0) {
  cat("\nProvinces in temperature data but not in GeoJSON:\n")
  cat(paste("-", unmatched_temp, collapse = "\n"))
}

if(length(unmatched_geojson) > 0) {
  cat("\nProvinces in GeoJSON but not in temperature data:\n")
  cat(paste("-", unmatched_geojson, collapse = "\n"))
}

# =============================================
# MERGE TEMPERATURE DATA WITH GEOJSON
# =============================================

# Merge climate data with province polygons
if(exists("indonesia_provinces") && nrow(indonesia_provinces) > 0) {
  # Merge based on cleaned names
  indonesia_provinces_with_climate <- merge(
    indonesia_provinces,
    provinces_climate_data,
    by.x = "name_clean",
    by.y = "province_name",
    all.x = TRUE
  )
  
  cat("\nMerged data: ", nrow(indonesia_provinces_with_climate), " provinces with polygon data\n")
  cat("Provinces with climate data: ", sum(!is.na(indonesia_provinces_with_climate$current_temp)), "\n")
  
} else {
  cat("\nWarning: No valid polygon data available for mapping\n")
}

# =============================================
# ADDITIONAL HELPER FUNCTIONS
# =============================================

# Function to check and validate GeoJSON file
validate_geojson <- function(file_path) {
  if(!file.exists(file_path)) {
    cat("ERROR: GeoJSON file not found at:", file_path, "\n")
    return(FALSE)
  }
  
  # Check if file is readable
  tryCatch({
    test_read <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
    if(!is.null(test_read$type) && test_read$type == "FeatureCollection") {
      cat("✓ Valid GeoJSON FeatureCollection found\n")
      return(TRUE)
    } else {
      cat("WARNING: File may not be a valid GeoJSON FeatureCollection\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("ERROR: Cannot parse GeoJSON file:", e$message, "\n")
    return(FALSE)
  })
}

# Function to explore GeoJSON structure
explore_geojson_structure <- function(file_path) {
  if(!file.exists(file_path)) {
    cat("File not found:", file_path, "\n")
    return(NULL)
  }
  
  tryCatch({
    # Read just the first feature to check structure
    geojson_data <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
    
    if(!is.null(geojson_data$features) && length(geojson_data$features) > 0) {
      first_feature <- geojson_data$features[[1]]
      if(!is.null(first_feature$properties)) {
        cat("Available properties in GeoJSON:\n")
        property_names <- names(first_feature$properties)
        for(prop in property_names) {
          cat("-", prop, ":", first_feature$properties[[prop]], "\n")
        }
        return(property_names)
      }
    }
  }, error = function(e) {
    cat("Error exploring GeoJSON:", e$message, "\n")
  })
  
  return(NULL)
}

# Function to find closest matching province names
find_closest_match <- function(temp_name, geojson_names, threshold = 0.8) {
  # Simple string distance matching
  distances <- sapply(geojson_names, function(geo_name) {
    # Calculate Jaccard similarity
    temp_words <- tolower(unlist(strsplit(temp_name, "\\s+")))
    geo_words <- tolower(unlist(strsplit(geo_name, "\\s+")))
    
    intersection <- length(intersect(temp_words, geo_words))
    union <- length(union(temp_words, geo_words))
    
    if(union == 0) return(0)
    return(intersection / union)
  })
  
  best_match_idx <- which.max(distances)
  if(distances[best_match_idx] >= threshold) {
    return(list(
      match = names(distances)[best_match_idx],
      similarity = distances[best_match_idx]
    ))
  }
  return(NULL)
}

# Try to find matches for unmatched provinces
if(length(unmatched_temp) > 0 && length(geojson_provinces) > 0) {
  cat("\n=== ATTEMPTING TO FIND CLOSE MATCHES ===\n")
  
  for(temp_prov in unmatched_temp) {
    closest <- find_closest_match(temp_prov, geojson_provinces)
    if(!is.null(closest)) {
      cat("Possible match: '", temp_prov, "' -> '", closest$match, "' (similarity: ", 
          round(closest$similarity, 2), ")\n")
    }
  }
}

# =============================================
# DATA VALIDATION AND SUMMARY
# =============================================

cat("\n=== DATA SUMMARY ===\n")
cat("Temperature data range:", min(temp_data$date), "to", max(temp_data$date), "\n")
cat("Number of provinces:", nrow(provinces_climate_data), "\n")
cat("Average current temperature:", round(mean(provinces_climate_data$current_temp, na.rm = TRUE), 1), "°C\n")
cat("Average temperature anomaly:", round(mean(provinces_climate_data$temp_anomaly, na.rm = TRUE), 1), "°C\n")
cat("Monthly temperature trends calculated for", nrow(monthly_temp_trend), "months\n")
cat("Date range:", min(monthly_temp_trend$year_month), "to", max(monthly_temp_trend$year_month), "\n")

# =============================================
# ENHANCED CLEAN UI (Same as before)
# =============================================

ui <- fluidPage(
  theme = NULL,
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    
    # Enhanced Clean CSS (same as original)
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
      
      /* Header/Navigation */
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
        background: rgba(255,255,255,0.2);
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
      }
      
      .nav-link::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: rgba(255,255,255,0.2);
        transition: left 0.3s ease;
      }
      
      .nav-link:hover::before {
        left: 0;
      }
      
      .nav-link:hover {
        background: rgba(255,255,255,0.1);
        transform: translateY(-2px);
      }
      
      .nav-link i {
        font-size: 16px;
      }
      
      /* Main Content */
      .container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 40px 20px;
      }
      
      /* Hero Section */
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
      
      .btn::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.4), transparent);
        transition: left 0.5s ease;
      }
      
      .btn:hover::before {
        left: 100%;
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
        box-shadow: 0 15px 35px rgba(0,0,0,0.1);
        min-height: 350px;
        border: 3px solid rgba(230, 57, 70, 0.1);
      }
      
      /* Enhanced Stat Cards */
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
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        position: relative;
        overflow: hidden;
        transition: all 0.3s ease;
        border: 2px solid transparent;
      }
      
      .stat-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 20px 40px rgba(0,0,0,0.15);
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
      
      /* Enhanced Section Titles */
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
      
      /* Enhanced Insights Cards */
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
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        transition: all 0.3s ease;
        border: 2px solid transparent;
      }
      
      .insight-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 20px 40px rgba(0,0,0,0.15);
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
      
      /* Additional Home Content */
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
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        border: 2px solid rgba(230, 57, 70, 0.1);
        transition: all 0.3s ease;
      }
      
      .content-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 20px 40px rgba(0,0,0,0.15);
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
      
      /* Tab Content Styling */
      .tab-content {
        padding: 40px;
        background: white;
        border-radius: 20px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
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
      
      /* Control Panel */
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
      
      .form-control, .form-select {
        width: 100%;
        padding: 12px 16px;
        border: 2px solid #e9ecef;
        border-radius: 10px;
        font-size: 14px;
        font-weight: 500;
        transition: all 0.3s ease;
        background: white;
      }
      
      .form-control:focus, .form-select:focus {
        outline: none;
        border-color: #e63946;
        box-shadow: 0 0 0 3px rgba(230, 57, 70, 0.1);
      }
      
      /* Chart Container */
      .chart-container {
        background: white;
        border-radius: 15px;
        padding: 30px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.1);
        border: 2px solid rgba(230, 57, 70, 0.1);
      }
      
      /* Data Table Styling */
      .dataTables_wrapper {
        font-family: 'Inter', sans-serif;
      }
      
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        color: #666;
        font-weight: 500;
      }
      
      /* Alert Boxes */
      .alert {
        padding: 20px;
        border-radius: 15px;
        margin-bottom: 30px;
        border: 2px solid;
        font-weight: 500;
      }
      
      .alert-warning {
        background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);
        border-color: #f9a826;
        color: #856404;
      }
      
      .alert-info {
        background: linear-gradient(135deg, #d1ecf1 0%, #bee5eb 100%);
        border-color: #4895ef;
        color: #0c5460;
      }
      
      /* Data Source Info */
      .data-source-info {
        background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%);
        border: 2px solid #2a9d8f;
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 30px;
      }
      
      /* Responsive Design */
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
      
      /* Fix actionLink styling for navigation */
      .nav-links .shiny-bound-input {
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
      }
      .nav-links .shiny-bound-input:hover {
        background: rgba(255,255,255,0.1);
        transform: translateY(-2px);
      }
    "))
  ),
  
  useShinyjs(),
  
  # Navigation Bar
  div(class = "navbar",
      div(class = "navbar-container",
          a(href = "#", class = "logo",
            tags$i(class = "fas fa-thermometer-half"),
            "Climate Pulse"
          ),
          div(class = "nav-links",
              actionLink("nav_home", 
                         div(class = "nav-link",
                             tags$i(class = "fas fa-home"),
                             "Home"
                         )
              ),
              actionLink("nav_trends", 
                         div(class = "nav-link",
                             tags$i(class = "fas fa-chart-line"),
                             "Trends"
                         )
              ),
              actionLink("nav_map", 
                         div(class = "nav-link",
                             tags$i(class = "fas fa-map-marked-alt"),
                             "Map"
                         )
              ),
              actionLink("nav_predictions", 
                         div(class = "nav-link",
                             tags$i(class = "fas fa-chart-area"),
                             "Predictions"
                         )
              ),
              actionLink("nav_about", 
                         div(class = "nav-link",
                             tags$i(class = "fas fa-info-circle"),
                             "About"
                         )
              )
          )
      )
  ),
  
  # Main Content Container
  div(class = "container",
      
      # Data Source Information
      div(class = "data-source-info",
          tags$strong("📊 Data Source: "), 
          "Temperature data loaded from Suhu_prepocessed.xlsx | ",
          textOutput("data_info", inline = TRUE)
      ),
      
      # Tab Content
      div(id = "tab_content",
          
          # HOME TAB
          div(id = "home_content",
              
              # Hero Section
              div(class = "hero",
                  div(class = "hero-content",
                      h1(class = "hero-title", "Indonesia Climate Monitor"),
                      p(class = "hero-description", 
                        "Real-time temperature analysis dashboard powered by comprehensive climate data."),
                      p(class = "hero-description",
                        "Explore temperature trends, regional variations, and climate projections across Indonesian provinces."),
                      div(class = "hero-buttons",
                          actionButton("explore_data", "EXPLORE DATA", class = "btn btn-primary"),
                          actionButton("learn_more", "LEARN MORE", class = "btn btn-secondary")
                      )
                  ),
                  div(class = "hero-map",
                      leafletOutput("preview_map", height = "350px")
                  )
              ),
              
              # Stats Cards
              div(class = "stats-container",
                  div(class = "stat-card stat-card-red",
                      div(class = "stat-value", textOutput("highest_temp")),
                      div(class = "stat-label", "Highest Current Temperature"),
                      tags$i(class = "fas fa-temperature-high stat-icon")
                  ),
                  div(class = "stat-card stat-card-orange",
                      div(class = "stat-value", textOutput("avg_temp")),
                      div(class = "stat-label", "National Average Temperature"),
                      tags$i(class = "fas fa-globe-asia stat-icon")
                  ),
                  div(class = "stat-card stat-card-blue",
                      div(class = "stat-value", textOutput("temp_anomaly")),
                      div(class = "stat-label", "Average Temperature Anomaly"),
                      tags$i(class = "fas fa-exclamation-triangle stat-icon")
                  ),
                  div(class = "stat-card stat-card-purple",
                      div(class = "stat-value", textOutput("data_coverage")),
                      div(class = "stat-label", "Provinces with Data"),
                      tags$i(class = "fas fa-chart-line stat-icon")
                  )
              ),
              
              # Featured Insights
              h2(class = "section-title", "Temperature Analysis"),
              div(class = "insights-container",
                  div(class = "insight-card",
                      div(class = "insight-header",
                          div(class = "insight-icon insight-icon-red",
                              tags$i(class = "fas fa-chart-line")
                          ),
                          div(class = "insight-title", "National Trends")
                      ),
                      div(class = "insight-content",
                          plotlyOutput("national_trends_chart", height = "250px")
                      )
                  ),
                  div(class = "insight-card",
                      div(class = "insight-header",
                          div(class = "insight-icon insight-icon-blue",
                              tags$i(class = "fas fa-map-marked-alt")
                          ),
                          div(class = "insight-title", "Regional Comparison")
                      ),
                      div(class = "insight-content",
                          plotlyOutput("regional_comparison_chart", height = "250px")
                      )
                  ),
                  div(class = "insight-card",
                      div(class = "insight-header",
                          div(class = "insight-icon insight-icon-green",
                              tags$i(class = "fas fa-calendar-alt")
                          ),
                          div(class = "insight-title", "Seasonal Patterns")
                      ),
                      div(class = "insight-content",
                          plotlyOutput("seasonal_patterns_chart", height = "250px")
                      )
                  )
              ),
              
              # Additional Analysis
              div(class = "additional-content",
                  h2(class = "section-title", "Temperature Distribution Analysis"),
                  div(class = "chart-container",
                      plotlyOutput("temperature_distribution_chart", height = "400px")
                  )
              )
          ),
          
          # TRENDS TAB (Fixed for proper Shiny output display)
          div(id = "trends_content", style = "display: none;",
              div(class = "tab-header",
                  h1(class = "tab-title", "📈 Temperature Trends"),
                  p(class = "tab-description", "Historical temperature analysis based on your Excel data")
              ),
              
              div(class = "tab-content",
                  div(class = "chart-container",
                      plotlyOutput("detailed_trends_chart", height = "500px")
                  ),
                  
                  div(style = "margin-top: 30px;",
                      div(class = "content-grid",
                          div(class = "content-card",
                              h3(tags$i(class = "fas fa-chart-bar"), "Trend Statistics"),
                              # Changed from div(id = "trend_statistics") to:
                              htmlOutput("trend_statistics")
                          ),
                          div(class = "content-card",
                              h3(tags$i(class = "fas fa-thermometer-half"), "Temperature Extremes"),
                              # Changed from div(id = "temperature_extremes") to:
                              htmlOutput("temperature_extremes")
                          )
                      )
                  )
              )
          ),
          
          # MAP TAB (Simplified)
          div(id = "map_content", style = "display: none;",
              div(class = "tab-header",
                  h1(class = "tab-title", "🗺️ Temperature Map"),
                  p(class = "tab-description", "Interactive map showing temperature variations across provinces")
              ),
              
              # Control Panel
              div(class = "control-panel",
                  div(class = "control-row",
                      div(class = "form-group",
                          tags$label("Display Parameter"),
                          selectInput("map_parameter", NULL,
                                      choices = c("Current Temperature" = "current_temp",
                                                  "Historical Average" = "historical_avg",
                                                  "Temperature Anomaly" = "temp_anomaly",
                                                  "Climate Risk Score" = "climate_risk_score"),
                                      selected = "current_temp")
                      ),
                      div(class = "form-group",
                          tags$label("Update Map"),
                          actionButton("update_map", "Update Visualization", class = "btn btn-primary", style = "width: 100%; margin-top: 8px;")
                      )
                  )
              ),
              
              div(class = "chart-container",
                  leafletOutput("climate_map", height = "600px")
              )
          )
      ),
      
      # PREDICTIONS TAB (Simplified & Corrected)
      div(id = "predictions_content", style = "display: none;",
          
          # Header
          div(class = "tab-header",
              h1(class = "tab-title", "🔮 Temperature Forecasting"),
              p(class = "tab-description", "Monthly ARIMA-based temperature predictions using time series data from Excel file")
          ),
          
          # Control Panel
          div(class = "tab-content",
              div(class = "control-panel",
                  div(class = "control-row",
                      div(class = "form-group",
                          tags$label("Forecast Period (Months)"),
                          selectInput("forecast_months", NULL,
                                      choices = c("6 Months" = "6",
                                                  "12 Months (1 Year)" = "12",
                                                  "24 Months (2 Years)" = "24",
                                                  "36 Months (3 Years)" = "36",
                                                  "60 Months (5 Years)" = "60"),
                                      selected = "12")
                      ),
                      div(class = "form-group",
                          tags$label("Generate Forecast"),
                          actionButton("update_predictions", "Run ARIMA Model", class = "btn btn-primary", style = "width: 100%; margin-top: 8px;")
                      )
                  )
              ),
              
              # Plot Forecast
              div(class = "chart-container",
                  plotlyOutput("arima_forecast_chart", height = "500px")
              ),
              
              # Forecast Summary + Model Performance
              div(style = "margin-top: 30px;",
                  div(class = "content-grid",
                      
                      # Model Performance
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-cogs"), " Model Performance"),
                          uiOutput("model_performance")
                      ),
                      
                      # Forecast Summary
                      div(class = "content-card",
                          h3(tags$i(class = "fas fa-chart-line"), " Forecast Summary"),
                          uiOutput("forecast_summary")
                      )
                  )
              )
          )
      ),
      
      
      # ABOUT TAB
      div(id = "about_content", style = "display: none;",
          div(class = "tab-header",
              h1(class = "tab-title", "ℹ️ About This Dashboard"),
              p(class = "tab-description", "Information about data sources, methodology, and technical implementation")
          ),
          
          div(class = "tab-content",
              div(class = "content-grid",
                  div(class = "content-card",
                      h3(tags$i(class = "fas fa-database"), "Data Source"),
                      p("This dashboard uses temperature data from the Suhu_prepocessed.xlsx file containing:"),
                      tags$ul(
                        tags$li("Date: Time series data points"),
                        tags$li("Tavg: Average temperature measurements"),
                        tags$li("Province ID: Unique province identifiers"),
                        tags$li("Province Name: Indonesian province names")
                      ),
                      div(id = "data_summary_info")
                  ),
                  div(class = "content-card",
                      h3(tags$i(class = "fas fa-cogs"), "Technical Details"),
                      p("Built using R Shiny with the following key features:"),
                      tags$ul(
                        tags$li("Interactive temperature visualization"),
                        tags$li("ARIMA time series forecasting"),
                        tags$li("Geospatial mapping with Leaflet"),
                        tags$li("Real-time data processing"),
                        tags$li("Responsive web design")
                      )
                  )
              ),
              
              h2(class = "section-title", "Complete Dataset"),
              div(class = "chart-container",
                  DT::dataTableOutput("complete_data_table")
              )
          )
      )
  )
)

# =============================================
# SERVER LOGIC
# =============================================

server <- function(input, output, session) {
  
  # Track active tab
  active_tab <- reactiveVal("home")
  
  # Tab navigation handlers
  observeEvent(input$nav_home, {
    active_tab("home")
    shinyjs::hide("trends_content")
    shinyjs::hide("map_content")
    shinyjs::hide("predictions_content")
    shinyjs::hide("about_content")
    shinyjs::show("home_content")
  })
  
  observeEvent(input$nav_trends, {
    active_tab("trends")
    shinyjs::hide("home_content")
    shinyjs::hide("map_content")
    shinyjs::hide("predictions_content")
    shinyjs::hide("about_content")
    shinyjs::show("trends_content")
  })
  
  observeEvent(input$nav_map, {
    active_tab("map")
    shinyjs::hide("home_content")
    shinyjs::hide("trends_content")
    shinyjs::hide("predictions_content")
    shinyjs::hide("about_content")
    shinyjs::show("map_content")
  })
  
  observeEvent(input$nav_predictions, {
    active_tab("predictions")
    shinyjs::hide("home_content")
    shinyjs::hide("trends_content")
    shinyjs::hide("map_content")
    shinyjs::hide("about_content")
    shinyjs::show("predictions_content")
  })
  
  observeEvent(input$nav_about, {
    active_tab("about")
    shinyjs::hide("home_content")
    shinyjs::hide("trends_content")
    shinyjs::hide("map_content")
    shinyjs::hide("predictions_content")
    shinyjs::show("about_content")
  })
  
  observeEvent(input$explore_data, {
    active_tab("map")
    shinyjs::hide("home_content")
    shinyjs::hide("trends_content")
    shinyjs::hide("predictions_content")
    shinyjs::hide("about_content")
    shinyjs::show("map_content")
  })
  
  observeEvent(input$learn_more, {
    active_tab("about")
    shinyjs::hide("home_content")
    shinyjs::hide("trends_content")
    shinyjs::hide("map_content")
    shinyjs::hide("predictions_content")
    shinyjs::show("about_content")
  })
  
  # =============================================
  # HELPER FUNCTIONS
  # =============================================
  
  # Improved polygon matching function
  find_matching_polygon <- function(province_name, geojson_data) {
    if(is.null(geojson_data) || nrow(geojson_data) == 0) {
      return(NULL)
    }
    
    # Clean the province name for matching
    clean_province <- gsub("Daerah Istimewa |Daerah Khusus Ibukota |Provinsi ", "", province_name)
    clean_province <- trimws(clean_province)
    
    # Strategy 1: Direct exact match
    matches <- NULL
    if("name_clean" %in% names(geojson_data)) {
      exact_match <- geojson_data[geojson_data$name_clean == clean_province, ]
      if(nrow(exact_match) > 0) return(exact_match[1, ])
    }
    
    # Strategy 2: Case-insensitive match
    if("name_clean" %in% names(geojson_data)) {
      case_match <- geojson_data[tolower(geojson_data$name_clean) == tolower(clean_province), ]
      if(nrow(case_match) > 0) return(case_match[1, ])
    }
    
    # Strategy 3: Partial match
    if("name_clean" %in% names(geojson_data)) {
      partial_match <- geojson_data[grepl(clean_province, geojson_data$name_clean, ignore.case = TRUE), ]
      if(nrow(partial_match) > 0) return(partial_match[1, ])
    }
    
    # Strategy 4: Try other name columns
    name_columns <- c("NAME_1", "NAME", "PROVINSI", "name")
    for(col in name_columns) {
      if(col %in% names(geojson_data)) {
        match_result <- geojson_data[grepl(clean_province, geojson_data[[col]], ignore.case = TRUE), ]
        if(nrow(match_result) > 0) return(match_result[1, ])
      }
    }
    
    # Strategy 5: Word-based matching
    province_words <- tolower(unlist(strsplit(clean_province, "\\s+")))
    province_words <- province_words[nchar(province_words) > 2]  # Only meaningful words
    
    if(length(province_words) > 0 && "name_clean" %in% names(geojson_data)) {
      for(word in province_words) {
        word_match <- geojson_data[grepl(word, geojson_data$name_clean, ignore.case = TRUE), ]
        if(nrow(word_match) > 0) return(word_match[1, ])
      }
    }
    
    return(NULL)
  }
  
  # Function to safely calculate statistics
  safe_calc <- function(data, func, default = NA) {
    tryCatch({
      result <- func(data)
      if(is.infinite(result) || is.nan(result)) return(default)
      return(result)
    }, error = function(e) {
      return(default)
    })
  }
  
  # =============================================
  # DATA INFO OUTPUT
  # =============================================
  
  output$data_info <- renderText({
    if(exists("temp_data") && nrow(temp_data) > 0) {
      paste("Loaded", nrow(temp_data), "temperature records from", 
            length(unique(temp_data$province_name)), "provinces |",
            "Date range:", min(temp_data$date), "to", max(temp_data$date))
    } else {
      "No temperature data available"
    }
  })
  
  # =============================================
  # HOME TAB OUTPUTS
  # =============================================
  
  output$highest_temp <- renderText({
    if (exists("provinces_climate_data") && nrow(provinces_climate_data) > 0) {
      max_temp <- max(provinces_climate_data$current_temp, na.rm = TRUE)
      if (!is.na(max_temp)) paste0(max_temp, "°C") else "N/A"
    } else {
      "N/A"
    }
  })
  
  output$avg_temp <- renderText({
    if (exists("provinces_climate_data") && nrow(provinces_climate_data) > 0) {
      avg_temp <- mean(provinces_climate_data$current_temp, na.rm = TRUE)
      if (!is.na(avg_temp)) paste0(round(avg_temp, 1), "°C") else "N/A"
    } else {
      "N/A"
    }
  })
  
  output$temp_anomaly <- renderText({
    if (exists("provinces_climate_data") && nrow(provinces_climate_data) > 0) {
      avg_anomaly <- mean(provinces_climate_data$temp_anomaly, na.rm = TRUE)
      if (!is.na(avg_anomaly)) paste0(ifelse(avg_anomaly >= 0, "+", ""), round(avg_anomaly, 1), "°C") else "N/A"
    } else {
      "N/A"
    }
  })
  
  output$data_coverage <- renderText({
    if(exists("provinces_climate_data") && exists("temp_data")) {
      paste0(nrow(provinces_climate_data), "/", length(unique(temp_data$province_name)))
    } else {
      "N/A"
    }
  })
  
  # Preview Map
  output$preview_map <- renderLeaflet({
    if(!exists("provinces_climate_data") || nrow(provinces_climate_data) == 0) {
      return(leaflet() %>% 
               setView(lng = 118, lat = -2, zoom = 5) %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               addControl("No climate data available", position = "topright"))
    }
    
    # Create color palette
    temp_values <- provinces_climate_data$current_temp[!is.na(provinces_climate_data$current_temp)]
    if(length(temp_values) == 0) {
      return(leaflet() %>% 
               setView(lng = 118, lat = -2, zoom = 5) %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               addControl("No valid temperature data", position = "topright"))
    }
    
    color_pal <- colorNumeric(
      palette = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"),
      domain = temp_values,
      na.color = "transparent"
    )
    
    map <- leaflet() %>% 
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    # Add polygons with better error handling
    if(exists("indonesia_provinces") && !is.null(indonesia_provinces) && nrow(indonesia_provinces) > 0) {
      tryCatch({
        matched_count <- 0
        
        for(i in 1:nrow(provinces_climate_data)) {
          prov_data <- provinces_climate_data[i, ]
          
          # Skip if essential data is missing
          if(is.na(prov_data$current_temp)) next
          
          # Find matching polygon
          matching_polygon <- find_matching_polygon(prov_data$province_name, indonesia_provinces)
          
          if(!is.null(matching_polygon)) {
            matched_count <- matched_count + 1
            
            # Create popup content
            popup_content <- paste0(
              "<div style='font-family: Inter; padding: 10px; min-width: 200px;'>",
              "<h4 style='color: #e63946; margin-bottom: 10px;'>", prov_data$province_name, "</h4>",
              "<p><strong>Current Temperature:</strong> ", prov_data$current_temp, "°C</p>",
              "<p><strong>Historical Average:</strong> ", ifelse(is.na(prov_data$historical_avg), "N/A", paste0(prov_data$historical_avg, "°C")), "</p>",
              "<p><strong>Anomaly:</strong> ", 
              ifelse(is.na(prov_data$temp_anomaly), "N/A", 
                     paste0(ifelse(prov_data$temp_anomaly >= 0, "+", ""), prov_data$temp_anomaly, "°C")), "</p>",
              "</div>"
            )
            
            map <- map %>%
              addPolygons(
                data = matching_polygon,
                fillColor = color_pal(prov_data$current_temp),
                fillOpacity = 0.7,
                color = "#FFFFFF",
                weight = 1,
                opacity = 0.8,
                popup = popup_content,
                highlightOptions = highlightOptions(
                  weight = 2,
                  color = "#e63946",
                  fillOpacity = 0.9,
                  bringToFront = TRUE
                )
              )
          }
        }
        
        cat("Preview map: ", matched_count, " polygons added successfully\n")
        
      }, error = function(e) {
        cat("Error adding polygons to preview map:", e$message, "\n")
      })
    }
    
    return(map)
  })
  
  # National Trends Chart
  output$national_trends_chart <- renderPlotly({
    if(!exists("national_temp_trend") || nrow(national_temp_trend) == 0) {
      return(plotly_empty() %>% layout(title = "No national trend data available"))
    }
    
    # Filter out NA values
    trend_data <- national_temp_trend[!is.na(national_temp_trend$temperature), ]
    
    if(nrow(trend_data) == 0) {
      return(plotly_empty() %>% layout(title = "No valid temperature data for trends"))
    }
    
    plot_ly(trend_data, x = ~year, y = ~temperature, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#e63946', width = 3),
            marker = list(color = '#e63946', size = 6),
            hovertemplate = 'Year: %{x}<br>Temperature: %{y:.1f}°C<extra></extra>') %>%
      layout(
        xaxis = list(title = "Year", titlefont = list(size = 12)),
        yaxis = list(title = "Temperature (°C)", titlefont = list(size = 12)),
        margin = list(l = 50, r = 20, t = 20, b = 50),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(size = 11)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Regional Comparison Chart
  output$regional_comparison_chart <- renderPlotly({
    if(!exists("provinces_climate_data") || nrow(provinces_climate_data) == 0) {
      return(plotly_empty() %>% layout(title = "No regional data available"))
    }
    
    regional_data <- provinces_climate_data %>%
      filter(!is.na(current_temp) & !is.na(historical_avg)) %>%
      group_by(region) %>%
      summarise(
        current_avg = mean(current_temp, na.rm = TRUE),
        historical_avg = mean(historical_avg, na.rm = TRUE),
        province_count = n(),
        .groups = 'drop'
      ) %>%
      filter(province_count > 0)
    
    if(nrow(regional_data) == 0) {
      return(plotly_empty() %>% layout(title = "No valid regional data"))
    }
    
    plot_ly(regional_data, x = ~region, y = ~current_avg, type = 'bar', name = 'Current',
            marker = list(color = '#e63946'),
            hovertemplate = 'Region: %{x}<br>Current: %{y:.1f}°C<extra></extra>') %>%
      add_trace(y = ~historical_avg, name = 'Historical', marker = list(color = '#4895ef'),
                hovertemplate = 'Region: %{x}<br>Historical: %{y:.1f}°C<extra></extra>') %>%
      layout(
        xaxis = list(title = "Region", titlefont = list(size = 12)),
        yaxis = list(title = "Temperature (°C)", titlefont = list(size = 12)),
        barmode = 'group',
        margin = list(l = 50, r = 20, t = 20, b = 80),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(size = 11),
        showlegend = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Seasonal Patterns Chart
  output$seasonal_patterns_chart <- renderPlotly({
    if(!exists("monthly_data") || nrow(monthly_data) == 0) {
      return(plotly_empty() %>% layout(title = "No seasonal data available"))
    }
    
    # Filter out NA values
    seasonal_data <- monthly_data[!is.na(monthly_data$avg_temp), ]
    
    if(nrow(seasonal_data) == 0) {
      return(plotly_empty() %>% layout(title = "No valid seasonal data"))
    }
    
    # Ensure proper month ordering
    month_levels <- c("January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December")
    
    # Convert month_name to factor with proper ordering
    seasonal_data$month_name <- factor(seasonal_data$month_name, levels = month_levels)
    
    # Sort data by month order
    seasonal_data <- seasonal_data[order(seasonal_data$month_name), ]
    
    # Remove any rows with missing month_name
    seasonal_data <- seasonal_data[!is.na(seasonal_data$month_name), ]
    
    # Create the plot
    plot_ly(seasonal_data, 
            x = ~month_name, 
            y = ~avg_temp, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(color = '#2a9d8f', width = 3, shape = 'linear'),
            marker = list(color = '#2a9d8f', size = 8, opacity = 0.8),
            hovertemplate = '<b>%{x}</b><br>Temperature: %{y:.1f}°C<extra></extra>') %>%
      layout(
        title = list(
          text = "Monthly Temperature Patterns",
          font = list(size = 16, color = '#2c3e50'),
          x = 0.5,
          xanchor = 'center'
        ),
        xaxis = list(
          title = list(text = "Month", font = list(size = 14, color = '#34495e')),
          tickangle = -45,
          tickfont = list(size = 12),
          showgrid = TRUE,
          gridcolor = 'rgba(0,0,0,0.1)',
          linecolor = 'rgba(0,0,0,0.3)',
          categoryorder = "array",
          categoryarray = month_levels
        ),
        yaxis = list(
          title = list(text = "Temperature (°C)", font = list(size = 14, color = '#34495e')),
          tickfont = list(size = 12),
          showgrid = TRUE,
          gridcolor = 'rgba(0,0,0,0.1)',
          linecolor = 'rgba(0,0,0,0.3)',
          zeroline = FALSE
        ),
        margin = list(l = 70, r = 30, t = 60, b = 100),
        plot_bgcolor = 'rgba(248,249,250,0.8)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(size = 11, color = '#2c3e50'),
        hovermode = 'x unified',
        showlegend = FALSE
      ) %>%
      config(
        displayModeBar = FALSE,
        staticPlot = FALSE,
        responsive = TRUE
      )
  })
  
  # Temperature Distribution Chart
  output$temperature_distribution_chart <- renderPlotly({
    if(!exists("provinces_climate_data") || nrow(provinces_climate_data) == 0) {
      return(plotly_empty() %>% layout(title = "No temperature data available"))
    }
    
    # Filter out NA values
    temp_data_clean <- provinces_climate_data$current_temp[!is.na(provinces_climate_data$current_temp)]
    
    if(length(temp_data_clean) == 0) {
      return(plotly_empty() %>% layout(title = "No valid temperature data"))
    }
    
    # Calculate statistics for enhanced display
    temp_mean <- mean(temp_data_clean)
    temp_median <- median(temp_data_clean)
    temp_range <- range(temp_data_clean)
    
    # Calculate histogram data for reference lines
    hist_data <- hist(temp_data_clean, breaks = 20, plot = FALSE)
    max_count <- max(hist_data$counts)
    
    # Create the base plot
    p <- plot_ly() %>%
      
      # Add histogram
      add_histogram(
        x = temp_data_clean,
        nbinsx = 20,
        marker = list(
          color = '#e63946',
          opacity = 0.8,
          line = list(color = '#c41e3a', width = 1.5)
        ),
        hovertemplate = '<b>Temperature Range:</b> %{x:.1f}°C<br><b>Count:</b> %{y} provinces<extra></extra>',
        name = 'Temperature Distribution'
      ) %>%
      
      # Add mean line
      add_segments(
        x = temp_mean, xend = temp_mean,
        y = 0, yend = max_count * 1.1,
        line = list(color = '#2a9d8f', width = 3, dash = 'dash'),
        name = paste0('Mean: ', round(temp_mean, 1), '°C'),
        hovertemplate = '<b>Mean Temperature:</b> %{x:.1f}°C<extra></extra>',
        showlegend = TRUE
      ) %>%
      
      # Add median line
      add_segments(
        x = temp_median, xend = temp_median,
        y = 0, yend = max_count * 1.1,
        line = list(color = '#f4a261', width = 3, dash = 'dot'),
        name = paste0('Median: ', round(temp_median, 1), '°C'),
        hovertemplate = '<b>Median Temperature:</b> %{x:.1f}°C<extra></extra>',
        showlegend = TRUE
      )
    
    p %>%
      
      layout(
        title = list(
          text = "Temperature Distribution Across Indonesian Provinces",
          font = list(size = 18, color = '#2c3e50', family = 'Arial'),
          x = 0.5,
          xanchor = 'center',
          pad = list(t = 20)
        ),
        
        xaxis = list(
          title = list(
            text = "Temperature (°C)",
            font = list(size = 14, color = '#34495e', family = 'Arial')
          ),
          tickfont = list(size = 12, color = '#34495e'),
          showgrid = TRUE,
          gridcolor = 'rgba(0,0,0,0.1)',
          linecolor = 'rgba(0,0,0,0.3)',
          zeroline = FALSE,
          range = c(temp_range[1] - 1, temp_range[2] + 1)
        ),
        
        yaxis = list(
          title = list(
            text = "Number of Provinces",
            font = list(size = 14, color = '#34495e', family = 'Arial')
          ),
          tickfont = list(size = 12, color = '#34495e'),
          showgrid = TRUE,
          gridcolor = 'rgba(0,0,0,0.1)',
          linecolor = 'rgba(0,0,0,0.3)',
          zeroline = FALSE
        ),
        
        # Enhanced layout styling
        plot_bgcolor = 'rgba(248,249,250,0.8)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(size = 11, color = '#2c3e50', family = 'Arial'),
        
        margin = list(l = 80, r = 40, t = 80, b = 60),
        
        # Legend styling
        legend = list(
          x = 0.98,
          y = 0.98,
          xanchor = 'right',
          yanchor = 'top',
          bgcolor = 'rgba(255,255,255,0.9)',
          bordercolor = 'rgba(0,0,0,0.2)',
          borderwidth = 1,
          font = list(size = 11)
        ),
        
        # Hover mode
        hovermode = 'x unified',
        
        # Add annotation with statistics
        annotations = list(
          list(
            x = 0.02,
            y = 0.98,
            xref = 'paper',
            yref = 'paper',
            text = paste0(
              '<b>Statistics:</b><br>',
              'Total Provinces: ', length(temp_data_clean), '<br>',
              'Min: ', round(temp_range[1], 1), '°C<br>',
              'Max: ', round(temp_range[2], 1), '°C<br>',
              'Range: ', round(temp_range[2] - temp_range[1], 1), '°C'
            ),
            showarrow = FALSE,
            bgcolor = 'rgba(255,255,255,0.9)',
            bordercolor = 'rgba(0,0,0,0.2)',
            borderwidth = 1,
            font = list(size = 10, color = '#2c3e50'),
            align = 'left',
            xanchor = 'left',
            yanchor = 'top'
          )
        )
      ) %>%
      
      config(
        displayModeBar = FALSE,
        staticPlot = FALSE,
        responsive = TRUE
      )
  })
  # =============================================
  # TRENDS TAB
  # =============================================
  
  output$detailed_trends_chart <- renderPlotly({
    if(!exists("national_temp_trend") || nrow(national_temp_trend) == 0) {
      return(plotly_empty() %>% layout(title = "No national trend data available"))
    }
    
    # Filter out NA values
    trend_data <- national_temp_trend[!is.na(national_temp_trend$temperature), ]
    
    if(nrow(trend_data) < 2) {
      return(plotly_empty() %>% layout(title = "Insufficient data for trend analysis"))
    }
    
    tryCatch({
      # Add trend line
      lm_model <- lm(temperature ~ year, data = trend_data)
      trend_line <- predict(lm_model, newdata = trend_data)
      
      plot_ly(trend_data, x = ~year, y = ~temperature, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#e63946', width = 3), marker = list(color = '#e63946', size = 8),
              name = 'Observed Temperature', hovertemplate = 'Year: %{x}<br>Temperature: %{y:.2f}°C<extra></extra>') %>%
        add_trace(x = trend_data$year, y = trend_line, type = 'scatter', mode = 'lines',
                  line = list(color = '#2a9d8f', width = 2, dash = 'dash'),
                  name = 'Linear Trend', hovertemplate = 'Trend: %{y:.2f}°C<extra></extra>') %>%
        layout(
          title = list(text = "National Temperature Trends Over Time", font = list(size = 20)),
          xaxis = list(title = "Year", titlefont = list(size = 16)),
          yaxis = list(title = "Temperature (°C)", titlefont = list(size = 16)),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          legend = list(orientation = "h", x = 0.3, y = -0.2)
        ) %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      cat("Error in detailed trends chart:", e$message, "\n")
      return(plotly_empty() %>% layout(title = "Error generating trend chart"))
    })
  })
  
  # Trend Statistics
  output$trend_statistics <- renderUI({
    if(!exists("national_temp_trend") || nrow(national_temp_trend) == 0) {
      return(p("No trend data available"))
    }
    
    trend_data <- national_temp_trend[!is.na(national_temp_trend$temperature), ]
    
    if(nrow(trend_data) < 2) {
      return(p("Insufficient data for trend statistics"))
    }
    
    tryCatch({
      lm_model <- lm(temperature ~ year, data = trend_data)
      warming_rate <- round(coef(lm_model)[2], 4)
      r_squared <- round(summary(lm_model)$r.squared, 3)
      
      tagList(
        div(style = "margin-bottom: 15px;",
            tags$strong("Data Period: "), paste(min(trend_data$year), "-", max(trend_data$year))
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Average Temperature: "), paste0(round(mean(trend_data$temperature), 2), "°C")
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Warming Rate: "), paste0(warming_rate, "°C/year")
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Trend Strength (R²): "), r_squared
        ),
        div(style = "margin-bottom: 15px;",
            tags$strong("Total Change: "), 
            paste0(round(warming_rate * (max(trend_data$year) - min(trend_data$year)), 2), "°C")
        )
      )
    }, error = function(e) {
      p("Error calculating trend statistics")
    })
  })
  
  # Temperature Extremes
  output$temperature_extremes <- renderUI({
    if(!exists("provinces_climate_data") || nrow(provinces_climate_data) == 0) {
      return(p("No temperature data available"))
    }
    
    valid_temps <- provinces_climate_data$current_temp[!is.na(provinces_climate_data$current_temp)]
    valid_anomalies <- provinces_climate_data$temp_anomaly[!is.na(provinces_climate_data$temp_anomaly)]
    
    if(length(valid_temps) == 0) {
      return(p("No valid temperature data"))
    }
    
    tagList(
      div(style = "margin-bottom: 15px;",
          tags$strong("Highest Province Temperature: "), 
          paste0(max(valid_temps), "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Lowest Province Temperature: "), 
          paste0(min(valid_temps), "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Temperature Range: "), 
          paste0(round(max(valid_temps) - min(valid_temps), 1), "°C")
      ),
      div(style = "margin-bottom: 15px;",
          tags$strong("Largest Anomaly: "), 
          if(length(valid_anomalies) > 0) {
            paste0("+", max(valid_anomalies), "°C")
          } else {
            "N/A"
          }
      )
    )
  })
  
  # =============================================
  # MAP TAB
  # =============================================
  
  output$climate_map <- renderLeaflet({
    req(input$update_map)
    
    isolate({
      if(!exists("provinces_climate_data") || nrow(provinces_climate_data) == 0) {
        return(leaflet() %>% 
                 setView(lng = 118, lat = -2, zoom = 5) %>%
                 addProviderTiles(providers$CartoDB.Positron) %>%
                 addControl("No climate data available", position = "topright"))
      }
      
      # Get parameter selection
      param_column <- input$map_parameter
      if(is.null(param_column) || !param_column %in% names(provinces_climate_data)) {
        param_column <- "current_temp"
      }
      
      # Filter data
      filtered_data <- provinces_climate_data[!is.na(provinces_climate_data[[param_column]]), ]
      
      if(nrow(filtered_data) == 0) {
        return(leaflet() %>% 
                 setView(lng = 118, lat = -2, zoom = 5) %>%
                 addProviderTiles(providers$CartoDB.Positron) %>%
                 addControl("No valid data for selected parameter", position = "topright"))
      }
      
      # Create color palette
      param_values <- filtered_data[[param_column]]
      colors <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
      color_pal <- colorNumeric(
        palette = colors,
        domain = param_values,
        na.color = "transparent"
      )
      
      map <- leaflet() %>% 
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      # Add polygons
      if(exists("indonesia_provinces") && !is.null(indonesia_provinces) && nrow(indonesia_provinces) > 0) {
        tryCatch({
          matched_count <- 0
          unmatched_provinces <- character()
          
          for(i in 1:nrow(filtered_data)) {
            prov_data <- filtered_data[i, ]
            
            # Find matching polygon
            matching_polygon <- find_matching_polygon(prov_data$province_name, indonesia_provinces)
            
            if(!is.null(matching_polygon)) {
              matched_count <- matched_count + 1
              
              # Create popup content
              popup_content <- paste0(
                "<div style='font-family: Inter; padding: 15px; min-width: 200px;'>",
                "<h4 style='color: #e63946; margin-bottom: 15px; border-bottom: 2px solid #e63946; padding-bottom: 5px;'>", 
                prov_data$province_name, "</h4>",
                "<p style='margin: 8px 0;'><strong>Current Temperature:</strong> ", 
                ifelse(is.na(prov_data$current_temp), "N/A", paste0(prov_data$current_temp, "°C")), "</p>",
                "<p style='margin: 8px 0;'><strong>Historical Average:</strong> ", 
                ifelse(is.na(prov_data$historical_avg), "N/A", paste0(prov_data$historical_avg, "°C")), "</p>",
                "<p style='margin: 8px 0;'><strong>Temperature Anomaly:</strong> ", 
                ifelse(is.na(prov_data$temp_anomaly), "N/A", 
                       paste0(ifelse(prov_data$temp_anomaly >= 0, "+", ""), prov_data$temp_anomaly, "°C")), "</p>",
                "<p style='margin: 8px 0;'><strong>Climate Risk Score:</strong> ", 
                ifelse(is.na(prov_data$climate_risk_score), "N/A", paste0(prov_data$climate_risk_score, "/10")), "</p>",
                "<p style='margin: 8px 0;'><strong>Data Years:</strong> ", 
                ifelse(is.na(prov_data$data_years), "N/A", paste0(prov_data$data_years, " years")), "</p>",
                "<p style='margin: 8px 0;'><strong>Region:</strong> ", 
                ifelse(is.na(prov_data$region), "N/A", prov_data$region), "</p>",
                "</div>"
              )
              
              map <- map %>%
                addPolygons(
                  data = matching_polygon,
                  fillColor = color_pal(prov_data[[param_column]]),
                  fillOpacity = 0.8,
                  color = "#FFFFFF",
                  weight = 2,
                  opacity = 0.8,
                  popup = popup_content,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "#e63946",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                  )
                )
            } else {
              unmatched_provinces <- c(unmatched_provinces, prov_data$province_name)
            }
          }
          
          # Log results
          cat("Climate map: ", matched_count, "/", nrow(filtered_data), " provinces matched\n")
          if(length(unmatched_provinces) > 0) {
            cat("Unmatched provinces:", paste(unmatched_provinces, collapse = ", "), "\n")
          }
          
        }, error = function(e) {
          cat("Error adding polygons to climate map:", e$message, "\n")
        })
      }
      
      # Add legend
      legend_title <- switch(param_column,
                             "current_temp" = "Current Temp (°C)",
                             "temp_anomaly" = "Anomaly (°C)",
                             "historical_avg" = "Historical Avg (°C)",
                             "climate_risk_score" = "Risk Score",
                             "data_years" = "Data Years",
                             "Default Parameter")
      
      map <- map %>%
        addLegend(
          pal = color_pal,
          values = param_values,
          title = legend_title,
          position = "bottomright",
          opacity = 0.8
        )
      
      return(map)
    })
  })
  
  # =============================================
  # PREDICTIONS TAB
  # =============================================
  
  # Plot ARIMA Forecast
  output$arima_forecast_chart <- renderPlotly({
    req(input$update_predictions)
    
    isolate({
      if (!is.null(monthly_temp_trend) && nrow(monthly_temp_trend) > 12) {
        forecast_months <- as.numeric(input$forecast_months)
        if (is.na(forecast_months) || forecast_months <= 0) {
          return(plotly_empty() %>% layout(title = "Invalid forecast period"))
        }
        
        # Format date jika belum
        if (!inherits(monthly_temp_trend$year_month, "Date")) {
          monthly_temp_trend$year_month <- as.Date(paste0(monthly_temp_trend$year, "-", monthly_temp_trend$month, "-01"))
        }
        
        # Imputasi NA pada suhu
        monthly_temp_trend$temperature <- ifelse(
          is.na(monthly_temp_trend$temperature),
          mean(monthly_temp_trend$temperature, na.rm = TRUE),
          monthly_temp_trend$temperature
        )
        
        # Penentuan start year & month
        start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
        valid_rows <- monthly_temp_trend$year == start_year & !is.na(monthly_temp_trend$month)
        if (any(valid_rows)) {
          start_month <- min(monthly_temp_trend$month[valid_rows], na.rm = TRUE)
        } else {
          start_month <- 1  # fallback
        }
        
        if (!is.na(start_year) && !is.na(start_month)) {
          ts_data <- ts(monthly_temp_trend$temperature,
                        start = c(start_year, start_month),
                        frequency = 12)
          
          # Fit ARIMA
          arima_model <- auto.arima(ts_data)
          forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
          
          # Tanggal
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
              name = 'Historical Data',
              hovertemplate = 'Date: %{x}<br>Temperature: %{y:.2f}°C<extra></extra>'
            ) %>%
            add_trace(
              x = forecast_dates,
              y = as.numeric(forecast_result$mean),
              type = 'scatter',
              mode = 'lines',
              line = list(color = '#2a9d8f', width = 3),
              name = 'ARIMA Forecast',
              hovertemplate = 'Date: %{x}<br>Forecast: %{y:.2f}°C<extra></extra>'
            ) %>%
            add_trace(
              x = c(forecast_dates, rev(forecast_dates)),
              y = c(as.numeric(forecast_result$lower[, 1]), rev(as.numeric(forecast_result$upper[, 1]))),
              type = 'scatter',
              mode = 'lines',
              fill = 'toself',
              fillcolor = 'rgba(42, 157, 143, 0.2)',
              line = list(color = 'rgba(42, 157, 143, 0)', width = 0),
              name = '95% Confidence Interval',
              hoverinfo = 'skip'
            ) %>%
            layout(
              title = list(text = paste("Monthly ARIMA Temperature Forecast -", forecast_months, "Months"), font = list(size = 20)),
              xaxis = list(title = "Date", titlefont = list(size = 16)),
              yaxis = list(title = "Temperature (°C)", titlefont = list(size = 16)),
              plot_bgcolor = 'rgba(0,0,0,0)',
              paper_bgcolor = 'rgba(0,0,0,0)',
              legend = list(orientation = "h", x = 0.1, y = -0.2)
            ) %>%
            config(displayModeBar = FALSE)
          
        } else {
          plotly_empty() %>% layout(title = "Invalid start date for ARIMA model")
        }
      } else {
        plotly_empty() %>% layout(title = "Insufficient monthly data for ARIMA forecasting")
      }
    })
  })
  
  output$model_performance <- renderUI({
    req(input$update_predictions)
    
    isolate({
      if (!is.null(monthly_temp_trend) && nrow(monthly_temp_trend) > 12) {
        
        # Imputasi nilai NA
        monthly_temp_trend$temperature <- ifelse(
          is.na(monthly_temp_trend$temperature),
          mean(monthly_temp_trend$temperature, na.rm = TRUE),
          monthly_temp_trend$temperature
        )
        
        start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
        valid_rows <- monthly_temp_trend$year == start_year & !is.na(monthly_temp_trend$month)
        
        if (any(valid_rows)) {
          start_month <- min(monthly_temp_trend$month[valid_rows], na.rm = TRUE)
        } else {
          start_month <- 1
        }
        
        if (!is.na(start_year) && !is.na(start_month)) {
          ts_data <- ts(monthly_temp_trend$temperature, start = c(start_year, start_month), frequency = 12)
          arima_model <- auto.arima(ts_data)
          
          residuals_data <- residuals(arima_model)
          mae <- round(mean(abs(residuals_data), na.rm = TRUE), 4)
          rmse <- round(sqrt(mean(residuals_data^2, na.rm = TRUE)), 4)
          mape <- round(mean(abs(residuals_data / ts_data) * 100, na.rm = TRUE), 2)
          
          tagList(
            div(style = "margin-bottom: 15px;",
                tags$strong("Model Type: "), paste0("ARIMA(", paste(arimaorder(arima_model), collapse = ","), ")")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("Data Frequency: "), "Monthly"
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("MAE: "), paste0(mae, "°C")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("RMSE: "), paste0(rmse, "°C")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("MAPE: "), paste0(mape, "%")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("AIC: "), round(AIC(arima_model), 2)
            )
          )
        } else {
          p("Start date invalid: Unable to fit ARIMA model")
        }
      } else {
        p("Insufficient monthly data for model performance calculation")
      }
    })
  })
  
  output$forecast_summary <- renderUI({
    req(input$update_predictions)
    
    isolate({
      if (!is.null(monthly_temp_trend) && nrow(monthly_temp_trend) > 12) {
        forecast_months <- as.numeric(input$forecast_months)
        
        # Imputasi NA
        monthly_temp_trend$temperature <- ifelse(
          is.na(monthly_temp_trend$temperature),
          mean(monthly_temp_trend$temperature, na.rm = TRUE),
          monthly_temp_trend$temperature
        )
        
        start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
        valid_rows <- monthly_temp_trend$year == start_year & !is.na(monthly_temp_trend$month)
        
        if (any(valid_rows)) {
          start_month <- min(monthly_temp_trend$month[valid_rows], na.rm = TRUE)
        } else {
          start_month <- 1
        }
        
        if (!is.na(start_year) && !is.na(start_month)) {
          ts_data <- ts(monthly_temp_trend$temperature,
                        start = c(start_year, start_month),
                        frequency = 12)
          
          arima_model <- auto.arima(ts_data)
          forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
          
          final_forecast <- round(as.numeric(forecast_result$mean[forecast_months]), 2)
          current_temp <- round(tail(monthly_temp_trend$temperature, 1), 2)
          temp_change <- round(final_forecast - current_temp, 2)
          
          # Format tanggal akhir
          last_date <- max(monthly_temp_trend$year_month, na.rm = TRUE)
          forecast_end_date <- last_date %m+% months(forecast_months)
          
          tagList(
            div(style = "margin-bottom: 15px;",
                tags$strong("Forecast Horizon: "), paste(forecast_months, "months")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("Forecast Period: "), paste("Until", format(forecast_end_date, "%B %Y"))
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("Current Temperature: "), paste0(current_temp, "°C")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("Projected Temperature: "), paste0(final_forecast, "°C")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("Expected Change: "), paste0(ifelse(temp_change >= 0, "+", ""), temp_change, "°C")
            ),
            div(style = "margin-bottom: 15px;",
                tags$strong("Monthly Change Rate: "), paste0(round(temp_change / forecast_months, 4), "°C/month")
            )
          )
        } else {
          p("Start date invalid: Unable to summarize forecast")
        }
      } else {
        p("Insufficient monthly data for forecast summary")
      }
    })
  })
  
  # Complete Dataset Table
  output$complete_data_table <- DT::renderDataTable({
    display_data <- provinces_climate_data %>%
      select(
        Province = province_name,
        `Province ID` = province_id,
        Region = region,
        `Current Temp (°C)` = current_temp,
        `Historical Avg (°C)` = historical_avg,
        `Temp Anomaly (°C)` = temp_anomaly,
        `Climate Risk Score` = climate_risk_score,
        `Data Years` = data_years,
        `First Year` = first_year,
        `Last Year` = last_year
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = 3:9)
        )
      ),
      extensions = 'Buttons',
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
# RUN APPLICATION
# =============================================

shinyApp(ui = ui, server = server)