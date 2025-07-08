# =============================================
# INDONESIA CLIMATE PULSE DASHBOARD - SERVER
# Server Logic and Reactive Functions
# =============================================

server <- function(input, output, session) {
  
  # Track active tab
  active_tab <- reactiveVal("beranda")
  
  # Navigation handlers
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
  
  # Stats outputs with error handling
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
    if(exists("fallback_data") && nrow(fallback_data) > 0) {
      colors <- colorNumeric("Reds", fallback_data$current_temp, na.color = "#808080")
      
      leaflet(fallback_data) %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat,
          radius = ~sqrt(current_temp) * 2,
          fillColor = ~colors(current_temp),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.8,
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
  })
  
  # Charts
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
        xaxis = list(title = "Bulan", tickangle = -45),
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
  
  # Detailed trends chart
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
  
  # Trend statistics
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
  
  # Temperature extremes
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
        addControl("Peta Iklim Indonesia", position = "topright")
    } else {
      leaflet() %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addControl("Peta Iklim Indonesia - Data tidak tersedia", position = "topright")
    }
  })
  
  # Update map when parameter changes
  observeEvent(input$perbarui_peta, {
    output$peta_iklim <- renderLeaflet({
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
          addControl("Peta Iklim Indonesia - Diperbarui", position = "topright")
      }
    })
  })
  
  # Province data table
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
    
    # Create time series
    start_year <- min(monthly_temp_trend$year, na.rm = TRUE)
    start_month <- min(monthly_temp_trend$month[monthly_temp_trend$year == start_year], na.rm = TRUE)
    
    ts_data <- ts(monthly_temp_trend$temperature,
                  start = c(start_year, start_month),
                  frequency = 12)
    
    # Fit ARIMA and forecast
    arima_model <- auto.arima(ts_data)
    forecast_result <- forecast(arima_model, h = forecast_months, level = 95)
    
    # Create dates
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
  
  # Complete data table
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
