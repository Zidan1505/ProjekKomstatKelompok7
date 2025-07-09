# =============================================
# INDONESIA CLIMATE PULSE DASHBOARD - UI
# =============================================
ui <- fluidPage(
  theme = NULL,
  
  # Sumber CSS
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  useShinyjs(),

  # Navbar
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
  
  # Konten Utama
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
                      h1(class = "hero-title", "Monitor Iklim Indonesia"),
                      p(class = "hero-description",
                        "Dashboard analisis suhu real-time berdasarkan data iklim komprehensif."),
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
                  p(class = "tab-description", "Analisis suhu historis berdasarkan data Excel")
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
                  p(class = "tab-description", "Peta interaktif menunjukkan variasi suhu antar provinsi")
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
              
              div(style = "margin-top: 30px;",
                  h2(class = "section-title", "Data Provinsi"),
                  div(class = "chart-container",
                      DT::dataTableOutput("tabel_provinsi")
                  )
              )
          ),
          
          # PREDIKSI TAB
          div(id = "prediksi_content", style = "display: none;",
              
              div(class = "tab-header",
                  h1(class = "tab-title", "🔮 Prediksi Suhu"),
                  p(class = "tab-description", "Prediksi suhu bulanan berbasis ARIMA menggunakan data time series")
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
