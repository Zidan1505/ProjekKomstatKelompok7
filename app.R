# =============================================
# INDONESIA CLIMATE PULSE DASHBOARD - MAIN
# =============================================

# Data Source
setwd("C:/Tugas Zidan/Sem 4/Komstat/Projek/deploy")
source("global.R")
source("ui.R")
source("server.R")

# Jalankan Aplikasi
shinyApp(ui = ui, server = server)
