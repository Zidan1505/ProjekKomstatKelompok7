# =============================================
# INDONESIA CLIMATE PULSE DASHBOARD - MAIN
# Main Application File
# =============================================

# Source all required files
setwd("C:/Tugas Zidan/Sem 4/Komstat/Projek/deploy")
source("global.R")
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
