library(shiny)
library(dplyr)
library(echarts4r)

patientsNumber_plot_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 6,
      echarts4rOutput(ns("patientsNumber"))
    )
  )
}

patientsNumber_plot_module <- function(input, output, session) {
  hospital <- c("Samsung", "Asan")
  patients <- c(20, 70)
  patientsNumber <- data.frame(hospital, patients)
  print(patientsNumber)
  
  output$patientsNumber <- renderEcharts4r({
    patientsNumber %>%
      e_charts(hospital) %>%
      e_bar(patients) %>%
      e_title("Patients Number")
  })
}