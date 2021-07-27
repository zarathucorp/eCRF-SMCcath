library(shiny)
library(dplyr)
library(echarts4r)

patientsNumber_plot_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 6,
      echarts4rOutput(ns("patientsNumber")) %>%
      withSpinner()
    )
  )
}

patientsNumber_plot_module <- function(input, output, session, data) {
  print(data)
  output$patientsNumber <- renderEcharts4r({
    data %>%
    e_charts(hospital) %>%
    e_bar(patients) %>%
    e_title("Patients Number")
  })
}