library(shiny)
library(dplyr)
library(echarts4r)

patientsNumber_plot_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      echarts4rOutput(ns("patientsNumber")) %>%
      withSpinner()
    )
  )
}

patientsNumber_plot_module <- function(input, output, session, data) {
  patientsPerHospital <- as.data.frame(table(data$Center))
  colnames(patientsPerHospital) <- c("hospital", "patients")
  
  output$patientsNumber <- renderEcharts4r({
    patientsPerHospital %>%
    e_charts(hospital) %>%
    e_bar(patients) %>%
    e_title("Patients Number")
  })
}