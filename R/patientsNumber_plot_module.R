patientsNumber_plot_module <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 6,
      echarts4rOutput("patientNumber")
    )
  )
}
