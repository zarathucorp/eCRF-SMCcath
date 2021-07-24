library(shiny)
library(dplyr)
library(echarts4r)

patientsNumber_plot_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 6,
      echarts4rOutput("patientsNumber")
    )
  )
}

patientsNumber_plot_module <- function(input, output, session) {
  patinetsNumber <- 0
  
  tryCatch(
    {
      patinetsNumber <- conn %>%
                        tbl('pros') %>%
                        collect() %>% count(pid)
    },
    error = function(err) {
      msg <- "Database Connection Error"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    }
  )
  
  output$patientsNumber <- renderEcharts4r({
    patientsNumber |> 
      e_charts(x) |>
      e_bar(y, name ="Hospital 1") |>
      e_title("Patients Number")
  })
}