date_select_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    selectInput(ns("month"), "Month", c(1:12)),
    selectInput(ns("year"), "Year", c(2020:2025))
  )
}

date_select_module <- function(input, output, session) {
  return (
    list(
      month = reactive({input$month}),
      year = reactive({input$year})
    )
  )
}