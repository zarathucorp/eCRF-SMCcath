date_select_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(2, selectInput(ns("month"), "Month", c(sprintf("%02d", 1:12)))),
    column(2, selectInput(ns("year"), "Year", c(2021:format(Sys.Date(), "%Y"))))
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