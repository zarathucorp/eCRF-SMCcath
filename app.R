source("global.R")
library(shinymanager)

# credentials <- data.frame(
#  user = c("admin", "chkh"),
#  password = c("zarathuadmin", "chkh"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE
# )

# create_db(credentials_data = credentials, sqlite_path = "data/database.sqlite")

ui <- tagList(
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  navbarPage("eCRF example",
    theme = bslib::bs_theme(version = 3),
    tabPanel("RCT",
      icon = icon("table"),
      cars_table_module_ui("table_rct")
    ),
    tabPanel("Prospective",
      icon = icon("table"),
      cars_table_module_ui("table_pros")
    )
  )
)

ui <- secure_app(ui, enable_admin = T, theme = "cosmo")

server <- function(input, output, session) {
  options(warn=-1)
  
  # Use session$userData to store user data that will be needed throughout

  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite")
  )

  # the Shiny application
  # session$userData$email <- res_auth$user

  # Call the server function portion of the `cars_table_module.R` module file
  callModule(cars_table_module, "table_rct", "rct", sessionid = res_auth$user)

  callModule(cars_table_module, "table_pros", "pros", sessionid = res_auth$user)
}

shinyApp(ui, server)
