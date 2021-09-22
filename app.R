source("global.R")
library(shinymanager)
library(shinydashboard)
library(shinydashboardPlus)
# credentials <- data.frame(
#  user = c("admin", "chkh"),
#  password = c("zarathuadmin", "chkh"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE
# )

# create_db(credentials_data = credentials, sqlite_path = "data/database.sqlite")

ui <- dashboardPage(
    skin = "black",
    title = "eCRF DashBoard",
    header = dashboardHeader(title = "eCRF DashBoard"),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Dashboard",
          tabName = "Dashboard",
          icon = icon("dashboard")
        ),
        menuItem(
          "eCRF",
          tabName = "eCRF",
          icon = icon("th")
        )
      )
    ),
    body = dashboardBody(
      shinyFeedback::useShinyFeedback(),
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
      ),
      tabItems(
        tabItem(
          tabName ="Dashboard",
          tabsetPanel(
            id = "chart_panel",
            selected = NULL,
            tabPanel(
              title = "RCT",
              patientsNumber_plot_module_ui("table_rct-Hospital1"),
              date_select_module_ui("table_rct-Date-date_selector"),
              patientsByDate_plot_module_ui("table_rct-Date")
            ),
            tabPanel(
              title = "Pros",
              patientsNumber_plot_module_ui("table_pros-Hospital1"),
              date_select_module_ui("table_pros-Date-date_selector"),
              patientsByDate_plot_module_ui("table_pros-Date")
            )
          )
        ),
        tabItem(
          tabName = "eCRF",
          tabsetPanel(
            id = "editer_panel",
            selected = NULL,
            tabPanel(
              title = "RCT",
              cars_table_module_ui("table_rct")
            ),
            tabPanel(
              title = "Pros",
              cars_table_module_ui("table_pros")
            )
          )
        )
      )
   )
)

# ui <- secure_app(ui, enable_admin = T, theme = "cosmo")
ui <- secure_app(ui, enable_admin = T, theme = "custom.css")

server <- function(input, output, session) {
  options(warn = -1)

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
