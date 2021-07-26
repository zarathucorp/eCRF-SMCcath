source("global.R")
library(shinymanager)
library(bslib)
library(bs4Dash)
# credentials <- data.frame(
#  user = c("admin", "chkh"),
#  password = c("zarathuadmin", "chkh"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE
# )

# create_db(credentials_data = credentials, sqlite_path = "data/database.sqlite")

ui <- dashboardPage(
    title = "Basic Dashboard",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "bs4Dash",
        color = "primary",
        href = "https://www.google.fr",
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg"
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      sidebarUserPanel(
        image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
        name = "Welcome Onboard!"
      ),
      sidebarMenu(
        menuItem(
          "Item 1",
          tabName = "item1",
          icon = icon("sliders")
        ),
        menuItem(
          "Item 2",
          tabName = "item2",
          icon = icon("id-card")
        )
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName ="item1",
          patientsNumber_plot_module_ui("table_rct-Hospital1")
        ),
        tabItem(
          tabName = "item2",
          cars_table_module_ui("table_rct")
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
