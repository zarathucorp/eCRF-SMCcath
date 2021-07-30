patientsByDate_plot_module_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      echarts4rOutput(ns("PatientsNumber")) %>%
        withSpinner()
    )
  )
}

patientsByDate_plot_module <- function(input, output, session, data) {
  date <- callModule(date_select_module, "date_selector")
  observeEvent(date$month(), {
    observeEvent(date$year(), {
      dateQuery <- paste0(date$month(), "/", date$year())
      
      data$created_at <- format(data$created_at, "%m/%Y")
      subset <- filter(data, data$created_at == dateQuery)
      selectedPatients <- as.data.frame(table(subset$Center))
      
      patientsPerDate <- data.frame(hospital = unique(data$Center), patients = 0)
      if (nrow(selectedPatients) > 0) {
        for(i in 1:nrow(selectedPatients)) {
          rowIndex <- which(patientsPerDate$hospital == selectedPatients[i, 1])
          patientsPerDate[rowIndex, "patients"] = selectedPatients[i, 2]
        }
      }
      
      patientsPerDate <- patientsPerDate[order(patientsPerDate$hospital), ]
      output$PatientsNumber <- renderEcharts4r({
        patientsPerDate %>%
          e_charts(hospital) %>%
          e_bar(patients) %>%
          e_title("Patients Number By Date")
      })
    })
  })
}