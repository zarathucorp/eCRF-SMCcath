
#' Event Add & Edit Module
#'
#' Module to add & edit cars in the mtcars database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param car_to_edit reactive returning a 1 row data frame of the car to edit
#' from the "mt_cars" table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#' @param tbl "rct" or "pros"
#' @param data data to extract pid
#' @return None
#'
event_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        dateInput(
          ns("Last_FU_Date"),
          'Last F/U date',
          value = hold$Last_FU_Date,
          language = "kr"
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("Death"), "Death", choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),  selected = hold$Death, inline = T)
          ),
          column(
            width = 6,
            conditionalPanel("input.Death == 1", ns = ns, dateInput(ns("Death_Date"), "Death date",  value = hold$Death_Date, language = "kr"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("MI"), "MI", choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),  selected = hold$MI, inline = T)
          ),
          column(
            width = 6,
            conditionalPanel("input.MI == 1", ns = ns, dateInput(ns("MI_Date"), "MI date",  value = hold$MI_Date, language = "kr"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("TVMI"), "TV MI", choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),  selected = hold$TVMI, inline = T)
          ),
          column(
            width = 6,
            conditionalPanel("input.TVMI == 1", ns = ns, dateInput(ns("TVMI_Date"), "TVMI date",  value = hold$TVMI_Date, language = "kr"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("TLR"), "TLR", choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),  selected = hold$TLR, inline = T)
          ),
          column(
            width = 6,
            conditionalPanel("input.TLR == 1", ns = ns, dateInput(ns("TLR_Date"), "TLR date",  value = hold$TLR_Date, language = "kr"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("TVR"), "TVR", choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),  selected = hold$TVR, inline = T)
          ),
          column(
            width = 6,
            conditionalPanel("input.TVR == 1", ns = ns, dateInput(ns("TVR_Date"), "TVR date",  value = hold$TVR_Date, language = "kr"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("Readmission_Total"), "Readmission total", choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),  selected = hold$Readmission_Total, inline = T)
          ),
          column(
            width = 6,
            conditionalPanel("input.Readmission_Total == 1", ns = ns, dateInput(ns("Readmission_Total_Date"), "Readmission total date",  value = hold$Readmission_Total_Date, language = "kr"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("TLF"), "TLF", choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),  selected = hold$TLF, inline = T)
          ),
          column(
            width = 6,
            conditionalPanel("input.TLF == 1", ns = ns, dateInput(ns("TLF_Date"), "TLF date",  value = hold$TLF_Date, language = "kr"))
          )
        ),
        title = modal_title,
        size = 'm',
        footer = list(
          modalButton('Cancel'),
          actionButton(
            ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )

    # Observe event for "Model" text input in Add/Edit Car Modal
    # `shinyFeedback`
    observeEvent(input$model, {
      if (input$model == "") {
        shinyFeedback::showFeedbackDanger(
          "model",
          text = "Must enter model of car!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("model")
        shinyjs::enable('submit')
      }
    })

  })





  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      data = list(
        "Last_FU_Date" = ifelse(is.null(input$Last_FU_Date), "", as.character(input$Last_FU_Date))
      )
  
    )
    
    for (v in c("Death", "MI", "TVMI", "TLR", "TVR", "Readmission_Total", "TLF")){
      out$data[[v]] <- input[[v]]
      out$data[[paste0(v, "_Date")]] <- ifelse(is.null(input[[paste0(v, "_Date")]]), "", as.character(input[[paste0(v, "_Date")]]))
    }

    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

    if (is.null(hold)) {
      # adding a new car

      out$data$created_at <- time_now
      out$data$created_by <- sessionid
    } else {
      # Editing existing car

      out$data$created_at <- as.character(hold$created_at)
      out$data$created_by <- hold$created_by
    }

    out$data$modified_at <- time_now
    out$data$modified_by <- sessionid

    out
  })

  validate_edit <- eventReactive(input$submit, {
    dat <- edit_car_dat()

    # Logic to validate inputs...

    dat
  })

  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()
    hold <- car_to_edit()
    sqlsub <- paste(paste0(names(dat$data),  "=$", 1:length(dat$data)), collapse = ",")

    tryCatch({

      dbExecute(
        conn,
        #paste0("UPDATE ", tbl," SET Last_FU_Date=$1, Death=$2, Death_Date=$3,
        #       created_at=$4, created_by=$5,modified_at=$6, modified_by=$7 WHERE pid='", hold$pid, "'"),
        paste0("UPDATE ", tbl," SET ", sqlsub, " WHERE pid='", hold$pid, "'"),
        params = c(
          unname(dat$data)
        )
      )

      session$userData$mtcars_trigger(session$userData$mtcars_trigger() + 1)
      showToast("success", paste0(modal_title, " Successs"))
    }, error = function(error) {

      msg <- paste0(modal_title, " Error")


      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
  })

}
