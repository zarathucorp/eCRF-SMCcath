#' m3 Add & Edit Module
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
m3_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(

        radioButtons(
          ns("FU_M3"),
          "F/U Date",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$FU_M3, inline = T
        ),

        ## If YES

        conditionalPanel(
          "input.FU_M3 == 0",
          ns = ns,
          dateInput(
            ns("Visit_Date_M3"),
            "Visit date",
            value = hold$Visit_Date_M3,
            language = "kr"
          ),
          radioButtons(
            ns("Visit_M3"),
            "Visit by",
            choices = c("Clinic" = 0, "Phone" = 1),
            selected = hold$Visit_M3, inline = T
          ),

          ## Implement Duration
        ),
        conditionalPanel(
          "input.FU_M3 == 1",
          ns = ns,
          radioButtons(
            ns("Reason_M3"),
            "Reason",
            choices = c("Patient Died" = 0, "Patient Lost to F/U" = 1, "Other" = 2),
            selected = hold$Reason_M3, inline = T
          ),
          conditionalPanel(
            "input.Reason_M3 == 2",
            ns = ns,
            textAreaInput(
              ns("Other_M3"),
              "Other",
              width = "400px",
              height = "100px"
            ),
            dateInput(
              ns("LastFU_M3"),
              "Date of Last F/U",
              value = hold$LastFU_M3,
              language = "kr"
            )
          )
          ## Implement Duration
        ),
        numericInput(
          ns("SBP_M3"), # Systolic BP
          "Systolic BP",
          value = ifelse(is.null(hold), NA, hold$SBP_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("DBP_M3"), # Diastolic BP
          "Diastolic BP",
          value = ifelse(is.null(hold), NA, hold$DBP_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HRT_M3"), # Heart Ratio
          "Heart Rate",
          value = ifelse(is.null(hold), NA, hold$HRT_M3),
          min = 0, max = 120,
          step = 1
        ),
        radioButtons(
          ns("Event_M3"),
          "Clinical Events",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Event_M3, inline = T
        ),
        radioButtons(
          ns("Withdrawal_M3"),
          "Withdrawal",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Withdrawal_M3, inline = T
        ),
        conditionalPanel(
          "input.Withdrawal_M3 == 0",
          ns = ns,
          dateInput(
            ns("Withdrawal_Date_M3"),
            "Date",
            value = hold$Withdrawal_Date_M3,
            language = "kr"
          ),
          radioButtons(
            ns("Cause_M3"),
            "Cause",
            choices = c("Voluntary WIthdrawal" = 0, "Withdrawal by Investigator as Clinically Indicated" = 1),
            selected = hold$Cause_M3, inline = T
          )
        ),

        # Medication ?

        textAreaInput(
          ns("Comment_M3"),
          "Comment",
          width = "400px",
          height = "100px",
          resize = "both"
        ),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("submit"),
            "Submit",
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
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("model")
        shinyjs::enable("submit")
      }
    })
  })

  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      data = list(
        "FU_M3" = input$FU_M3,
        "Visit_Date_M3" = ifelse(is.null(input$Visit_Date_M3), "", as.character(input$Visit_Date_M3)),
        "Visit_M3" = ifelse(is.null(input$Visit_M3),"", input$Visit_M3),
        "Reason_M3" = ifelse(is.null(input$Reason_M3),"", input$Reason_M3),
        "Other_M3" = ifelse(is.null(input$Other_M3),"", input$Other_M3),
        "LastFU_M3" = ifelse(is.null(input$LastFU_M3), "",as.character(input$LastFU_M3)),
        "SBP_M3" = ifelse(is.null(input$SBP_M3),"",input$SBP_M3),
        "DBP_M3" = ifelse(is.null(input$DBP_M3),"",input$DBP_M3),
        "HRT_M3" = ifelse(is.null(input$HRT_M3),"",input$HRT_M3),
        "Event_M3" = ifelse(is.null(input$Event_M3),"",input$Event_M3),
        "Withdrawal_M3" = ifelse(is.null(input$Withdrawal_M3),"", input$Withdrawal_M3),
        "Withdrawal_Date_M3" = ifelse(is.null(input$Withdrawal_Date_M3), "",as.character(input$Withdrawal_Date_M3)),
        "Cause_M3" = ifelse(is.null(input$Cause_M3),"",input$Cause_M3),
        "Comment_M3" = ifelse(is.null(input$Comment_M3), "", input$Comment_M3)
      )
    )

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
    sqlsub <- paste(paste0(names(dat$data), "=$", 1:length(dat$data)), collapse = ",")

    tryCatch(
      {
        dbExecute(
          conn,
          # paste0("UPDATE ", tbl," SET Last_FU_Date=$1, Death=$2, Death_Date=$3,
          #       created_at=$4, created_by=$5,modified_at=$6, modified_by=$7 WHERE pid='", hold$pid, "'"),
          paste0("UPDATE ", tbl, " SET ", sqlsub, " WHERE pid='", hold$pid, "'"),
          params = c(
            unname(dat$data)
          )
        )

        session$userData$mtcars_trigger(session$userData$mtcars_trigger() + 1)
        showToast("success", paste0(modal_title, " Successs"))
      },
      error = function(error) {
        msg <- paste0(modal_title, " Error")


        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      }
    )
  })
}
