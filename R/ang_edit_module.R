#' ang Add & Edit Module
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
ang_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        dateInput(
          ns("Date_ang"),
          "Primary 시술일자",
          value = ifelse(is.null(hold), NA, lubridate::as_date(hold$Date_ang)),
          language = "kr"
        ),
        radioButtons(
          ns("Aspirin_ang"),
          label = "Aspirin",
          choices = c("Yes" = 0, "No" = 1),
          selected = character(0),
          inline = TRUE
        ),
        conditionalPanel(
          "input.Aspirin_ang == 0",
          ns = ns,
          textInput(
            ns("Aspirin_loading_ang"),
            label = "if Yes ->",
            placeholder = "mg"
          ),
          radioButtons(
            ns("Aspirin_detail_ang"),
            label = "Loading Time",
            choices = c("Before Cathlab Arrival" = 0, "After CAG before PCI" = 1, "After PCI" = 2),
            selected = character(0),
            inline = TRUE
          )
        ),
        radioButtons(
          ns("Clopidogrel_ang"),
          label = "Clopidogrel",
          choices = c("Yes" = 0, "No" = 1),
          selected = character(0),
          inline = TRUE
        ),
        conditionalPanel(
          "input.Clopidogrel_ang == 0",
          ns = ns,
          textInput(
            ns("Clopidogrel_loading_ang"),
            label = "if Yes ->",
            placeholder = "mg"
          ),
          radioButtons(
            ns("Clopidogrel_detail_ang"),
            label = "Loading Time",
            choices = c("Before Cathlab Arrival" = 0, "After CAG before PCI" = 1, "After PCI" = 2),
            selected = character(0),
            inline = TRUE
          )
        ),
        radioButtons(
          ns("Prasugrel_ang"),
          label = "Prasugrel",
          choices = c("Yes" = 0, "No" = 1),
          selected = character(0),
          inline = TRUE
        ),
        conditionalPanel(
          "input.Prasugrel_ang == 0",
          ns = ns,
          textInput(
            ns("Prasugrel_loading_ang"),
            label = "if Yes ->",
            placeholder = "mg"
          ),
          radioButtons(
            ns("Prasugrel_detail_ang"),
            label = "Loading Time",
            choices = c("Before Cathlab Arrival" = 0, "After CAG before PCI" = 1, "After PCI" = 2),
            selected = character(0),
            inline = TRUE
          )
        ),
        radioButtons(
          ns("Ticagrelor_ang"),
          label = "Ticagrelor",
          choices = c("Yes" = 0, "No" = 1),
          selected = character(0),
          inline = TRUE
        ),
        conditionalPanel(
          "input.Ticagrelor_ang == 0",
          ns = ns,
          textInput(
            ns("Ticagrelor_loading_ang"),
            label = "if Yes ->",
            placeholder = "mg"
          ),
          radioButtons(
            ns("Ticagrelor_detail_ang"),
            label = "Loading Time",
            choices = c("Before Cathlab Arrival" = 0, "After CAG before PCI" = 1, "After PCI" = 2),
            selected = character(0),
            inline = TRUE
          )
        ),
        radioButtons(
          ns("Procedure_type_ang"),
          label = "Procedure Type",
          choices = c("Transradial" = 0, "Transfemoral" = 1),
          selected = character(0),
          inline = TRUE
        ),
        textInput(
          ns("Procedure_dose_ang"),
          label = "조영제 사용량",
          placeholder = "cc"
        ),
        radioButtons(
          ns("Disease_extent_ang"),
          label = "Angiographica Disease Extent",
          choices = c("1 VD" = 0, "2 VD" = 1, "3 VD" = 2),
          selected = character(0),
          inline = TRUE
        ),
        radioButtons(
          ns("Left_main_ang"),
          label = "Left Main Disease",
          choices = c("Yes" = 0, "No" = 1),
          selected = character(0),
          inline = TRUE
        ),
        numericInput(
          ns("start_hour_ang"),
          label = "시작시간", min = 0, max = 23, step = 1, value = NULL
        ),
        numericInput(
          ns("start_min_ang"),
          label = "분", min = 0, max = 59, step = 1, value = NULL
        ),
        numericInput(
          ns("end_hour_ang"),
          label = "종료시간", min = 0, max = 23, step = 1, value = NULL
        ),
        numericInput(
          ns("end_min_ang"),
          label = "분", min = 0, max = 59, step = 1, value = NULL
        ),
        shinyjs::disabled(
          numericInput(
            ns("time_ang"),
            label = "소요시간(분)",
            step = 1, value = 0
          )
        ),
        textInput(
          ns("Catheter_diagnostic_ang"),
          label = "Diagnostic Catheter"
        ),
        textInput(
          ns("Catheter_guiding_ang"),
          label = "Guiding Catheter"
        ),
        radioButtons(
          ns("staged_PCI_ang"),
          label = "Staged PCI",
          choices = c("Performed" = 0, "Not Performed" = 1),
          selected = character(0),
          inline = TRUE
        ),
        conditionalPanel(
          "input.staged_PCI_ang == 0",
          ns = ns,
          
            numericInput(
              ns("stagedPCI_count_ang"),
              label = "ea", max = 2, min = 1, step = 1, value = 1
            ),
            dateInput(
              ns('stagedPCI_1_ang'),
              "시술일자",
              value = lubridate::as_date(hold$stagedPCI_1_ang),
              language = "kr"
            ),
            radioButtons(
              ns("stagedPCI_1_procedure_ang"),
              label = "Procedure Type",
              choices = c("Transradial" = 0, "Transfemoral" = 1),
              selected = character(0),
              inline = TRUE
            ),
            textInput(
              ns("stagedPCI_1_procedure_dose_ang"),
              label = "조영제 사용량",
              placeholder = "cc"
            ),
            numericInput(
              ns(paste0("start_hour_PCI_1_ang")),
              label = "시작시간", min = 0, max = 23, step = 1, value = NULL
            ),
            numericInput(
              ns("start_min_PCI_1_ang"),
              label = "분", min = 0, max = 59, step = 1, value = NULL
            ),
            numericInput(
              ns("end_hour_PCI_1_ang"),
              label = "종료시간", min = 0, max = 23, step = 1, value = NULL
            ),
            numericInput(
              ns("end_min_PCI_1_ang"),
              label = "분", min = 0, max = 59, step = 1, value = NULL
            ),
            shinyjs::disabled(
              numericInput(
                ns("time_PCI_1_ang"),
                label = "소요시간(분)",
                value = NULL
              )
            ),
            
            textInput(
              ns("Diagnostic_PCI_1_ang"),
              label = "Diagnostic",
              placeholder = "Fr"
            ),
            textInput(
              ns("Guiding_PCI_1_ang"),
              label = "Guiding",
              placeholder = "Fr"
            ),
          
          
          ## PCI 1
          conditionalPanel(
            "input.stagedPCI_count_ang >= 2",
            ns = ns,
            
              dateInput(
                ns('stagedPCI_2_ang'),
                "시술일자",
                value = lubridate::as_date(hold$stagedPCI_2_ang),
                language = "kr"
              ),
              radioButtons(
                ns("stagedPCI_2_procedure_ang"),
                label = "Procedure Type",
                choices = c("Transradial" = 0, "Transfemoral" = 1),
                selected = character(0),
                inline = TRUE
              ),
              textInput(
                ns("stagedPCI_2_procedure_dose_ang"),
                label = "조영제 사용량",
                placeholder = "cc"
              ),
              numericInput(
                ns(paste0("start_hour_PCI_2_ang")),
                label = "시작시간", min = 0, max = 23, step = 1, value = NULL
              ),
              numericInput(
                ns("start_min_PCI_2_ang"),
                label = "분", min = 0, max = 59, step = 1, value = NULL
              ),
              numericInput(
                ns("end_hour_PCI_2_ang"),
                label = "종료시간", min = 0, max = 23, step = 1, value = NULL
              ),
              numericInput(
                ns("end_min_PCI_2_ang"),
                label = "분", min = 0, max = 59, step = 1, value = NULL
              ),
              shinyjs::disabled(
                numericInput(
                  ns("time_PCI_2_ang"),
                  label = "소요시간(분)",
                  value = NULL
                )
              ),
              textInput(
                ns("Diagnostic_PCI_2_ang"),
                label = "Diagnostic",
                placeholder = "Fr"
              ),
              textInput(
                ns("Guiding_PCI_2_ang"),
                label = "Guiding",
                placeholder = "Fr"
              )
            
          )
        ),
        radioButtons(
          ns("Gp_ang"),
          label = "   Gp IIb/IIIa inhibitor	",
          choices = c("Yes" = 0, "No" = 1),
          checkboxGroupInput(
            ns("Gp_detail_ang"),
            label = "",
            choices = c("Intra-procedure" = 0, "Post-procedure" = 1, "Abciximab" = 2, "Eptifibatide" = 3, "Tirofiban" = 4),
            selected = character(0),
            inline = TRUE
          )
        ),
        numericInput(
          ns("Cul_cnt_ang"),
          label = "Culprit Lesion 개수",
          min = 0, max = 2, step = 1, value = 0
        ),
        numericInput(
          ns("Non_Cul_cnt_ang"),
          label = "Non-Culprit Lesion 개수",
          min = 0, max = 4, step = 1, value = 0
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

  observeEvent(
    {
      input$start_hour_ang
      input$start_min_ang
      input$end_hour_ang
      input$end_min_ang
    },
    {
      updateNumericInput(
        session, 
        inputId = "time_ang",
        label = "소요시간(분)",
        value = (input$end_hour_ang * 60 + input$end_min_ang - input$start_hour_ang * 60 - input$start_min_ang)
      )
    }
  )

  
  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      data = list(
        "Date_ang" = ifelse(is.null(input$Date_ang), "", as.character(input$Date_ang)),
        "stagedPCI_1_ang" = ifelse(is.null(input$stagedPCI_1_ang), "", as.character(input$stagedPCI_1_ang)),
        "stagedPCI_2_ang" = ifelse(is.null(input$stagedPCI_2_ang), "", as.character(input$stagedPCI_2_ang)),
        "Date_ang" = ifelse(is.null(input$Date_ang), "", input$Date_ang),
        "Aspirin_ang" = ifelse(is.null(input$Aspirin_ang), "", input$Aspirin_ang),
        "Aspirin_loading_ang" = ifelse(is.null(input$Aspirin_loading_ang), "", input$Aspirin_loading_ang),
        "Aspirin_detail_ang" = ifelse(is.null(input$Aspirin_detail_ang), "", input$Aspirin_detail_ang),
        "Clopidogrel_ang" = ifelse(is.null(input$Clopidogrel_ang), "", input$Clopidogrel_ang),
        "Clopidogrel_loading_ang" = ifelse(is.null(input$Clopidogrel_loading_ang), "", input$Clopidogrel_loading_ang),
        "Clopidogrel_detail_ang" = ifelse(is.null(input$Clopidogrel_detail_ang), "", input$Clopidogrel_detail_ang),
        "Prasugrel_ang" = ifelse(is.null(input$Prasugrel_ang), "", input$Prasugrel_ang),
        "Prasugrel_loading_ang" = ifelse(is.null(input$Prasugrel_loading_ang), "", input$Prasugrel_loading_ang),
        "Prasugrel_detail_ang" = ifelse(is.null(input$Prasugrel_detail_ang), "", input$Prasugrel_detail_ang),
        "Ticagrelor_ang" = ifelse(is.null(input$Ticagrelor_ang), "", input$Ticagrelor_ang),
        "Ticagrelor_loading_ang" = ifelse(is.null(input$Ticagrelor_loading_ang), "", input$Ticagrelor_loading_ang),
        "Ticagrelor_detail_ang" = ifelse(is.null(input$Ticagrelor_detail_ang), "", input$Ticagrelor_detail_ang),
        "Procedure_type_ang" = ifelse(is.null(input$Procedure_type_ang), "", input$Procedure_type_ang),
        "Procedure_dose_ang" = ifelse(is.null(input$Procedure_dose_ang), "", input$Procedure_dose_ang),
        "Disease_extent_ang" = ifelse(is.null(input$Disease_extent_ang), "", input$Disease_extent_ang),
        "Left_main_ang" = ifelse(is.null(input$Left_main_ang), "", input$Left_main_ang),
        "start_hour_ang" = ifelse(is.null(input$start_hour_ang), "", input$start_hour_ang),
        "start_min_ang" = ifelse(is.null(input$start_min_ang), "", input$start_min_ang),
        "end_hour_ang" = ifelse(is.null(input$end_hour_ang), "", input$end_hour_ang),
        "end_min_ang" = ifelse(is.null(input$end_min_ang), "", input$end_min_ang),
        "time_ang" = ifelse(is.null(input$time_ang), "", input$time_ang),
        "Catheter_diagnostic_ang" = ifelse(is.null(input$Catheter_diagnostic_ang), "", input$Catheter_diagnostic_ang),
        "Catheter_guiding_ang" = ifelse(is.null(input$Catheter_guiding_ang), "", input$Catheter_guiding_ang),
        "staged_PCI_ang" = ifelse(is.null(input$staged_PCI_ang), "", input$staged_PCI_ang),
        "stagedPCI_count_ang" = ifelse(is.null(input$stagedPCI_count_ang), "", input$stagedPCI_count_ang),
        "Gp_ang" = ifelse(is.null(input$Gp_ang), "", input$Gp_ang),
        "Gp_detail_ang" = ifelse(is.null(input$Gp_detail_ang), "", input$Gp_detail_ang),
        "Cul_cnt_ang" = ifelse(is.null(input$Cul_cnt_ang), "", input$Cul_cnt_ang),
        "Non_Cul_cnt_ang" = ifelse(is.null(input$Non_Cul_cnt_ang), "", input$Non_Cul_cnt_ang),
        "stagedPCI_1_ang" = ifelse(is.null(input$stagedPCI_1_ang), "", input$stagedPCI_1_ang),
        "stagedPCI_2_ang" = ifelse(is.null(input$stagedPCI_2_ang), "", input$stagedPCI_2_ang),
        "stagedPCI_1_procedure_ang" = ifelse(is.null(input$stagedPCI_1_procedure_ang), "", input$stagedPCI_1_procedure_ang),
        "stagedPCI_2_procedure_ang" = ifelse(is.null(input$stagedPCI_2_procedure_ang), "", input$stagedPCI_2_procedure_ang),
        "stagedPCI_1_procedure_dose_ang" = ifelse(is.null(input$stagedPCI_1_procedure_dose_ang), "", input$stagedPCI_1_procedure_dose_ang),
        "stagedPCI_2_procedure_dose_ang" = ifelse(is.null(input$stagedPCI_2_procedure_dose_ang), "", input$stagedPCI_2_procedure_dose_ang),
        "start_hour_PCI_1_ang" = ifelse(is.null(input$start_hour_PCI_1_ang), "", input$start_hour_PCI_1_ang),
        "start_hour_PCI_2_ang" = ifelse(is.null(input$start_hour_PCI_2_ang), "", input$start_hour_PCI_2_ang),
        "start_min_PCI_1_ang" = ifelse(is.null(input$start_min_PCI_1_ang), "", input$start_min_PCI_1_ang),
        "start_min_PCI_2_ang" = ifelse(is.null(input$start_min_PCI_2_ang), "", input$start_min_PCI_2_ang),
        "end_hour_PCI_1_ang" = ifelse(is.null(input$end_hour_PCI_1_ang), "", input$end_hour_PCI_1_ang),
        "end_hour_PCI_2_ang" = ifelse(is.null(input$end_hour_PCI_2_ang), "", input$end_hour_PCI_2_ang),
        "end_min_PCI_1_ang" = ifelse(is.null(input$end_min_PCI_1_ang), "", input$end_min_PCI_1_ang),
        "end_min_PCI_2_ang" = ifelse(is.null(input$end_min_PCI_2_ang), "", input$end_min_PCI_2_ang),
        "time_PCI_1_ang" = ifelse(is.null(input$time_PCI_1_ang), "", input$time_PCI_1_ang),
        "time_PCI_2_ang" = ifelse(is.null(input$time_PCI_2_ang), "", input$time_PCI_2_ang),
        "Diagnostic_PCI_1_ang" = ifelse(is.null(input$Diagnostic_PCI_1_ang), "", input$Diagnostic_PCI_1_ang),
        "Diagnostic_PCI_2_ang" = ifelse(is.null(input$Diagnostic_PCI_2_ang), "", input$Diagnostic_PCI_2_ang),
        "Guiding_PCI_1_ang" = ifelse(is.null(input$Guiding_PCI_1_ang), "", input$Guiding_PCI_1_ang),
        "Guiding_PCI_2_ang" = ifelse(is.null(input$Guiding_PCI_2_ang), "", input$Guiding_PCI_2_ang)
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
