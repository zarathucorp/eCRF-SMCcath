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
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Primary PCI",
              tags$div(
                modalButton("", icon("times")),
                style = "float:right;"
              ),
              actionButton(
                ns("submit0"),
                HTML('<i class="fas fa-check"></i>'),
                class = "btn",
                style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
              )
            ),
            "</h3>"
          )
        ),
        fluidRow(
          column(
            width = 4,
            dateInput(
              ns("Date_ang"),
              "Primary 시술일자",
              value = ifelse(is.null(hold), "", lubridate::as_date(hold$Date_ang)),
              language = "ko"
            )
          ),
          column(
            width = 4,
            radioButtons(
              ns("Procedure_type_ang"),
              label = "Procedure Type",
              choices = c("Transradial", "Transfemoral"),
              selected = ifelse(is.null(hold$Procedure_type_ang), character(0), hold$Procedure_type_ang),
              inline = TRUE
            )
          ),
          column(
            width = 4,
            textInput(
              ns("Procedure_dose_ang"),
              label = "조영제 사용량",
              placeholder = "cc",
              value = hold$Procedure_dose_ang
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Antiplatelet agent loading",
              tags$div(
                modalButton("", icon("times")),
                style = "float:right;"
              ),
              actionButton(
                ns("submit1"),
                HTML('<i class="fas fa-check"></i>'),
                class = "btn",
                style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
              )
            ),
            "</h3>"
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("Aspirin_ang"),
              label = "Aspirin",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$Aspirin_ang), character(0), hold$Aspirin_ang),
              inline = TRUE
            ),
            conditionalPanel(
              "input.Aspirin_ang == 'Yes'",
              ns = ns,
              textInput(
                ns("Aspirin_loading_ang"),
                label = "Dose",
                placeholder = "mg",
                value = hold$Aspirin_loading_ang
              ),
              radioButtons(
                ns("Aspirin_detail_ang"),
                label = "Loading Time",
                choices = c("Before Cathlab Arrival" = 0, "After CAG before PCI" = 1, "After PCI" = 2),
                selected = ifelse(is.null(hold$Aspirin_detail_ang), character(0), hold$Aspirin_detail_ang),
                inline = TRUE
              )
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Clopidogrel_ang"),
              label = "Clopidogrel",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$Clopidogrel_ang), character(0), hold$Clopidogrel_ang),
              inline = TRUE
            ),
            conditionalPanel(
              "input.Clopidogrel_ang == 'Yes'",
              ns = ns,
              textInput(
                ns("Clopidogrel_loading_ang"),
                label = "Dose",
                placeholder = "mg",
                value = hold$Clopidogrel_loading_ang
              ),
              radioButtons(
                ns("Clopidogrel_detail_ang"),
                label = "Loading Time",
                choices = c("Before Cathlab Arrival", "After CAG before PCI", "After PCI"),
                selected = ifelse(is.null(hold$Clopidogrel_detail_ang), character(0), hold$Clopidogrel_detail_ang),
                inline = TRUE
              )
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Prasugrel_ang"),
              label = "Prasugrel",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$Prasugrel_ang), character(0), hold$Prasugrel_ang),
              inline = TRUE
            ),
            conditionalPanel(
              "input.Prasugrel_ang == 'Yes'",
              ns = ns,
              textInput(
                ns("Prasugrel_loading_ang"),
                label = "Dose",
                placeholder = "mg",
                value = hold$Prasugrel_loading_ang
              ),
              radioButtons(
                ns("Prasugrel_detail_ang"),
                label = "Loading Time",
                choices = c("Before Cathlab Arrival", "After CAG before PCI", "After PCI"),
                selected = ifelse(is.null(hold$Prasugrel_detail_ang), character(0), hold$Prasugrel_detail_ang),
                inline = TRUE
              )
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Ticagrelor_ang"),
              label = "Ticagrelor",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$Ticagrelor_ang), character(0), hold$Ticagrelor_ang),
              inline = TRUE
            ),
            conditionalPanel(
              "input.Ticagrelor_ang == 'Yes'",
              ns = ns,
              textInput(
                ns("Ticagrelor_loading_ang"),
                label = "Dose",
                placeholder = "mg",
                value = hold$Ticagrelor_loading_ang
              ),
              radioButtons(
                ns("Ticagrelor_detail_ang"),
                label = "Loading Time",
                choices = c("Before Cathlab Arrival", "After CAG before PCI", "After PCI"),
                selected = ifelse(is.null(hold$Ticagrelor_detail_ang), character(0), hold$Ticagrelor_detail_ang),
                inline = TRUE
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Angiographic data",
              tags$div(
                modalButton("", icon("times")),
                style = "float:right;"
              ),
              actionButton(
                ns("submit2"),
                HTML('<i class="fas fa-check"></i>'),
                class = "btn",
                style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
              )
            ),
            "</h3>"
          )
        ),
        fluidRow(
          column(
            width = 4,
            radioButtons(
              ns("Disease_extent_ang"),
              label = "Angiographica Disease Extent",
              choices = c("1 VD", "2 VD", "3 VD"),
              selected = ifelse(is.null(hold$Disease_extent_ang), character(0), hold$Disease_extent_ang),
              inline = TRUE
            )
          ),
          column(
            width = 4,
            radioButtons(
              ns("Left_main_ang"),
              label = "Left Main Disease",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$Left_main_ang), character(0), hold$Left_main_ang),
              inline = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            numericInput(
              ns("start_hour_ang"),
              label = "시작시간", min = 0, max = 23, step = 1, value = hold$start_hour_ang
            )
          ),
          column(
            width = 2,
            numericInput(
              ns("start_min_ang"),
              label = "분", min = 0, max = 59, step = 1, value = hold$start_min_ang
            )
          ),
          column(
            width = 2,
            numericInput(
              ns("end_hour_ang"),
              label = "종료시간", min = 0, max = 23, step = 1, value = hold$end_hour_ang
            )
          ),
          column(
            width = 2,
            numericInput(
              ns("end_min_ang"),
              label = "분", min = 0, max = 59, step = 1, value = hold$end_min_ang
            )
          ),
          column(
            width = 4,
            shinyjs::disabled(
              numericInput(
                ns("time_ang"),
                label = "소요시간(분)",
                step = 1, 
                value = hold$time_ang
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("Catheter_diagnostic_ang"),
              label = "Diagnostic Catheter",
              value = hold$Catheter_diagnostic_ang,
              placeholder = "Fr"
            )
          ),
          column(
            width = 3,
            textInput(
              ns("Catheter_guiding_ang"),
              label = "Guiding Catheter",
              placeholder = "Fr",
              value = hold$Catheter_guiding_ang
            )
          ),
          column(
            width = 4,
            radioButtons(
              ns("staged_PCI_ang"),
              label = "Staged PCI",
              choices = c("Performed", "Not Performed"),
              selected = ifelse(is.null(hold$staged_PCI_ang), character(0), hold$staged_PCI_ang),
              inline = TRUE
            )
          ),
          column(
            width = 2,
            conditionalPanel(
              "input.staged_PCI_ang == 'Yes'",
              ns = ns,
              numericInput(
                ns("stagedPCI_count_ang"),
                label = "개수", 
                max = 2, 
                min = 1, 
                step = 1, 
                value = ifelse(is.null(hold$stagedPCI_count_ang), 1, hold$stagedPCI_count_ang),
              )
            )
          )
        ),
        conditionalPanel(
          "input.staged_PCI_ang == 'Yes'",
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "PCI 1st",
                tags$div(
                  modalButton("", icon("times")),
                  style = "float:right;"
                ),
                actionButton(
                  ns("submit3"),
                  HTML('<i class="fas fa-check"></i>'),
                  class = "btn",
                  style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
                )
              ),
              "</h3>"
            )
          ),
          fluidRow(
            column(
              width = 4,
              dateInput(
                ns("stagedPCI_1_ang"),
                "시술일자",
                value = lubridate::as_date(hold$stagedPCI_1_ang),
                language = "ko"
              )
            ),
            column(
              width = 4,
              radioButtons(
                ns("stagedPCI_1_procedure_ang"),
                label = "Procedure Type",
                choices = c("Transradial" = 0, "Transfemoral" = 1),
                selected = ifelse(is.null(hold$stagedPCI_1_procedure_ang), character(0), hold$stagedPCI_1_procedure_ang),
                inline = TRUE
              ) 
            ),
            column(
              width = 4,
              textInput(
                ns("stagedPCI_1_procedure_dose_ang"),
                label = "조영제 사용량",
                placeholder = "cc",
                value = hold$stagedPCI_1_procedure_dose_ang
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              numericInput(
                ns(paste0("start_hour_PCI_1_ang")),
                label = "시작시간", min = 0, max = 23, step = 1, 
                value = ifelse(is.null(hold$start_hour_PCI_1_ang), character(0), hold$start_hour_PCI_1_ang)
              )
            ),
            column(
              width = 2,
              numericInput(
                ns("start_min_PCI_1_ang"),
                label = "분", min = 0, max = 59, step = 1, 
                value = ifelse(is.null(hold$start_min_PCI_1_ang), character(0), hold$start_min_PCI_1_ang)
              )
            ),
            column(
              width = 2,
              numericInput(
                ns("end_hour_PCI_1_ang"),
                label = "종료시간", min = 0, max = 23, step = 1, 
                value = ifelse(is.null(hold$end_hour_PCI_1_ang), character(0), hold$end_hour_PCI_1_ang)
              )
            ),
            column(
              width = 2,
              numericInput(
                ns("end_min_PCI_1_ang"),
                label = "분", min = 0, max = 59, step = 1, 
                value = ifelse(is.null(hold$end_min_PCI_1_ang), character(0), hold$end_min_PCI_1_ang)
              )
            ),
            column(
              width = 4,
              shinyjs::disabled(
                numericInput(
                  ns("time_PCI_1_ang"),
                  label = "소요시간(분)",
                  value = hold$time_PCI_1_ang
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              textInput(
                ns("Diagnostic_PCI_1_ang"),
                label = "Diagnostic",
                placeholder = "Fr",
                value = hold$Diagnostic_PCI_1_ang
              ) 
            ),
            column(
              width = 4,
              textInput(
                ns("Guiding_PCI_1_ang"),
                label = "Guiding",
                placeholder = "Fr",
                value = hold$Guiding_PCI_1_ang
              )
            )
          )
        ),
        conditionalPanel(
          "input.stagedPCI_count_ang == 2",
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "PCI 2nd",
                tags$div(
                  modalButton("", icon("times")),
                  style = "float:right;"
                ),
                actionButton(
                  ns("submit4"),
                  HTML('<i class="fas fa-check"></i>'),
                  class = "btn",
                  style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
                )
              ),
              "</h3>"
            )
          ),
          fluidRow(
            column(
              width = 4,
              dateInput(
                ns("stagedPCI_2_ang"),
                "시술일자",
                value = lubridate::as_date(hold$stagedPCI_2_ang),
                language = "ko"
              )
            ),
            column(
              width = 4,
              radioButtons(
                ns("stagedPCI_2_procedure_ang"),
                label = "Procedure Type",
                choices = c("Transradial" = 0, "Transfemoral" = 1),
                selected = ifelse(is.null(hold$stagedPCI_2_procedure_ang), character(0), hold$stagedPCI_2_procedure_ang),
                inline = TRUE
              )
            ),
            column(
              width = 4,
              textInput(
                ns("stagedPCI_2_procedure_dose_ang"),
                label = "조영제 사용량",
                placeholder = "cc",
                value = ifelse(is.null(hold$stagedPCI_2_procedure_dose_ang), character(0), hold$stagedPCI_2_procedure_dose_ang)
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              numericInput(
                ns(paste0("start_hour_PCI_2_ang")),
                label = "시작시간", min = 0, max = 23, step = 1, 
                value = ifelse(is.null(hold$start_hour_PCI_2_ang), character(0), hold$start_hour_PCI_2_ang)
              )
            ),
            column(
              width = 2,
              numericInput(
                ns("start_min_PCI_2_ang"),
                label = "분", min = 0, max = 59, step = 1, 
                value = ifelse(is.null(hold$start_min_PCI_2_ang), character(0), hold$start_min_PCI_2_ang)
              )
            ),
            column(
              width = 2,
              numericInput(
                ns("end_hour_PCI_2_ang"),
                label = "종료시간", min = 0, max = 23, step = 1, 
                value = ifelse(is.null(hold$end_hour_PCI_2_ang), character(0), hold$end_hour_PCI_2_ang)
              ) 
            ),
            column(
              width = 2,
              numericInput(
                ns("end_min_PCI_2_ang"),
                label = "분", min = 0, max = 59, step = 1,
                value = ifelse(is.null(hold$end_min_PCI_2_ang), character(0), hold$end_min_PCI_2_ang)
              )
            ),
            column(
              width = 4,
              shinyjs::disabled(
                numericInput(
                  ns("time_PCI_2_ang"),
                  label = "소요시간(분)",
                  value = ifelse(is.null(hold$time_PCI_2_ang), character(0), hold$time_PCI_2_ang)
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              textInput(
                ns("Diagnostic_PCI_2_ang"),
                label = "Diagnostic",
                placeholder = "Fr",
                value = ifelse(is.null(hold$Diagnostic_PCI_2_ang), character(0), hold$Diagnostic_PCI_2_ang)
              )
            ),
            column(
              width = 4,
              textInput(
                ns("Guiding_PCI_2_ang"),
                label = "Guiding",
                placeholder = "Fr",
                value = ifelse(is.null(hold$Guiding_PCI_2_ang), character(0), hold$Guiding_PCI_2_ang)
              )
            )
          ),
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Lesion",
              tags$div(
                modalButton("", icon("times")),
                style = "float:right;"
              ),
              actionButton(
                ns("submit5"),
                HTML('<i class="fas fa-check"></i>'),
                class = "btn",
                style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
              )
            ),
            "</h3>"
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            radioButtons(
              ns("Gp_ang"),
              label = "Gp IIb/IIIa inhibitor	",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$Gp_ang), character(0), hold$Gp_ang),
              inline = TRUE
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.Gp_ang == 'Yes'",
              ns = ns,
              checkboxGroupInput(
                ns("Gp_detail_ang"),
                label = "Detail",
                choices = c("Intra-procedure" = 0, "Post-procedure" = 1, "Abciximab" = 2, "Eptifibatide" = 3, "Tirofiban" = 4),
                selected = ifelse(is.null(hold$Gp_detail_ang), character(0), hold$Gp_detail_ang),
                inline = TRUE
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(
              ns("Cul_cnt_ang"),
              label = "Culprit Lesion 개수",
              min = 0, 
              max = 2, 
              step = 1, 
              value = ifelse(is.null(hold$Cul_cnt_ang), 0, hold$Cul_cnt_ang)
            )
          ),
          column(
            width = 6,
            numericInput(
              ns("Non_Cul_cnt_ang"),
              label = "Non-Culprit Lesion 개수",
              min = 0, 
              max = 4, 
              step = 1, 
              value = ifelse(is.null(hold$Non_Cul_cnt_ang), 0, hold$Non_Cul_cnt_ang )
            )
          )
        ),
        title = modal_title,
        size = "l",
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
  
  observeEvent(
    {
      input$start_hour_PCI_1_ang
      input$start_min_PCI_1_ang
      input$end_hour_PCI_1_ang
      input$end_min_PCI_1_ang
    },
    {
      updateNumericInput(
        session,
        inputId = "time_PCI_1_ang",
        label = "소요시간(분)",
        value = (input$end_hour_PCI_1_ang * 60 + input$end_min_PCI_1_ang - input$start_hour_PCI_1_ang * 60 - input$start_min_PCI_1_ang)
      )
    }
  )
  
  observeEvent(
    {
      input$start_hour_PCI_2_ang
      input$start_min_PCI_2_ang
      input$end_hour_PCI_2_ang
      input$end_min_PCI_2_ang
    },
    {
      updateNumericInput(
        session,
        inputId = "time_PCI_2_ang",
        label = "소요시간(분)",
        value = (input$end_hour_PCI_2_ang * 60 + input$end_min_PCI_2_ang - input$start_hour_PCI_2_ang * 60 - input$start_min_PCI_2_ang)
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

  callEdit <- reactive({
    list(
      input$submit,
      input$submit0,
      input$submit1,
      input$submit2,
      input$submit3,
      input$submit4,
      input$submit5
    )
  })
  
  # Reference : https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
  
  validate_edit <- eventReactive(
    eventExpr = callEdit(),
    valueExpr = {
      if(input$submit == 0 && input$submit0 == 0 && input$submit1 == 0 && 
         input$submit2 == 0 && input$submit3 == 0 && input$submit4 == 0 && input$submit5 == 0){return()}
      dat <- edit_car_dat()
      # Logic to validate inputs...
      dat
    },ignoreInit = TRUE)
  
  
  # validate_edit <- eventReactive({
  #   input$submit
  #   input$submit0
  #   }, {
  #   dat <- edit_car_dat()
  # 
  #   # Logic to validate inputs...
  # 
  #   dat
  # })



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
