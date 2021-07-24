#' Demographics Add & Edit Module
#'
#' Module to Add & edit Demographics in to DB
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable disabled
#' @importFrom lubridate with_tz as_date
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param car_to_edit reactive returning a 1 row data frame of the car to edit
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#' @param tbl "rct" or "pros"
#' @param data data to extract pid
#' @return None
#'
#'
demographics_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()
    choiceNames.DM <- c("Yes", "No")
    choiceValues.DM <- c("0", "1")
    selected.DM <- NULL

    choices.AMI_Type <- c("NSTEMI", "STEMI")
    selected.AMI_Type <- NULL
    if (tbl == "rct") {
      choiceNames.DM <- ifelse(hold$DM == "0", "Yes", "No")
      choiceValues.DM <- hold$DM
      # choices.AMI_Type <- hold$AMI_Type
      selected.AMI_Type <- hold$AMI_Type
    }

    # CHECK !!! as.numeric으로 풀어야 됨.

    output$pid_demo <- renderText(hold$pid)
    output$Group <- renderText(hold$group)

    if (tbl == "rct") {
      showModal(
        modalDialog(
          fluidRow(
            column(
              width = 3,
              disabled(
                radioButtons(
                  inputId = ns("pid_demo"),
                  label = "Subject No :",
                  choices = hold$pid,
                  selected = hold$pid,
                  inline = T
                )
              ),
            ),
            column(
              width = 3,
              disabled(
                radioButtons(
                  inputId = ns("Group"),
                  label = "Allocation Group :",
                  choices = hold$Group,
                  selected = hold$Group,
                  inline = T
                )
              )
            ),
            column(
              width = 3,
              disabled(
                radioButtons(
                  inputId = ns("DM"),
                  label = "Previous DM",
                  choices = c("Yes" = "Yes", "No" = "No"),
                  selected = hold$DM,
                  inline = T
                )
              )
            ),
            column(
              width = 3,
              disabled(
                radioButtons(
                  inputId = ns("AMI_Type"),
                  label = "AMI Type",
                  choices = choices.AMI_Type,
                  selected = selected.AMI_Type,
                  inline = T
                )
              ),
            )
          ),
          fluidRow(
            column(
              width = 2,
              textInput(
                inputId = ns("Initial"),
                label = "Initial",
                value = ifelse(is.null(hold), "", hold$Initial)
              )
            ),
            column(
              width = 2,
              dateInput(
                inputId = ns("Birthday"),
                label = "Date of Birth",
                value = as_date(hold$Birthday),
                language = "ko"
              )
            ),
            column(
              width = 2,
              disabled(
                numericInput(
                  inputId = ns("Age"),
                  label = "Age",
                  value = ifelse(is.null(hold), "", hold$Age),
                  min = 0,
                  max = 120,
                  step = 1
                )
              )
            ),
            column(
              width = 2,
              radioButtons(
                inputId = ns("Sex"),
                label = "Sex",
                choices = c("M", "F"),
                selected = hold$Sex,
                inline = T
              )
            ),
            column(
              width = 2,
              disabled(
                radioButtons(
                  ns('CENTER'),
                  'Center',
                  c('삼성서울병원','전남대병원'),
                  selected = hold$Center
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              dateInput(
                inputId = ns("Agree_Date"),
                label = "동의서 서명일",
                value = as_date(as.numeric(hold$Agree_Date)),
                language = "ko"
              )
            ),
            column(
              width = 2,
              dateInput(
                inputId = ns("Index_PCI_Date"),
                label = "시술일자",
                value = as_date(as.numeric(hold$Index_PCI_Date)),
                language = "ko"
              )
            ),
            column(
              width = 2,
              radioButtons(
                inputId = ns("Withdrawal"),
                label = "Withdrawal",
                choices = c("Yes" = "Yes", "No" = "No"),
                selected = ifelse(is.null(hold), character(0), hold$Withdrawal),
                inline = T
              )
            ),
            column(
              width = 2,
              conditionalPanel(
                condition = "input.Withdrawal == 'Yes'",
                ns = ns,
                dateInput(
                  inputId = "Withdrawal_date",
                  label = "Date",
                  value = as_date(as.numeric(hold$Withdrawal_date)),
                  language = "ko"
                )
              )
            ),
            column(
              width = 2,
              conditionalPanel(
                condition = "input.Withdrawal == 'Yes'",
                ns = ns,
                textInput(
                  inputId = "Withdrawal_reason",
                  label = "Reason",
                  value = ifelse(is.null(hold), "", hold$Withdrawal_reason)
                ),
              )
            ),
            column(
              width = 2,
              radioButtons(
                inputId = ns("SGLT"),
                label = "SGLT2 Timing",
                choices = c("Pre-PCI", "Post-PCI"),
                selected = ifelse(is.null(hold$SGLT), character(0), hold$SGLT)
              )
            )
          ),
          title = modal_title,
          size = "l",
          footer = list(
            modalButton("Cancel"),
            actionButton(
              inputId = ns("submit"),
              label = "Submit",
              class = "btn btn-primary",
              style = "color: white"
            )
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          fluidRow(
            column(
              width = 3,
              disabled(
                radioButtons(
                  inputId = ns("pid_demo"),
                  label = "Subject No :",
                  choices = hold$pid,
                  selected = hold$pid,
                  inline = T
                )
              )
            ),
            column(
              width = 3,
              disabled(
                radioButtons(
                  inputId = ns("Group"),
                  label = "Allocation Group :",
                  choices = hold$Group,
                  selected = hold$Group,
                  inline = T
                )
              )
            ),
            column(
              width = 3,
              textInput(
                inputId = ns("Initial"),
                label = "Initial",
                value = hold$Initial
              )
            ),
            column(
              width = 3,
              dateInput(
                inputId = ns("Agree_Date"),
                label = "동의서 서명일",
                value = as_date(as.numeric(hold$Agree_Date)),
                language = "ko"
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              dateInput(
                inputId = ns("Index_PCI_Date"),
                label = "시술일자",
                value = as_date(as.numeric(hold$Index_PCI_Date)),
                language = "ko"
              )
            ),
            column(
              width = 3,
              dateInput(
                inputId = ns("Birthday"),
                label = "Date of Birth",
                value = as_date(hold$Birthday),
                language = "ko"
              )
            ),
            column(
              width = 3,
              disabled(
                numericInput(
                  inputId = ns("Age"),
                  label = "Age",
                  value = ifelse(is.null(hold), "", hold$Age),
                  min = 0,
                  max = 120,
                  step = 1
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                inputId = ns("Sex"),
                label = "Sex",
                choices = c("M", "F"),
                selected = hold$Sex,
                inline = T
              )
            ),
            
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                inputId = ns("Withdrawal"),
                label = "Withdrawal of Study",
                choices = c("Yes" = "Yes", "No" = "No"),
                selected = hold$Withdrawal,
                inline = T
              ),
              conditionalPanel(
                "input.Withdrawal == 'Yes'",
                ns = ns,
                dateInput(
                  inputId = "Withdrawal_date",
                  label = "Date",
                  language = "ko"
                ),
                textInput(
                  inputId = "Withdrawal_reason",
                  label = "Reason",
                  value = ifelse(is.null(hold), "", hold$Withdrawal_reason)
                ),
              )
            ),
            column(
              width = 2,
              disabled(
                radioButtons(
                  ns('CENTER'),
                  'Center',
                  c('삼성서울병원','전남대병원'),
                  selected = hold$Center
                )
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
    }

    observeEvent(
      input$Birthday,
      {
        updateNumericInput(
          session = session,
          inputId = "Age",
          value = as.numeric(as.period(interval(start = as_date(input$Birthday), end = Sys.Date()))$year)
        )
      }
    )

    observeEvent(
      input$model,
      {
        if (input$model == "") {
          shinyFeedback::showFeedbackDanger(
            inputId = "model",
            text = "Must enter model of car!",
            session = session
          )
          disable("submit")
        } else {
          shinyFeedback::hideFeedback(
            inputId = "model",
            session = session
          )
          enable("submit")
        }
      }
    )
  })

  # pid_demo, Group, DM, AMI_Type will not change

  edit_car_dat <- reactive({
    hold <- car_to_edit()

    dat <- list(
      "Initial" = ifelse(is.null(input$Initial), "", input$Initial),
      "Agree_Date" = ifelse(is.null(input$Agree_Date), "", input$Agree_Date),
      "Index_PCI_Date" = ifelse(is.null(input$Index_PCI_Date), "", input$Index_PCI_Date),
      "Birthday" = ifelse(is.null(input$Birthday), "", input$Birthday),
      "Age" = ifelse(is.null(input$Age), "", input$Age),
      "Sex" = ifelse(is.null(input$Sex), "", input$Sex),
      "Withdrawal" = ifelse(is.null(input$Withdrawal), "", input$Withdrawal),
      "SGLT" = ifelse(is.null(input$SGLT), "", input$SGLT),
      "Withdrawal_date" = ifelse(is.null(input$Withdrawal_date), "", input$Withdrawal_date),
      "Withdrawal_reason" = ifelse(is.null(input$Withdrawal_reason), "", input$Withdrawal_reason)
    )

    time_now <- as.character(with_tz(Sys.time(), tzone = "UTC"))

    dat$created_at <- as.character(hold$created_at)
    dat$created_by <- hold$created_by

    dat$modified_at <- time_now
    dat$modified_by <- sessionid

    return(dat)
  })

  # Logic to validate inputs...
  validate_edit <- eventReactive(
    input$submit,
    {
      dat <- edit_car_dat()
      dat
    }
  )

  observeEvent(
    validate_edit(),
    {
      removeModal()
      dat <- validate_edit()
      hold <- car_to_edit()
      sqlsub <- paste(paste0(names(dat), "=$", 1:length(dat)), collapse = ",")

      tryCatch(
        {
          dbExecute(
            conn,
            paste0("UPDATE ", tbl, " SET ", sqlsub, " WHERE pid='", hold$pid, "'"),
            params = c(
              unname(dat)
            )
          )

          session$userData$mtcars_trigger(session$userData$mtcars_trigger() + 1)
          showToast("success", paste0(modal_title, " Successs"))
        },
        error = function(error) {
          msg <- paste0(modal_title, " Error")
          print(msg)
          print(error)
          showToast("error", msg)
        }
      )
    }
  )
}
