#' Demographics Add & Edit Module
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
demographics_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    choiceNames.DM <- c("No", "Yes")
    choiceValues.DM <- c("0", "1")
    selected.DM <- NULL

    choices.AMI_Type <- c("NSTEMI", "STEMI")
    selected.AMI_Type <- NULL
    if (tbl == "rct") {
      choiceNames.DM <- ifelse(hold$DM == "0", "No", "Yes")
      choiceValues.DM <- hold$DM
      #choices.AMI_Type <- hold$AMI_Type
      selected.AMI_Type <- hold$AMI_Type
    }
    
    # CHECK !!! as.numeric으로 풀어야 됨.
    
    output$pid_demo <- renderText(hold$pid)
    #output$PCI_Date_demo <- renderText(paste0("시술일자 : ", as.character(lubridate::as_date(as.numeric(hold$Index_PCI_Date)))))
    #output$Birthday_demo <- renderText(paste0("Birth Date : ", as.character(lubridate::as_date(hold$Birthday))))
    
    if (tbl == "rct"){
      showModal(
        modalDialog(
          h5('Subject No'),
          textOutput(
            ns('pid_demo')
          ),
          #selectInput(
          #  ns("pid"),
          #  "Subject No",
          #  choices = hold$pid,
          #  selected = hold$pid,
          #),
          radioButtons(
            ns("Group"),
            "Allocation Group",
            hold$Group,
            hold$Group,
            inline = T
          ),
          textInput(
            ns("Initial"),
            "Initial",
            value = hold$Initial
          ),
          dateInput(
            ns("Agree_Date"),
            "동의서 서명일",
            value = lubridate::as_date(as.numeric(hold$Agree_Date)),
            language = "kr"
          ),
          
          dateInput(
            ns("Index_PCI_Date"),
            "시술일자",
            value = lubridate::as_date(as.numeric(hold$Index_PCI_Date)),
            language = "kr"
          ),
          #fluidRow(
          #  column(
          #    width = 6, 
          #    textOutput(
          #      ns('Birthday_demo')
          #    )
          #  ),
          #  column(
          #    width = 6, 
          #    textOutput(
          #      ns('PCI_Date_demo')
          #    )
          #  )
          #),
          dateInput(
            ns("Birthday"),
            "Date of Birth",
            value = lubridate::as_date(hold$Birthday),
            language = "kr"
          ),
          h5("Age"),
          textOutput(
            ns("Age")
          ),
          #        numericInput(
          #          ns("Age"),
          #          "Age",
          #          value = 
          #          min = 19, max = 120,
          #          step = 1
          #        ) 
          radioButtons(
            ns("Sex"),
            "Sex",
            choices = c("M", "F"),
            selected = hold$Sex, inline = T
          ),
          shinyjs::disabled(
            radioButtons(
              ns("DM"),
              "Previous DM",
              choices = c("No" = 0, "Yes" = 1),
              selected = hold$DM, inline = T
            )
          ),
          
          #radioButtons(
          #  ns("DM_Tx"),
          #  "Previous DM treatment",
          #  choices = c("No" = 0, "Yes" = 1, "Unknown" = 2),
          #  selected = hold$DM_Tx, inline = T
          #),
          
          shinyjs::disabled(
            radioButtons( # 층화?
              ns("AMI_Type"),
              "AMI Type",
              choices = choices.AMI_Type, 
              selected.AMI_Type, 
              inline = T
            )
          ),
          radioButtons(
            ns("Withdrawal"),
            "Withdrawal of Study",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$Withdrawal,
            inline = T
          ),
          conditionalPanel(
            "input.Withdrawal == 0",
            ns = ns,
            dateInput(
              "Withdrawal_date",
              "",
              language = "kr"
            ),
            textInput(
              "Withdrawal_reason",
              label = "Reason",
              value = ifelse(is.null(hold), NA, hold$Withdrawal_reason)
            ),
          ),
          textAreaInput(
            ns("Comment_demo"),
            "Comment",
            width = "400px",
            height = "100px"
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
    } else{
      showModal(
        modalDialog(
          h5('Subject No'),
          textOutput(
            ns('pid_demo')
          ),
          #selectInput(
          #  ns("pid"),
          #  "Subject No",
          #  choices = hold$pid,
          #  selected = hold$pid,
          #),
          radioButtons(
            ns("Group"),
            "Allocation Group",
            hold$Group,
            hold$Group,
            inline = T
          ),
          textInput(
            ns("Initial"),
            "Initial",
            value = hold$Initial
          ),
          dateInput(
            ns("Agree_Date"),
            "동의서 서명일",
            value = lubridate::as_date(as.numeric(hold$Agree_Date)),
            language = "kr"
          ),
          
          dateInput(
            ns("Index_PCI_Date"),
            "시술일자",
            value = lubridate::as_date(as.numeric(hold$Index_PCI_Date)),
            language = "kr"
          ),
          #fluidRow(
          #  column(
          #    width = 6, 
          #    textOutput(
          #      ns('Birthday_demo')
          #    )
          #  ),
          #  column(
          #    width = 6, 
          #    textOutput(
          #      ns('PCI_Date_demo')
          #    )
          #  )
          #),
          dateInput(
            ns("Birthday"),
            "Date of Birth",
            value = lubridate::as_date(hold$Birthday),
            language = "kr"
          ),
          h5("Age"),
          textOutput(
            ns("Age")
          ),
          #        numericInput(
          #          ns("Age"),
          #          "Age",
          #          value = 
          #          min = 19, max = 120,
          #          step = 1
          #        ) 
          radioButtons(
            ns("Sex"),
            "Sex",
            choices = c("M", "F"),
            selected = hold$Sex, inline = T
          ),
          radioButtons(
            ns("Withdrawal"),
            "Withdrawal of Study",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$Withdrawal,
            inline = T
          ),
          conditionalPanel(
            "input.Withdrawal == 0",
            ns = ns,
            dateInput(
              "Withdrawal_date",
              "",
              language = "kr"
            ),
            textInput(
              "Withdrawal_reason",
              label = "Reason",
              value = ifelse(is.null(hold), NA, hold$Withdrawal_reason)
            ),
          ),
          textAreaInput(
            ns("Comment_demo"),
            "Comment",
            width = "400px",
            height = "100px"
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

    # Observe event for "Model" text input in Add/Edit Car Modal
    # `shinyFeedback`
    
    observeEvent(input$Birthday,{
      output$Age <- renderText(as.period(interval(start = lubridate::as_date(hold$Birthday), end = Sys.Date()))$year)
    })
    
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

    dat <- list(
      "Initial" = ifelse(is.null(input$Initial), "", input$Initial),
      "Agree_Date" = ifelse(is.null(input$Agree_Date), "", input$Agree_Date),
      "Index_PCI_Date" = ifelse(is.null(input$Index_PCI_Date), "", input$Index_PCI_Date),
      "Birthday" = ifelse(is.null(input$Birthday), "", input$Birthday),
      "Age" = ifelse(is.null(input$Birthday), "", as.period(interval(start = lubridate::as_date(input$Birthday), end = Sys.Date()))$year),
      "Sex" = ifelse(is.null(input$Sex), "", input$Sex),
      "Withdrawal" = ifelse(is.null(input$Withdrawal), "", input$Withdrawal),
      "Withdrawal_date" = ifelse(is.null(input$Withdrawal_date), "", input$Withdrawal_date),
      "Withdrawal_reason" = ifelse(is.null(input$Withdrawal_reason), "", input$Withdrawal_reason),
      "Comment_demo" = ifelse(is.null(input$Comment_demo), "", input$Comment_demo)
    )

    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

    dat$created_at <- as.character(hold$created_at)
    dat$created_by <- hold$created_by

    dat$modified_at <- time_now
    dat$modified_by <- sessionid

    return(dat)
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
