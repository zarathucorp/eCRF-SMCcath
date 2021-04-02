
#' Lab Add & Edit Module
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
lab_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        
        dateInput(
          ns("Lab_Date"),
          "Lab date",
          value = hold$Lab_Date,
          language = "kr"
        ),
        
        numericInput(
          ns("WBC"),
          "WBC",
          value = ifelse(is.null(hold), NA, hold$WBC),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Hb"),
          "Hb",
          value = ifelse(is.null(hold), NA, hold$Hb),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Plt"),
          "Plt",
          value = ifelse(is.null(hold), NA, hold$Plt),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Cholesterol"),
          "Cholesterol",
          value = ifelse(is.null(hold), NA, hold$Cholesterol),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("TG"),
          "TG",
          value = ifelse(is.null(hold), NA, hold$TG),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("HDL"),
          "HDL",
          value = ifelse(is.null(hold), NA, hold$HDL),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("LDL"),
          "LDL",
          value = ifelse(is.null(hold), NA, hold$LDL),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("BUN"),
          "BUN",
          value = ifelse(is.null(hold), NA, hold$BUN),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Creatinine"),
          "Creatinine",
          value = ifelse(is.null(hold), NA, hold$Creatinine),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("eCFR"),
          "eCFR",
          value = ifelse(is.null(hold), NA, hold$eCFR),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Bilirubin"),
          "Bilirubin",
          value = ifelse(is.null(hold), NA, hold$Bilirubin),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("AST"),
          "AST",
          value = ifelse(is.null(hold), NA, hold$AST),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("ALT"),
          "ALT",
          value = ifelse(is.null(hold), NA, hold$ALT),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("hsCRP"),
          "hsCRP",
          value = ifelse(is.null(hold), NA, hold$hsCRP),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("HbA1c"),
          "HbA1c",
          value = ifelse(is.null(hold), NA, hold$HbA1c),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Pre_CK_MB"),
          "Pre_CK_MB",
          value = ifelse(is.null(hold), NA, hold$Pre_CK_MB),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Pre_cTnT"),
          "Pre_cTnT",
          value = ifelse(is.null(hold), NA, hold$Pre_cTnT),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Post_Peak_CK_MB"),
          "Post_Peak_CK_MB",
          value = ifelse(is.null(hold), NA, hold$Post_Peak_CK_MB),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Post_Peak_cTnT"),
          "Post_Peak_cTnT",
          value = ifelse(is.null(hold), NA, hold$Post_Peak_cTnT),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("NT_proBNP_Baseline"),
          "NT_proBNP_Baseline",
          value = ifelse(is.null(hold), NA, hold$NT_proBNP_Baseline),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Lactic_Acid_Pre"),
          "Lactic_Acid_Pre",
          value = ifelse(is.null(hold), NA, hold$Lactic_Acid_Pre),
          min = 0, max = 120,
          step = 1
        ),
        
        numericInput(
          ns("Lactic_Acid_Peak"),
          "Lactic_Acid_Peak",
          value = ifelse(is.null(hold), NA, hold$Lactic_Acid_Peak),
          min = 0, max = 120,
          step = 1
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
        "Lab_Date" = as.character(input$Lab_Date),
        "WBC" = input$WBC,
        "Hb" = input$Hb,
        "Plt" = input$Plt,
        "Cholesterol" = input$Cholesterol,
        "TG" = input$TG,
        "LDL" = input$LDL,
        "HDL" = input$HDL,
        "BUN" = input$BUN,
        "Creatinine" = input$Creatinine,
        "eCFR" = input$eCFR,
        "Bilirubin" = input$Bilirubin,
        "AST" = input$AST,
        "ALT" = input$ALT,
        "hsCRP" = input$hsCRP,
        "HbA1c" = input$HbA1c,
        "Pre_CK_MB" = input$Pre_CK_MB,
        "Pre_cTnT" = input$Pre_cTnT,
        "Post_Peak_CK_MB" = input$Post_Peak_CK_MB,
        "Post_Peak_cTnT" = input$Post_Peak_cTnT,
        "NT_proBNP_Baseline" = input$NT_proBNP_Baseline,
        "Lactic_Acid_Pre" = input$Lactic_Acid_Pre,
        "Lactic_Acid_Peak" = input$Lactic_Acid_Peak
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
