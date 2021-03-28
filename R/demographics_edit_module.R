
#' Car Add & Edit Module
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
    choiceValues.DM <- c("0" ,"1")
    selected.DM <- NULL
    
    choices.AMI_Type <- c("NSTEMI", "STEMI")
    selected.AMI_Type <- NULL
    if (tbl == "rct"){
      choiceNames.DM <- ifelse(hold$DM == "0", "No", "Yes")
      choiceValues.DM <- hold$DM
      choices.AMI_Type <- hold$AMI_Type
      selected.AMI_Type <- hold$AMI_Type
    }
    
    
    

    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("pid"),
              'pid',
              choices = hold$pid,
              selected = hold$pid,
            ),
            radioButtons(
              ns("Group"), "Group", hold$Group, hold$Group, inline = T
            ),
            textInput(
              ns("Initial"),
              'Initial',
              value = hold$Initial
            ),
            dateInput(ns("Index_PCI_Date"), "Index PCI Date",  value = hold$Index_PCI_Date, language = "kr"),
            numericInput(
              ns('Age'),
              'Age',
              value = hold$Age,
              min = 19, max = 120,
              step = 1
            ),
            radioButtons(
              ns('Sex'),
              'Sex',
              choices = c('M', 'F'),
              selected = hold$Sex, inline =T
            ),
            numericInput(
              ns('Height'),
              'Height(cm)',
              value = hold$Height,
              min = 50, max = 300,
              step = 0.1
            ),
            numericInput(
              ns('Weight'),
              'Weight(kg)',
              value = hold$Weight,
              min = 0, max = 200,
              step = 0.1
            ),
            numericInput(
              ns('BMI'),
              'BMI',
              value = hold$BMI,
              min = 10, max = 40,
              step = 0.01
            ),
            radioButtons(
              ns('Smoking'),
              'Smoking',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Smoking, inline = T
            ),
            radioButtons(
              ns('AMI_Type'),
              'AMI Type',
              choices = choices.AMI_Type, selected.AMI_Type, inline = T
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns('HTN'),
              'Previous HTN',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$HTN, inline = T
            ),
            radioButtons(
              ns('DM'),
              'Previous DM',
              choiceNames = choiceNames.DM, choiceValues = choiceValues.DM,
              selected = selected.DM, inline = T
            ),
            radioButtons(
              ns('DM_Tx'),
              'Previous DM treatment',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$DM_Tx, inline = T
            ),
            radioButtons(
              ns('Dyslipidemia'),
              'Previous Dyslipidemia',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Dyslipidemia, inline = T
            ),
            radioButtons(
              ns('CKD'),
              'Previous CKD',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$CKD, inline = T
            ),
            radioButtons(
              ns('Dialysis'),
              'Previous Dialysis',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Dialysis, inline = T
            ),
            radioButtons(
              ns('Prev_Bleeding'),
              'Previous Bleeding',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Prev_Bleeding, inline = T
            ),
            radioButtons(
              ns('Prev_HF_Adm'),
              'Previous Heart failure admission',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Prev_HF_Adm, inline = T
            ),
            radioButtons(
              ns('Hx_MI'),
              'Previous MI',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Hx_MI, inline = T
            ),
            radioButtons(
              ns('Hx_PCI'),
              'Previous PCI',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Hx_PCI, inline = T
            ),
            radioButtons(
              ns('Hx_CABG'),
              'Previous CABG',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Hx_CABG, inline = T
            ),
            radioButtons(
              ns('Hx_CVA'),
              'Previous CVA',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Hx_CVA, inline = T
            ),
            radioButtons(
              ns('Hx_AF'),
              'Previous AF',
              choices = c('No' = 0, 'Yes' = 1, 'Unknown' = ""),
              selected = hold$Hx_AF, inline = T
            )
            
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

    dat = list(
      "Initial" = input$Initial,
      "Index_PCI_Date" = ifelse(is.null(input$Index_PCI_Date), "", as.character(input$Index_PCI_Date)),
      "Age" = input$Age, 
      "Sex" = ifelse(is.null(input$Sex), "", input$Sex),
      "Height" = input$Height,
      "Weight" = input$Weight,
      "BMI" = input$BMI,
      "Smoking" = ifelse(is.null(input$Smoking), "", input$Smoking),
      "HTN" = ifelse(is.null(input$HTN), "", input$HTN),
      "DM_Tx" = ifelse(is.null(input$DM_Tx), "", input$DM_Tx),
      "Dyslipidemia" = ifelse(is.null(input$Dyslipidemia), "", input$Dyslipidemia),
      "CKD" = ifelse(is.null(input$CKD), "", input$CKD),
      "Dialysis" = ifelse(is.null(input$Dialysis), "", input$Dialysis),
      "Prev_Bleeding" = ifelse(is.null(input$Prev_Bleeding), "", input$Prev_Bleeding),
      "Prev_HF_Adm" = ifelse(is.null(input$Prev_HF_Adm), "", input$Prev_HF_Adm),
      "Hx_MI" = ifelse(is.null(input$Hx_MI), "", input$Hx_MI),
      "Hx_PCI" = ifelse(is.null(input$Hx_PCI), "", input$Hx_PCI),
      "Hx_CABG" = ifelse(is.null(input$Hx_CABG), "", input$Hx_CABG),
      "Hx_CVA" = ifelse(is.null(input$Hx_CVA), "", input$Hx_CVA),
      "Hx_AF" = ifelse(is.null(input$Hx_AF), "", input$Hx_AF)
      
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

    tryCatch({

      dbExecute(
        conn,
        paste0("UPDATE ", tbl," SET 'Initial'=$1, Index_PCI_Date=$2, Age=$3, Sex=$4, Height=$5, Weight=$6, BMI=$7, Smoking=$8, HTN=$9,
          DM_Tx=$10, Dyslipidemia=$11, CKD=$12, Dialysis=$13, Prev_Bleeding=$14, Prev_HF_Adm=$15, Hx_MI=$16,Hx_PCI=$17,Hx_CABG=$18,Hx_CVA=$19, Hx_AF=$20,
          created_at=$21, created_by=$22,modified_at=$23, modified_by=$24 WHERE pid='", hold$pid, "'"),
        params = unname(dat)
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
