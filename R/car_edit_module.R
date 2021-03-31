
#' Enroll Add & Edit Module
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
car_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()
    idlist <- setdiff(rd$pid, data()$pid)

    choices.group <- rd[rd$pid == idlist[1], ]$Group
    if (tbl == "pros") {
      idlist <- setdiff(paste0("P-", 1:100000), data()$pid)
      choices.group <- levels(rd$Group)
    }

    choices.pid <- ifelse(is.null(hold), idlist[1], hold$pid)

    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("pid"),
              "pid",
              choices = choices.pid,
              selected = choices.pid[1],
            ),
            radioButtons(
              ns("Group"), "Group", choices.group, choices.group[1],
              inline = T
            ),
            textInput(
              ns("Initial"),
              "Initial",
              value = ifelse(is.null(hold), "", hold$Initial)
            ),
            numericInput(
              ns("Age"),
              "Age",
              value = ifelse(is.null(hold), NA, hold$Age),
              min = 0, max = 120,
              step = 1
            ),
            radioButtons(
              ns("Sex"),
              "Sex",
              choices = c("M", "F"),
              selected = ifelse(is.null(hold), "M", hold$Sex), inline = T
            ),
            numericInput(
              ns("Height"),
              "Height(cm)",
              value = ifelse(is.null(hold), NA, hold$Height),
              min = 50, max = 300,
              step = 0.1
            ),
            numericInput(
              ns("Weight"),
              "Weight(kg)",
              value = ifelse(is.null(hold), NA, hold$Weight),
              min = 0, max = 200,
              step = 0.1
            ),
            numericInput(
              ns("BMI"),
              "BMI",
              value = ifelse(is.null(hold), NA, hold$BMI),
              min = 10, max = 40,
              step = 0.01
            ),
            radioButtons(
              ns("Smoking"),
              "Smoking",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Smoking), inline = T
            ),
            selectInput(
              ns("AMI_Type"),
              "AMI Type",
              choices = c("0" = 0, "1" = 1, "2" = 2, "Unknown" = ""), selected = ifelse(is.null(hold), "", hold$AMI_Type)
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("HTN"),
              "HTN",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$HTN), inline = T
            ),
            radioButtons(
              ns("DM"),
              "DM",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$DM), inline = T
            ),
            radioButtons(
              ns("DM_Tx"),
              "DM_Tx",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$DM_Tx), inline = T
            ),
            radioButtons(
              ns("Dyslipidemia"),
              "Dyslipidemia",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Dyslipidemia), inline = T
            ),
            radioButtons(
              ns("CKD"),
              "CKD",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$CKD), inline = T
            ),
            radioButtons(
              ns("Dialysis"),
              "Dialysis",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Dialysis), inline = T
            ),
            radioButtons(
              ns("Prev_Bleeding"),
              "Previous Bleeding",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Prev_Bleeding), inline = T
            ),
            radioButtons(
              ns("Prev_HF_Adm"),
              "Prev_HF_Adm",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Prev_HF_Adm), inline = T
            ),
            radioButtons(
              ns("Hx_MI"),
              "Hx_MI",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Hx_MI), inline = T
            ),
            radioButtons(
              ns("Hx_PCI"),
              "Hx_PCI",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Hx_PCI), inline = T
            ),
            radioButtons(
              ns("Hx_CABG"),
              "Hx_CABG",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Hx_CABG), inline = T
            ),
            radioButtons(
              ns("Hx_CVA"),
              "Hx_CVA",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Hx_CVA), inline = T
            ),
            radioButtons(
              ns("Hx_AF"),
              "Hx_AF",
              choices = c("No" = 0, "Yes" = 1, "Unknown" = ""),
              selected = ifelse(is.null(hold), "", hold$Hx_AF), inline = T
            )
          )
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
        "pid" = input$pid,
        "Group" = input$Group,
        "Initial" = input$Initial,
        "Age" = input$Age,
        "Sex" = input$Sex,
        "Height" = input$Height,
        "Weight" = input$Weight,
        "BMI" = input$BMI,
        "Smoking" = input$Smoking,
        "AMI_Type" = input$AMI_Type,
        "HTN" = input$HTN,
        "DM" = input$DM,
        "DM_Tx" = input$DM_Tx,
        "Dyslipidemia" = input$Dyslipidemia,
        "CKD" = input$CKD,
        "Dialysis" = input$Dialysis,
        "Prev_Bleeding" = input$Prev_Bleeding,
        "Prev_HF_Adm" = input$Prev_HF_Adm,
        "Hx_MI" = input$Hx_MI,
        "Hx_PCI" = input$Hx_PCI,
        "Hx_CABG" = input$Hx_CABG,
        "Hx_CVA" = input$Hx_CVA,
        "Hx_AF" = input$Hx_AF
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

    tryCatch(
      {
        if (is.null(hold)) {
          dbExecute(
            conn,
            paste0("INSERT INTO ", tbl, " (pid, 'Group', Initial,Age,Sex,Height,Weight,BMI,Smoking,AMI_Type,HTN,DM,DM_Tx,Dyslipidemia,
          CKD,Dialysis,Prev_Bleeding,Prev_HF_Adm,Hx_MI,Hx_PCI,Hx_CABG,Hx_CVA, Hx_AF,
          created_at, created_by, modified_at, modified_by) VALUES
          ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26, $27)"),
            params = c(
              unname(dat$data)
            )
          )
        } else {
          # editing an existing car
          dbExecute(
            conn,
            paste0("UPDATE ", tbl, " SET 'Group'=$2, Initial=$3, Age=$4, Sex=$5, Height=$6, Weight=$7, BMI=$8, Smoking=$9, AMI_Type=$10, HTN=$11,
          DM=$12, DM_Tx=$13, Dyslipidemia=$14, CKD=$15, Dialysis=$16, Prev_Bleeding=$17, Prev_HF_Adm=$18, Hx_MI=$19,Hx_PCI=$20,Hx_CABG=$21,Hx_CVA=$22, Hx_AF=$23,
          created_at=$24, created_by=$25,modified_at=$26, modified_by=$27 WHERE pid='", dat$data[1], "'"),
            params = c(
              unname(dat$data)[-1]
            )
          )
        }

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
