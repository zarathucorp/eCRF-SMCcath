#' outc Add & Edit Module
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
outc_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        dateInput(
          ns("Discharge_out"),
          "Date of Discharge",
          value = hold$Discharge_out,
          language = "kr"
        ),
        radioButtons(
          ns("General_out"),
          label = "General Complication",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.General_out == '0'",
          checkboxGroupInput(
            ns("General_detail_out"),
            label = "",
            choices = c(
              "CHF" = 0, "Emergency PCI" = 1, "Emergency CABG" = 2, "Cardiogenic Shock" = 3,
              "Contrast Allergic Reaction" = 4, "Tamponade" = 5, "Bleeding at Access Site" = 6,
              "Retroperitoneal Bleeding" = 7, "Access Site Occlusion" = 8, "Dissection" = 9,
              "AV Fistula" = 10, "Peripheral Embolization" = 11, "Pseudoaneurysm" = 12,
              "CIN" = 13, "Others" = 14
            ),
            selected = NULL,
            inline = TRUE
          ),
          conditionalPanel(
            "input.General_detail_out.checkbox == 14",
            textInput(
              "General_detail_others_out",
              label = "",
              value = ifelse(is.null(hold), NA, hold$General_detail_others_out)
            ),
          )
        ),

        # Medication Data

        radioButtons(
          ns("Aspirin_out"),
          label = "Aspirin",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Aspirin_out
        ),
        radioButtons(
          ns("Clopidogrel_out"),
          label = "Clopidogrel",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Clopidogrel_out
        ),
        radioButtons(
          ns("Prasugrel_out"),
          label = "Prasugrel",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Prasugrel_out
        ),
        radioButtons(
          ns("Ticagrelor_out"),
          label = "Ticagrelor",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Ticagrelor_out
        ),
        radioButtons( # Beta Blocker
          ns("BB_out"),
          label = "Beta Blocker",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$BB_out
        ),
        radioButtons(
          ns("WN_out"),
          label = "Wafarin or NOAC",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$WN_out
        ),
        radioButtons(
          ns("Statin_out"),
          label = "Statin",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Statin_out
        ),
        radioButtons(
          ns("ACE_out"),
          label = "ACE Inhibitor or ARB",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$ACE_out
        ),
        radioButtons(
          ns("Nitrate_out"),
          label = "Nitrate (Sigmart)",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Nitrate_out
        ),
        radioButtons(
          ns("Calcium_out"),
          label = "Calcium channel antagonist",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Calcium_out
        ),
        radioButtons(
          ns("Trimetazidine_out"),
          label = "Trimetazidine (Vastinan)",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Trimetazidine_out
        ),

        # Clinical Events

        radioButtons(
          ns("Events_out"),
          label = "Clinical Events",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.Events_out == 0",
          checkboxGroupInput(
            ns("Events_detail_out"),
            label = "",
            choices = c("Death" = 0, "MI" = 1, "Repeat Revascularization" = 2, "Stent Thrombosis" = 3, "CVA" = 4),
            selected = NULL,
            inline = TRUE
          )
        ),

        # Death

        conditionalPanel(
          'input.Events_out.checkbox=="0"',
          dateInput(
            ns("Death_date_out"),
            "",
            value = hold$Death_date_out,
            language = "kr"
          ),
          radioButtons(
            ns("Death_cause_out"),
            "",
            choices = c("Cardiac Death" = 0, "Non-Cardiovascular Death" = 1, "Unknown Origin Death" = 2),
            selected = NULL,
            inline = TRUE
          )
        ),

        # Myocardial Infarction

        conditionalPanel(
          'input.Events_out.includes("1")',
          dateInput(
            ns("MI_date_out"),
            "Date",
            value = hold$MI_date_out,
            language = "kr"
          ),
          textInput(
            "MI_Segment_out",
            label = "Segment",
            value = ifelse(is.null(hold), NA, hold$MI_Segment_out)
          ),
          radioButtons(
            ns("MI_Type_out"),
            "Type",
            choices = c("STEMI" = 0, "NSTEMI" = 1),
            selected = NULL,
            inline = TRUE
          ),

          # ? 72시간 이나에 CK_MB > URL * 10 Or URL*5 + 도움말 참조
          radioButtons(
            ns("MI_Pre_out"),
            "Pre Procedural MI",
            choices = c("Yes" = 0, "No" = 1),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("MI_ST_out"),
            "Related with Stent Thrombosis",
            choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("MI_TL_out"),
            "Related with Target Lesion",
            choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("MI_TV_out"),
            "Related with Target Vessel",
            choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
            selected = NULL,
            inline = TRUE
          ),
          checkboxGroupInput(
            ns("MI_Treat_out"),
            label = "Type of Treatment",
            choices = c("Medication Only" = 0, "Thrombolysis" = 1, "only Ballooning" = 2, "Stenting" = 3, "Bypass Surgery" = 4),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("MI_After_out"),
            "After Treatment",
            choices = c("Recovered" = 0, "Death" = 1, "Unknown" = 2),
            selected = NULL,
            inline = TRUE
          )
        ),


        # Revascularization

        conditionalPanel(
          'input.Events_out.includes("2")',
          dateInput(
            ns("RV_date_out"),
            "Date",
            value = hold$RV_date_out,
            language = "kr"
          ),
          textInput(
            "RV_Segment_out",
            label = "Segment",
            value = ifelse(is.null(hold), NA, hold$RV_Segment_out)
          ),
          radioButtons(
            ns("RV_CD_out"),
            "Clinically Driven",
            choices = c("Yes" = 0, "No" = 1),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("RV_ID_out"),
            "Ischemia Driven",
            choices = c("Yes" = 0, "No" = 1),
            selected = NULL,
            inline = TRUE
          ),
          checkboxGroupInput(
            ns("RV_Treat_out"),
            label = "Type of Treatment",
            choices = c("only Ballooning" = 0, "Stenting" = 1, "Bypass Surgery" = 2),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("RV_TL_out"),
            "Related with Target Lesion",
            choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("RV_TV_out"),
            "Related with Target Vessel",
            choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("RV_AVP_out"),
            "Another Vessel PCI",
            choices = c("Yes" = 0, "No" = 1),
            selected = NULL,
            inline = TRUE
          )
        ),

        # Stent Thrombosis

        conditionalPanel(
          'input.Events_out.includes("3")',
          dateInput(
            ns("ST_date_out"),
            "Date",
            value = hold$ST_date_out,
            language = "kr"
          ),
          textInput(
            "ST_Segment_out",
            label = "Segment",
            value = ifelse(is.null(hold), NA, hold$ST_Segment_out)
          ),
          radioButtons(
            ns("ST_Type_out"),
            "Type",
            choices = c("Acute (< 1d)" = 0, "Subacute (1-30d)" = 1, "Late (>1m)" = 2, "Very Late(>1y)" = 3),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("ST_ARC_out"),
            "ARC",
            choices = c("Definite/Confirmed" = 0, "Probable" = 1, "Possible" = 2),
            selected = NULL,
            inline = TRUE
          ),
          checkboxGroupInput(
            ns("ST_Clinical_out"),
            label = "Clinical Features",
            choices = c("Sudden Death" = 0, "STEMI" = 1, "NSTEMI" = 2, "Unstable Angina" = 3, "Stable Angina" = 4, "Other" = 5),
            selected = NULL,
            inline = TRUE
          ),
          conditionalPanel(
            "input.ST_Clinical_out == 5",
            textInput(
              "ST_Clinical_other_out",
              label = "",
              value = ifelse(is.null(hold), NA, hold$ST_Clinical_other_out)
            ),
          ),
        ),

        # CVA

        conditionalPanel(
          'input.Events_out.includes("4")',
          dateInput(
            ns("CVA_date_out"),
            "Date",
            value = hold$CVA_date_out,
            language = "kr"
          ),
          radioButtons(
            ns("CVA_Type_out"),
            "Type",
            choices = c("Ischemic" = 0, "Hemorrhagic" = 1, "Unknown" = 2),
            selected = NULL,
            inline = TRUE
          ),
          radioButtons(
            ns("CVA_VIS_out"),
            "Verified with Imaging Studies",
            choices = c("Yes" = 0, "No" = 1),
            selected = NULL,
            inline = TRUE
          )
        ),

        # Any AE / SAE ...

        textAreaInput(
          ns("Comment_out"),
          "Comment",
          width = "400px",
          height = "100px",
          value =  ifelse(is.null(hold$Comment_out), "", hold$Comment_out),
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
        "Discharge_out" = ifelse(is.null(input$Discharge_out), "", as.character(input$Discharge_out)),
        "Death_date_out" = ifelse(is.null(input$Death_date_out), "", as.character(input$Death_date_out)),
        "MI_date_out" = ifelse(is.null(input$MI_date_out), "", as.character(input$MI_date_out)),
        "RV_date_out" = ifelse(is.null(input$RV_date_out), "", as.character(input$RV_date_out)),
        "ST_date_out" = ifelse(is.null(input$ST_date_out), "", as.character(input$ST_date_out)),
        "CVA_date_out" = ifelse(is.null(input$CVA_date_out), "", as.character(input$CVA_date_out)),
        "General_out" = ifelse(is.null(input$General_out), "", input$General_out),
        "General_detail_out" = ifelse(is.null(input$General_detail_out), "", input$General_detail_out),
        "General_detail_others_out" = ifelse(is.null(input$General_detail_others_out), "", input$General_detail_others_out),
        "Aspirin_out" = ifelse(is.null(input$Aspirin_out), "", input$Aspirin_out),
        "Clopidogrel_out" = ifelse(is.null(input$Clopidogrel_out), "", input$Clopidogrel_out),
        "Prasugrel_out" = ifelse(is.null(input$Prasugrel_out), "", input$Prasugrel_out),
        "Ticagrelor_out" = ifelse(is.null(input$Ticagrelor_out), "", input$Ticagrelor_out),
        "BB_out" = ifelse(is.null(input$BB_out), "", input$BB_out),
        "WN_out" = ifelse(is.null(input$WN_out), "", input$WN_out),
        "Statin_out" = ifelse(is.null(input$Statin_out), "", input$Statin_out),
        "ACE_out" = ifelse(is.null(input$ACE_out), "", input$ACE_out),
        "Nitrate_out" = ifelse(is.null(input$Nitrate_out), "", input$Nitrate_out),
        "Calcium_out" = ifelse(is.null(input$Calcium_out), "", input$Calcium_out),
        "Trimetazidine_out" = ifelse(is.null(input$Trimetazidine_out), "", input$Trimetazidine_out),
        "Events_out" = ifelse(is.null(input$Events_out), "", input$Events_out),
        "Events_detail_out" = ifelse(is.null(input$Events_detail_out), "", input$Events_detail_out),
        "Death_cause_out" = ifelse(is.null(input$Death_cause_out), "", input$Death_cause_out),
        "MI_Segment_out" = ifelse(is.null(input$MI_Segment_out), "", input$MI_Segment_out),
        "MI_Type_out" = ifelse(is.null(input$MI_Type_out), "", input$MI_Type_out),
        "MI_Pre_out" = ifelse(is.null(input$MI_Pre_out), "", input$MI_Pre_out),
        "MI_ST_out" = ifelse(is.null(input$MI_ST_out), "", input$MI_ST_out),
        "MI_TL_out" = ifelse(is.null(input$MI_TL_out), "", input$MI_TL_out),
        "MI_TV_out" = ifelse(is.null(input$MI_TV_out), "", input$MI_TV_out),
        "MI_Treat_out" = ifelse(is.null(input$MI_Treat_out), "", input$MI_Treat_out),
        "MI_After_out" = ifelse(is.null(input$MI_After_out), "", input$MI_After_out),
        "RV_Segment_out" = ifelse(is.null(input$RV_Segment_out), "", input$RV_Segment_out),
        "RV_CD_out" = ifelse(is.null(input$RV_CD_out), "", input$RV_CD_out),
        "RV_ID_out" = ifelse(is.null(input$RV_ID_out), "", input$RV_ID_out),
        "RV_Treat_out" = ifelse(is.null(input$RV_Treat_out), "", input$RV_Treat_out),
        "RV_TL_out" = ifelse(is.null(input$RV_TL_out), "", input$RV_TL_out),
        "RV_TV_out" = ifelse(is.null(input$RV_TV_out), "", input$RV_TV_out),
        "RV_AVP_out" = ifelse(is.null(input$RV_AVP_out), "", input$RV_AVP_out),
        "ST_Segment_out" = ifelse(is.null(input$ST_Segment_out), "", input$ST_Segment_out),
        "ST_Type_out" = ifelse(is.null(input$ST_Type_out), "", input$ST_Type_out),
        "ST_ARC_out" = ifelse(is.null(input$ST_ARC_out), "", input$ST_ARC_out),
        "ST_Clinical_out" = ifelse(is.null(input$ST_Clinical_out), "", input$ST_Clinical_out),
        "ST_Clinical_other_out" = ifelse(is.null(input$ST_Clinical_other_out), "", input$ST_Clinical_other_out),
        "CVA_Type_out" = ifelse(is.null(input$CVA_Type_out), "", input$CVA_Type_out),
        "CVA_VIS_out" = ifelse(is.null(input$CVA_VIS_out), "", input$CVA_VIS_out),
        "Comment_out" = ifelse(is.null(input$Comment_out), "", input$Comment_out)
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
