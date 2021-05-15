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
            "input.Reason_M3 == 2", # if Other : Show text box.
            ns = ns,
            textAreaInput(
              ns("Other_M3"),
              "Other",
              width = "400px",
              height = "100px"
            )
          ),
          dateInput(
            ns("LastFU_M3"),
            "Date of Last F/U",
            value = hold$LastFU_M3,
            language = "kr"
          )
          ## Implement Duration
        ),

        ## ALL ND
        actionButton(ns("ALLND_1"), "ALL ND", class = "btn btn-default"),
        textInput(
          ns("SBP_M3"), # Systolic BP
          "Systolic BP",
          value = ifelse(is.null(hold), NA, hold$SBP_M3)
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
          ns("Cardiac_Status_M3"),
          "Cardiac Status",
          choices = c("Unknown" = 0, "Asymptomatic" = 1, "Angina" = 2, "Other" = 3),
          selected = hold$Cardiac_Status_M3, inline = T
        ),
        conditionalPanel(
          "input.Cardiac_Status_M3 == 2", # if Angina, show CCS I, II, III, IV
          ns = ns,
          radioButtons(
            ns("CCS_M3"),
            "CCS",
            choices = c("I" = 0, "II" = 1, "III" = 2, "IV" = 3),
            selected = hold$CCS_M3, inline = T
          )
        ),
        conditionalPanel(
          "input.Cardiac_Status_M3 == 3", # if Other, show textInput
          ns = ns,
          textAreaInput(
            ns("Other_Cardiac_Status_M3"),
            "Other",
            width = "400px",
            height = "100px"
          )
        ),
        radioButtons(
          ns("Readmission_M3"),
          "Readmission",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Readmission_M3, inline = T
        ),
        conditionalPanel(
          "input.Readmission_M3 == 0",
          ns = ns,
          
          dateInput(
            ns('Readmission_Date_M3'),
            "Readmission Date",
            value = lubridate::as_date(hold$Readmission_Date_M3),
            language = "kr"
          ),
          
          checkboxGroupInput(
            ns("Readmission_reason_M3"),
            label = "",
            choices = c("Cardiac Reason" = 0, "Non-Cardiac Reason" = 1),
            selected = NULL,
            inline = TRUE
          ),
          textAreaInput(
            ns("Readmission_reason_text_M3"),
            "Reason",
            width = "400px",
            height = "100px"
          )
        ),
        radioButtons(
          ns("ECG_Rhythm_M3"),
          "ECG Rhythm",
          choices = c("Sinus Rhytum" = 0, "Atrial Fibrillation" = 1, "Others" = 2, "Not Done" = 3),
          selected = hold$ECG_Rhythm_M3,
          inline = T
        ),
        conditionalPanel(
          "input.ECG_Rhythm_M3 == 2",
          ns = ns,
          textAreaInput(
            ns("ECG_Rhythm_Other_M3"),
            "Others",
            width = "400px",
            height = "100px"
          )
        ),

        # Stress ECG
        radioButtons(
          ns("Stress_ECG_M3"),
          "Stress ECG (TMT)",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Stress_ECG_M3,
          inline = T
        ),
        conditionalPanel(
          "input.Stress_ECG_M3 == 0",
          ns = ns,
          dateInput(
            ns("Stress_ECG_Date_M3"),
            "",
            value = hold$Stress_ECG_Date_M3,
            language = "kr"
          ),
          radioButtons(
            ns("Stress_ECG_Detail_M3"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$Stress_ECG_Detail_M3,
            inline = T
          )
        ),

        # Lab Data
        radioButtons(
          ns("Lab_data_Unknown_M3"),
          label = "Date Unknown",
          choices = c("Yes" = 0, "No" = 1)
        ),
        conditionalPanel(
          "input.Lab_data_Unknown_M3 == 1",
          dateInput(
            ns("Lab_Data_M3"),
            "Date of Lab",
            value = hold$Lab_Data_M3,
            language = "kr"
          ),
        ),
        ## All ND
        numericInput(
          ns("WBC_M3"),
          "WBC",
          value = ifelse(is.null(hold), NA, hold$WBC_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TC_M3"),
          "Total Chol",
          value = ifelse(is.null(hold), NA, hold$TC_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Hb_M3"),
          "Hb",
          value = ifelse(is.null(hold), NA, hold$Hb_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TG_M3"),
          "TG",
          value = ifelse(is.null(hold), NA, hold$TG_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Platelet_M3"),
          "Platelet",
          value = ifelse(is.null(hold), NA, hold$Platelet_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HDL_M3"),
          "HDL",
          value = ifelse(is.null(hold), NA, hold$HDL_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BUN_M3"),
          "BUN",
          value = ifelse(is.null(hold), NA, hold$BUN_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_M3"),
          "LDL",
          value = ifelse(is.null(hold), NA, hold$LDL_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Cr_M3"),
          "Cr",
          value = ifelse(is.null(hold), NA, hold$Cr_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_Cal_M3"),
          "LDL, cal",
          value = ifelse(is.null(hold), NA, hold$LDL_Cal_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BNP_M3"),
          "BNP",
          value = ifelse(is.null(hold), NA, hold$BNP_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("FG_M3"),
          "Fasting Glucose",
          value = ifelse(is.null(hold), NA, hold$FG_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Pro_BNP_M3"),
          "pro BNP",
          value = ifelse(is.null(hold), NA, hold$Pro_BNP_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HbA1C_M3"),
          "HbA1C",
          value = ifelse(is.null(hold), NA, hold$HbA1C_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("N_Pro_BNP_M3"),
          "N-pro BNP",
          value = ifelse(is.null(hold), NA, hold$N_Pro_BNP_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("hs_CRP_M3"),
          "hs-CRP",
          value = ifelse(is.null(hold), NA, hold$hs_CRP_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("CK_MB_M3"),
          "Platelet",
          value = ifelse(is.null(hold), NA, hold$Platelet_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Troponin_I_M3"),
          "Troponin I",
          value = ifelse(is.null(hold), NA, hold$Troponin_I_M3),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Troponin_T_M3"),
          "Troponin T",
          value = ifelse(is.null(hold), NA, hold$Troponin_T_M3),
          min = 0, max = 120,
          step = 1
        ),


        # Medication Data
        radioButtons(
          ns("Aspirin_M3"),
          label = "Aspirin",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Aspirin_M3
        ),
        radioButtons(
          ns("Clopidogrel_M3"),
          label = "Clopidogrel",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Clopidogrel_M3
        ),
        radioButtons(
          ns("Prasugrel_M3"),
          label = "Prasugrel",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Prasugrel_M3
        ),
        radioButtons(
          ns("Ticagrelor_M3"),
          label = "Ticagrelor",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Ticagrelor_M3
        ),
        radioButtons( # Beta Blocker
          ns("BB_M3"),
          label = "Beta Blocker",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$BB_M3
        ),
        radioButtons(
          ns("WN_M3"),
          label = "Wafarin or NOAC",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$WN_M3
        ),
        radioButtons(
          ns("Statin_M3"),
          label = "Statin",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Statin_M3
        ),
        radioButtons(
          ns("ACE_M3"),
          label = "ACE Inhibitor or ARB",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$ACE_M3
        ),
        radioButtons(
          ns("Nitrate_M3"),
          label = "Nitrate (Sigmart)",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Nitrate_M3
        ),
        radioButtons(
          ns("Calcium_M3"),
          label = "Calcium channel antagonist",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Calcium_M3
        ),
        radioButtons(
          ns("Trimetazidine_M3"),
          label = "Trimetazidine",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Trimetazidine_M3
        ),

        # Clinical Events
        radioButtons(
          ns("Event_M3"),
          "Clinical Events",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Event_M3
        ),
        conditionalPanel(
          "input.Event_M3 == 0",
          ns = ns,
          checkboxGroupInput(
            ns("Event_Details_M3"),
            "",
            choices = c("Death" = 0, "MI" = 1, "Repeat Revascularization" = 2, "Stent Thrombosis" = 3, "CVA" = 4),
            selected = hold$Event_Details_M3
          ),
          # Death
          conditionalPanel(
            'input.Event_Details_M3.includes("0")',
            ns = ns,
            dateInput(
              "Death_date_M3",
              label = "Date",
              language = "kr"
            ),
            radioButtons(
              "Death_reason_M3",
              label = "",
              choices = c("Cardiac Death" = 1, "Non-Cardiovascular Death" = 2, "Unknown Origin Death" = 3)
            )
          ),

          # MI

          conditionalPanel(
            'input.Event_Details_M3.includes("1")',
            ns = ns,
            dateInput(
              "MI_date_M3",
              label = "Date",
              language = "kr"
            ),
            numericInput(
              "MI_Segment_M3",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$MI_Segment_M3),
              min = 0, max = 120,
              step = 1
            ),
            radioButtons(
              "MI_Type_M3",
              label = "Type", choices = c("STEMI" = 0, "NSTEMI" = 1), inline = TRUE
            ),
            radioButtons(
              "MI_Stent_M3",
              label = "Related with Stent Thrombosis", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ),
            radioButtons(
              "MI_Lesion_M3",
              label = "Related with Target Lesion", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ),
            radioButtons(
              "MI_Vessel_M3",
              label = "Related with Target Vessel", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ), # Hover popup

            checkboxGroupInput(
              "MI_Treatment_M3",
              label = "Type of Treatment",
              choices = c("Medication Only" = 0, "Thrombolysis" = 1, "only Ballooning" = 2, "Stenting" = 3, "Bypass Surgery" = 4)
            ),
            radioButtons(
              "MI_After_M3",
              label = "After Treatment",
              choices = c("Recovered" = 0, "Death" = 1, "Unknown" = 2)
            )
          ),

          # Revascularization
          conditionalPanel(
            'input.Event_Details_M3.includes("2")',
            ns = ns,
            dateInput(
              "Rev_date_M3",
              label = "Date",
              language = "kr"
            ),
            numericInput(
              "Rev_Segment_M3",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$Rev_Segment_M3),
              min = 0, max = 120,
              step = 1
            ),
            checkboxGroupInput(
              "Rev_Treatment_M3",
              label = "Type of Treatment",
              choices = c("only Ballooning" = 0, "Stenting" = 1, "Bypass Surgery" = 2)
            ),
            radioButtons(
              "Rev_Lesion_M3",
              label = "Related with Target Lesion",
              choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
              inline = TRUE
            ),
            radioButtons(
              "Rev_Vessel_M3",
              label = "Related with Target Vessel",
              choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
              inline = TRUE
            ), # Hover popup

            radioButtons(
              "Rev_PCI_M3",
              label = "Another Vessel PCI",
              choices = c("Yes" = 0, "No" = 1)
            )
          ),

          # Stent Thrombosis
          conditionalPanel(
            'input.Event_Details_M3.includes("3")',
            ns = ns,
            dateInput(
              "Stent_date_M3",
              label = "Date",
              language = "kr"
            ),
            textInput(
              "Stent_Segment_M3",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$Stent_Segment_M3)
            ),
            radioButtons(
              "Stent_Type_M3",
              label = "Type",
              choices = c("Acute (< 1d)" = 0, "Subacute (1-30d)" = 1, "Late(> 1m)" = 2, "Very Late(> 1y)" = 3), inline = TRUE
            ),
            radioButtons(
              "Stent_arc_M3",
              label = "ARC", choices = c("Definite/Confirmed" = 0, "Probable" = 1, "Possible" = 2), inline = TRUE
            ),
            checkboxGroupInput(
              inputId = "Clinical_feature_M3",
              label = "Clinical Features",
              choices = c("Sudden Death" = 0, "STEMI" = 1, "NSTEMI" = 2, "Unstable Angina" = 3, "Stable Angina" = 4, "Other" = 5),
              inline = TRUE
            ),
            conditionalPanel(
              'input.Clinical_feature_M3.includes("5")',
              ns = ns,
              textAreaInput(
                ns("Clinical_feature_other_M3"),
                "Other",
                width = "400px",
                height = "100px"
              )
            ),
          ),

          # CVA
          conditionalPanel(
            'input.Event_Details_M3.includes("4")',
            ns = ns,
            dateInput(
              "CVA_date_M3",
              label = "Date",
              language = "kr"
            ),
            radioButtons(
              "CVA_Type_M3",
              label = "Type",
              choices = c("Ischemic" = 0, "Hemorrhagic" = 1, "Unknown" = 2), inline = TRUE
            ),
            radioButtons(
              "Imaging_M3",
              label = "Verified with imaging studies", choices = c("Yes" = 0, "No" = 1), inline = TRUE
            )
          ),
        ),
        textAreaInput(
          ns("Comment_M3"),
          "Comment",
          width = "400px",
          height = "100px", 
          value =  ifelse(is.null(hold$Comment_M3), "", hold$Comment_M3),
          
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

  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      data = list(
        "FU_M3" = ifelse(is.null(input$FU_M3), "", input$FU_M3),
        "Visit_Date_M3" = ifelse(is.null(input$Visit_Date_M3), "", as.character(input$Visit_Date_M3)),
        "Visit_M3" = ifelse(is.null(input$Visit_M3), "", input$Visit_M3),
        "Reason_M3" = ifelse(is.null(input$Reason_M3), "", input$Reason_M3),
        "Other_M3" = ifelse(is.null(input$Other_M3), "", input$Other_M3),
        "LastFU_M3" = ifelse(is.null(input$LastFU_M3), "", as.character(input$LastFU_M3)),
        "SBP_M3" = ifelse(is.null(input$SBP_M3), "", input$SBP_M3),
        "DBP_M3" = ifelse(is.null(input$DBP_M3), "", input$DBP_M3),
        "HRT_M3" = ifelse(is.null(input$HRT_M3), "", input$HRT_M3),
        "Event_M3" = ifelse(is.null(input$Event_M3), "", input$Event_M3),
        "Cardiac_Status_M3" = ifelse(is.null(input$Cardiac_Status_M3), "", input$Cardiac_Status_M3),
        "CCS_M3" = ifelse(is.null(input$CCS_M3), "", input$CCS_M3),
        "Other_Cardiac_Status_M3" = ifelse(is.null(input$Other_Cardiac_Status_M3), "", input$Other_Cardiac_Status_M3),
        "Readmission_M3" = ifelse(is.null(input$Readmission_M3), "", input$Readmission_M3),
        "Readmission_Date_M3" = ifelse(is.null(input$Readmission_Date_M3), "", input$Readmission_Date_M3),
        "Readmission_reason_M3" = ifelse(is.null(input$Readmission_reason_M3), "", input$Readmission_reason_M3),
        "Readmission_reason_text_M3" = ifelse(is.null(input$Readmission_reason_text_M3), "", input$Readmission_reason_text_M3),
        "ECG_Rhythm_M3" = ifelse(is.null(input$ECG_Rhythm_M3), "", input$ECG_Rhythm_M3),
        "ECG_Rhythm_Other_M3" = ifelse(is.null(input$ECG_Rhythm_Other_M3), "", input$ECG_Rhythm_Other_M3),
        "Stress_ECG_M3" = ifelse(is.null(input$Stress_ECG_M3), "", input$Stress_ECG_M3),
        "Stress_ECG_Date_M3" = ifelse(is.null(input$Stress_ECG_Date_M3), "", as.character(input$Stress_ECG_Date_M3)),
        "Stress_ECG_Detail_M3" = ifelse(is.null(input$Stress_ECG_Detail_M3), "", input$Stress_ECG_Detail_M3),
        "Lab_data_Unknown_M3" = ifelse(is.null(input$Lab_data_Unknown_M3), "", input$Lab_data_Unknown_M3),
        "Lab_Data_M3" = ifelse(is.null(input$Lab_Data_M3), "", as.character(input$Lab_Data_M3)),
        "WBC_M3" = ifelse(is.null(input$WBC_M3), "", input$WBC_M3),
        "TC_M3" = ifelse(is.null(input$TC_M3), "", input$TC_M3),
        "Hb_M3" = ifelse(is.null(input$Hb_M3), "", input$Hb_M3),
        "TG_M3" = ifelse(is.null(input$TG_M3), "", input$TG_M3),
        "Platelet_M3" = ifelse(is.null(input$Platelet_M3), "", input$Platelet_M3),
        "HDL_M3" = ifelse(is.null(input$HDL_M3), "", input$HDL_M3),
        "BUN_M3" = ifelse(is.null(input$BUN_M3), "", input$BUN_M3),
        "LDL_M3" = ifelse(is.null(input$LDL_M3), "", input$LDL_M3),
        "Cr_M3" = ifelse(is.null(input$Cr_M3), "", input$Cr_M3),
        "LDL_Cal_M3" = ifelse(is.null(input$LDL_Cal_M3), "", input$LDL_Cal_M3),
        "BNP_M3" = ifelse(is.null(input$BNP_M3), "", input$BNP_M3),
        "FG_M3" = ifelse(is.null(input$FG_M3), "", input$FG_M3),
        "Pro_BNP_M3" = ifelse(is.null(input$Pro_BNP_M3), "", input$Pro_BNP_M3),
        "HbA1C_M3" = ifelse(is.null(input$HbA1C_M3), "", input$HbA1C_M3),
        "N_Pro_BNP_M3" = ifelse(is.null(input$N_Pro_BNP_M3), "", input$N_Pro_BNP_M3),
        "hs_CRP_M3" = ifelse(is.null(input$hs_CRP_M3), "", input$hs_CRP_M3),
        "CK_MB_M3" = ifelse(is.null(input$CK_MB_M3), "", input$CK_MB_M3),
        "Troponin_I_M3" = ifelse(is.null(input$Troponin_I_M3), "", input$Troponin_I_M3),
        "Troponin_T_M3" = ifelse(is.null(input$Troponin_T_M3), "", input$Troponin_T_M3),
        "Aspirin_M3" = ifelse(is.null(input$Aspirin_M3), "", input$Aspirin_M3),
        "Clopidogrel_M3" = ifelse(is.null(input$Clopidogrel_M3), "", input$Clopidogrel_M3),
        "Prasugrel_M3" = ifelse(is.null(input$Prasugrel_M3), "", input$Prasugrel_M3),
        "Ticagrelor_M3" = ifelse(is.null(input$Ticagrelor_M3), "", input$Ticagrelor_M3),
        "BB_M3" = ifelse(is.null(input$BB_M3), "", input$BB_M3),
        "WN_M3" = ifelse(is.null(input$WN_M3), "", input$WN_M3),
        "Statin_M3" = ifelse(is.null(input$Statin_M3), "", input$Statin_M3),
        "ACE_M3" = ifelse(is.null(input$ACE_M3), "", input$ACE_M3),
        "Nitrate_M3" = ifelse(is.null(input$Nitrate_M3), "", input$Nitrate_M3),
        "Calcium_M3" = ifelse(is.null(input$Calcium_M3), "", input$Calcium_M3),
        "Trimetazidine_M3" = ifelse(is.null(input$Trimetazidine_M3), "", input$Trimetazidine_M3),
        "Event_Details_M3" = ifelse(is.null(input$Event_Details_M3), "", input$Event_Details_M3),
        "Death_date_M3" = ifelse(is.null(input$Death_date_M3), "", as.character(input$Death_date_M3)),
        "Death_reason_M3" = ifelse(is.null(input$Death_reason_M3), "", input$Death_reason_M3),
        "MI_date_M3" = ifelse(is.null(input$MI_date_M3), "", as.character(input$MI_date_M3)),
        "MI_Segment_M3" = ifelse(is.null(input$MI_Segment_M3), "", input$MI_Segment_M3),
        "MI_Type_M3" = ifelse(is.null(input$MI_Type_M3), "", input$MI_Type_M3),
        "MI_Stent_M3" = ifelse(is.null(input$MI_Stent_M3), "", input$MI_Stent_M3),
        "MI_Lesion_M3" = ifelse(is.null(input$MI_Lesion_M3), "", input$MI_Lesion_M3),
        "MI_Vessel_M3" = ifelse(is.null(input$MI_Vessel_M3), "", input$MI_Vessel_M3),
        "MI_Treatment_M3" = ifelse(is.null(input$MI_Treatment_M3), "", input$MI_Treatment_M3),
        "MI_After_M3" = ifelse(is.null(input$MI_After_M3), "", input$MI_After_M3),
        "Rev_date_M3" = ifelse(is.null(input$Rev_date_M3), "", as.character(input$Rev_date_M3)),
        "Rev_Segment_M3" = ifelse(is.null(input$Rev_Segment_M3), "", input$Rev_Segment_M3),
        "Rev_Treatment_M3" = ifelse(is.null(input$Rev_Treatment_M3), "", input$Rev_Treatment_M3),
        "Rev_Lesion_M3" = ifelse(is.null(input$Rev_Lesion_M3), "", input$Rev_Lesion_M3),
        "Rev_Vessel_M3" = ifelse(is.null(input$Rev_Vessel_M3), "", input$Rev_Vessel_M3),
        "Rev_PCI_M3" = ifelse(is.null(input$Rev_PCI_M3), "", input$Rev_PCI_M3),
        "Stent_date_M3" = ifelse(is.null(input$Stent_date_M3), "", as.character(input$Stent_date_M3)),
        "Stent_Segment_M3" = ifelse(is.null(input$Stent_Segment_M3), "", input$Stent_Segment_M3),
        "Stent_Type_M3" = ifelse(is.null(input$Stent_Type_M3), "", input$Stent_Type_M3),
        "Stent_arc_M3" = ifelse(is.null(input$Stent_arc_M3), "", input$Stent_arc_M3),
        "Clinical_feature_M3" = ifelse(is.null(input$Clinical_feature_M3), "", input$Clinical_feature_M3),
        "Clinical_feature_other_M3" = ifelse(is.null(input$Clinical_feature_other_M3), "", input$Clinical_feature_other_M3),
        "CVA_date_M3" = ifelse(is.null(input$CVA_date_M3), "", as.character(input$CVA_date_M3)),
        "CVA_Type_M3" = ifelse(is.null(input$CVA_Type_M3), "", input$CVA_Type_M3),
        "Imaging_M3" = ifelse(is.null(input$Imaging_M3), "", input$Imaging_M3),
        "Comment_M3" = ifelse(is.null(input$Comment_M3), "", input$Comment_M3)

        # "Withdrawal_M3" = ifelse(is.null(input$Withdrawal_M3), "", input$Withdrawal_M3),
        # "Withdrawal_Date_M3" = ifelse(is.null(input$Withdrawal_Date_M3), "", as.character(input$Withdrawal_Date_M3)),
        # "Cause_M3" = ifelse(is.null(input$Cause_M3), "", input$Cause_M3),
        # "Comment_M3" = ifelse(is.null(input$Comment_M3), "", input$Comment_M3)
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

  observeEvent(input$ALLND_1, {
    updateTextInput(session, "SBP_M3", label = "Systolic BP", value = "ND")
  })
}
