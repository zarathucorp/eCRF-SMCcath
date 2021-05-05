#' m6 Add & Edit Module
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
m6_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- car_to_edit()
    
    showModal(
      modalDialog(
        
        dateInput(
          ns('Readm_M6'),
          "Readmission Date",
          value = lubridate::as_date(hold$Readm_M6),
          language = "kr"
        ),
        
        
        radioButtons(
          ns("FU_M6"),
          "F/U Date",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$FU_M6, inline = T
        ),
        
        ## If YES
        
        conditionalPanel(
          "input.FU_M6 == 0",
          ns = ns,
          dateInput(
            ns("Visit_Date_M6"),
            "Visit date",
            value = hold$Visit_Date_M6,
            language = "kr"
          ),
          radioButtons(
            ns("Visit_M6"),
            "Visit by",
            choices = c("Clinic" = 0, "Phone" = 1),
            selected = hold$Visit_M6, inline = T
          ),
          
          ## Implement Duration
        ),
        conditionalPanel(
          "input.FU_M6 == 1",
          ns = ns,
          radioButtons(
            ns("Reason_M6"),
            "Reason",
            choices = c("Patient Died" = 0, "Patient Lost to F/U" = 1, "Other" = 2),
            selected = hold$Reason_M6, inline = T
          ),
          conditionalPanel(
            "input.Reason_M6 == 2", # if Other : Show text box.
            ns = ns,
            textAreaInput(
              ns("Other_M6"),
              "Other",
              width = "400px",
              height = "100px"
            )
          ),
          dateInput(
            ns("LastFU_M6"),
            "Date of Last F/U",
            value = hold$LastFU_M6,
            language = "kr"
          )
          ## Implement Duration
        ),
        
        ## ALL ND
        actionButton(ns("ALLND_1"), "ALL ND", class = "btn btn-default"),
        textInput(
          ns("SBP_M6"), # Systolic BP
          "Systolic BP",
          value = ifelse(is.null(hold), NA, hold$SBP_M6)
        ),
        numericInput(
          ns("DBP_M6"), # Diastolic BP
          "Diastolic BP",
          value = ifelse(is.null(hold), NA, hold$DBP_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HRT_M6"), # Heart Ratio
          "Heart Rate",
          value = ifelse(is.null(hold), NA, hold$HRT_M6),
          min = 0, max = 120,
          step = 1
        ),
        radioButtons(
          ns("Cardiac_Status_M6"),
          "Cardiac Status",
          choices = c("Unknown" = 0, "Asymptomatic" = 1, "Angina" = 2, "Other" = 3),
          selected = hold$Cardiac_Status_M6, inline = T
        ),
        conditionalPanel(
          "input.Cardiac_Status_M6 == 2", # if Angina, show CCS I, II, III, IV
          ns = ns,
          radioButtons(
            ns("CCS_M6"),
            "CCS",
            choices = c("I" = 0, "II" = 1, "III" = 2, "IV" = 3),
            selected = hold$CCS_M6, inline = T
          )
        ),
        conditionalPanel(
          "input.Cardiac_Status_M6 == 3", # if Other, show textInput
          ns = ns,
          textAreaInput(
            ns("Other_Cardiac_Status_M6"),
            "Other",
            width = "400px",
            height = "100px"
          )
        ),
        radioButtons(
          ns("Readmission_M6"),
          "Readmission",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Readmission_M6, inline = T
        ),
        conditionalPanel(
          "input.Readmission_M6 == 0",
          ns = ns,
          checkboxGroupInput(
            ns("Readmission_reason_M6"),
            label = "",
            choices = c("Cardiac Reason" = 0, "Non-Cardiac Reason" = 1),
            selected = NULL,
            inline = TRUE
          ),
          textAreaInput(
            ns("Readmission_reason_text_M6"),
            "Reason",
            width = "400px",
            height = "100px"
          )
        ),
        radioButtons(
          ns("ECG_Rhythm_M6"),
          "ECG Rhythm",
          choices = c("Sinus Rhytum" = 0, "Atrial Fibrillation" = 1, "Others" = 2, "Not Done" = 3),
          selected = hold$ECG_Rhythm_M6,
          inline = T
        ),
        conditionalPanel(
          "input.ECG_Rhythm_M6 == 2",
          ns = ns,
          textAreaInput(
            ns("ECG_Rhythm_Other_M6"),
            "Others",
            width = "400px",
            height = "100px"
          )
        ),
        
        # Stress ECG
        radioButtons(
          ns("Stress_ECG_M6"),
          "Stress ECG (TMT)",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Stress_ECG_M6,
          inline = T
        ),
        conditionalPanel(
          "input.Stress_ECG_M6 == 0",
          ns = ns,
          dateInput(
            ns("Stress_ECG_Date_M6"),
            "",
            value = hold$Stress_ECG_Date_M6,
            language = "kr"
          ),
          radioButtons(
            ns("Stress_ECG_Detail_M6"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$Stress_ECG_Detail_M6,
            inline = T
          )
        ),
        
        # Lab Data
        radioButtons(
          ns("Lab_data_Unknown_M6"),
          label = "Date Unknown",
          choices = c("Yes" = 0, "No" = 1)
        ),
        conditionalPanel(
          "input.Lab_data_Unknown_M6 == 1",
          dateInput(
            ns("Lab_Data_M6"),
            "Date of Lab",
            value = hold$Lab_Data_M6,
            language = "kr"
          ),
        ),
        ## All ND
        numericInput(
          ns("WBC_M6"),
          "WBC",
          value = ifelse(is.null(hold), NA, hold$WBC_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TC_M6"),
          "Total Chol",
          value = ifelse(is.null(hold), NA, hold$TC_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Hb_M6"),
          "Hb",
          value = ifelse(is.null(hold), NA, hold$Hb_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TG_M6"),
          "TG",
          value = ifelse(is.null(hold), NA, hold$TG_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Platelet_M6"),
          "Platelet",
          value = ifelse(is.null(hold), NA, hold$Platelet_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HDL_M6"),
          "HDL",
          value = ifelse(is.null(hold), NA, hold$HDL_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BUN_M6"),
          "BUN",
          value = ifelse(is.null(hold), NA, hold$BUN_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_M6"),
          "LDL",
          value = ifelse(is.null(hold), NA, hold$LDL_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Cr_M6"),
          "Cr",
          value = ifelse(is.null(hold), NA, hold$Cr_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_Cal_M6"),
          "LDL, cal",
          value = ifelse(is.null(hold), NA, hold$LDL_Cal_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BNP_M6"),
          "BNP",
          value = ifelse(is.null(hold), NA, hold$BNP_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("FG_M6"),
          "Fasting Glucose",
          value = ifelse(is.null(hold), NA, hold$FG_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Pro_BNP_M6"),
          "pro BNP",
          value = ifelse(is.null(hold), NA, hold$Pro_BNP_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HbA1C_M6"),
          "HbA1C",
          value = ifelse(is.null(hold), NA, hold$HbA1C_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("N_Pro_BNP_M6"),
          "N-pro BNP",
          value = ifelse(is.null(hold), NA, hold$N_Pro_BNP_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("hs_CRP_M6"),
          "hs-CRP",
          value = ifelse(is.null(hold), NA, hold$hs_CRP_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("CK_MB_M6"),
          "Platelet",
          value = ifelse(is.null(hold), NA, hold$Platelet_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Troponin_I_M6"),
          "Troponin I",
          value = ifelse(is.null(hold), NA, hold$Troponin_I_M6),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Troponin_T_M6"),
          "Troponin T",
          value = ifelse(is.null(hold), NA, hold$Troponin_T_M6),
          min = 0, max = 120,
          step = 1
        ),
        
        
        # Medication Data
        radioButtons(
          ns("Aspirin_M6"),
          label = "Aspirin",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Aspirin_M6
        ),
        radioButtons(
          ns("Clopidogrel_M6"),
          label = "Clopidogrel",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Clopidogrel_M6
        ),
        radioButtons(
          ns("Prasugrel_M6"),
          label = "Prasugrel",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Prasugrel_M6
        ),
        radioButtons(
          ns("Ticagrelor_M6"),
          label = "Ticagrelor",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Ticagrelor_M6
        ),
        radioButtons( # Beta Blocker
          ns("BB_M6"),
          label = "Beta Blocker",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$BB_M6
        ),
        radioButtons(
          ns("WN_M6"),
          label = "Wafarin or NOAC",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$WN_M6
        ),
        radioButtons(
          ns("Statin_M6"),
          label = "Statin",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Statin_M6
        ),
        radioButtons(
          ns("ACE_M6"),
          label = "ACE Inhibitor or ARB",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$ACE_M6
        ),
        radioButtons(
          ns("Nitrate_M6"),
          label = "Nitrate (Sigmart)",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Nitrate_M6
        ),
        radioButtons(
          ns("Calcium_M6"),
          label = "Calcium channel antagonist",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Calcium_M6
        ),
        radioButtons(
          ns("Trimetazidine_M6"),
          label = "Trimetazidine",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Trimetazidine_M6
        ),
        
        # Clinical Events
        radioButtons(
          ns("Event_M6"),
          "Clinical Events",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Event_M6
        ),
        conditionalPanel(
          "input.Event_M6 == 0",
          ns = ns,
          checkboxGroupInput(
            ns("Event_Details_M6"),
            "",
            choices = c("Death" = 0, "MI" = 1, "Repeat Revascularization" = 2, "Stent Thrombosis" = 3, "CVA" = 4),
            selected = hold$Event_Details_M6
          ),
          # Death
          conditionalPanel(
            'input.Event_Details_M6.includes("0")',
            ns = ns,
            dateInput(
              "Death_date_M6",
              label = "Date",
              language = "kr"
            ),
            radioButtons(
              "Death_reason_M6",
              label = "",
              choices = c("Cardiac Death" = 1, "Non-Cardiovascular Death" = 2, "Unknown Origin Death" = 3)
            )
          ),
          
          # MI
          
          conditionalPanel(
            'input.Event_Details_M6.includes("1")',
            ns = ns,
            dateInput(
              "MI_date_M6",
              label = "Date",
              language = "kr"
            ),
            numericInput(
              "MI_Segment_M6",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$MI_Segment_M6),
              min = 0, max = 120,
              step = 1
            ),
            radioButtons(
              "MI_Type_M6",
              label = "Type", choices = c("STEMI" = 0, "NSTEMI" = 1), inline = TRUE
            ),
            radioButtons(
              "MI_Stent_M6",
              label = "Related with Stent Thrombosis", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ),
            radioButtons(
              "MI_Lesion_M6",
              label = "Related with Target Lesion", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ),
            radioButtons(
              "MI_Vessel_M6",
              label = "Related with Target Vessel", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ), # Hover popup
            
            checkboxGroupInput(
              "MI_Treatment_M6",
              label = "Type of Treatment",
              choices = c("Medication Only" = 0, "Thrombolysis" = 1, "only Ballooning" = 2, "Stenting" = 3, "Bypass Surgery" = 4)
            ),
            radioButtons(
              "MI_After_M6",
              label = "After Treatment",
              choices = c("Recovered" = 0, "Death" = 1, "Unknown" = 2)
            )
          ),
          
          # Revascularization
          conditionalPanel(
            'input.Event_Details_M6.includes("2")',
            ns = ns,
            dateInput(
              "Rev_date_M6",
              label = "Date",
              language = "kr"
            ),
            numericInput(
              "Rev_Segment_M6",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$Rev_Segment_M6),
              min = 0, max = 120,
              step = 1
            ),
            checkboxGroupInput(
              "Rev_Treatment_M6",
              label = "Type of Treatment",
              choices = c("only Ballooning" = 0, "Stenting" = 1, "Bypass Surgery" = 2)
            ),
            radioButtons(
              "Rev_Lesion_M6",
              label = "Related with Target Lesion",
              choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
              inline = TRUE
            ),
            radioButtons(
              "Rev_Vessel_M6",
              label = "Related with Target Vessel",
              choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
              inline = TRUE
            ), # Hover popup
            
            radioButtons(
              "Rev_PCI_M6",
              label = "Another Vessel PCI",
              choices = c("Yes" = 0, "No" = 1)
            )
          ),
          
          # Stent Thrombosis
          conditionalPanel(
            'input.Event_Details_M6.includes("3")',
            ns = ns,
            dateInput(
              "Stent_date_M6",
              label = "Date",
              language = "kr"
            ),
            textInput(
              "Stent_Segment_M6",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$Stent_Segment_M6)
            ),
            radioButtons(
              "Stent_Type_M6",
              label = "Type",
              choices = c("Acute (< 1d)" = 0, "Subacute (1-30d)" = 1, "Late(> 1m)" = 2, "Very Late(> 1y)" = 3), inline = TRUE
            ),
            radioButtons(
              "Stent_arc_M6",
              label = "ARC", choices = c("Definite/Confirmed" = 0, "Probable" = 1, "Possible" = 2), inline = TRUE
            ),
            checkboxGroupInput(
              inputId = "Clinical_feature_M6",
              label = "Clinical Features",
              choices = c("Sudden Death" = 0, "STEMI" = 1, "NSTEMI" = 2, "Unstable Angina" = 3, "Stable Angina" = 4, "Other" = 5),
              inline = TRUE
            ),
            conditionalPanel(
              'input.Clinical_feature_M6.includes("5")',
              ns = ns,
              textAreaInput(
                ns("Clinical_feature_other_M6"),
                "Other",
                width = "400px",
                height = "100px"
              )
            ),
          ),
          
          # CVA
          conditionalPanel(
            'input.Event_Details_M6.includes("4")',
            ns = ns,
            dateInput(
              "CVA_date_M6",
              label = "Date",
              language = "kr"
            ),
            radioButtons(
              "CVA_Type_M6",
              label = "Type",
              choices = c("Ischemic" = 0, "Hemorrhagic" = 1, "Unknown" = 2), inline = TRUE
            ),
            radioButtons(
              "Imaging_M6",
              label = "Verified with imaging studies", choices = c("Yes" = 0, "No" = 1), inline = TRUE
            )
          ),
        ),
        textAreaInput(
          ns("Comment_M6"),
          "Comment",
          width = "400px",
          height = "100px", 
          value =  ifelse(is.null(hold$Comment_M6), "", hold$Comment_M6),
          
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
        "Readm_M6" = ifelse(is.null(input$Readm_M6), "", input$Readm_M6),
        "FU_M6" = ifelse(is.null(input$FU_M6), "", input$FU_M6),
        "Visit_Date_M6" = ifelse(is.null(input$Visit_Date_M6), "", as.character(input$Visit_Date_M6)),
        "Visit_M6" = ifelse(is.null(input$Visit_M6), "", input$Visit_M6),
        "Reason_M6" = ifelse(is.null(input$Reason_M6), "", input$Reason_M6),
        "Other_M6" = ifelse(is.null(input$Other_M6), "", input$Other_M6),
        "LastFU_M6" = ifelse(is.null(input$LastFU_M6), "", as.character(input$LastFU_M6)),
        "SBP_M6" = ifelse(is.null(input$SBP_M6), "", input$SBP_M6),
        "DBP_M6" = ifelse(is.null(input$DBP_M6), "", input$DBP_M6),
        "HRT_M6" = ifelse(is.null(input$HRT_M6), "", input$HRT_M6),
        "Event_M6" = ifelse(is.null(input$Event_M6), "", input$Event_M6),
        "Cardiac_Status_M6" = ifelse(is.null(input$Cardiac_Status_M6), "", input$Cardiac_Status_M6),
        "CCS_M6" = ifelse(is.null(input$CCS_M6), "", input$CCS_M6),
        "Other_Cardiac_Status_M6" = ifelse(is.null(input$Other_Cardiac_Status_M6), "", input$Other_Cardiac_Status_M6),
        "Readmission_M6" = ifelse(is.null(input$Readmission_M6), "", input$Readmission_M6),
        "Readmission_reason_M6" = ifelse(is.null(input$Readmission_reason_M6), "", input$Readmission_reason_M6),
        "Readmission_reason_text_M6" = ifelse(is.null(input$Readmission_reason_text_M6), "", input$Readmission_reason_text_M6),
        "ECG_Rhythm_M6" = ifelse(is.null(input$ECG_Rhythm_M6), "", input$ECG_Rhythm_M6),
        "ECG_Rhythm_Other_M6" = ifelse(is.null(input$ECG_Rhythm_Other_M6), "", input$ECG_Rhythm_Other_M6),
        "Stress_ECG_M6" = ifelse(is.null(input$Stress_ECG_M6), "", input$Stress_ECG_M6),
        "Stress_ECG_Date_M6" = ifelse(is.null(input$Stress_ECG_Date_M6), "", as.character(input$Stress_ECG_Date_M6)),
        "Stress_ECG_Detail_M6" = ifelse(is.null(input$Stress_ECG_Detail_M6), "", input$Stress_ECG_Detail_M6),
        "Lab_data_Unknown_M6" = ifelse(is.null(input$Lab_data_Unknown_M6), "", input$Lab_data_Unknown_M6),
        "Lab_Data_M6" = ifelse(is.null(input$Lab_Data_M6), "", as.character(input$Lab_Data_M6)),
        "WBC_M6" = ifelse(is.null(input$WBC_M6), "", input$WBC_M6),
        "TC_M6" = ifelse(is.null(input$TC_M6), "", input$TC_M6),
        "Hb_M6" = ifelse(is.null(input$Hb_M6), "", input$Hb_M6),
        "TG_M6" = ifelse(is.null(input$TG_M6), "", input$TG_M6),
        "Platelet_M6" = ifelse(is.null(input$Platelet_M6), "", input$Platelet_M6),
        "HDL_M6" = ifelse(is.null(input$HDL_M6), "", input$HDL_M6),
        "BUN_M6" = ifelse(is.null(input$BUN_M6), "", input$BUN_M6),
        "LDL_M6" = ifelse(is.null(input$LDL_M6), "", input$LDL_M6),
        "Cr_M6" = ifelse(is.null(input$Cr_M6), "", input$Cr_M6),
        "LDL_Cal_M6" = ifelse(is.null(input$LDL_Cal_M6), "", input$LDL_Cal_M6),
        "BNP_M6" = ifelse(is.null(input$BNP_M6), "", input$BNP_M6),
        "FG_M6" = ifelse(is.null(input$FG_M6), "", input$FG_M6),
        "Pro_BNP_M6" = ifelse(is.null(input$Pro_BNP_M6), "", input$Pro_BNP_M6),
        "HbA1C_M6" = ifelse(is.null(input$HbA1C_M6), "", input$HbA1C_M6),
        "N_Pro_BNP_M6" = ifelse(is.null(input$N_Pro_BNP_M6), "", input$N_Pro_BNP_M6),
        "hs_CRP_M6" = ifelse(is.null(input$hs_CRP_M6), "", input$hs_CRP_M6),
        "CK_MB_M6" = ifelse(is.null(input$CK_MB_M6), "", input$CK_MB_M6),
        "Troponin_I_M6" = ifelse(is.null(input$Troponin_I_M6), "", input$Troponin_I_M6),
        "Troponin_T_M6" = ifelse(is.null(input$Troponin_T_M6), "", input$Troponin_T_M6),
        "Aspirin_M6" = ifelse(is.null(input$Aspirin_M6), "", input$Aspirin_M6),
        "Clopidogrel_M6" = ifelse(is.null(input$Clopidogrel_M6), "", input$Clopidogrel_M6),
        "Prasugrel_M6" = ifelse(is.null(input$Prasugrel_M6), "", input$Prasugrel_M6),
        "Ticagrelor_M6" = ifelse(is.null(input$Ticagrelor_M6), "", input$Ticagrelor_M6),
        "BB_M6" = ifelse(is.null(input$BB_M6), "", input$BB_M6),
        "WN_M6" = ifelse(is.null(input$WN_M6), "", input$WN_M6),
        "Statin_M6" = ifelse(is.null(input$Statin_M6), "", input$Statin_M6),
        "ACE_M6" = ifelse(is.null(input$ACE_M6), "", input$ACE_M6),
        "Nitrate_M6" = ifelse(is.null(input$Nitrate_M6), "", input$Nitrate_M6),
        "Calcium_M6" = ifelse(is.null(input$Calcium_M6), "", input$Calcium_M6),
        "Trimetazidine_M6" = ifelse(is.null(input$Trimetazidine_M6), "", input$Trimetazidine_M6),
        "Event_Details_M6" = ifelse(is.null(input$Event_Details_M6), "", input$Event_Details_M6),
        "Death_date_M6" = ifelse(is.null(input$Death_date_M6), "", as.character(input$Death_date_M6)),
        "Death_reason_M6" = ifelse(is.null(input$Death_reason_M6), "", input$Death_reason_M6),
        "MI_date_M6" = ifelse(is.null(input$MI_date_M6), "", as.character(input$MI_date_M6)),
        "MI_Segment_M6" = ifelse(is.null(input$MI_Segment_M6), "", input$MI_Segment_M6),
        "MI_Type_M6" = ifelse(is.null(input$MI_Type_M6), "", input$MI_Type_M6),
        "MI_Stent_M6" = ifelse(is.null(input$MI_Stent_M6), "", input$MI_Stent_M6),
        "MI_Lesion_M6" = ifelse(is.null(input$MI_Lesion_M6), "", input$MI_Lesion_M6),
        "MI_Vessel_M6" = ifelse(is.null(input$MI_Vessel_M6), "", input$MI_Vessel_M6),
        "MI_Treatment_M6" = ifelse(is.null(input$MI_Treatment_M6), "", input$MI_Treatment_M6),
        "MI_After_M6" = ifelse(is.null(input$MI_After_M6), "", input$MI_After_M6),
        "Rev_date_M6" = ifelse(is.null(input$Rev_date_M6), "", as.character(input$Rev_date_M6)),
        "Rev_Segment_M6" = ifelse(is.null(input$Rev_Segment_M6), "", input$Rev_Segment_M6),
        "Rev_Treatment_M6" = ifelse(is.null(input$Rev_Treatment_M6), "", input$Rev_Treatment_M6),
        "Rev_Lesion_M6" = ifelse(is.null(input$Rev_Lesion_M6), "", input$Rev_Lesion_M6),
        "Rev_Vessel_M6" = ifelse(is.null(input$Rev_Vessel_M6), "", input$Rev_Vessel_M6),
        "Rev_PCI_M6" = ifelse(is.null(input$Rev_PCI_M6), "", input$Rev_PCI_M6),
        "Stent_date_M6" = ifelse(is.null(input$Stent_date_M6), "", as.character(input$Stent_date_M6)),
        "Stent_Segment_M6" = ifelse(is.null(input$Stent_Segment_M6), "", input$Stent_Segment_M6),
        "Stent_Type_M6" = ifelse(is.null(input$Stent_Type_M6), "", input$Stent_Type_M6),
        "Stent_arc_M6" = ifelse(is.null(input$Stent_arc_M6), "", input$Stent_arc_M6),
        "Clinical_feature_M6" = ifelse(is.null(input$Clinical_feature_M6), "", input$Clinical_feature_M6),
        "Clinical_feature_other_M6" = ifelse(is.null(input$Clinical_feature_other_M6), "", input$Clinical_feature_other_M6),
        "CVA_date_M6" = ifelse(is.null(input$CVA_date_M6), "", as.character(input$CVA_date_M6)),
        "CVA_Type_M6" = ifelse(is.null(input$CVA_Type_M6), "", input$CVA_Type_M6),
        "Imaging_M6" = ifelse(is.null(input$Imaging_M6), "", input$Imaging_M6),
        "Comment_M6" = ifelse(is.null(input$Comment_M6), "", input$Comment_M6)
        
        # "Withdrawal_M6" = ifelse(is.null(input$Withdrawal_M6), "", input$Withdrawal_M6),
        # "Withdrawal_Date_M6" = ifelse(is.null(input$Withdrawal_Date_M6), "", as.character(input$Withdrawal_Date_M6)),
        # "Cause_M6" = ifelse(is.null(input$Cause_M6), "", input$Cause_M6),
        # "Comment_M6" = ifelse(is.null(input$Comment_M6), "", input$Comment_M6)
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
    updateTextInput(session, "SBP_M6", label = "Systolic BP", value = "ND")
  })
}
