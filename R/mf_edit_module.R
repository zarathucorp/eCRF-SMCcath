#' mf Add & Edit Module
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
mf_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- car_to_edit()
    
    showModal(
      modalDialog(
      
        radioButtons(
          ns("FU_Mf"),
          "F/U Date",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$FU_Mf, inline = T
        ),
        
        ## If YES
        
        conditionalPanel(
          "input.FU_Mf == 0",
          ns = ns,
          dateInput(
            ns("Visit_Date_Mf"),
            "Visit date",
            value = hold$Visit_Date_Mf,
            language = "kr"
          ),
          radioButtons(
            ns("Visit_Mf"),
            "Visit by",
            choices = c("Clinic" = 0, "Phone" = 1),
            selected = hold$Visit_Mf, inline = T
          ),
          
          ## Implement Duration
        ),
        conditionalPanel(
          "input.FU_Mf == 1",
          ns = ns,
          radioButtons(
            ns("Reason_Mf"),
            "Reason",
            choices = c("Patient Died" = 0, "Patient Lost to F/U" = 1, "Other" = 2),
            selected = hold$Reason_Mf, inline = T
          ),
          conditionalPanel(
            "input.Reason_Mf == 2", # if Other : Show text box.
            ns = ns,
            textAreaInput(
              ns("Other_Mf"),
              "Other",
              width = "400px",
              height = "100px"
            )
          ),
          dateInput(
            ns("LastFU_Mf"),
            "Date of Last F/U",
            value = hold$LastFU_Mf,
            language = "kr"
          )
          ## Implement Duration
        ),
        
        ## ALL ND
        actionButton(ns("ALLND_1"), "ALL ND", class = "btn btn-default"),
        textInput(
          ns("SBP_Mf"), # Systolic BP
          "Systolic BP",
          value = ifelse(is.null(hold), NA, hold$SBP_Mf)
        ),
        numericInput(
          ns("DBP_Mf"), # Diastolic BP
          "Diastolic BP",
          value = ifelse(is.null(hold), NA, hold$DBP_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HRT_Mf"), # Heart Ratio
          "Heart Rate",
          value = ifelse(is.null(hold), NA, hold$HRT_Mf),
          min = 0, max = 120,
          step = 1
        ),
        radioButtons(
          ns("Cardiac_Status_Mf"),
          "Cardiac Status",
          choices = c("Unknown" = 0, "Asymptomatic" = 1, "Angina" = 2, "Other" = 3),
          selected = hold$Cardiac_Status_Mf, inline = T
        ),
        conditionalPanel(
          "input.Cardiac_Status_Mf == 2", # if Angina, show CCS I, II, III, IV
          ns = ns,
          radioButtons(
            ns("CCS_Mf"),
            "CCS",
            choices = c("I" = 0, "II" = 1, "III" = 2, "IV" = 3),
            selected = hold$CCS_Mf, inline = T
          )
        ),
        conditionalPanel(
          "input.Cardiac_Status_Mf == 3", # if Other, show textInput
          ns = ns,
          textAreaInput(
            ns("Other_Cardiac_Status_Mf"),
            "Other",
            width = "400px",
            height = "100px"
          )
        ),
        radioButtons(
          ns("Readmission_Mf"),
          "Readmission",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Readmission_Mf, inline = T
        ),
        conditionalPanel(
          "input.Readmission_Mf == 0",
          ns = ns,
          dateInput(
            ns('Readmission_Date_Mf'),
            "Readmission Date",
            value = lubridate::as_date(hold$Readmission_Date_Mf),
            language = "kr"
          ),
          
          checkboxGroupInput(
            ns("Readmission_reason_Mf"),
            label = "",
            choices = c("Cardiac Reason" = 0, "Non-Cardiac Reason" = 1),
            selected = NULL,
            inline = TRUE
          ),
          textAreaInput(
            ns("Readmission_reason_text_Mf"),
            "Reason",
            width = "400px",
            height = "100px"
          )
        ),
        radioButtons(
          ns("ECG_Rhythm_Mf"),
          "ECG Rhythm",
          choices = c("Sinus Rhytum" = 0, "Atrial Fibrillation" = 1, "Others" = 2, "Not Done" = 3),
          selected = hold$ECG_Rhythm_Mf,
          inline = T
        ),
        conditionalPanel(
          "input.ECG_Rhythm_Mf == 2",
          ns = ns,
          textAreaInput(
            ns("ECG_Rhythm_Other_Mf"),
            "Others",
            width = "400px",
            height = "100px"
          )
        ),
        
        # Stress ECG
        radioButtons(
          ns("Stress_ECG_Mf"),
          "Stress ECG (TMT)",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Stress_ECG_Mf,
          inline = T
        ),
        conditionalPanel(
          "input.Stress_ECG_Mf == 0",
          ns = ns,
          dateInput(
            ns("Stress_ECG_Date_Mf"),
            "",
            value = hold$Stress_ECG_Date_Mf,
            language = "kr"
          ),
          radioButtons(
            ns("Stress_ECG_Detail_Mf"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$Stress_ECG_Detail_Mf,
            inline = T
          )
        ),
        
        # Lab Data
        radioButtons(
          ns("Lab_data_Unknown_Mf"),
          label = "Date Unknown",
          choices = c("Yes" = 0, "No" = 1)
        ),
        conditionalPanel(
          "input.Lab_data_Unknown_Mf == 1",
          dateInput(
            ns("Lab_Data_Mf"),
            "Date of Lab",
            value = hold$Lab_Data_Mf,
            language = "kr"
          ),
        ),
        ## All ND
        numericInput(
          ns("WBC_Mf"),
          "WBC",
          value = ifelse(is.null(hold), NA, hold$WBC_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TC_Mf"),
          "Total Chol",
          value = ifelse(is.null(hold), NA, hold$TC_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Hb_Mf"),
          "Hb",
          value = ifelse(is.null(hold), NA, hold$Hb_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TG_Mf"),
          "TG",
          value = ifelse(is.null(hold), NA, hold$TG_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Platelet_Mf"),
          "Platelet",
          value = ifelse(is.null(hold), NA, hold$Platelet_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HDL_Mf"),
          "HDL",
          value = ifelse(is.null(hold), NA, hold$HDL_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BUN_Mf"),
          "BUN",
          value = ifelse(is.null(hold), NA, hold$BUN_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_Mf"),
          "LDL",
          value = ifelse(is.null(hold), NA, hold$LDL_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Cr_Mf"),
          "Cr",
          value = ifelse(is.null(hold), NA, hold$Cr_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_Cal_Mf"),
          "LDL, cal",
          value = ifelse(is.null(hold), NA, hold$LDL_Cal_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BNP_Mf"),
          "BNP",
          value = ifelse(is.null(hold), NA, hold$BNP_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("FG_Mf"),
          "Fasting Glucose",
          value = ifelse(is.null(hold), NA, hold$FG_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Pro_BNP_Mf"),
          "pro BNP",
          value = ifelse(is.null(hold), NA, hold$Pro_BNP_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HbA1C_Mf"),
          "HbA1C",
          value = ifelse(is.null(hold), NA, hold$HbA1C_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("N_Pro_BNP_Mf"),
          "N-pro BNP",
          value = ifelse(is.null(hold), NA, hold$N_Pro_BNP_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("hs_CRP_Mf"),
          "hs-CRP",
          value = ifelse(is.null(hold), NA, hold$hs_CRP_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("CK_MB_Mf"),
          "Platelet",
          value = ifelse(is.null(hold), NA, hold$Platelet_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Troponin_I_Mf"),
          "Troponin I",
          value = ifelse(is.null(hold), NA, hold$Troponin_I_Mf),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Troponin_T_Mf"),
          "Troponin T",
          value = ifelse(is.null(hold), NA, hold$Troponin_T_Mf),
          min = 0, max = 120,
          step = 1
        ),
        
        
        # Medication Data
        radioButtons(
          ns("Aspirin_Mf"),
          label = "Aspirin",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Aspirin_Mf
        ),
        radioButtons(
          ns("Clopidogrel_Mf"),
          label = "Clopidogrel",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Clopidogrel_Mf
        ),
        radioButtons(
          ns("Prasugrel_Mf"),
          label = "Prasugrel",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Prasugrel_Mf
        ),
        radioButtons(
          ns("Ticagrelor_Mf"),
          label = "Ticagrelor",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Ticagrelor_Mf
        ),
        radioButtons( # Beta Blocker
          ns("BB_Mf"),
          label = "Beta Blocker",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$BB_Mf
        ),
        radioButtons(
          ns("WN_Mf"),
          label = "Wafarin or NOAC",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$WN_Mf
        ),
        radioButtons(
          ns("Statin_Mf"),
          label = "Statin",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Statin_Mf
        ),
        radioButtons(
          ns("ACE_Mf"),
          label = "ACE Inhibitor or ARB",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$ACE_Mf
        ),
        radioButtons(
          ns("Nitrate_Mf"),
          label = "Nitrate (Sigmart)",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Nitrate_Mf
        ),
        radioButtons(
          ns("Calcium_Mf"),
          label = "Calcium channel antagonist",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Calcium_Mf
        ),
        radioButtons(
          ns("Trimetazidine_Mf"),
          label = "Trimetazidine",
          choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
          selected = hold$Trimetazidine_Mf
        ),
        
        # Clinical Events
        radioButtons(
          ns("Event_Mf"),
          "Clinical Events",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Event_Mf
        ),
        conditionalPanel(
          "input.Event_Mf == 0",
          ns = ns,
          checkboxGroupInput(
            ns("Event_Details_Mf"),
            "",
            choices = c("Death" = 0, "MI" = 1, "Repeat Revascularization" = 2, "Stent Thrombosis" = 3, "CVA" = 4),
            selected = hold$Event_Details_Mf
          ),
          # Death
          conditionalPanel(
            'input.Event_Details_Mf.includes("0")',
            ns = ns,
            dateInput(
              "Death_date_Mf",
              label = "Date",
              language = "kr"
            ),
            radioButtons(
              "Death_reason_Mf",
              label = "",
              choices = c("Cardiac Death" = 1, "Non-Cardiovascular Death" = 2, "Unknown Origin Death" = 3)
            )
          ),
          
          # MI
          
          conditionalPanel(
            'input.Event_Details_Mf.includes("1")',
            ns = ns,
            dateInput(
              "MI_date_Mf",
              label = "Date",
              language = "kr"
            ),
            numericInput(
              "MI_Segment_Mf",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$MI_Segment_Mf),
              min = 0, max = 120,
              step = 1
            ),
            radioButtons(
              "MI_Type_Mf",
              label = "Type", choices = c("STEMI" = 0, "NSTEMI" = 1), inline = TRUE
            ),
            radioButtons(
              "MI_Stent_Mf",
              label = "Related with Stent Thrombosis", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ),
            radioButtons(
              "MI_Lesion_Mf",
              label = "Related with Target Lesion", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ),
            radioButtons(
              "MI_Vessel_Mf",
              label = "Related with Target Vessel", choices = c("Unknown" = 0, "No" = 1, "Yes" = 2), inline = TRUE
            ), # Hover popup
            
            checkboxGroupInput(
              "MI_Treatment_Mf",
              label = "Type of Treatment",
              choices = c("Medication Only" = 0, "Thrombolysis" = 1, "only Ballooning" = 2, "Stenting" = 3, "Bypass Surgery" = 4)
            ),
            radioButtons(
              "MI_After_Mf",
              label = "After Treatment",
              choices = c("Recovered" = 0, "Death" = 1, "Unknown" = 2)
            )
          ),
          
          # Revascularization
          conditionalPanel(
            'input.Event_Details_Mf.includes("2")',
            ns = ns,
            dateInput(
              "Rev_date_Mf",
              label = "Date",
              language = "kr"
            ),
            numericInput(
              "Rev_Segment_Mf",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$Rev_Segment_Mf),
              min = 0, max = 120,
              step = 1
            ),
            checkboxGroupInput(
              "Rev_Treatment_Mf",
              label = "Type of Treatment",
              choices = c("only Ballooning" = 0, "Stenting" = 1, "Bypass Surgery" = 2)
            ),
            radioButtons(
              "Rev_Lesion_Mf",
              label = "Related with Target Lesion",
              choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
              inline = TRUE
            ),
            radioButtons(
              "Rev_Vessel_Mf",
              label = "Related with Target Vessel",
              choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
              inline = TRUE
            ), # Hover popup
            
            radioButtons(
              "Rev_PCI_Mf",
              label = "Another Vessel PCI",
              choices = c("Yes" = 0, "No" = 1)
            )
          ),
          
          # Stent Thrombosis
          conditionalPanel(
            'input.Event_Details_Mf.includes("3")',
            ns = ns,
            dateInput(
              "Stent_date_Mf",
              label = "Date",
              language = "kr"
            ),
            textInput(
              "Stent_Segment_Mf",
              label = "Segment",
              value = ifelse(is.null(hold), NA, hold$Stent_Segment_Mf)
            ),
            radioButtons(
              "Stent_Type_Mf",
              label = "Type",
              choices = c("Acute (< 1d)" = 0, "Subacute (1-30d)" = 1, "Late(> 1m)" = 2, "Very Late(> 1y)" = 3), inline = TRUE
            ),
            radioButtons(
              "Stent_arc_Mf",
              label = "ARC", choices = c("Definite/Confirmed" = 0, "Probable" = 1, "Possible" = 2), inline = TRUE
            ),
            checkboxGroupInput(
              inputId = "Clinical_feature_Mf",
              label = "Clinical Features",
              choices = c("Sudden Death" = 0, "STEMI" = 1, "NSTEMI" = 2, "Unstable Angina" = 3, "Stable Angina" = 4, "Other" = 5),
              inline = TRUE
            ),
            conditionalPanel(
              'input.Clinical_feature_Mf.includes("5")',
              ns = ns,
              textAreaInput(
                ns("Clinical_feature_other_Mf"),
                "Other",
                width = "400px",
                height = "100px"
              )
            ),
          ),
          
          # CVA
          conditionalPanel(
            'input.Event_Details_Mf.includes("4")',
            ns = ns,
            dateInput(
              "CVA_date_Mf",
              label = "Date",
              language = "kr"
            ),
            radioButtons(
              "CVA_Type_Mf",
              label = "Type",
              choices = c("Ischemic" = 0, "Hemorrhagic" = 1, "Unknown" = 2), inline = TRUE
            ),
            radioButtons(
              "Imaging_Mf",
              label = "Verified with imaging studies", choices = c("Yes" = 0, "No" = 1), inline = TRUE
            )
          ),
        ),
        textAreaInput(
          ns("Comment_Mf"),
          "Comment",
          width = "400px",
          height = "100px", 
          value =  ifelse(is.null(hold$Comment_Mf), "", hold$Comment_Mf),
          
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
        "FU_Mf" = ifelse(is.null(input$FU_Mf), "", input$FU_Mf),
        "Visit_Date_Mf" = ifelse(is.null(input$Visit_Date_Mf), "", as.character(input$Visit_Date_Mf)),
        "Visit_Mf" = ifelse(is.null(input$Visit_Mf), "", input$Visit_Mf),
        "Reason_Mf" = ifelse(is.null(input$Reason_Mf), "", input$Reason_Mf),
        "Other_Mf" = ifelse(is.null(input$Other_Mf), "", input$Other_Mf),
        "LastFU_Mf" = ifelse(is.null(input$LastFU_Mf), "", as.character(input$LastFU_Mf)),
        "SBP_Mf" = ifelse(is.null(input$SBP_Mf), "", input$SBP_Mf),
        "DBP_Mf" = ifelse(is.null(input$DBP_Mf), "", input$DBP_Mf),
        "HRT_Mf" = ifelse(is.null(input$HRT_Mf), "", input$HRT_Mf),
        "Event_Mf" = ifelse(is.null(input$Event_Mf), "", input$Event_Mf),
        "Cardiac_Status_Mf" = ifelse(is.null(input$Cardiac_Status_Mf), "", input$Cardiac_Status_Mf),
        "CCS_Mf" = ifelse(is.null(input$CCS_Mf), "", input$CCS_Mf),
        "Other_Cardiac_Status_Mf" = ifelse(is.null(input$Other_Cardiac_Status_Mf), "", input$Other_Cardiac_Status_Mf),
        "Readmission_Mf" = ifelse(is.null(input$Readmission_Mf), "", input$Readmission_Mf),
        "Readmission_Date_Mf" = ifelse(is.null(input$Readmission_Date_Mf), "", input$Readmission_Date_Mf),
        "Readmission_reason_Mf" = ifelse(is.null(input$Readmission_reason_Mf), "", input$Readmission_reason_Mf),
        "Readmission_reason_text_Mf" = ifelse(is.null(input$Readmission_reason_text_Mf), "", input$Readmission_reason_text_Mf),
        "ECG_Rhythm_Mf" = ifelse(is.null(input$ECG_Rhythm_Mf), "", input$ECG_Rhythm_Mf),
        "ECG_Rhythm_Other_Mf" = ifelse(is.null(input$ECG_Rhythm_Other_Mf), "", input$ECG_Rhythm_Other_Mf),
        "Stress_ECG_Mf" = ifelse(is.null(input$Stress_ECG_Mf), "", input$Stress_ECG_Mf),
        "Stress_ECG_Date_Mf" = ifelse(is.null(input$Stress_ECG_Date_Mf), "", as.character(input$Stress_ECG_Date_Mf)),
        "Stress_ECG_Detail_Mf" = ifelse(is.null(input$Stress_ECG_Detail_Mf), "", input$Stress_ECG_Detail_Mf),
        "Lab_data_Unknown_Mf" = ifelse(is.null(input$Lab_data_Unknown_Mf), "", input$Lab_data_Unknown_Mf),
        "Lab_Data_Mf" = ifelse(is.null(input$Lab_Data_Mf), "", as.character(input$Lab_Data_Mf)),
        "WBC_Mf" = ifelse(is.null(input$WBC_Mf), "", input$WBC_Mf),
        "TC_Mf" = ifelse(is.null(input$TC_Mf), "", input$TC_Mf),
        "Hb_Mf" = ifelse(is.null(input$Hb_Mf), "", input$Hb_Mf),
        "TG_Mf" = ifelse(is.null(input$TG_Mf), "", input$TG_Mf),
        "Platelet_Mf" = ifelse(is.null(input$Platelet_Mf), "", input$Platelet_Mf),
        "HDL_Mf" = ifelse(is.null(input$HDL_Mf), "", input$HDL_Mf),
        "BUN_Mf" = ifelse(is.null(input$BUN_Mf), "", input$BUN_Mf),
        "LDL_Mf" = ifelse(is.null(input$LDL_Mf), "", input$LDL_Mf),
        "Cr_Mf" = ifelse(is.null(input$Cr_Mf), "", input$Cr_Mf),
        "LDL_Cal_Mf" = ifelse(is.null(input$LDL_Cal_Mf), "", input$LDL_Cal_Mf),
        "BNP_Mf" = ifelse(is.null(input$BNP_Mf), "", input$BNP_Mf),
        "FG_Mf" = ifelse(is.null(input$FG_Mf), "", input$FG_Mf),
        "Pro_BNP_Mf" = ifelse(is.null(input$Pro_BNP_Mf), "", input$Pro_BNP_Mf),
        "HbA1C_Mf" = ifelse(is.null(input$HbA1C_Mf), "", input$HbA1C_Mf),
        "N_Pro_BNP_Mf" = ifelse(is.null(input$N_Pro_BNP_Mf), "", input$N_Pro_BNP_Mf),
        "hs_CRP_Mf" = ifelse(is.null(input$hs_CRP_Mf), "", input$hs_CRP_Mf),
        "CK_MB_Mf" = ifelse(is.null(input$CK_MB_Mf), "", input$CK_MB_Mf),
        "Troponin_I_Mf" = ifelse(is.null(input$Troponin_I_Mf), "", input$Troponin_I_Mf),
        "Troponin_T_Mf" = ifelse(is.null(input$Troponin_T_Mf), "", input$Troponin_T_Mf),
        "Aspirin_Mf" = ifelse(is.null(input$Aspirin_Mf), "", input$Aspirin_Mf),
        "Clopidogrel_Mf" = ifelse(is.null(input$Clopidogrel_Mf), "", input$Clopidogrel_Mf),
        "Prasugrel_Mf" = ifelse(is.null(input$Prasugrel_Mf), "", input$Prasugrel_Mf),
        "Ticagrelor_Mf" = ifelse(is.null(input$Ticagrelor_Mf), "", input$Ticagrelor_Mf),
        "BB_Mf" = ifelse(is.null(input$BB_Mf), "", input$BB_Mf),
        "WN_Mf" = ifelse(is.null(input$WN_Mf), "", input$WN_Mf),
        "Statin_Mf" = ifelse(is.null(input$Statin_Mf), "", input$Statin_Mf),
        "ACE_Mf" = ifelse(is.null(input$ACE_Mf), "", input$ACE_Mf),
        "Nitrate_Mf" = ifelse(is.null(input$Nitrate_Mf), "", input$Nitrate_Mf),
        "Calcium_Mf" = ifelse(is.null(input$Calcium_Mf), "", input$Calcium_Mf),
        "Trimetazidine_Mf" = ifelse(is.null(input$Trimetazidine_Mf), "", input$Trimetazidine_Mf),
        "Event_Details_Mf" = ifelse(is.null(input$Event_Details_Mf), "", input$Event_Details_Mf),
        "Death_date_Mf" = ifelse(is.null(input$Death_date_Mf), "", as.character(input$Death_date_Mf)),
        "Death_reason_Mf" = ifelse(is.null(input$Death_reason_Mf), "", input$Death_reason_Mf),
        "MI_date_Mf" = ifelse(is.null(input$MI_date_Mf), "", as.character(input$MI_date_Mf)),
        "MI_Segment_Mf" = ifelse(is.null(input$MI_Segment_Mf), "", input$MI_Segment_Mf),
        "MI_Type_Mf" = ifelse(is.null(input$MI_Type_Mf), "", input$MI_Type_Mf),
        "MI_Stent_Mf" = ifelse(is.null(input$MI_Stent_Mf), "", input$MI_Stent_Mf),
        "MI_Lesion_Mf" = ifelse(is.null(input$MI_Lesion_Mf), "", input$MI_Lesion_Mf),
        "MI_Vessel_Mf" = ifelse(is.null(input$MI_Vessel_Mf), "", input$MI_Vessel_Mf),
        "MI_Treatment_Mf" = ifelse(is.null(input$MI_Treatment_Mf), "", input$MI_Treatment_Mf),
        "MI_After_Mf" = ifelse(is.null(input$MI_After_Mf), "", input$MI_After_Mf),
        "Rev_date_Mf" = ifelse(is.null(input$Rev_date_Mf), "", as.character(input$Rev_date_Mf)),
        "Rev_Segment_Mf" = ifelse(is.null(input$Rev_Segment_Mf), "", input$Rev_Segment_Mf),
        "Rev_Treatment_Mf" = ifelse(is.null(input$Rev_Treatment_Mf), "", input$Rev_Treatment_Mf),
        "Rev_Lesion_Mf" = ifelse(is.null(input$Rev_Lesion_Mf), "", input$Rev_Lesion_Mf),
        "Rev_Vessel_Mf" = ifelse(is.null(input$Rev_Vessel_Mf), "", input$Rev_Vessel_Mf),
        "Rev_PCI_Mf" = ifelse(is.null(input$Rev_PCI_Mf), "", input$Rev_PCI_Mf),
        "Stent_date_Mf" = ifelse(is.null(input$Stent_date_Mf), "", as.character(input$Stent_date_Mf)),
        "Stent_Segment_Mf" = ifelse(is.null(input$Stent_Segment_Mf), "", input$Stent_Segment_Mf),
        "Stent_Type_Mf" = ifelse(is.null(input$Stent_Type_Mf), "", input$Stent_Type_Mf),
        "Stent_arc_Mf" = ifelse(is.null(input$Stent_arc_Mf), "", input$Stent_arc_Mf),
        "Clinical_feature_Mf" = ifelse(is.null(input$Clinical_feature_Mf), "", input$Clinical_feature_Mf),
        "Clinical_feature_other_Mf" = ifelse(is.null(input$Clinical_feature_other_Mf), "", input$Clinical_feature_other_Mf),
        "CVA_date_Mf" = ifelse(is.null(input$CVA_date_Mf), "", as.character(input$CVA_date_Mf)),
        "CVA_Type_Mf" = ifelse(is.null(input$CVA_Type_Mf), "", input$CVA_Type_Mf),
        "Imaging_Mf" = ifelse(is.null(input$Imaging_Mf), "", input$Imaging_Mf),
        "Comment_Mf" = ifelse(is.null(input$Comment_Mf), "", input$Comment_Mf)
        
        # "Withdrawal_Mf" = ifelse(is.null(input$Withdrawal_Mf), "", input$Withdrawal_Mf),
        # "Withdrawal_Date_Mf" = ifelse(is.null(input$Withdrawal_Date_Mf), "", as.character(input$Withdrawal_Date_Mf)),
        # "Cause_Mf" = ifelse(is.null(input$Cause_Mf), "", input$Cause_Mf),
        # "Comment_Mf" = ifelse(is.null(input$Comment_Mf), "", input$Comment_Mf)
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
    updateTextInput(session, "SBP_Mf", label = "Systolic BP", value = "ND")
  })
}
