#' m1 Add & Edit Module
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
m1_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "F/U Date",
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
            width = 3,
            radioButtons(
              ns("FU_M1"),
              "F/U",
              choices = c("Yes" = 0, "No" = 1),
              selected = ifelse(is.null(hold), character(0), hold$FU_M1),
              inline = T
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.FU_M1 == 0",
              ns = ns,
              dateInput(
                ns("Visit_Date_M1"),
                "Visit date",
                value = hold$Visit_Date_M1,
                language = "ko"
              )
            ),
            conditionalPanel(
              "input.FU_M1 == 1",
              ns = ns,
              dateInput(
                ns("LastFU_M1"),
                "Date of Last F/U",
                value = hold$LastFU_M1,
                language = "ko"
              )
              ## Implement Duration
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.FU_M1 == 0",
              ns = ns,
              radioButtons(
                ns("Visit_M1"),
                "Visit by",
                choices = c("Clinic" = 0, "Phone" = 1),
                selected = ifelse(is.null(hold), character(0), hold$Visit_M1),
                inline = T
              )
            ),
            conditionalPanel(
              "input.FU_M1 == 1",
              ns = ns,
              radioButtons(
                ns("Reason_M1"),
                "Reason",
                choices = c("Patient Died" = 0, "Patient Lost to F/U" = 1, "Other" = 2),
                selected = ifelse(is.null(hold), character(0), hold$Reason_M1)
              )
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.Reason_M1 == 2", # if Other : Show text box.
              ns = ns,
              textAreaInput(
                ns("Other_M1"),
                "Other",
                width = "400px",
                height = "100px"
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Vital Sign",
              tags$div(
                modalButton("", icon("times")),
                style = "float:right;"
              ),
              actionButton(
                ns("submit0"),
                HTML('<i class="fas fa-check"></i>'),
                class = "btn",
                style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
              ),
              actionButton(
                ns("ALLND_1"),
                label = "ND",
                class = "btn",
                style = "color: white; float:right; margin-right:10px; background-color : #f39c12;"
              )
            ),
            "</h3>"
          )
        ),
        fluidRow(
          column(
            width = 4,
            textInput(
              ns("SBP_M1"), # Systolic BP
              "Systolic BP",
              value = ifelse(is.null(hold), "", hold$SBP_M1)
            )
          ),
          column(
            width = 4,
            textInput(
              ns("DBP_M1"), # Diastolic BP
              "Diastolic BP",
              value = ifelse(is.null(hold), "", hold$DBP_M1)
            )
          ),
          column(
            width = 4,
            textInput(
              ns("HRT_M1"), # Heart Ratio
              "Heart Rate",
              value = ifelse(is.null(hold), "", hold$HRT_M1)
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Title A",
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
            width = 6,
            radioButtons(
              ns("Cardiac_Status_M1"),
              "Cardiac Status",
              choices = c("Unknown" = 0, "Asymptomatic" = 1, "Angina" = 2, "Other" = 3),
              selected = hold$Cardiac_Status_M1,
              inline = TRUE
            )
          ),
          column(
            width = 6,
            conditionalPanel(
              "input.Cardiac_Status_M1 == 2", # if Angina, show CCS I, II, III, IV
              ns = ns,
              radioButtons(
                ns("CCS_M1"),
                "CCS",
                choices = c("I" = 0, "II" = 1, "III" = 2, "IV" = 3),
                selected = hold$CCS_M1, inline = T
              )
            ),
            conditionalPanel(
              "input.Cardiac_Status_M1 == 3", # if Other, show textInput
              ns = ns,
              textAreaInput(
                ns("Other_Cardiac_Status_M1"),
                "Other",
                height = "3em"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            radioButtons(
              ns("Readmission_M1"),
              "Readmission",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Readmission_M1,
              inline = T
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.Readmission_M1 == 0",
              ns = ns,
              dateInput(
                ns("Readmission_Date_M1"),
                "Readmission Date",
                value = lubridate::as_date(hold$Readmission_Date_M1),
                language = "ko"
              )
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.Readmission_M1 == 0",
              ns = ns,
              checkboxGroupInput(
                ns("Readmission_reason_M1"),
                label = "Reason",
                choices = c("Cardiac" = 0, "Non-Cardiac" = 1),
                inline = TRUE,
                selected = character(0)
              )
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              "input.Readmission_M1 == 0",
              ns = ns,
              textAreaInput(
                ns("Readmission_reason_text_M1"),
                "Detail",
                height = "3em"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(
              ns("ECG_Rhythm_M1"),
              "ECG Rhythm",
              choices = c("Sinus Rhytum" = 0, "Atrial Fibrillation" = 1, "Others" = 2, "Not Done" = 3),
              selected = hold$ECG_Rhythm_M1,
              inline = T
            )
          ),
          column(
            width = 6,
            conditionalPanel(
              "input.ECG_Rhythm_M1 == 2",
              ns = ns,
              textAreaInput(
                ns("ECG_Rhythm_Other_M1"),
                "Others",
                width = "400px",
                height = "3em"
              )
            )
          )
        ),
        # Stress ECG

        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("Stress_ECG_M1"),
              "Stress ECG (TMT)",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Stress_ECG_M1,
              inline = T
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.Stress_ECG_M1 == 0",
              ns = ns,
              dateInput(
                ns("Stress_ECG_Date_M1"),
                "Date",
                value = hold$Stress_ECG_Date_M1,
                language = "ko"
              )
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              "input.Stress_ECG_M1 == 0",
              ns = ns,
              radioButtons(
                ns("Stress_ECG_Detail_M1"),
                "Detail",
                choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
                selected = hold$Stress_ECG_Detail_M1
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Lab Data",
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
            width = 6,
            conditionalPanel(
              "input.Lab_data_Unknown_M1 == 1",
              ns = ns,
              dateInput(
                ns("Lab_Data_M1"),
                "Date of Lab",
                value = hold$Lab_Data_M1,
                language = "ko"
              )
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("Lab_data_Unknown_M1"),
              label = "Date Unknown",
              choices = c("Yes" = 0, "No" = 1),
              selected = character(0)
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(
              ns("WBC_M1"),
              "WBC",
              value = ifelse(is.null(hold), NA, hold$WBC_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("TC_M1"),
              "Total Chol",
              value = ifelse(is.null(hold), NA, hold$TC_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("Hb_M1"),
              "Hb",
              value = ifelse(is.null(hold), NA, hold$Hb_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("TG_M1"),
              "TG",
              value = ifelse(is.null(hold), NA, hold$TG_M1),
              min = 0, max = 120,
              step = 1
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(
              ns("Platelet_M1"),
              "Platelet",
              value = ifelse(is.null(hold), NA, hold$Platelet_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("HDL_M1"),
              "HDL",
              value = ifelse(is.null(hold), NA, hold$HDL_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("BUN_M1"),
              "BUN",
              value = ifelse(is.null(hold), NA, hold$BUN_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("LDL_M1"),
              "LDL",
              value = ifelse(is.null(hold), NA, hold$LDL_M1),
              min = 0, max = 120,
              step = 1
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(
              ns("Cr_M1"),
              "Cr",
              value = ifelse(is.null(hold), NA, hold$Cr_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("LDL_Cal_M1"),
              "LDL, cal",
              value = ifelse(is.null(hold), NA, hold$LDL_Cal_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("BNP_M1"),
              "BNP",
              value = ifelse(is.null(hold), NA, hold$BNP_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("FG_M1"),
              "Fasting Glucose",
              value = ifelse(is.null(hold), NA, hold$FG_M1),
              min = 0, max = 120,
              step = 1
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(
              ns("Pro_BNP_M1"),
              "pro BNP",
              value = ifelse(is.null(hold), NA, hold$Pro_BNP_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("HbA1C_M1"),
              "HbA1C",
              value = ifelse(is.null(hold), NA, hold$HbA1C_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("N_Pro_BNP_M1"),
              "N-pro BNP",
              value = ifelse(is.null(hold), NA, hold$N_Pro_BNP_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("hs_CRP_M1"),
              "hs-CRP",
              value = ifelse(is.null(hold), NA, hold$hs_CRP_M1),
              min = 0, max = 120,
              step = 1
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(
              ns("CK_MB_M1"),
              "Platelet",
              value = ifelse(is.null(hold), NA, hold$Platelet_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("Troponin_I_M1"),
              "Troponin I",
              value = ifelse(is.null(hold), NA, hold$Troponin_I_M1),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("Troponin_T_M1"),
              "Troponin T",
              value = ifelse(is.null(hold), NA, hold$Troponin_T_M1),
              min = 0, max = 120,
              step = 1
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Medication Data",
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
            width = 3,
            radioButtons(
              ns("Aspirin_M1"),
              label = "Aspirin",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Aspirin_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Clopidogrel_M1"),
              label = "Clopidogrel",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Clopidogrel_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Prasugrel_M1"),
              label = "Prasugrel",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Prasugrel_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Ticagrelor_M1"),
              label = "Ticagrelor",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Ticagrelor_M1
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons( # Beta Blocker
              ns("BB_M1"),
              label = "Beta Blocker",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$BB_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("WN_M1"),
              label = "Wafarin or NOAC",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$WN_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Statin_M1"),
              label = "Statin",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Statin_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("ACE_M1"),
              label = "ACE Inhibitor or ARB",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$ACE_M1
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("Nitrate_M1"),
              label = "Nitrate (Sigmart)",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Nitrate_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Calcium_M1"),
              label = "Calcium channel antagonist",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Calcium_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Calcium_M1"),
              label = "Calcium channel antagonist",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Calcium_M1
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Trimetazidine_M1"),
              label = "Trimetazidine",
              choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
              selected = hold$Trimetazidine_M1
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Clinical Events",
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
            width = 3,
            radioButtons(
              ns("Event_M1"),
              "Clinical Events",
              choices = c("Yes" = 0, "No" = 1),
              selected = ifelse(is.null(hold), character(0), hold$Event_M1),
              inline = TRUE
            )
          ),
          column(
            width = 9,
            conditionalPanel(
              "input.Event_M1 == 0",
              ns = ns,
              checkboxGroupInput(
                ns("Event_Details_M1"),
                "Detail",
                choices = c("Death" = 0, "MI" = 1, "Repeat Revascularization" = 2, "Stent Thrombosis" = 3, "CVA" = 4),
                selected = hold$Event_Details_M1,
                inline = TRUE
              )
            )
          )
        ),

        # Death
        conditionalPanel(
          'input.Event_Details_M1.includes("0")',
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Death",
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
              width = 3,
              dateInput(
                ns("Death_date_M1"),
                label = "Date",
                language = "ko"
              )
            ),
            column(
              width = 9,
              radioButtons(
                ns("Death_reason_M1"),
                label = "",
                choices = c("Cardiac Death" = 1, "Non-Cardiovascular Death" = 2, "Unknown Origin Death" = 3),
                inline = TRUE,
                selected = ifelse(is.null(hold), character(0), hold$Death_reason_M1)
              )
            )
          )
        ),
        conditionalPanel(
          'input.Event_Details_M1.includes("1")',
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "MI",
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
              width = 3,
              dateInput(
                ns("MI_date_M1"),
                label = "Date",
                language = "ko"
              )
            ),
            column(
              width = 3,
              numericInput(
                ns("MI_Segment_M1"),
                label = "Segment",
                value = ifelse(is.null(hold), NA, hold$MI_Segment_M1),
                min = 0, max = 120,
                step = 1
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("MI_Type_M1"),
                label = "Type", choices = c("STEMI" = 0, "NSTEMI" = 1), inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("MI_Stent_M1"),
                label = "Related with Stent Thrombosis",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("MI_Lesion_M1"),
                label = "Related with Target Lesion",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("MI_Vessel_M1"),
                label = "Related with Target Vessel",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                selected = ifelse(is.null(hold), character(0), hold$MI_Vessel_M1),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              checkboxGroupInput(
                ns("MI_Treatment_M1"),
                label = "Type of Treatment",
                choices = c("Medication Only" = 0, "Thrombolysis" = 1, "only Ballooning" = 2, "Stenting" = 3, "Bypass Surgery" = 4)
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("MI_After_M1"),
                label = "After Treatment",
                selected = ifelse(is.null(hold), character(0), hold$MI_After_M1),
                choices = c("Recovered" = 0, "Death" = 1, "Unknown" = 2)
              )
            )
          )
        ),

        # Revascularization
        conditionalPanel(
          'input.Event_Details_M1.includes("2")',
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Revascularization",
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
              width = 3,
              dateInput(
                ns("Rev_date_M1"),
                label = "Date",
                language = "ko"
              )
            ),
            column(
              width = 3,
              numericInput(
                ns("Rev_Segment_M1"),
                label = "Segment",
                value = ifelse(is.null(hold), NA, hold$Rev_Segment_M1),
                min = 0, max = 120,
                step = 1
              )
            ),
            column(
              width = 3,
              checkboxGroupInput(
                ns("Rev_Treatment_M1"),
                label = "Type of Treatment",
                choices = c("only Ballooning" = 0, "Stenting" = 1, "Bypass Surgery" = 2)
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Rev_Lesion_M1"),
                label = "Related with Target Lesion",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("Rev_Vessel_M1"),
                label = "Related with Target Vessel",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Rev_PCI_M1"),
                label = "Another Vessel PCI",
                choices = c("Yes" = 0, "No" = 1),
                inline = TRUE
              )
            )
          )
        ),
        
        conditionalPanel(
          'input.Event_Details_M1.includes("3")',
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Stent Thrombosis",
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
              width = 3,
              dateInput(
                ns("Stent_date_M1"),
                label = "Date",
                language = "ko"
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Stent_Segment_M1"),
                label = "Segment",
                value = ifelse(is.null(hold), "", hold$Stent_Segment_M1)
              )
            ),
            column(
              width = 6,
              radioButtons(
                ns("Stent_arc_M1"),
                label = "ARC",
                choices = c("Definite/Confirmed" = 0, "Probable" = 1, "Possible" = 2),
                selected = ifelse(is.null(hold), character(0), hold$Stent_arc_M1),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              radioButtons(
                ns("Stent_Type_M1"),
                label = "Type",
                choices = c("Acute (< 1d)" = 0, "Subacute (1-30d)" = 1, "Late(> 1m)" = 2, "Very Late(> 1y)" = 3),
                selected = ifelse(is.null(hold), character(0), hold$Stent_Type_M1)
              )
            ),
            column(
              width = 4,
              checkboxGroupInput(
                inputId = ns("Clinical_feature_M1"),
                label = "Clinical Features",
                choices = c("Sudden Death" = 0, "STEMI" = 1, "NSTEMI" = 2, "Unstable Angina" = 3, "Stable Angina" = 4, "Other" = 5),
                selected = ifelse(is.null(hold), character(0), hold$Clinical_feature_M1)
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                'input.Clinical_feature_M1.includes("5")',
                ns = ns,
                textAreaInput(
                  ns("Clinical_feature_other_M1"),
                  "Other",
                  width = "400px",
                  height = "3em"
                )
              )
            )
          )
        ),
        conditionalPanel(
          'input.Event_Details_M1.includes("4")',
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "CVA",
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
              width = 3,
              dateInput(
                ns("CVA_date_M1"),
                label = "Date",
                language = "ko"
              )
            ),
            column(
              width = 5,
              radioButtons(
                ns("CVA_Type_M1"),
                label = "Type",
                choices = c("Ischemic" = 0, "Hemorrhagic" = 1, "Unknown" = 2),
                inline = TRUE
              )
            ),
            column(
              width = 4,
              radioButtons(
                ns("Imaging_M1"),
                label = "Verified with imaging studies",
                choices = c("Yes" = 0, "No" = 1),
                inline = TRUE
              )
            )
          )
        ),

        # textAreaInput(
        #   ns("Comment_M1"),
        #   "Comment",
        #   width = "400px",
        #   height = "100px",
        #   value =  ifelse(is.null(hold$Comment_M1), "", hold$Comment_M1)
        # ),
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

  observeEvent(input$ALLND_1, {
    updateTextInput(session, "SBP_M1", value = "ND")
    updateTextInput(session, "DBP_M1", value = "ND")
    updateTextInput(session, "HRT_M1", value = "ND")
  })

  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      data = list(
        "FU_M1" = ifelse(is.null(input$FU_M1), "", input$FU_M1),
        "Visit_Date_M1" = ifelse(is.null(input$Visit_Date_M1), "", as.character(input$Visit_Date_M1)),
        "Visit_M1" = ifelse(is.null(input$Visit_M1), "", input$Visit_M1),
        "Reason_M1" = ifelse(is.null(input$Reason_M1), "", input$Reason_M1),
        "Other_M1" = ifelse(is.null(input$Other_M1), "", input$Other_M1),
        "LastFU_M1" = ifelse(is.null(input$LastFU_M1), "", as.character(input$LastFU_M1)),
        "SBP_M1" = ifelse(is.null(input$SBP_M1), "", input$SBP_M1),
        "DBP_M1" = ifelse(is.null(input$DBP_M1), "", input$DBP_M1),
        "HRT_M1" = ifelse(is.null(input$HRT_M1), "", input$HRT_M1),
        "Event_M1" = ifelse(is.null(input$Event_M1), "", input$Event_M1),
        "Cardiac_Status_M1" = ifelse(is.null(input$Cardiac_Status_M1), "", input$Cardiac_Status_M1),
        "CCS_M1" = ifelse(is.null(input$CCS_M1), "", input$CCS_M1),
        "Other_Cardiac_Status_M1" = ifelse(is.null(input$Other_Cardiac_Status_M1), "", input$Other_Cardiac_Status_M1),
        "Readmission_M1" = ifelse(is.null(input$Readmission_M1), "", input$Readmission_M1),
        "Readmission_Date_M1" = ifelse(is.null(input$Readmission_Date_M1), "", input$Readmission_Date_M1),
        "Readmission_reason_M1" = ifelse(is.null(input$Readmission_reason_M1), "", input$Readmission_reason_M1),
        "Readmission_reason_text_M1" = ifelse(is.null(input$Readmission_reason_text_M1), "", input$Readmission_reason_text_M1),
        "ECG_Rhythm_M1" = ifelse(is.null(input$ECG_Rhythm_M1), "", input$ECG_Rhythm_M1),
        "ECG_Rhythm_Other_M1" = ifelse(is.null(input$ECG_Rhythm_Other_M1), "", input$ECG_Rhythm_Other_M1),
        "Stress_ECG_M1" = ifelse(is.null(input$Stress_ECG_M1), "", input$Stress_ECG_M1),
        "Stress_ECG_Date_M1" = ifelse(is.null(input$Stress_ECG_Date_M1), "", as.character(input$Stress_ECG_Date_M1)),
        "Stress_ECG_Detail_M1" = ifelse(is.null(input$Stress_ECG_Detail_M1), "", input$Stress_ECG_Detail_M1),
        "Lab_data_Unknown_M1" = ifelse(is.null(input$Lab_data_Unknown_M1), "", input$Lab_data_Unknown_M1),
        "Lab_Data_M1" = ifelse(is.null(input$Lab_Data_M1), "", as.character(input$Lab_Data_M1)),
        "WBC_M1" = ifelse(is.null(input$WBC_M1), "", input$WBC_M1),
        "TC_M1" = ifelse(is.null(input$TC_M1), "", input$TC_M1),
        "Hb_M1" = ifelse(is.null(input$Hb_M1), "", input$Hb_M1),
        "TG_M1" = ifelse(is.null(input$TG_M1), "", input$TG_M1),
        "Platelet_M1" = ifelse(is.null(input$Platelet_M1), "", input$Platelet_M1),
        "HDL_M1" = ifelse(is.null(input$HDL_M1), "", input$HDL_M1),
        "BUN_M1" = ifelse(is.null(input$BUN_M1), "", input$BUN_M1),
        "LDL_M1" = ifelse(is.null(input$LDL_M1), "", input$LDL_M1),
        "Cr_M1" = ifelse(is.null(input$Cr_M1), "", input$Cr_M1),
        "LDL_Cal_M1" = ifelse(is.null(input$LDL_Cal_M1), "", input$LDL_Cal_M1),
        "BNP_M1" = ifelse(is.null(input$BNP_M1), "", input$BNP_M1),
        "FG_M1" = ifelse(is.null(input$FG_M1), "", input$FG_M1),
        "Pro_BNP_M1" = ifelse(is.null(input$Pro_BNP_M1), "", input$Pro_BNP_M1),
        "HbA1C_M1" = ifelse(is.null(input$HbA1C_M1), "", input$HbA1C_M1),
        "N_Pro_BNP_M1" = ifelse(is.null(input$N_Pro_BNP_M1), "", input$N_Pro_BNP_M1),
        "hs_CRP_M1" = ifelse(is.null(input$hs_CRP_M1), "", input$hs_CRP_M1),
        "CK_MB_M1" = ifelse(is.null(input$CK_MB_M1), "", input$CK_MB_M1),
        "Troponin_I_M1" = ifelse(is.null(input$Troponin_I_M1), "", input$Troponin_I_M1),
        "Troponin_T_M1" = ifelse(is.null(input$Troponin_T_M1), "", input$Troponin_T_M1),
        "Aspirin_M1" = ifelse(is.null(input$Aspirin_M1), "", input$Aspirin_M1),
        "Clopidogrel_M1" = ifelse(is.null(input$Clopidogrel_M1), "", input$Clopidogrel_M1),
        "Prasugrel_M1" = ifelse(is.null(input$Prasugrel_M1), "", input$Prasugrel_M1),
        "Ticagrelor_M1" = ifelse(is.null(input$Ticagrelor_M1), "", input$Ticagrelor_M1),
        "BB_M1" = ifelse(is.null(input$BB_M1), "", input$BB_M1),
        "WN_M1" = ifelse(is.null(input$WN_M1), "", input$WN_M1),
        "Statin_M1" = ifelse(is.null(input$Statin_M1), "", input$Statin_M1),
        "ACE_M1" = ifelse(is.null(input$ACE_M1), "", input$ACE_M1),
        "Nitrate_M1" = ifelse(is.null(input$Nitrate_M1), "", input$Nitrate_M1),
        "Calcium_M1" = ifelse(is.null(input$Calcium_M1), "", input$Calcium_M1),
        "Trimetazidine_M1" = ifelse(is.null(input$Trimetazidine_M1), "", input$Trimetazidine_M1),
        "Event_Details_M1" = ifelse(is.null(input$Event_Details_M1), "", input$Event_Details_M1),
        "Death_date_M1" = ifelse(is.null(input$Death_date_M1), "", as.character(input$Death_date_M1)),
        "Death_reason_M1" = ifelse(is.null(input$Death_reason_M1), "", input$Death_reason_M1),
        "MI_date_M1" = ifelse(is.null(input$MI_date_M1), "", as.character(input$MI_date_M1)),
        "MI_Segment_M1" = ifelse(is.null(input$MI_Segment_M1), "", input$MI_Segment_M1),
        "MI_Type_M1" = ifelse(is.null(input$MI_Type_M1), "", input$MI_Type_M1),
        "MI_Stent_M1" = ifelse(is.null(input$MI_Stent_M1), "", input$MI_Stent_M1),
        "MI_Lesion_M1" = ifelse(is.null(input$MI_Lesion_M1), "", input$MI_Lesion_M1),
        "MI_Vessel_M1" = ifelse(is.null(input$MI_Vessel_M1), "", input$MI_Vessel_M1),
        "MI_Treatment_M1" = ifelse(is.null(input$MI_Treatment_M1), "", input$MI_Treatment_M1),
        "MI_After_M1" = ifelse(is.null(input$MI_After_M1), "", input$MI_After_M1),
        "Rev_date_M1" = ifelse(is.null(input$Rev_date_M1), "", as.character(input$Rev_date_M1)),
        "Rev_Segment_M1" = ifelse(is.null(input$Rev_Segment_M1), "", input$Rev_Segment_M1),
        "Rev_Treatment_M1" = ifelse(is.null(input$Rev_Treatment_M1), "", input$Rev_Treatment_M1),
        "Rev_Lesion_M1" = ifelse(is.null(input$Rev_Lesion_M1), "", input$Rev_Lesion_M1),
        "Rev_Vessel_M1" = ifelse(is.null(input$Rev_Vessel_M1), "", input$Rev_Vessel_M1),
        "Rev_PCI_M1" = ifelse(is.null(input$Rev_PCI_M1), "", input$Rev_PCI_M1),
        "Stent_date_M1" = ifelse(is.null(input$Stent_date_M1), "", as.character(input$Stent_date_M1)),
        "Stent_Segment_M1" = ifelse(is.null(input$Stent_Segment_M1), "", input$Stent_Segment_M1),
        "Stent_Type_M1" = ifelse(is.null(input$Stent_Type_M1), "", input$Stent_Type_M1),
        "Stent_arc_M1" = ifelse(is.null(input$Stent_arc_M1), "", input$Stent_arc_M1),
        "Clinical_feature_M1" = ifelse(is.null(input$Clinical_feature_M1), "", input$Clinical_feature_M1),
        "Clinical_feature_other_M1" = ifelse(is.null(input$Clinical_feature_other_M1), "", input$Clinical_feature_other_M1),
        "CVA_date_M1" = ifelse(is.null(input$CVA_date_M1), "", as.character(input$CVA_date_M1)),
        "CVA_Type_M1" = ifelse(is.null(input$CVA_Type_M1), "", input$CVA_Type_M1),
        "Imaging_M1" = ifelse(is.null(input$Imaging_M1), "", input$Imaging_M1),
        "Comment_M1" = ifelse(is.null(input$Comment_M1), "", input$Comment_M1)

        # "Withdrawal_M1" = ifelse(is.null(input$Withdrawal_M1), "", input$Withdrawal_M1),
        # "Withdrawal_Date_M1" = ifelse(is.null(input$Withdrawal_Date_M1), "", as.character(input$Withdrawal_Date_M1)),
        # "Cause_M1" = ifelse(is.null(input$Cause_M1), "", input$Cause_M1),
        # "Comment_M1" = ifelse(is.null(input$Comment_M1), "", input$Comment_M1)
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
