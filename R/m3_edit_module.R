#' m3 Add & Edit Module
#'
#' Module to add & edit 3 month in the database file
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
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Follow Up",
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
              ns("FU_M3"),
              "F/U",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$FU_M3), character(0), hold$FU_M3),
              inline = T
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.FU_M3 == 'Yes'",
              ns = ns,
              dateInput(
                ns("Visit_Date_M3"),
                "Visit date",
                value = lubridate::as_date(hold$Visit_Date_M3),
                language = "ko"
              )
            ),
            conditionalPanel(
              "input.FU_M3 == 'No'",
              ns = ns,
              dateInput(
                ns("LastFU_M3"),
                "Date of Last F/U",
                value = lubridate::as_date(hold$LastFU_M3),
                language = "ko"
              )
              ## Implement Duration
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.FU_M3 == 'Yes'",
              ns = ns,
              radioButtons(
                ns("Visit_M3"),
                "Visit by",
                choices = c("Clinic", "Phone"),
                selected = ifelse(is.null(hold$Visit_M3), character(0), hold$Visit_M3),
                inline = T
              )
            ),
            conditionalPanel(
              "input.FU_M3 == 'No",
              ns = ns,
              radioButtons(
                ns("Reason_M3"),
                "Reason",
                choices = c("Patient Died", "Patient Lost to F/U", "Other"),
                selected = ifelse(is.null(hold$Reason_M3), character(0), hold$Reason_M3)
              )
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.Reason_M3 == 'Other'", # if Other : Show text box.
              ns = ns,
              textAreaInput(
                ns("Other_M3"),
                "Other",
                width = "400px",
                value = ifelse(is.null(hold$Other_M3), '', hold$Other_M3),
                height = "100px"
              )
            )
          )
        ),
        
        conditionalPanel(
          "input.FU_M3 == 'Yes'",
          ns = ns,
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
                  ns("submit1"),
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
                ns("SBP_M3"), # Systolic BP
                "Systolic BP",
                value = ifelse(is.null(hold$SBP_M3), "", hold$SBP_M3)
              )
            ),
            column(
              width = 4,
              textInput(
                ns("DBP_M3"), # Diastolic BP
                "Diastolic BP",
                value = ifelse(is.null(hold$DBP_M3), "", hold$DBP_M3)
              )
            ),
            column(
              width = 4,
              textInput(
                ns("HRT_M3"), # Heart Ratio
                "Heart Rate",
                value = ifelse(is.null(hold$HRT_M3), "", hold$HRT_M3)
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
                  ns("submit2"),
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
                ns("Cardiac_Status_M3"),
                "Cardiac Status",
                choices = c("Unknown", "Asymptomatic", "Angina", "Other"),
                selected = ifelse(is.null(hold$Cardiac_Status_M3),character(0), hold$Cardiac_Status_M3),
                inline = TRUE
              )
            ),
            column(
              width = 6,
              conditionalPanel(
                "input.Cardiac_Status_M3 == 'Angina'", # if Angina, show CCS I, II, III, IV
                ns = ns,
                radioButtons(
                  ns("CCS_M3"),
                  "CCS",
                  choices = c("I", "II", "III", "IV"),
                  selected = ifelse(is.null(hold$CCS_M3),character(0), hold$CCS_M3),
                  inline = T
                )
              ),
              conditionalPanel(
                "input.Cardiac_Status_M3 == 'Other'", # if Other, show textInput
                ns = ns,
                textAreaInput(
                  ns("Other_Cardiac_Status_M3"),
                  "Other",
                  value = ifelse(is.null(hold$Other_Cardiac_Status_M3), '', hold$Other_Cardiac_Status_M3),
                  height = "3em"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("ECG_Rhythm_M3"),
                "ECG Rhythm",
                choices = c("Sinus Rhytum", "Atrial Fibrillation", "Others", "Not Done"),
                selected = ifelse(is.null(hold$ECG_Rhythm_M3), character(0), hold$ECG_Rhythm_M3),
                inline = T
              )
            ),
            column(
              width = 6,
              conditionalPanel(
                "input.ECG_Rhythm_M3 == 'Others'",
                ns = ns,
                textAreaInput(
                  ns("ECG_Rhythm_Other_M3"),
                  "Others",
                  value = ifelse(is.null(hold$ECG_Rhythm_Other_M3), "", hold$ECG_Rhythm_Other_M3),
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
                ns("Stress_ECG_M3"),
                "Stress ECG (TMT)",
                choices = c("Yes", "No"),
                selected = ifelse(is.null(hold$Stress_ECG_M3), character(0), hold$Stress_ECG_M3),
                inline = T
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                "input.Stress_ECG_M3 == 'Yes'",
                ns = ns,
                dateInput(
                  ns("Stress_ECG_Date_M3"),
                  "Date",
                  value = lubridate::as_date(hold$Stress_ECG_Date_M3),
                  language = "ko"
                )
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                "input.Stress_ECG_M3 == 'Yes'",
                ns = ns,
                radioButtons(
                  ns("Stress_ECG_Detail_M3"),
                  "Detail",
                  choices = c("Positive", "Negative", "Equivocal", "Incomplete (Inadequate)"),
                  selected = ifelse(is.null(hold$Stress_ECG_Detail_M3), character(0), hold$Stress_ECG_Detail_M3),
                )
              )
            )
          ),
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Baseline Lab Data",
                tags$div(
                  modalButton("", icon("times")),
                  style = "float:right;"
                ),
                actionButton(
                  ns("submit3"),
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
                ns("Lab_Date_M3"),
                "Lab Date",
                value = lubridate::as_date(hold$Lab_Date_M3),
                language = "ko"
              ),
            ),
            column(
              width = 3,
              textInput(
                ns("WBC_M3"),
                placeholder = "10^3/ul",
                HTML(
                  paste0(
                    "WBC",
                    actionButton(
                      inputId = ns("wbcND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold), "", hold$WBC_M3)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("TC_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "Total Chol",
                    actionButton(
                      inputId = ns("tcND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$TC_M3), "", hold$TC_M3)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Hb_M3"),
                placeholder = "g/dl",
                HTML(
                  paste0(
                    "Hgb",
                    actionButton(
                      inputId = ns("hbND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$Hb_M3), "", hold$Hb_M3)
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("TG_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "Triglyceride",
                    actionButton(
                      inputId = ns("tgND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$TG_M3), "", hold$TG_M3)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Platelet_M3"),
                placeholder = "10^3/ul",
                HTML(
                  paste0(
                    "Platelet",
                    actionButton(
                      inputId = ns("plND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$Platelet_M3), "", hold$Platelet_M3)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("HDL_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "HDL",
                    actionButton(
                      inputId = ns("hdlND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$HDL_M3), "", hold$HDL_M3),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("BUN_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "BUN",
                    actionButton(
                      inputId = ns("bunND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$BUN_M3), "", hold$BUN_M3),
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("LDL_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "LDL",
                    actionButton(
                      inputId = ns("ldlND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$LDL_M3), "", hold$LDL_M3),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Cr_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "Cr",
                    actionButton(
                      inputId = ns("crND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$Cr_M3), "", hold$Cr_M3),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("FG_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "Fasting Glucose",
                    actionButton(
                      inputId = ns("fgND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$FG_M3), "", hold$FG_M3),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("MDRD_M3"),
                placeholder = "ml/min/1.73m2",
                HTML(
                  paste0(
                    "MDRD-eGFR",
                    actionButton(
                      inputId = ns("mdND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$MDRD_M3), "", hold$MDRD_M3),
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("HbA1C_M3"),
                placeholder = "%",
                HTML(
                  paste0(
                    "HbA1C",
                    actionButton(
                      inputId = ns("hb1ND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$HbA1C_M3), "", hold$HbA1C_M3),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("AST_M3"),
                placeholder = "IU/L",
                HTML(
                  paste0(
                    "AST",
                    actionButton(
                      inputId = ns("astND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$AST_M3), "", hold$AST_M3),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("hs_CRP_M3"),
                placeholder = "mg/dl",
                HTML(
                  paste0(
                    "hs-CRP",
                    actionButton(
                      inputId = ns("hsND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$hs_CRP_M3), "", hold$hs_CRP_M3),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("ALT_M3"),
                placeholder = "IU/L",
                HTML(
                  paste0(
                    "ALT",
                    actionButton(
                      inputId = ns("altND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$ALT_M3), "", hold$ALT_M3)
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("NT_Pro_BNP_M3"),
                placeholder = "pg/ml",
                HTML(
                  paste0(
                    "NT-Pro BNP",
                    actionButton(
                      inputId = ns("ntpND"),
                      label = "ND",
                      style = "font-size : 2px; padding : 0px 2em 0px 2em; margin-left : 10px; background-color : #f39c12"
                    )
                  )
                ),
                value = ifelse(is.null(hold$NT_Pro_BNP_M3), "", hold$NT_Pro_BNP_M3),
              )
            )
          ),
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Medication",
                tags$div(
                  modalButton("", icon("times")),
                  style = "float:right;"
                ),
                actionButton(
                  ns("submit4"),
                  HTML('<i class="fas fa-check"></i>'),
                  class = "btn",
                  style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
                ),
                actionButton(
                  ns("CYfA"),
                  "Yes",
                  class = "btn",
                  style = "color: white; float:right; margin-right:10px; background-color : #f39c12;"
                )
              ),
              "</h3>"
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("Aspirin_M3"),
                label = "Aspirin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Aspirin_M3) ,character(0),hold$Aspirin_M3),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Trimetazidine_M3"),
                label = "Trimetazidine (Vastinan)",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Trimetazidine_M3), character(0),hold$Trimetazidine_M3),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Clopidogrel_M3"),
                label = "Clopidogrel",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Clopidogrel_M3), character(0),hold$Clopidogrel_M3),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Nitrate_M3"),
                label = "Nitrate",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Nitrate_M3), character(0),hold$Nitrate_M3),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("Prasugrel_M3"),
                label = "Prasugrel",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Prasugrel_M3), character(0),hold$Prasugrel_M3),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Ticagrelor_M3"),
                label = "Ticagrelor",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Ticagrelor_M3),character(0), hold$Ticagrelor_M3),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Wafarin_M3"),
                label = "Wafarin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Wafarin_M3),character(0), hold$Wafarin_M3),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Nicorandil_M3"),
                label = "Nicorandil",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Nicorandil_M3),character(0), hold$Nicorandil_M3),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("NOAC_M3"),
                label = "NOAC",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$NOAC_M3),character(0), hold$NOAC_M3),
                inline = T
              ),
              conditionalPanel(
                "input.NOAC_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("NOAC_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$NOAC_name_M3), '', hold$NOAC_name_M3),
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("NOAC_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$NOAC_dose_M3), character(0), hold$NOAC_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Statin_M3"),
                label = "Statin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Statin_M3), character(0),hold$Statin_M3),
                inline = T
              ),
              conditionalPanel(
                "input.Statin_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Statin_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$Statin_name_M3), '',hold$Statin_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Statin_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Statin_dose_M3),character(0), hold$Statin_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons( # Beta Blocker
                ns("BB_M3"),
                label = "Beta Blocker",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$BB_M3), character(0), hold$BB_M3),
                inline = T
              ),
              conditionalPanel(
                "input.BB_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("BB_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$BB_name_M3), '',hold$BB_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("BB_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$BB_dose_M3), character(0), hold$BB_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Insulin_M3"),
                label = "Insulin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Insulin_M3),character(0), hold$Insulin_M3),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("ACE_M3"),
                label = "ACE Inhibitor or ARB",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$ACE_M3), character(0), hold$ACE_M3),
                inline = T
              ),
              conditionalPanel(
                "input.ACE_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("ACE_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$ACE_name_M3), '',hold$ACE_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("ACE_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$ACE_dose_M3), character(0), hold$ACE_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("DPP4_M3"),
                label = "DPP4 Inhibitor",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$DPP4_M3), character(0), hold$DPP4_M3),
                inline = T
              ),
              conditionalPanel(
                "input.DPP4_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("DPP4_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$DPP4_name_M3), '',hold$DPP4_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("DPP4_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$DPP4_dose_M3), character(0), hold$DPP4_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Calcium_M3"),
                label = "Calcium channel blocker",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Calcium_M3), character(0), hold$Calcium_M3),
                inline = T
              ),
              conditionalPanel(
                "input.Calcium_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Calcium_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$Calcium_name_M3), '',hold$Calcium_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Calcium_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Calcium_dose_M3), character(0), hold$Calcium_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Metformin_M3"),
                label = "Metformin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Metformin_M3),character(0), hold$Metformin_M3),
                inline = T
              ),
              conditionalPanel(
                "input.Metformin_M3 == 'Yes'",
                ns = ns,
                numericInput(
                  ns("Metformin_dose_M3"),
                  label = "Dose",
                  value = ifelse(is.null(hold$Metformin_dose_M3),character(0), hold$Metformin_dose_M3),
                  min = 0,
                  max = 200,
                  step = 1
                )
              )
            ),
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("Sulf_M3"),
                label = "Sulfonylurea",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Sulf_M3),character(0), hold$Sulf_M3),
                inline = T
              ),
              conditionalPanel(
                "input.Sulf_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Sulf_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$Sulf_name_M3), '', hold$Sulf_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Sulf_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Sulf_dose_M3), character(0), hold$Sulf_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Thia_M3"),
                label = "Thiazolidinedione",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Thia_M3),character(0), hold$Thia_M3),
                inline = T
              ),
              conditionalPanel(
                "input.Thia_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Thia_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$Thia_name_M3), '', hold$Thia_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Thia_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Thia_dose_M3),character(0), hold$Thia_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("GLP_M3"),
                label = "GLP-1 Agonist",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$GLP_M3),character(0), hold$GLP_M3),
                inline = T
              ),
              conditionalPanel(
                "input.GLP_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("GLP_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$GLP_name_M3), '', hold$GLP_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("GLP_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$GLP_dose_M3),character(0),hold$GLP_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Alpha_M3"),
                label = "Alpha-glucosidase inhibitor",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Alpha_M3),character(0),hold$Alpha_M3),
                inline = T
              ),
              conditionalPanel(
                "input.Alpha_M3 == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Alpha_name_M3"),
                      label = "Name",
                      value = ifelse(is.null(hold$Alpha_name_M3), '',hold$Alpha_name_M3)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Alpha_dose_M3"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Alpha_dose_M3),character(0), hold$Alpha_dose_M3),
                      min = 0,
                      max = 200,
                      step = 1
                    )
                  )
                )
              )
            ),
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
                  ns("submit5"),
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
              width = 2,
              radioButtons(
                ns("Event_M3"),
                "Clinical Events",
                choices = c("Yes", "No"),
                selected = ifelse(is.null(hold), character(0), hold$Event_M3),
                inline = TRUE
              )
            ),
            column(
              width = 10,
              conditionalPanel(
                "input.Event_M3 == 'Yes'",
                ns = ns,
                checkboxGroupInput(
                  ns("Event_Details_M3"),
                  "Detail",
                  choices = c("Death", "MI", "Repeat Revascularization", "Stent Thrombosis", "CVA", "Bleeding", "Readmission"),
                  selected = strsplit(ifelse(is.null(hold$Event_Details_M3), character(0), hold$Event_Details_M3), ',')[[1]],
                  inline = TRUE
                )
              )
            )
          ),
          
          # Death
          conditionalPanel(
            'input.Event_Details_M3.includes("Death")',
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
                    ns("submit6"),
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
                  ns("Death_date_M3"),
                  label = "Date",
                  value = lubridate::as_date(hold$Death_date_M3),
                  language = "ko"
                )
              ),
              column(
                width = 9,
                radioButtons(
                  ns("Death_reason_M3"),
                  label = "",
                  choices = c("Cardiac Death", "Non-Cardiovascular Death", "Unknown Origin Death"),
                  inline = TRUE,
                  selected = ifelse(is.null(hold$Death_reason_M3), character(0), hold$Death_reason_M3)
                )
              )
            )
          ),
          conditionalPanel(
            'input.Event_Details_M3.includes("MI")',
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
                    ns("submit7"),
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
                  ns("MI_date_M3"),
                  label = "Date",
                  value = lubridate::as_date(hold$MI_date_M3),
                  language = "ko"
                )
              ),
              column(
                width = 3,
                numericInput(
                  ns("MI_Segment_M3"),
                  label = "Segment",
                  value = ifelse(is.null(hold$MI_Segment_M3), character(0), hold$MI_Segment_M3),
                  min = 0, max = 120,
                  step = 1
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_Type_M3"),
                  label = "Type", 
                  choices = c("STEMI", "NSTEMI"), 
                  selected = ifelse(is.null(hold$MI_Type_M3), character(0), hold$MI_Type_M3),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_Stent_M3"),
                  label = "Related with Stent Thrombosis",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$MI_Stent_M3), character(0), hold$MI_Stent_M3),
                  inline = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                radioButtons(
                  ns("MI_Lesion_M3"),
                  label = "Related with Target Lesion",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$MI_Lesion_M3), character(0), hold$MI_Lesion_M3),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_Vessel_M3"),
                  label = "Related with Target Vessel",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold), character(0), hold$MI_Vessel_M3),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                checkboxGroupInput(
                  ns("MI_Treatment_M3"),
                  label = "Type of Treatment",
                  choices = c("Medication Only", "Thrombolysis", "only Ballooning", "Stenting", "Bypass Surgery"),
                  selected = strsplit(ifelse(is.null(hold$MI_Treatment_M3), character(0), hold$MI_Treatment_M3), ',')[[1]]
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_After_M3"),
                  label = "After Treatment",
                  selected = ifelse(is.null(hold$MI_After_M3), character(0), hold$MI_After_M3),
                  choices = c("Recovered", "Death", "Unknown")
                )
              )
            )
          ),
          
          # Revascularization
          conditionalPanel(
            'input.Event_Details_M3.includes("Repeat Revascularization")',
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
                    ns("submit8"),
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
                  ns("Rev_date_M3"),
                  label = "Date",
                  language = "ko",
                  value = lubridate::as_date(hold$Rev_date_M3)
                )
              ),
              column(
                width = 3,
                numericInput(
                  ns("Rev_Segment_M3"),
                  label = "Segment",
                  value = ifelse(is.null(hold$Rev_Segment_M3), character(0), hold$Rev_Segment_M3),
                  min = 0, max = 120,
                  step = 1
                )
              ),
              column(
                width = 3,
                checkboxGroupInput(
                  ns("Rev_Treatment_M3"),
                  label = "Type of Treatment",
                  choices = c("only Ballooning", "Stenting", "Bypass Surgery"),
                  selected = strsplit(ifelse(is.null(hold$Rev_Treatment_M3), character(0), hold$Rev_Treatment_M3), ',')[[1]]
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("Rev_Lesion_M3"),
                  label = "Related with Target Lesion",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$Rev_Lesion_M3), character(0), hold$Rev_Lesion_M3),
                  inline = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                radioButtons(
                  ns("Rev_Vessel_M3"),
                  label = "Related with Target Vessel",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$Rev_Vessel_M3), character(0), hold$Rev_Vessel_M3),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("Rev_PCI_M3"),
                  label = "Another Vessel PCI",
                  choices = c("Yes", "No"),
                  selected = ifelse(is.null(hold$Rev_PCI_M3), character(0), hold$Rev_PCI_M3),
                  inline = TRUE
                )
              )
            )
          ),
          conditionalPanel(
            'input.Event_Details_M3.includes("Stent Thrombosis")',
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
                    ns("submit9"),
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
                  ns("Stent_date_M3"),
                  label = "Date",
                  value = lubridate::as_date(hold$Stent_date_M3),
                  language = "ko"
                )
              ),
              column(
                width = 3,
                textInput(
                  ns("Stent_Segment_M3"),
                  label = "Segment",
                  value = ifelse(is.null(hold$Stent_Segment_M3), "", hold$Stent_Segment_M3)
                )
              ),
              column(
                width = 6,
                radioButtons(
                  ns("Stent_arc_M3"),
                  label = "ARC",
                  choices = c("Definite/Confirmed", "Probable", "Possible"),
                  selected = ifelse(is.null(hold$Stent_arc_M3), character(0), hold$Stent_arc_M3),
                  inline = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                radioButtons(
                  ns("Stent_Type_M3"),
                  label = "Type",
                  choices = c("Acute (< 1d)", "Subacute (1-30d)", "Late(> 1m)", "Very Late(> 1y)"),
                  selected = ifelse(is.null(hold$Stent_Type_M3), character(0), hold$Stent_Type_M3)
                )
              ),
              column(
                width = 4,
                checkboxGroupInput(
                  inputId = ns("Clinical_feature_M3"),
                  label = "Clinical Features",
                  choices = c("Sudden Death", "STEMI", "NSTEMI", "Unstable Angina", "Stable Angina", "Other"),
                  selected = strsplit(ifelse(is.null(hold$Clinical_feature_M3), character(0), hold$Clinical_feature_M3), ',')[[1]],
                )
              ),
              column(
                width = 4,
                conditionalPanel(
                  'input.Clinical_feature_M3.includes("Other")',
                  ns = ns,
                  textAreaInput(
                    ns("Clinical_feature_other_M3"),
                    "Other",
                    width = "400px",
                    value = ifelse(is.null(hold$Clinical_feature_other_M3), '', hold$Clinical_feature_other_M3),
                    height = "3em"
                  )
                )
              )
            )
          ),
          conditionalPanel(
            'input.Event_Details_M3.includes("CVA")',
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
                    ns("submit10"),
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
                  ns("CVA_date_M3"),
                  label = "Date",
                  value = lubridate::as_date(hold$CVA_date_M3),
                  language = "ko"
                )
              ),
              column(
                width = 5,
                radioButtons(
                  ns("CVA_Type_M3"),
                  label = "Type",
                  choices = c("Ischemic", "Hemorrhagic", "Unknown"),
                  selected = ifelse(is.null(hold$CVA_Type_M3), character(0), hold$CVA_Type_M3),
                  inline = TRUE
                )
              ),
              column(
                width = 4,
                radioButtons(
                  ns("Imaging_M3"),
                  label = "Verified with imaging studies",
                  selected = ifelse(is.null(hold$Imaging_M3), character(0), hold$Imaging_M3),
                  choices = c("Yes", "No"),
                  inline = TRUE
                )
              )
            )
          ),
          
          # Bleeding
          conditionalPanel(
            'input.Event_Details_M3.includes("Bleeding")',
            ns = ns,
            tags$div(
              HTML(
                paste0(
                  '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                  "Bleeding",
                  tags$div(
                    modalButton("", icon("times")),
                    style = "float:right;"
                  ),
                  actionButton(
                    ns("submit11"),
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
                width = 2,
                numericInput(
                  ns("Bleed_count_M3"),
                  "Count",
                  value = ifelse(is.null(hold$Bleed_count_M3), 0, hold$Bleed_count_M3),
                  min = 1,
                  max = 2
                )
              )
            ),
            conditionalPanel(
              "input.Bleed_count_M3 >= 1",
              ns = ns,
              fluidRow(
                column(
                  width = 2,
                  dateInput(
                    ns("Bleed1_date_M3"),
                    "Date",
                    value = lubridate::as_date(hold$Bleed1_date_M3),
                    language = "ko"
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_Type_M3"),
                    "BARC Type",
                    choices = c("BARC 2", "BARC 3", "BARC 5"),
                    selected = ifelse(is.null(hold$BARC1_Type_M3), character(0), hold$BARC1_Type_M3),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_ST_M3"),
                    "Spontaneous or Traumatic",
                    choices = c("Spontaneous", "Traumatic"),
                    selected = ifelse(is.null(hold$BARC1_ST_M3), character(0), hold$BARC1_ST_M3),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_HT_M3"),
                    "Requiring hospitalization or transfusion",
                    choices = c("Yes", "No"),
                    selected = ifelse(is.null(hold$BARC1_HT_M3), character(0), hold$BARC1_HT_M3),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  textInput(
                    ns("BARC1_Origin_M3"),
                    label = "Origin of Bleeding	",
                    value = ifelse(is.null(hold$BARC1_Origin_M3), "", hold$BARC1_Origin_M3)
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_AT_M3"),
                    "After Treatment",
                    choices = c("Recovered", "Death"),
                    selected = ifelse(is.null(hold$BARC1_AT_M3), character(0), hold$BARC1_AT_M3),
                    inline = TRUE
                  )
                )
              )
            ),
            conditionalPanel(
              "input.Bleed_count_M3 >= 2",
              ns = ns,
              fluidRow(
                column(
                  width = 2,
                  dateInput(
                    ns("Bleed2_date_M3"),
                    "Date",
                    value = lubridate::as_date(hold$Bleed2_date_M3),
                    language = "ko"
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_Type_M3"),
                    "BARC Type",
                    choices = c("BARC 2", "BARC 3", "BARC 5"),
                    selected = ifelse(is.null(hold$BARC2_Type_M3), character(0), hold$BARC2_Type_M3),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_ST_M3"),
                    "Spontaneous or Traumatic",
                    choices = c("Spontaneous", "Traumatic"),
                    selected = ifelse(is.null(hold$BARC2_ST_M3), character(0), hold$BARC2_ST_M3),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_HT_M3"),
                    "Requiring hospitalization or transfusion",
                    choices = c("Yes", "No"),
                    selected = ifelse(is.null(hold$BARC2_HT_M3), character(0), hold$BARC2_HT_M3),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  textInput(
                    ns("BARC2_Origin_M3"),
                    label = "Origin of Bleeding	",
                    value = ifelse(is.null(hold$BARC2_Origin_M3), "", hold$BARC2_Origin_M3)
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_AT_M3"),
                    "After Treatment",
                    choices = c("Recovered", "Death"),
                    selected = ifelse(is.null(hold$BARC2_AT_M3), character(0), hold$BARC2_AT_M3),
                    inline = TRUE
                  )
                )
              )
            )
          ),
          
          # Readmission
          conditionalPanel(
            'input.Event_Details_M3.includes("Readmission")',
            ns = ns,
            tags$div(
              HTML(
                paste0(
                  '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                  "Readmission",
                  tags$div(
                    modalButton("", icon("times")),
                    style = "float:right;"
                  ),
                  actionButton(
                    ns("submit12"),
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
                  ns("Readmission_Date_M3"),
                  label = "Date",
                  value = lubridate::as_date(hold$Readmission_Date_M3),
                  language = "ko"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("Readmission_Type_M3"),
                  label = "Type",
                  choices = c("Heart failure", "cardiac problem(not heart failure)", "Other"),
                  selected = ifelse(is.null(hold$Readmission_Type_M3), character(0), hold$Readmission_Type_M3),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                conditionalPanel(
                  "input.Readmission_Type_M3 == 'Other'", # if Other : Show text box.
                  ns = ns,
                  textAreaInput(
                    ns("Readmission_Other_M3"),
                    "Comment",
                    width = "400px",
                    height = "100px",
                    value = ifelse(is.null(hold$Readmission_Other_M3), "", hold$Readmission_Other_M3)
                  )
                )
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
    updateTextInput(session, "SBP_M3", value = "ND")
    updateTextInput(session, "DBP_M3", value = "ND")
    updateTextInput(session, "HRT_M3", value = "ND")
  })
  
  observeEvent(input$wbcND, {
    updateTextInput(session, "WBC_M3", value = "ND")
  })
  
  observeEvent(input$tcND, {
    updateTextInput(session, "TC_M3", value = "ND")
  })
  
  observeEvent(input$tgND, {
    updateTextInput(session, "TG_M3", value = "ND")
  })
  
  observeEvent(input$hbND, {
    updateTextInput(session, "Hb_M3", value = "ND")
  })
  
  observeEvent(input$plND, {
    updateTextInput(session, "Platelet_M3", value = "ND")
  })
  
  observeEvent(input$hdlND, {
    updateTextInput(session, "HDL_M3", value = "ND")
  })
  
  observeEvent(input$bunND, {
    updateTextInput(session, "BUN_M3", value = "ND")
  })
  
  observeEvent(input$ldlND, {
    updateTextInput(session, "LDL_M3", value = "ND")
  })
  
  observeEvent(input$crND, {
    updateTextInput(session, "Cr_M3", value = "ND")
  })
  
  observeEvent(input$fgND, {
    updateTextInput(session, "FG_M3", value = "ND")
  })
  
  observeEvent(input$mdND, {
    updateTextInput(session, "MDRD_M3", value = "ND")
  })
  
  observeEvent(input$hb1ND, {
    updateTextInput(session, "HbA1C_M3", value = "ND")
  })
  
  observeEvent(input$astND, {
    updateTextInput(session, "AST_M3", value = "ND")
  })
  
  observeEvent(input$hsND, {
    updateTextInput(session, "hs_CRP_M3", value = "ND")
  })
  
  observeEvent(input$altND, {
    updateTextInput(session, "ALT_M3", value = "ND")
  })
  
  observeEvent(input$ntpND, {
    updateTextInput(session, "NT_Pro_BNP_M3", value = "ND")
  })
  
  observeEvent(input$CYfA, {
    updateRadioButtons(session, "Aspirin_M3", selected = "Yes")
    updateRadioButtons(session, "Trimetazidine_M3", selected = "Yes")
    updateRadioButtons(session, "Clopidogrel_M3", selected = "Yes")
    updateRadioButtons(session, "Nitrate_M3", selected = "Yes")
    updateRadioButtons(session, "Prasugrel_M3", selected = "Yes")
    updateRadioButtons(session, "Statin_M3", selected = "Yes")
    updateRadioButtons(session, "Ticagrelor_M3", selected = "Yes")
    updateRadioButtons(session, "BB_M3", selected = "Yes")
    updateRadioButtons(session, "ACE_M3", selected = "Yes")
    updateRadioButtons(session, "DPP4_M3", selected = "Yes")
    updateRadioButtons(session, "Wafarin_M3", selected = "Yes")
    updateRadioButtons(session, "NOAC_M3", selected = "Yes")
    updateRadioButtons(session, "Nicorandil_M3", selected = "Yes")
    updateRadioButtons(session, "Calcium_M3", selected = "Yes")
    updateRadioButtons(session, "Metformin_M3", selected = "Yes")
    updateRadioButtons(session, "Sulf_M3", selected = "Yes")
    updateRadioButtons(session, "Thia_M3", selected = "Yes")
    updateRadioButtons(session, "GLP_M3", selected = "Yes")
    updateRadioButtons(session, "Alpha_M3", selected = "Yes")
    updateRadioButtons(session, "Insulin_M3", selected = "Yes")
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
        # "Readmission_M3" = ifelse(is.null(input$Readmission_M3), "", input$Readmission_M3),
        # "Readmission_Date_M3" = ifelse(is.null(input$Readmission_Date_M3), "", input$Readmission_Date_M3),
        # "Readmission_reason_M3" = ifelse(is.null(input$Readmission_reason_M3), "", input$Readmission_reason_M3),
        # "Readmission_reason_text_M3" = ifelse(is.null(input$Readmission_reason_text_M3), "", input$Readmission_reason_text_M3),
        "ECG_Rhythm_M3" = ifelse(is.null(input$ECG_Rhythm_M3), "", input$ECG_Rhythm_M3),
        "ECG_Rhythm_Other_M3" = ifelse(is.null(input$ECG_Rhythm_Other_M3), "", input$ECG_Rhythm_Other_M3),
        "Stress_ECG_M3" = ifelse(is.null(input$Stress_ECG_M3), "", input$Stress_ECG_M3),
        "Stress_ECG_Date_M3" = ifelse(is.null(input$Stress_ECG_Date_M3), "", as.character(input$Stress_ECG_Date_M3)),
        "Stress_ECG_Detail_M3" = ifelse(is.null(input$Stress_ECG_Detail_M3), "", input$Stress_ECG_Detail_M3),
        "Lab_Date_M3" = ifelse(is.null(input$Lab_Date_M3), "", lubridate::as_date(input$Lab_Date_M3)),
        "WBC_M3" = ifelse(is.null(input$WBC_M3), "", input$WBC_M3),
        "TC_M3" = ifelse(is.null(input$TC_M3), "", input$TC_M3),
        "Hb_M3" = ifelse(is.null(input$Hb_M3), "", input$Hb_M3),
        "TG_M3" = ifelse(is.null(input$TG_M3), "", input$TG_M3),
        "Platelet_M3" = ifelse(is.null(input$Platelet_M3), "", input$Platelet_M3),
        "HDL_M3" = ifelse(is.null(input$HDL_M3), "", input$HDL_M3),
        "BUN_M3" = ifelse(is.null(input$BUN_M3), "", input$BUN_M3),
        "LDL_M3" = ifelse(is.null(input$LDL_M3), "", input$LDL_M3),
        "Cr_M3" = ifelse(is.null(input$Cr_M3), "", input$Cr_M3),
        "FG_M3" = ifelse(is.null(input$FG_M3), "", input$FG_M3),
        "MDRD_M3" = ifelse(is.null(input$MDRD_M3), "", input$MDRD_M3),
        "HbA1C_M3" = ifelse(is.null(input$HbA1C_M3), "", input$HbA1C_M3),
        "AST_M3" = ifelse(is.null(input$AST_M3), "", input$AST_M3),
        "hs_CRP_M3" = ifelse(is.null(input$hs_CRP_M3), "", input$hs_CRP_M3),
        "ALT_M3" = ifelse(is.null(input$ALT_M3), "", input$ALT_M3),
        "NT_Pro_BNP_M3" = ifelse(is.null(input$NT_Pro_BNP_M3), "", input$NT_Pro_BNP_M3),
        "Aspirin_M3" = ifelse(is.null(input$Aspirin_M3), "", input$Aspirin_M3),
        "Trimetazidine_M3" = ifelse(is.null(input$Trimetazidine_M3), "", input$Trimetazidine_M3),
        "Clopidogrel_M3" = ifelse(is.null(input$Clopidogrel_M3), "", input$Clopidogrel_M3),
        "Nitrate_M3" = ifelse(is.null(input$Nitrate_M3), "", input$Nitrate_M3),
        "Nicorandil_M3" = ifelse(is.null(input$Nicorandil_M3), "", input$Nicorandil_M3),
        "Prasugrel_M3" = ifelse(is.null(input$Prasugrel_M3), "", input$Prasugrel_M3),
        "Statin_M3" = ifelse(is.null(input$Statin_M3), "", input$Statin_M3),
        "Statin_name_M3" = ifelse(is.null(input$Statin_name_M3), "", input$Statin_name_M3),
        "Statin_dose_M3" = ifelse(is.null(input$Statin_dose_M3), "", input$Statin_dose_M3),
        "Ticagrelor_M3" = ifelse(is.null(input$Ticagrelor_M3), "", input$Ticagrelor_M3),
        "Wafarin_M3" = ifelse(is.null(input$Wafarin_M3), "", input$Wafarin_M3),
        "NOAC_M3" = ifelse(is.null(input$NOAC_M3), "", input$NOAC_M3),
        "NOAC_name_M3" = ifelse(is.null(input$NOAC_name_M3), "", input$NOAC_name_M3),
        "NOAC_dose_M3" = ifelse(is.null(input$NOAC_dose_M3), "", input$NOAC_dose_M3),
        "BB_M3" = ifelse(is.null(input$BB_M3), "", input$BB_M3),
        "BB_name_M3" = ifelse(is.null(input$BB_name_M3), "", input$BB_name_M3),
        "BB_dose_M3" = ifelse(is.null(input$BB_dose_M3), "", input$BB_dose_M3),
        "ACE_M3" = ifelse(is.null(input$ACE_M3), "", input$ACE_M3),
        "ACE_name_M3" = ifelse(is.null(input$ACE_name_M3), "", input$ACE_name_M3),
        "ACE_dose_M3" = ifelse(is.null(input$ACE_dose_M3), "", input$ACE_dose_M3),
        "DPP4_M3" = ifelse(is.null(input$DPP4_M3), "", input$DPP4_M3),
        "DPP4_name_M3" = ifelse(is.null(input$DPP4_name_M3), "", input$DPP4_name_M3),
        "DPP4_dose_M3" = ifelse(is.null(input$DPP4_dose_M3), "", input$DPP4_dose_M3),
        "Calcium_M3" = ifelse(is.null(input$Calcium_M3), "", input$Calcium_M3),
        "Calcium_name_M3" = ifelse(is.null(input$Calcium_name_M3), "", input$Calcium_name_M3),
        "Calcium_dose_M3" = ifelse(is.null(input$Calcium_dose_M3), "", input$Calcium_dose_M3),
        "Metformin_M3" = ifelse(is.null(input$Metformin_M3), "", input$Metformin_M3),
        "Metformin_dose_M3" = ifelse(is.null(input$Metformin_dose_M3), "", input$Metformin_dose_M3),
        "Sulf_M3" = ifelse(is.null(input$Sulf_M3), "", input$Sulf_M3),
        "Sulf_name_M3" = ifelse(is.null(input$Sulf_name_M3), "", input$Sulf_name_M3),
        "Sulf_dose_M3" = ifelse(is.null(input$Sulf_dose_M3), "", input$Sulf_dose_M3),
        "Thia_M3" = ifelse(is.null(input$Thia_M3), "", input$Thia_M3),
        "Thia_name_M3" = ifelse(is.null(input$Thia_name_M3), "", input$Thia_name_M3),
        "Thia_dose_M3" = ifelse(is.null(input$Thia_dose_M3), "", input$Thia_dose_M3),
        "GLP_M3" = ifelse(is.null(input$GLP_M3), "", input$GLP_M3),
        "GLP_name_M3" = ifelse(is.null(input$GLP_name_M3), "", input$GLP_name_M3),
        "GLP_dose_M3" = ifelse(is.null(input$GLP_dose_M3), "", input$GLP_dose_M3),
        "Alpha_M3" = ifelse(is.null(input$Alpha_M3), "", input$Alpha_M3),
        "Alpha_name_M3" = ifelse(is.null(input$Alpha_name_M3), "", input$Alpha_name_M3),
        "Alpha_dose_M3" = ifelse(is.null(input$Alpha_dose_M3), "", input$Alpha_dose_M3),
        "Insulin_M3" = ifelse(is.null(input$Insulin_M3), "", input$Insulin_M3),
        
        "Event_Details_M3" = ifelse(is.null(input$Event_Details_M3),"", paste0(input$Event_Details_M3, collapse = ',')),
        
        "Death_date_M3" = ifelse(is.null(input$Death_date_M3), "", as.character(input$Death_date_M3)),
        "Death_reason_M3" = ifelse(is.null(input$Death_reason_M3), "", input$Death_reason_M3),
        "MI_date_M3" = ifelse(is.null(input$MI_date_M3), "", as.character(input$MI_date_M3)),
        "MI_Segment_M3" = ifelse(is.null(input$MI_Segment_M3), "", input$MI_Segment_M3),
        "MI_Type_M3" = ifelse(is.null(input$MI_Type_M3), "", input$MI_Type_M3),
        "MI_Stent_M3" = ifelse(is.null(input$MI_Stent_M3), "", input$MI_Stent_M3),
        "MI_Lesion_M3" = ifelse(is.null(input$MI_Lesion_M3), "", input$MI_Lesion_M3),
        "MI_Vessel_M3" = ifelse(is.null(input$MI_Vessel_M3), "", input$MI_Vessel_M3),
        "MI_Treatment_M3" = ifelse(is.null(input$MI_Treatment_M3),"", paste0(input$MI_Treatment_M3, collapse = ',')),
        "MI_After_M3" = ifelse(is.null(input$MI_After_M3), "", input$MI_After_M3),
        "Rev_date_M3" = ifelse(is.null(input$Rev_date_M3), "", as.character(input$Rev_date_M3)),
        "Rev_Segment_M3" = ifelse(is.null(input$Rev_Segment_M3), "", input$Rev_Segment_M3),
        "Rev_Treatment_M3" = ifelse(is.null(input$Rev_Treatment_M3),"", paste0(input$Rev_Treatment_M3, collapse = ',')),
        "Rev_Lesion_M3" = ifelse(is.null(input$Rev_Lesion_M3), "", input$Rev_Lesion_M3),
        "Rev_Vessel_M3" = ifelse(is.null(input$Rev_Vessel_M3), "", input$Rev_Vessel_M3),
        "Rev_PCI_M3" = ifelse(is.null(input$Rev_PCI_M3), "", input$Rev_PCI_M3),
        "Stent_date_M3" = ifelse(is.null(input$Stent_date_M3), "", as.character(input$Stent_date_M3)),
        "Stent_Segment_M3" = ifelse(is.null(input$Stent_Segment_M3), "", input$Stent_Segment_M3),
        "Stent_Type_M3" = ifelse(is.null(input$Stent_Type_M3), "", input$Stent_Type_M3),
        "Stent_arc_M3" = ifelse(is.null(input$Stent_arc_M3), "", input$Stent_arc_M3),
        "Clinical_feature_M3" = ifelse(is.null(input$Clinical_feature_M3),"", paste0(input$Clinical_feature_M3, collapse = ',')),
        "Clinical_feature_other_M3" = ifelse(is.null(input$Clinical_feature_other_M3), "", input$Clinical_feature_other_M3),
        "CVA_date_M3" = ifelse(is.null(input$CVA_date_M3), "", as.character(input$CVA_date_M3)),
        "CVA_Type_M3" = ifelse(is.null(input$CVA_Type_M3), "", input$CVA_Type_M3),
        "Bleed_count_M3" = ifelse(is.null(input$Bleed_count_M3), "", input$Bleed_count_M3),
        "Bleed1_date_M3" = ifelse(is.null(input$Bleed1_date_M3), "", lubridate::as_date(input$Bleed1_date_M3)),
        "BARC1_Type_M3" = ifelse(is.null(input$BARC1_Type_M3), "", input$BARC1_Type_M3),
        "BARC1_ST_M3" = ifelse(is.null(input$BARC1_ST_M3), "", input$BARC1_ST_M3),
        "BARC1_HT_M3" = ifelse(is.null(input$BARC1_HT_M3), "", input$BARC1_HT_M3),
        "BARC1_Origin_M3" = ifelse(is.null(input$BARC1_Origin_M3), "", input$BARC1_Origin_M3),
        "BARC1_AT_M3" = ifelse(is.null(input$BARC1_AT_M3), "", input$BARC1_AT_M3),
        "Bleed2_date_M3" = ifelse(is.null(input$Bleed2_date_M3), "", lubridate::as_date(input$Bleed2_date_M3)),
        "BARC2_Type_M3" = ifelse(is.null(input$BARC2_Type_M3), "", input$BARC2_Type_M3),
        "BARC2_ST_M3" = ifelse(is.null(input$BARC2_ST_M3), "", input$BARC2_ST_M3),
        "BARC2_HT_M3" = ifelse(is.null(input$BARC2_HT_M3), "", input$BARC2_HT_M3),
        "BARC2_Origin_M3" = ifelse(is.null(input$BARC2_Origin_M3), "", input$BARC2_Origin_M3),
        "BARC2_AT_M3" = ifelse(is.null(input$BARC2_AT_M3), "", input$BARC2_AT_M3),
        "Readmission_Date_M3" = ifelse(is.null(input$Readmission_Date_M3), "", lubridate::as_date(input$Readmission_Date_M3)),
        "Readmission_Type_M3" = ifelse(is.null(input$Readmission_Type_M3), "", input$Readmission_Type_M3),
        "Readmission_Other_M3" = ifelse(is.null(input$Readmission_Other_M3), "", input$Readmission_Other_M3),
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
  
  callEdit <- reactive({
    list(
      input$submit,
      input$submit1,
      input$submit2,
      input$submit3,
      input$submit4,
      input$submit5,
      input$submit6,
      input$submit7,
      input$submit8,
      input$submit9,
      input$submit10,
      input$submit11
    )
  })
  
  # Reference : https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
  
  validate_edit <- eventReactive(
    eventExpr = callEdit(),
    valueExpr = {
      if (input$submit == 0 && input$submit1 == 0 && input$submit2 == 0 &&
          input$submit3 == 0 && input$submit4 == 0 && input$submit5 == 0 &&
          input$submit6 == 0 && input$submit7 == 0 && input$submit8 == 0 && 
          input$submit9 ==0 && input$submit10 ==0 && input$submit11 ==0) {
        return()
      }
      dat <- edit_car_dat()
      # Logic to validate inputs...
      dat
    }, ignoreInit = TRUE
  )
  
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
