#' mf Add & Edit Module
#'
#' Module to add & edit final month in the database file
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
              ns("FU_Mf"),
              "F/U",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold$FU_Mf), character(0), hold$FU_Mf),
              inline = T
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.FU_Mf == 'Yes'",
              ns = ns,
              dateInput(
                ns("Visit_Date_Mf"),
                "Visit date",
                value = lubridate::as_date(hold$Visit_Date_Mf),
                language = "ko"
              )
            ),
            conditionalPanel(
              "input.FU_Mf == 'No'",
              ns = ns,
              dateInput(
                ns("LastFU_Mf"),
                "Date of Last F/U",
                value = lubridate::as_date(hold$LastFU_Mf),
                language = "ko"
              )
              ## Implement Duration
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.FU_Mf == 'Yes'",
              ns = ns,
              radioButtons(
                ns("Visit_Mf"),
                "Visit by",
                choices = c("Clinic", "Phone"),
                selected = ifelse(is.null(hold$Visit_Mf), character(0), hold$Visit_Mf),
                inline = T
              )
            ),
            conditionalPanel(
              "input.FU_Mf == 'No",
              ns = ns,
              radioButtons(
                ns("Reason_Mf"),
                "Reason",
                choices = c("Patient Died", "Patient Lost to F/U", "Other"),
                selected = ifelse(is.null(hold$Reason_Mf), character(0), hold$Reason_Mf)
              )
            )
          ),
          column(
            width = 3,
            conditionalPanel(
              "input.Reason_Mf == 'Other'", # if Other : Show text box.
              ns = ns,
              textAreaInput(
                ns("Other_Mf"),
                "Other",
                width = "400px",
                value = ifelse(is.null(hold$Other_Mf), '', hold$Other_Mf),
                height = "100px"
              )
            )
          )
        ),
        
        conditionalPanel(
          "input.FU_Mf == 'Yes'",
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
                ns("SBP_Mf"), # Systolic BP
                "Systolic BP",
                value = ifelse(is.null(hold$SBP_Mf), "", hold$SBP_Mf)
              )
            ),
            column(
              width = 4,
              textInput(
                ns("DBP_Mf"), # Diastolic BP
                "Diastolic BP",
                value = ifelse(is.null(hold$DBP_Mf), "", hold$DBP_Mf)
              )
            ),
            column(
              width = 4,
              textInput(
                ns("HRT_Mf"), # Heart Ratio
                "Heart Rate",
                value = ifelse(is.null(hold$HRT_Mf), "", hold$HRT_Mf)
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
                ns("Cardiac_Status_Mf"),
                "Cardiac Status",
                choices = c("Unknown", "Asymptomatic", "Angina", "Other"),
                selected = ifelse(is.null(hold$Cardiac_Status_Mf),character(0), hold$Cardiac_Status_Mf),
                inline = TRUE
              )
            ),
            column(
              width = 6,
              conditionalPanel(
                "input.Cardiac_Status_Mf == 'Angina'", # if Angina, show CCS I, II, III, IV
                ns = ns,
                radioButtons(
                  ns("CCS_Mf"),
                  "CCS",
                  choices = c("I", "II", "III", "IV"),
                  selected = ifelse(is.null(hold$CCS_Mf),character(0), hold$CCS_Mf),
                  inline = T
                )
              ),
              conditionalPanel(
                "input.Cardiac_Status_Mf == 'Other'", # if Other, show textInput
                ns = ns,
                textAreaInput(
                  ns("Other_Cardiac_Status_Mf"),
                  "Other",
                  value = ifelse(is.null(hold$Other_Cardiac_Status_Mf), '', hold$Other_Cardiac_Status_Mf),
                  height = "3em"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("ECG_Rhythm_Mf"),
                "ECG Rhythm",
                choices = c("Sinus Rhytum", "Atrial Fibrillation", "Others", "Not Done"),
                selected = ifelse(is.null(hold$ECG_Rhythm_Mf), character(0), hold$ECG_Rhythm_Mf),
                inline = T
              )
            ),
            column(
              width = 6,
              conditionalPanel(
                "input.ECG_Rhythm_Mf == 'Others'",
                ns = ns,
                textAreaInput(
                  ns("ECG_Rhythm_Other_Mf"),
                  "Others",
                  value = ifelse(is.null(hold$ECG_Rhythm_Other_Mf), "", hold$ECG_Rhythm_Other_Mf),
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
                ns("Stress_ECG_Mf"),
                "Stress ECG (TMT)",
                choices = c("Yes", "No"),
                selected = ifelse(is.null(hold$Stress_ECG_Mf), character(0), hold$Stress_ECG_Mf),
                inline = T
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                "input.Stress_ECG_Mf == 'Yes'",
                ns = ns,
                dateInput(
                  ns("Stress_ECG_Date_Mf"),
                  "Date",
                  value = lubridate::as_date(hold$Stress_ECG_Date_Mf),
                  language = "ko"
                )
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                "input.Stress_ECG_Mf == 'Yes'",
                ns = ns,
                radioButtons(
                  ns("Stress_ECG_Detail_Mf"),
                  "Detail",
                  choices = c("Positive", "Negative", "Equivocal", "Incomplete (Inadequate)"),
                  selected = ifelse(is.null(hold$Stress_ECG_Detail_Mf), character(0), hold$Stress_ECG_Detail_Mf),
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
                ns("Lab_Date_Mf"),
                "Lab Date",
                value = lubridate::as_date(hold$Lab_Date_Mf),
                language = "ko"
              ),
            ),
            column(
              width = 3,
              textInput(
                ns("WBC_Mf"),
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
                value = ifelse(is.null(hold), "", hold$WBC_Mf)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("TC_Mf"),
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
                value = ifelse(is.null(hold$TC_Mf), "", hold$TC_Mf)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Hb_Mf"),
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
                value = ifelse(is.null(hold$Hb_Mf), "", hold$Hb_Mf)
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("TG_Mf"),
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
                value = ifelse(is.null(hold$TG_Mf), "", hold$TG_Mf)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Platelet_Mf"),
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
                value = ifelse(is.null(hold$Platelet_Mf), "", hold$Platelet_Mf)
              )
            ),
            column(
              width = 3,
              textInput(
                ns("HDL_Mf"),
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
                value = ifelse(is.null(hold$HDL_Mf), "", hold$HDL_Mf),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("BUN_Mf"),
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
                value = ifelse(is.null(hold$BUN_Mf), "", hold$BUN_Mf),
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("LDL_Mf"),
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
                value = ifelse(is.null(hold$LDL_Mf), "", hold$LDL_Mf),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Cr_Mf"),
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
                value = ifelse(is.null(hold$Cr_Mf), "", hold$Cr_Mf),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("FG_Mf"),
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
                value = ifelse(is.null(hold$FG_Mf), "", hold$FG_Mf),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("MDRD_Mf"),
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
                value = ifelse(is.null(hold$MDRD_Mf), "", hold$MDRD_Mf),
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("HbA1C_Mf"),
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
                value = ifelse(is.null(hold$HbA1C_Mf), "", hold$HbA1C_Mf),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("AST_Mf"),
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
                value = ifelse(is.null(hold$AST_Mf), "", hold$AST_Mf),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("hs_CRP_Mf"),
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
                value = ifelse(is.null(hold$hs_CRP_Mf), "", hold$hs_CRP_Mf),
              )
            ),
            column(
              width = 3,
              textInput(
                ns("ALT_Mf"),
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
                value = ifelse(is.null(hold$ALT_Mf), "", hold$ALT_Mf)
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("NT_Pro_BNP_Mf"),
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
                value = ifelse(is.null(hold$NT_Pro_BNP_Mf), "", hold$NT_Pro_BNP_Mf),
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
                ns("Aspirin_Mf"),
                label = "Aspirin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Aspirin_Mf) ,character(0),hold$Aspirin_Mf),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Trimetazidine_Mf"),
                label = "Trimetazidine (Vastinan)",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Trimetazidine_Mf), character(0),hold$Trimetazidine_Mf),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Clopidogrel_Mf"),
                label = "Clopidogrel",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Clopidogrel_Mf), character(0),hold$Clopidogrel_Mf),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Nitrate_Mf"),
                label = "Nitrate",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Nitrate_Mf), character(0),hold$Nitrate_Mf),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("Prasugrel_Mf"),
                label = "Prasugrel",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Prasugrel_Mf), character(0),hold$Prasugrel_Mf),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Ticagrelor_Mf"),
                label = "Ticagrelor",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Ticagrelor_Mf),character(0), hold$Ticagrelor_Mf),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Wafarin_Mf"),
                label = "Wafarin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Wafarin_Mf),character(0), hold$Wafarin_Mf),
                inline = T
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Nicorandil_Mf"),
                label = "Nicorandil",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Nicorandil_Mf),character(0), hold$Nicorandil_Mf),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("NOAC_Mf"),
                label = "NOAC",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$NOAC_Mf),character(0), hold$NOAC_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.NOAC_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("NOAC_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$NOAC_name_Mf), '', hold$NOAC_name_Mf),
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("NOAC_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$NOAC_dose_Mf), character(0), hold$NOAC_dose_Mf),
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
                ns("Statin_Mf"),
                label = "Statin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Statin_Mf), character(0),hold$Statin_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.Statin_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Statin_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$Statin_name_Mf), '',hold$Statin_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Statin_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Statin_dose_Mf),character(0), hold$Statin_dose_Mf),
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
                ns("BB_Mf"),
                label = "Beta Blocker",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$BB_Mf), character(0), hold$BB_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.BB_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("BB_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$BB_name_Mf), '',hold$BB_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("BB_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$BB_dose_Mf), character(0), hold$BB_dose_Mf),
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
                ns("Insulin_Mf"),
                label = "Insulin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Insulin_Mf),character(0), hold$Insulin_Mf),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("ACE_Mf"),
                label = "ACE Inhibitor or ARB",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$ACE_Mf), character(0), hold$ACE_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.ACE_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("ACE_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$ACE_name_Mf), '',hold$ACE_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("ACE_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$ACE_dose_Mf), character(0), hold$ACE_dose_Mf),
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
                ns("DPP4_Mf"),
                label = "DPP4 Inhibitor",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$DPP4_Mf), character(0), hold$DPP4_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.DPP4_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("DPP4_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$DPP4_name_Mf), '',hold$DPP4_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("DPP4_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$DPP4_dose_Mf), character(0), hold$DPP4_dose_Mf),
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
                ns("Calcium_Mf"),
                label = "Calcium channel blocker",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Calcium_Mf), character(0), hold$Calcium_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.Calcium_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Calcium_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$Calcium_name_Mf), '',hold$Calcium_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Calcium_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Calcium_dose_Mf), character(0), hold$Calcium_dose_Mf),
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
                ns("Metformin_Mf"),
                label = "Metformin",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Metformin_Mf),character(0), hold$Metformin_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.Metformin_Mf == 'Yes'",
                ns = ns,
                numericInput(
                  ns("Metformin_dose_Mf"),
                  label = "Dose",
                  value = ifelse(is.null(hold$Metformin_dose_Mf),character(0), hold$Metformin_dose_Mf),
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
                ns("Sulf_Mf"),
                label = "Sulfonylurea",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Sulf_Mf),character(0), hold$Sulf_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.Sulf_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Sulf_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$Sulf_name_Mf), '', hold$Sulf_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Sulf_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Sulf_dose_Mf), character(0), hold$Sulf_dose_Mf),
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
                ns("Thia_Mf"),
                label = "Thiazolidinedione",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Thia_Mf),character(0), hold$Thia_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.Thia_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Thia_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$Thia_name_Mf), '', hold$Thia_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Thia_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Thia_dose_Mf),character(0), hold$Thia_dose_Mf),
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
                ns("GLP_Mf"),
                label = "GLP-1 Agonist",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$GLP_Mf),character(0), hold$GLP_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.GLP_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("GLP_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$GLP_name_Mf), '', hold$GLP_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("GLP_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$GLP_dose_Mf),character(0),hold$GLP_dose_Mf),
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
                ns("Alpha_Mf"),
                label = "Alpha-glucosidase inhibitor",
                choices = c("Yes", "No", "Unknown"),
                selected = ifelse(is.null(hold$Alpha_Mf),character(0),hold$Alpha_Mf),
                inline = T
              ),
              conditionalPanel(
                "input.Alpha_Mf == 'Yes'",
                ns = ns,
                fluidRow(
                  column(
                    width = 6,
                    textInput(
                      ns("Alpha_name_Mf"),
                      label = "Name",
                      value = ifelse(is.null(hold$Alpha_name_Mf), '',hold$Alpha_name_Mf)
                    )
                  ),
                  column(
                    width = 6,
                    numericInput(
                      ns("Alpha_dose_Mf"),
                      label = "Dose",
                      value = ifelse(is.null(hold$Alpha_dose_Mf),character(0), hold$Alpha_dose_Mf),
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
                ns("Event_Mf"),
                "Clinical Events",
                choices = c("Yes", "No"),
                selected = ifelse(is.null(hold), character(0), hold$Event_Mf),
                inline = TRUE
              )
            ),
            column(
              width = 10,
              conditionalPanel(
                "input.Event_Mf == 'Yes'",
                ns = ns,
                checkboxGroupInput(
                  ns("Event_Details_Mf"),
                  "Detail",
                  choices = c("Death", "MI", "Repeat Revascularization", "Stent Thrombosis", "CVA", "Bleeding", "Readmission"),
                  selected = strsplit(ifelse(is.null(hold$Event_Details_Mf), character(0), hold$Event_Details_Mf), ',')[[1]],
                  inline = TRUE
                )
              )
            )
          ),
          
          # Death
          conditionalPanel(
            'input.Event_Details_Mf.includes("Death")',
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
                  ns("Death_date_Mf"),
                  label = "Date",
                  value = lubridate::as_date(hold$Death_date_Mf),
                  language = "ko"
                )
              ),
              column(
                width = 9,
                radioButtons(
                  ns("Death_reason_Mf"),
                  label = "",
                  choices = c("Cardiac Death", "Non-Cardiovascular Death", "Unknown Origin Death"),
                  inline = TRUE,
                  selected = ifelse(is.null(hold$Death_reason_Mf), character(0), hold$Death_reason_Mf)
                )
              )
            )
          ),
          conditionalPanel(
            'input.Event_Details_Mf.includes("MI")',
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
                  ns("MI_date_Mf"),
                  label = "Date",
                  value = lubridate::as_date(hold$MI_date_Mf),
                  language = "ko"
                )
              ),
              column(
                width = 3,
                numericInput(
                  ns("MI_Segment_Mf"),
                  label = "Segment",
                  value = ifelse(is.null(hold$MI_Segment_Mf), character(0), hold$MI_Segment_Mf),
                  min = 0, max = 120,
                  step = 1
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_Type_Mf"),
                  label = "Type", 
                  choices = c("STEMI", "NSTEMI"), 
                  selected = ifelse(is.null(hold$MI_Type_Mf), character(0), hold$MI_Type_Mf),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_Stent_Mf"),
                  label = "Related with Stent Thrombosis",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$MI_Stent_Mf), character(0), hold$MI_Stent_Mf),
                  inline = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                radioButtons(
                  ns("MI_Lesion_Mf"),
                  label = "Related with Target Lesion",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$MI_Lesion_Mf), character(0), hold$MI_Lesion_Mf),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_Vessel_Mf"),
                  label = "Related with Target Vessel",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold), character(0), hold$MI_Vessel_Mf),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                checkboxGroupInput(
                  ns("MI_Treatment_Mf"),
                  label = "Type of Treatment",
                  choices = c("Medication Only", "Thrombolysis", "only Ballooning", "Stenting", "Bypass Surgery"),
                  selected = strsplit(ifelse(is.null(hold$MI_Treatment_Mf), character(0), hold$MI_Treatment_Mf), ',')[[1]]
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("MI_After_Mf"),
                  label = "After Treatment",
                  selected = ifelse(is.null(hold$MI_After_Mf), character(0), hold$MI_After_Mf),
                  choices = c("Recovered", "Death", "Unknown")
                )
              )
            )
          ),
          
          # Revascularization
          conditionalPanel(
            'input.Event_Details_Mf.includes("Repeat Revascularization")',
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
                  ns("Rev_date_Mf"),
                  label = "Date",
                  language = "ko",
                  value = lubridate::as_date(hold$Rev_date_Mf)
                )
              ),
              column(
                width = 3,
                numericInput(
                  ns("Rev_Segment_Mf"),
                  label = "Segment",
                  value = ifelse(is.null(hold$Rev_Segment_Mf), character(0), hold$Rev_Segment_Mf),
                  min = 0, max = 120,
                  step = 1
                )
              ),
              column(
                width = 3,
                checkboxGroupInput(
                  ns("Rev_Treatment_Mf"),
                  label = "Type of Treatment",
                  choices = c("only Ballooning", "Stenting", "Bypass Surgery"),
                  selected = strsplit(ifelse(is.null(hold$Rev_Treatment_Mf), character(0), hold$Rev_Treatment_Mf), ',')[[1]]
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("Rev_Lesion_Mf"),
                  label = "Related with Target Lesion",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$Rev_Lesion_Mf), character(0), hold$Rev_Lesion_Mf),
                  inline = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                radioButtons(
                  ns("Rev_Vessel_Mf"),
                  label = "Related with Target Vessel",
                  choices = c("Unknown", "No", "Yes"),
                  selected = ifelse(is.null(hold$Rev_Vessel_Mf), character(0), hold$Rev_Vessel_Mf),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("Rev_PCI_Mf"),
                  label = "Another Vessel PCI",
                  choices = c("Yes", "No"),
                  selected = ifelse(is.null(hold$Rev_PCI_Mf), character(0), hold$Rev_PCI_Mf),
                  inline = TRUE
                )
              )
            )
          ),
          conditionalPanel(
            'input.Event_Details_Mf.includes("Stent Thrombosis")',
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
                  ns("Stent_date_Mf"),
                  label = "Date",
                  value = lubridate::as_date(hold$Stent_date_Mf),
                  language = "ko"
                )
              ),
              column(
                width = 3,
                textInput(
                  ns("Stent_Segment_Mf"),
                  label = "Segment",
                  value = ifelse(is.null(hold$Stent_Segment_Mf), "", hold$Stent_Segment_Mf)
                )
              ),
              column(
                width = 6,
                radioButtons(
                  ns("Stent_arc_Mf"),
                  label = "ARC",
                  choices = c("Definite/Confirmed", "Probable", "Possible"),
                  selected = ifelse(is.null(hold$Stent_arc_Mf), character(0), hold$Stent_arc_Mf),
                  inline = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                radioButtons(
                  ns("Stent_Type_Mf"),
                  label = "Type",
                  choices = c("Acute (< 1d)", "Subacute (1-30d)", "Late(> 1m)", "Very Late(> 1y)"),
                  selected = ifelse(is.null(hold$Stent_Type_Mf), character(0), hold$Stent_Type_Mf)
                )
              ),
              column(
                width = 4,
                checkboxGroupInput(
                  inputId = ns("Clinical_feature_Mf"),
                  label = "Clinical Features",
                  choices = c("Sudden Death", "STEMI", "NSTEMI", "Unstable Angina", "Stable Angina", "Other"),
                  selected = strsplit(ifelse(is.null(hold$Clinical_feature_Mf), character(0), hold$Clinical_feature_Mf), ',')[[1]],
                )
              ),
              column(
                width = 4,
                conditionalPanel(
                  'input.Clinical_feature_Mf.includes("Other")',
                  ns = ns,
                  textAreaInput(
                    ns("Clinical_feature_other_Mf"),
                    "Other",
                    width = "400px",
                    value = ifelse(is.null(hold$Clinical_feature_other_Mf), '', hold$Clinical_feature_other_Mf),
                    height = "3em"
                  )
                )
              )
            )
          ),
          conditionalPanel(
            'input.Event_Details_Mf.includes("CVA")',
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
                  ns("CVA_date_Mf"),
                  label = "Date",
                  value = lubridate::as_date(hold$CVA_date_Mf),
                  language = "ko"
                )
              ),
              column(
                width = 5,
                radioButtons(
                  ns("CVA_Type_Mf"),
                  label = "Type",
                  choices = c("Ischemic", "Hemorrhagic", "Unknown"),
                  selected = ifelse(is.null(hold$CVA_Type_Mf), character(0), hold$CVA_Type_Mf),
                  inline = TRUE
                )
              ),
              column(
                width = 4,
                radioButtons(
                  ns("Imaging_Mf"),
                  label = "Verified with imaging studies",
                  selected = ifelse(is.null(hold$Imaging_Mf), character(0), hold$Imaging_Mf),
                  choices = c("Yes", "No"),
                  inline = TRUE
                )
              )
            )
          ),
          
          # Bleeding
          conditionalPanel(
            'input.Event_Details_Mf.includes("Bleeding")',
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
                  ns("Bleed_count_Mf"),
                  "Count",
                  value = ifelse(is.null(hold$Bleed_count_Mf), 0, hold$Bleed_count_Mf),
                  min = 1,
                  max = 2
                )
              )
            ),
            conditionalPanel(
              "input.Bleed_count_Mf >= 1",
              ns = ns,
              fluidRow(
                column(
                  width = 2,
                  dateInput(
                    ns("Bleed1_date_Mf"),
                    "Date",
                    value = lubridate::as_date(hold$Bleed1_date_Mf),
                    language = "ko"
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_Type_Mf"),
                    "BARC Type",
                    choices = c("BARC 2", "BARC 3", "BARC 5"),
                    selected = ifelse(is.null(hold$BARC1_Type_Mf), character(0), hold$BARC1_Type_Mf),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_ST_Mf"),
                    "Spontaneous or Traumatic",
                    choices = c("Spontaneous", "Traumatic"),
                    selected = ifelse(is.null(hold$BARC1_ST_Mf), character(0), hold$BARC1_ST_Mf),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_HT_Mf"),
                    "Requiring hospitalization or transfusion",
                    choices = c("Yes", "No"),
                    selected = ifelse(is.null(hold$BARC1_HT_Mf), character(0), hold$BARC1_HT_Mf),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  textInput(
                    ns("BARC1_Origin_Mf"),
                    label = "Origin of Bleeding	",
                    value = ifelse(is.null(hold$BARC1_Origin_Mf), "", hold$BARC1_Origin_Mf)
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_AT_Mf"),
                    "After Treatment",
                    choices = c("Recovered", "Death"),
                    selected = ifelse(is.null(hold$BARC1_AT_Mf), character(0), hold$BARC1_AT_Mf),
                    inline = TRUE
                  )
                )
              )
            ),
            conditionalPanel(
              "input.Bleed_count_Mf >= 2",
              ns = ns,
              fluidRow(
                column(
                  width = 2,
                  dateInput(
                    ns("Bleed2_date_Mf"),
                    "Date",
                    value = lubridate::as_date(hold$Bleed2_date_Mf),
                    language = "ko"
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_Type_Mf"),
                    "BARC Type",
                    choices = c("BARC 2", "BARC 3", "BARC 5"),
                    selected = ifelse(is.null(hold$BARC2_Type_Mf), character(0), hold$BARC2_Type_Mf),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_ST_Mf"),
                    "Spontaneous or Traumatic",
                    choices = c("Spontaneous", "Traumatic"),
                    selected = ifelse(is.null(hold$BARC2_ST_Mf), character(0), hold$BARC2_ST_Mf),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_HT_Mf"),
                    "Requiring hospitalization or transfusion",
                    choices = c("Yes", "No"),
                    selected = ifelse(is.null(hold$BARC2_HT_Mf), character(0), hold$BARC2_HT_Mf),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  textInput(
                    ns("BARC2_Origin_Mf"),
                    label = "Origin of Bleeding	",
                    value = ifelse(is.null(hold$BARC2_Origin_Mf), "", hold$BARC2_Origin_Mf)
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC2_AT_Mf"),
                    "After Treatment",
                    choices = c("Recovered", "Death"),
                    selected = ifelse(is.null(hold$BARC2_AT_Mf), character(0), hold$BARC2_AT_Mf),
                    inline = TRUE
                  )
                )
              )
            )
          ),
          
          # Readmission
          conditionalPanel(
            'input.Event_Details_Mf.includes("Readmission")',
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
                  ns("Readmission_Date_Mf"),
                  label = "Date",
                  value = lubridate::as_date(hold$Readmission_Date_Mf),
                  language = "ko"
                )
              ),
              column(
                width = 3,
                radioButtons(
                  ns("Readmission_Type_Mf"),
                  label = "Type",
                  choices = c("Heart failure", "cardiac problem(not heart failure)", "Other"),
                  selected = ifelse(is.null(hold$Readmission_Type_Mf), character(0), hold$Readmission_Type_Mf),
                  inline = TRUE
                )
              ),
              column(
                width = 3,
                conditionalPanel(
                  "input.Readmission_Type_Mf == 'Other'", # if Other : Show text box.
                  ns = ns,
                  textAreaInput(
                    ns("Readmission_Other_Mf"),
                    "Comment",
                    width = "400px",
                    height = "100px",
                    value = ifelse(is.null(hold$Readmission_Other_Mf), "", hold$Readmission_Other_Mf)
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
    updateTextInput(session, "SBP_Mf", value = "ND")
    updateTextInput(session, "DBP_Mf", value = "ND")
    updateTextInput(session, "HRT_Mf", value = "ND")
  })
  
  observeEvent(input$wbcND, {
    updateTextInput(session, "WBC_Mf", value = "ND")
  })
  
  observeEvent(input$tcND, {
    updateTextInput(session, "TC_Mf", value = "ND")
  })
  
  observeEvent(input$tgND, {
    updateTextInput(session, "TG_Mf", value = "ND")
  })
  
  observeEvent(input$hbND, {
    updateTextInput(session, "Hb_Mf", value = "ND")
  })
  
  observeEvent(input$plND, {
    updateTextInput(session, "Platelet_Mf", value = "ND")
  })
  
  observeEvent(input$hdlND, {
    updateTextInput(session, "HDL_Mf", value = "ND")
  })
  
  observeEvent(input$bunND, {
    updateTextInput(session, "BUN_Mf", value = "ND")
  })
  
  observeEvent(input$ldlND, {
    updateTextInput(session, "LDL_Mf", value = "ND")
  })
  
  observeEvent(input$crND, {
    updateTextInput(session, "Cr_Mf", value = "ND")
  })
  
  observeEvent(input$fgND, {
    updateTextInput(session, "FG_Mf", value = "ND")
  })
  
  observeEvent(input$mdND, {
    updateTextInput(session, "MDRD_Mf", value = "ND")
  })
  
  observeEvent(input$hb1ND, {
    updateTextInput(session, "HbA1C_Mf", value = "ND")
  })
  
  observeEvent(input$astND, {
    updateTextInput(session, "AST_Mf", value = "ND")
  })
  
  observeEvent(input$hsND, {
    updateTextInput(session, "hs_CRP_Mf", value = "ND")
  })
  
  observeEvent(input$altND, {
    updateTextInput(session, "ALT_Mf", value = "ND")
  })
  
  observeEvent(input$ntpND, {
    updateTextInput(session, "NT_Pro_BNP_Mf", value = "ND")
  })
  
  observeEvent(input$CYfA, {
    updateRadioButtons(session, "Aspirin_Mf", selected = "Yes")
    updateRadioButtons(session, "Trimetazidine_Mf", selected = "Yes")
    updateRadioButtons(session, "Clopidogrel_Mf", selected = "Yes")
    updateRadioButtons(session, "Nitrate_Mf", selected = "Yes")
    updateRadioButtons(session, "Prasugrel_Mf", selected = "Yes")
    updateRadioButtons(session, "Statin_Mf", selected = "Yes")
    updateRadioButtons(session, "Ticagrelor_Mf", selected = "Yes")
    updateRadioButtons(session, "BB_Mf", selected = "Yes")
    updateRadioButtons(session, "ACE_Mf", selected = "Yes")
    updateRadioButtons(session, "DPP4_Mf", selected = "Yes")
    updateRadioButtons(session, "Wafarin_Mf", selected = "Yes")
    updateRadioButtons(session, "NOAC_Mf", selected = "Yes")
    updateRadioButtons(session, "Nicorandil_Mf", selected = "Yes")
    updateRadioButtons(session, "Calcium_Mf", selected = "Yes")
    updateRadioButtons(session, "Metformin_Mf", selected = "Yes")
    updateRadioButtons(session, "Sulf_Mf", selected = "Yes")
    updateRadioButtons(session, "Thia_Mf", selected = "Yes")
    updateRadioButtons(session, "GLP_Mf", selected = "Yes")
    updateRadioButtons(session, "Alpha_Mf", selected = "Yes")
    updateRadioButtons(session, "Insulin_Mf", selected = "Yes")
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
        # "Readmission_Mf" = ifelse(is.null(input$Readmission_Mf), "", input$Readmission_Mf),
        # "Readmission_Date_Mf" = ifelse(is.null(input$Readmission_Date_Mf), "", input$Readmission_Date_Mf),
        # "Readmission_reason_Mf" = ifelse(is.null(input$Readmission_reason_Mf), "", input$Readmission_reason_Mf),
        # "Readmission_reason_text_Mf" = ifelse(is.null(input$Readmission_reason_text_Mf), "", input$Readmission_reason_text_Mf),
        "ECG_Rhythm_Mf" = ifelse(is.null(input$ECG_Rhythm_Mf), "", input$ECG_Rhythm_Mf),
        "ECG_Rhythm_Other_Mf" = ifelse(is.null(input$ECG_Rhythm_Other_Mf), "", input$ECG_Rhythm_Other_Mf),
        "Stress_ECG_Mf" = ifelse(is.null(input$Stress_ECG_Mf), "", input$Stress_ECG_Mf),
        "Stress_ECG_Date_Mf" = ifelse(is.null(input$Stress_ECG_Date_Mf), "", as.character(input$Stress_ECG_Date_Mf)),
        "Stress_ECG_Detail_Mf" = ifelse(is.null(input$Stress_ECG_Detail_Mf), "", input$Stress_ECG_Detail_Mf),
        "Lab_Date_Mf" = ifelse(is.null(input$Lab_Date_Mf), "", lubridate::as_date(input$Lab_Date_Mf)),
        "WBC_Mf" = ifelse(is.null(input$WBC_Mf), "", input$WBC_Mf),
        "TC_Mf" = ifelse(is.null(input$TC_Mf), "", input$TC_Mf),
        "Hb_Mf" = ifelse(is.null(input$Hb_Mf), "", input$Hb_Mf),
        "TG_Mf" = ifelse(is.null(input$TG_Mf), "", input$TG_Mf),
        "Platelet_Mf" = ifelse(is.null(input$Platelet_Mf), "", input$Platelet_Mf),
        "HDL_Mf" = ifelse(is.null(input$HDL_Mf), "", input$HDL_Mf),
        "BUN_Mf" = ifelse(is.null(input$BUN_Mf), "", input$BUN_Mf),
        "LDL_Mf" = ifelse(is.null(input$LDL_Mf), "", input$LDL_Mf),
        "Cr_Mf" = ifelse(is.null(input$Cr_Mf), "", input$Cr_Mf),
        "FG_Mf" = ifelse(is.null(input$FG_Mf), "", input$FG_Mf),
        "MDRD_Mf" = ifelse(is.null(input$MDRD_Mf), "", input$MDRD_Mf),
        "HbA1C_Mf" = ifelse(is.null(input$HbA1C_Mf), "", input$HbA1C_Mf),
        "AST_Mf" = ifelse(is.null(input$AST_Mf), "", input$AST_Mf),
        "hs_CRP_Mf" = ifelse(is.null(input$hs_CRP_Mf), "", input$hs_CRP_Mf),
        "ALT_Mf" = ifelse(is.null(input$ALT_Mf), "", input$ALT_Mf),
        "NT_Pro_BNP_Mf" = ifelse(is.null(input$NT_Pro_BNP_Mf), "", input$NT_Pro_BNP_Mf),
        "Aspirin_Mf" = ifelse(is.null(input$Aspirin_Mf), "", input$Aspirin_Mf),
        "Trimetazidine_Mf" = ifelse(is.null(input$Trimetazidine_Mf), "", input$Trimetazidine_Mf),
        "Clopidogrel_Mf" = ifelse(is.null(input$Clopidogrel_Mf), "", input$Clopidogrel_Mf),
        "Nitrate_Mf" = ifelse(is.null(input$Nitrate_Mf), "", input$Nitrate_Mf),
        "Nicorandil_Mf" = ifelse(is.null(input$Nicorandil_Mf), "", input$Nicorandil_Mf),
        "Prasugrel_Mf" = ifelse(is.null(input$Prasugrel_Mf), "", input$Prasugrel_Mf),
        "Statin_Mf" = ifelse(is.null(input$Statin_Mf), "", input$Statin_Mf),
        "Statin_name_Mf" = ifelse(is.null(input$Statin_name_Mf), "", input$Statin_name_Mf),
        "Statin_dose_Mf" = ifelse(is.null(input$Statin_dose_Mf), "", input$Statin_dose_Mf),
        "Ticagrelor_Mf" = ifelse(is.null(input$Ticagrelor_Mf), "", input$Ticagrelor_Mf),
        "Wafarin_Mf" = ifelse(is.null(input$Wafarin_Mf), "", input$Wafarin_Mf),
        "NOAC_Mf" = ifelse(is.null(input$NOAC_Mf), "", input$NOAC_Mf),
        "NOAC_name_Mf" = ifelse(is.null(input$NOAC_name_Mf), "", input$NOAC_name_Mf),
        "NOAC_dose_Mf" = ifelse(is.null(input$NOAC_dose_Mf), "", input$NOAC_dose_Mf),
        "BB_Mf" = ifelse(is.null(input$BB_Mf), "", input$BB_Mf),
        "BB_name_Mf" = ifelse(is.null(input$BB_name_Mf), "", input$BB_name_Mf),
        "BB_dose_Mf" = ifelse(is.null(input$BB_dose_Mf), "", input$BB_dose_Mf),
        "ACE_Mf" = ifelse(is.null(input$ACE_Mf), "", input$ACE_Mf),
        "ACE_name_Mf" = ifelse(is.null(input$ACE_name_Mf), "", input$ACE_name_Mf),
        "ACE_dose_Mf" = ifelse(is.null(input$ACE_dose_Mf), "", input$ACE_dose_Mf),
        "DPP4_Mf" = ifelse(is.null(input$DPP4_Mf), "", input$DPP4_Mf),
        "DPP4_name_Mf" = ifelse(is.null(input$DPP4_name_Mf), "", input$DPP4_name_Mf),
        "DPP4_dose_Mf" = ifelse(is.null(input$DPP4_dose_Mf), "", input$DPP4_dose_Mf),
        "Calcium_Mf" = ifelse(is.null(input$Calcium_Mf), "", input$Calcium_Mf),
        "Calcium_name_Mf" = ifelse(is.null(input$Calcium_name_Mf), "", input$Calcium_name_Mf),
        "Calcium_dose_Mf" = ifelse(is.null(input$Calcium_dose_Mf), "", input$Calcium_dose_Mf),
        "Metformin_Mf" = ifelse(is.null(input$Metformin_Mf), "", input$Metformin_Mf),
        "Metformin_dose_Mf" = ifelse(is.null(input$Metformin_dose_Mf), "", input$Metformin_dose_Mf),
        "Sulf_Mf" = ifelse(is.null(input$Sulf_Mf), "", input$Sulf_Mf),
        "Sulf_name_Mf" = ifelse(is.null(input$Sulf_name_Mf), "", input$Sulf_name_Mf),
        "Sulf_dose_Mf" = ifelse(is.null(input$Sulf_dose_Mf), "", input$Sulf_dose_Mf),
        "Thia_Mf" = ifelse(is.null(input$Thia_Mf), "", input$Thia_Mf),
        "Thia_name_Mf" = ifelse(is.null(input$Thia_name_Mf), "", input$Thia_name_Mf),
        "Thia_dose_Mf" = ifelse(is.null(input$Thia_dose_Mf), "", input$Thia_dose_Mf),
        "GLP_Mf" = ifelse(is.null(input$GLP_Mf), "", input$GLP_Mf),
        "GLP_name_Mf" = ifelse(is.null(input$GLP_name_Mf), "", input$GLP_name_Mf),
        "GLP_dose_Mf" = ifelse(is.null(input$GLP_dose_Mf), "", input$GLP_dose_Mf),
        "Alpha_Mf" = ifelse(is.null(input$Alpha_Mf), "", input$Alpha_Mf),
        "Alpha_name_Mf" = ifelse(is.null(input$Alpha_name_Mf), "", input$Alpha_name_Mf),
        "Alpha_dose_Mf" = ifelse(is.null(input$Alpha_dose_Mf), "", input$Alpha_dose_Mf),
        "Insulin_Mf" = ifelse(is.null(input$Insulin_Mf), "", input$Insulin_Mf),
        
        "Event_Details_Mf" = ifelse(is.null(input$Event_Details_Mf),"", paste0(input$Event_Details_Mf, collapse = ',')),
        
        "Death_date_Mf" = ifelse(is.null(input$Death_date_Mf), "", as.character(input$Death_date_Mf)),
        "Death_reason_Mf" = ifelse(is.null(input$Death_reason_Mf), "", input$Death_reason_Mf),
        "MI_date_Mf" = ifelse(is.null(input$MI_date_Mf), "", as.character(input$MI_date_Mf)),
        "MI_Segment_Mf" = ifelse(is.null(input$MI_Segment_Mf), "", input$MI_Segment_Mf),
        "MI_Type_Mf" = ifelse(is.null(input$MI_Type_Mf), "", input$MI_Type_Mf),
        "MI_Stent_Mf" = ifelse(is.null(input$MI_Stent_Mf), "", input$MI_Stent_Mf),
        "MI_Lesion_Mf" = ifelse(is.null(input$MI_Lesion_Mf), "", input$MI_Lesion_Mf),
        "MI_Vessel_Mf" = ifelse(is.null(input$MI_Vessel_Mf), "", input$MI_Vessel_Mf),
        "MI_Treatment_Mf" = ifelse(is.null(input$MI_Treatment_Mf),"", paste0(input$MI_Treatment_Mf, collapse = ',')),
        "MI_After_Mf" = ifelse(is.null(input$MI_After_Mf), "", input$MI_After_Mf),
        "Rev_date_Mf" = ifelse(is.null(input$Rev_date_Mf), "", as.character(input$Rev_date_Mf)),
        "Rev_Segment_Mf" = ifelse(is.null(input$Rev_Segment_Mf), "", input$Rev_Segment_Mf),
        "Rev_Treatment_Mf" = ifelse(is.null(input$Rev_Treatment_Mf),"", paste0(input$Rev_Treatment_Mf, collapse = ',')),
        "Rev_Lesion_Mf" = ifelse(is.null(input$Rev_Lesion_Mf), "", input$Rev_Lesion_Mf),
        "Rev_Vessel_Mf" = ifelse(is.null(input$Rev_Vessel_Mf), "", input$Rev_Vessel_Mf),
        "Rev_PCI_Mf" = ifelse(is.null(input$Rev_PCI_Mf), "", input$Rev_PCI_Mf),
        "Stent_date_Mf" = ifelse(is.null(input$Stent_date_Mf), "", as.character(input$Stent_date_Mf)),
        "Stent_Segment_Mf" = ifelse(is.null(input$Stent_Segment_Mf), "", input$Stent_Segment_Mf),
        "Stent_Type_Mf" = ifelse(is.null(input$Stent_Type_Mf), "", input$Stent_Type_Mf),
        "Stent_arc_Mf" = ifelse(is.null(input$Stent_arc_Mf), "", input$Stent_arc_Mf),
        "Clinical_feature_Mf" = ifelse(is.null(input$Clinical_feature_Mf),"", paste0(input$Clinical_feature_Mf, collapse = ',')),
        "Clinical_feature_other_Mf" = ifelse(is.null(input$Clinical_feature_other_Mf), "", input$Clinical_feature_other_Mf),
        "CVA_date_Mf" = ifelse(is.null(input$CVA_date_Mf), "", as.character(input$CVA_date_Mf)),
        "CVA_Type_Mf" = ifelse(is.null(input$CVA_Type_Mf), "", input$CVA_Type_Mf),
        "Bleed_count_Mf" = ifelse(is.null(input$Bleed_count_Mf), "", input$Bleed_count_Mf),
        "Bleed1_date_Mf" = ifelse(is.null(input$Bleed1_date_Mf), "", lubridate::as_date(input$Bleed1_date_Mf)),
        "BARC1_Type_Mf" = ifelse(is.null(input$BARC1_Type_Mf), "", input$BARC1_Type_Mf),
        "BARC1_ST_Mf" = ifelse(is.null(input$BARC1_ST_Mf), "", input$BARC1_ST_Mf),
        "BARC1_HT_Mf" = ifelse(is.null(input$BARC1_HT_Mf), "", input$BARC1_HT_Mf),
        "BARC1_Origin_Mf" = ifelse(is.null(input$BARC1_Origin_Mf), "", input$BARC1_Origin_Mf),
        "BARC1_AT_Mf" = ifelse(is.null(input$BARC1_AT_Mf), "", input$BARC1_AT_Mf),
        "Bleed2_date_Mf" = ifelse(is.null(input$Bleed2_date_Mf), "", lubridate::as_date(input$Bleed2_date_Mf)),
        "BARC2_Type_Mf" = ifelse(is.null(input$BARC2_Type_Mf), "", input$BARC2_Type_Mf),
        "BARC2_ST_Mf" = ifelse(is.null(input$BARC2_ST_Mf), "", input$BARC2_ST_Mf),
        "BARC2_HT_Mf" = ifelse(is.null(input$BARC2_HT_Mf), "", input$BARC2_HT_Mf),
        "BARC2_Origin_Mf" = ifelse(is.null(input$BARC2_Origin_Mf), "", input$BARC2_Origin_Mf),
        "BARC2_AT_Mf" = ifelse(is.null(input$BARC2_AT_Mf), "", input$BARC2_AT_Mf),
        "Readmission_Date_Mf" = ifelse(is.null(input$Readmission_Date_Mf), "", lubridate::as_date(input$Readmission_Date_Mf)),
        "Readmission_Type_Mf" = ifelse(is.null(input$Readmission_Type_Mf), "", input$Readmission_Type_Mf),
        "Readmission_Other_Mf" = ifelse(is.null(input$Readmission_Other_Mf), "", input$Readmission_Other_Mf),
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
