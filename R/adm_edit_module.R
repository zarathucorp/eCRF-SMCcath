#' adm Add & Edit Module
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
adm_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- car_to_edit()

    showModal(
      modalDialog(
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Physical Exam",
              tags$div(
                modalButton("", icon("times")),
                style = "float:right;"
              ),
              actionButton(
                ns("submit"),
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
            numericInput(
              ns("Height"),
              "Height",
              value = ifelse(is.null(hold), "", hold$Height),
              min = 0, max = 200,
              step = .1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("Weight"),
              "Weight",
              value = ifelse(is.null(hold), "", hold$Weight),
              min = 0, max = 120,
              step = .1
            )
          ),
          # Auto calculation
          column(
            width = 3,
            shinyjs::disabled(
              numericInput(
                ns("BMI"),
                "BMI",
                value = ifelse(is.null(hold), "", hold$BMI),
                min = 0, max = 120,
                step = 1
              )
            )
          ),
          column(
            width = 3,
            shinyjs::disabled(
              numericInput(
                ns("BSA_adm"),
                "BSA",
                value = ifelse(is.null(hold), "", hold$BSA_adm),
                min = 0, max = 120,
                step = 1
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(
              ns("SBP_adm"),
              "Systolic BP",
              value = ifelse(is.null(hold), "", hold$SBP_adm),
              min = 0, max = 200,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("DBP_adm"),
              "Diastolic BP",
              value = ifelse(is.null(hold), "", hold$DBP_adm),
              min = 0, max = 200,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("HR_adm"),
              "Heart Rate",
              value = ifelse(is.null(hold), "", hold$HR_adm),
              min = 0, max = 200,
              step = 1
            )
          ),
          column(
            width = 3,
            dateInput(
              ns("Date_adm"),
              "Admission Date",
              value = lubridate::as_date(hold$Date_adm),
              language = "ko"
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Risk Factors",
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
                ns("CNfA"),
                "No",
                class = "btn",
                style = "color: white; float:right; margin-right:10px; background-color : #f39c12;"
              )
            ),
            "</h3>"
          )
        ),
        fluidRow(
          column(
            width = 2,
            radioButtons(
              ns("HTN"), # Hypertension
              label = "Hypertension",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$HTN), character(0), hold$HTN),
              inline = TRUE
            )
          ),
          column(
            width = 2,
            radioButtons(
              ns("Hld_adm"), # Hyperlipidemia
              label = "Hyperlipidemia",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$Hld_adm), character(0), hold$Hld_adm),
              inline = TRUE
            )
          ),
          column(
            width = 2,
            radioButtons(
              ns("Smoking"), # Current Smoking
              label = "Smoking",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$Smoking), character(0), hold$Smoking),
              inline = TRUE
            )
          ),
          column(
            width = 2,
            shinyjs::disabled(
              radioButtons(
                ns("Diabetes_adm"),
                label = "Diabetes",
                choices = c("Yes" = "Yes", "No" = "No"),
                selected = hold$DM, # character(0),
                inline = TRUE
              )
            )
          ),
          column(
            width = 4,
              conditionalPanel(
                "input.Diabetes_adm == 'Yes'",
                ns = ns,
                checkboxGroupInput(
                  ns("Diabetes_detail_adm"),
                  label = "Diabetes: Detail",
                  choices = c("Diet Only" = "Diet Only", "OHA" = "OHA", "Insulin" = "Insulin"),
                  inline = TRUE,
                  selected = ifelse(is.null(hold$Diabetes_detail_adm), character(0), hold$Diabetes_detail_adm),
                )
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("FHC_adm"), # Family History of CAD
              label = "Family History of CAD",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$FHC_adm), character(0), hold$FHC_adm),
              inline = TRUE
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("PCHF_adm"), # Previous CHF
              label = "Previous CHF Admission",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$PCHF_adm), character(0), hold$PCHF_adm),
              inline = TRUE
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Pvd_adm"), # Peripheral vascular ds.
              label = "Peripheral Vascular ds.",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$Pvd_adm), character(0), hold$Pvd_adm),
              inline = TRUE
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("PCI_adm"), # Previous PCI
              label = "Previous PCI",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$PCI_adm), character(0), hold$PCI_adm),
              inline = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("PCABG_adm"), # Previous CABG
              label = "Previous CABG",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$PCABG_adm), character(0), hold$PCABG_adm),
              inline = TRUE
            )
          ),
          
          column(
            width = 3,
            radioButtons(
              ns("PCVA_adm"), # Previous CVA
              label = "Previous CVA",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$PCVA_adm), character(0), hold$PCVA_adm),
              inline = TRUE
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("CRF_adm"), # CRF
              label = "CRF (eGFR < 60ml/min/1.73m2)",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$CRF_adm), character(0), hold$CRF_adm),
              inline = TRUE
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("PNTB_adm"),
              label = "Previous Spontaneuous Bleeding (BARC>=2)",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$PNTB_adm), character(0), hold$PNTB_adm),
              inline = TRUE
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
                ns("submit2"),
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
              ns("Aspirin_adm"),
              label = "Aspirin",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Aspirin_adm),character(0), hold$Aspirin_adm),
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Trimetazidine_adm"),
              label = "Trimetazidine (Vastinan)",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Trimetazidine_adm),character(0), hold$Trimetazidine_adm),
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Clopidogrel_adm"),
              label = "Clopidogrel",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Clopidogrel_adm),character(0), hold$Clopidogrel_adm),
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Nitrate_adm"),
              label = "Nitrate",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Nitrate_adm),character(0), hold$Nitrate_adm),
              inline = T
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("Prasugrel_adm"),
              label = "Prasugrel",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Prasugrel_adm),character(0), hold$Prasugrel_adm),
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Ticagrelor_adm"),
              label = "Ticagrelor",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Ticagrelor_adm),character(0), hold$Ticagrelor_adm),
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Wafarin_adm"),
              label = "Wafarin",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Wafarin_adm),character(0), hold$Wafarin_adm),
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Nicorandil_adm"),
              label = "Nicorandil",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Nicorandil_adm),character(0), hold$Nicorandil_adm),
              inline = T
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("NOAC_adm"),
              label = "NOAC",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$NOAC_adm),character(0), hold$NOAC_adm),
              inline = T
            ),
            conditionalPanel(
              "input.NOAC_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                    width = 6,
                    textInput(
                      ns("NOAC_name_adm"),
                      label = "Name",
                      value = hold$NOAC_name_adm
                    )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("NOAC_dose_adm"),
                    label = "Dose",
                    value = hold$NOAC_dose_adm,
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
              ns("Statin_adm"),
              label = "Statin",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Statin_adm),character(0), hold$Statin_adm),
              inline = T
            ),
            conditionalPanel(
              "input.Statin_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Statin_name_adm"),
                    label = "Name",
                    value = hold$Statin_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Statin_dose_adm"),
                    label = "Dose",
                    value = hold$Statin_dose_adm,
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
              ns("BB_adm"),
              label = "Beta Blocker",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$BB_adm), character(0), hold$BB_adm),
              inline = T
            ),
            conditionalPanel(
              "input.BB_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("BB_name_adm"),
                    label = "Name",
                    value = hold$BB_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("BB_dose_adm"),
                    label = "Dose",
                    value = hold$BB_dose_adm,
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
              ns("Insulin_adm"),
              label = "Insulin",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Insulin_adm),character(0), hold$Insulin_adm),
              inline = T
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("ACE_adm"),
              label = "ACE Inhibitor or ARB",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$ACE_adm),character(0), hold$ACE_adm),
              inline = T
            ),
            conditionalPanel(
              "input.ACE_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("ACE_name_adm"),
                    label = "Name",
                    value = hold$ACE_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("ACE_dose_adm"),
                    label = "Dose",
                    value = hold$ACE_dose_adm,
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
              ns("DPP4_adm"),
              label = "DPP4 Inhibitor",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$DPP4_adm),character(0), hold$DPP4_adm),
              inline = T
            ),
            conditionalPanel(
              "input.DPP4_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("DPP4_name_adm"),
                    label = "Name",
                    value = hold$DPP4_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("DPP4_dose_adm"),
                    label = "Dose",
                    value = hold$DPP4_dose_adm,
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
              ns("Calcium_adm"),
              label = "Calcium channel blocker",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Calcium_adm),character(0), hold$Calcium_adm),
              inline = T
            ),
            conditionalPanel(
              "input.Calcium_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Calcium_name_adm"),
                    label = "Name",
                    value = hold$Calcium_name_adm
                  ) 
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Calcium_dose_adm"),
                    label = "Dose",
                    value = hold$Calcium_dose_adm,
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
              ns("Metformin_adm"),
              label = "Metformin",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Metformin_adm),character(0), hold$Metformin_adm),
              inline = T
            ),
            conditionalPanel(
              "input.Metformin_adm == 'Yes'",
              ns = ns,
              numericInput(
                ns("Metformin_dose_adm"),
                label = "Dose",
                value = hold$Metformin_dose_adm,
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
              ns("Sulf_adm"),
              label = "Sulfonylurea",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Sulf_adm),character(0), hold$Sulf_adm),
              inline = T
            ),
            conditionalPanel(
              "input.Sulf_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Sulf_name_adm"),
                    label = "Name",
                    value = hold$Sulf_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Sulf_dose_adm"),
                    label = "Dose",
                    value = hold$Sulf_dose_adm,
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
              ns("Thia_adm"),
              label = "Thiazolidinedione",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Thia_adm),character(0), hold$Thia_adm),
              inline = T
            ),
            conditionalPanel(
              "input.Thia_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Thia_name_adm"),
                    label = "Name",
                    value = hold$Thia_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Thia_dose_adm"),
                    label = "Dose",
                    value = hold$Thia_dose_adm,
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
              ns("GLP_adm"),
              label = "GLP-1 Agonist",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$GLP_adm),character(0), hold$GLP_adm),
              inline = T
            ),
            conditionalPanel(
              "input.GLP_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("GLP_name_adm"),
                    label = "Name",
                    value = hold$GLP_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("GLP_dose_adm"),
                    label = "Dose",
                    value = hold$GLP_dose_adm,
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
              ns("Alpha_adm"),
              label = "Alpha-glucosidase inhibitor",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.na(hold$Alpha_adm),character(0), hold$Alpha_adm),
              inline = T
            ),
            conditionalPanel(
              "input.Alpha_adm == 'Yes'",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Alpha_name_adm"),
                    label = "Name",
                    value = hold$Alpha_name_adm
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Alpha_dose_adm"),
                    label = "Dose",
                    value = hold$Alpha_dose_adm,
                    min = 0,
                    max = 200,
                    step = 1
                  )
                )
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
              ns("Lab_Date_adm"),
              "Lab Date",
              value = ifelse(is.null(hold), "", lubridate::as_date(hold$Lab_Date_adm)),
              language = "ko"
            ),
          ),
          column(
            width = 3,
            textInput(
              ns("WBC_adm"),
              placeholder = '10^3/ul',
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
              value = ifelse(is.null(hold), "", hold$WBC_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("TC_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$TC_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("Hb_adm"),
              placeholder = 'g/dl',
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
              value = ifelse(is.null(hold), "", hold$Hb_adm)
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("TG_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$TG_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("Platelet_adm"),
              placeholder = '10^3/ul',
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
              value = ifelse(is.null(hold), "", hold$Platelet_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("HDL_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$HDL_adm),
            )
          ),
          column(
            width = 3,
            textInput(
              ns("BUN_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$BUN_adm),
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("LDL_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$LDL_adm),
            )
          ),
          column(
            width = 3,
            textInput(
              ns("Cr_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$Cr_adm),
            )
          ),
          column(
            width = 3,
            textInput(
              ns("FG_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$FG_adm),
            )
          ),
          column(
            width = 3,
            textInput(
              ns("MDRD_adm"),
              placeholder = 'ml/min/1.73m2',
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
              value = ifelse(is.null(hold), "", hold$MDRD_adm),
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("HbA1C_adm"),
              placeholder = '%',
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
              value = ifelse(is.null(hold), "", hold$HbA1C_adm),
            )
          ),
          column(
            width = 3,
            textInput(
              ns("AST_adm"),
              placeholder = 'IU/L',
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
              value = ifelse(is.null(hold), "", hold$AST_adm),
            )
          ),
          column(
            width = 3,
            textInput(
              ns("hs_CRP_adm"),
              placeholder = 'mg/dl',
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
              value = ifelse(is.null(hold), "", hold$hs_CRP_adm),
            )
          ),
          column(
            width = 3,
            textInput(
              ns("ALT_adm"),
              placeholder = 'IU/L',
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
              value = ifelse(is.null(hold), "", hold$ALT_adm)
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("NT_Pro_BNP_adm"),
              placeholder = 'pg/ml',
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
              value = ifelse(is.null(hold), "", hold$NT_Pro_BNP_adm),
            )
          ),
          column(
            width = 4,
            radioButtons(
              ns("hCG_adm"),
              label = "hCG",
              choices = c("Positive" , "Negative", "NA"),
              selected = ifelse(is.null(hold$hCG_adm), character(0), hold$hCG_adm),
              inline = TRUE
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Cardiac Enzyme",
              tags$div(
                modalButton("", icon("times")),
                style = "float:right;"
              ),
              actionButton(
                ns("submit4"),
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
            width = 3
          ),
          column(
            width = 3,
            style = "text-align : center",
            h5("CK (IU/L)")
          ),
          column(
            width = 3,
            style = "text-align : center",
            h5("CK-MB (ng/ml)")
          ),
          column(
            width = 3,
            style = "text-align : center",
            h5("TroponinT (ng/ml)")
          )
        ),
        fluidRow(
          column(
            width = 3,
            h5("시술 전 peak 값", style = "padding-top : 15px")
          ),
          column(
            width = 3,
            numericInput(
              ns("BCK_adm"),
              "",
              value = ifelse(is.null(hold), "", hold$BCK_adm),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("BCKMB_adm"),
              "",
              value = ifelse(is.null(hold), "", hold$BCKMB_adm),
              min = 0, max = 120,
              step = .01
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("BTroT_adm"),
              "",
              value = ifelse(is.null(hold), "", hold$BTroT_adm),
              min = 0, max = 120,
              step = .001
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            h5("시술 후 peak 값", style = "padding-top : 15px")
          ),
          column(
            width = 3,
            numericInput(
              ns("PCK_adm"),
              "",
              value = ifelse(is.null(hold), "", hold$PCK_adm),
              min = 0, max = 120,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("PCKMB_adm"),
              "",
              value = ifelse(is.null(hold), "", hold$PCKMB_adm),
              min = 0, max = 120,
              step = .01
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("PTroT_adm"),
              "",
              value = ifelse(is.null(hold), "", hold$PTroT_adm),
              min = 0, max = 120,
              step = .001
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Baseline EKG",
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
            width = 4,
            radioButtons(
              ns("ECG_Rhythm_adm"),
              label = "",
              choices = c("Sinus", "Atrial Fibrillation", "Others"),
              selected = ifelse(is.null(hold$ECG_Rhythm_adm), character(0), hold$ECG_Rhythm_adm),
              inline = TRUE
            ),
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.ECG_Rhythm_adm == 'Others'", # if other
              ns = ns,
              textAreaInput(
                ns("ECG_Rhythm_others_adm"),
                "Others",
                height = "3em",
                value =  ifelse(is.null(hold$ECG_Rhythm_others_adm), "", hold$ECG_Rhythm_others_adm),
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Non-Invasive Test",
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
            width = 2,
            radioButtons(
              ns("MSPECT_adm"),
              label = "SPECT",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$MSPECT_adm), character(0), hold$MSPECT_adm),
              inline = TRUE
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.MSPECT_adm == 'Yes'",
              ns = ns,
              radioButtons(
                ns("MSPECT_detail_adm"),
                "",
                choices = c("Positive", "Negative"),
                selected = ifelse(is.null(hold$MSPECT_detail_adm), character(0), hold$MSPECT_detail_adm),
                inline = T
              )
            )
          ),
          column(
            width = 2,
            conditionalPanel(
              "input.MSPECT_adm == 'Yes'",
              ns = ns,
              dateInput(
                ns("MSPECT_Date_adm"),
                "Date",
                value = lubridate::as_date(hold$MSPECT_Date_adm),
                language = "ko"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            radioButtons(
              ns("TMT_adm"),
              label = "TMT",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$TMT_adm), character(0), hold$TMT_adm),
              inline = TRUE
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.TMT_adm == 'Yes'",
              ns = ns,
              radioButtons(
                ns("TMT_detail_adm"),
                "",
                choices = c("Positive", "Negative", "Suggestive Positivie", "Incomplete (Inadequate)"),
                selected = ifelse(is.null(hold$TMT_detail_adm), character(0), hold$TMT_detail_adm),
                inline = T
              )
            )
          ),
          column(
            width = 2,
            conditionalPanel(
              "input.TMT_adm == 'Yes'",
              ns = ns,
              dateInput(
                ns("TMT_Date_adm"),
                "Date",
                value = lubridate::as_date(hold$TMT_Date_adm),
                language = "ko"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            radioButtons(
              ns("EEcho_adm"),
              label = "Stress Echo",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$EEcho_adm), character(0), hold$EEcho_adm),
              inline = TRUE
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.EEcho_adm == 'Yes'",
              ns = ns,
              radioButtons(
                ns("EEcho_detail_adm"),
                "",
                choices = c("Positive", "Negative", "Equivocal", "Incomplete"),
                selected = ifelse(is.null(hold$EEcho_detail_adm), character(0), hold$EEcho_detail_adm),
                inline = T
              )
            )
          ),
          column(
            width = 2,
            conditionalPanel(
              "input.EEcho_adm == 'Yes'",
              ns = ns,
              dateInput(
                ns("EEcho_Date_adm"),
                "Date",
                value = lubridate::as_date(hold$EEcho_Date_adm),
                language = "ko"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 2,
            radioButtons(
              ns("APET_adm"),
              label = "PET",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$APET_adm), character(0), hold$APET_adm),
              inline = TRUE
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.APET_adm == 'Yes'",
              ns = ns,
              radioButtons(
                ns("APET_detail_adm"),
                "",
                choices = c("Positive", "Negative", "Equivocal"),
                selected = ifelse(is.null(hold$APET_detail_adm), character(0), hold$APET_detail_adm),
                inline = T
              )
            )
          ),
          column(
            width = 2,
            conditionalPanel(
              "input.APET_adm == 'Yes'",
              ns = ns,
              dateInput(
                ns("APET_Date_adm"),
                "Date",
                value = lubridate::as_date(hold$APET_Date_adm),
                language = "ko"
              )
            )
          )
        ),

        ## CMR
        fluidRow(
          column(
            width = 2,
            radioButtons(
              ns("CMR_adm"),
              label = "CMR",
              choices = c("Yes" = "Yes", "No" = "No"),
              selected = ifelse(is.null(hold$CMR_adm), character(0), hold$CMR_adm),
              inline = TRUE
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.CMR_adm == 'Yes'",
              ns = ns,
              radioButtons(
                ns("CMR_detail_adm"),
                "",
                choices = c("Positive", "Negative"),
                selected = ifelse(is.null(hold$CMR_detail_adm), character(0), hold$CMR_detail_adm),
                inline = T
              )
            )
          ),
          column(
            width = 2,
            conditionalPanel(
              "input.CMR_adm == 'Yes'",
              ns = ns,
              dateInput(
                ns("CMR_Date_adm"),
                "Date",
                value = lubridate::as_date(hold$CMR_Date_adm),
                language = "ko"
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Echocardiography (Post-PCI)",
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
              ns("Echo_Date_adm"),
              "Examination Date",
              value = lubridate::as_date(hold$Echo_Date_adm),
              language = "ko"
            )
          ),
          column(
            width = 3,
            textInput(
              ns("LVIDs_adm"),
              "LVIDs",
              placeholder = "mm",
              value = ifelse(is.null(hold), "", hold$LVIDs_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("LVEDV_adm"),
              "LVEDV",
              placeholder = "ml",
              value = ifelse(is.null(hold), "", hold$LVEDV_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("LVESV_adm"),
              "LVESV",
              placeholder = "ml",
              value = ifelse(is.null(hold), "", hold$LVESV_adm)
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("LA_adm"),
              "LA",
              placeholder = "mm",
              value = ifelse(is.null(hold), "", hold$LA_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("LAVI_adm"),
              "LAVI",
              placeholder = "ml/m2",
              value = ifelse(is.null(hold), "", hold$LAVI_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("A_adm"),
              "A",
              placeholder = "m/s",
              value = ifelse(is.null(hold), "", hold$A_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("EA_adm"),
              "E/A",
              value = ifelse(is.null(hold), "", hold$EA_adm)
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("DT_adm"),
              "DT",
              placeholder = "msec",
              value = ifelse(is.null(hold), "", hold$DT_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("ep_adm"),
              "e'",
              placeholder = "m/s",
              value = ifelse(is.null(hold), "", hold$ep_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("ap_adm"),
              "a'",
              placeholder = "m/s",
              value = ifelse(is.null(hold), "", hold$ap_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("Eep_adm"),
              "E/e'",
              value = ifelse(is.null(hold), "", hold$Eep_adm)
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              ns("RVSP_adm"),
              "RVSP",
              placeholder = "mmHg",
              value = ifelse(is.null(hold), "", hold$RVSP_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("RAP_adm"),
              "RAP",
              placeholder = "mmHg",
              value = ifelse(is.null(hold), "", hold$RAP_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("LVEF_adm"),
              "LVEF",
              placeholder = "%",
              value = ifelse(is.null(hold), "", hold$LVEF_adm)
            )
          ),
          column(
            width = 3,
            textInput(
              ns("Strain_adm"),
              "Strain",
              placeholder = "%",
              value = ifelse(is.null(hold), "", hold$Strain_adm)
            )
          )
        ),
        dateInput(
          ns("CMR_Date_POST_adm"),
          "CMR Date (POST-PCI)",
          value = lubridate::as_date(hold$CMR_Date_POST_adm),
          language = "ko"
        ),
        title =
          tags$div(
            HTML(
              paste0('<h3 style = "float:left;margin-top:0px">', modal_title, "</h3>")
            ),
            tags$div(
              modalButton("", icon("times")),
              style = "float:right;"
            ),
            actionButton(
              inputId = ns("submit7"),
              HTML('<i class="fas fa-check"></i>'),
              class = "btn",
              style = "color: white; float:right; margin-right:10px; background-color : #27ae60;"
            )
          ),
        # modal_title,
        size = "l",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            inputId = ns("submit8"),
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

  observeEvent(
    {
      input$Height
      input$Weight
    },
    {
      updateNumericInput(session, "BMI", value = round(input$Weight / (input$Height / 100)^2, 1))
      updateNumericInput(session, "BSA_adm", value = round(sqrt(input$Weight * input$Height / 3600), 1))
    }
  )

  observeEvent(input$CNfA, {
    updateRadioButtons(session, "HTN", selected = "No")
    # updateRadioButtons(session, "Diabetes_adm", selected = 1)
    updateRadioButtons(session, "Hld_adm", selected = "No")
    updateRadioButtons(session, "Smoking", selected = "No")
    updateRadioButtons(session, "FHC_adm", selected = "No")
    updateRadioButtons(session, "CRF_adm", selected = "No")
    updateRadioButtons(session, "Pvd_adm", selected = "No")
    updateRadioButtons(session, "PCI_adm", selected = "No")
    updateRadioButtons(session, "PCABG_adm", selected = "No")
    updateRadioButtons(session, "PCVA_adm", selected = "No")
    updateRadioButtons(session, "PCHF_adm", selected = "No")
    updateRadioButtons(session, "PNTB_adm", selected = "No")
  })

  observeEvent(input$wbcND, {
    updateTextInput(session, "WBC_adm", value = "ND")
  })

  observeEvent(input$tcND, {
    updateTextInput(session, "TC_adm", value = "ND")
  })

  observeEvent(input$tgND, {
    updateTextInput(session, "TG_adm", value = "ND")
  })

  observeEvent(input$hbND, {
    updateTextInput(session, "Hb_adm", value = "ND")
  })

  observeEvent(input$plND, {
    updateTextInput(session, "Platelet_adm", value = "ND")
  })

  observeEvent(input$hdlND, {
    updateTextInput(session, "HDL_adm", value = "ND")
  })

  observeEvent(input$bunND, {
    updateTextInput(session, "BUN_adm", value = "ND")
  })

  observeEvent(input$ldlND, {
    updateTextInput(session, "LDL_adm", value = "ND")
  })

  observeEvent(input$crND, {
    updateTextInput(session, "Cr_adm", value = "ND")
  })

  observeEvent(input$fgND, {
    updateTextInput(session, "FG_adm", value = "ND")
  })

  observeEvent(input$mdND, {
    updateTextInput(session, "MDRD_adm", value = "ND")
  })

  observeEvent(input$hb1ND, {
    updateTextInput(session, "HbA1C_adm", value = "ND")
  })

  observeEvent(input$astND, {
    updateTextInput(session, "AST_adm", value = "ND")
  })

  observeEvent(input$hsND, {
    updateTextInput(session, "hs_CRP_adm", value = "ND")
  })

  observeEvent(input$altND, {
    updateTextInput(session, "ALT_adm", value = "ND")
  })

  observeEvent(input$ntpND, {
    updateTextInput(session, "NT_Pro_BNP_adm", value = "ND")
  })
  observeEvent(input$CYfA, {
    updateRadioButtons(session, "Aspirin_adm", selected = "Yes")
    updateRadioButtons(session, "Trimetazidine_adm", selected = "Yes")
    updateRadioButtons(session, "Clopidogrel_adm", selected = "Yes")
    updateRadioButtons(session, "Nitrate_adm", selected = "Yes")
    updateRadioButtons(session, "Prasugrel_adm", selected = "Yes")
    updateRadioButtons(session, "Statin_adm", selected = "Yes")
    updateRadioButtons(session, "Ticagrelor_adm", selected = "Yes")
    updateRadioButtons(session, "BB_adm", selected = "Yes")
    updateRadioButtons(session, "ACE_adm", selected = "Yes")
    updateRadioButtons(session, "DPP4_adm", selected = "Yes")
    updateRadioButtons(session, "Wafarin_adm", selected = "Yes")
    updateRadioButtons(session, "NOAC_adm", selected = "Yes")
    updateRadioButtons(session, "Nicorandil_adm", selected = "Yes")
    updateRadioButtons(session, "Calcium_adm", selected = "Yes")
    updateRadioButtons(session, "Metformin_adm", selected = "Yes")
    updateRadioButtons(session, "Sulf_adm", selected = "Yes")
    updateRadioButtons(session, "Thia_adm", selected = "Yes")
    updateRadioButtons(session, "GLP_adm", selected = "Yes")
    updateRadioButtons(session, "Alpha_adm", selected = "Yes")
    updateRadioButtons(session, "Insulin_adm", selected = "Yes")
  })

  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      data = list(
        "Date_adm" = ifelse(is.null(input$Date_adm), "", lubridate::as_date(input$Date_adm)),
        "Height" = ifelse(is.null(input$Height), "", input$Height),
        "Weight" = ifelse(is.null(input$Weight), "", input$Weight),
        "BMI" = ifelse(is.null(input$BMI), "", input$BMI),
        "BSA_adm" = ifelse(is.null(input$BSA_adm), "", input$BSA_adm),
        "SBP_adm" = ifelse(is.null(input$SBP_adm), "", input$SBP_adm),
        "DBP_adm" = ifelse(is.null(input$DBP_adm), "", input$DBP_adm),
        "HR_adm" = ifelse(is.null(input$HR_adm), "", input$HR_adm),
        
        "HTN" = ifelse(is.null(input$HTN), "", input$HTN),
        "Diabetes_adm" = ifelse(is.null(input$Diabetes_adm), "", input$Diabetes_adm),
        "Diabetes_detail_adm" = ifelse(is.null(input$Diabetes_detail_adm), "", input$Diabetes_detail_adm),
        "Hld_adm" = ifelse(is.null(input$Hld_adm), "", input$Hld_adm),
        "Smoking" = ifelse(is.null(input$Smoking), "", input$Smoking),
        "FHC_adm" = ifelse(is.null(input$FHC_adm), "", input$FHC_adm),
        "CRF_adm" = ifelse(is.null(input$CRF_adm), "", input$CRF_adm),
        "Pvd_adm" = ifelse(is.null(input$Pvd_adm), "", input$Pvd_adm),
        "PCI_adm" = ifelse(is.null(input$PCI_adm), "", input$PCI_adm),
        "PCABG_adm" = ifelse(is.null(input$PCABG_adm), "", input$PCABG_adm),
        "PCVA_adm" = ifelse(is.null(input$PCVA_adm), "", input$PCVA_adm),
        "PCHF_adm" = ifelse(is.null(input$PCHF_adm), "", input$PCHF_adm),
        "PNTB_adm" = ifelse(is.null(input$PNTB_adm), "", input$PNTB_adm),
        
        "Aspirin_adm" = ifelse(is.null(input$Aspirin_adm), "", input$Aspirin_adm),
        "Trimetazidine_adm" = ifelse(is.null(input$Trimetazidine_adm), "", input$Trimetazidine_adm),
        "Clopidogrel_adm" = ifelse(is.null(input$Clopidogrel_adm), "", input$Clopidogrel_adm),
        "Nitrate_adm" = ifelse(is.null(input$Nitrate_adm), "", input$Nitrate_adm),
        "Nicorandil_adm" = ifelse(is.null(input$Nicorandil_adm), "", input$Nicorandil_adm),
        "Prasugrel_adm" = ifelse(is.null(input$Prasugrel_adm), "", input$Prasugrel_adm),
        "Statin_adm" = ifelse(is.null(input$Statin_adm), "", input$Statin_adm),
        "Statin_name_adm" = ifelse(is.null(input$Statin_name_adm), "", input$Statin_name_adm),
        "Statin_dose_adm" = ifelse(is.null(input$Statin_dose_adm), "", input$Statin_dose_adm),
        "Ticagrelor_adm" = ifelse(is.null(input$Ticagrelor_adm), "", input$Ticagrelor_adm),
        'Wafarin_adm' = ifelse(is.null(input$Wafarin_adm), '', input$Wafarin_adm),
        'NOAC_adm' = ifelse(is.null(input$NOAC_adm), '', input$NOAC_adm),
        'NOAC_name_adm' = ifelse(is.null(input$NOAC_name_adm), '', input$NOAC_name_adm),
        'NOAC_dose_adm' = ifelse(is.null(input$NOAC_dose_adm), '', input$NOAC_dose_adm),
        "BB_adm" = ifelse(is.null(input$BB_adm), "", input$BB_adm),
        "BB_name_adm" = ifelse(is.null(input$BB_name_adm), "", input$BB_name_adm),
        "BB_dose_adm" = ifelse(is.null(input$BB_dose_adm), "", input$BB_dose_adm),
        "ACE_adm" = ifelse(is.null(input$ACE_adm), "", input$ACE_adm),
        "ACE_name_adm" = ifelse(is.null(input$ACE_name_adm), "", input$ACE_name_adm),
        "ACE_dose_adm" = ifelse(is.null(input$ACE_dose_adm), "", input$ACE_dose_adm),
        'DPP4_adm' = ifelse(is.null(input$DPP4_adm), '', input$DPP4_adm),
        'DPP4_name_adm' = ifelse(is.null(input$DPP4_name_adm), '', input$DPP4_name_adm),
        'DPP4_dose_adm' = ifelse(is.null(input$DPP4_dose_adm), '', input$DPP4_dose_adm),
        "Calcium_adm" = ifelse(is.null(input$Calcium_adm), "", input$Calcium_adm),
        "Calcium_name_adm" = ifelse(is.null(input$Calcium_name_adm), "", input$Calcium_name_adm),
        "Calcium_dose_adm" = ifelse(is.null(input$Calcium_dose_adm), "", input$Calcium_dose_adm),
        'Metformin_adm' = ifelse(is.null(input$Metformin_adm), '', input$Metformin_adm),
        'Metformin_dose_adm' = ifelse(is.null(input$Metformin_dose_adm), '', input$Metformin_dose_adm),
        'Sulf_adm' = ifelse(is.null(input$Sulf_adm), '', input$Sulf_adm),
        'Sulf_name_adm' = ifelse(is.null(input$Sulf_name_adm), '', input$Sulf_name_adm),
        'Sulf_dose_adm' = ifelse(is.null(input$Sulf_dose_adm), '', input$Sulf_dose_adm),
        'Thia_adm' = ifelse(is.null(input$Thia_adm), '', input$Thia_adm),
        'Thia_name_adm' = ifelse(is.null(input$Thia_name_adm), '', input$Thia_name_adm),
        'Thia_dose_adm' = ifelse(is.null(input$Thia_dose_adm), '', input$Thia_dose_adm),
        'GLP_adm' = ifelse(is.null(input$GLP_adm), '', input$GLP_adm),
        'GLP_name_adm' = ifelse(is.null(input$GLP_name_adm), '', input$GLP_name_adm),
        'GLP_dose_adm' = ifelse(is.null(input$GLP_dose_adm), '', input$GLP_dose_adm),
        'Alpha_adm' = ifelse(is.null(input$Alpha_adm), '', input$Alpha_adm),
        'Alpha_name_adm' = ifelse(is.null(input$Alpha_name_adm), '', input$Alpha_name_adm),
        'Alpha_dose_adm' = ifelse(is.null(input$Alpha_dose_adm), '', input$Alpha_dose_adm),
        'Insulin_adm' = ifelse(is.null(input$Insulin_adm), '', input$Insulin_adm),
        
        "Lab_Date_adm" = ifelse(is.null(input$Lab_Date_adm), "", lubridate::as_date(input$Lab_Date_adm)),
        "WBC_adm" = ifelse(is.null(input$WBC_adm), "", input$WBC_adm),
        "TC_adm" = ifelse(is.null(input$TC_adm), "", input$TC_adm),
        "Hb_adm" = ifelse(is.null(input$Hb_adm), "", input$Hb_adm),
        "TG_adm" = ifelse(is.null(input$TG_adm), "", input$TG_adm),
        "Platelet_adm" = ifelse(is.null(input$Platelet_adm), "", input$Platelet_adm),
        "HDL_adm" = ifelse(is.null(input$HDL_adm), "", input$HDL_adm),
        "BUN_adm" = ifelse(is.null(input$BUN_adm), "", input$BUN_adm),
        "LDL_adm" = ifelse(is.null(input$LDL_adm), "", input$LDL_adm),
        "Cr_adm" = ifelse(is.null(input$Cr_adm), "", input$Cr_adm),
        "FG_adm" = ifelse(is.null(input$FG_adm), "", input$FG_adm),
        "MDRD_adm" = ifelse(is.null(input$MDRD_adm), "", input$MDRD_adm),
        "HbA1C_adm" = ifelse(is.null(input$HbA1C_adm), "", input$HbA1C_adm),
        "AST_adm" = ifelse(is.null(input$AST_adm), "", input$AST_adm),
        "hs_CRP_adm" = ifelse(is.null(input$hs_CRP_adm), "", input$hs_CRP_adm),
        "ALT_adm" = ifelse(is.null(input$ALT_adm), "", input$ALT_adm),
        "NT_Pro_BNP_adm" = ifelse(is.null(input$NT_Pro_BNP_adm), "", input$NT_Pro_BNP_adm),
        'hCG_adm' = ifelse(is.null(input$hCG_adm), '', input$hCG_adm),
        "BCK_adm" = ifelse(is.null(input$BCK_adm), "", input$BCK_adm),
        "BCKMB_adm" = ifelse(is.null(input$BCKMB_adm), "", input$BCKMB_adm),
        "BTroT_adm" = ifelse(is.null(input$BTroT_adm), "", input$BTroT_adm),
        "PCK_adm" = ifelse(is.null(input$PCK_adm), "", input$PCK_adm),
        "PCKMB_adm" = ifelse(is.null(input$PCKMB_adm), "", input$PCKMB_adm),
        "PTroT_adm" = ifelse(is.null(input$PTroT_adm), "", input$PTroT_adm),
        
        "ECG_Rhythm_adm" = ifelse(is.null(input$ECG_Rhythm_adm), "", input$ECG_Rhythm_adm),
        "ECG_Rhythm_others_adm" = ifelse(is.null(input$ECG_Rhythm_others_adm), "", input$ECG_Rhythm_others_adm),
        
        "MSPECT_adm" = ifelse(is.null(input$MSPECT_adm), '', input$MSPECT_adm),
        "MSPECT_detail_adm" = ifelse(is.null(input$MSPECT_detail_adm), '', input$MSPECT_detail_adm),
        "MSPECT_Date_adm" = ifelse(is.null(input$MSPECT_Date_adm), Sys.Date(), input$MSPECT_Date_adm),
        "TMT_adm" = ifelse(is.null(input$TMT_adm), '', input$TMT_adm),
        "TMT_Date_adm" = ifelse(is.null(input$TMT_Date_adm), Sys.Date(), input$TMT_Date_adm),
        "TMT_detail_adm" = ifelse(is.null(input$TMT_detail_adm), '', input$TMT_detail_adm),
        "EEcho_adm" = ifelse(is.null(input$EEcho_adm), "", input$EEcho_adm),
        "EEcho_detail_adm" = ifelse(is.null(input$EEcho_detail_adm), "", input$EEcho_detail_adm),
        "EEcho_Date_adm" = ifelse(is.null(input$EEcho_Date_adm), Sys.Date(), input$EEcho_Date_adm),
        "APET_adm" = ifelse(is.null(input$APET_adm), "", input$APET_adm),
        "APET_detail_adm" = ifelse(is.null(input$APET_detail_adm), "", input$APET_detail_adm),
        "APET_Date_adm" = ifelse(is.null(input$APET_Date_adm), Sys.Date(), input$APET_Date_adm),
        "CMR_adm" = ifelse(is.null(input$CMR_adm), "", input$CMR_adm),
        "CMR_detail_adm" = ifelse(is.null(input$CMR_detail_adm), "", input$CMR_detail_adm),
        "CMR_Date_adm" = ifelse(is.null(input$CMR_Date_adm), Sys.Date(), input$CMR_Date_adm),
        
        "Echo_Date_adm" = ifelse(is.null(input$Echo_Date_adm), Sys.Date(), input$Echo_Date_adm),
        'LVIDs_adm' = ifelse(is.null(input$LVIDs_adm), '', input$LVIDs_adm),
        'LVEDV_adm' = ifelse(is.null(input$LVEDV_adm), '', input$LVEDV_adm),
        'LVESV_adm' = ifelse(is.null(input$LVESV_adm), '', input$LVESV_adm),
        'LA_adm' = ifelse(is.null(input$LA_adm), '', input$LA_adm),
        'LAVI_adm' = ifelse(is.null(input$LAVI_adm), '', input$LAVI_adm),
        'A_adm' = ifelse(is.null(input$A_adm), '', input$A_adm),
        'EA_adm' = ifelse(is.null(input$EA_adm), '', input$EA_adm),
        'DT_adm' = ifelse(is.null(input$DT_adm), '', input$DT_adm),
        'ep_adm' = ifelse(is.null(input$ep_adm), '', input$ep_adm),
        'ap_adm' = ifelse(is.null(input$ap_adm), '', input$ap_adm),
        'Eep_adm' = ifelse(is.null(input$Eep_adm), '', input$Eep_adm),
        'RVSP_adm' = ifelse(is.null(input$RVSP_adm), '', input$RVSP_adm),
        'RAP_adm' = ifelse(is.null(input$RAP_adm), '', input$RAP_adm),
        'LVEF_adm' = ifelse(is.null(input$LVEF_adm), '', input$LVEF_adm),
        'Strain_adm' = ifelse(is.null(input$Strain_adm), '', input$Strain_adm),
        'CMR_Date_POST_adm' = ifelse(is.null(input$CMR_Date_POST_adm), Sys.Date(), as.character(input$CMR_Date_POST_adm))
        

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
      input$submit9
    )
  })

  # Reference : https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent

  validate_edit <- eventReactive(
    eventExpr = callEdit(),
    valueExpr = {
      if (input$submit == 0 && input$submit1 == 0 && input$submit2 == 0 &&
        input$submit3 == 0 && input$submit4 == 0 && input$submit5 == 0 &&
        input$submit6 == 0 && input$submit7 == 0 && input$submit8 == 0 && 
        input$submit9 ==0) {
        return()
      }
      dat <- edit_car_dat()
      # Logic to validate inputs...
      dat
    }, ignoreInit = TRUE
  )


  observeEvent(
    {
      validate_edit()
    },
    {
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
    }
  )
}
