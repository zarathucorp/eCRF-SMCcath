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
        # physical exam
        fluidRow(
          column(
            width = 12,
            dateInput(
              ns("Date_adm"),
              "Admission Date",
              value = lubridate::as_date(hold$Date_adm),
              language = "kr"
            )
          )
        ),
        tags$div(HTML('<h3><i class="fa fa-weight" style = "color:#0072B2;"></i> Physical Exam</h3>')),
        # h3('Physical Exam', style = "color : #1283EF"),
        fluidRow(
          column(
            width = 3,
            numericInput(
              ns("Height"),
              "Height",
              value = ifelse(is.null(hold), NA, hold$Height),
              min = 0, max = 200,
              step = .1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("Weight"),
              "Weight",
              value = ifelse(is.null(hold), NA, hold$Weight),
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
                value = ifelse(is.null(hold), NA, hold$BMI),
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
                value = ifelse(is.null(hold), NA, hold$BSA_adm),
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
              value = ifelse(is.null(hold), NA, hold$SBP_adm),
              min = 0, max = 200,
              step = 1
            )
          ),
          column(
            width = 3,
            numericInput(
              ns("DBP_adm"),
              "Diastolic BP",
              value = ifelse(is.null(hold), NA, hold$DBP_adm),
              min = 0, max = 200,
              step = 1
            )
          ),
          
          
          column(
            width = 3,
            numericInput(
              ns("HR_adm"),
              #label = tags$div(HTML('<i class="fa fa-heartbeat" style = "color:#0072B2;"></i> Heart Rate')),
              "Heart Rate",
              value = ifelse(is.null(hold), NA, hold$HR_adm),
              min = 0, max = 200,
              step = 1
            )
          )
        ),

        # Diagnosis at Discharge

        #radioButtons(
        #  ns("Diagnosis_adm"),
        #  label = "Diagnosis at Discharge",
        #  choices = c("Silent Ischemia" = 0, "Stable Angina" = 1, "Unstable Angina" = 2, "NSTEMI" = 3, "STEMI" = 4, "Atypical Chest Pain" = 5, "Others" = 6),
        #  inline = TRUE
        #),
        #conditionalPanel(
        #  "input.Diagnosis_adm == 1",
        #  radioButtons(
        #    ns("CCS_adm"),
        #    label = "CCS",
        #    choices = c("I" = 0, "II" = 1, "III" = 2, "IV" = 3),
        #    inline = TRUE
        #  ),
        #),
        #conditionalPanel(
        #  "input.Diagnosis_adm == 2",
        #  radioButtons(
        #    ns("BC_adm"), # Braunwald Class
        #    label = "Braunwald Class",
        #    choices = c("I" = 0, "II" = 1, "III" = 2),
        #    inline = TRUE
        #  ),
        #),
        #conditionalPanel(
        #  "input.Diagnosis_adm == 6",
        #  textInput(
        #    "Diagnosis_other_adm",
        #    label = "Others",
        #    value = ifelse(is.null(hold), NA, hold$Diagnosis_other_adm)
        #  ),
        #),

        # Diagnosis Comment
        #textAreaInput(
        #  ns("Diagnosis_comment_adm"),
        #  "Diagnosis Comment",
        #  width = "400px",
        #  height = "100px",
        #  value =  ifelse(is.null(hold$Diagnosis_comment_adm), "", hold$Diagnosis_comment_adm),
        #),

        # Risk Factors
        h3('Risk Factors', style = "color : #FFFFFF; padding: 0.3em; background : #1283ef"),
        fluidRow(
          style = 'text-align : center',
          column(
            width = 6,
            radioButtons(
              ns("HTN"), # Hypertension
              label = "Hypertension",
              choices = c("Yes" = 0, "No" = 1),
              selected = character(0),
              inline = TRUE
            ),
          ),
          
          column(
            width = 6,
            radioButtons(
              ns("Diabetes_adm"),
              label = "Diabetes",
              choices = c("Yes" = 0, "No" = 1),
              selected = character(0),
              inline = TRUE
            ),
            conditionalPanel(
              "input.Diabetes_adm == 0",
              ns = ns,
              checkboxGroupInput(
                ns("Diabetes_detail_adm"),
                label = "if Yes",
                choices = c("Diet Only" = 0, "OHA" = 1, "Insulin" = 2),
                selected = character(0),
                inline = TRUE
              )
            ),
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(
              ns("Hld_adm"), # Hyperlipidemia
              label = "Hyperlipidemia",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("Smoking"), # Current Smoking
              label = "Current Smoking",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            radioButtons(
              ns("FHC_adm"), # Family History of CAD
              label = "Family History of CAD",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("CRF_adm"), # CRF
              label = "CRF (eGFR < 60ml/min/1.73m2)",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(
              ns("Pvd_adm"), # Peripheral vascular ds.
              label = "Peripheral Vascular ds.",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("PCI_adm"), # Previous MI
              label = "Previous PCI",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            radioButtons(
              ns("PCABG_adm"), # Previous CABG
              label = "Previous CABG",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("PCVA_adm"), # Previous CVA
              label = "Previous CVA",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            radioButtons(
              ns("PCHF_adm"), # Previous CHF
              label = "Previous CHF Admission",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("PNTB_adm"), 
              label = "Previous Spontaneuous Bleeding (BARC>=2)",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE
            )
          )
        ),
        
        # Risk Factors Comment
        #textAreaInput(
        #  ns("Risk_comment_adm"),
        #  "Risk Factors Comment",
        #  width = "400px",
        #  height = "100px",
        #  value =  ifelse(is.null(hold$Risk_comment_adm), "", hold$Risk_comment_adm),
        #),

        
        # Medication Data
        h3('Medication Data', style = "color : #FFFFFF; padding: 0.3em; background : #1283ef"),
        radioButtons(
          ns("Aspirin_adm"),
          label = "Aspirin",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Aspirin_adm
        ),
        radioButtons(
          ns("Clopidogrel_adm"),
          label = "Clopidogrel",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Clopidogrel_adm
        ),
        radioButtons(
          ns("Prasugrel_adm"),
          label = "Prasugrel",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Prasugrel_adm
        ),
        radioButtons(
          ns("Ticagrelor_adm"),
          label = "Ticagrelor",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Ticagrelor_adm
        ),
        radioButtons( # Beta Blocker
          ns("BB_adm"),
          label = "Beta Blocker",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$BB_adm
        ),
        radioButtons(
          ns("WN_adm"),
          label = "Wafarin or NOAC",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$WN_adm
        ),
        radioButtons(
          ns("Statin_adm"),
          label = "Statin",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Statin_adm
        ),
        radioButtons(
          ns("ACE_adm"),
          label = "ACE Inhibitor or ARB",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$ACE_adm
        ),
        radioButtons(
          ns("Nitrate_adm"),
          label = "Nitrate (Sigmart)",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Nitrate_adm
        ),
        radioButtons(
          ns("Calcium_adm"),
          label = "Calcium channel antagonist",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Calcium_adm
        ),
        radioButtons(
          ns("Trimetazidine_adm"),
          label = "Trimetazidine (Vastinan)",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Trimetazidine_adm
        ),
        
        # Medication Comment
        
        textAreaInput(
          ns("Medication_comment_adm"),
          "Medication Comment",
          width = "400px",
          height = "100px",
          value =  ifelse(is.null(hold$Medication_comment_adm), "", hold$Medication_comment_adm),
        ),
        
        # Non-invasive Tests

        radioButtons(
          ns("TMT_adm"),
          label = "Stress ECG (TMT)",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.TMT_adm == 0",
          ns = ns,
          dateInput(
            ns("TMT_Date_adm"),
            "",
            value = lubridate::as_date(hold$TMT_Date_adm),
            language = "kr"
          ),
          radioButtons(
            ns("TMT_detail_adm"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$TMT_detail_adm, inline = T
          )
        ),
        radioButtons(
          ns("MSPECT_adm"),
          label = "Myocardial SPECT",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.MSPECT_adm == 0",
          ns = ns,
          dateInput(
            ns("MSPECT_Date_adm"),
            "",
            value = lubridate::as_date(hold$MSPECT_Date_adm),
            language = "kr"
          ),
          radioButtons(
            ns("MSPECT_detail_adm"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$MSPECT_detail_adm, inline = T
          )
        ),
        radioButtons(
          ns("APET_adm"),
          label = "Ammonia PET",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.APET_adm == 0",
          ns = ns,
          dateInput(
            ns("APET_Date_adm"),
            "",
            value = lubridate::as_date(hold$APET_Date_adm),
            language = "kr"
          ),
          radioButtons(
            ns("APET_detail_adm"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$APET_detail_adm, inline = T
          )
        ),
        radioButtons(
          ns("DEcho_adm"),
          label = "Dobutamine Echo",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.DEcho_adm == 0",
          ns = ns,
          dateInput(
            ns("DEcho_Date_adm"),
            "",
            value = lubridate::as_date(hold$DEcho_Date_adm),
            language = "kr"
          ),
          radioButtons(
            ns("DEcho_detail_adm"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$DEcho_detail_adm, inline = T
          )
        ),
        radioButtons(
          ns("EEcho_adm"),
          label = "Exercise Stress Echo",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.EEcho_adm == 0",
          ns = ns,
          dateInput(
            ns("EEcho_Date_adm"),
            "",
            value = lubridate::as_date(hold$EEcho_Date_adm),
            language = "kr"
          ),
          radioButtons(
            ns("EEcho_detail_adm"),
            "",
            choices = c("Positive" = 0, "Negative" = 1, "Equivocal" = 2, "Incomplete (Inadequate)" = 3),
            selected = hold$EEcho_detail_adm, inline = T
          )
        ),

        # Coronary CT Angiography

        radioButtons(
          ns("Corona_adm"),
          label = "Coronary CT Angiography",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.Corona_adm == 0",
          ns = ns,
          dateInput(
            ns("Corona_date_adm"),
            "",
            value = lubridate::as_date(hold$Corona_date_adm),
            language = "kr"
          ),
          radioButtons(
            ns("Corona_BB_adm"),
            "Beta Blocker Before cCTA",
            choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
            selected = hold$Corona_BB_adm, inline = T
          ),
          radioButtons(
            ns("Corona_Nitrate_adm"),
            "Nitrate Before cCTA",
            choices = c("Yes" = 0, "No" = 1, "Unknown" = 2),
            selected = hold$Corona_Nitrate_adm, inline = T
          )
        ),

        # EchoCG

        radioButtons(
          ns("EchoCG_adm"),
          label = "EchoCG",
          choices = c("Yes" = 0, "No" = 1),
          inline = TRUE
        ),
        conditionalPanel(
          "input.EchoCG_adm == 0",
          ns = ns,
          dateInput(
            ns("EchoCG_date_adm"),
            "",
            value = lubridate::as_date(hold$EchoCG_date_adm),
            language = "kr"
          ),
          numericInput(
            ns("EchoCG_LVEF_adm"),
            "LV Ejection Fraction",
            value = ifelse(is.null(hold), NA, hold$EchoCG_LVEF_adm),
            min = 0, max = 120,
            step = 1
          ),
          radioButtons(
            ns("EchoCG_LVWMA_adm"),
            "LV Wall Motion Abnormality",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$EchoCG_LVWMA_adm, inline = T
          )
        ),


        # ECG Rhythm

        radioButtons(
          ns("ECG_Rhythm_adm"),
          label = "ECG Rhythm (pre PCI)",
          choices = c("Sinus Rhythm" = 0, "Atrial Fibrillation" = 1, "Others" = 2, "Not Done" = 3),
          inline = TRUE
        ),
        conditionalPanel(
          "input.ECG_Rhythm_adm == 2",
          ns = ns,
          textAreaInput(
            ns("ECG_Rhythm_others_adm"),
            "Others",
            width = "400px",
            height = "100px",
            value =  ifelse(is.null(hold$ECG_Rhythm_others_adm), "", hold$ECG_Rhythm_others_adm),
          )
        ),

        # Tests Comment

        textAreaInput(
          ns("Tests_comment_adm"),
          "Tests Comment",
          width = "400px",
          height = "100px",
          value =  ifelse(is.null(hold$Tests_comment_adm), "", hold$Tests_comment_adm),
        ),


        # Lab Data

        numericInput(
          ns("WBC_adm"),
          "WBC",
          value = ifelse(is.null(hold), NA, hold$WBC_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TC_adm"),
          "Total Chol",
          value = ifelse(is.null(hold), NA, hold$TC_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Hb_adm"),
          "Hb",
          value = ifelse(is.null(hold), NA, hold$Hb_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("TG_adm"),
          "TG",
          value = ifelse(is.null(hold), NA, hold$TG_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Hct_adm"),
          "Hct",
          value = ifelse(is.null(hold), NA, hold$Hct_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HDL_adm"),
          "HDL",
          value = ifelse(is.null(hold), NA, hold$HDL_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Platelet_adm"),
          "Platelet",
          value = ifelse(is.null(hold), NA, hold$Platelet_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_adm"),
          "LDL",
          value = ifelse(is.null(hold), NA, hold$LDL_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BUN_adm"),
          "BUN",
          value = ifelse(is.null(hold), NA, hold$BUN_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("LDL_Cal_adm"),
          "LDL, cal",
          value = ifelse(is.null(hold), NA, hold$LDL_Cal_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Cr_adm"),
          "Cr",
          value = ifelse(is.null(hold), NA, hold$Cr_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("FG_adm"),
          "Fasting Glucose",
          value = ifelse(is.null(hold), NA, hold$FG_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("MDRD_adm"),
          "MDRD-eGFR",
          value = ifelse(is.null(hold), NA, hold$MDRD_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("HbA1C_adm"),
          "HbA1C",
          value = ifelse(is.null(hold), NA, hold$HbA1C_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("AST_adm"),
          "AST",
          value = ifelse(is.null(hold), NA, hold$AST_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("hs_CRP_adm"),
          "hs-CRP",
          value = ifelse(is.null(hold), NA, hold$hs_CRP_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("ALT_adm"),
          "ALT",
          value = ifelse(is.null(hold), NA, hold$ALT_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("BNP_adm"),
          "BNP",
          value = ifelse(is.null(hold), NA, hold$BNP_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("aPTT_adm"),
          "aPTT",
          value = ifelse(is.null(hold), NA, hold$aPTT_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("Pro_BNP_adm"),
          "pro BNP",
          value = ifelse(is.null(hold), NA, hold$Pro_BNP_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("PT_INR_adm"),
          "PT_INR",
          value = ifelse(is.null(hold), NA, hold$PT_INR_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("NT_Pro_BNP_adm"),
          "NT-Pro BNP",
          value = ifelse(is.null(hold), NA, hold$NT_Pro_BNP_adm),
          min = 0, max = 120,
          step = 1
        ),
        radioButtons(
          ns("hCG_adm"),
          label = "hCG",
          choices = c("Positive" = 0, "Negative" = 1, "NA" = 2),
          inline = TRUE
        ),

        # Baseline (Peak pre-intervention)

        p("Baseline (Peak pre-Intervention)"),
        p("퇴원전 Peak 값"),
        p("CK (IU/L)"),
        numericInput(
          ns("BCK_adm"),
          "Baseline",
          value = ifelse(is.null(hold), NA, hold$BCK_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("PCK_adm"),
          "Peak 값",
          value = ifelse(is.null(hold), NA, hold$PCK_adm),
          min = 0, max = 120,
          step = 1
        ),
        p("CK-MB (ng/mL)"),
        numericInput(
          ns("BCKMB_adm"),
          "Baseline",
          value = ifelse(is.null(hold), NA, hold$BCKMB_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("PCKMB_adm"),
          "Peak 값",
          value = ifelse(is.null(hold), NA, hold$PCKMB_adm),
          min = 0, max = 120,
          step = 1
        ),
        p("Troponin I (ng/mL)"),
        numericInput(
          ns("BTroI_adm"),
          "Baseline",
          value = ifelse(is.null(hold), NA, hold$BTroI_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("PTroI_adm"),
          "Peak 값",
          value = ifelse(is.null(hold), NA, hold$PTroI_adm),
          min = 0, max = 120,
          step = 1
        ),
        p("Troponin T (ng/mL)"),
        numericInput(
          ns("BTroT_adm"),
          "Baseline",
          value = ifelse(is.null(hold), NA, hold$BTroT_adm),
          min = 0, max = 120,
          step = 1
        ),
        numericInput(
          ns("PTroT_adm"),
          "Peak 값",
          value = ifelse(is.null(hold), NA, hold$PTroT_adm),
          min = 0, max = 120,
          step = 1
        ),

        # Lab Comment

        textAreaInput(
          ns("Lab_comment_adm"),
          "Lab Comment",
          width = "400px",
          height = "100px",
          value =  ifelse(is.null(hold$Lab_comment_adm), "", hold$Lab_comment_adm),
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
        "Diagnosis_adm" = ifelse(is.null(input$Diagnosis_adm), "", input$Diagnosis_adm),
        "CCS_adm" = ifelse(is.null(input$CCS_adm), "", input$CCS_adm),
        "BC_adm" = ifelse(is.null(input$BC_adm), "", input$BC_adm),
        "Diagnosis_other_adm" = ifelse(is.null(input$Diagnosis_other_adm), "", input$Diagnosis_other_adm),
        "Diagnosis_comment_adm" = ifelse(is.null(input$Diagnosis_comment_adm), "", input$Diagnosis_comment_adm),
        "HTN" = ifelse(is.null(input$HTN), "", input$HTN),
        "Diabetes_adm" = ifelse(is.null(input$Diabetes_adm), "", input$Diabetes_adm),
        "Diabetes_detail_adm" = ifelse(is.null(input$Diabetes_detail_adm), "", input$Diabetes_detail_adm),
        "Hld_adm" = ifelse(is.null(input$Hld_adm), "", input$Hld_adm),
        "Smoking" = ifelse(is.null(input$Smoking), "", input$Smoking),
        "FHC_adm" = ifelse(is.null(input$FHC_adm), "", input$FHC_adm),
        "CRF_adm" = ifelse(is.null(input$CRF_adm), "", input$CRF_adm),
        "PP_adm" = ifelse(is.null(input$PP_adm), "", input$PP_adm),
        "PM_adm" = ifelse(is.null(input$PM_adm), "", input$PM_adm),
        "PCABG_adm" = ifelse(is.null(input$PCABG_adm), "", input$PCABG_adm),
        "PCVA_adm" = ifelse(is.null(input$PCVA_adm), "", input$PCVA_adm),
        "PCHF_adm" = ifelse(is.null(input$PCHF_adm), "", input$PCHF_adm),
        "Pvd_adm" = ifelse(is.null(input$Pvd_adm), "", input$Pvd_adm),
        "PNTB_adm" = ifelse(is.null(input$PNTB_adm), "", input$PNTB_adm),
        "Risk_comment_adm" = ifelse(is.null(input$Risk_comment_adm), "", input$Risk_comment_adm),
        "TMT_adm" = ifelse(is.null(input$TMT_adm), "", input$TMT_adm),
        "TMT_detail_adm" = ifelse(is.null(input$TMT_detail_adm), "", input$TMT_detail_adm),
        "MSPECT_adm" = ifelse(is.null(input$MSPECT_adm), "", input$MSPECT_adm),
        "MSPECT_detail_adm" = ifelse(is.null(input$MSPECT_detail_adm), "", input$MSPECT_detail_adm),
        "APET_adm" = ifelse(is.null(input$APET_adm), "", input$APET_adm),
        "APET_detail_adm" = ifelse(is.null(input$APET_detail_adm), "", input$APET_detail_adm),
        "DEcho_adm" = ifelse(is.null(input$DEcho_adm), "", input$DEcho_adm),
        "DEcho_detail_adm" = ifelse(is.null(input$DEcho_detail_adm), "", input$DEcho_detail_adm),
        "EEcho_adm" = ifelse(is.null(input$EEcho_adm), "", input$EEcho_adm),
        "EEcho_detail_adm" = ifelse(is.null(input$EEcho_detail_adm), "", input$EEcho_detail_adm),
        "Corona_adm" = ifelse(is.null(input$Corona_adm), "", input$Corona_adm),
        "Corona_BB_adm" = ifelse(is.null(input$Corona_BB_adm), "", input$Corona_BB_adm),
        "Corona_Nitrate_adm" = ifelse(is.null(input$Corona_Nitrate_adm), "", input$Corona_Nitrate_adm),
        "EchoCG_adm" = ifelse(is.null(input$EchoCG_adm), "", input$EchoCG_adm),
        "EchoCG_LVEF_adm" = ifelse(is.null(input$EchoCG_LVEF_adm), "", input$EchoCG_LVEF_adm),
        "EchoCG_LVWMA_adm" = ifelse(is.null(input$EchoCG_LVWMA_adm), "", input$EchoCG_LVWMA_adm),
        "ECG_Rhythm_adm" = ifelse(is.null(input$ECG_Rhythm_adm), "", input$ECG_Rhythm_adm),
        "ECG_Rhythm_others_adm" = ifelse(is.null(input$ECG_Rhythm_others_adm), "", input$ECG_Rhythm_others_adm),
        "Tests_comment_adm" = ifelse(is.null(input$Tests_comment_adm), "", input$Tests_comment_adm),
        "hCG_adm" = ifelse(is.null(input$hCG_adm), "", input$hCG_adm),
        "Lab_comment_adm" = ifelse(is.null(input$Lab_comment_adm), "", input$Lab_comment_adm),
        "Aspirin_adm" = ifelse(is.null(input$Aspirin_adm), "", input$Aspirin_adm),
        "Clopidogrel_adm" = ifelse(is.null(input$Clopidogrel_adm), "", input$Clopidogrel_adm),
        "Prasugrel_adm" = ifelse(is.null(input$Prasugrel_adm), "", input$Prasugrel_adm),
        "Ticagrelor_adm" = ifelse(is.null(input$Ticagrelor_adm), "", input$Ticagrelor_adm),
        "BB_adm" = ifelse(is.null(input$BB_adm), "", input$BB_adm),
        "WN_adm" = ifelse(is.null(input$WN_adm), "", input$WN_adm),
        "Statin_adm" = ifelse(is.null(input$Statin_adm), "", input$Statin_adm),
        "ACE_adm" = ifelse(is.null(input$ACE_adm), "", input$ACE_adm),
        "Nitrate_adm" = ifelse(is.null(input$Nitrate_adm), "", input$Nitrate_adm),
        "Calcium_adm" = ifelse(is.null(input$Calcium_adm), "", input$Calcium_adm),
        "Trimetazidine_adm" = ifelse(is.null(input$Trimetazidine_adm), "", input$Trimetazidine_adm),
        "Medication_comment_adm" = ifelse(is.null(input$Medication_comment_adm), "", input$Medication_comment_adm),
        "TMT_Date_adm" = ifelse(is.null(input$TMT_Date_adm), Sys.Date(), input$TMT_Date_adm),
        "MSPECT_Date_adm" = ifelse(is.null(input$MSPECT_Date_adm), Sys.Date(), input$MSPECT_Date_adm),
        "APET_Date_adm" = ifelse(is.null(input$APET_Date_adm), Sys.Date(), input$APET_Date_adm),
        "DEcho_Date_adm" = ifelse(is.null(input$DEcho_Date_adm), Sys.Date(), input$DEcho_Date_adm),
        "EEcho_Date_adm" = ifelse(is.null(input$EEcho_Date_adm), Sys.Date(), input$EEcho_Date_adm),
        "Corona_date_adm" = ifelse(is.null(input$Corona_date_adm), Sys.Date(), input$Corona_date_adm),
        "EchoCG_date_adm" = ifelse(is.null(input$EchoCG_date_adm), Sys.Date(), input$EchoCG_date_adm),
        "WBC_adm" = ifelse(is.null(input$WBC_adm), "", input$WBC_adm),
        "TC_adm" = ifelse(is.null(input$TC_adm), "", input$TC_adm),
        "Hb_adm" = ifelse(is.null(input$Hb_adm), "", input$Hb_adm),
        "TG_adm" = ifelse(is.null(input$TG_adm), "", input$TG_adm),
        "Hct_adm" = ifelse(is.null(input$Hct_adm), "", input$Hct_adm),
        "HDL_adm" = ifelse(is.null(input$HDL_adm), "", input$HDL_adm),
        "Platelet_adm" = ifelse(is.null(input$Platelet_adm), "", input$Platelet_adm),
        "LDL_adm" = ifelse(is.null(input$LDL_adm), "", input$LDL_adm),
        "BUN_adm" = ifelse(is.null(input$BUN_adm), "", input$BUN_adm),
        "LDL_Cal_adm" = ifelse(is.null(input$LDL_Cal_adm), "", input$LDL_Cal_adm),
        "Cr_adm" = ifelse(is.null(input$Cr_adm), "", input$Cr_adm),
        "FG_adm" = ifelse(is.null(input$FG_adm), "", input$FG_adm),
        "MDRD_adm" = ifelse(is.null(input$MDRD_adm), "", input$MDRD_adm),
        "HbA1C_adm" = ifelse(is.null(input$HbA1C_adm), "", input$HbA1C_adm),
        "AST_adm" = ifelse(is.null(input$AST_adm), "", input$AST_adm),
        "hs_CRP_adm" = ifelse(is.null(input$hs_CRP_adm), "", input$hs_CRP_adm),
        "ALT_adm" = ifelse(is.null(input$ALT_adm), "", input$ALT_adm),
        "BNP_adm" = ifelse(is.null(input$BNP_adm), "", input$BNP_adm),
        "aPTT_adm" = ifelse(is.null(input$aPTT_adm), "", input$aPTT_adm),
        "Pro_BNP_adm" = ifelse(is.null(input$Pro_BNP_adm), "", input$Pro_BNP_adm),
        "PT_INR_adm" = ifelse(is.null(input$PT_INR_adm), "", input$PT_INR_adm),
        "NT_Pro_BNP_adm" = ifelse(is.null(input$NT_Pro_BNP_adm), "", input$NT_Pro_BNP_adm),
        "BCK_adm" = ifelse(is.null(input$BCK_adm), "", input$BCK_adm),
        "PCK_adm" = ifelse(is.null(input$PCK_adm), "", input$PCK_adm),
        "BCKMB_adm" = ifelse(is.null(input$BCKMB_adm), "", input$BCKMB_adm),
        "PCKMB_adm" = ifelse(is.null(input$PCKMB_adm), "", input$PCKMB_adm),
        "BTroI_adm" = ifelse(is.null(input$BTroI_adm), "", input$BTroI_adm),
        "PTroI_adm" = ifelse(is.null(input$PTroI_adm), "", input$PTroI_adm),
        "BTroT_adm" = ifelse(is.null(input$BTroT_adm), "", input$BTroT_adm),
        "PTroT_adm" = ifelse(is.null(input$PTroT_adm), "", input$PTroT_adm)
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
