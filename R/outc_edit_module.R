#' discharge Add & Edit Module
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
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Discharge information",
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
            dateInput(
              ns("Discharge_out"),
              "Date of Discharge",
              value = lubridate::as_date(hold$Discharge_out),
              language = "ko"
            )
          ),
          column(
            width = 6,
            radioButtons(
              ns("General_out"),
              label = "General Complication",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE,
              selected = ifelse(is.null(hold$General_out), character(0), hold$General_out)
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
          )
        ),
        conditionalPanel(
          "input.General_out == 0",
          ns = ns,
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
            selected = strsplit(ifelse(is.null(hold$General_detail_out), character(0), hold$General_detail_out), ',')[[1]],
            inline = TRUE
          ),
          conditionalPanel(
            "input.General_detail_out == 14",
            ns = ns,
            textInput(
              "General_detail_others_out",
              label = "Detail",
              value = ifelse(is.null(hold), "", hold$General_detail_others_out)
            ),
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
                ns("submit1"),
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
              ns("Aspirin_out"),
              label = "Aspirin",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Aspirin_out,
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Trimetazidine_out"),
              label = "Trimetazidine (Vastinan)",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Trimetazidine_out,
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Clopidogrel_out"),
              label = "Clopidogrel",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Clopidogrel_out,
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Nitrate_out"),
              label = "Nitrate",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Nitrate_out,
              inline = T
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("Prasugrel_out"),
              label = "Prasugrel",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Prasugrel_out,
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Statin_out"),
              label = "Statin",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Statin_out,
              inline = T
            ),
            conditionalPanel(
              "input.Statin_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Statin_name_out"),
                    label = "Name",
                    value = hold$Statin_name_out
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Statin_dose_out"),
                    label = "Dose",
                    value = hold$Statin_dose_out,
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
              ns("Ticagrelor_out"),
              label = "Ticagrelor",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Ticagrelor_out,
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Wafarin_out"),
              label = "Wafarin",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Wafarin_out,
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("Nicorandil_out"),
              label = "Nicorandil",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Nicorandil_out,
              inline = T
            )
          ),
          column(
            width = 3,
            radioButtons(
              ns("NOAC_out"),
              label = "NOAC",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$NOAC_out,
              inline = T
            ),
            conditionalPanel(
              "input.NOAC_out == 0",
              ns = ns,
              textInput(
                ns("NOAC_name_out"),
                label = "Name",
                value = hold$NOAC_name_out
              ),
              numericInput(
                ns("NOAC_dose_out"),
                label = "Dose",
                value = hold$NOAC_dose_out,
                min = 0,
                max = 200,
                step = 1
              )
            )
          ),
          column(
            width = 3,
            radioButtons( # Beta Blocker
              ns("BB_out"),
              label = "Beta Blocker",
              choices = c("Yes" = 0, "No" = 1),
              selected = ifelse(is.null(hold$BB_out), character(0), hold$BB_out),
              inline = T
            ),
            conditionalPanel(
              "input.BB_out == 0",
              ns = ns,
              textInput(
                ns("BB_name_out"),
                label = "Name",
                value = hold$BB_name_out
              ),
              numericInput(
                ns("BB_dose_out"),
                label = "Dose",
                value = hold$BB_dose_out,
                min = 0,
                max = 200,
                step = 1
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("ACE_out"),
              label = "ACE Inhibitor or ARB",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$ACE_out,
              inline = T
            ),
            conditionalPanel(
              "input.ACE_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("ACE_name_out"),
                    label = "Name",
                    value = hold$ACE_name_out
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("ACE_dose_out"),
                    label = "Dose",
                    value = hold$ACE_dose_out,
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
              ns("DPP4_out"),
              label = "DPP4 Inhibitor",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$DPP4_out,
              inline = T
            ),
            conditionalPanel(
              "input.DPP4_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("DPP4_name_out"),
                    label = "Name",
                    value = hold$DPP4_name_out
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("DPP4_dose_out"),
                    label = "Dose",
                    value = hold$DPP4_dose_out,
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
              ns("Calcium_out"),
              label = "Calcium channel blocker",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Calcium_out,
              inline = T
            ),
            conditionalPanel(
              "input.Calcium_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Calcium_name_out"),
                    label = "Name",
                    value = hold$Calcium_name_out
                  ) 
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Calcium_dose_out"),
                    label = "Dose",
                    value = hold$Calcium_dose_out,
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
              ns("Metformin_out"),
              label = "Metformin",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Metformin_out,
              inline = T
            ),
            conditionalPanel(
              "input.Metformin_out == 0",
              ns = ns,
              numericInput(
                ns("Metformin_dose_out"),
                label = "Dose",
                value = hold$Metformin_dose_out,
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
              ns("Sulf_out"),
              label = "Sulfonylurea",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Sulf_out,
              inline = T
            ),
            conditionalPanel(
              "input.Sulf_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Sulf_name_out"),
                    label = "Name",
                    value = hold$Sulf_name_out
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Sulf_dose_out"),
                    label = "Dose",
                    value = hold$Sulf_dose_out,
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
              ns("Thia_out"),
              label = "Thiazolidinedione",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Thia_out,
              inline = T
            ),
            conditionalPanel(
              "input.Thia_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Thia_name_out"),
                    label = "Name",
                    value = hold$Thia_name_out
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Thia_dose_out"),
                    label = "Dose",
                    value = hold$Thia_dose_out,
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
              ns("GLP_out"),
              label = "GLP-1 Agonist",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$GLP_out,
              inline = T
            ),
            conditionalPanel(
              "input.GLP_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("GLP_name_out"),
                    label = "Name",
                    value = hold$GLP_name_out
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("GLP_dose_out"),
                    label = "Dose",
                    value = hold$GLP_dose_out,
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
              ns("Alpha_out"),
              label = "Alpha-glucosidase inhibitor",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Alpha_out,
              inline = T
            ),
            conditionalPanel(
              "input.Alpha_out == 0",
              ns = ns,
              fluidRow(
                column(
                  width = 6,
                  textInput(
                    ns("Alpha_name_out"),
                    label = "Name",
                    value = hold$Alpha_name_out
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    ns("Alpha_dose_out"),
                    label = "Dose",
                    value = hold$Alpha_dose_out,
                    min = 0,
                    max = 200,
                    step = 1
                  )
                )
              )
            )
          ),
        ),
        fluidRow(
          column(
            width = 3,
            radioButtons(
              ns("Insulin_out"),
              label = "Insulin",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Insulin_out,
              inline = T
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
            width = 4,
            radioButtons(
              ns("Events_out"),
              label = "Clinical Events",
              choices = c("Yes" = 0, "No" = 1),
              inline = TRUE,
              selected = ifelse(is.null(hold$Events_out), character(0), hold$Events_out)
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.Events_out == 0 ",
              ns = ns,
              checkboxGroupInput(
                ns("Events_detail_out"),
                label = "Detail",
                choices = c("Death" = 0, "MI" = 1, "Repeat Revascularization" = 2, "Stent Thrombosis" = 3, "CVA" = 4, "Bleeding" = 5),
                selected = strsplit(ifelse(is.null(hold$Events_detail_out), character(0), hold$Events_detail_out), ',')[[1]],
                inline = TRUE
              )
            )
          )
        ),
        conditionalPanel(
          'input.Events_detail_out.includes("0")',
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
              width = 4,
              dateInput(
                ns("Death_date_out"),
                "Date",
                value = hold$Death_date_out,
                language = "ko"
              )
            ),
            column(
              width = 8,
              radioButtons(
                ns("Death_cause_out"),
                "Cause",
                choices = c("Cardiac Death" = 0, "Non-Cardiovascular Death" = 1, "Unknown Origin Death" = 2),
                selected = ifelse(is.null(hold$Death_cause_out), character(0),hold$Death_cause_out),
                inline = TRUE
              )
            )
          )
        ),


        # Myocardial Infarction

        conditionalPanel(
          'input.Events_detail_out.includes("1")',
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Myocardial Infarction",
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
              width = 3,
              dateInput(
                ns("MI_date_out"),
                "Date",
                value = hold$MI_date_out,
                language = "ko"
              )
            ),
            column(
              width = 3,
              textInput(
                ns("MI_Segment_out"),
                label = "Segment",
                value = ifelse(is.null(hold$MI_Segment_out), '', hold$MI_Segment_out)
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("MI_Type_out"),
                "Type",
                choices = c("STEMI" = 0, "NSTEMI" = 1),
                selected = ifelse(is.null(hold$MI_Type_out),character(0),hold$MI_Type_out),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("MI_Pre_out"),
                "Pre Procedural MI",
                choices = c("Yes" = 0, "No" = 1),
                selected = ifelse(is.null(hold$MI_Pre_out), character(0),hold$MI_Pre_out),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              radioButtons(
                ns("MI_ST_out"),
                "Related with Stent Thrombosis",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                selected = ifelse(is.null(hold$MI_ST_out), character(0),hold$MI_ST_out),
                inline = TRUE
              )
            ),
            column(
              width = 4,
              radioButtons(
                ns("MI_TL_out"),
                "Related with Target Lesion",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                selected = ifelse(is.null(hold$MI_TL_out), character(0),hold$MI_TL_out),
                inline = TRUE
              )
            ),
            column(
              width = 4,
              radioButtons(
                ns("MI_TV_out"),
                "Related with Target Vessel",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                selected = ifelse(is.null(hold$MI_TV_out), character(0),hold$MI_TV_out),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              checkboxGroupInput(
                ns("MI_Treat_out"),
                label = "Type of Treatment",
                choices = c("Medication Only" = 0, "Thrombolysis" = 1, "only Ballooning" = 2, "Stenting" = 3, "Bypass Surgery" = 4),
                selected = strsplit(ifelse(is.null(hold$MI_Treat_out), character(0), hold$MI_Treat_out), ',')[[1]],
                inline = TRUE
              ),
            ),
            column(
              width = 4,
              radioButtons(
                ns("MI_After_out"),
                "After Treatment",
                choices = c("Recovered" = 0, "Death" = 1, "Unknown" = 2),
                selected = ifelse(is.null(hold$MI_After_out), character(0),hold$MI_After_out),
                inline = TRUE
              )
            )
          )
        ),
        conditionalPanel(
          'input.Events_detail_out.includes("2")',
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
              width = 3,
              dateInput(
                ns("RV_date_out"),
                "Date",
                value = hold$RV_date_out,
                language = "ko"
              )
            ),
            column(
              width = 3,
              textInput(
                ns("RV_Segment_out"),
                label = "Segment",
                value = ifelse(is.null(hold$RV_Segment_out), "", hold$RV_Segment_out)
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("RV_CD_out"),
                "Clinically Driven",
                choices = c("Yes" = 0, "No" = 1),
                selected = ifelse(is.null(hold$RV_CD_out), "", hold$RV_CD_out),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("RV_ID_out"),
                "Ischemia Driven",
                choices = c("Yes" = 0, "No" = 1),
                selected = ifelse(is.null(hold$RV_ID_out), "", hold$RV_ID_out),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              checkboxGroupInput(
                ns("RV_Treat_out"),
                label = "Type of Treatment",
                choices = c("only Ballooning" = 0, "Stenting" = 1, "Bypass Surgery" = 2),
                selected = strsplit(ifelse(is.null(hold$RV_Treat_out), character(0), hold$RV_Treat_out), ',')[[1]],
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("RV_TL_out"),
                "Related with Target Lesion",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                selected = ifelse(is.null(hold$RV_TL_out), "", hold$RV_TL_out),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("RV_TV_out"),
                "Related with Target Vessel",
                choices = c("Unknown" = 0, "No" = 1, "Yes" = 2),
                selected = ifelse(is.null(hold$RV_TV_out), "", hold$RV_TV_out),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("RV_AVP_out"),
                "Another Vessel PCI",
                choices = c("Yes" = 0, "No" = 1),
                selected = ifelse(is.null(hold$RV_AVP_out), "", hold$RV_AVP_out),
                inline = TRUE
              )
            )
          )
        ),
        conditionalPanel(
          'input.Events_detail_out.includes("3")',
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
              dateInput(
                ns("ST_date_out"),
                "Date",
                value = hold$ST_date_out,
                language = "ko"
              )
            ),
            column(
              width = 2,
              textInput(
                ns("ST_Segment_out"),
                label = "Segment",
                value = ifelse(is.null(hold$ST_Segment_out), "", hold$ST_Segment_out)
              )
            ),
            column(
              width = 8,
              radioButtons(
                ns("ST_Type_out"),
                "Type",
                choices = c("Acute (< 1d)" = 0, "Subacute (1-30d)" = 1, "Late (>1m)" = 2, "Very Late(>1y)" = 3),
                selected = ifelse(is.null(hold$ST_Type_out), "", hold$ST_Type_out),
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              radioButtons(
                ns("ST_ARC_out"),
                "ARC",
                choices = c("Definite/Confirmed" = 0, "Probable" = 1, "Possible" = 2),
                selected = ifelse(is.null(hold$ST_ARC_out), "", hold$ST_ARC_out),
                inline = TRUE
              )
            ),
            column(
              width = 8,
              checkboxGroupInput(
                ns("ST_Clinical_out"),
                label = "Clinical Features",
                choices = c("Sudden Death" = 0, "STEMI" = 1, "NSTEMI" = 2, "Unstable Angina" = 3, "Stable Angina" = 4, "Other" = 5),
                selected = strsplit(ifelse(is.null(hold$ST_Clinical_out), character(0), hold$ST_Clinical_out), ',')[[1]],
                inline = TRUE
              ),
              conditionalPanel(
                "input.ST_Clinical_out == 5",
                ns = ns,
                textInput(
                  ns("ST_Clinical_other_out"),
                  label = "Detail",
                  value = ifelse(is.null(hold$ST_Clinical_other_out), "", hold$ST_Clinical_other_out)
                )
              )
            )
          )
        ),
        conditionalPanel(
          'input.Events_detail_out.includes("4")',
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
              width = 2,
              dateInput(
                ns("CVA_date_out"),
                "Date",
                value = hold$CVA_date_out,
                language = "ko"
              )
            ),
            column(
              width = 5,
              radioButtons(
                ns("CVA_Type_out"),
                "Type",
                choices = c("Ischemic" = 0, "Hemorrhagic" = 1, "Unknown" = 2),
                selected = ifelse(is.null(hold$CVA_Type_out), character(0), hold$CVA_Type_out),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("CVA_VIS_out"),
                "Verified with Imaging Studies",
                choices = c("Yes" = 0, "No" = 1),
                selected = ifelse(is.null(hold$CVA_VIS_out), character(0), hold$CVA_VIS_out),
                inline = TRUE
              )
            )
          )
        ),
        
        # Bleeding
        conditionalPanel(
          'input.Events_detail_out.includes("5")',
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
              width = 2, 
              numericInput(
                ns('Bleed_count_out'),
                "Count",
                value = ifelse(is.null(hold$Bleed_count_out), 0, hold$Bleed_count_out),
                min = 1, 
                max = 2
              )
            ),
            
            conditionalPanel(
              "input.Bleed_count_out >= 1",
              ns = ns,
              fluidRow(
                column(
                  width = 2,
                  dateInput(
                    ns("Bleed1_date_out"),
                    "Date",
                    value = lubridate::as_date(hold$Bleed1_date_out),
                    language = "ko"
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_Type_out"),
                    "BARC Type",
                    choices = c("BARC 2" = 0, "BARC 3" = 1, "BARC 5" = 2),
                    selected = ifelse(is.null(hold$BARC1_Type_out), character(0), hold$BARC1_Type_out),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_ST_out"),
                    "Spontaneous or Traumatic",
                    choices = c("Spontaneous" = 0, "Traumatic" = 1),
                    selected = ifelse(is.null(hold$BARC1_ST_out), character(0), hold$BARC1_ST_out),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_HT_out"),
                    "Requiring hospitalization or transfusion",
                    choices = c("Yes" = 0, "No" = 1),
                    selected = ifelse(is.null(hold$BARC1_HT_out), character(0), hold$BARC1_HT_out),
                    inline = TRUE
                  )
                ),
                column(
                  width = 2,
                  textInput(
                    ns("BARC1_Origin_out"),
                    label = "Origin of Bleeding	",
                    value = ifelse(is.null(hold$BARC1_Origin_out), "", hold$BARC1_Origin_out)
                  )
                ),
                column(
                  width = 2,
                  radioButtons(
                    ns("BARC1_AT_out"),
                    "After Treatment",
                    choices = c("Recovered" = 0, "Death" = 1),
                    selected = ifelse(is.null(hold$BARC1_AT_out), character(0), hold$BARC1_AT_out),
                    inline = TRUE
                  )
                )
              )
            ),
            
            conditionalPanel(
            "input.Bleed_count_out >= 2",
            ns = ns,
            fluidRow(
              column(
                width = 2,
                dateInput(
                  ns("Bleed2_date_out"),
                  "Date",
                  value = lubridate::as_date(hold$Bleed2_date_out),
                  language = "ko"
                )
              ),
              column(
                width = 2,
                radioButtons(
                  ns("BARC2_Type_out"),
                  "BARC Type",
                  choices = c("BARC 2" = 0, "BARC 3" = 1, "BARC 5" = 2),
                  selected = ifelse(is.null(hold$BARC2_Type_out), character(0), hold$BARC2_Type_out),
                  inline = TRUE
                )
              ),
              column(
                width = 2,
                radioButtons(
                  ns("BARC2_ST_out"),
                  "Spontaneous or Traumatic",
                  choices = c("Spontaneous" = 0, "Traumatic" = 1),
                  selected = ifelse(is.null(hold$BARC2_ST_out), character(0), hold$BARC2_ST_out),
                  inline = TRUE
                )
              ),
              column(
                width = 2,
                radioButtons(
                  ns("BARC2_HT_out"),
                  "Requiring hospitalization or transfusion",
                  choices = c("Yes" = 0, "No" = 1),
                  selected = ifelse(is.null(hold$BARC2_HT_out), character(0), hold$BARC2_HT_out),
                  inline = TRUE
                )
              ),
              column(
                width = 2,
                textInput(
                  ns("BARC2_Origin_out"),
                  label = "Origin of Bleeding	",
                  value = ifelse(is.null(hold$BARC2_Origin_out), "", hold$BARC2_Origin_out)
                )
              ),
              column(
                width = 2,
                radioButtons(
                  ns("BARC2_AT_out"),
                  "After Treatment",
                  choices = c("Recovered" = 0, "Death" = 1),
                  selected = ifelse(is.null(hold$BARC2_AT_out), character(0), hold$BARC2_AT_out),
                  inline = TRUE
                )
              )
            )
          ),
            
            
          )
        ),
        
        # Any AE / SAE ...
        # textAreaInput(
        #   ns("Comment_out"),
        #   "Comment",
        #   width = "400px",
        #   height = "100px",
        #   value =  ifelse(is.null(hold$Comment_out), "", hold$Comment_out),
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

  edit_car_dat <- reactive({
    hold <- car_to_edit()

    out <- list(
      data = list(
        
        "Discharge_out" = ifelse(is.null(input$Discharge_out), "", lubridate::as_date(input$Discharge_out)),
        "Death_date_out" = ifelse(is.null(input$Death_date_out), "", as.character(input$Death_date_out)),
        "MI_date_out" = ifelse(is.null(input$MI_date_out), "", as.character(input$MI_date_out)),
        "RV_date_out" = ifelse(is.null(input$RV_date_out), "", as.character(input$RV_date_out)),
        "ST_date_out" = ifelse(is.null(input$ST_date_out), "", as.character(input$ST_date_out)),
        "CVA_date_out" = ifelse(is.null(input$CVA_date_out), "", as.character(input$CVA_date_out)),
        "General_out" = ifelse(is.null(input$General_out), "", input$General_out),
        "General_detail_out" = ifelse(is.null(input$General_detail_out),"", paste0(input$General_detail_out, collapse = ',')),
        "General_detail_others_out" = ifelse(is.null(input$General_detail_others_out), "", input$General_detail_others_out),
        
        "Aspirin_out" = ifelse(is.null(input$Aspirin_out), "", input$Aspirin_out),
        "Trimetazidine_out" = ifelse(is.null(input$Trimetazidine_out), "", input$Trimetazidine_out),
        "Clopidogrel_out" = ifelse(is.null(input$Clopidogrel_out), "", input$Clopidogrel_out),
        "Nitrate_out" = ifelse(is.null(input$Nitrate_out), "", input$Nitrate_out),
        "Nicorandil_out" = ifelse(is.null(input$Nicorandil_out), "", input$Nicorandil_out),
        "Prasugrel_out" = ifelse(is.null(input$Prasugrel_out), "", input$Prasugrel_out),
        "Statin_out" = ifelse(is.null(input$Statin_out), "", input$Statin_out),
        "Statin_name_out" = ifelse(is.null(input$Statin_name_out), "", input$Statin_name_out),
        "Statin_dose_out" = ifelse(is.null(input$Statin_dose_out), "", input$Statin_dose_out),
        "Ticagrelor_out" = ifelse(is.null(input$Ticagrelor_out), "", input$Ticagrelor_out),
        'Wafarin_out' = ifelse(is.null(input$Wafarin_out), '', input$Wafarin_out),
        'NOAC_out' = ifelse(is.null(input$NOAC_out), '', input$NOAC_out),
        'NOAC_name_out' = ifelse(is.null(input$NOAC_name_out), '', input$NOAC_name_out),
        'NOAC_dose_out' = ifelse(is.null(input$NOAC_dose_out), '', input$NOAC_dose_out),
        "BB_out" = ifelse(is.null(input$BB_out), "", input$BB_out),
        "BB_name_out" = ifelse(is.null(input$BB_name_out), "", input$BB_name_out),
        "BB_dose_out" = ifelse(is.null(input$BB_dose_out), "", input$BB_dose_out),
        "ACE_out" = ifelse(is.null(input$ACE_out), "", input$ACE_out),
        "ACE_name_out" = ifelse(is.null(input$ACE_name_out), "", input$ACE_name_out),
        "ACE_dose_out" = ifelse(is.null(input$ACE_dose_out), "", input$ACE_dose_out),
        'DPP4_out' = ifelse(is.null(input$DPP4_out), '', input$DPP4_out),
        'DPP4_name_out' = ifelse(is.null(input$DPP4_name_out), '', input$DPP4_name_out),
        'DPP4_dose_out' = ifelse(is.null(input$DPP4_dose_out), '', input$DPP4_dose_out),
        "Calcium_out" = ifelse(is.null(input$Calcium_out), "", input$Calcium_out),
        "Calcium_name_out" = ifelse(is.null(input$Calcium_name_out), "", input$Calcium_name_out),
        "Calcium_dose_out" = ifelse(is.null(input$Calcium_dose_out), "", input$Calcium_dose_out),
        'Metformin_out' = ifelse(is.null(input$Metformin_out), '', input$Metformin_out),
        'Metformin_dose_out' = ifelse(is.null(input$Metformin_dose_out), '', input$Metformin_dose_out),
        'Sulf_out' = ifelse(is.null(input$Sulf_out), '', input$Sulf_out),
        'Sulf_name_out' = ifelse(is.null(input$Sulf_name_out), '', input$Sulf_name_out),
        'Sulf_dose_out' = ifelse(is.null(input$Sulf_dose_out), '', input$Sulf_dose_out),
        'Thia_out' = ifelse(is.null(input$Thia_out), '', input$Thia_out),
        'Thia_name_out' = ifelse(is.null(input$Thia_name_out), '', input$Thia_name_out),
        'Thia_dose_out' = ifelse(is.null(input$Thia_dose_out), '', input$Thia_dose_out),
        'GLP_out' = ifelse(is.null(input$GLP_out), '', input$GLP_out),
        'GLP_name_out' = ifelse(is.null(input$GLP_name_out), '', input$GLP_name_out),
        'GLP_dose_out' = ifelse(is.null(input$GLP_dose_out), '', input$GLP_dose_out),
        'Alpha_out' = ifelse(is.null(input$Alpha_out), '', input$Alpha_out),
        'Alpha_name_out' = ifelse(is.null(input$Alpha_name_out), '', input$Alpha_name_out),
        'Alpha_dose_out' = ifelse(is.null(input$Alpha_dose_out), '', input$Alpha_dose_out),
        'Insulin_out' = ifelse(is.null(input$Insulin_out), '', input$Insulin_out),
        
        "Events_out" = ifelse(is.null(input$Events_out), "", input$Events_out),
        "Events_detail_out" = ifelse(is.null(input$Events_detail_out),"", paste0(input$Events_detail_out, collapse = ',')),
        "Death_cause_out" = ifelse(is.null(input$Death_cause_out), "", input$Death_cause_out),
        "MI_Segment_out" = ifelse(is.null(input$MI_Segment_out), "", input$MI_Segment_out),
        "MI_Type_out" = ifelse(is.null(input$MI_Type_out), "", input$MI_Type_out),
        "MI_Pre_out" = ifelse(is.null(input$MI_Pre_out), "", input$MI_Pre_out),
        "MI_ST_out" = ifelse(is.null(input$MI_ST_out),"", paste0(input$MI_ST_out, collapse = ',')),
        "MI_TL_out" = ifelse(is.null(input$MI_TL_out), "", input$MI_TL_out),
        "MI_TV_out" = ifelse(is.null(input$MI_TV_out), "", input$MI_TV_out),
        "MI_Treat_out" = ifelse(is.null(input$MI_Treat_out),"", paste0(input$MI_Treat_out, collapse = ',')),
        "MI_After_out" = ifelse(is.null(input$MI_After_out), "", input$MI_After_out),
        "RV_Segment_out" = ifelse(is.null(input$RV_Segment_out), "", input$RV_Segment_out),
        "RV_CD_out" = ifelse(is.null(input$RV_CD_out), "", input$RV_CD_out),
        "RV_ID_out" = ifelse(is.null(input$RV_ID_out), "", input$RV_ID_out),
        "RV_Treat_out" = ifelse(is.null(input$RV_Treat_out),"", paste0(input$RV_Treat_out, collapse = ',')),
        "RV_TL_out" = ifelse(is.null(input$RV_TL_out), "", input$RV_TL_out),
        "RV_TV_out" = ifelse(is.null(input$RV_TV_out), "", input$RV_TV_out),
        "RV_AVP_out" = ifelse(is.null(input$RV_AVP_out), "", input$RV_AVP_out),
        "ST_Segment_out" = ifelse(is.null(input$ST_Segment_out), "", input$ST_Segment_out),
        "ST_Type_out" = ifelse(is.null(input$ST_Type_out), "", input$ST_Type_out),
        "ST_ARC_out" = ifelse(is.null(input$ST_ARC_out), "", input$ST_ARC_out),
        "ST_Clinical_out" = ifelse(is.null(input$ST_Clinical_out),"", paste0(input$ST_Clinical_out, collapse = ',')),
        "ST_Clinical_other_out" = ifelse(is.null(input$ST_Clinical_other_out), "", input$ST_Clinical_other_out),
        "CVA_Type_out" = ifelse(is.null(input$CVA_Type_out), "", input$CVA_Type_out),
        "CVA_VIS_out" = ifelse(is.null(input$CVA_VIS_out), "", input$CVA_VIS_out),
        
        "Bleed_count_out" = ifelse(is.null(input$Bleed_count_out), "", input$Bleed_count_out),
        "Bleed1_date_out" = ifelse(is.null(input$Bleed1_date_out), "", lubridate::as_date(input$Bleed1_date_out)),
        "BARC1_Type_out" = ifelse(is.null(input$BARC1_Type_out), "", input$BARC1_Type_out),
        "BARC1_ST_out" = ifelse(is.null(input$BARC1_ST_out), "", input$BARC1_ST_out),
        "BARC1_HT_out" = ifelse(is.null(input$BARC1_HT_out), "", input$BARC1_HT_out),
        "BARC1_Origin_out" = ifelse(is.null(input$BARC1_Origin_out), "", input$BARC1_Origin_out),
        "BARC1_AT_out" = ifelse(is.null(input$BARC1_AT_out), "", input$BARC1_AT_out),
        "Bleed2_date_out" = ifelse(is.null(input$Bleed2_date_out), "", lubridate::as_date(input$Bleed2_date_out)),
        "BARC2_Type_out" = ifelse(is.null(input$BARC2_Type_out), "", input$BARC2_Type_out),
        "BARC2_ST_out" = ifelse(is.null(input$BARC2_ST_out), "", input$BARC2_ST_out),
        "BARC2_HT_out" = ifelse(is.null(input$BARC2_HT_out), "", input$BARC2_HT_out),
        "BARC2_Origin_out" = ifelse(is.null(input$BARC2_Origin_out), "", input$BARC2_Origin_out),
        "BARC2_AT_out" = ifelse(is.null(input$BARC2_AT_out), "", input$BARC2_AT_out),
        
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

  callEdit <- reactive({
    list(
      input$submit,
      input$submit0,
      input$submit1,
      input$submit2,
      input$submit3,
      input$submit4,
      input$submit5,
      input$submit6,
      input$submit7
    )
  })
  
  # Reference : https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
  
  validate_edit <- eventReactive(
    eventExpr = callEdit(),
    valueExpr = {
      if (input$submit0 == 0 && input$submit1 == 0 && input$submit2 == 0 &&
          input$submit3 == 0 && input$submit4 == 0 && input$submit5 == 0 &&
          input$submit6 == 0 && input$submit == 0 && input$submit7 == 0) {
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
