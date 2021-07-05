#' cul2 Add & Edit Module
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
cul2_edit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", data, sessionid) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- car_to_edit()
    
    showModal(
      modalDialog(
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Lesion location",
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
              ns("Lesion_cul2"),
              label = "Lesion",
              choices = c(1:3),
              selected = ifelse(is.null(hold$Lesion_cul2), character(0), hold$Lesion_cul2),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            checkboxGroupInput(
              ns("Vessel_cul2"),
              label = "Vessel",
              choices = c("LM" = "LM", "LAD" = "LAD", "LCx" = "LCx", "RCA" = "RCA"),
              selected = strsplit(ifelse(is.null(hold$Vessel_cul2), character(0), hold$Vessel_cul2), ",")[[1]],
              inline = TRUE
            )
          )
        ),
        conditionalPanel(
          "input.Lesion_cul2 >= 1",
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Lesion",
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
              width = 4,
              textInput(
                ns("Lesion_segment_1_cul2"),
                label = "Segment",
                value = hold$Lesion_segment_1_cul2
              ),
              conditionalPanel(
                "input.Lesion_cul2 >= 2",
                ns = ns,
                textInput(
                  ns("Lesion_segment_2_cul2"),
                  label = "Segment",
                  value = hold$Lesion_segment_2_cul2
                )
              ),
              conditionalPanel(
                "input.Lesion_cul2 >= 3",
                ns = ns,
                textInput(
                  ns("Lesion_segment_3_cul2"),
                  label = "Segment",
                  value = hold$Lesion_segment_3_cul2
                )
              )
            ),
            column(
              width = 4,
              textInput(
                ns("Lesion_VeS_1_cul2"),
                label = "Visually estimated Stenosis(%)",
                value = hold$Lesion_VeS_1_cul2
              ),
              conditionalPanel(
                "input.Lesion_cul2 >= 2",
                ns = ns,
                textInput(
                  ns("Lesion_VeS_2_cul2"),
                  label = "Visually estimated Stenosis(%)",
                  value = hold$Lesion_VeS_2_cul2
                )
              ),
              conditionalPanel(
                "input.Lesion_cul2 >= 3",
                ns = ns,
                textInput(
                  ns("Lesion_VeS_3_cul2"),
                  label = "Visually estimated Stenosis(%)",
                  value = hold$Lesion_VeS_3_cul2
                )
              )
            ),
            column(
              width = 4,
              checkboxGroupInput(
                ns("Lesion_Type_cul2"),
                label = "Lesion Type",
                choices = c(
                  "CTO Lesion" , "True bifurcation lesion ( medina 1,1,1/1,0,1/0,1,1)" , "Long lesion (≥38mm stent)" ,
                  "Unprotected Left Main" , "ISR lesion" , "Calcified lesion", "Not applicable"
                ),
                selected = strsplit(ifelse(is.null(hold$Lesion_Type_cul2), character(0), hold$Lesion_Type_cul2), ",")[[1]]
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "FFR",
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
              ns("FFR_cul2"),
              label = "FFR",
              choices = c("Yes", "No"),
              selected = hold$FFR_cul2,
              inline = TRUE
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              "input.FFR_cul2 == 'Yes'",
              ns = ns,
              radioButtons(
                ns("FFR_pre_cul2"),
                label = "Pre-PCI FFR",
                choices = c("Yes", "No"),
                selected = hold$FFR_pre_cul2,
                inline = TRUE
              )
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              "input.FFR_cul2 == 'Yes'",
              ns = ns,
              radioButtons(
                ns("FFR_post_cul2"),
                label = "Post-PCI FFR",
                choices = c("Yes", "No"),
                selected = hold$FFR_post_cul2,
                inline = TRUE
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Image Device",
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
            radioButtons(
              ns("Image_cul2"),
              label = "Image Device",
              choices = c("IVUS" , "OCT" , "ND" ),
              selected = hold$FFR_cul2,
              inline = TRUE
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              "input.Image_cul2 <= 1",
              ns = ns,
              radioButtons(
                ns("Pre_stent_cul2"),
                label = "Pre-Stenting",
                choices = c("Yes", "No"),
                selected = hold$Pre_stent_cul2,
                inline = TRUE
              )
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              "input.Image_cul2 <= 1",
              ns = ns,
              radioButtons(
                ns("Post_stent_cul2"),
                label = "Post-Stenting",
                choices = c("Yes", "No"),
                selected = hold$Post_stent_cul2,
                inline = TRUE
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Intervention",
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
            width = 4,
            radioButtons(
              ns("Intervention_cul2"),
              label = "Intervention Data",
              choices = c("Yes", "No"),
              selected = hold$Intervention_cul2,
              inline = TRUE
            )
          ),
          column(
            width = 8,
            conditionalPanel(
              "input.Intervention_cul2 == 'Yes'",
              ns = ns,
              radioButtons(
                ns("Intervention_cnt_cul2"),
                label = "개수",
                choices = c(1:10),
                selected = ifelse(is.null(hold$Intervention_cnt_cul2), character(0), hold$Intervention_cnt_cul2),
                inline = TRUE
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cul2 == 'Yes'",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention1_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention1_treat_cul2), character(0), hold$Intervention1_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention1_segment_cul2"),
                label = "Segment",
                value = hold$Intervention1_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention1_name_cul2"),
                label = "Name",
                value = hold$Intervention1_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention1_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention1_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention1_length_cul2"),
                label = "Length",
                value = hold$Intervention1_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention1_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention1_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 2",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention2_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention2_treat_cul2), character(0), hold$Intervention2_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention2_segment_cul2"),
                label = "Segment",
                value = hold$Intervention2_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention2_name_cul2"),
                label = "Name",
                value = hold$Intervention2_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention2_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention2_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention2_length_cul2"),
                label = "Length",
                value = hold$Intervention2_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention2_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention2_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 3",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention3_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention3_treat_cul2), character(0), hold$Intervention3_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention3_segment_cul2"),
                label = "Segment",
                value = hold$Intervention3_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention3_name_cul2"),
                label = "Name",
                value = hold$Intervention3_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention3_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention3_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention3_length_cul2"),
                label = "Length",
                value = hold$Intervention3_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention3_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention3_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 4",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention4_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention4_treat_cul2), character(0), hold$Intervention4_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention4_segment_cul2"),
                label = "Segment",
                value = hold$Intervention4_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention4_name_cul2"),
                label = "Name",
                value = hold$Intervention4_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention4_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention4_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention4_length_cul2"),
                label = "Length",
                value = hold$Intervention4_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention4_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention4_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 5",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention5_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention5_treat_cul2), character(0), hold$Intervention5_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention5_segment_cul2"),
                label = "Segment",
                value = hold$Intervention5_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention5_name_cul2"),
                label = "Name",
                value = hold$Intervention5_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention5_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention5_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention5_length_cul2"),
                label = "Length",
                value = hold$Intervention5_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention5_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention5_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 6",
          ns = ns,
          tags$div(
            HTML(
              paste0(
                '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
                "Intervention 6-",
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
                ns("Intervention6_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention6_treat_cul2), character(0), hold$Intervention6_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention6_segment_cul2"),
                label = "Segment",
                value = hold$Intervention6_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention6_name_cul2"),
                label = "Name",
                value = hold$Intervention6_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention6_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention6_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention6_length_cul2"),
                label = "Length",
                value = hold$Intervention6_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention6_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention6_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 7",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention7_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention7_treat_cul2), character(0), hold$Intervention7_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention7_segment_cul2"),
                label = "Segment",
                value = hold$Intervention7_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention7_name_cul2"),
                label = "Name",
                value = hold$Intervention7_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention7_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention7_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention7_length_cul2"),
                label = "Length",
                value = hold$Intervention7_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention7_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention7_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 8",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention8_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention8_treat_cul2), character(0), hold$Intervention8_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention8_segment_cul2"),
                label = "Segment",
                value = hold$Intervention8_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention8_name_cul2"),
                label = "Name",
                value = hold$Intervention8_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention8_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention8_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention8_length_cul2"),
                label = "Length",
                value = hold$Intervention8_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention8_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention8_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 9",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention9_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention9_treat_cul2), character(0), hold$Intervention9_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention9_segment_cul2"),
                label = "Segment",
                value = hold$Intervention9_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention9_name_cul2"),
                label = "Name",
                value = hold$Intervention9_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention9_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention9_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention9_length_cul2"),
                label = "Length",
                value = hold$Intervention9_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention9_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention9_pressure_cul2
              )
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cnt_cul2 >= 10",
          ns = ns,
          fluidRow(
            column(
              width = 2,
              radioButtons(
                ns("Intervention10_treat_cul2"),
                label = "Treatment",
                choices = c("Ballooning" , "Stenting" ),
                selected = ifelse(is.null(hold$Intervention10_treat_cul2), character(0), hold$Intervention10_treat_cul2)
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention10_segment_cul2"),
                label = "Segment",
                value = hold$Intervention10_segment_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention10_name_cul2"),
                label = "Name",
                value = hold$Intervention10_name_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention10_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention10_diameter_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention10_length_cul2"),
                label = "Length",
                value = hold$Intervention10_length_cul2
              )
            ),
            column(
              width = 2,
              textInput(
                ns("Intervention10_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention10_pressure_cul2
              )
            )
          )
        ),
        tags$div(
          HTML(
            paste0(
              '<h3 style= "background:#3466A1; color:#FFFFFF; padding:0.3em;padding-bottom:0.6em;">',
              "Adjunctive Baloon",
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
            radioButtons(
              ns("Intervention_ab_cul2"),
              label = "Adjunctive Balloon",
              choices = c("Yes", "No"),
              selected = ifelse(is.null(hold), character(0), hold$Intervention_ab_cul2),
              inline = TRUE,
            )
          ),
          conditionalPanel(
            "input.Intervention_ab_cul2 == 'Yes'",
            ns = ns,
            column(
              width = 3,
              textInput(
                ns("Intervention_ab_diameter_cul2"),
                label = "Diameter",
                value = hold$Intervention_ab_diameter_cul2
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Intervention_ab_length_cul2"),
                label = "Length",
                value = hold$Intervention_ab_length_cul2
              )
            ),
            column(
              width = 3,
              textInput(
                ns("Intervention_ab_pressure_cul2"),
                label = "Max Pressure",
                value = hold$Intervention_ab_pressure_cul2
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            radioButtons(
              ns("Intervention_Rota_cul2"),
              label = "Rota-ablation",
              choices = c("Yes", "No"),
              selected = hold$Intervention_Rota_cul2,
              inline = TRUE
            )
          ),
          column(
            width = 4,
            radioButtons(
              ns("Intervention_cd_cul2"),
              label = "Clinical Device Success",
              choices = c("Yes", "No", "NA"),
              selected = hold$Intervention_cd_cul2,
              inline = TRUE
            )
          ),
          column(
            width = 4,
            radioButtons(
              ns("Intervention_cl_cul2"),
              label = "Clinical Lesion Success",
              choices = c("Yes" , "No" , "NA" ),
              selected = hold$Intervention_cl_cul2,
              inline = TRUE
            )
          )
        ),
        conditionalPanel(
          "input.Intervention_cl_cul2 == 'No'",
          ns = ns,
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("Intervention_SRS_cul2"),
                label = "Significant Residual Stenosis",
                choices = c("Yes", "No"),
                selected = hold$Intervention_SRS_cul2,
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Intervention_NR_cul2"),
                label = "No Reflow",
                choices = c("Yes", "No"),
                selected = hold$Intervention_NR_cul2,
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Intervention_D_cul2"),
                label = "Dissection",
                choices = c("Yes", "No"),
                selected = hold$Intervention_D_cul2,
                inline = TRUE
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Intervention_AC_cul2"),
                label = "Acute Closure",
                choices = c("Yes", "No"),
                selected = hold$Intervention_AC_cul2,
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("Intervention_O_cul2"),
                label = "Other",
                choices = c("Yes", "No"),
                selected = ifelse(is.null(hold), character(0), hold$Intervention_O_cul2),
                inline = TRUE
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                "Intervention_O_cul2 == 'Yes'",
                ns = ns,
                textInput(
                  ns("Intervention_O_detail_cul2"),
                  label = "Detail",
                  value = hold$Intervention_O_detail_cul2
                )
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Intervention_CPS_cul2"),
                label = "Clinical Procedureal Success",
                choices = c("Yes" , "No" , "NA" ),
                selected = hold$Intervention_CPS_cul2,
                inline = TRUE
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            radioButtons(
              ns("Trombosuction_cul2"),
              label = "Trombosuction",
              choices = c("Yes", "No"),
              selected = hold$Trombosuction_cul2,
              inline = TRUE
            )
          ),
          column(
            width = 4,
            radioButtons(
              ns("Perforation_cul2"),
              label = "Perforation",
              choices = c("Yes", "No"),
              selected = hold$Perforation_cul2,
              inline = TRUE
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
  
  edit_car_dat <- reactive({
    hold <- car_to_edit()
    
    out <- list(
      data = list(
        "Vessel_cul2" = ifelse(is.null(input$Vessel_cul2), "", paste0(input$Vessel_cul2, collapse = ",")),
        "Lesion_cul2" = ifelse(is.null(input$Lesion_cul2), "", input$Lesion_cul2),
        "Lesion_segment_1_cul2" = ifelse(is.null(input$Lesion_segment_1_cul2), "", input$Lesion_segment_1_cul2),
        "Lesion_VeS_1_cul2" = ifelse(is.null(input$Lesion_VeS_1_cul2), "", input$Lesion_VeS_1_cul2),
        "Lesion_segment_2_cul2" = ifelse(is.null(input$Lesion_segment_2_cul2), "", input$Lesion_segment_2_cul2),
        "Lesion_VeS_2_cul2" = ifelse(is.null(input$Lesion_VeS_2_cul2), "", input$Lesion_VeS_2_cul2),
        "Lesion_segment_3_cul2" = ifelse(is.null(input$Lesion_segment_3_cul2), "", input$Lesion_segment_3_cul2),
        "Lesion_VeS_3_cul2" = ifelse(is.null(input$Lesion_VeS_3_cul2), "", input$Lesion_VeS_3_cul2),
        "Lesion_Type_cul2" = ifelse(is.null(input$Lesion_Type_cul2), "", paste0(input$Lesion_Type_cul2, collapse = ",")),
        "FFR_cul2" = ifelse(is.null(input$FFR_cul2), "", input$FFR_cul2),
        "FFR_pre_cul2" = ifelse(is.null(input$FFR_pre_cul2), "", input$FFR_pre_cul2),
        "FFR_post_cul2" = ifelse(is.null(input$FFR_post_cul2), "", input$FFR_post_cul2),
        "Image_cul2" = ifelse(is.null(input$Image_cul2), "", input$Image_cul2),
        "Pre_stent_cul2" = ifelse(is.null(input$Pre_stent_cul2), "", input$Pre_stent_cul2),
        "Post_stent_cul2" = ifelse(is.null(input$Post_stent_cul2), "", input$Post_stent_cul2),
        "Intervention_cul2" = ifelse(is.null(input$Intervention_cul2), "", input$Intervention_cul2),
        "Intervention_cnt_cul2" = ifelse(is.null(input$Intervention_cnt_cul2), "", input$Intervention_cnt_cul2),
        "Intervention1_treat_cul2" = ifelse(is.null(input$Intervention1_treat_cul2), "", input$Intervention1_treat_cul2),
        "Intervention1_segment_cul2" = ifelse(is.null(input$Intervention1_segment_cul2), "", input$Intervention1_segment_cul2),
        "Intervention1_name_cul2" = ifelse(is.null(input$Intervention1_name_cul2), "", input$Intervention1_name_cul2),
        "Intervention1_diameter_cul2" = ifelse(is.null(input$Intervention1_diameter_cul2), "", input$Intervention1_diameter_cul2),
        "Intervention1_length_cul2" = ifelse(is.null(input$Intervention1_length_cul2), "", input$Intervention1_length_cul2),
        "Intervention1_pressure_cul2" = ifelse(is.null(input$Intervention1_pressure_cul2), "", input$Intervention1_pressure_cul2),
        "Intervention2_treat_cul2" = ifelse(is.null(input$Intervention2_treat_cul2), "", input$Intervention2_treat_cul2),
        "Intervention2_segment_cul2" = ifelse(is.null(input$Intervention2_segment_cul2), "", input$Intervention2_segment_cul2),
        "Intervention2_name_cul2" = ifelse(is.null(input$Intervention2_name_cul2), "", input$Intervention2_name_cul2),
        "Intervention2_diameter_cul2" = ifelse(is.null(input$Intervention2_diameter_cul2), "", input$Intervention2_diameter_cul2),
        "Intervention2_length_cul2" = ifelse(is.null(input$Intervention2_length_cul2), "", input$Intervention2_length_cul2),
        "Intervention2_pressure_cul2" = ifelse(is.null(input$Intervention2_pressure_cul2), "", input$Intervention2_pressure_cul2),
        "Intervention3_treat_cul2" = ifelse(is.null(input$Intervention3_treat_cul2), "", input$Intervention3_treat_cul2),
        "Intervention3_segment_cul2" = ifelse(is.null(input$Intervention3_segment_cul2), "", input$Intervention3_segment_cul2),
        "Intervention3_name_cul2" = ifelse(is.null(input$Intervention3_name_cul2), "", input$Intervention3_name_cul2),
        "Intervention3_diameter_cul2" = ifelse(is.null(input$Intervention3_diameter_cul2), "", input$Intervention3_diameter_cul2),
        "Intervention3_length_cul2" = ifelse(is.null(input$Intervention3_length_cul2), "", input$Intervention3_length_cul2),
        "Intervention3_pressure_cul2" = ifelse(is.null(input$Intervention3_pressure_cul2), "", input$Intervention3_pressure_cul2),
        "Intervention4_treat_cul2" = ifelse(is.null(input$Intervention4_treat_cul2), "", input$Intervention4_treat_cul2),
        "Intervention4_segment_cul2" = ifelse(is.null(input$Intervention4_segment_cul2), "", input$Intervention4_segment_cul2),
        "Intervention4_name_cul2" = ifelse(is.null(input$Intervention4_name_cul2), "", input$Intervention4_name_cul2),
        "Intervention4_diameter_cul2" = ifelse(is.null(input$Intervention4_diameter_cul2), "", input$Intervention4_diameter_cul2),
        "Intervention4_length_cul2" = ifelse(is.null(input$Intervention4_length_cul2), "", input$Intervention4_length_cul2),
        "Intervention4_pressure_cul2" = ifelse(is.null(input$Intervention4_pressure_cul2), "", input$Intervention4_pressure_cul2),
        "Intervention5_treat_cul2" = ifelse(is.null(input$Intervention5_treat_cul2), "", input$Intervention5_treat_cul2),
        "Intervention5_segment_cul2" = ifelse(is.null(input$Intervention5_segment_cul2), "", input$Intervention5_segment_cul2),
        "Intervention5_name_cul2" = ifelse(is.null(input$Intervention5_name_cul2), "", input$Intervention5_name_cul2),
        "Intervention5_diameter_cul2" = ifelse(is.null(input$Intervention5_diameter_cul2), "", input$Intervention5_diameter_cul2),
        "Intervention5_length_cul2" = ifelse(is.null(input$Intervention5_length_cul2), "", input$Intervention5_length_cul2),
        "Intervention5_pressure_cul2" = ifelse(is.null(input$Intervention5_pressure_cul2), "", input$Intervention5_pressure_cul2),
        "Intervention6_treat_cul2" = ifelse(is.null(input$Intervention6_treat_cul2), "", input$Intervention6_treat_cul2),
        "Intervention6_segment_cul2" = ifelse(is.null(input$Intervention6_segment_cul2), "", input$Intervention6_segment_cul2),
        "Intervention6_name_cul2" = ifelse(is.null(input$Intervention6_name_cul2), "", input$Intervention6_name_cul2),
        "Intervention6_diameter_cul2" = ifelse(is.null(input$Intervention6_diameter_cul2), "", input$Intervention6_diameter_cul2),
        "Intervention6_length_cul2" = ifelse(is.null(input$Intervention6_length_cul2), "", input$Intervention6_length_cul2),
        "Intervention6_pressure_cul2" = ifelse(is.null(input$Intervention6_pressure_cul2), "", input$Intervention6_pressure_cul2),
        "Intervention7_treat_cul2" = ifelse(is.null(input$Intervention7_treat_cul2), "", input$Intervention7_treat_cul2),
        "Intervention7_segment_cul2" = ifelse(is.null(input$Intervention7_segment_cul2), "", input$Intervention7_segment_cul2),
        "Intervention7_name_cul2" = ifelse(is.null(input$Intervention7_name_cul2), "", input$Intervention7_name_cul2),
        "Intervention7_diameter_cul2" = ifelse(is.null(input$Intervention7_diameter_cul2), "", input$Intervention7_diameter_cul2),
        "Intervention7_length_cul2" = ifelse(is.null(input$Intervention7_length_cul2), "", input$Intervention7_length_cul2),
        "Intervention7_pressure_cul2" = ifelse(is.null(input$Intervention7_pressure_cul2), "", input$Intervention7_pressure_cul2),
        "Intervention8_treat_cul2" = ifelse(is.null(input$Intervention8_treat_cul2), "", input$Intervention8_treat_cul2),
        "Intervention8_segment_cul2" = ifelse(is.null(input$Intervention8_segment_cul2), "", input$Intervention8_segment_cul2),
        "Intervention8_name_cul2" = ifelse(is.null(input$Intervention8_name_cul2), "", input$Intervention8_name_cul2),
        "Intervention8_diameter_cul2" = ifelse(is.null(input$Intervention8_diameter_cul2), "", input$Intervention8_diameter_cul2),
        "Intervention8_length_cul2" = ifelse(is.null(input$Intervention8_length_cul2), "", input$Intervention8_length_cul2),
        "Intervention8_pressure_cul2" = ifelse(is.null(input$Intervention8_pressure_cul2), "", input$Intervention8_pressure_cul2),
        "Intervention9_treat_cul2" = ifelse(is.null(input$Intervention9_treat_cul2), "", input$Intervention9_treat_cul2),
        "Intervention9_segment_cul2" = ifelse(is.null(input$Intervention9_segment_cul2), "", input$Intervention9_segment_cul2),
        "Intervention9_name_cul2" = ifelse(is.null(input$Intervention9_name_cul2), "", input$Intervention9_name_cul2),
        "Intervention9_diameter_cul2" = ifelse(is.null(input$Intervention9_diameter_cul2), "", input$Intervention9_diameter_cul2),
        "Intervention9_length_cul2" = ifelse(is.null(input$Intervention9_length_cul2), "", input$Intervention9_length_cul2),
        "Intervention9_pressure_cul2" = ifelse(is.null(input$Intervention9_pressure_cul2), "", input$Intervention9_pressure_cul2),
        "Intervention10_treat_cul2" = ifelse(is.null(input$Intervention10_treat_cul2), "", input$Intervention10_treat_cul2),
        "Intervention10_segment_cul2" = ifelse(is.null(input$Intervention10_segment_cul2), "", input$Intervention10_segment_cul2),
        "Intervention10_name_cul2" = ifelse(is.null(input$Intervention10_name_cul2), "", input$Intervention10_name_cul2),
        "Intervention10_diameter_cul2" = ifelse(is.null(input$Intervention10_diameter_cul2), "", input$Intervention10_diameter_cul2),
        "Intervention10_length_cul2" = ifelse(is.null(input$Intervention10_length_cul2), "", input$Intervention10_length_cul2),
        "Intervention10_pressure_cul2" = ifelse(is.null(input$Intervention10_pressure_cul2), "", input$Intervention10_pressure_cul2),
        "Intervention_ab_cul2" = ifelse(is.null(input$Intervention_ab_cul2), "", input$Intervention_ab_cul2),
        "Intervention_ab_diameter_cul2" = ifelse(is.null(input$Intervention_ab_diameter_cul2), "", input$Intervention_ab_diameter_cul2),
        "Intervention_ab_length_cul2" = ifelse(is.null(input$Intervention_ab_length_cul2), "", input$Intervention_ab_length_cul2),
        "Intervention_ab_pressure_cul2" = ifelse(is.null(input$Intervention_ab_pressure_cul2), "", input$Intervention_ab_pressure_cul2),
        "Intervention_Rota_cul2" = ifelse(is.null(input$Intervention_Rota_cul2), "", input$Intervention_Rota_cul2),
        "Intervention_cd_cul2" = ifelse(is.null(input$Intervention_cd_cul2), "", input$Intervention_cd_cul2),
        "Intervention_cl_cul2" = ifelse(is.null(input$Intervention_cl_cul2), "", input$Intervention_cl_cul2),
        "Intervention_SRS_cul2" = ifelse(is.null(input$Intervention_SRS_cul2), "", input$Intervention_SRS_cul2),
        "Intervention_NR_cul2" = ifelse(is.null(input$Intervention_NR_cul2), "", input$Intervention_NR_cul2),
        "Intervention_D_cul2" = ifelse(is.null(input$Intervention_D_cul2), "", input$Intervention_D_cul2),
        "Intervention_AC_cul2" = ifelse(is.null(input$Intervention_AC_cul2), "", input$Intervention_AC_cul2),
        "Intervention_O_cul2" = ifelse(is.null(input$Intervention_O_cul2), "", input$Intervention_O_cul2),
        "Intervention_O_detail_cul2" = ifelse(is.null(input$Intervention_O_detail_cul2), "", input$Intervention_O_detail_cul2),
        # "Intervention_P_cul2" = ifelse(is.null(input$Intervention_P_cul2), "", input$Intervention_P_cul2),
        "Intervention_CPS_cul2" = ifelse(is.null(input$Intervention_CPS_cul2), "", input$Intervention_CPS_cul2),
        "Intervention_CPS_detail_cul2" = ifelse(is.null(input$Intervention_CPS_detail_cul2), "", input$Intervention_CPS_detail_cul2),
        "Trombosuction_cul2" = ifelse(is.null(input$Trombosuction_cul2), "", input$Trombosuction_cul2),
        "Perforation_cul2" = ifelse(is.null(input$Perforation_cul2), "", input$Perforation_cul2)
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
      input$submit6
    )
  })
  
  # Reference : https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
  
  validate_edit <- eventReactive(
    eventExpr = callEdit(),
    valueExpr = {
      if (input$submit0 == 0 && input$submit1 == 0 && input$submit2 == 0 &&
          input$submit3 == 0 && input$submit4 == 0 && input$submit5 == 0 &&
          input$submit6 == 0 && input$submit == 0) {
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
