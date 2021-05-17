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
        checkboxGroupInput(
          ns("Vessel_cul2"),
          label = "Vessel",
          choices = c("LM" = 0, "LAD" = 1, "LCx" = 2, "RCA" = 3),
          selected = character(0),
          inline = TRUE
        ),
        radioButtons(
          ns("Lesion_cul2"),
          label = "Lesion",
          choices = c("1" = 1, "2" = 2, "3" = 3),
          selected = character(0),
          inline = TRUE
        ),
        textInput(
          ns("Lesion_segment_1_cul2"),
          label = "Segment",
          value = hold$Lesion_segment_1_cul2
        ),
        textInput(
          ns("Lesion_VeS_1_cul2"),
          label = "Visually estimated Stenosis(%)",
          value = hold$Lesion_VeS_1_cul2
        ),
        
        conditionalPanel(
          "input.Lesion_cul2 >= 2",
          ns = ns,
          textInput(
            ns("Lesion_segment_2_cul2"),
            label = "Segment",
            value = hold$Lesion_segment_2_cul2
          ),
          textInput(
            ns("Lesion_VeS_2_cul2"),
            label = "Visually estimated Stenosis(%)",
            value = hold$Lesion_VeS_2_cul2
          ),
        ),
        
        conditionalPanel(
          "input.Lesion_cul2 >= 3",
          ns = ns,
          textInput(
            ns("Lesion_segment_3_cul2"),
            label = "Segment",
            value = hold$Lesion_segment_3_cul2
          ),
          textInput(
            ns("Lesion_VeS_3_cul2"),
            label = "Visually estimated Stenosis(%)",
            value = hold$Lesion_VeS_3_cul2
          ),
        ),
        
        checkboxGroupInput(
          ns("Lesion_Type_cul2"),
          label = "Lesion Type",
          choices = c("CTO Lesion" = 0, "True bifurcation lesion ( medina 1,1,1/1,0,1/0,1,1)" = 1, "Long lesion (≥38mm stent)"= 2, 
                      "Unprotected Left Main" = 3, "ISR lesion" = 4, "Calcified lesion" = 5, "Not applicable" = 6),
          selected = character(0),
          inline = TRUE
        ),
        
        radioButtons(
          ns("FFR_cul2"),
          label = "FFR",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$FFR_cul2
        ),
        
        conditionalPanel(
          "input.FFR_cul2 == 0",
          ns = ns,
          radioButtons(
            ns("FFR_pre_cul2"),
            label = "Pre-PCI FFR",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$FFR_pre_cul2
          ),
          radioButtons(
            ns("FFR_post_cul2"),
            label = "Post-PCI FFR",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$FFR_post_cul2
          )
        ),
        
        radioButtons(
          ns("Image_cul2"),
          label = "Image Device",
          choices = c("IVUS" = 0, "OCT" = 1, "ND" = 2),
          selected = hold$FFR_cul2
        ),
        conditionalPanel(
          "input.Image_cul2 <= 1",
          ns = ns,
          radioButtons(
            ns("Pre_stent_cul2"),
            label = "Pre-Stenting",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$Pre_stent_cul2
          ),
          radioButtons(
            ns("Post_stent_cul2"),
            label = "Post-Stenting",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$Post_stent_cul2
          )
        ),
        
        radioButtons(
          ns("Intervention_cul2"),
          label = "Intervention Data",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Intervention_cul2
        ),
        
        conditionalPanel(
          "input.Intervention_cul2 == 0",
          ns = ns,
          radioButtons(
            ns("Intervention_cnt_cul2"),
            label = "",
            choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,"6" = 6, "7"=7,"8"=8,"9"=9,"10"=10),
            selected = "1"
          ),
          radioButtons(
            ns('Intervention1_treat_cul2'),
            label = 'Treatment',
            choices = c('Ballooning' = 0, 'Stenting'=1),
            selected = character(0)
          ),
          textInput(
            ns("Intervention1_segment_cul2"),
            label = "Segment",
            value = hold$Intervention1_segment_cul2
          ),
          textInput(
            ns("Intervention1_name_cul2"),
            label = "Name",
            value = hold$Intervention1_name_cul2
          ),
          textInput(
            ns("Intervention1_diameter_cul2"),
            label = "Diameter",
            value = hold$Intervention1_diameter_cul2
          ),
          textInput(
            ns("Intervention1_length_cul2"),
            label = "Length",
            value = hold$Intervention1_length_cul2
          ),
          textInput(
            ns("Intervention1_pressure_cul2"),
            label = "Max Pressure",
            value = hold$Intervention1_pressure_cul2
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 2",
            ns = ns,
            radioButtons(
              ns('Intervention2_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention2_segment_cul2"),
              label = "Segment",
              value = hold$Intervention2_segment_cul2
            ),
            textInput(
              ns("Intervention2_name_cul2"),
              label = "Name",
              value = hold$Intervention2_name_cul2
            ),
            textInput(
              ns("Intervention2_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention2_diameter_cul2
            ),
            textInput(
              ns("Intervention2_length_cul2"),
              label = "Length",
              value = hold$Intervention2_length_cul2
            ),
            textInput(
              ns("Intervention2_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention2_pressure_cul2
            )
          ),
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 3",
            ns = ns,
            radioButtons(
              ns('Intervention3_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention3_segment_cul2"),
              label = "Segment",
              value = hold$Intervention3_segment_cul2
            ),
            textInput(
              ns("Intervention3_name_cul2"),
              label = "Name",
              value = hold$Intervention3_name_cul2
            ),
            textInput(
              ns("Intervention3_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention3_diameter_cul2
            ),
            textInput(
              ns("Intervention3_length_cul2"),
              label = "Length",
              value = hold$Intervention3_length_cul2
            ),
            textInput(
              ns("Intervention3_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention3_pressure_cul2
            )
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 4",
            ns = ns,
            radioButtons(
              ns('Intervention4_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention4_segment_cul2"),
              label = "Segment",
              value = hold$Intervention4_segment_cul2
            ),
            textInput(
              ns("Intervention4_name_cul2"),
              label = "Name",
              value = hold$Intervention4_name_cul2
            ),
            textInput(
              ns("Intervention4_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention4_diameter_cul2
            ),
            textInput(
              ns("Intervention4_length_cul2"),
              label = "Length",
              value = hold$Intervention4_length_cul2
            ),
            textInput(
              ns("Intervention4_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention4_pressure_cul2
            )
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 5",
            ns = ns,
            radioButtons(
              ns('Intervention5_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention5_segment_cul2"),
              label = "Segment",
              value = hold$Intervention5_segment_cul2
            ),
            textInput(
              ns("Intervention5_name_cul2"),
              label = "Name",
              value = hold$Intervention5_name_cul2
            ),
            textInput(
              ns("Intervention5_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention5_diameter_cul2
            ),
            textInput(
              ns("Intervention5_length_cul2"),
              label = "Length",
              value = hold$Intervention5_length_cul2
            ),
            textInput(
              ns("Intervention5_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention5_pressure_cul2
            )
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 6",
            ns = ns,
            radioButtons(
              ns('Intervention6_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention6_segment_cul2"),
              label = "Segment",
              value = hold$Intervention6_segment_cul2
            ),
            textInput(
              ns("Intervention6_name_cul2"),
              label = "Name",
              value = hold$Intervention6_name_cul2
            ),
            textInput(
              ns("Intervention6_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention6_diameter_cul2
            ),
            textInput(
              ns("Intervention6_length_cul2"),
              label = "Length",
              value = hold$Intervention6_length_cul2
            ),
            textInput(
              ns("Intervention6_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention6_pressure_cul2
            )
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 7",
            ns = ns,
            radioButtons(
              ns('Intervention7_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention7_segment_cul2"),
              label = "Segment",
              value = hold$Intervention7_segment_cul2
            ),
            textInput(
              ns("Intervention7_name_cul2"),
              label = "Name",
              value = hold$Intervention7_name_cul2
            ),
            textInput(
              ns("Intervention7_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention7_diameter_cul2
            ),
            textInput(
              ns("Intervention7_length_cul2"),
              label = "Length",
              value = hold$Intervention7_length_cul2
            ),
            textInput(
              ns("Intervention7_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention7_pressure_cul2
            )
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 8",
            ns = ns,
            radioButtons(
              ns('Intervention8_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention8_segment_cul2"),
              label = "Segment",
              value = hold$Intervention8_segment_cul2
            ),
            textInput(
              ns("Intervention8_name_cul2"),
              label = "Name",
              value = hold$Intervention8_name_cul2
            ),
            textInput(
              ns("Intervention8_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention8_diameter_cul2
            ),
            textInput(
              ns("Intervention8_length_cul2"),
              label = "Length",
              value = hold$Intervention8_length_cul2
            ),
            textInput(
              ns("Intervention8_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention8_pressure_cul2
            )
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 9",
            ns = ns,
            radioButtons(
              ns('Intervention9_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention9_segment_cul2"),
              label = "Segment",
              value = hold$Intervention9_segment_cul2
            ),
            textInput(
              ns("Intervention9_name_cul2"),
              label = "Name",
              value = hold$Intervention9_name_cul2
            ),
            textInput(
              ns("Intervention9_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention9_diameter_cul2
            ),
            textInput(
              ns("Intervention9_length_cul2"),
              label = "Length",
              value = hold$Intervention9_length_cul2
            ),
            textInput(
              ns("Intervention9_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention9_pressure_cul2
            )
          ),
          
          conditionalPanel(
            "input.Intervention_cnt_cul2 >= 10",
            ns = ns,
            radioButtons(
              ns('Intervention10_treat_cul2'),
              label = 'Treatment',
              choices = c('Ballooning' = 0, 'Stenting'=1),
              selected = character(0)
            ),
            textInput(
              ns("Intervention10_segment_cul2"),
              label = "Segment",
              value = hold$Intervention10_segment_cul2
            ),
            textInput(
              ns("Intervention10_name_cul2"),
              label = "Name",
              value = hold$Intervention10_name_cul2
            ),
            textInput(
              ns("Intervention10_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention10_diameter_cul2
            ),
            textInput(
              ns("Intervention10_length_cul2"),
              label = "Length",
              value = hold$Intervention10_length_cul2
            ),
            textInput(
              ns("Intervention10_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention10_pressure_cul2
            )
          ),
          
          radioButtons(
            ns("Intervention_ab_cul2"),
            label = "Adjunctive Balloon",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$Intervention_adjballon_cul2
          ),
          
          conditionalPanel(
            "input.Intervention_ab_cul2 == 0",
            ns = ns,
            textInput(
              ns("Intervention_ab_diameter_cul2"),
              label = "Diameter",
              value = hold$Intervention_adjballon_diameter_cul2
            ),
            textInput(
              ns("Intervention_ab_length_cul2"),
              label = "Length",
              value = hold$Intervention_adjballon_length_cul2
            ),
            textInput(
              ns("Intervention_ab_pressure_cul2"),
              label = "Max Pressure",
              value = hold$Intervention_adjballon_pressure_cul2
            )
          ),
          
          radioButtons(
            ns("Intervention_Rota_cul2"),
            label = "Rota-ablation",
            choices = c("Yes" = 0, "No" = 1),
            selected = hold$Intervention_Rota_cul2
          ),
          
          radioButtons(
            ns("Intervention_cd_cul2"),
            label = "Clinical Device Success",
            choices = c("Yes" = 0, "No" = 1, "NA" = 2),
            selected = hold$Intervention_cd_cul2
          ),
          
          radioButtons(
            ns("Intervention_cln_cul2"),
            label = "Clinical Lesion Success",
            choices = c("Yes" = 0, "No" = 1, "NA" = 2),
            selected = hold$Intervention_cl_cul2
          ),
          
          conditionalPanel(
            "input.Intervention_clinical_lesion_cul2 == 1",
            ns = ns,
            radioButtons(
              ns("Intervention_SRS_cul2"),
              label = "Significant Residual Stenosis",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Intervention_SRS_cul2
            ),
            radioButtons(
              ns("Intervention_NR_cul2"),
              label = "No Reflow",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Intervention_NR_cul2
            ),
            
            radioButtons(
              ns("Intervention_D_cul2"),
              label = "Dissection",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Intervention_D_cul2
            ),
            
            radioButtons(
              ns("Intervention_AC_cul2"),
              label = "Acute Closure",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Intervention_AC_cul2
            ),
            radioButtons(
              ns("Intervention_O_cul2"),
              label = "Other",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Intervention_O_cul2
            ),
            conditionalPanel(
              "Intervention_O_cul2 == 0",
              ns = ns,
              textInput(
                ns("Intervention_O_detail_cul2"),
                label = "",
                value = hold$Intervention_O_detail_cul2
              )
            ),
            
            radioButtons(
              ns("Intervention_P_cul2"),
              label = "Perforation",
              choices = c("Yes" = 0, "No" = 1),
              selected = hold$Intervention_P_cul2
            )
            
          ),
          
          radioButtons(
            ns("Intervention_CPS_cul2"),
            label = "Clinical Procedureal Success",
            choices = c("Yes" = 0, "No" = 1, "NA" = 2),
            selected = hold$Intervention_CPS_cul2
          ),
          conditionalPanel(
            "Intervention_CPS_cul2 == 1",
            ns = ns,
            textInput(
              ns("Intervention_CPS_detail_cul2"),
              label = "Next Treatment",
              value = hold$Intervention_CPS_detail_cul2
            )
          )
        ),
        
        
        radioButtons(
          ns("Trombosuction_cul2"),
          label = "Trombosuction",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Trombosuction_cul2
        ),
        
        radioButtons(
          ns("Perforation_cul2"),
          label = "Perforation",
          choices = c("Yes" = 0, "No" = 1),
          selected = hold$Perforation_cul2
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
        'Vessel_cul2' = ifelse(is.null(input$Vessel_cul2), '', input$Vessel_cul2),
        'Lesion_cul2' = ifelse(is.null(input$Lesion_cul2), '', input$Lesion_cul2),
        'Lesion_segment_1_cul2' = ifelse(is.null(input$Lesion_segment_1_cul2), '', input$Lesion_segment_1_cul2),
        'Lesion_VeS_1_cul2' = ifelse(is.null(input$Lesion_VeS_1_cul2), '', input$Lesion_VeS_1_cul2),
        'Lesion_segment_2_cul2' = ifelse(is.null(input$Lesion_segment_2_cul2), '', input$Lesion_segment_2_cul2),
        'Lesion_VeS_2_cul2' = ifelse(is.null(input$Lesion_VeS_2_cul2), '', input$Lesion_VeS_2_cul2),
        'Lesion_segment_3_cul2' = ifelse(is.null(input$Lesion_segment_3_cul2), '', input$Lesion_segment_3_cul2),
        'Lesion_VeS_3_cul2' = ifelse(is.null(input$Lesion_VeS_3_cul2), '', input$Lesion_VeS_3_cul2),
        'Lesion_Type_cul2' = ifelse(is.null(input$Lesion_Type_cul2), '', input$Lesion_Type_cul2),
        'FFR_cul2' = ifelse(is.null(input$FFR_cul2), '', input$FFR_cul2),
        'FFR_pre_cul2' = ifelse(is.null(input$FFR_pre_cul2), '', input$FFR_pre_cul2),
        'FFR_post_cul2' = ifelse(is.null(input$FFR_post_cul2), '', input$FFR_post_cul2),
        'Image_cul2' = ifelse(is.null(input$Image_cul2), '', input$Image_cul2),
        'Pre_stent_cul2' = ifelse(is.null(input$Pre_stent_cul2), '', input$Pre_stent_cul2),
        'Post_stent_cul2' = ifelse(is.null(input$Post_stent_cul2), '', input$Post_stent_cul2),
        'Intervention_cul2' = ifelse(is.null(input$Intervention_cul2), '', input$Intervention_cul2),
        'Intervention_cnt_cul2' = ifelse(is.null(input$Intervention_cnt_cul2), '', input$Intervention_cnt_cul2),
        'Intervention1_treat_cul2' = ifelse(is.null(input$Intervention1_treat_cul2), '', input$Intervention1_treat_cul2),
        'Intervention1_segment_cul2' = ifelse(is.null(input$Intervention1_segment_cul2), '', input$Intervention1_segment_cul2),
        'Intervention1_name_cul2' = ifelse(is.null(input$Intervention1_name_cul2), '', input$Intervention1_name_cul2),
        'Intervention1_diameter_cul2' = ifelse(is.null(input$Intervention1_diameter_cul2), '', input$Intervention1_diameter_cul2),
        'Intervention1_length_cul2' = ifelse(is.null(input$Intervention1_length_cul2), '', input$Intervention1_length_cul2),
        'Intervention1_pressure_cul2' = ifelse(is.null(input$Intervention1_pressure_cul2), '', input$Intervention1_pressure_cul2),
        'Intervention2_treat_cul2' = ifelse(is.null(input$Intervention2_treat_cul2), '', input$Intervention2_treat_cul2),
        'Intervention2_segment_cul2' = ifelse(is.null(input$Intervention2_segment_cul2), '', input$Intervention2_segment_cul2),
        'Intervention2_name_cul2' = ifelse(is.null(input$Intervention2_name_cul2), '', input$Intervention2_name_cul2),
        'Intervention2_diameter_cul2' = ifelse(is.null(input$Intervention2_diameter_cul2), '', input$Intervention2_diameter_cul2),
        'Intervention2_length_cul2' = ifelse(is.null(input$Intervention2_length_cul2), '', input$Intervention2_length_cul2),
        'Intervention2_pressure_cul2' = ifelse(is.null(input$Intervention2_pressure_cul2), '', input$Intervention2_pressure_cul2),
        'Intervention3_treat_cul2' = ifelse(is.null(input$Intervention3_treat_cul2), '', input$Intervention3_treat_cul2),
        'Intervention3_segment_cul2' = ifelse(is.null(input$Intervention3_segment_cul2), '', input$Intervention3_segment_cul2),
        'Intervention3_name_cul2' = ifelse(is.null(input$Intervention3_name_cul2), '', input$Intervention3_name_cul2),
        'Intervention3_diameter_cul2' = ifelse(is.null(input$Intervention3_diameter_cul2), '', input$Intervention3_diameter_cul2),
        'Intervention3_length_cul2' = ifelse(is.null(input$Intervention3_length_cul2), '', input$Intervention3_length_cul2),
        'Intervention3_pressure_cul2' = ifelse(is.null(input$Intervention3_pressure_cul2), '', input$Intervention3_pressure_cul2),
        'Intervention4_treat_cul2' = ifelse(is.null(input$Intervention4_treat_cul2), '', input$Intervention4_treat_cul2),
        'Intervention4_segment_cul2' = ifelse(is.null(input$Intervention4_segment_cul2), '', input$Intervention4_segment_cul2),
        'Intervention4_name_cul2' = ifelse(is.null(input$Intervention4_name_cul2), '', input$Intervention4_name_cul2),
        'Intervention4_diameter_cul2' = ifelse(is.null(input$Intervention4_diameter_cul2), '', input$Intervention4_diameter_cul2),
        'Intervention4_length_cul2' = ifelse(is.null(input$Intervention4_length_cul2), '', input$Intervention4_length_cul2),
        'Intervention4_pressure_cul2' = ifelse(is.null(input$Intervention4_pressure_cul2), '', input$Intervention4_pressure_cul2),
        'Intervention5_treat_cul2' = ifelse(is.null(input$Intervention5_treat_cul2), '', input$Intervention5_treat_cul2),
        'Intervention5_segment_cul2' = ifelse(is.null(input$Intervention5_segment_cul2), '', input$Intervention5_segment_cul2),
        'Intervention5_name_cul2' = ifelse(is.null(input$Intervention5_name_cul2), '', input$Intervention5_name_cul2),
        'Intervention5_diameter_cul2' = ifelse(is.null(input$Intervention5_diameter_cul2), '', input$Intervention5_diameter_cul2),
        'Intervention5_length_cul2' = ifelse(is.null(input$Intervention5_length_cul2), '', input$Intervention5_length_cul2),
        'Intervention5_pressure_cul2' = ifelse(is.null(input$Intervention5_pressure_cul2), '', input$Intervention5_pressure_cul2),
        'Intervention6_treat_cul2' = ifelse(is.null(input$Intervention6_treat_cul2), '', input$Intervention6_treat_cul2),
        'Intervention6_segment_cul2' = ifelse(is.null(input$Intervention6_segment_cul2), '', input$Intervention6_segment_cul2),
        'Intervention6_name_cul2' = ifelse(is.null(input$Intervention6_name_cul2), '', input$Intervention6_name_cul2),
        'Intervention6_diameter_cul2' = ifelse(is.null(input$Intervention6_diameter_cul2), '', input$Intervention6_diameter_cul2),
        'Intervention6_length_cul2' = ifelse(is.null(input$Intervention6_length_cul2), '', input$Intervention6_length_cul2),
        'Intervention6_pressure_cul2' = ifelse(is.null(input$Intervention6_pressure_cul2), '', input$Intervention6_pressure_cul2),
        'Intervention7_treat_cul2' = ifelse(is.null(input$Intervention7_treat_cul2), '', input$Intervention7_treat_cul2),
        'Intervention7_segment_cul2' = ifelse(is.null(input$Intervention7_segment_cul2), '', input$Intervention7_segment_cul2),
        'Intervention7_name_cul2' = ifelse(is.null(input$Intervention7_name_cul2), '', input$Intervention7_name_cul2),
        'Intervention7_diameter_cul2' = ifelse(is.null(input$Intervention7_diameter_cul2), '', input$Intervention7_diameter_cul2),
        'Intervention7_length_cul2' = ifelse(is.null(input$Intervention7_length_cul2), '', input$Intervention7_length_cul2),
        'Intervention7_pressure_cul2' = ifelse(is.null(input$Intervention7_pressure_cul2), '', input$Intervention7_pressure_cul2),
        'Intervention8_treat_cul2' = ifelse(is.null(input$Intervention8_treat_cul2), '', input$Intervention8_treat_cul2),
        'Intervention8_segment_cul2' = ifelse(is.null(input$Intervention8_segment_cul2), '', input$Intervention8_segment_cul2),
        'Intervention8_name_cul2' = ifelse(is.null(input$Intervention8_name_cul2), '', input$Intervention8_name_cul2),
        'Intervention8_diameter_cul2' = ifelse(is.null(input$Intervention8_diameter_cul2), '', input$Intervention8_diameter_cul2),
        'Intervention8_length_cul2' = ifelse(is.null(input$Intervention8_length_cul2), '', input$Intervention8_length_cul2),
        'Intervention8_pressure_cul2' = ifelse(is.null(input$Intervention8_pressure_cul2), '', input$Intervention8_pressure_cul2),
        'Intervention9_treat_cul2' = ifelse(is.null(input$Intervention9_treat_cul2), '', input$Intervention9_treat_cul2),
        'Intervention9_segment_cul2' = ifelse(is.null(input$Intervention9_segment_cul2), '', input$Intervention9_segment_cul2),
        'Intervention9_name_cul2' = ifelse(is.null(input$Intervention9_name_cul2), '', input$Intervention9_name_cul2),
        'Intervention9_diameter_cul2' = ifelse(is.null(input$Intervention9_diameter_cul2), '', input$Intervention9_diameter_cul2),
        'Intervention9_length_cul2' = ifelse(is.null(input$Intervention9_length_cul2), '', input$Intervention9_length_cul2),
        'Intervention9_pressure_cul2' = ifelse(is.null(input$Intervention9_pressure_cul2), '', input$Intervention9_pressure_cul2),
        'Intervention10_treat_cul2' = ifelse(is.null(input$Intervention10_treat_cul2), '', input$Intervention10_treat_cul2),
        'Intervention10_segment_cul2' = ifelse(is.null(input$Intervention10_segment_cul2), '', input$Intervention10_segment_cul2),
        'Intervention10_name_cul2' = ifelse(is.null(input$Intervention10_name_cul2), '', input$Intervention10_name_cul2),
        'Intervention10_diameter_cul2' = ifelse(is.null(input$Intervention10_diameter_cul2), '', input$Intervention10_diameter_cul2),
        'Intervention10_length_cul2' = ifelse(is.null(input$Intervention10_length_cul2), '', input$Intervention10_length_cul2),
        'Intervention10_pressure_cul2' = ifelse(is.null(input$Intervention10_pressure_cul2), '', input$Intervention10_pressure_cul2),
        'Intervention_ab_cul2' = ifelse(is.null(input$Intervention_ab_cul2), '', input$Intervention_ab_cul2),
        'Intervention_ab_diameter_cul2' = ifelse(is.null(input$Intervention_ab_diameter_cul2), '', input$Intervention_ab_diameter_cul2),
        'Intervention_ab_length_cul2' = ifelse(is.null(input$Intervention_ab_length_cul2), '', input$Intervention_ab_length_cul2),
        'Intervention_ab_pressure_cul2' = ifelse(is.null(input$Intervention_ab_pressure_cul2), '', input$Intervention_ab_pressure_cul2),
        'Intervention_Rota_cul2' = ifelse(is.null(input$Intervention_Rota_cul2), '', input$Intervention_Rota_cul2),
        'Intervention_cd_cul2' = ifelse(is.null(input$Intervention_cd_cul2), '', input$Intervention_cd_cul2),
        'Intervention_cl_cul2' = ifelse(is.null(input$Intervention_cl_cul2), '', input$Intervention_cl_cul2),
        'Intervention_SRS_cul2' = ifelse(is.null(input$Intervention_SRS_cul2), '', input$Intervention_SRS_cul2),
        'Intervention_NR_cul2' = ifelse(is.null(input$Intervention_NR_cul2), '', input$Intervention_NR_cul2),
        'Intervention_D_cul2' = ifelse(is.null(input$Intervention_D_cul2), '', input$Intervention_D_cul2),
        'Intervention_AC_cul2' = ifelse(is.null(input$Intervention_AC_cul2), '', input$Intervention_AC_cul2),
        'Intervention_O_cul2' = ifelse(is.null(input$Intervention_O_cul2), '', input$Intervention_O_cul2),
        'Intervention_O_detail_cul2' = ifelse(is.null(input$Intervention_O_detail_cul2), '', input$Intervention_O_detail_cul2),
        'Intervention_P_cul2' = ifelse(is.null(input$Intervention_P_cul2), '', input$Intervention_P_cul2),
        'Intervention_CPS_cul2' = ifelse(is.null(input$Intervention_CPS_cul2), '', input$Intervention_CPS_cul2),
        'Intervention_CPS_detail_cul2' = ifelse(is.null(input$Intervention_CPS_detail_cul2), '', input$Intervention_CPS_detail_cul2),
        'Trombosuction_cul2' = ifelse(is.null(input$Trombosuction_cul2), '', input$Trombosuction_cul2),
        'Perforation_cul2' = ifelse(is.null(input$Perforation_cul2), '', input$Perforation_cul2)
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
