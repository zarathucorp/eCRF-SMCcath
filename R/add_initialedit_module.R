
#' Enroll Add & Edit Module
#'
#' Module to add & initial edit
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
add_initialedit_module <- function(input, output, session, modal_title, car_to_edit, modal_trigger, tbl = "rct", sessionid, data, rd = rd) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- car_to_edit()
    
    if (tbl == "rct") {
      showModal(
        modalDialog(
          h3("Stratified randomization"),
          uiOutput(ns("pidui")),
          fluidRow(
            column(
              width = 3,
              radioButtons(
                ns("DM_random"),
                "DM",
                c("No","Yes"),
                inline = T,
                selected = character(0)
              ),
            ),
            column(
              width = 3,
              radioButtons(
                ns("STEMI_random"),
                "AMI Type",
                c("NSTEMI", "STEMI"),
                inline = T,
                selected = character(0)
              ),
            ),
            column(
              width = 3,
              radioButtons(
                ns('Center'),
                'Center',
                c('삼성서울병원','전남대병원'),
                inline = T,
                selected = character(0)
              )
            ),
            column(
              width = 3,
              radioButtons(
                ns("Sex"),
                "Sex",
                choices = c("M", "F"),
                selected = character(0),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("Initial"),
                "Initial",
                value = ifelse(is.null(hold), "", hold$Initial)
              )
            ),
            column(
              width = 3,
              dateInput(
                ns("Index_PCI_Date"),
                "Index PCI Date",
                value = NULL,
                language = "kr"
              )
            ),
            column(
              width = 3,
              dateInput(
                ns("Agree_Date"),
                "동의서 서명일",
                value = NULL,
                language = "kr"
              )
            ),
            column(
              width = 3,
              dateInput(
                ns("Birthday"),
                "Birthday",
                value = as.character(lubridate::as_date(NA)),
                language = "kr"
              )
            )
          ),
          h3("Inclusion"),
          
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("in_1"),
                "1. 만 19세 이상",
                choices = c("Yes", "No"),
                selected = character(0),
                inline = T
              )
            ),
            column(
              width = 6,
              actionButton(
                ns("CYfA"),
                "Check Yes for All",
                class = "btn btn-default"
              )
            )
          ),
          radioButtons(
            ns("in_2"),
            # "2.	관상동맥 질환으로 경피적 관상동맥 중재시술이 필요한 환자",
            "2.	임상시험의 시험군 및 대조군의 정의 및 시술의 위험성을 인지하고 임상시험 참가에 환자 또는 법정 대리인이 자발적으로 동의한 경우",
            choices = c("Yes", "No"),
            selected = character(0),
            inline = T
          ),
          radioButtons(
            ns("in_3"),
            "3. Type 1 급성심근경색으로 진단된 환자 (ST 분절 상승 심근경색 또는 비 ST 분절 상승 심근경색)",
            choices = c("Yes", "No"),
            selected = character(0),
            inline = T
          ),
          h5("1) 심근 효소(troponin)의 값이 참고치의 상위 99% 이상 상승 (above the 99th percentile upper reference limit)"),
          h5("2) 심근 허혈을 시사하는 증상 혹은 심전도 변화"),
          radioButtons(
            ns("in_4"),
            "4. 심부전 발생 고 위험 환자 (아래 두 가지 기준 중 하나 이상 만족하는 경우)",
            choices = c("Yes", "No"),
            selected = character(0),
            inline = T
          ),
          h5("1) 좌심실 구혈율 (left ventricular ejection fraction) <50% 또는"),
          h5("2) 폐 울혈의 증상이나 징후가 있어 치료가 필요한 경우"),
          br(),
          h3("Exclusion"),
          actionButton(
            ns("CNfA"),
            "Check No for All",
            class = "btn btn-default"
          ),
          radioButtons(
            ns("ex_1"),
            # "1.	시술자에 의해 표적혈관의 협착이 관상동맥 중재시술에 적합하지 않다고 판단되는 경우(Target lesion not amenable for PCI by operators decision)",
            "1.	시술자에 의해 표적혈관의 협착이 관상동맥 중재시술에 적합하지 않다고 판단되는 경우",
            choices = c("Yes", "No"),
            selected = character(0),
            inline = T
          ),
          radioButtons(
            ns("ex_2"),
            # "2. 심혈관성 쇼크 상태인 경우 (Cardiogenic shock (Killip class IV) at presentation)",
            "2. 무작위 배정 전 심정지로 심폐소생술이 필요한 경우",
            choices = c("Yes", "No"),
            selected = character(0),
            inline = T
          ),
          radioButtons(
            ns("ex_3"),
            # "3.	다음 약제에 과민성이 있거나, 투약의 금기사항이 있는 경우(aspirin, clopidogrel, ticagrelor, prasugrel, heparin, everolimus, zotarolimus, biolimus, sirolimus)",
            "3.	혈전용해술 이후 구제적 관상동맥 중재술/용이성 관상동맥 중재술을 시행한 경우",
            choices = c("Yes", "No"),
            selected = character(0),
            inline = T
          ),
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("ex_4"),
                "4. 과거에 심근경색이 있었던 경우",
                choices = c("Yes", "No"),
                selected = character(0),
                inline = T
              )
            ),
            column(
              width = 6,
              radioButtons(
                ns("ex_5"),
                "5. SGLT-2 억제제를 지속 복용중인 환자",
                choices = c("Yes", "No"),
                selected = character(0),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("ex_6"),
                # "6.	비 심장질환으로 기대 여명이 1년 미만이거나 치료에 순응도가 낮을 것으로 기대되는 자(조사자가 의학적인 판단으로 정함)",
                "6.	사구체 여과율 30 ml/min/1.73m2 미만이거나 투석중인 환자",
                choices = c("Yes", "No"),
                selected = character(0),
                inline = T
              )
            ),
            column(
              width = 6,
              radioButtons(
                ns("ex_7"),
                # "7. 연구 참여를 거부한 환자",
                "7. 1형 당뇨병을 앓고 있는 경우",
                choices = c("Yes", "No"),
                selected = character(0),
                inline = T
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("ex_8"),
                # "7. 연구 참여를 거부한 환자",
                "8. SGLT-2 억제제에 과민성이 있는 환자",
                choices = c("Yes", "No"),
                selected = character(0),
                inline = T
              )
            ),
            column(
              width = 6,
              radioButtons(
                ns("ex_9"),
                # "7. 연구 참여를 거부한 환자",
                "9. 임산부 및 수유부",
                choices = c("Yes", "No"),
                selected = character(0),
                inline = T
              )
            )
          ),
          radioButtons(
            ns("ex_10"),
            # "7. 연구 참여를 거부한 환자",
            "10. 비 심장질환으로 인하여 기대여명이 1년 이내이거나 치료에 순응도가 낮을 것으로 기대되는 자 (조사자가 의학적인 판단으로 정함)",
            choices = c("Yes", "No"),
            selected = character(0),
            inline = T
          ),
          title = modal_title,
          size = "l",
          footer = list(
            modalButton("Cancel"),
            actionButton(
              ns("submit"),
              "Randomization & submit",
              class = "btn btn-primary",
              style = "color: white"
            )
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          h3("Prospective study"),
          uiOutput(ns("pidui")),
          fluidRow(
            column(
              width = 3,
              textInput(
                ns("Initial"),
                "Initial",
                value = ifelse(is.null(hold), "", hold$Initial)
              )
            ),
            column(
              width = 3,
              dateInput(
                ns("Index_PCI_Date"),
                "Index PCI Date",
                value = NULL,
                language = "kr"
              )
            ),
            column(
              width = 3,
              dateInput(
                ns("Agree_Date"),
                "동의서 서명일",
                value = NULL,
                language = "kr"
              )
            ),
            column(
              width = 3,
              dateInput(
                ns("Birthday"),
                "Birthday",
                value = as.character(lubridate::as_date(NA)),
                language = "kr"
              )
            )
          ),
          radioButtons(
            ns("Sex"),
            "Sex",
            choices = c("M", "F"),
            selected = character(0),
            inline = T
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
    }
    
    
    
    # Observe event for "Model" text input in Add/Edit Car Modal
    # `shinyFeedback`
    observe({
      req(!is.null(input$Initial))
        if (input$Initial == "") {
          shinyFeedback::showFeedbackDanger(
            "Initial",
            text = "환자 이니셜 입력"
          )
        } else {
          shinyFeedback::hideFeedback("Initial")
        }
        
        if (length(input$Index_PCI_Date) == 0) {
          shinyFeedback::showFeedbackDanger(
            "Index_PCI_Date",
            text = "Index_PCI_Date 입력"
          )
        } else {
          shinyFeedback::hideFeedback("Index_PCI_Date")
        }
        
        if (length(input$Birthday) == 0) {
          shinyFeedback::showFeedbackDanger(
            "Birthday",
            text = "Birthday 입력"
          )
        }
        else {
          shinyFeedback::hideFeedback("Birthday")
        }
      
      if (tbl == "rct"){
        if (!is.null(input$pid) && (length(input$DM_random) != 0) && (length(input$STEMI_random) != 0) && (length(input$Birthday) != 0) && (length(input$Index_PCI_Date)) != 0 && (length(input$Agree_Date) != 0) && 
            (input$Initial != "") && (length(input$Sex) != 0) && (length(input$in_1) != 0) && (length(input$in_2) != 0) && (length(input$in_3) != 0) && (length(input$in_4) != 0) &&
            (length(input$ex_1) != 0) && (length(input$ex_2) != 0) && (length(input$ex_3) != 0) && (length(input$ex_4) != 0) && (length(input$ex_5) != 0) && (length(input$ex_6) != 0) && (length(input$ex_7) != 0) && (length(input$ex_8) != 0) && (length(input$ex_9) != 0) && (length(input$ex_10) != 0)) {
          shinyjs::enable("submit")
        } else{
          shinyjs::disable("submit")
        }
      } else{
        if ((length(input$Birthday) != 0) && (length(input$Index_PCI_Date) != 0) && (length(input$Agree_Date) != 0) && (input$Initial != "") && (length(input$Sex) != 0)) {
          shinyjs::enable("submit")
        } else{
          shinyjs::disable("submit")
        }
      }
      
        
        

      })
})
    
    
    
    output$pidui <- renderUI({
      idlist <- choices.group <- NULL
      if (tbl == "rct") {
        req(input$DM_random)
        req(input$STEMI_random)
        type.strata <- ifelse(
          input$DM_random == "No",
          ifelse(input$STEMI_random == "NSTEMI", "R-NDNST", "R-NDST"),
          ifelse(input$STEMI_random == "NSTEMI", "R-DNST", "R-DST")
        )
        
        pid.group <- grep(type.strata, rd$pid, value = T)
        data.stata <- subset(data(), DM == input$DM_random & AMI_Type == input$STEMI_random)
        # idlist <- setdiff(pid.group, data()$pid)
        idlist <- setdiff(paste0("R-", 1:100000), data()$pid)
        
        if (nrow(data.stata) >= length(pid.group)) {
          ## Random assign
          choices.group <- ifelse(rbinom(1, 1, 0.5) == 1, "SGLT-inhibitor", "Control")
        } else {
          choices.group <- rd[rd$pid %in% pid.group, ]$Group[nrow(data.stata) + 1]
        }
      } else {
        idlist <- setdiff(paste0("P-", 1:100000), data()$pid)
        choices.group <- ""
      }
      
      hold <- car_to_edit()
      choices.pid <- ifelse(is.null(hold), idlist[1], hold$pid)
      
      tagList(
        fluidRow(
          column(
            width = 6,
            selectInput(
              session$ns("pid"),
              "pid",
              choices = choices.pid,
              selected = choices.pid
            )
          ),
          hidden(
            column(
              width = 6,
              radioButtons(
                session$ns("Group"),
                "Group",
                choices.group,
                choices.group[1],
                inline = T
              )
            )
          )
        )
      )
    })
    
    observeEvent(input$CYfA, {
      updateRadioButtons(session, "in_1", selected = "Yes")
      updateRadioButtons(session, "in_2", selected = "Yes")
      updateRadioButtons(session, "in_3", selected = "Yes")
      updateRadioButtons(session, "in_4", selected = "Yes")
    })
    
    observeEvent(input$CNfA, {
      updateRadioButtons(session, "ex_1", selected = "No")
      updateRadioButtons(session, "ex_2", selected = "No")
      updateRadioButtons(session, "ex_3", selected = "No")
      updateRadioButtons(session, "ex_4", selected = "No")
      updateRadioButtons(session, "ex_5", selected = "No")
      updateRadioButtons(session, "ex_6", selected = "No")
      updateRadioButtons(session, "ex_7", selected = "No")
      updateRadioButtons(session, "ex_8", selected = "No")
      updateRadioButtons(session, "ex_9", selected = "No")
      updateRadioButtons(session, "ex_10", selected = "No")
    })
    
    edit_car_dat <- reactive({
      hold <- car_to_edit()
      
      dat <- list(
        "pid" = input$pid,
        "Group" = input$Group,
        "Center" = input$Center,
        
        # Essentials
        "Initial" = input$Initial,
        "Birthday" = lubridate::as_date(input$Birthday),
        "Age" = as.period(interval(start = lubridate::as_date(input$Birthday), end = Sys.Date()))$year,
        "Sex" = input$Sex,
        "Agree_Date" = lubridate::as_date(input$Agree_Date),
        "Index_PCI_Date" = lubridate::as_date(input$Index_PCI_Date)
        
        # index_PCI_Date 필수 입력
        # "Index_PCI_Date" = ifelse(is.null(input$Index_PCI_Date), "", as.character(input$Index_PCI_Date)),
      )
      if (tbl == "rct") {
        dat$DM <- input$DM_random
        dat$AMI_Type <- input$STEMI_random
      }
      
      time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
      
      if (is.null(hold)) {
        # adding a new car
        
        dat$created_at <- time_now
        dat$created_by <- sessionid
      } else {
        # Editing existing car
        
        dat$created_at <- as.character(hold$created_at)
        dat$created_by <- hold$created_by
      }
      
      dat$modified_at <- time_now
      dat$modified_by <- sessionid
      
      return(dat)
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
      
      # sqlsub <- paste(paste0(names(dat$data), "=$", 1:length(dat$data)), collapse = ",")
      
      # [1] "pid"               "Group"             "CENTER"            "DM"                "AMI_Type"          "Initial"          
      # [7] "Birthday"          "Age"               "Sex"               "Agree_Date"        "Index_PCI_Date"    "Withdrawal"       
      # [13] "SGLT"              "Withdrawal_date"   "Withdrawal_reason" "Date_adm"          "Height"            "Weight"           
      # [19] "BMI"               "BSA_adm"     
      
      code.sql <- paste0(
        "INSERT INTO ", 
        tbl, 
        " (pid, 'Group', Center, Initial, ",
        " Birthday, Age, Sex, Agree_Date, Index_PCI_Date, DM, AMI_Type, created_at, created_by, modified_at, modified_by)",
        " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)")
        # " (pid, 'Group', Index_PCI_Date, Agree_Date, Initial, Age, Birthday, Sex, CENTER, DM, AMI_Type, created_at, created_by, modified_at, modified_by) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)")
      if (tbl == "pros") {
        code.sql <- paste0(
          "INSERT INTO ", 
          tbl, 
          " (pid, 'Group', Center, Initial, ",
          " Birthday, Age, Sex, Agree_Date, Index_PCI_Date, created_at, created_by, modified_at, modified_by)",
          " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)")
        # " (pid, 'Group', Index_PCI_Date, Agree_Date, Initial, Age, Birthday, Sex, CENTER, created_at, created_by, modified_at, modified_by) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)")
      }
      
      tryCatch(
        {
          dbExecute(conn, code.sql, params = unname(dat))
          
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
