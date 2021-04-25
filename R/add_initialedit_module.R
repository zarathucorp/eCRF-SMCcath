
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
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("DM_random"),
                "DM",
                c("No" = "0", "Yes" = "1"),
                inline = T
              ),
            ),
            column(
              width = 6,
              radioButtons(
                ns("STEMI_random"),
                "AMI Type",
                c("NSTEMI", "STEMI"),
                inline = T
              ),
            )
          ),
          uiOutput(ns("pidui")),
          fluidRow(
            column(
              width = 6,
              textInput(
                ns("Initial"),
                "Initial",
                value = ifelse(is.null(hold), "", hold$Initial)
              )
            ),
            column(
              width = 6,
              dateInput(
                ns("Index_PCI_Date"),
                "Index PCI Date",
                value = as.Date(NA),
                language = "kr"
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              dateInput(
                ns("Birthday"),
                "Birthday",
                value = as.character(as.Date(NA)),
                language = 'kr'
              )
            ),
            column(
              width = 6,
              radioButtons(
                ns("Sex"),
                "Sex",
                choices = c("M" = 0, "F" = 1),
                selected = NULL, 
                inline = T
              )
            )
          ),
          
          h3("Inclusion"),
          actionButton(
            ns("CYfA"), 
            "Check Yes for All", 
            class = "btn btn-default"
          ),
          radioButtons(
            ns("in_1"), 
            "1. 만 19세 이상", 
            choices = c("Yes", "No"), 
            selected = NULL, 
            inline = T
            ),
          radioButtons(
            ns("in_2"), 
            "2.	관상동맥 질환으로 경피적 관상동맥 중재시술이 필요한 환자", 
            choices = c("Yes", "No"), 
            selected = NULL, 
            inline = T
            ),
          radioButtons(
            ns("in_3"), 
            "3. 관상동맥 복잡 병변이 있는 환자", 
            choices = c("Yes", "No"), 
            selected = NULL, 
            inline = T
            ),
          tags$b("[※ 관상동맥 복잡병변은 아래 9가지 중 하나 이상을 동반하고 있는 경우로 정의한다.]"),
          h5("1) 진성 분지 병변 (Medina classification 1,1,1/1,0,1/0,1,1) 이면서 측부 가지 크기가 2.5mm 이상인 경우"),
          h5("2) 표적 혈관이 만성 완전 폐색 병변인 경우 (≥3 months)"),
          h5("3) 보호되지 않는 좌주간지 병변인 경우 (좌주간지 분지 병변은 비-진성 분지병변도 포함한다.)"),
          h5("4) 삽입될 스텐트의 길이가 38mm 이상일 것으로 예상되는 긴 병변인 경우"),
          h5("5) 다혈관 질환으로 2개 이상의 주요 관상동맥에 관상동맥 중재시술을 시행해야 하는 경우"),
          h5("6) 복수의 스텐트가 필요한 경우 (≥3 more stent per patient)"),
          h5("7) 관상동맥 스텐트 재 협착 병변인 경우"),
          h5("8) 심한 관상동맥 석회화 병변 (encircling calcium in angiography)"),
          h5("9) 좌측관상동맥(LAD, LCX) 및 우측관상동맥(RCA)의 개구부 병변 (ostial lesion)"),
          br(),
          h3("Exclusion"),
          actionButton(
            ns("CNfA"), 
            "Check No for All", 
            class = "btn btn-default"
            ),
          radioButtons(
            ns("ex_1"), 
            "1.	시술자에 의해 표적혈관의 협착이 관상동맥 중재시술에 적합하지 않다고 판단되는 경우(Target lesion not amenable for PCI by operators decision)", 
            choices = c("Yes", "No"), 
            selected = NULL, 
            inline = T
          ),
          radioButtons(
            ns("ex_2"), 
            "2. 심혈관성 쇼크 상태인 경우 (Cardiogenic shock (Killip class IV) at presentation)", 
            choices = c("Yes", "No"), 
            selected = NULL, 
            inline = T
          ),
          radioButtons(
            ns("ex_3"), 
            "3.	다음 약제에 과민성이 있거나, 투약의 금기사항이 있는 경우(aspirin, clopidogrel, ticagrelor, prasugrel, heparin, everolimus, zotarolimus, biolimus, sirolimus)", 
            choices = c("Yes", "No"), 
            selected = NULL, 
            inline = T
          ),
          radioButtons(
            ns("ex_4"), 
           "4. 조영제에 대한 아나필락시스의 기왕력이 있는 경우 (단순 알레르기 반응은 제외)", 
           choices = c("Yes", "No"), 
           selected = NULL, 
           inline = T
           ),
          radioButtons(
            ns("ex_5"), 
            "5. 임산부 및 수유부", 
            choices = c("Yes", "No"), 
            selected = NULL, 
            inline = T
            ),
          radioButtons(
          ns("ex_6"), 
          "6.	비 심장질환으로 기대 여명이 1년 미만이거나 치료에 순응도가 낮을 것으로 기대되는 자(조사자가 의학적인 판단으로 정함)", 
          choices = c("Yes", "No"), 
          selected = NULL, 
          inline = T
          ),
          radioButtons(
            ns("ex_7"), 
            "7. 연구 참여를 거부한 환자", 
            choices = c("Yes", "No"), 
            selected = NULL, 
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
              width = 6,
              textInput(ns("Initial"), "Initial", value = ifelse(is.null(hold), "", hold$Initial))
            ),
            column(
              width = 6,
              dateInput(ns("Index_PCI_Date"), "Index PCI Date", value = ifelse(is.null(hold), "", hold$Index_PCI_Date), language = "kr")
            )
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
    observeEvent(input$Initial, {
      if (input$Initial == "") {
        shinyFeedback::showFeedbackDanger(
          "Initial",
          text = "Must enter Initial of Patient!"
        )
        shinyjs::disable("submit")
      }
      else{
        shinyFeedback::hideFeedback("Initial")
      }
      if (length(input$Birthday)!=0 && length(input$Index_PCI_Date)!=0 && input$Initial != "") {
        shinyjs::enable("submit")
      }
    })
    
    observeEvent(input$Index_PCI_Date, {
      if (length(input$Index_PCI_Date)==0) {
        shinyFeedback::showFeedbackDanger(
          "Index_PCI_Date",
          text = "Must enter Index_PCI_Date"
        )
        shinyjs::disable("submit")
      } 
      else{
        shinyFeedback::hideFeedback("Index_PCI_Date")
      }
      if (length(input$Birthday)!=0 && length(input$Index_PCI_Date)!=0 && input$Initial != ""){
        shinyjs::enable("submit")
      }
    })
    
    observeEvent(input$Birthday, {
      if(length(input$Birthday)==0) {
        shinyFeedback::showFeedbackDanger(
          "Birthday",
          text = "Must enter Birthday"
        )
        shinyjs::disable("submit")
      } 
      else{
        shinyFeedback::hideFeedback("Birthday")
      }
      if (length(input$Birthday)!=0 && length(input$Index_PCI_Date)!=0 && input$Initial != ""){
        shinyjs::enable("submit")
      }
    })
    
    observeEvent(input$Sex, {
      if (is.null(input$Sex)) {
        shinyFeedback::showFeedbackDanger(
          "Sex",
          text = "Must enter Sex"
        )
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("Sex")
        shinyjs::enable("submit")
      }
    })
    
  })

  output$pidui <- renderUI({
    idlist <- choices.group <- NULL
    if (tbl == "rct") {
      type.strata <- ifelse(
        input$DM_random == "0", 
        ifelse(input$STEMI_random == "NSTEMI", "R-NDNST", "R-NDST"), 
        ifelse(input$STEMI_random == "NSTEMI", "R-DNST", "R-DST")
      )
      
      pid.group <- grep(type.strata, rd$pid, value = T)
      data.stata <- subset(data(), DM == input$DM_random & AMI_Type == input$STEMI_random)
      #idlist <- setdiff(pid.group, data()$pid)
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
  })

  observeEvent(input$CNfA, {
    updateRadioButtons(session, "ex_1", selected = "No")
    updateRadioButtons(session, "ex_2", selected = "No")
    updateRadioButtons(session, "ex_3", selected = "No")
    updateRadioButtons(session, "ex_4", selected = "No")
    updateRadioButtons(session, "ex_5", selected = "No")
    updateRadioButtons(session, "ex_6", selected = "No")
    updateRadioButtons(session, "ex_7", selected = "No")
  })

  edit_car_dat <- reactive({
    hold <- car_to_edit()

    dat <- list(
      "pid" = input$pid,
      "Group" = input$Group,
      "Index_PCI_Date" = ifelse(is.null(input$Index_PCI_Date), "", as.character(input$Index_PCI_Date)),
      "Initial" = input$Initial
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

    sqlsub <- paste(paste0(names(dat$data), "=$", 1:length(dat$data)), collapse = ",")
    
    
    code.sql <- paste0("INSERT INTO ", tbl, " (pid, 'Group', Index_PCI_Date, Initial, DM, AMI_Type, created_at, created_by, modified_at, modified_by) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)")
    if (tbl == "pros") {
      code.sql <- paste0("INSERT INTO ", tbl, " (pid, 'Group', Index_PCI_Date, Initial, created_at, created_by, modified_at, modified_by) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)")
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
