#' Cars Table Module UI
#'
#' The UI portion of the module for displaying the mtcars datatable
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id The id for this module
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#'

library(magrittr)
library(shinycssloaders)
library(DT)
library(tibble)
library(shiny)
library(purrr)
library(dplyr)

cars_table_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::downloadButton(ns('mydownload'), "Download"),
    fluidRow(
      column(
        width = 2,
        actionButton(
          ns("add_patient"),
          "Add Patient",
          class = "btn-success",
          style = "color: #fff;",
          icon = icon("plus"),
          width = "100%"
        ),
        tags$br(),
        tags$br()
      )
    ),
    fluidRow(
      column(
        width = 12,
        title = "Data",
        DTOutput(ns("car_table")) %>%
          withSpinner(),
        tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "cars_table_module.js"),
    tags$script(paste0("cars_table_module_js('", ns(""), "')"))
  )
}

#' Cars Table Module Server
#'
#' The Server portion of the module for displaying the mtcars datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#'
#' @param None
#'
#' @return None

cars_table_module <- function(input, output, session, tbl = "rct", sessionid) {

  # trigegr to reload data from the "mtcars" table
  session$userData$mtcars_trigger <- reactiveVal(0)

  # Read in "mtcars" table from the database
  cars <- reactive({
    session$userData$mtcars_trigger()

    out <- NULL
    
    if(tbl != 'rct'){
      tryCatch(
        {
          out <- conn %>%
            tbl('pros') %>%
            collect() %>%
            rbind(conn %>% tbl('rct') %>% collect()) %>%
            mutate(
              created_at = as.POSIXct(created_at, tz = "UTC"),
              modified_at = as.POSIXct(modified_at, tz = "UTC")
            ) %>%
            arrange(desc(modified_at))
        },
        error = function(err) {
          msg <- "Database Connection Error"
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
    if(tbl=='rct'){
      tryCatch(
        {
          out <- conn %>%
            tbl(tbl) %>%
            collect() %>%
            mutate(
              created_at = as.POSIXct(created_at, tz = "UTC"),
              modified_at = as.POSIXct(modified_at, tz = "UTC")
            ) %>%
            arrange(desc(modified_at))
        },
        error = function(err) {
          msg <- "Database Connection Error"
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
    

    out
  })

  output$mydownload <- shiny::downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(cars(), file)
    }
  )
  
  car_table_prep <- reactiveVal(NULL)

  observeEvent(cars(), {
    out <- cars() # one observation

    ids <- out$pid

    # data에 입력 없을시 Error
    # ids.na <- ids[apply(select(out, Initial:Comment_demo), 1, function(x) {
    # Withdrawal State 까지만 입력시 Green 으로 색상 변경

    ids.na <- ids[apply(select(out, Initial:Withdrawal), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # button color
    actions <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit demographics">',
        '<button class="btn btn-', btn.demo, ' edits_btn" data-toggle="tooltip" data-placement="top" title="Edit demographics" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })

    # adm에 입력 없을시 Warning
    ids.na.adm <- ids[apply(select(out, Date_adm:PTroT_adm), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # adm button color
    adm <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.adm, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit admission">',
        '<button class="btn btn-', btn.demo, ' edit_btnadm" data-toggle="tooltip" data-placement="top" title="Edit admission" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })

    # angio에 입력 없을시 Warning
    ids.na.ang <- ids[apply(select(out, Date_ang:Non_Cul_cnt_ang), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # ang button color
    ang <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.ang, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit angiographic">',
        '<button class="btn btn-', btn.demo, ' edit_btnang" data-toggle="tooltip" data-placement="top" title="Edit angiographic" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })

    # Cul1 에 입력 없을시 Warning

    ids.na.cul1 <- ids[apply(select(out, Vessel_cul1:Perforation_cul1), 1, function(x) {
      any(is.na(x) | x == "")
    })]
    
    
    ids.na.cul1 <- ids[apply(select(out, Cul_cnt_ang), 1, function(x){
      any(is.na(x) | x < 1)
    })]
    
    # 1 or 0 이면 cul 2 <- primary 가 나와야 함.
    

    # cul1 button color
    cul1 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.cul1, "primary", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit Culprit1">',
        '<button class="btn btn-', btn.demo, ' edit_btncul1" data-toggle="tooltip" data-placement="top" title="Edit Culprit1" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })

    # cul2 에 입력 없을시 Warning
    
    ids.na.cul2 <- ids[apply(select(out, Vessel_cul2:Perforation_cul2), 1, function(x) {
      any(is.na(x) | x == "")
    })]
    
    # ids <- out$pid
    
    ids.na.cul2 <- ids[apply(select(out, Cul_cnt_ang), 1, function(x){
      any(is.na(x) | x < 2)
    })]
    
    
    # 1 or 0 이면 cul 2 <- primary 가 나와야 함.
    
    # cul2 button color
    cul2 <- sapply(ids, function(id_) {
      
      # btn.demo <- ifelse(id_ %in% ids.na.cul2, "primary", "success")
      btn.demo <- ifelse(id_ %in% ids.na.cul2, 'primary','success')
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit Culprit2">',
        '<button class="btn btn-', btn.demo, ' edit_btncul2" data-toggle="tooltip" data-placement="top" title="Edit Culprit2" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })    

    # culn1 에 입력 없을시 Warning
    
    ids.na.culn1 <- ids[apply(select(out, Vessel_culn1:Perforation_culn1), 1, function(x) {
      any(is.na(x) | x == "")
    })]
    
    # culn1 button color
    culn1 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.culn1, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit Non-Culprit1">',
        '<button class="btn btn-', btn.demo, ' edit_btnculn1" data-toggle="tooltip" data-placement="top" title="Edit Non-Culprit1" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })
    
    # culn2 에 입력 없을시 Warning
    
    ids.na.culn2 <- ids[apply(select(out, Vessel_culn2:Perforation_culn2), 1, function(x) {
      any(is.na(x) | x == "")
    })]
    
    # culn2 button color
    culn2 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.culn2, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit Non-Culprit2">',
        '<button class="btn btn-', btn.demo, ' edit_btnculn2" data-toggle="tooltip" data-placement="top" title="Edit Non-Culprit2" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })
    
    # culn3 에 입력 없을시 Warning
    
    ids.na.culn3 <- ids[apply(select(out, Vessel_culn3:Perforation_culn3), 1, function(x) {
      any(is.na(x) | x == "")
    })]
    
    # culn3 button color
    culn3 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.culn3, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit Non-Culprit3">',
        '<button class="btn btn-', btn.demo, ' edit_btnculn3" data-toggle="tooltip" data-placement="top" title="Edit Non-Culprit3" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })
    
    # culn4 에 입력 없을시 Warning
    
    ids.na.culn4 <- ids[apply(select(out, Vessel_culn4:Perforation_culn4), 1, function(x) {
      any(is.na(x) | x == "")
    })]
    
    # culn4 button color
    culn4 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.culn4, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit Non-Culprit4">',
        '<button class="btn btn-', btn.demo, ' edit_btnculn4" data-toggle="tooltip" data-placement="top" title="Edit Non-Culprit4" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })
    
    # outc에 입력 없을시 Warning
    ids.na.outc <- ids[apply(select(out, Discharge_out:Comment_out), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # outc button color
    outc <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.outc, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit discharge">',
        '<button class="btn btn-', btn.demo, ' edit_btnoutc" data-toggle="tooltip" data-placement="top" title="Edit discharge" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })


    # Event에 입력 없을시 Warning
    # ids.na.event <- ids[apply(select(out, Last_FU_Date:TLF_Date), 1, function(x) {
    #  any(is.na(x) | x == "")
    # })]

    # event button color
    # events <- sapply(ids, function(id_) {
    #  btn.demo <- ifelse(id_ %in% ids.na.event, "warning", "success")
    #  paste0(
    #    "<center>",
    #    '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit events">',
    #    '<button class="btn btn-', btn.demo, ' edit_btn" data-toggle="tooltip" data-placement="top" title="Edit events" id = ', id_, ' style="margin: 0">',
    #    '<i class="fa fa-pencil-square-o"></i>',
    #    "</button>",
    #    "</div>",
    #    "</center>"
    #  )
    # })

    # Lab에 입력 없을시 Warning
    # ids.na.lab <- ids[apply(select(out, Lab_Date:Lactic_Acid_Peak), 1, function(x) {
    #  any(is.na(x) | x == "")
    # })]

    # Lab button color
    # labs <- sapply(ids, function(id_) {
    #  btn.demo <- ifelse(id_ %in% ids.na.lab, "warning", "success")
    #  paste0(
    #    "<center>",
    #    '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit labs">',
    #    '<button class="btn btn-', btn.demo, ' editl_btn" data-toggle="tooltip" data-placement="top" title="Edit labs" id = ', id_, ' style="margin: 0">',
    #    '<i class="fa fa-pencil-square-o"></i>',
    #    "</button>",
    #    "</div>",
    #    "</center>"
    #  )
    # })

    ## M1

    # M1에 입력 없을시 Warning
    ids.na.m1 <- ids[apply(select(out, FU_M1:Comment_M1), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # M1 button color
    m1 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.m1, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit M1">',
        '<button class="btn btn-', btn.demo, ' editm1_btn" data-toggle="tooltip" data-placement="top" title="Edit M1" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })

    ## M3

    # M3에 입력 없을시 Warning
    ids.na.m3 <- ids[apply(select(out, FU_M3:Comment_M3), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # M3 button color
    m3 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.m3, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit M3">',
        '<button class="btn btn-', btn.demo, ' editm3_btn" data-toggle="tooltip" data-placement="top" title="Edit M3" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })


    ## M6

    # M6에 입력 없을시 Warning
    ids.na.m6 <- ids[apply(select(out, FU_M6:Comment_M6), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # M6 button color
    m6 <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.m6, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit M6">',
        '<button class="btn btn-', btn.demo, ' editm6_btn" data-toggle="tooltip" data-placement="top" title="Edit M6" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })

    ## Mf (Final)

    # Mf에 입력 없을시 Warning
    ids.na.mf <- ids[apply(select(out, FU_Mf:Comment_Mf), 1, function(x) {
      any(is.na(x) | x == "")
    })]

    # Mf button color
    mf <- sapply(ids, function(id_) {
      btn.demo <- ifelse(id_ %in% ids.na.mf, "warning", "success")
      paste0(
        "<center>",
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Edit Mf">',
        '<button class="btn btn-', btn.demo, ' editmf_btn" data-toggle="tooltip" data-placement="top" title="Edit Mf" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-pencil-square-o"></i>',
        "</button>",
        "</div>",
        "</center>"
      )
    })

    # delete button
    deletes <- sapply(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Delete row">
          <button class="btn btn-danger delete_btn" data-toggle="tooltip" data-placement="top" title="Delete row" id = ', id_, ' style="margin: 0">',
        '<i class="fa fa-trash-o"></i>',
        "</button>",
        "</div>"
      )
    })

    # Remove the `uid` column. We don't want to show this column to the user
    # out <- out %>%
    #  select(-pid)
    
    
    # Set the Action Buttons row to the first column of the `mtcars` table
    out <- cbind(
      tibble(" " = deletes),
      out[, c(1, 2, 3, 6, 8, 9)], # pid, Group, CENTER, Initial, Age, Sex
      `Demographics` = actions,
      `Admission` = adm,
      `Angiographics` = ang,
      `Discharge` = outc,
      `Culprit1` = cul1,
      `Culprit2` = cul2,
      `Non-Culprit1` = culn1,
      `Non-Culprit2` = culn2,
      `Non-Culprit3` = culn3,
      `Non-Culprit4` = culn4,
      `1m-fu` = m1,
      `3m-fu` = m3,
      `6m-fu` = m6,
      `scv` = mf,
      out[,(ncol(out)-4):ncol(out)] # Created at, Created by, Modified at, Modified by
    )
    
    # Data is empty
    if (is.null(car_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      car_table_prep(out)
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(car_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
    
    callModule(
      patientsNumber_plot_module,
      "Hospital1",
      data = out[, c("Center", "created_at")]
    )
    
    callModule(
      patientsByDate_plot_module,
      "Date",
      data = out[, c("Center", "created_at")]
    )
  })
  
  output$car_table <- renderDT({
    req(car_table_prep())
    out <- car_table_prep()

    datatable(
      out,
      rownames = FALSE,
      colnames = names(out),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -which(names(out) %in% c(" ", "Demographics", "Admission", 
                                        "Angiographics", "Culprit1", "Culprit2", 
                                        "Non-Culprit1", "Non-Culprit2", "Non-Culprit3", "Non-Culprit4", 
                                        "Discharge", "1m-fu", "3m-fu", "6m-fu", "scv")),
      # extensions = c("Buttons"),
      options = list(
        scrollX = TRUE,
        dom = "Bftip",
        columnDefs = list(
          # not sortable columns 
          list(
            targets = which(names(out) %in% 
                              c(" ", "Demographics", "Admission", 
                                "Angiographics", "Culprit1", "Culprit2", 
                                "Non-Culprit1", "Non-Culprit2", "Non-Culprit3", "Non-Culprit4", 
                                "Discharge", "1m-fu", "3m-fu", "6m-fu", "scv")) - 1, 
            orderable = FALSE
          ),
          # show these columns
          list(
            targets = which(!(names(out) %in% 
                                c(" ", 
                                  "pid", "Group", "Center", "Initial", "Age", "Sex",
                                  "Demographics", "Admission", 
                                  "Angiographics", "Culprit1", "Culprit2", 
                                  "Non-Culprit1", "Non-Culprit2", "Non-Culprit3", "Non-Culprit4", 
                                  "Discharge", "1m-fu", "3m-fu", "6m-fu", "scv", 
                                  "created_at", "created_by", "modified_at", "modified_by"))) - 1, 
            visible = F
          )
        ),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }")
      )
    ) %>%
      formatDate(
        columns = c("created_at", "modified_at"),
        method = "toLocaleString"
      )
  })

  
  
  car_table_proxy <- DT::dataTableProxy("car_table")

  callModule(
    add_initialedit_module,
    "add_patient",
    modal_title = "Add Patient",
    car_to_edit = function() NULL,
    modal_trigger = reactive({
      input$add_patient
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid,
    rd = rd
  )

  # Demographics

  car_to_edit <- eventReactive(input$car_id_to_edit, {
    cars() %>%
      filter(pid == input$car_id_to_edit)
  })

  callModule(
    demographics_edit_module,
    "edit_car",
    modal_title = "Edit Demographics",
    car_to_edit = car_to_edit,
    modal_trigger = reactive({
      input$car_id_to_edit
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )

  ## Admission

  car_to_edit_adm <- eventReactive(input$car_id_to_edit_adm, {
    cars() %>%
      filter(pid == input$car_id_to_edit_adm)
  })

  callModule(
    adm_edit_module,
    "edit_adm",
    modal_title = "Edit Admission",
    car_to_edit = car_to_edit_adm,
    modal_trigger = reactive({
      input$car_id_to_edit_adm
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )

  ## Angio

  car_to_edit_ang <- eventReactive(input$car_id_to_edit_ang, {
    cars() %>%
      filter(pid == input$car_id_to_edit_ang)
  })

  callModule(
    ang_edit_module,
    "edit_ang",
    modal_title = "Edit Angiographics",
    car_to_edit = car_to_edit_ang,
    modal_trigger = reactive({
      input$car_id_to_edit_ang
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )

  # cul1
  car_to_edit_cul1 <- eventReactive(input$car_id_to_edit_cul1, {
    cars() %>%
      filter(pid == input$car_id_to_edit_cul1)
  })

  callModule(
    cul1_edit_module,
    "edit_cul1",
    modal_title = "Edit Culprit1",
    car_to_edit = car_to_edit_cul1,
    modal_trigger = reactive({
      input$car_id_to_edit_cul1
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )
  
  # cul2
  car_to_edit_cul2 <- eventReactive(input$car_id_to_edit_cul2, {
    cars() %>%
      filter(pid == input$car_id_to_edit_cul2)
  })
  
  callModule(
    cul2_edit_module,
    "edit_cul2",
    modal_title = "Edit Culprit2",
    car_to_edit = car_to_edit_cul2,
    modal_trigger = reactive({
      input$car_id_to_edit_cul2
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )

  # culn1
  car_to_edit_culn1 <- eventReactive(input$car_id_to_edit_culn1, {
    cars() %>%
      filter(pid == input$car_id_to_edit_culn1)
  })
  
  callModule(
    culn1_edit_module,
    "edit_culn1",
    modal_title = "Edit Non-Culprit1",
    car_to_edit = car_to_edit_culn1,
    modal_trigger = reactive({
      input$car_id_to_edit_culn1
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )
  
  # culn2
  car_to_edit_culn2 <- eventReactive(input$car_id_to_edit_culn2, {
    cars() %>%
      filter(pid == input$car_id_to_edit_culn2)
  })
  
  callModule(
    culn2_edit_module,
    "edit_culn2",
    modal_title = "Edit Non-Culprit2",
    car_to_edit = car_to_edit_culn2,
    modal_trigger = reactive({
      input$car_id_to_edit_culn2
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )
  
  # culn2
  car_to_edit_culn2 <- eventReactive(input$car_id_to_edit_culn2, {
    cars() %>%
      filter(pid == input$car_id_to_edit_culn2)
  })
  
  callModule(
    culn2_edit_module,
    "edit_culn2",
    modal_title = "Edit Non-Culprit2",
    car_to_edit = car_to_edit_culn2,
    modal_trigger = reactive({
      input$car_id_to_edit_culn2
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )
  
  # culn3
  car_to_edit_culn3 <- eventReactive(input$car_id_to_edit_culn3, {
    cars() %>%
      filter(pid == input$car_id_to_edit_culn3)
  })
  
  callModule(
    culn3_edit_module,
    "edit_culn3",
    modal_title = "Edit Non-Culprit3",
    car_to_edit = car_to_edit_culn3,
    modal_trigger = reactive({
      input$car_id_to_edit_culn3
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )
  
  car_to_edit_culn4 <- eventReactive(input$car_id_to_edit_culn4, {
    cars() %>%
      filter(pid == input$car_id_to_edit_culn4)
  })
  
  callModule(
    culn4_edit_module,
    "edit_culn4",
    modal_title = "Edit Non-Culprit4",
    car_to_edit = car_to_edit_culn4,
    modal_trigger = reactive({
      input$car_id_to_edit_culn4
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )
  
  # Outcomes

  car_to_edit_outc <- eventReactive(input$car_id_to_edit_outc, {
    cars() %>%
      filter(pid == input$car_id_to_edit_outc)
  })

  callModule(
    outc_edit_module,
    "edit_outc",
    modal_title = "Edit Discharge",
    car_to_edit = car_to_edit_outc,
    modal_trigger = reactive({
      input$car_id_to_edit_outc
    }),
    tbl = tbl,
    data = cars,
    sessionid = sessionid
  )

  ## EVENT
  # car_to_edit_event <- eventReactive(input$car_id_to_edit_event, {
  #  cars() %>%
  #    filter(pid == input$car_id_to_edit_event)
  # })

  # callModule(
  #  event_edit_module,
  #  "edit_event",
  #  modal_title = "Edit Event",
  #  car_to_edit = car_to_edit_event,
  #  data = cars,
  #  modal_trigger = reactive({
  #    input$car_id_to_edit_event
  #  }),
  #  tbl = tbl,
  #  sessionid = sessionid
  # )

  ## LAB
  # car_to_edit_lab <- eventReactive(input$car_id_to_edit_lab, {
  #  cars() %>%
  #    filter(pid == input$car_id_to_edit_lab)
  # })

  # callModule(
  #  lab_edit_module,
  #  "edit_lab",
  #  modal_title = "Edit Lab",
  #  car_to_edit = car_to_edit_lab,
  #  data = cars,
  #  modal_trigger = reactive({
  #    input$car_id_to_edit_lab
  #  }),
  #  tbl = tbl,
  #  sessionid = sessionid
  # )

  ## M1
  car_to_edit_m1 <- eventReactive(input$car_id_to_edit_m1, {
    cars() %>%
      filter(pid == input$car_id_to_edit_m1)
  })

  callModule(
    m1_edit_module,
    "edit_m1",
    modal_title = "Edit M1",
    car_to_edit = car_to_edit_m1,
    data = cars,
    modal_trigger = reactive({
      input$car_id_to_edit_m1
    }),
    tbl = tbl,
    sessionid = sessionid
  )

  ## M3
  car_to_edit_m3 <- eventReactive(input$car_id_to_edit_m3, {
    cars() %>%
      filter(pid == input$car_id_to_edit_m3)
  })

  callModule(
    m3_edit_module,
    "edit_m3",
    modal_title = "Edit M3",
    car_to_edit = car_to_edit_m3,
    data = cars,
    modal_trigger = reactive({
      input$car_id_to_edit_m3
    }),
    tbl = tbl,
    sessionid = sessionid
  )

  ## M6
  car_to_edit_m6 <- eventReactive(input$car_id_to_edit_m6, {
    cars() %>%
      filter(pid == input$car_id_to_edit_m6)
  })

  callModule(
    m6_edit_module,
    "edit_m6",
    modal_title = "Edit M6",
    car_to_edit = car_to_edit_m6,
    data = cars,
    modal_trigger = reactive({
      input$car_id_to_edit_m6
    }),
    tbl = tbl,
    sessionid = sessionid
  )

  ## Mf
  car_to_edit_mf <- eventReactive(input$car_id_to_edit_mf, {
    cars() %>%
      filter(pid == input$car_id_to_edit_mf)
  })

  callModule(
    mf_edit_module,
    "edit_mf",
    modal_title = "Edit Mf",
    car_to_edit = car_to_edit_mf,
    data = cars,
    modal_trigger = reactive({
      input$car_id_to_edit_mf
    }),
    tbl = tbl,
    sessionid = sessionid
  )

  car_to_delete <- eventReactive(input$car_id_to_delete, {
    out <- cars() %>%
      filter(pid == input$car_id_to_delete) %>%
      as.list()
  })

  callModule(
    car_delete_module,
    "delete_car",
    modal_title = "Delete Patient",
    car_to_delete = car_to_delete,
    modal_trigger = reactive({
      input$car_id_to_delete
    }),
    tbl = tbl,
    sessionid = sessionid
  )
}
