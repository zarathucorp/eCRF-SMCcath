
#' Car Delete Module
#'
#' This module is for deleting a row's information from the mtcars database file
#'
#' @importFrom shiny observeEvent req showModal h3 modalDialog removeModal actionButton modalButton
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param car_to_delete string - the model of the car to be deleted
#' @param modal_trigger reactive trigger to open the modal (Delete button)
#'
#' @return None
#'
car_delete_module <- function(input, output, session, modal_title, car_to_delete, modal_trigger, tbl = "rct", sessionid) {
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {
    # Authorize who is able to access particular buttons (here, modules)
    req(sessionid == 'admin')

    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2(
            style = "line-height: 1.75;",
            paste0(
              'Are you sure you want to delete the "',
              car_to_delete()$pid,
              '"?'
            )
          )
        ),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("submit_delete"),
            "Delete patient",
            class = "btn-danger",
            style="color: #fff;"
          )
        )
      )
    )
  })

  observeEvent(input$submit_delete, {
    req(car_to_delete())

    removeModal()

    tryCatch({

      pid <- car_to_delete()$pid

      DBI::dbExecute(
        conn,
        paste0("DELETE FROM ", tbl," WHERE pid=$1"),
        params = c(pid)
      )

      session$userData$mtcars_trigger(session$userData$mtcars_trigger() + 1)
      showToast("success", "Patient Successfully Deleted")
    }, error = function(error) {

      msg <- "Error Deleting Patient"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
  })
}
