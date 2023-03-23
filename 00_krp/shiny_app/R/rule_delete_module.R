
#' Rule Delete Module
#'
#' This module is for deleting a row's information from the rules database file
#'
#' @importFrom shiny observeEvent req showModal h3 modalDialog removeModal actionButton modalButton
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param rule_to_delete string - the key of the rule to be deleted
#' @param modal_trigger reactive trigger to open the modal (Delete button)
#'
#' @return None
#'
rule_delete_module <- function(input, output, session, modal_title, rule_to_delete, modal_trigger) {
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {
    # Authorize who is able to access particular buttons (here, modules)
    req(session$userData$email == 'kurt.r.peters@gmail.com')

    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2(
            style = "line-height: 1.75;",
            paste0(
              'Are you sure you want to delete the rule for "',
              rule_to_delete()$rule_name,
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
            "Delete Rule",
            class = "btn-danger",
            style="color: #fff;"
          )
        )
      )
    )
  })

  observeEvent(input$submit_delete, {
    req(rule_to_delete())

    removeModal()

    tryCatch({

      # uid <- rule_to_delete()$uid
      # DBI::dbExecute(
      #   conn,
      #   "DELETE FROM rules WHERE uid=$1",
      #   params = c(uid)
      # )

      DBI::dbExecute(
        conn,
        "DELETE FROM fct_rules WHERE account_nickname=$account_nickname AND rule_name=$rule_name",
        params = 
          c(
            account_nickname = rule_to_delete()$account_nickname,
            rule_name = rule_to_delete()$rule_name
          )
      )

      session$userData$rules_trigger(session$userData$rules_trigger() + 1)
      showToast("success", "Rule Successfully Deleted")
    }, error = function(error) {

      msg <- "Error Deleting Rule"
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
