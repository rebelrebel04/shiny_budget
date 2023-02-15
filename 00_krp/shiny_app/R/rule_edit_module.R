
#' Rule Add & Edit Module
#'
#' Module to add & edit rules in the rules database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param car_to_edit reactive returning a 1 row data frame of the car to edit
#' from the "rules" table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#'
rule_edit_module <- function(input, output, session, modal_title, rule_to_edit, modal_trigger) {
  ns <- session$ns

  observeEvent(modal_trigger(), {
    hold <- rule_to_edit()

    showModal(
      modalDialog(
        
        # Row 1
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("key"),
              "Key",
              value = ifelse(is.null(hold), "", hold$key)
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("expense_type"),
              "Type",
              choices = c("expense", "income"),
              selected = ifelse(is.null(hold), "expense", hold$expense_type)
            )
          )
        ),
        
        # Row 2
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("category"),
              "Category",
              value = ifelse(is.null(hold), "", hold$category)
            )
          ),
          column(
            width = 6,
            textInput(
              ns("subcategory"),
              "Subcategory",
              value = ifelse(is.null(hold), "", hold$subcategory)
            )
          )
        ),
        
        # Row 3
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("rule"),
              "Rule",
              value = ifelse(is.null(hold), "", hold$rule)
            )
          ),
          column(
            width = 6,
            textInput(
              ns("tags"),
              "Tags",
              value = ifelse(is.null(hold), "", hold$tags)
            )
          )
        ),
        
        title = modal_title,
        size = 'm',
        footer = list(
          modalButton('Cancel'),
          actionButton(
            ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )

    # Observe event for "Key" text input in Add/Edit Rule Modal
    # `shinyFeedback`
    observeEvent(input$key, {
      if (input$key == "") {
        shinyFeedback::showFeedbackDanger(
          "key",
          text = "Must enter a key!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("key")
        shinyjs::enable('submit')
      }
    })

    # Observe event for "Rule" text input in Add/Edit Rule Modal
    # `shinyFeedback`
    observeEvent(input$rule, {
      if (input$rule == "") {
        shinyFeedback::showFeedbackDanger(
          "rule",
          text = "Must enter a regex rule!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("rule")
        shinyjs::enable('submit')
      }
    })

  })


  edit_rule_dat <- reactive({
    hold <- rule_to_edit()

    out <- list(
      uid = if (is.null(hold)) NA else hold$uid,
      data = list(
        "key" = input$key,
        "category" = input$category,
        "subcategory" = input$subcategory,
        "expense_type" = input$expense_type,
        "tags" = input$tags,
        "rule" = input$rule
      )
    )

    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))

    if (is.null(hold)) {
      # adding a new rule

      out$data$created_at <- time_now
      out$data$created_by <- session$userData$email
    } else {
      # Editing existing rule

      out$data$created_at <- as.character(hold$created_at)
      out$data$created_by <- hold$created_by
    }

    out$data$modified_at <- time_now
    out$data$modified_by <- session$userData$email

    out
  })

  validate_edit <- eventReactive(input$submit, {
    dat <- edit_rule_dat()

    # Logic to validate inputs...

    dat
  })

  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()

    tryCatch({

      if (is.na(dat$uid)) {
        # creating a new rule
        uid <- uuid::UUIDgenerate()

        dbExecute(
          conn,
          "INSERT INTO rules (uid, key, category, subcategory, expense_type, tags, rule, created_at, created_by, modified_at, modified_by) VALUES
          ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)",
          params = c(
            list(uid),
            unname(dat$data)
          )
        )
      } else {
        # editing an existing rule
        dbExecute(
          conn,
          "UPDATE rules SET key=$1, category=$2, subcategory=$3, expense_type=$4, tags=$5, rule=$6, created_at=$7, created_by=$8,
          modified_at=$9, modified_by=$10 WHERE uid=$11",
          params = c(
            unname(dat$data),
            list(dat$uid)
          )
        )
      }

      session$userData$rules_trigger(session$userData$rules_trigger() + 1)
      showToast("success", paste0(modal_title, " Successs"))
    }, error = function(error) {

      msg <- paste0(modal_title, " Error")


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
