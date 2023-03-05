
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
rule_edit_module <- function(input, output, session, modal_title, rule_to_edit, modal_trigger, selected_accounts, valid_choices) {
  ns <- session$ns
  
  # print(valid_choices)

  observeEvent(modal_trigger(), {
    hold <- rule_to_edit()

    showModal(
      modalDialog(
        
        # Row 1
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("account_nickname"),
              "Account",
              # Passed in from categorize_module
              # so choices reflect the currently selected accounts
              choices = selected_accounts(),
              selected = ifelse(is.null(hold), selected_accounts()[1], hold$account_nickname)              
            ),
          ),
          column(
            width = 6,
            selectInput(
              ns("tx_type"),
              "Type",
              choices = valid_choices$tx_type,
              selected = ifelse(is.null(hold), valid_choices$tx_type[1], hold$tx_type)
            )
          )
        ),
        
        # Row 2
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("rule_name"),
              "Rule Name",
              value = ifelse(is.null(hold), "", hold$rule_name)
            )
          ),
          column(
            width = 6,
            textInput(
              ns("rule_regex"),
              "Rule Regex",
              value = ifelse(is.null(hold), "", hold$rule_regex)
            )
          )
        ),
        
        # Row 3
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("category_name"),
              "Category",
              choices = valid_choices$category_name,
              selected = ifelse(is.null(hold), valid_choices$category_name[1], hold$category_name)
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("subcategory_name"),
              "Subcategory",
              choices = "",
              selected = hold$subcategory_name %||% ""
            )
          )
        ),
        
        # Row 4
        fluidRow(
          column(
            width = 6,
            numericInput(
              ns("subscription_months"),
              "Subscription months",
              value = NULL
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
        
        # Row 5
        fluidRow(
          column(
            width = 6,
            textOutput(
              ns("created_at")
            )
          ),
          column(
            width = 6,
            textOutput(
              ns("modified_at")
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
    
    # # Update the subcategory choices based on the selected category
    observeEvent(input$category_name, {
      updateSelectInput(
        session = session,
        inputId = "subcategory_name",
        choices = valid_choices$subcategories |>
          filter(category_name == input$category_name) |> 
          pull(subcategory_name)
      )
    })

    # Observe event for "Key" text input in Add/Edit Rule Modal
    # `shinyFeedback`
    observeEvent(input$rule_name, {
      if (input$rule_name == "") {
        shinyFeedback::showFeedbackDanger(
          "rule_name",
          text = "Must enter a rule name!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("rule_name")
        shinyjs::enable('submit')
      }
    })

    # Observe event for "Rule" text input in Add/Edit Rule Modal
    # `shinyFeedback`
    observeEvent(input$rule_regex, {
      if (input$rule_regex == "") {
        shinyFeedback::showFeedbackDanger(
          "rule_regex",
          text = "Must enter a regex rule!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("rule_regex")
        shinyjs::enable('submit')
      }
    })

  })


  edit_rule_dat <- reactive({
    hold <- rule_to_edit()

    out <- list(
      is_new = is.null(hold),
      data = list(
        "account_nickname" = input$account_nickname,
        "rule_name" = input$rule_name,
        "rule_regex" = input$rule_regex,
        "category_name" = input$category_name,
        "subcategory_name" = input$subcategory_name,
        "tx_type" = input$tx_type,
        "subscription_months" = input$subscription_months,
        "tags" = input$tags        
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
    # Ensure subscription_months is positive
    dat$data$subscription_months <- abs(dat$data$subscription_months)
    # Remove any whitespace from tags
    dat$data$tags <- gsub(" ", "", dat$data$tags)

    dat
  })

  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()

    tryCatch({

      if (dat$is_new) {
        # creating a new rule
        # uid <- uuid::UUIDgenerate()
        
        # A tibble: 0 × 12
        # … with 12 variables: account_nickname <chr>,
        #   rule_name <chr>, rule_regex <chr>,
        #   category_name <chr>, subcategory_name <chr>,
        #   tx_type <chr>, subscription_months <dbl>,
        #   tags <chr>, created_at <dbl>, created_by <chr>,
        #   modified_at <dbl>, modified_by <chr>        
        
        dbExecute(
          conn,
          "INSERT INTO fct_rules (
            account_nickname, rule_name, rule_regex, 
            category_name, subcategory_name, 
            tx_type, subscription_months, tags,
            created_at, created_by,
            modified_at, modified_by
            ) VALUES
          ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)",
          params = c(
            # list(uid),
            unname(dat$data)
          )
        )
      } else {
        # editing an existing rule
        dbExecute(
          conn,
          "UPDATE fct_rules SET 
            rule_regex=$3,
            category_name=$4, subcategory_name=$5, 
            tx_type=$6, subscription_months=$7, tags=$8,
            created_at=$9, created_by=$10,
            modified_at=$11, modified_by=$12
          WHERE
            account_nickname=$1 AND rule_name=$2",
          params = unname(dat$data)
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
