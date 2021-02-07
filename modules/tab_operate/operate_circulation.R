operate_circulation_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Ausleihen & Zurückgeben",
    shiny::uiOutput(
      outputId = ns("user"),
    ),
    shiny::uiOutput(
      outputId = ns("type")
    ),
    shiny::uiOutput(
      outputId = ns("subtype")
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::actionButton(
          inputId = ns("borrow"),
          label = "Ausleihen",
          icon = shiny::icon("sign-out-alt"),
          width = "100%",
          class = "borrow-btn"
        )
      ),
      shiny::column(
        width = 6,
        shiny::actionButton(
          inputId = ns("return"),
          label = "Zurückgeben",
          icon = shiny::icon("sign-in-alt"),
          width = "100%",
          class = "return-btn"
        )
      )
    )
  )
}

operate_circulation_server <- function(id, .values, trigger_type_id_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_r <- shiny::reactive({
        .values$update$user()
        db_get_users(.values$db)
      })

      output$user <- shiny::renderUI({
        if (.values$user$status() == "admin") {
          shiny::selectInput(
            inputId = ns("user"),
            label = "Ausführender Nutzer",
            choices = user_r(),
            selected = .values$user$id()
          )
        }
      })

      user_id_r <- shiny::reactive({
        if (.values$user$status() == "admin") {
          input$user
        } else {
          .values$user$id()
        }
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, user_id_r())
      })

      types_r <- shiny::reactive({
        .values$update$type()
        db_get_types(.values$db)
      })

      output$type <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("type"),
          label = "Typ",
          choices = types_r(),
          selected = .values$query$type()
        )
      })

      shiny::observeEvent(trigger_type_id_r(), {
        shiny::updateSelectInput(
          inputId = "type",
          selected = trigger_type_id_r()
        )
      })

      type_id_r <- shiny::reactive({
        shiny::req(input$type)
      })

      type_name_r <- shiny::reactive({
        names(types_r())[types_r() == type_id_r()]
      })

      subtypes_r <- shiny::reactive({
        .values$update$subtype()
        db_get_subtypes_by_type_id(.values$db, type_id_r())
      })

      output$subtype <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("subtype"),
          label = "Untertyp",
          choices = subtypes_r()
        )
      })

      subtype_name_r <- shiny::reactive({
        names(subtypes_r())[subtypes_r() == shiny::req(input$subtype)]
      })

      shiny::observeEvent(input$borrow, {
        borrow_rv(TRUE)
        shiny::showModal(modal_dialog_r())
      })

      shiny::observeEvent(input$return, {
        borrow_rv(FALSE)
        shiny::showModal(modal_dialog_r())
      })

      borrow_rv <- shiny::reactiveVal(NULL)

      modal_dialog_r <- shiny::reactive({
        borrow <- borrow_rv()

        shiny::modalDialog(
          title = if (borrow) "Ausleihen" else "Zurückgeben",
          easyClose = TRUE,
          shinyjs::disabled(
            shiny::textInput(
              inputId = "undefined",
              label = "Nutzer",
              value = user_name_r()
            )
          ),
          shinyjs::disabled(
            shiny::textInput(
              inputId = "undefined",
              label = "Typ",
              value = type_name_r()
            )
          ),
          shinyjs::disabled(
            shiny::textInput(
              inputId = "undefined",
              label = "Untertyp",
              value = subtype_name_r()
            )
          ),
          object_quantity_input_ui(
            id = ns("object_quantity_input"),
            old_quantity = 1,
            label = if (borrow) borrow_label_r() else return_label_r()
          ),
          footer = shiny::uiOutput(
            outputId = ns("footer"),
            style = "width: 100%"
          )
        )
      })

      output$footer <- shiny::renderUI({
        borrow <- borrow_rv()
        footer <- if (borrow) borrow_footer_r() else return_footer_r()
        if (error_r()) footer <- shinyjs::disabled(footer)
        footer
      })

      error_r <- shiny::reactive({
        quantity_return$error_r()
      })

      borrow_label_r <- shiny::reactive({
        paste0(
          "Menge (",
          max_borrow_r(),
          " verfügbar)"
        )
      })

      return_label_r <- shiny::reactive({
        paste0(
          "Menge (",
          max_return_r(),
          " ausgeliehen)"
        )
      })

      borrow_footer_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("confirm_borrow"),
          label = "Ausleihen bestätigen",
          icon = shiny::icon("sign-out-alt"),
          class = "borrow-btn",
          width = "100%"
        )
      })

      return_footer_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("confirm_return"),
          label = "Zurückgeben bestätigen",
          icon = shiny::icon("sign-in-alt"),
          class = "return-btn",
          width = "100%"
        )
      })

      shiny::observeEvent(input$confirm_borrow, {
        shiny::removeModal()

        db_add_circulation(
          db = .values$db,
          user_id = user_id_r(),
          subtype_id = input$subtype,
          quantity = quantity_return$quantity_r()
        )

        .values$update$circulation(.values$update$circulation() + 1)

        shiny::showNotification(
          ui = operate_notification_text(
            borrow = TRUE,
            status = .values$user$status(),
            user_name = user_name_r(),
            quantity = quantity_return$quantity_r(),
            type_name = type_name_r(),
            subtype_name = subtype_name_r()
          ),
          duration = 5,
          type = "warning"
        )
      })

      shiny::observeEvent(input$confirm_return, {
        shiny::removeModal()

        db_add_circulation(
          db = .values$db,
          user_id = user_id_r(),
          subtype_id = input$subtype,
          quantity = -quantity_return$quantity_r()
        )

        .values$update$circulation(.values$update$circulation() + 1)

        shiny::showNotification(
          ui = operate_notification_text(
            borrow = FALSE,
            status = .values$user$status(),
            user_name = user_name_r(),
            quantity = quantity_return$quantity_r(),
            type_name = type_name_r(),
            subtype_name = subtype_name_r()
          ),
          duration = 5,
          type = "warning"
        )
      })

      max_r <- shiny::reactive({
        if (borrow_rv()) max_borrow_r() else max_return_r()
      })

      max_borrow_r <- shiny::reactive({
        .values$update$subtype()
        .values$update$circulation()
        db_get_available_quantity(
          db = .values$db,
          subtype_id = input$subtype
        )
      })

      max_return_r <- shiny::reactive({
        .values$update$subtype()
        .values$update$circulation()
        db_get_borrowed_quantity_by_user_id(
          db = .values$db,
          user_id = .values$user$id(),
          subtype_id = input$subtype
        )
      })

      max_message_r <- shiny::reactive({
        if (borrow_rv()) {
          "Du kannst nicht mehr Einheiten ausleihen als verfügbar sind!\n\n"
        } else {
          "Du kannst nicht mehr Einheiten zurückgeben als Du ausgeliehen hast!\n\n"
        }
      })

      min_message_r <- shiny::reactive({
        if (borrow_rv()) {
          "Du musst mindestens eine Einheit ausleihen!"
        } else {
          "Du musst mindestens eine Einheit zurückgeben!"
        }
      })

      quantity_return <- object_quantity_input_server(
        id = "object_quantity_input",
        .values = .values,
        max_r = max_r,
        max_message_r = max_message_r,
        min_r = shiny::reactive(1),
        min_message_r = min_message_r
      )

      return_list <- list(
        type_id_r = type_id_r
      )

      return(return_list)
    }
  )
}

operate_notification_text <- function(borrow,
                                      status,
                                      user_name,
                                      quantity,
                                      type_name,
                                      subtype_name
) {
  verb <- if (borrow) "ausgeliehen." else "zurückgegeben."

  if (status == "admin") {
    paste(
      "Du hast für",
      user_name,
      quantity,
      type_name,
      ":",
      subtype_name,
      verb
    )
  } else {
    paste(
      "Du hast",
      quantity,
      type_name,
      ":",
      subtype_name,
      verb
    )
  }
}
