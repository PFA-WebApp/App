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
    shiny::uiOutput(
      outputId = ns("buttons")
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

      # Otherwise input$type might be NULL even though choices are displayed
      # Reproduce: after app start go to later tab and then to operate tab
      shiny::outputOptions(
        x = output,
        name = "type",
        suspendWhenHidden = FALSE
      )

      shiny::observeEvent(trigger_type_id_r(), {
        shiny::updateSelectInput(
          inputId = "type",
          selected = trigger_type_id_r()
        )
      })

      type_id_r <- shiny::reactive({
        input$type %||% numeric()
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

      subtype_id_r <- shiny::reactive({
        shiny::req(input$subtype)
      })

      subtype_name_r <- shiny::reactive({
        names(subtypes_r())[subtypes_r() == shiny::req(input$subtype)]
      })

      output$buttons <- shiny::renderUI({
        if (.values$user$status() == "admin") {
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = ns("borrow"),
                label = "Ausleihen",
                icon = shiny::icon("sign-out-alt"),
                width = "100%",
                class = "borrow-btn"
              )
            ),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = ns("write_off"),
                label = "Abschreiben",
                icon = shiny::icon("balance-scale-right"),
                width = "100%",
                class = "write-off-btn"
              )
            ),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = ns("return"),
                label = "Zurückgeben",
                icon = shiny::icon("sign-in-alt"),
                width = "100%",
                class = "return-btn"
              )
            )
          )
        } else {
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
        }
      })

      shiny::observeEvent(input$borrow, {
        operate_rv(1)
        shiny::showModal(modal_dialog_r())
      })

      shiny::observeEvent(input$return, {
        operate_rv(2)
        shiny::showModal(modal_dialog_r())
      })

      shiny::observeEvent(input$write_off, {
        operate_rv(3)
        shiny::showModal(modal_dialog_r())
      })

      operate_rv <- shiny::reactiveVal(1)

      modal_dialog_r <- shiny::reactive({
        operate_index <- operate_rv()

        title <- c("Ausleihen", "Zurückgeben", "Abschreiben")

        shiny::modalDialog(
          title = title[operate_index],
          easyClose = TRUE,
          shinyjs::disabled(
            shiny::textInput(
              inputId = "disabled",
              label = "Nutzer",
              value = user_name_r()
            )
          ),
          shinyjs::disabled(
            shiny::textInput(
              inputId = "disabled",
              label = "Typ",
              value = type_name_r()
            )
          ),
          shinyjs::disabled(
            shiny::textInput(
              inputId = "disabled",
              label = "Untertyp",
              value = subtype_name_r()
            )
          ),
          object_quantity_input_ui(
            id = ns("object_quantity_input"),
            old_quantity = 1,
            label = switch(
              operate_index,
              "1" = borrow_label_r(),
              "2" = return_label_r(),
              "3" = write_off_label_r()
            )
          ),
          footer = shiny::uiOutput(
            outputId = ns("footer"),
            style = "width: 100%"
          )
        )
      })

      output$footer <- shiny::renderUI({
        footer <- switch(
          operate_rv(),
          "1" = borrow_footer_r(),
          "2" = return_footer_r(),
          "3" = write_off_footer_r()
        )

        if (error_r()) footer <- shinyjs::disabled(footer)

        footer
      })

      error_r <- shiny::reactive({
        quantity_return$error_r()
      })

      borrow_label_r <- shiny::reactive({
        glue::glue(
          "Menge ({quantity} verfügbar)",
          quantity = max_borrow_r()
        )
      })

      return_label_r <- shiny::reactive({
        glue::glue(
          "Menge ({quantity} ausgeliehen)",
          quantity = max_return_r()
        )
      })

      write_off_label_r <- shiny::reactive({
        glue::glue(
          "Menge ({available} verfügbar, {borrowed} insgesamt ausgeliehen)",
          available = max_borrow_r(),
          borrowed = max_borrowed_r()
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

      write_off_footer_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("confirm_write_off"),
          label = "Abschreiben bestätigen",
          icon = shiny::icon("balance-scale-right"),
          class = "write-off-btn",
          width = "100%"
        )
      })

      shiny::observeEvent(input$confirm_borrow, {
        shiny::removeModal()

        db_add_circulation(
          db = .values$db,
          user_id = user_id_r(),
          subtype_id = input$subtype,
          quantity = quantity_return$quantity_r(),
          op_type = 1L
        )

        .values$update$circulation(.values$update$circulation() + 1)

        shiny::showNotification(
          ui = operate_notification_text(
            operation = operate_rv(),
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
          quantity = -1 * quantity_return$quantity_r(),
          op_type = 1L
        )

        .values$update$circulation(.values$update$circulation() + 1)

        shiny::showNotification(
          ui = operate_notification_text(
            operation = operate_rv(),
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

      shiny::observeEvent(input$confirm_write_off, {
        shiny::removeModal()

        amount <- -1 * quantity_return$quantity_r()

        db_change_subtype_max_quantity(
          .values$db,
          subtype_id = input$subtype,
          amount = amount
        )

        .values$update$subtype(.values$update$subtype() + 1)

        db_add_circulation(
          db = .values$db,
          user_id = user_id_r(),
          subtype_id = input$subtype,
          quantity = amount,
          op_type = 2
        )

        .values$update$circulation(.values$update$circulation() + 1)

        shiny::showNotification(
          ui = operate_notification_text(
            operation = operate_rv(),
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
        switch(
          operate_rv(),
          "1" = max_borrow_r(),
          "2" = max_return_r(),
          "3" = max_borrow_r()
        )
      })

      # Available quantity
      max_borrow_r <- shiny::reactive({
        .values$update$subtype()
        .values$update$circulation()
        db_get_available_quantity(
          db = .values$db,
          subtype_id = input$subtype
        )
      })

      # Borrowed quantity by user
      max_return_r <- shiny::reactive({
        .values$update$subtype()
        .values$update$circulation()
        db_get_borrowed_quantity_by_user_id(
          db = .values$db,
          user_id = user_id_r(),
          subtype_id = input$subtype
        )
      })

      # Total borrowed quantity
      max_borrowed_r <- shiny::reactive({
        .values$update$subtype()
        .values$update$circulation()
        db_get_borrowed_quantity(
          db = .values$db,
          subtype_id = input$subtype
        )
      })

      operate_verb_r <- shiny::reactive({
        get_operation_verb(operate_rv())
      })

      max_message_r <- shiny::reactive({
        context <- c(
          "als verfügbar sind!\n\n",
          "als Du ausgeliehen hast!\n\n",
          "als verfügbar sind!\n\n"
        )

        paste(
          "Du kannst nicht mehr Einheiten",
          operate_verb_r(),
          context[operate_rv()]
        )
      })

      min_message_r <- shiny::reactive({
        switch(
          operate_rv(),
          "1" = "Du musst mindestens eine Einheit ausleihen!",
          "2" = "Du musst mindestens eine Einheit zurückgeben!",
          "3" = "Du musst mindestens eine Einheit abschreiben!"
        )
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
        type_id_r = type_id_r,
        subtype_id_r = subtype_id_r
      )

      return(return_list)
    }
  )
}



operate_notification_text <- function(operation,
                                      status,
                                      user_name,
                                      quantity,
                                      type_name,
                                      subtype_name
) {
  verb <- get_operation_verb(operation, participle = TRUE)

  if (status == "admin" && operation != 3) {
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



get_operation_verb <- function(operation, participle = FALSE) {
  stopifnot(operation %in% 1:3)
  verb <- if (participle) {
    c("ausgeliehen", "zurückgegeben", "abgeschrieben")
  } else {
    c("ausleihen", "zurückgeben", "abschreiben")
  }
  verb[operation]
}
