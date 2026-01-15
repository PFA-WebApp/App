operate_circulation_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = i18n$t("tab_operate"),
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
            label = .values$i18n$t("executing_user"),
            choices = user_r(),
            selected = .values$user$id(),
            selectize = .values$device$large
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
          label = .values$i18n$t("type"),
          choices = types_r(),
          selected = .values$query$type(),
          selectize = .values$device$large
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
        input$type %||% types_r()[1]
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
          label = .values$i18n$t("subtype"),
          choices = subtypes_r(),
          selectize = .values$device$large
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
                label = .values$i18n$t("borrow"),
                icon = shiny::icon("sign-out-alt"),
                width = "100%",
                class = "borrow-btn"
              )
            ),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = ns("write_off"),
                label = .values$i18n$t("write_off"),
                icon = shiny::icon("balance-scale-right"),
                width = "100%",
                class = "write-off-btn"
              )
            ),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = ns("return"),
                label = .values$i18n$t("return"),
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
                label = .values$i18n$t("borrow"),
                icon = shiny::icon("sign-out-alt"),
                width = "100%",
                class = "borrow-btn"
              )
            ),
            shiny::column(
              width = 6,
              shiny::actionButton(
                inputId = ns("return"),
                label = .values$i18n$t("return"),
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

        title <- list(
          .values$i18n$t("borrow"),
          .values$i18n$t("return"),
          .values$i18n$t("write_off")
        )

        shiny::modalDialog(
          title = htmltools::tagList(
            title[operate_index],
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          shinyjs::disabled(
            shiny::textInput(
              inputId = "disabled",
              label = .values$i18n$t("user"),
              value = user_name_r()
            )
          ),
          shinyjs::disabled(
            shiny::textInput(
              inputId = "disabled",
              label = .values$i18n$t("type"),
              value = type_name_r()
            )
          ),
          shinyjs::disabled(
            shiny::textInput(
              inputId = "disabled",
              label = .values$i18n$t("subtype"),
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
        .values$i18n$t(
          "quantity_available",
          max_borrow_r()
        )
      })

      return_label_r <- shiny::reactive({
        .values$i18n$t(
          "quantity_borrowed",
          max_return_r()
        )
      })

      write_off_label_r <- shiny::reactive({
        .values$i18n$t(
          "quantity_write_off",
          max_borrow_r(),
          max_borrowed_r()
        )
      })

      borrow_footer_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("confirm_borrow"),
          label = .values$i18n$t("confirm_borrow"),
          icon = shiny::icon("sign-out-alt"),
          class = "borrow-btn",
          width = "100%"
        )
      })

      return_footer_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("confirm_return"),
          label = .values$i18n$t("confirm_return"),
          icon = shiny::icon("sign-in-alt"),
          class = "return-btn",
          width = "100%"
        )
      })

      write_off_footer_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("confirm_write_off"),
          label = .values$i18n$t("confirm_write_off"),
          icon = shiny::icon("balance-scale-right"),
          class = "write-off-btn",
          width = "100%"
        )
      })

      shiny::observeEvent(input$confirm_borrow, {
        shiny::removeModal()

        DBI::dbWithTransaction(
          .values$db,
          {
            available_quantity <- db_get_available_quantity(
              .values$db,
              input$subtype
            )

            if (available_quantity < quantity_return$quantity_r()) {
              success <- FALSE
              DBI::dbBreak()
            } else {
              success <- TRUE
            }

            db_add_circulation(
              db = .values$db,
              user_id = user_id_r(),
              subtype_id = input$subtype,
              quantity = quantity_return$quantity_r(),
              op_type = 1L
            )
          }
        )

        .values$update$circulation(.values$update$circulation() + 1)

        if (success) {
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
        } else {
          shiny::showNotification(
            ui = .values$i18n$t("err_borrow"),
            duration = NULL,
            type = "error"
          )
        }
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

        success <- db_change_subtype_max_quantity(
          .values$db,
          subtype_id = input$subtype,
          amount = amount
        )

        if (success) {
          db_add_circulation(
            db = .values$db,
            user_id = user_id_r(),
            subtype_id = input$subtype,
            quantity = amount,
            op_type = 2L
          )

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
        } else {
          shiny::showNotification(
            ui = .values$i18n$t("err_write_off"),
            duration = 5,
            type = "error"
          )
        }

        .values$update$subtype(.values$update$subtype() + 1)
        .values$update$circulation(.values$update$circulation() + 1)
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

      max_message_r <- shiny::reactive({
        switch(
          operate_rv(),
          "1" = "err_borrow_max",
          "2" = "err_return_max",
          "3" = "err_write_off_max"
        )
      })

      min_message_r <- shiny::reactive({
        switch(
          operate_rv(),
          "1" = "err_borrow_positive",
          "2" = "err_return_positive",
          "3" = "err_write_off_positive"
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

  obj_name <- paste0(type_name, ":", subtype_name)

  if (status == "admin" && operation != 3) {
    .values$i18n$t(
      "msg_operate_successful_admin",
      user_name,
      quantity,
      obj_name,
      verb
    )
  } else {
    .values$i18n$t(
      "msg_operate_successful",
      quantity,
      obj_name,
      verb
    )
  }
}



get_operation_verb <- function(operation, participle = FALSE) {
  stopifnot(operation %in% 1:3)
  verb <- if (participle) {
    c("${borrowed}", "${returned}", "${wrote_off}")
  } else {
    c("${borrow_s}", "${return_s}", "${write_off_s}")
  }
  verb[operation]
}
