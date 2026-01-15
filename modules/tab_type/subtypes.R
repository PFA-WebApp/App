subtypes_ui <- function(id, collapsed) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    collapsible = TRUE,
    collapsed = collapsed,
    title = i18n$t("edit_subtypes"),
    shiny::uiOutput(
      outputId = ns("select_type")
    ),
    object_table_ui(
      id = ns("object_table")
    ),
    htmltools::br(),
    shiny::actionButton(
      inputId = ns("add_subtype"),
      label = i18n$t("add_subtype"),
      icon = shiny::icon("plus"),
      width = "100%"
    )
  )
}

subtypes_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      settings = list(
        is_group_object = FALSE,
        update_name = c("subtype", "type"),
        length_name = "subtype_name",
        show = c("name", "quantity", "critical_quantity", "remove")
      )

      db <- list(
        table = "subtype",
        name_column = "subtype_name",
        func = list(
          add_object = function(db, name) {
            success <- db_add_subtype(
              db = db,
              type_id = input$type,
              subtype_name = name,
              quantity = quantity_return$quantity_r(),
              critical_quantity = 0
            )

            if (!success) return(FALSE)

            subtype_id <- db_get_subtype_id(
              db = db,
              type_id = input$type,
              subtype_name = name
            )

            db_add_circulation(
              db = db,
              user_id = .values$user$id(),
              subtype_id = subtype_id,
              quantity = quantity_return$quantity_r(),
              op_type = 2
            )

            .values$update$circulation(.values$update$circulation() + 1)

            TRUE
          },
          add_object_allowed = function(db, name) !quantity_return$error_r(),
          filter_table = function(db) db_get_subtypes_by_type_id(db, input$type),
          get_object_name = db_get_subtype_name,
          get_object_quantity = db_get_subtype_max_quantity,
          get_object_critical_quantity = db_get_critical_quantity,
          has_object_id = db_has_subtype_id,
          has_object_name = function(db, name) {
            db_has_type_subtype_name(
              db = db,
              type_id = input$type,
              name = name
            )
          },
          set_object_name = db_set_subtype_name,
          set_object_quantity = db_set_subtype_max_quantity,
          set_object_critical_quantity = db_set_critical_quantity,
          remove_object = db_remove_subtype,
          remove_object_allowed = remove_subtype_allowed
        )
      )

      label <- list(
        add_label = "add_subtype",
        change_name = "edit_subtype_name",
        change_quantity = "edit_quantity",
        change_critical_quantity = "edit_critical_quantity",
        colnames = c("subtype_name", "quantity", "critical_quantity", "remove"),
        new_name = "new_subtype_name",
        new_quantity = "new_quantity",
        new_critical_quantity = "new_critical_quantity",
        object = "subtype",
        object_name_with_article = "${subtype_name_with_article}",
        object_quantity_with_article = "${subtype_quantity_with_article}",
        object_critical_quantity_with_article = "${critical_quantity_with_article}",
        object_with_article = "${subtype_with_article}",
        object_with_small_article = "${subtype_with_small_article}",
        remove_btn_title = "remove_subtype"
      )

      object_table_server(
        id = "object_table",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )

      output$select_type <- shiny::renderUI({
        .values$update$type()
        shiny::selectInput(
          inputId = ns("type"),
          label = .values$i18n$t("type"),
          choices = db_get_types(.values$db),
          selected = input$type,
          selectize = .values$device$large
        )
      })

      shiny::observeEvent(input$add_subtype, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = htmltools::tagList(
            .values$i18n$t("add_subtype"),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          add_object_ui(
            id = ns("add_object"),
            label = .values$i18n$t("subtype"),
            placeholder = NULL,
            object_quantity_input_ui(
              id = ns("object_quantity_input"),
              old_quantity = 0,
              label = .values$i18n$t("quantity")
            )
          ),
          footer = NULL
        ))
      })

      add_object_return <- add_object_server(
        id = "add_object",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )

      quantity_return <- object_quantity_input_server(
        id = "object_quantity_input",
        .values = .values,
        reset_r = add_object_return$on_add_r
      )
    }
  )
}
