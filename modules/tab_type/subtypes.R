subtypes_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shinydashboard::box(
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      title = "Untertypen bearbeiten",
      shiny::uiOutput(
        outputId = ns("select_type")
      ),
      object_table_ui(
        id = ns("object_table")
      ),
      htmltools::br(),
      shiny::actionButton(
        inputId = ns("add_subtype"),
        label = "Untertyp hinzufügen",
        icon = shiny::icon("plus"),
        width = "100%"
      )
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
        update_name = "subtype",
        length_name = "subtype_name",
        show = c("name", "quantity", "remove")
      )

      db <- list(
        table = "subtype",
        name_column = "subtype_name",
        func = list(
          add_object = function(db, name) db_add_subtype(db, input$type, name, 0),
          filter_table = function(db) db_get_subtypes_by_type_id(db, input$type),
          get_object_name = db_get_subtype_name,
          get_object_quantity = db_get_subtype_quantity,
          has_object_id = db_has_subtype_id,
          has_object_name = db_has_subtype_name,
          set_object_name = db_set_subtype_name,
          set_object_quantity = db_set_subtype_quantity,
          remove_object = db_remove_subtype,
          remobe_object_allowed = remove_subtype_allowed
        )
      )

      label <- list(
        add_label = "Untertypen hinzufügen",
        change_name = "Untertypennamen bearbeiten",
        change_quantity = "Untertypenmenge bearbeiten",
        colnames = c("Untertypname", "Menge", "Entfernen"),
        new_name = "Neuer Untertypenname",
        new_quantity = "Neue Untertypenmenge",
        object = "Untertyp",
        object_name_with_article = "Der Untertypenname",
        object_quantity_with_article = "Die Untertypenmenge",
        object_with_article = "Der Untertyp",
        object_with_small_article = "den Untertypen",
        remove_btn_title = "Untertyp entfernen"
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
          label = "Typ",
          choices = db_get_types(.values$db)
        )
      })

      shiny::observeEvent(input$add_subtype, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          add_object_ui(
            id = ns("add_object"),
            title = "Untertyp hinzufügen",
            label = "Untertyp",
            placeholder = NULL,
            collapsible = FALSE
          ),
          footer = shiny::modalButton(
            label = "Abbrechen"
          )
        ))
      })

      add_object_server(
        id = "add_object",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )
    }
  )
}
