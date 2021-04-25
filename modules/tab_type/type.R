type_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      add_object_box_ui(
        id = ns("add_type"),
        title = "Typ hinzufügen",
        label = "Typname",
        placeholder = "PT 100"
      ),
      object_table_box_ui(
        id = ns("type_table"),
        title = "Typtabelle"
      )
    ),
    shiny::column(
      width = 6,
      subtypes_ui(
        id = ns("subtypes"),
        collapsed = TRUE
      ),
      show_connections_ui(
        id = ns("show_groups"),
        title = "Gruppen anzeigen"
      )
    )
  )
}

type_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      link_r <- eventReactive(input$generate, {
        type <- input$type
        subtype <- input$subtype
        paste0("www.google.de?", type, "d", subtype)
      })

      output$code <- shiny::renderPlot({
        qrcode_gen(link_r())
      })

      settings = list(
        is_group_object = FALSE,
        update_name = "type",
        length_name = "type_name",
        show = c("name", "connections", "remove")
      )

      db <- list(
        table = "type",
        name_column = "type_name",
        func = list(
          add_object = function(db, name) {
            success <- db_add_type(db, name)

            if (!success) return(FALSE)

            type_id <- db_get_type_id(
              db = db,
              type_name = name
            )

            db_add_subtype(
              db = db,
              type_id = type_id,
              subtype_name = "Standard",
              quantity = 0,
              critical_quantity = 0
            )

            subtype_id <- db_get_subtype_id(
              db = db,
              type_id = type_id,
              subtype_name = "Standard"
            )

            db_add_circulation(
              db = db,
              user_id = .values$user$id(),
              subtype_id = subtype_id,
              quantity = 0,
              op_type = 2
            )

            .values$update$circulation(.values$update$circulation() + 1)

            TRUE
          },
          get_connections = db_get_groups_by_type,
          get_possible_connections = db_get_groups,
          get_objects = db_get_types,
          get_object_name = db_get_type_name,
          has_object_id = db_has_type_id,
          has_object_name = db_has_type_name,
          set_object_name = db_set_type_name,
          remove_object = function(db, type_id) {
            db_remove_type(db, type_id)
            db_remove_subtypes_by_type_id(db, type_id)
          },
          remove_object_allowed = remove_type_allowed
        )
      )

      label <- list(
        add_label = "Typ hinzufügen",
        change_connections = "Gruppen bearbeiten für Typ",
        change_name = "Typname bearbeiten",
        colnames = c("Typname", "Gruppen bearbeiten", "Entfernen"),
        connection_modification = "Die Gruppen von Typ",
        connections = "Gruppen",
        connection_name = "Gruppenname",
        new_name = "Neuer Typname",
        object = "${type}",
        object_name_with_article = "${type_name_with_article}",
        object_with_article = "${type_with_article}",
        object_with_small_article = "${type_with_small_article}",
        remove_btn_title = "Typ entfernen"
      )


      add_object_server(
        id = "add_type",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )

      show_connections_server(
        id = "show_groups",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )

      subtypes_server(
        id = "subtypes",
        .values = .values
      )

      object_table_server(
        id = "type_table",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )
    }
  )
}
