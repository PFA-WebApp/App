type_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      add_object_ui(
        id = ns("add_type"),
        title = "Typ hinzufügen",
        label = "Typname",
        placeholder = "PT 100"
      ),
      object_table_ui(
        id = ns("type_table"),
        title = "Typtabelle"
      )
    ),
    shiny::column(
      width = 6,
      show_connections_ui(
        id = ns("show_groups"),
        title = "Gruppen anzeigen"
      ),
      subtypes_ui(
        id = ns("subtypes")
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
        length_name = "type_name"
      )

      db <- list(
        table = "type",
        name_column = "type_name",
        func = list(
          get_connections = db_get_groups_by_type,
          get_possible_connections = db_get_groups,
          get_objects = db_get_types,
          get_object_name = db_get_type_name,
          has_object_name = db_has_type_name,
          set_object_name = db_set_type_name,
          remove_object = function(db, type_id) {
            db_remove_type(db, type_id)
            db_remove_subtypes_by_type_id(db, type_id)
          }
        )
      )

      label <- list(
        change_connection = "Gruppen bearbeiten für Typ",
        change_name = "Typname bearbeiten",
        colnames = c("Typname", "Typname bearbeiten", "Gruppen bearbeiten", "Entfernen"),
        connection_modification = "Die Gruppen von Typ",
        connections = "Gruppen",
        connection_name = "Gruppenname",
        new_name = "Neuer Typname",
        object = "Typ",
        object_name_with_article = "Der Typname",
        object_with_article = "Der Typ",
        object_with_small_article = "den Typen",
        remove_btn_title = "Typ entfernen"
      )


      add_object_server(
        id = "add_type",
        .values = .values,
        object_id = "type",
        object_name = "type_name",
        object_with_article = "Der Typ",
        add_label = "Typ hinzufügen",
        add_object_func = function(db, name) {
          db_add_type(db, name)
          id <- db_get_type_id(db, name)
          db_add_subtype(db, id, "Standard", 0)
        },
        has_object_name_func = db_has_type_name
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
