group_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      add_object_ui(
        id = ns("add_group"),
        title = "Gruppe hinzufügen",
        label = "Gruppenname",
        placeholder = "Versuchsaufbau"
      ),
      object_table_box_ui(
        id = ns("group_table"),
        title = "Gruppentabelle"
      )
    ),
    shiny::column(
      width = 6,
      show_connections_ui(
        id = ns("show_types"),
        title = "Typen anzeigen"
      )
    )
  )
}

group_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      settings = list(
        is_group_object = TRUE,
        update_name = "group",
        length_name = "group_name"
      )

      db <- list(
        table = "groups",
        name_column = "group_name",
        func = list(
          add_object = db_add_group,
          get_connections = db_get_types_by_group,
          get_possible_connections = db_get_types,
          get_objects = db_get_groups,
          get_object_name = db_get_group_name,
          has_object_id = db_has_group_id,
          has_object_name = db_has_group_name,
          set_object_name = db_set_group_name,
          remove_object = db_remove_group
        )
      )

      label <- list(
        add_label = "Gruppe hinzufügen",
        change_connections = "Typen bearbeiten für Gruppe",
        change_name = "Gruppenname bearbeiten",
        colnames = c("Gruppenname", "Typen bearbeiten", "Entfernen"),
        connection_modification = "Die Typen von Gruppe",
        connections = "Typen",
        connection_name = "Typname",
        new_name = "Neuer Gruppenname",
        object = "Gruppe",
        object_name_with_article = "Der Gruppenname",
        object_with_article = "Die Gruppe",
        object_with_small_article = "die Gruppe",
        remove_btn_title = "Gruppe entfernen"
      )

      add_object_server(
        id = "add_group",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )

      show_connections_server(
        id = "show_types",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )

      object_table_server(
        id = "group_table",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )
    }
  )
}
