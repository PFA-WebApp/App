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
      )
    ),
    shiny::column(
      width = 6,
      object_table_ui(
        id = ns("group_table"),
        title = "Gruppentabelle"
      )
    )
  )
}

group_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      add_object_server(
        id = "add_group",
        .values = .values,
        object_id = "group",
        object_name = "group_name",
        object_with_article =  "Die Gruppe",
        add_label = "Gruppe hinzufügen",
        add_object_func = DB::db_add_group,
        has_object_name_func = DB::db_has_group_name
      )

      settings = list(
        is_group_object = TRUE,
        update_name = "group",
        length_name = "group_name"
      )

      db <- list(
        table = "groups",
        id_column = "group_id",
        name_column = "group_name",
        func = list(
          get_connections = DB::db_get_types_by_group,
          get_possible_connections = DB::db_get_types,
          get_objects = DB::db_get_groups,
          get_object_name = DB::db_get_group_name,
          has_object_name = DB::db_has_group_name,
          set_object_name = DB::db_set_group_name,
          remove_object = DB::db_remove_group
        )
      )

      label <- list(
        change_connections = "Typen bearbeiten",
        change_name = "Gruppenname bearbeiten",
        colnames = c("Gruppenname", "Gruppenname bearbeiten", "Typen bearbeiten", "Entfernen"),
        connection_modification = "Die Typen von Gruppe",
        connections = "Typen",
        new_name = "Neuer Gruppenname",
        object_name_with_article = "Der Gruppenname",
        object_with_article = "Die Gruppe",
        object_with_small_article = "die Gruppe",
        remove_btn_title = "Gruppe entfernen"
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
