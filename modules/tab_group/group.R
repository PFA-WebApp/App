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

      object_table_server(
        id = "group_table",
        .values = .values,
        .values_type = "group",
        db_table = "groups",
        db_object_id = "group_id",
        db_object_name = "group_name",
        colnames = c("Gruppenname", "Gruppenname ändern", "Entfernen"),
        get_objects_func = DB::db_get_groups,
        set_object_name_func = DB::db_set_group_name,
        remove_object_func = DB::db_remove_group,
        label = list(
          change_name = "Gruppe ändern",
          new_name = "Neuer Gruppenname",
          object_name_with_article = "Der Gruppenname",
          object_with_article = "Die Gruppe",
          object_with_small_article = "die Gruppe",
          remove_btn_title = "Gruppe entfernen"
        )
      )
    }
  )
}
