group_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      add_object_box_ui(
        id = ns("add_group"),
        title = i18n$t("add_group"),
        label = i18n$t("group_name"),
        placeholder = "Versuchsaufbau"
      ),
      object_table_box_ui(
        id = ns("group_table"),
        title = i18n$t("group_table")
      )
    ),
    shiny::column(
      width = 6,
      show_connections_ui(
        id = ns("show_types"),
        title = i18n$t("show_types")
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
        length_name = "group_name",
        show = c("name", "connections", "remove")
      )

      db <- list(
        table = "groups",
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
        add_label = "${add_group}",
        change_connections = "${edit_group_types}",
        change_name = "${edit_group_name}",
        colnames = c("Gruppenname", "Typen bearbeiten", "Entfernen"),
        connection_modification = "${types_of_group}",
        connections = "${types}",
        connection_name = "${type_name}",
        new_name = "${new_group_name}",
        object = "${group}",
        object_name_with_article = "${group_name_with_article}",
        object_with_article = "{group_with_article}",
        object_with_small_article = "${group_with_small_article}",
        remove_btn_title = "${remove_group}"
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
