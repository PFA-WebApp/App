operate_files_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabBox(
    id = ns("file_tabs"),
    width = NULL,
    title = "Dateien",
    shiny::tabPanel(
      title = "Gruppen",
      icon = shiny::icon("layer-group"),
      shiny::uiOutput(
        outputId = ns("group")
      ),
      operate_file_manager_ui(
        id = ns("operate_file_manager_group")
      )
    ),
    shiny::tabPanel(
      title = "Typ",
      icon = shiny::icon("tags"),
      operate_file_manager_ui(
        id = ns("operate_file_manager_type")
      )
    ),
    shiny::tabPanel(
      title = "Untertyp",
      icon = shiny::icon("tag"),
      operate_file_manager_ui(
        id = ns("operate_file_manager_subtype")
      )
    )
  )
}

operate_files_server <- function(id,
                                 .values,
                                 type_id_r,
                                 subtype_id_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      group_ids_r <- shiny::reactive({
        db_get_groups_by_type(.values$db, type_id_r())
      })

      operate_file_manager_server(
        id = "operate_file_manager_group",
        .values = .values,
        db = list(
          get_object_name = db_get_group_name
        ),
        settings = list(table_name = "group"),
        label = list(
          object_name = "Gruppe"
        ),
        object_ids_r = group_ids_r
      )

      operate_file_manager_server(
        id = "operate_file_manager_type",
        .values = .values,
        db = list(
          get_object_name = db_get_type_name
        ),
        settings = list(table_name = "type"),
        label = list(
          object_name = "Typ"
        ),
        object_ids_r = type_id_r
      )

      operate_file_manager_server(
        id = "operate_file_manager_subtype",
        .values = .values,
        db = list(
          get_object_name = db_get_subtype_name
        ),
        settings = list(table_name = "subtype"),
        label = list(
          object_name = "Untertyp"
        ),
        object_ids_r = subtype_id_r
      )
    }
  )
}
