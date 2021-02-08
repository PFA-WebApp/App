operate_files_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabBox(
    id = ns("file_tabs"),
    width = NULL,
    title = "Dateien",
    shiny::tabPanel(
      title = "Gruppen",
      icon = shiny::icon("layer-group"),
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

      group_id_r <- shiny::reactive({
        db_get_groups_by_type(.values$db, type_id_r())[1]
      })

      operate_file_manager_server(
        id = "operate_file_manager_group",
        .values = .values,
        settings = list(table_name = "group"),
        object_id_r = group_id_r
      )

      operate_file_manager_server(
        id = "operate_file_manager_type",
        .values = .values,
        settings = list(table_name = "type"),
        object_id_r = type_id_r
      )

      operate_file_manager_server(
        id = "operate_file_manager_subtype",
        .values = .values,
        settings = list(table_name = "subtype"),
        object_id_r = subtype_id_r
      )
    }
  )
}
