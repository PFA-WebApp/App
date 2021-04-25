file_management_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    bs4Dash::tabBox(
      id = ns("file_tabs"),
      width = 12,
      title = i18n$t("${tab_file_management}"),
      shiny::tabPanel(
        title = i18n$t("${groups}"),
        icon = shiny::icon("layer-group"),
        file_manager_ui(
          id = ns("file_manager_group")
        )
      ),
      shiny::tabPanel(
        title = i18n$t("${types}"),
        icon = shiny::icon("tags"),
        file_manager_ui(
          id = ns("file_manager_type")
        )
      ),
      shiny::tabPanel(
        title = i18n$t("${subtypes}"),
        icon = shiny::icon("tag"),
        file_manager_ui(
          id = ns("file_manager_subtype")
        )
      )
    )
  )
}

file_management_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      file_manager_server(
        id = "file_manager_group",
        .values = .values,
        db = list(
          get_objects = db_get_groups
        ),
        settings = list(
          table_name = "group",
          update_name = "group"
        ),
        label = list(
          object_name = "group"
        )
      )

      file_manager_server(
        id = "file_manager_type",
        .values = .values,
        db = list(
          get_objects = db_get_types
        ),
        settings = list(
          table_name = "type",
          update_name = "type"
        ),
        label = list(
          object_name = "type"
        )
      )

      file_manager_server(
        id = "file_manager_subtype",
        .values = .values,
        db = list(
          get_objects = db_get_subtypes
        ),
        settings = list(
          table_name = "subtype",
          update_name = "subtype"
        ),
        label = list(
          object_name = "subtype"
        )
      )
    }
  )
}
