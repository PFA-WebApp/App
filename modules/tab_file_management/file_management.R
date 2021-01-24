file_management_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shinydashboard::tabBox(
      width = 12,
      title = "Dateiverwaltung",
      shiny::tabPanel(
        title = "Gruppen",
        icon = shiny::icon("layer-group"),
        file_manager_ui(
          id = ns("file_manager_group")
        )
      ),
      shiny::tabPanel(
        title = "Typen",
        icon = shiny::icon("tags"),
        file_manager_ui(
          id = ns("file_manager_type")
        )
      ),
      shiny::tabPanel(
        title = "Untertypen",
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
          object_name = "Gruppe"
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
          object_name = "Typ"
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
          object_name = "Untertyp"
        )
      )
    }
  )
}
