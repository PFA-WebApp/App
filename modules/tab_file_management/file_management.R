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
          id = ns("file_manager")
        )
      ),
      shiny::tabPanel(
        title = "Typen",
        icon = shiny::icon("tags")
      ),
      shiny::tabPanel(
        title = "Untertypen",
        icon = shiny::icon("tag")
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
        id = "file_manager",
        .values = .values
      )
    }
  )
}
