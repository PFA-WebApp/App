file_management_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shinydashboard::tabBox(
      width = 12,
      title = "Dateiverwaltung",
      shiny::tabPanel(
        title = "Gruppen",
        icon = shiny::icon("layer-group")
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
    }
  )
}
