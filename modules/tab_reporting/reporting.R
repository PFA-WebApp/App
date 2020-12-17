reporting_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Berichtswesen"
      )
    )
  )
}

reporting_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
