subtype_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        status = "primary",
        title = "Sensorinformationen",
        solidHeader = TRUE
      ),
      shinydashboard::box(
        width = NULL,
        status = "success",
        title = "QR-Code generieren",
        solidHeader = TRUE
      )
    )
  )
}

subtype_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
