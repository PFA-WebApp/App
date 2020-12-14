sensor_management_ui <- function(id) {
  ns <- shiny::NS(id)

  qr_generator_ui(
    id = ns("qr_generator")
  )
}

sensor_management_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      qr_generator_server(
        id = "qr_generator",
        .values = .values
      )
    }
  )
}
