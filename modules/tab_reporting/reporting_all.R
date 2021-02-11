reporting_all_ui <- function(id) {
  ns <- shiny::NS(id)

  reporting_table_ui(
    id = ns("reporting_table")
  )
}

reporting_all_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      reporting_table_server(
        id = "reporting_table",
        .values = .values,
        settings = list(summary = "all"),
        object_id_r = NULL
      )
    }
  )
}
