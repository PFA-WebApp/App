reporting_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    reporting_available_ui(
      id = ns("reporting_available")
    ),
    bs4Dash::tabBox(
      id = ns("tabs"),
      width = 12,
      title = "Ausleihübersicht",
      shiny::tabPanel(
        title = "Gesamt",
        reporting_all_ui(
          id = ns("reporting_all")
        )
      ),
      shiny::tabPanel(
        title = "Nach Untertyp",
        reporting_subtype_ui(
          id = ns("reporting_subtype")
        )
      ),
      shiny::tabPanel(
        title = "Nach Nutzer",
        reporting_user_ui(
          id = ns("reporting_user")
        )
      ),
      shiny::tabPanel(
        title = "Transaktionen",
        reporting_transaction_ui(
          id = ns("reporting_transaction")
        )
      )
    )
  )
}

reporting_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      reporting_user_server(
        id = "reporting_user",
        .values = .values
      )

      reporting_subtype_server(
        id = "reporting_subtype",
        .values = .values
      )

      reporting_all_server(
        id = "reporting_all",
        .values = .values
      )

      reporting_transaction_server(
        id = "reporting_transaction",
        .values = .values
      )

      reporting_available_server(
        id = "reporting_available",
        .values = .values
      )
    }
  )
}
