reporting_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::uiOutput(
      outputId = ns("borrow"),
      container = function(...) shiny::column(width = 12, ...)
    ),
    reporting_available_ui(
      id = ns("reporting_available")
    )
  )
}

reporting_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$borrow <- shiny::renderUI({
        if (.values$user$status() == "admin") {
          bs4Dash::tabBox(
            id = ns("tabs"),
            width = NULL,
            title = "Ausleihübersicht",
            shiny::tabPanel(
              title = "Nach Nutzer",
              reporting_user_ui(
                id = ns("reporting_user")
              )
            ),
            shiny::tabPanel(
              title = "Nach Untertyp",
              reporting_subtype_ui(
                id = ns("reporting_subtype")
              )
            ),
            shiny::tabPanel(
              title = "Gesamt",
              reporting_all_ui(
                id = ns("reporting_all")
              )
            ),
            shiny::tabPanel(
              title = "Transaktionen",
              reporting_transaction_ui(
                id = ns("reporting_transaction")
              )
            )
          )
        } else {
          bs4Dash::box(
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            title = "Ausleihübersicht",
            reporting_user_ui(
              id = ns("reporting_user")
            )
          )
        }
      })

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
