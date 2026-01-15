reporting_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    reporting_available_ui(
      id = ns("reporting_available")
    ),
    bs4Dash::tabBox(
      id = ns("tabs"),
      width = 12,
      title = i18n$t("lending_overview"),
      shiny::tabPanel(
        title = i18n$t("total"),
        reporting_all_ui(
          id = ns("reporting_all")
        )
      ),
      shiny::tabPanel(
        title = i18n$t("by_subtype"),
        reporting_subtype_ui(
          id = ns("reporting_subtype")
        )
      ),
      shiny::tabPanel(
        title = i18n$t("by_user"),
        reporting_user_ui(
          id = ns("reporting_user")
        )
      ),
      shiny::tabPanel(
        title = i18n$t("transactions"),
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
