reporting_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    reporting_available_ui(
      id = ns("reporting_available")
    ),
    shiny::uiOutput(
      outputId = ns("borrow"),
      container = function(...) shiny::column(width = 12, ...)
    )
  )
}

reporting_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      tab_panels <- list(
        all = shiny::tabPanel(
          title = "Gesamt",
          reporting_all_ui(
            id = ns("reporting_all")
          )
        ),
        subtype = shiny::tabPanel(
          title = "Nach Untertyp",
          reporting_subtype_ui(
            id = ns("reporting_subtype")
          )
        ),
        user = shiny::tabPanel(
          title = "Nach Nutzer",
          reporting_user_ui(
            id = ns("reporting_user")
          )
        ),
        transaction = shiny::tabPanel(
          title = "Transaktionen",
          reporting_transaction_ui(
            id = ns("reporting_transaction")
          )
        )
      )

      tab_panel_dict <- list(
        admin = c("all", "subtype", "user", "transaction"),
        mod = c("user", "transaction"),
        user = c("user", "transaction")
      )

      output$borrow <- shiny::renderUI({
        do.call(
          bs4Dash::tabBox,
          c(
            list(
              id = ns("tabs"),
              width = NULL,
              title = "Ausleihübersicht"
            ),
            unname(tab_panels[tab_panel_dict[[.values$user$status()]]])
          )
        )
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
