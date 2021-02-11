reporting_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("ui")
  )
}

reporting_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$ui <- shiny::renderUI({
        if (.values$user$status() == "admin") {
          shiny::fluidRow(
            bs4Dash::tabBox(
              id = ns("tabs"),
              width = 12,
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
                title = "Transaktionen"
              )
            )
          )
        } else {
          shiny::fluidRow(
            bs4Dash::box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "Ausleihübersicht",
              reporting_user_ui(
                id = ns("reporting_user")
              )
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
    }
  )
}
