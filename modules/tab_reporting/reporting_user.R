reporting_user_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("select_user")
    ),
    reporting_table_ui(
      id = ns("reporting_table")
    )
  )
}

reporting_user_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_choices_r <- shiny::reactive({
        .values$update$user()
        db_get_users(.values$db)
      })

      output$select_user <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("user"),
          label = "Nutzer",
          choices = user_choices_r(),
          selected = .values$user$id()
        )
      })

      user_id_r <- shiny::reactive({
        shiny::req(input$user)
      })

      reporting_table_server(
        id = "reporting_table",
        .values = .values,
        settings = list(summary = "user"),
        object_id_r = user_id_r
      )
    }
  )
}
