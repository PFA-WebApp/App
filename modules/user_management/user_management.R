user_management_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      add_user_ui(
        id = ns("add_user")
      ),
      password_reset_ui(
        id = ns("password_reset")
      )
    ),
    shiny::column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        status = "primary",
        title = "Nutzertabelle",
        solidHeader = TRUE,
        DT::dataTableOutput(
          outputId = ns("user_table")
        )
      )
    )
  )
}

user_management_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$user_table <- DT::renderDataTable({
        .values$update$user()

        DB::db_get_table(.values$db, "user") %>%
          dplyr::select(Benutzername = name, Status = status)
      })

      add_user_server(
        id = "add_user",
        .values = .values
      )

      password_reset_server(
        id = "password_reset",
        .values = .values
      )
    }
  )
}
