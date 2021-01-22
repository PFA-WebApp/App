settings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      settings_user_name_ui(
        id = ns("settings_user_name")
      ),
      settings_password_ui(
        id = ns("settings_password")
      )
    )
  )
}

settings_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      settings_user_name_server(
        id = "settings_user_name",
        .values = .values
      )

      settings_password_server(
        id = "settings_password",
        .values = .values
      )
    }
  )
}
