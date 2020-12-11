login_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::selectInput(
    inputId = ns("user_type"),
    label = "User type",
    choices = c(
      `Not logged` = "not_logged",
      Admin = "admin",
      Moderator = "moderator",
      User = "user"
    )
  )
}

login_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$user_type, {
        print(input$user_type)
        if (input$user_type == "not_logged") {
          .values$user$logged(FALSE)
          .values$user$type(NULL)
        } else {
          .values$user$logged(TRUE)
          .values$user$type(input$user_type)
        }
      })
    }
  )
}
