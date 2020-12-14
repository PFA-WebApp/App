login_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("user_type"),
      label = "User type",
      choices = c(
        `Not logged` = "not_logged",
        Admin = "admin",
        Moderator = "moderator",
        User = "user"
      )
    ),
    shiny::actionButton(
      inputId = ns("print_client_data"),
      label = "Gimme Client Data"
    )
  )
}

login_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$user_type, {
        if (input$user_type == "not_logged") {
          .values$user$logged(FALSE)
          .values$user$type(NULL)
        } else {
          .values$user$logged(TRUE)
          .values$user$type(input$user_type)
        }
      })

      shiny::observeEvent(input$print_client_data, {
        print(parseQueryString(session$clientData$url_search))
      })
    }
  )
}
