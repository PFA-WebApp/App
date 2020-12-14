login_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        status = "primary",
        title = "Anmeldung",
        solidHeader = TRUE,
        shiny::uiOutput(
          outputId = ns("login")
        )
      )
    )
  )
}

login_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$login <- shiny::renderUI({
        if (.values$user$status() == "not_logged") {
          login_r()
        } else {
          logout_r()
        }
      })

      login_r <- shiny::reactive({
        htmltools::tagList(
          shiny::selectInput(
            inputId = ns("user_name"),
            label = "Benutzername",
            choices = user_name_choices_r()
          ),
          shiny::passwordInput(
            inputId = ns("user_password"),
            label = "Passwort",
            placeholder = "Passwort"
          ),
          shiny::actionButton(
            inputId = ns("user_login"),
            label = "Anmelden",
            width = "100%"
          )
        )
      })

      shiny::observeEvent(input$user_login, {
        user_pwd <- db_get_password(
          db = .values$db,
          name = input$user_name
        )

        pwd_correct <- bcrypt::checkpw(input$user_password, user_pwd)

        if (pwd_correct) {
          .values$user$status(db_get_user_status(db, input$user_name))

          shiny::showNotification(
            ui = "Du hast Dich erfolgreich angemeldet.",
            type = "default",
            duration = 3
          )
        } else {
          shiny::showNotification(
            ui = "Falsches Passwort! Bitte versuche es erneut.",
            type = "error",
            duration = 3
          )
        }

        shiny::updateTextInput(
          session = session,
          inputId = "user_password",
          value = ""
        )
      })

      logout_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("user_logout"),
          label = "Abmelden",
          width = "100%"
        )
      })

      shiny::observeEvent(input$user_logout, {
        .values$user$status("not_logged")

        shiny::showNotification(
          ui = "Du hast Dich erfolgreich abgemeldet. Bis zum nächsten Mal.",
          type = "default",
          duration = 3
        )
      })

      user_name_choices_r <- shiny::reactive({
        db_get_user_names(.values$db)
      })
    }
  )
}
