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
    ),
    shiny::column(
      width = 6,
      login_user_info_ui(
        id = ns("login_user_info")
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
        user_id <- db_get_user_id(.values$db, input$user_name)

        user_pwd <- db_get_password(
          db = .values$db,
          user_id = user_id
        )

        pwd_correct <- bcrypt::checkpw(input$user_password, user_pwd)

        if (pwd_correct) {
          .values$user$id(user_id)
          .values$user$name(input$user_name)
          .values$user$status(db_get_user_status(.values$db, .values$user$id()))
          .values$user$last_logged(
            db_get_user_last_logged(.values$db, .values$user$id())
          )
          db_log_user_in(.values$db, .values$user$id())
          .values$update$user(.values$update$user() + 1)

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
        .values$user$id(0)
        .values$user$status("not_logged")
        .values$user$name("")
        .values$user$last_logged("")

        shiny::showNotification(
          ui = "Du hast Dich erfolgreich abgemeldet. Bis zum nächsten Mal.",
          type = "default",
          duration = 3
        )
      })

      user_name_choices_r <- shiny::reactive({
        .values$update$user()

        names(db_get_users(.values$db))
      })



      login_user_info_server(
        id = "login_user_info",
        .values = .values
      )
    }
  )
}
