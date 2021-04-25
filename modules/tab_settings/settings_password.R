settings_password_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Passwort ändern",
    shiny::passwordInput(
      inputId = ns("user_password_1"),
      label = "Neues Passwort"
    ),
    shiny::uiOutput(
      outputId = ns("wrong_password_length")
    ),
    shiny::passwordInput(
      inputId = ns("user_password_2"),
      label = "Neues Passwort bestätigen"
    ),
    shiny::uiOutput(
      outputId = ns("non_matching_passwords")
    ),
    shiny::uiOutput(
      outputId = ns("change_password")
    )
  )
}

settings_password_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$wrong_password_length <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !password_too_short_r(),
            paste(
              "Das Passwort benötigt mindestens",
              format_number(.values$settings$password$length$min),
              "Zeichen!"
            )
          ),
          shiny::need(
            !password_too_long_r(),
            paste(
              "Das Passwort darf nicht länger sein als",
              format_number(.values$settings$password$length$max),
              "Zeichen!"
            )
          ),
          errorClass = "PFA"
        )
      })

      output$non_matching_passwords <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !non_matching_passwords_r(),
            "Die Passwörter stimmen nicht überein!"
          ),
          errorClass = "PFA"
        )
      })

      password_too_short_r <- shiny::reactive({
        nchar(input$user_password_1) < .values$settings$password$length$min
      })

      password_too_long_r <- shiny::reactive({
        nchar(input$user_password_1) > .values$settings$password$length$max
      })

      non_matching_passwords_r <- shiny::reactive({
        input$user_password_1 != input$user_password_2
      })

      error_r <- shiny::reactive({
        password_too_short_r() ||
          password_too_long_r() ||
          non_matching_passwords_r()
      })

      output$change_password <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("change_password"),
              label = "Passwort ändern",
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("change_password"),
            label = "Passwort ändern",
            width = "100%"
          )
        }
      })

      shiny::observeEvent(input$change_password, {
        shiny::updateTextInput(
          session = session,
          inputId = "user_password_1",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          inputId = "user_password_2",
          value = ""
        )

        if (.values$yaml$showcase && .values$user$id() %in% 1:3) {
          shiny::showNotification(
            ui = "Das Passwort der Standardnutzer kann in der Testversion nicht
            geändert werden.",
            type = "error",
            duration = 5
          )

          return()
        }

        shiny::showNotification(
          ui = "Dein Passwort wurde erfolgreich geändert.",
          type = "warning",
          duration = 5
        )

        db_set_password(
          db = .values$db,
          user_id = .values$user$id(),
          password = bcrypt::hashpw(input$user_password_1)
        )
      })
    }
  )
}
