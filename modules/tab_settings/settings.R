settings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shinydashboard::box(
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
    )
  )
}

settings_server <- function(id, .values) {
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
              as_german(.values$settings$password$length$min),
              "Zeichen!\n\n"
            )
          ),
          shiny::need(
            !password_too_long_r(),
            paste(
              "Das Passwort darf nicht länger sein als",
              as_german(.values$settings$user_name$length$max),
              "Zeichen!\n\n"
            )
          ),
          errorClass = "PFA"
        )
      })

      output$non_matching_passwords <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !non_matching_passwords_r(),
            "Die Passwörter stimmen nicht überein!\n\n"
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
        shiny::showNotification(
          ui = "Dein Passwort wurde erfolgreich geändert.",
          type = "warning",
          duration = 5
        )

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

        DB::db_set_password(
          db = .values$db,
          name = .values$user$name(),
          password = bcrypt::hashpw(input$user_password_1)
        )
      })
    }
  )
}
