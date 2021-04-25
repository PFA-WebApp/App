settings_user_name_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Benutzernamen ändern",
    shiny::uiOutput(
      outputId = ns("user_name")
    ),
    shiny::uiOutput(
      outputId = ns("wrong_user_name_length")
    ),
    shiny::uiOutput(
      outputId = ns("user_name_taken")
    ),
    shiny::passwordInput(
      inputId = ns("password"),
      label = "Aktuelles Passwort"
    ),
    shiny::uiOutput(
      outputId = ns("change_user_name")
    )
  )
}

settings_user_name_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$user_name <- shiny::renderUI({
        shiny::textInput(
          inputId = ns("user_name"),
          label = "Neuer Benutzername",
          value = .values$user$name()
        )
      })

      output$wrong_user_name_length <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !user_name_too_short_r(),
            paste(
              "Der Benutzername benötigt mindestens",
              format_number(.values$settings$user_name$length$min),
              "Zeichen!"
            )
          ),
          shiny::need(
            !user_name_too_long_r(),
            paste(
              "Der Benutzername darf nicht länger sein als",
              format_number(.values$settings$user_name$length$max),
              "Zeichen!"
            )
          ),
          errorClass = "PFA"
        )
      })

      output$user_name_taken <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !user_name_taken_r(),
            "Der Benutzername existiert bereits!"
          ),
          errorClass = "PFA"
        )
      })

      user_name_taken_r <- shiny::reactive({
        shiny::req(!is.null(input$user_name))
        if (input$user_name == .values$user$name()) return(FALSE)
        db_has_user_name(.values$db, input$user_name)
      })

      user_name_too_short_r <- shiny::reactive({
        nchar(input$user_name) < .values$settings$user_name$length$min
      })

      user_name_too_long_r <- shiny::reactive({
        nchar(input$user_name) > .values$settings$user_name$length$max
      })

      error_r <- shiny::reactive({
        user_name_taken_r() ||
          user_name_too_short_r() ||
          user_name_too_long_r()
      })

      output$change_user_name <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("change_user_name"),
              label = "Benutzernamen ändern",
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("change_user_name"),
            label = "Benutzernamen ändern",
            width = "100%"
          )
        }
      })

      shiny::observeEvent(input$change_user_name, {
        shiny::updateTextInput(
          inputId = "password",
          value = ""
        )

        if (.values$yaml$showcase && .values$user$id() %in% 1:3) {
          shiny::showNotification(
            ui = "Der Benutzername der Standardnutzer kann in der Testversion
            nicht geändert werden.",
            type = "error",
            duration = 3
          )

          return()
        }

        user_pwd <- db_get_password(
          db = .values$db,
          user_id = .values$user$id()
        )

        pwd_correct <- bcrypt::checkpw(input$password, user_pwd)

        if (!pwd_correct) {
          shiny::showNotification(
            ui = "Falsches Passwort! Bitte versuche es erneut.",
            type = "error",
            duration = 3
          )
        }

        if (pwd_correct) {
          success <- db_set_user_name(.values$db, .values$user$id(), input$user_name)

          if (success) {
            shiny::showNotification(
              ui = paste0(
                "Dein Benutzername wurde erfolgreich zu \"",
                input$user_name,
                "\" geändert."
              ),
              duration = 5,
              type = "warning"
            )
          } else {
            shiny::showNotification(
              ui = "Dein Benutzername konnte nicht geändert werden.",
              duration = 5,
              type = "error"
            )
          }

          .values$user$name(input$user_name)
          .values$update$user(.values$update$user() + 1)
        }
      })
    }
  )
}
