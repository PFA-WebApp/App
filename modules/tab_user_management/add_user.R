add_user_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    status = "primary",
    title = "Nutzer hinzufügen",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    shiny::textInput(
      inputId = ns("user_name"),
      label = "Benutzername",
      placeholder = "Max Mustermann"
    ),
    shiny::uiOutput(
      outputId = ns("wrong_name_length")
    ),
    shiny::uiOutput(
      outputId = ns("user_name_taken")
    ),
    shiny::passwordInput(
      inputId = ns("user_password_1"),
      label = "Passwort"
    ),
    shiny::uiOutput(
      outputId = ns("wrong_password_length")
    ),
    shiny::passwordInput(
      inputId = ns("user_password_2"),
      label = "Passwort bestätigen"
    ),
    shiny::uiOutput(
      outputId = ns("non_matching_passwords")
    ),
    shiny::uiOutput(
      outputId = ns("status")
    ),
    shiny::uiOutput(
      outputId = ns("add_user")
    )
  )
}

add_user_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns



      choices_r <- shiny::reactive({
        if (.values$user$status() == "admin") {
          c(Administrator = "admin", Moderator = "mod", Benutzer = "user")
        } else {
          c(Benutzer = "user")
        }
      })

      output$status <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("user_status"),
          label = "Status",
          choices = choices_r(),
          selectize = .values$device$large
        )
      })

      output$wrong_name_length <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !user_name_too_short_r(),
            paste(
              "Der Benutzername benötigt mindestens",
              as_german(.values$settings$user_name$length$min),
              "Zeichen!"
            )
          ),
          shiny::need(
            !user_name_too_long_r(),
            paste(
              "Der Benutzername darf nicht länger sein als",
              as_german(.values$settings$user_name$length$max),
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

      output$wrong_password_length <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !password_too_short_r(),
            paste(
              "Das Passwort benötigt mindestens",
              as_german(.values$settings$password$length$min),
              "Zeichen!"
            )
          ),
          shiny::need(
            !password_too_long_r(),
            paste(
              "Das Passwort darf nicht länger sein als",
              as_german(.values$settings$user_name$length$max),
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

      output$add_user <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("add_user"),
              label = "Nutzer hinzufügen",
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("add_user"),
            label = "Nutzer hinzufügen",
            width = "100%"
          )
        }
      })

      error_r <- shiny::reactive({
        user_name_too_short_r() ||
          user_name_too_long_r() ||
          user_name_taken_r() ||
          password_too_short_r() ||
          password_too_long_r() ||
          non_matching_passwords_r()
      })

      user_name_too_short_r <- shiny::reactive({
        nchar(input$user_name) < .values$settings$user_name$length$min
      })

      user_name_too_long_r <- shiny::reactive({
        nchar(input$user_name) > .values$settings$user_name$length$max
      })

      user_name_taken_r <- shiny::reactive({
        db_has_user_name(.values$db, input$user_name)
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

      shiny::observeEvent(input$add_user, {
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

        shiny::updateTextInput(
          session = session,
          inputId = "user_name",
          value = ""
        )

        success <- db_add_user(
          db = .values$db,
          name = input$user_name,
          status = input$user_status,
          password = bcrypt::hashpw(input$user_password_1),
          added_from = .values$user$id()
        )

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "Der Nutzer \"",
              input$user_name,
              "\" wurde erfolgreich hinzugefügt."
            ),
            duration = 5,
            type = "warning"
          )
        } else {
          shiny::showNotification(
            ui = paste(
              "Hinzufügen fehlgeschlagen. Es gibt bereits einen Nutzer",
              "mit dem selben Namen."
            ),
            duration = 5,
            type = "error"
          )
        }

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
