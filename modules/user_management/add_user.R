add_user_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = "Nutzer hinzufügen",
    solidHeader = TRUE,
    shiny::textInput(
      inputId = ns("user_name"),
      label = "Benutzername",
      placeholder = "Max Mustermann"
    ),
    shiny::uiOutput(
      outputId = ns("wrong_name_length")
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
          choices = choices_r()
        )
      })

      wrong_length <- function(x, id) {
        shiny::renderUI({
          shiny::validate(
            shiny::need(
              nchar(input[[id]]) >= 4,
              paste(
                x,
                "benötigt mindestens vier Zeichen!\n\n"
              )
            ),
            shiny::need(
              nchar(input[[id]]) <= 16,
              paste(
                x,
                "darf nicht länger sein als 16 Zeichen!\n\n"
              )
            ),
            errorClass = "PFA"
          )
        })
      }

      output$wrong_name_length <- wrong_length(
        x = "Benutzername",
        id = "user_name"
      )

      output$wrong_password_length <- wrong_length(
        x = "Passwort",
        id = "user_password_1"
      )

      output$non_matching_passwords <- shiny::renderUI({

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
        nchar(input$user_name) < 4 || nchar(input$user_password_1) < 4 ||
          nchar(input$user_name) > 16 || nchar(input$user_password_1) > 16
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

        if (DB::db_has_user_name(.values$db, input$user_name)) {
          shiny::showNotification(
            ui = paste0(
              "Es exisitiert bereits ein Nutzer mit dem Benutzernamen \"",
              input$user_name,
              "\"! Bitte versuche es erneut."
            ),
            type = "error",
            duration = 3
          )

          shiny::updateTextInput(
            session = session,
            inputId = "user_name",
            value = ""
          )

          return()
        }

        if (input$user_password_1 != input$user_password_2) {
          shiny::showNotification(
            ui = "Die Passwörter stimmen nicht überein! Bitte versuche es erneut.",
            type = "error",
            duration = 3
          )

          return()
        }

        shiny::updateTextInput(
          session = session,
          inputId = "user_name",
          value = ""
        )

        shiny::showNotification(
          ui = paste0(
            "Der Nutzer \"",
            input$user_name,
            "\" wurde erfolgreich hinzugefügt."
          )
        )

        DB::db_add_user(
          db = .values$db,
          name = input$user_name,
          status = input$user_status,
          password = bcrypt::hashpw(input$user_password_1)
        )

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
