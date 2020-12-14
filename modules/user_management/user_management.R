user_management_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
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
        shiny::passwordInput(
          inputId = ns("user_password_1"),
          label = "Passwort"
        ),
        shiny::passwordInput(
          inputId = ns("user_password_2"),
          label = "Passwort bestätigen"
        ),
        shiny::uiOutput(
          outputId = ns("status")
        ),
        shiny::actionButton(
          inputId = ns("add_user"),
          label = "Nutzer hinzufügen",
          width = "100%"
        )
      ),
      password_reset_ui(
        id = ns("password_reset")
      )
    ),
    shiny::column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        status = "primary",
        title = "Nutzertabelle",
        solidHeader = TRUE,
        DT::dataTableOutput(
          outputId = ns("user_table")
        )
      )
    )
  )
}

user_management_server <- function(id, .values) {
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

        if (db_has_user_name(.values$db, input$user_name)) {
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

        db_add_user(
          db = db,
          name = input$user_name,
          status = input$user_status,
          password = bcrypt::hashpw(input$user_password_1)
        )

        .values$update$user(.values$update$user() + 1)
      })

      output$user_table <- DT::renderDataTable({
        .values$update$user()

        db_get_table(.values$db, "user") %>%
          dplyr::select(Benutzername = name, Status = status)
      })



      password_reset_server(
        id = "password_reset",
        .values = .values
      )
    }
  )
}
