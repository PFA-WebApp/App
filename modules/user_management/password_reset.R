password_reset_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = "Nutzerpasswort zurücksetzen",
    solidHeader = TRUE,
    shiny::uiOutput(
      outputId = ns("user_name")
    ),
    shiny::actionButton(
      inputId = ns("start_reset"),
      label = "Zurücksetzen",
      width = "100%"
    )
  )
}

password_reset_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_name_choices_r <- shiny::reactive({
        .values$update$user()

        sort(db_get_user_names(.values$db))
      })

      output$user_name <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("user_name"),
          label = "Benutzername",
          choices = user_name_choices_r()
        )
      })

      shiny::observeEvent(input$start_reset, {
        shiny::showModal(shiny::modalDialog(
          title = "Passwort zurücksetzen",
          easyClose = TRUE,
          htmltools::div(
            paste0(
              "Bist Du sicher, dass Du das Passwort für \"",
              input$user_name,
              "\" zurücksetzen möchtest?"
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_reset"),
            label = "Bestätigen"
          )
        ))
      })

      shiny::observeEvent(input$confirm_reset, {
        shiny::removeModal()

        reset_pwd <- "1234"

        shiny::showNotification(
          ui = paste0(
            "Das Passwort für \"",
            input$user_name,
            "\" wurde erfolgreich auf \"",
            reset_pwd,
            "\" zurückgesetzt."
          ),
          type = "warning",
          duration = NULL
        )

        db_set_password(db, input$user_name, bcrypt::hashpw(reset_pwd))
      })

    }
  )
}
