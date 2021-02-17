# Currently not used

password_reset_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
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

      user_choices_r <- shiny::reactive({
        .values$update$user()

        db_get_users(.values$db)
      })

      output$user_name <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("user_id"),
          label = "Benutzername",
          choices = user_choices_r()
        )
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, input$user_id)
      })

      shiny::observeEvent(input$start_reset, {
        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            "Passwort zurücksetzen",
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          htmltools::div(
            paste0(
              "Bist Du sicher, dass Du das Passwort für \"",
              user_name_r(),
              "\" zurücksetzen möchtest?"
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_reset"),
            label = "Ja"
          )
        ))
      })

      shiny::observeEvent(input$confirm_reset, {
        shiny::removeModal()

        reset_pwd <- "1234"

        shiny::showNotification(
          ui = paste0(
            "Das Passwort für \"",
            user_name_r(),
            "\" wurde erfolgreich auf \"",
            reset_pwd,
            "\" zurückgesetzt."
          ),
          type = "warning",
          duration = NULL
        )

        db_set_password(.values$db, input$user_id, bcrypt::hashpw(reset_pwd))
      })

    }
  )
}
