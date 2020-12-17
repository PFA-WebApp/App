user_table_reset_password_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("reset_password"),
      label = NULL,
      icon = shiny::icon("eraser"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("reset_password")
      )
    )
  )
}

user_table_reset_password_server <- function(id, .values, user_name) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$reset_password, {
        shiny::showModal(shiny::modalDialog(
          title = "Passwort zurücksetzen",
          easyClose = TRUE,
          htmltools::div(
            paste0(
              "Bist Du sicher, dass Du das Passwort für \"",
              user_name,
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
            user_name,
            "\" wurde erfolgreich auf \"",
            reset_pwd,
            "\" zurückgesetzt."
          ),
          type = "warning",
          duration = 5
        )

        DB::db_set_password(.values$db, user_name, bcrypt::hashpw(reset_pwd))
      })
    }
  )
}
