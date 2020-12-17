user_table_remove_button_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("remove"),
      label = NULL,
      icon = shiny::icon("user-alt-slash"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("remove")
      )
    )
  )
}

user_table_remove_button_server <- function(id, .values, user_name, status) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$remove, {
        if (.values$user$status() == "mod" && status != "user") {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = "Zugriff verweigert!",
            htmltools::div(
              "Moderatoren dürfen nur Benutzer löschen."
            ),
            footer = shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ))

          return()
        }

        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = "Benutzer löschen",
          htmltools::div(
            paste0(
              "Bist du sicher, dass du den Benutzer \"",
              user_name,
              "\" löschen möchtest?"
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_remove"),
            label = "Ja"
          )
        ))
      })

      shiny::observeEvent(input$confirm_remove, {
        shiny::removeModal()

        shiny::showNotification(
          ui = paste0(
            "Der Benutzer \"",
            user_name,
            "\" wurde erfolgreich gelöscht."
          ),
          type = "warning",
          duration = 5
        )

        DB::db_remove_user(.values$db, user_name)

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
