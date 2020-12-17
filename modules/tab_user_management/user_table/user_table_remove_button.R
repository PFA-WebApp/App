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

user_table_remove_button_server <- function(id,
                                            .values,
                                            user_name,
                                            status,
                                            added_from
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$remove, {
        # Check that moderators can only remove users they added themselves
        if (
          .values$user$status() == "mod" &&
          (status != "user" || added_from != .values$user$name())
        ) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = "Zugriff verweigert!",
            htmltools::div(
              paste0(
                "Moderatoren dürfen nur Benutzer löschen, die sie selbst
                hinzugefügt haben. \"",
                user_name,
                "\" wurde von \"",
                added_from,
                "\" hinzugefügt."
              )
            ),
            footer = shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ))

          return()
        }


        # Check that admins can't remove themselves
        if (user_name == .values$user$name()) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = "Zugriff verweigert!",
            htmltools::div(
              "Administratoren können sich selbst nicht löschen."
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

        success <- DB::db_remove_user(.values$db, user_name)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "Der Benutzer \"",
              user_name,
              "\" wurde erfolgreich gelöscht."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              "Der Benutzer \"",
              user_name,
              "\" konnte nicht gelöscht werden."
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
