user_table_remove_user_ui <- function(id, user_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("remove"),
      label = NULL,
      icon = shiny::icon("user-minus"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          user_id: {user_id},
          nonce: Math.random()
        }});',
        inputId = ns("remove"),
        user_id = user_id
      )
    )
  )
}

user_table_remove_user_server <- function(id,
                                          .values
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_id_r <- shiny::reactive({
        shiny::req(input$remove)$user_id
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, user_id_r())
      })

      added_from_r <- shiny::reactive({
        user_id <- db_get_adding_user(.values$db, user_id_r())
      })

      added_from_name_r <- shiny::reactive({
        .values$update$user()
        user_id <- added_from_r()
        db_get_user_name(.values$db, user_id)
      })

      shiny::observeEvent(user_id_r(), {
        # In showcase mode default users must not be removed
        if (.values$yaml$showcase && user_id_r() %in% 1:3) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = "Zugriff verweigert!",
            htmltools::div(
              "Die Standardnutzer können in der Testversion nicht entfernt
              werden."
            ),
            footer = shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ))

          return()
        }

        status <- .values$user$status()

        # Check that moderators can only remove users they added themselves
        if (
          status == "mod" && added_from_r() != .values$user$id()
        ) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = "Zugriff verweigert!",
            htmltools::div(
              paste0(
                "Moderatoren dürfen nur Benutzer löschen, die sie selbst
                hinzugefügt haben. \"",
                user_name_r(),
                "\" wurde von \"",
                added_from_name_r(),
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
        if (user_name_r() == .values$user$name()) {
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
              user_name_r(),
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

        success <- db_remove_user(.values$db, user_id_r())

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "Der Benutzer \"",
              user_name_r(),
              "\" wurde erfolgreich gelöscht."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              "Der Benutzer \"",
              user_name_r(),
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
