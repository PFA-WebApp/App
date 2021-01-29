user_table_change_status_ui <- function(id, user_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("change_status"),
      label = NULL,
      icon = shiny::icon("edit"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          user_id: {user_id},
          nonce: Math.random()
        }});',
        inputId = ns("change_status"),
        user_id = user_id
      )
    )
  )
}

user_table_change_status_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_id_r <- shiny::reactive({
        shiny::req(input$change_status)$user_id
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, user_id_r())
      })

      status_r <- shiny::reactive({
        db_get_user_status(.values$db, user_id_r())
      })

      shiny::observeEvent(user_id_r(), {
        if (user_name_r() == .values$user$name()) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = "Zugriff verweigert!",
            htmltools::div(
              "Administratoren können ihren eigenen Status nicht ändern."
            ),
            footer = shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ))

          return()
        }

        shiny::showModal(shiny::modalDialog(
          title = "Status ändern",
          easyClose = TRUE,
          shiny::selectInput(
            inputId = ns("user_status"),
            label = "Status",
            choices = c(
              Administrator = "admin",
              Moderator = "mod",
              Benutzer = "user"
            ),
            selected = status_r()
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_status"),
            label = "Bestätigen"
          )
        ))
      })

      shiny::observeEvent(input$confirm_status, {
        shiny::removeModal()

        success <- db_set_user_status(.values$db, user_id_r(), input$user_status)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "Der Status von Benutzer \"",
              user_name_r(),
              "\" wurde erfolgreich auf \"",
              .values$settings$status_dict[input$user_status],
              "\" geändert."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              "Der Status von Benutzer \"",
              user_name_r(),
              "\" konnte nicht geändert werden."
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
