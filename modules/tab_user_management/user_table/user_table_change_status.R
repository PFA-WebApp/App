user_table_change_status_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("change_status"),
      label = NULL,
      icon = shiny::icon("edit"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("change_status")
      )
    )
  )
}

user_table_change_status_server <- function(id, .values, user_name, status) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$change_status, {
        if (user_name == .values$user$name()) {
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
            selected = status
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_status"),
            label = "Bestätigen"
          )
        ))
      })

      shiny::observeEvent(input$confirm_status, {
        shiny::removeModal()

        success <- DB::db_set_user_status(.values$db, user_name, input$user_status)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "Der Status von Benutzer \"",
              user_name,
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
              user_name,
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
