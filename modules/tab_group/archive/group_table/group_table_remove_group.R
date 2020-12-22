group_table_remove_group_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("remove"),
      label = NULL,
      icon = shiny::icon("trash-alt"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("remove")
      )
    )
  )
}

group_table_remove_group_server <- function(id,
                                            .values,
                                            group_id
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      group_name_r <- shiny::reactive({
        .values$update$group()
        groups <- DB::db_get_groups(.values$db)
        names(groups[groups == group_id][1])
      })

      shiny::observeEvent(input$remove, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = "Gruppe löschen",
          htmltools::div(
            paste0(
              "Bist du sicher, dass du die Gruppe \"",
              group_name_r(),
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

        success <- DB::db_remove_group(.values$db, group_id)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "Die Gruppe \"",
              group_name_r(),
              "\" wurde erfolgreich gelöscht."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              "Die Gruppe \"",
              group_name_r(),
              "\" konnte nicht gelöscht werden."
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update$group(.values$update$group() + 1)
      })
    }
  )
}
