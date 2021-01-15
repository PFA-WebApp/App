group_table_change_group_name_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("change_group_name"),
      label = NULL,
      icon = shiny::icon("edit"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("change_group_name")
      )
    )
  )
}

group_table_change_group_name_server <- function(id, .values, group_id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      old_group_name_r <- shiny::reactive({
        .values$update$group()
        groups <- db_get_groups(.values$db)
        names(groups[groups == group_id][1])
      })

      shiny::observeEvent(input$change_group_name, {
        shiny::showModal(shiny::modalDialog(
          title = "Gruppenname ändern",
          easyClose = TRUE,
          shiny::textInput(
            inputId = ns("group_name"),
            label = "Neuer Gruppenname",
            value = old_group_name_r()
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_group_name"),
            label = "Bestätigen"
          )
        ))
      })

      shiny::observeEvent(input$confirm_group_name, {
        shiny::removeModal()

        success <- db_set_group_name(.values$db, group_id, input$group_name)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "Der Gruppenname wurde erfolgreich zu \"",
              input$group_name,
              "\" geändert."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              "Der Gruppenname konnte nicht geändert werden."
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
