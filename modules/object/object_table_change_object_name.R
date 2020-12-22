object_table_change_object_name_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("change_object_name"),
      label = NULL,
      icon = shiny::icon("edit"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("change_object_name")
      )
    )
  )
}

object_table_change_object_name_server <- function(id,
                                                   .values,
                                                   object_id,
                                                   .values_type,
                                                   get_objects_func,
                                                   set_object_name_func,
                                                   label
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      old_object_name_r <- shiny::reactive({
        .values$update[[.values_type]]()
        objects <- get_objects_func(.values$db)
        names(objects[objects == object_id][1])
      })

      shiny::observeEvent(input$change_object_name, {
        shiny::showModal(shiny::modalDialog(
          title = label$change_name,
          easyClose = TRUE,
          shiny::textInput(
            inputId = ns("object_name"),
            label = label$new_name,
            value = old_object_name_r()
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_object_name"),
            label = "Bestätigen"
          )
        ))
      })

      shiny::observeEvent(input$confirm_object_name, {
        shiny::removeModal()

        success <- set_object_name_func(.values$db, object_id, input$object_name)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              label$object_name_with_article,
              " wurde erfolgreich zu \"",
              input$object_name,
              "\" geändert."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              label$object_name_with_article,
              " konnte nicht geändert werden."
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update[[.values_type]](.values$update[[.values_type]]() + 1)
      })
    }
  )
}
