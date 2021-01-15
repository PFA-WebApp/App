object_table_remove_object_ui <- function(id) {
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

object_table_remove_object_server <- function(id,
                                              .values,
                                              object_id,
                                              settings,
                                              db,
                                              label
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      object_name_r <- shiny::reactive({
        .values$update[[settings$update_name]]()
        objects <- db$func$get_objects(.values$db)
        names(objects[objects == object_id][1])
      })

      shiny::observeEvent(input$remove, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = label$remove_btn_title,
          htmltools::div(
            paste0(
              "Bist du sicher, dass du ",
              label$object_with_small_article,
              " \"",
              object_name_r(),
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

        success <- db$func$remove_object(.values$db, object_id)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              label$object_with_article,
              " \"",
              object_name_r(),
              "\" wurde erfolgreich gelöscht."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              label$object_with_article,
              " \"",
              object_name_r(),
              "\" konnte nicht gelöscht werden."
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update[[settings$update_name]](.values$update[[settings$update_name]]() + 1)
      })
    }
  )
}
