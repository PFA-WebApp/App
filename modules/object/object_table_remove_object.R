object_table_remove_object_ui <- function(id, object_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("remove") %_% object_id,
      label = NULL,
      icon = shiny::icon("trash-alt"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          object_id: {object_id},
          nonce: Math.random()
        }});',
        inputId = ns("remove"),
        object_id = object_id
      )
    )
  )
}

object_table_remove_object_server <- function(id,
                                              .values,
                                              settings,
                                              db,
                                              label
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      object_id_r <- shiny::reactive(
        input$remove$object_id
      )

      object_name_r <- shiny::reactive({
        .values$update[[settings$update_name]]()
        db$func$get_object_name(.values$db, object_id_r())
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

        # Check that object exist (object may be removed when clicking twice)
        if (!db$func$has_object_id(.values$db, object_id_r())) return()

        # Check that removal is allowed
        if (!is.null(db$func$remove_allowed)) {
          if (!db$func$remove_allowed(.values$db, object_id_r())) return()
        }

        success <- db$func$remove_object(.values$db, object_id_r())

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
