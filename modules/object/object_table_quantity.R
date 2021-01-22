object_table_quantity_ui <- function(id, quantity) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionLink(
      inputId = ns("quantity"),
      label = htmltools::div(
        id = ns("label-container"),
        htmltools::div(
          id = ns("label"),
          quantity
        )
      ),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("quantity")
      )
    )
  )
}

object_table_quantity_server <- function(id,
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

      old_quantity_r <- shiny::reactive({
        .values$update[[settings$update_name]]()
        db$func$get_object_quantity(.values$db, object_id)
      })

      shiny::observeEvent(input$quantity, {
        shiny::showModal(shiny::modalDialog(
          title = label$change_quantity,
          easyClose = TRUE,
          object_quantity_input_ui(
            id = ns("object_quantity_input"),
            old_quantity = old_quantity_r(),
            label = label
          ),
          footer = shiny::uiOutput(
            outputId = ns("confirm_object_quantity")
          )
        ))
      })

      output$confirm_object_quantity <- shiny::renderUI({
        if (quantity_return$error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("confirm_object_quantity"),
              label = "Bestätigen"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("confirm_object_quantity"),
            label = "Bestätigen"
          )
        }
      })

      shiny::observeEvent(input$confirm_object_quantity, {
        shiny::removeModal()

        success <- db$func$set_object_quantity(
          .values$db,
          object_id,
          quantity_return$quantity_r()
        )

        if (success) {
          shiny::showNotification(
            ui = paste0(
              label$object_quantity_with_article,
              " wurde erfolgreich zu \"",
              quantity_return$quantity_r(),
              "\" geändert."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              label$object_quantity_with_article,
              " konnte nicht geändert werden."
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update[[settings$update_name]](
          .values$update[[settings$update_name]]() + 1
        )
      })

      quantity_return <- object_quantity_input_server(
        id = "object_quantity_input",
        .values = .values,
        settings = settings,
        db = db,
        label = label
      )
    }
  )
}
