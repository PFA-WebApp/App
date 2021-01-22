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
          shiny::numericInput(
            inputId = ns("object_quantity"),
            label = label$new_quantity,
            value = old_quantity_r(),
            min = 0,
            step = 1
          ),
          shiny::uiOutput(
            outputId = ns("not_integer")
          ),
          shiny::uiOutput(
            outputId = ns("negative")
          ),
          footer = shiny::uiOutput(
            outputId = ns("confirm_object_quantity")
          )
        ))
      })

      output$confirm_object_quantity <- shiny::renderUI({
        if (error_r()) {
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

      output$not_integer <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !not_integer_r(),
            "Die Menge muss ganzzahlig sein!"
          ),
          errorClass = "PFA"
        )
      })

      output$negative <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !negative_r(),
            "Die Menge muss größer gleich Null sein!"
          ),
          errorClass = "PFA"
        )
      })

      not_integer_r <- shiny::reactive({
        quantity <- as.integer(input$object_quantity)
        if (is.na(quantity)) return(TRUE)
        quantity != input$object_quantity
      })

      negative_r <- shiny::reactive({
        input$object_quantity < 0
      })

      error_r <- shiny::reactive({
        not_integer_r() ||
          negative_r()
      })

      shiny::observeEvent(input$confirm_object_quantity, {
        shiny::removeModal()

        success <- db$func$set_object_quantity(
          .values$db,
          object_id,
          input$object_quantity
        )

        if (success) {
          shiny::showNotification(
            ui = paste0(
              label$object_quantity_with_article,
              " wurde erfolgreich zu \"",
              input$object_quantity,
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
    }
  )
}
