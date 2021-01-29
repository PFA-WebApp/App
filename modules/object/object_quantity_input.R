object_quantity_input_ui <- function(id, old_quantity, label) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::numericInput(
      inputId = ns("object_quantity"),
      label = label,
      value = old_quantity,
      min = 0,
      step = 1
    ),
    shiny::uiOutput(
      outputId = ns("not_integer")
    ),
    shiny::uiOutput(
      outputId = ns("negative")
    )
  )
}

object_quantity_input_server <- function(id,
                                         .values,
                                         reset_r = shiny::reactive(0)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$not_integer <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !not_integer_r(),
            "Die Menge muss ganzzahlig sein!\n\n"
          ),
          errorClass = "PFA"
        )
      })

      output$negative <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !negative_r(),
            "Die Menge muss größer gleich Null sein!\n\n"
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

      shiny::observeEvent(reset_r(), {
        shiny::updateNumericInput(
          session = session,
          inputId = "object_quantity",
          value = 0
        )
      }, ignoreInit = TRUE)

      return_list <- list(
        error_r = error_r,
        quantity_r = shiny::reactive(input$object_quantity)
      )

      return(return_list)
    }
  )
}
