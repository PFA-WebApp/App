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
      outputId = ns("not_integer"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("negative"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("too_small"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("too_big"),
      class = "pfa-error"
    )
  )
}

object_quantity_input_server <- function(id,
                                         .values,
                                         reset_r = shiny::reactive(0),
                                         max_r = shiny::reactive(Inf),
                                         max_message_r = shiny::reactive(""),
                                         min_r = shiny::reactive(-Inf),
                                         min_message_r = shiny::reactive(""),
                                         object_label = "${quantity}"
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$not_integer <- shiny::renderUI({
        if (not_integer_r()) {
          .values$i18n$t(
            "err_must_be_integer",
            object_label
          )
        }
      })

      output$negative <- shiny::renderUI({
        if (negative_r()) {
          .values$i18n$t(
            "err_must_be_positive",
            object_label
          )
        }
      })

      output$too_small <- shiny::renderUI({
        if (too_small_r()) {
          .values$i18n$t(
            min_message_r(),
            min_r()
          )
        }
      })

      output$too_big <- shiny::renderUI({
        if (too_big_r()) {
          if (!nchar(max_message_r())) return()

          .values$i18n$t(
            max_message_r(),
            max_r()
          )
        }
      })

      not_integer_r <- shiny::reactive({
        if (is_empty(input$object_quantity)) return(TRUE)
        quantity <- as.integer(input$object_quantity)
        if (is.na(quantity)) return(TRUE)
        quantity != input$object_quantity
      })

      negative_r <- shiny::reactive({
        if (min_r() > 0) return(FALSE)
        if (is_empty(input$object_quantity)) return(TRUE)
        input$object_quantity < 0
      })

      too_small_r <- shiny::reactive({
        if (is_empty(input$object_quantity)) return(TRUE)
        input$object_quantity < min_r()
      })

      too_big_r <- shiny::reactive({
        if (is_empty(input$object_quantity)) return(TRUE)
        input$object_quantity > max_r()
      })

      error_r <- shiny::reactive({
        not_integer_r() ||
          negative_r() ||
          too_small_r() ||
          too_big_r()
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
