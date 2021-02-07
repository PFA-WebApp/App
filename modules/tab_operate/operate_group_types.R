operate_group_types_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("types"),
    container = htmltools::tags$ul,
    class = "st-badges flex bordered"
  )
}

operate_group_types_server <- function(id, .values, group_id_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      types_r <- shiny::reactive({
        db_get_types_by_group(.values$db, group_id_r())
      })

      clicked_type_index_rv <- shiny::reactiveVal(NULL)

      type_id_r <- shiny::eventReactive(clicked_type_index_rv(), {
        types_r()[shiny::req(clicked_type_index_rv()$index)]
      })

      index_env <- new.env()
      index_env$indices <- numeric()

      output$types <- shiny::renderUI({
        purrr::map2(
          seq_along(types_r()), names(types_r()),
          function(index, type_name) {
            if (!index %in% index_env$indices) {
              index_env$indices <- c(index_env$indices, index)

              shiny::observeEvent(input[["type" %_% index]], {
                clicked_type_index_rv(list(index = index, nonce = runif(1)))
                shiny::removeModal()
              })
            }

            htmltools::tagList(
              htmltools::tags$li(
                class = "st-badge bordered bg-lightgray",
                shiny::actionLink(
                  inputId = ns("type" %_% index),
                  label = type_name
                )
              )
            )
          }
        )
      })

      return_list <- list(
        type_id_r = type_id_r
      )

      return(return_list)
    }
  )
}
