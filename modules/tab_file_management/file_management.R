file_management_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

file_management_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
