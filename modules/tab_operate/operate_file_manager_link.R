operate_file_manager_link_ui <- function(id, index, name) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionLink(
      inputId = "undefined",
      label = name,
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          index: {index},
          nonce: Math.random()
        }});',
        inputId = ns("download"),
        index = index
      )
    )
  )
}

operate_file_manager_link_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
