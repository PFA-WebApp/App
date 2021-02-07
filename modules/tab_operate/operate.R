operate_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 12,
      operate_circulation_ui(
        id = ns("operate_circulation")
      ),
      operate_groups_ui(
        id = ns("operate_groups")
      )
    )
  )
}

operate_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      circulation_return <- operate_circulation_server(
        id = "operate_circulation",
        .values = .values
      )

      operate_groups_server(
        id = "operate_groups",
        .values = .values,
        type_id_r = circulation_return$type_id_r
      )
    }
  )
}
