operate_groups_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Gruppen des ausgewählten Typs",
    shiny::uiOutput(
      outputId = ns("groups"),
      container = htmltools::tags$ul,
      class = "group-badges flex bordered"
    )
  )
}

operate_groups_server <- function(id, .values, type_id_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      groups_r <- shiny::reactive({
        db_get_groups_by_type(.values$db, type_id_r())
      })

      output$groups <- shiny::renderUI({
        purrr::map(names(groups_r()), function(group_name) {
          htmltools::tagList(
            htmltools::tags$li(
              class = "group-badge bordered bg-lightgray",
              htmltools::tags$a(
                href = "#",
                group_name
              )
            )
          )
        })
      })
    }
  )
}
