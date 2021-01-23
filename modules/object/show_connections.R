show_connections_ui <- function(id, title) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = title,
    solidHeader = TRUE,
    shiny::uiOutput(
      outputId = ns("select_object")
    ),
    DT::dataTableOutput(
      outputId = ns("connections_table")
    )
  )
}

show_connections_server <- function(id, .values, settings, db, label) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      objects_r <- shiny::reactive({
        .values$update[[settings$update_name]]()
        db$func$get_objects(.values$db)
      })

      output$select_object <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("object"),
          label = label$object,
          choices = objects_r(),
          selected = NA
        )
      })

      output$connections_table <- DT::renderDataTable({
        .values$update$group_type()
        connections <- db$func$get_connections(.values$db, input$object)

        tbl <- tibble(connections = names(connections))

        DT::datatable(
          tbl,
          colnames = label$connection_name
        )
      })
    }
  )
}
