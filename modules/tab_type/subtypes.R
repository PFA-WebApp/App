subtypes_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Untertypen bearbeiten",
    shiny::uiOutput(
      outputId = ns("select_type")
    ),
    DT::dataTableOutput(
      outputId = ns("subtype_table")
    )
  )
}

subtypes_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_type <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("type"),
          label = "Typ",
          choices = db_get_types(db)
        )
      })

      output$subtype_table <- DT::renderDataTable({
        subtype_table <- db_get_subtype_table_by_type_id(db, input$type)

        DT::datatable(
          subtype_table
        )
      })
    }
  )
}
