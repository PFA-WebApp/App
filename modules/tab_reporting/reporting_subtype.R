reporting_subtype_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("type")
    ),
    shiny::uiOutput(
      outputId = ns("subtype")
    ),
    reporting_table_ui(
      id = ns("reporting_table")
    )
  )
}

reporting_subtype_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      type_choices_r <- shiny::reactive({
        .values$update$type()
        db_get_types(.values$db)
      })

      output$type <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("type"),
          label = "Typ",
          choices = type_choices_r(),
          width = "100%",
          selectize = .values$device$large
        )
      })

      subtype_choices_r <- shiny::reactive({
        .values$update$subtype()
        db_get_subtypes_by_type_id(.values$db, shiny::req(input$type))
      })

      output$subtype <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("subtype"),
          label = "Untertyp",
          choices = subtype_choices_r(),
          width = "100%",
          selectize = .values$device$large
        )
      })

      reporting_table_server(
        id = "reporting_table",
        .values = .values,
        settings = list(summary = "subtype"),
        object_id_r = shiny::reactive(shiny::req(input$subtype))
      )
    }
  )
}
