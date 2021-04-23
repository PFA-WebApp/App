reporting_available_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    title = "Bestandsübersicht",
    shiny::uiOutput(
      outputId = ns("type")
    ),
    shiny::checkboxInput(
      inputId = ns("critical"),
      label = "Nur kritische Bestände anzeigen"
    ),
    DT::dataTableOutput(
      outputId = ns("table")
    )
  )
}

reporting_available_server <- function(id, .values) {
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
          selectize = .values$device$large
        )
      })

      available_table_r <- shiny::reactive({
        .values$update$circulation()
        tbl <- db_get_available_summary(.values$db, shiny::req(input$type))

        if (input$critical) {
          tbl <- tbl %>%
            dplyr::filter(quantity <= critical_quantity)
        }

        tbl
      })

      formatted_available_table_r <- shiny::reactive({
        available_table_r() %>%
          dplyr::mutate(
            subtype_name = db_get_subtype_name(.values$db, subtype_id)
          ) %>%
          dplyr::select(
            Untertyp = subtype_name,
            "Verfügbar" = quantity,
            "Maximal verfügbar" = max_quantity
          )
      })

      output$table <- DT::renderDataTable({
        DT::datatable(
          formatted_available_table_r(),
          options = list(
            language = list(
              url = .values$dt_language_r()
            )
          )
        )
      })
    }
  )
}
