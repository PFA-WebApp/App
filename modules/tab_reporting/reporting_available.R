reporting_available_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    title = i18n$t("stock_overview"),
    shiny::uiOutput(
      outputId = ns("type")
    ),
    shiny::checkboxInput(
      inputId = ns("critical"),
      label = i18n$t("only_critical_stocks")
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
          label = i18n$t("type"),
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

      tbl_names_r <- shiny::reactive({
        .values$language_rv()

        c(
          i18n$t_chr("subtype"),
          i18n$t_chr("available"),
          i18n$t_chr("max_available")
        )
      })

      formatted_available_table_r <- shiny::reactive({
        tbl <- available_table_r() %>%
          dplyr::mutate(
            subtype_name = db_get_subtype_name(.values$db, subtype_id)
          ) %>%
          dplyr::select(
            subtype_name, quantity, max_quantity
          )

        names(tbl) <- tbl_names_r()

        tbl
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
