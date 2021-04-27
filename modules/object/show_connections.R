show_connections_ui <- function(id, title) {
  ns <- shiny::NS(id)

  bs4Dash::box(
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
        purrr::walk(settings$update_name, function(update_name) {
          .values$update[[update_name]]()
        })

        db$func$get_objects(.values$db)
      })

      output$select_object <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("object"),
          label = i18n$t(label$object),
          choices = objects_r(),
          selected = NA,
          selectize = .values$device$large
        )
      })

      colnames_r <- shiny::reactive({
        .values$language_rv()

        i18n$t_chr(label$connection_name)
      })

      output$connections_table <- DT::renderDataTable({
        .values$update$group_type()
        connections <- db$func$get_connections(.values$db, input$object)

        tbl <- tibble(connections = names(connections)) %>%
          dplyr::arrange(connections)

        DT::datatable(
          tbl,
          colnames = colnames_r(),
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
