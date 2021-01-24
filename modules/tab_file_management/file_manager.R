file_manager_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("select_group")
    ),
    DT::dataTableOutput(
      outputId = ns("files")
    )
  )
}

file_manager_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_group <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("select_group"),
          label = "Gruppe",
          choices = choices_r()
        )
      })

      choices_r <- shiny::reactive({
        db_get_groups(.values$db)
      })

      object_id_r <- shiny::reactive({
        shiny::req(input$select_group)
      })

      path_r <- shiny::reactive(
        file.path("files", "group", object_id_r())
      )

      files_r <- shiny::reactive({
        list.files(
          path = path_r()
        )
      })

      paths_r <- shiny::reactive({
        file.path(path_r(), files_r())
      })

      output$files <- DT::renderDataTable({
        files_ui <- purrr::map2_chr(files_r(), paths_r(), function(name, href) {
          file_manager_link(name, href)
        })

        tbl <- tibble::tibble(
          Datei = files_ui
        )

        DT::datatable(
          tbl,
          escape = FALSE
        )
      })
    }
  )
}
