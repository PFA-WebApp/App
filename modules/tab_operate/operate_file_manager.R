operate_file_manager_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    DT::dataTableOutput(
      outputId = ns("files")
    ),
    htmltools::br(),
    shiny::uiOutput(
      outputId = ns("download_all_button")
    )
  )
}

operate_file_manager_server <- function(id,
                                        .values,
                                        settings,
                                        object_id_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      path_r <- shiny::reactive(
        file.path("files", settings$table_name, object_id_r())
      )

      files_r <- shiny::reactive({
        .values$update$files()
        list.files(
          path = path_r(),
          all.files = TRUE,
          no.. = TRUE
        )
      })

      href_r <- shiny::reactive({
        file.path(path_r(), files_r())
      })

      output$files <- DT::renderDataTable({
        files_ui <- purrr::map2_chr(
          files_r(), href_r(),
          function(name, href) {
            operate_file_manager_link(
              name = name,
              href = href
            )
          }
        )

        tbl <- tibble::tibble(
          Datei = files_ui
        )

        DT::datatable(
          tbl,
          escape = FALSE
        )
      })

      output$download_all_button <- shiny::renderUI({
        if (length(files_r())) {
          shiny::downloadButton(
            outputId = ns("download_all"),
            label = "Verzeichnis herunterladen",
            width = "100%",
            style = "display: block"
          )
        }
      })
    }
  )
}
