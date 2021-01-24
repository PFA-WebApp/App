file_manager_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("select_group")
    ),
    shiny::fileInput(
      inputId = ns("upload"),
      label = "Datei hochladen",
      multiple = TRUE,
      accept = "application/pdf",
      buttonLabel = "Durchsuchen",
      placeholder = "Keine Datei ausgewählt",
      width = "100%"
    ),
    DT::dataTableOutput(
      outputId = ns("files")
    ),
    htmltools::br(),
    shiny::downloadButton(
      outputId = ns("download_all"),
      label = "Verzeichnis herunterladen",
      width = "100%",
      style = "display: block"
    )
  )
}

file_manager_server <- function(id, .values, db, table_name, label) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_group <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("select_group"),
          label = label$object_name,
          choices = choices_r()
        )
      })

      choices_r <- shiny::reactive({
        db$get_objects(.values$db)
      })

      object_id_r <- shiny::reactive({
        shiny::req(input$select_group)
      })

      path_r <- shiny::reactive(
        file.path("files", table_name, object_id_r())
      )

      files_r <- shiny::reactive({
        .values$update$files()
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

      shiny::observeEvent(input$upload, {
        purrr::walk2(
          input$upload$name, input$upload$datapath,
          function(name, path) {
            if (!stringr::str_detect(name, "\\.(pdf|PDF|Pdf)$")) {
              shiny::showNotification(paste0(
                "Es dürfen nur .pdf-Dateien hochgeladen werden!"
              ), type = "error")
              return()
            }

            target <- file.path("files", table_name, object_id_r(), name)
            file.copy(path, target)

            short_name <- if (nchar(name) > 25) {
              paste0(substr(name, 1, 22), "...")
            } else name

            shiny::showNotification(paste0(
              "Die Datei \"",
              short_name,
              "\" wurde erfolgreich hochgeladen."
            ))
          }
        )

        .values$update$files(.values$update$files() + 1)
      })
    }
  )
}
