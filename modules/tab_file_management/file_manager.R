file_manager_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("select_type")
    ),
    shiny::uiOutput(
      outputId = ns("select_object")
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
    shiny::uiOutput(
      outputId = ns("download_all_button")
    )
  )
}

file_manager_server <- function(id, .values, db, settings, label) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      if (settings$table_name == "subtype") {
        type_choices_r <- shiny::reactive({
          .values$update$type()
          db_get_types(.values$db)
        })

        output$select_type <- shiny::renderUI({
          shiny::selectInput(
            inputId = ns("select_type"),
            label = "Typ",
            choices = type_choices_r(),
            .values$device$large
          )
        })
      }

      output$select_object <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("select_object"),
          label = label$object_name,
          choices = choices_r(),
          selectize = .values$device$large
        )
      })

      choices_r <- shiny::reactive({
        purrr::walk(settings$update_name, function(update_name) {
          .values$update[[update_name]]()
        })

        if (settings$table_name == "subtype") {
          db_get_subtypes_by_type_id(.values$db, shiny::req(input$select_type))
        } else {
          db$get_objects(.values$db)
        }
      })

      object_id_r <- shiny::reactive({
        shiny::req(input$select_object)
      })

      object_name_r <- shiny::reactive({
        names(choices_r()[choices_r() == input$select_object])
      })

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

      paths_r <- shiny::reactive({
        file.path(path_r(), files_r())
      })

      output$files <- DT::renderDataTable({
        files_ui <- purrr::map2_chr(
          files_r(), seq_along(files_r()),
          function(name, index) {
            file_manager_rename_ui(
              id = ns("file_manager_rename"),
              index = index,
              name = name
            )
          }
        )

        download_ui <- purrr::map_chr(paths_r(), function(href) {
          file_manager_download_btn(href)
        })

        remove_ui <- purrr::map_chr(seq_along(files_r()), function(index) {
          file_manager_remove_ui(
            id = ns("file_manager_remove"),
            index = index
          )
        })

        tbl <- tibble::tibble(
          Datei = files_ui,
          Herunterladen = download_ui,
          "Löschen" = remove_ui
        )

        DT::datatable(
          tbl,
          escape = FALSE,
          options = list(
            columnDefs = list(
              list(
                className = 'dt-center',
                targets = 2:3
              )
            ),
            language = list(
              url = .values$dt_language_r()
            )
          )
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

            target <- file.path("files", settings$table_name, object_id_r(), name)
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

      download_all_name_r <- shiny::reactive({
        name <- .values$settings$table_dict()[[settings$table_name]] %_% object_name_r() %>%
          paste0(".zip")
        stringr::str_replace_all(name, "\\s", "_")
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

      output$download_all <- shiny::downloadHandler(
        filename = download_all_name_r,
        content = function(file) {
          utils::zip(
            zipfile = file,
            files = file.path("files", settings$table_name, object_id_r()),
            extras = "-j -q"
          )
        },
        contentType = "application/zip"
      )

      file_manager_rename_server(
        id = "file_manager_rename",
        .values = .values,
        files_r = files_r,
        paths_r = paths_r,
        object_id_r = object_id_r,
        settings = settings
      )

      file_manager_remove_server(
        id = "file_manager_remove",
        .values = .values,
        files_r = files_r,
        paths_r = paths_r,
        object_id = object_id_r,
        settings = settings
      )
    }
  )
}
