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
                                        db,
                                        settings,
                                        label,
                                        type_id_r,
                                        object_ids_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      type_name_r <- shiny::reactive({
        .values$update$type()
        db_get_type_name(.values$db, type_id_r())
      })

      object_names_r <- shiny::reactive({
        .values$update[[settings$table_name]]()
        db$get_object_name(.values$db, object_ids_r())
      })

      path_r <- shiny::reactive(
        # returns a vector of paths if length(object_ids_r()) > 1
        file.path("files", settings$table_name, object_ids_r())
      )

      files_table_r <- shiny::reactive({
        .values$update$files()

        empty_tbl <- tibble::tibble(
          path = character(),
          file = character(),
          name = character()
        )

        tbls <- if (length(object_ids_r())) {
          purrr::map2(path_r(), object_names_r(), function(path, name) {
            files <- list.files(
              path = path,
              all.files = TRUE,
              no.. = TRUE
            )

            if (length(files)) {
              tibble::tibble(
                path = path,
                file = files,
                name = name
              )
            } else {
              empty_tbl
            }
          })
        } else {
          empty_tbl
        }

        dplyr::bind_rows(tbls)
      })

      files_r <- shiny::reactive({
        files_table_r()$file
      })

      href_r <- shiny::reactive({
        tbl <- files_table_r()
        file.path(tbl$path, tbl$file)
      })

      file_table_names_r <- shiny::reactive({
        .values$language_rv()
        c(
          .values$i18n$t_chr("file"),
          if (length(object_ids_r()) > 1) .values$i18n$t_chr(label$object_name)
        )
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
          file = files_ui
        )

        # Add object_name_information (multiple groups)
        if (length(object_ids_r()) > 1) {
          tbl[[settings$table_name]] <- files_table_r()$name
        }

        names(tbl) <- file_table_names_r()

        DT::datatable(
          tbl,
          escape = FALSE,
          options = list(
            language = list(
              url = .values$dt_language_r()
            )
          )
        )
      })

      output$download_all_button <- shiny::renderUI({
        if (length(files_r())) {
          shiny::downloadButton(
            outputId = ns("download_all"),
            label = .values$i18n$t("download_directory"),
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
            files = href_r(),
            extras = "-j -q"
          )
        },
        contentType = "application/zip"
      )

      download_all_name_r <- shiny::reactive({
        name <- type_name_r() %_%
          .values$settings$table_dict()[[settings$table_name]] %>%
          paste0(".zip")
        stringr::str_replace_all(name, "\\s", "_")
      })

      return_list <- list(
        files_r = files_r
      )

      return(return_list)
    }
  )
}
