file_manager_rename_ui <- function(id, index, name) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionLink(
      inputId = ns("name") %_% index,
      label = htmltools::div(
        id = ns("label-container"),
        htmltools::div(
          id = ns("label"),
          name
        )
      ),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          index: {index},
          nonce: Math.random()
        }});',
        inputId = ns("name"),
        index = index
      )
    )
  )
}

file_manager_rename_server <- function(id,
                                       .values,
                                       files_r,
                                       paths_r,
                                       object_id_r,
                                       settings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      file_r <- shiny::eventReactive(input$name, {
        files_r()[[shiny::req(input$name$index)]]
      })

      file_path_r <- shiny::reactive({
        paths_r()[[shiny::req(input$name$index)]]
      })

      other_files_r <- shiny::reactive({
        setdiff(files_r(), file_r())
      })

      shiny::observeEvent(file_r(), {
        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            i18n$t("${edit_file_name}"),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          shiny::textInput(
            inputId = ns("object_name"),
            label = i18n$t("${new_file_name}"),
            value = stringr::str_split(file_r(), "\\.(pdf|PDF|Pdf)$")[[1]][1]
          ),
          shiny::uiOutput(
            outputId = ns("name_taken"),
            class = "pfa-error"
          ),
          shiny::uiOutput(
            outputId = ns("name_too_short"),
            class = "pfa-error"
          ),
          footer = shiny::uiOutput(
            outputId = ns("confirm_object_name"),
            class = "pfa-error"
          )
        ))
      }, priority = -1)

      output$name_taken <- shiny::renderUI({
        if (name_taken_r()) {
          i18n$t(
            "${err_name_taken}",
            "${file_name_with_article}"
          )
        }
      })

      output$name_too_short <- shiny::renderUI({
        if (name_too_short_r()) {
          i18n$t("${err_file_name_too_short}")
        }
      })

      output$confirm_object_name <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("confirm_object_name"),
              label = i18n$t("${confirm}")
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("confirm_object_name"),
            label = i18n$t("${confirm}")
          )
        }
      })

      name_taken_r <- shiny::reactive({
        input$object_name %in% other_files_r()
      })

      name_too_short_r <- shiny::reactive({
        nchar(input$object_name) < 1
      })

      error_r <- shiny::reactive({
        name_taken_r() ||
          name_too_short_r()
      })

      shiny::observeEvent(input$confirm_object_name, {
        shiny::removeModal()

        file_name <- paste0(input$object_name, ".pdf")

        target <- file.path(
          "files", settings$table_name, object_id_r(), file_name
        )

        file.rename(file_path_r(), target)

        shiny::showNotification(
          ui = i18n$t(
            "${msg_file_name_edit_successful}",
            file_name
          ),
          type = "warning",
          duration = 5
        )

        .values$update$files(.values$update$files() + 1)
      })
    }
  )
}
