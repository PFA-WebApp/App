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
            "Dateinamen bearbeiten",
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          shiny::textInput(
            inputId = ns("object_name"),
            label = "Neuer Dateiname",
            value = stringr::str_split(file_r(), "\\.(pdf|PDF|Pdf)$")[[1]][1]
          ),
          shiny::uiOutput(
            outputId = ns("name_taken")
          ),
          shiny::uiOutput(
            outputId = ns("name_too_short")
          ),
          footer = shiny::uiOutput(
            outputId = ns("confirm_object_name")
          )
        ))
      }, priority = -1)

      output$name_taken <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !name_taken_r(),
            "Der Dateiname existiert bereits!"
          ),
          errorClass = "PFA"
        )
      })

      output$name_too_short <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !name_too_short_r(),
            "Der Dateiname ist zu kurz."
          ),
          errorClass = "PFA"
        )
      })

      output$confirm_object_name <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("confirm_object_name"),
              label = "Bestätigen"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("confirm_object_name"),
            label = "Bestätigen"
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
          ui = paste0(
            "Der Dateiname wurde erfolgreich zu \"",
            file_name,
            "\" geändert."
          ),
          type = "warning",
          duration = 5
        )

        .values$update$files(.values$update$files() + 1)
      })
    }
  )
}
