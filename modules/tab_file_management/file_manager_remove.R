file_manager_remove_ui <- function(id, index) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = "undefined",
      label = NULL,
      icon = shiny::icon("trash-alt"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          index: {index},
          nonce: Math.random()
        }});',
        inputId = ns("remove"),
        index = index
      )
    )
  )
}

file_manager_remove_server <- function(id,
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

      file_r <- shiny::eventReactive(input$remove, {
        files_r()[[shiny::req(input$remove$index)]]
      })

      file_path_r <- shiny::reactive({
        paths_r()[[shiny::req(input$remove$index)]]
      })

      shiny::observeEvent(file_r(), {
        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            i18n$t("remove_file"),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          i18n$t(
            "msg_confirm_file_remove",
            file_r()
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_remove"),
            label = i18n$t("confirm")
          )
        ))
      }, priority = -1)

      shiny::observeEvent(input$confirm_remove, {
        shiny::removeModal()

        file.remove(file_path_r())

        shiny::showNotification(
          ui = i18n$t(
            "msg_remove_successful",
            "${file_with_article} \"${p_[[2]]}\"",
            file_r()
          ),
          type = "warning",
          duration = 5
        )

        .values$update$files(.values$update$files() + 1)
      })
    }
  )
}
