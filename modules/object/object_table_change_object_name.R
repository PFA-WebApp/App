object_table_change_object_name_ui <- function(id, name) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionLink(
      inputId = ns("change_object_name"),
      label = htmltools::div(
        id = ns("label-container"),
        htmltools::div(
          id = ns("label"),
          name
        )
      ),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("change_object_name")
      )
    )
  )
}

object_table_change_object_name_server <- function(id,
                                                   .values,
                                                   object_id,
                                                   settings,
                                                   db,
                                                   label
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      old_object_name_r <- shiny::reactive({
        .values$update[[settings$update_name]]()
        objects <- db$func$get_objects(.values$db)
        names(objects[objects == object_id][1])
      })

      shiny::observeEvent(input$change_object_name, {
        shiny::showModal(shiny::modalDialog(
          title = label$change_name,
          easyClose = TRUE,
          shiny::textInput(
            inputId = ns("object_name"),
            label = label$new_name,
            value = old_object_name_r()
          ),
          shiny::uiOutput(
            outputId = ns("wrong_name_length")
          ),
          shiny::uiOutput(
            outputId = ns("name_taken")
          ),
          footer = shiny::uiOutput(
            outputId = ns("confirm_object_name")
          )
        ))
      })

      output$wrong_name_length <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !name_too_short_r(),
            paste(
              label$object_name_with_article,
              "benötigt mindestens",
              as_german(.values$settings[[settings$length_name]]$length$min),
              "Zeichen!\n\n"
            )
          ),
          shiny::need(
            !name_too_long_r(),
            paste(
              label$object_name_with_article,
              "darf nicht länger sein als",
              as_german(.values$settings[[settings$length_name]]$length$max),
              "Zeichen!\n\n"
            )
          ),
          errorClass = "PFA"
        )
      })

      output$name_taken <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !name_taken_r(),
            paste(
              label$object_name_with_article,
              "existiert bereits!\n\n"
            )
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

      name_too_short_r <- shiny::reactive({
        nchar(input$object_name) < .values$settings[[settings$length_name]]$length$min
      })

      name_too_long_r <- shiny::reactive({
        nchar(input$object_name) > .values$settings[[settings$length_name]]$length$max
      })

      name_taken_r <- shiny::reactive({
        db$func$has_object_name(.values$db, input$object_name)
      })

      error_r <- shiny::reactive({
        name_too_short_r() ||
          name_too_long_r() ||
          name_taken_r()
      })

      shiny::observeEvent(input$confirm_object_name, {
        shiny::removeModal()

        success <- db$func$set_object_name(.values$db, object_id, input$object_name)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              label$object_name_with_article,
              " wurde erfolgreich zu \"",
              input$object_name,
              "\" geändert."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              label$object_name_with_article,
              " konnte nicht geändert werden."
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update[[settings$update_name]](
          .values$update[[settings$update_name]]() + 1
        )
      })
    }
  )
}
