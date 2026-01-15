object_table_name_ui <- function(id, object_id, name) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionLink(
      inputId = ns("name") %_% object_id,
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
          object_id: {object_id},
          nonce: Math.random()
        }});',
        inputId = ns("name"),
        object_id = object_id
      )
    )
  )
}

object_table_name_server <- function(id,
                                     .values,
                                     settings,
                                     db,
                                     label
) {
  required <- list(
    settings = c(
      "length_name",
      "update_name"
    ),
    db = "func",
    func = c(
      "get_object_name",
      "has_object_name",
      "set_object_name"
    ),
    label = c(
      "change_name",
      "new_name",
      "object_name_with_article"
    )
  )

  check_required(
    required,
    settings,
    db,
    db$func,
    label
  )

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      object_id_r <- shiny::reactive({
        input$name$object_id
      })

      old_object_name_r <- shiny::reactive({
        purrr::walk(settings$update_name, function(update_name) {
          .values$update[[update_name]]()
        })

        db$func$get_object_name(.values$db, object_id_r())
      })

      # observe input$name and not object_id_r, because input$name fires
      # even if the same object is clicked again
      shiny::observeEvent(object_id_r(), {
        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            .values$i18n$t(label$change_name),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          shiny::textInput(
            inputId = ns("object_name"),
            label = .values$i18n$t(label$new_name),
            value = old_object_name_r()
          ),
          shiny::uiOutput(
            outputId = ns("wrong_name_length"),
            class = "pfa-error"
          ),
          shiny::uiOutput(
            outputId = ns("name_taken"),
            class = "pfa-error"
          ),
          footer = shiny::uiOutput(
            outputId = ns("confirm_object_name")
          )
        ))
      }, priority = -1)

      output$wrong_name_length <- shiny::renderUI({
        if (name_too_short_r()) {
          return(.values$i18n$t(
            "err_min_chars",
            label$object_name_with_article,
            format_number(.values$settings[[settings$length_name]]$length$min)
          ))
        }

        if (name_too_short_r()) {
          return(.values$i18n$t(
            "err_max_chars",
            label$object_name_with_article,
            format_number(.values$settings[[settings$length_name]]$length$max)
          ))
        }
      })

      output$name_taken <- shiny::renderUI({
        if (name_taken_r()) {
          .values$i18n$t(
            "err_name_taken",
            label$object_name_with_article
          )
        }
      })

      output$confirm_object_name <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("confirm_object_name"),
              label = .values$i18n$t("confirm")
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("confirm_object_name"),
            label = .values$i18n$t("confirm")
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
        if (input$object_name == old_object_name_r()) return(FALSE)
        db$func$has_object_name(.values$db, input$object_name)
      })

      error_r <- shiny::reactive({
        name_too_short_r() ||
          name_too_long_r() ||
          name_taken_r()
      })

      shiny::observeEvent(input$confirm_object_name, {
        shiny::removeModal()

        success <- db$func$set_object_name(.values$db, object_id_r(), input$object_name)

        if (success) {
          shiny::showNotification(
            ui = .values$i18n$t(
              "msg_obj_changed_to",
              label$object_name_with_article,
              input$object_name
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = .values$i18n$t(
              "err_object_could_not_be_changed",
              label$object_name_with_article
            ),
            type = "error",
            duration = 5
          )
        }

        purrr::walk(settings$update_name, function(update_name) {
          .values$update[[update_name]](
            .values$update[[update_name]]() + 1
          )
        })
      })
    }
  )
}
