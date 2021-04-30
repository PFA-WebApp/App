object_table_remove_object_ui <- function(id, object_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("remove") %_% object_id,
      label = NULL,
      icon = shiny::icon("trash-alt"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          object_id: {object_id},
          nonce: Math.random()
        }});',
        inputId = ns("remove"),
        object_id = object_id
      )
    )
  )
}

object_table_remove_object_server <- function(id,
                                              .values,
                                              settings,
                                              db,
                                              label
) {
  required <- list(
    settings = "update_name",
    db = "func",
    func = c(
      "get_object_name",
      "has_object_id",
      "remove_object"
    ),
    label = c(
      "object_with_article",
      "object_with_small_article",
      "remove_btn_title"
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

      object_id_r <- shiny::reactive(
        input$remove$object_id
      )

      object_name_r <- shiny::reactive({
        purrr::walk(settings$update_name, function(update_name) {
          .values$update[[update_name]]()
        })

        db$func$get_object_name(.values$db, object_id_r())
      })

      shiny::observeEvent(input$remove, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = htmltools::tagList(
            .values$i18n$t(label$remove_btn_title),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          htmltools::p(
            .values$i18n$t(
              "msg_confirm_remove_obj",
              label$object_with_small_article,
              object_name_r()
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_remove"),
            label = .values$i18n$t("confirm")
          )
        ))
      })

      shiny::observeEvent(input$confirm_remove, {
        shiny::removeModal()

        # Check that object exist (object may be removed when clicking twice)
        if (!db$func$has_object_id(.values$db, object_id_r())) return()

        # Check that removal is allowed
        if (!is.null(db$func$remove_object_allowed)) {
          if (!db$func$remove_object_allowed(.values$db, object_id_r())) return()
        }

        success <- db$func$remove_object(.values$db, object_id_r())

        if (success) {
          shiny::showNotification(
            ui = .values$i18n$t(
              "msg_remove_successful",
              "${p_[[2]]} \"${p_[[3]]}\"",
              label$object_with_article,
              object_name_r()
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = .values$i18n$t(
              "err_remove_not_successful",
              "${p_[[2]]} \"${p_[[3]]}\"",
              label$object_with_article,
              object_name_r()
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

        .values$update$circulation(.values$update$circulation() + 1)
      })
    }
  )
}
