add_object_ui <- function(...,
                          id,
                          label,
                          placeholder
) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::textInput(
      inputId = ns("object_name"),
      label = label,
      placeholder = placeholder
    ),
    shiny::uiOutput(
      outputId = ns("wrong_name_length"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("name_taken"),
      class = "pfa-error"
    ),
    ...,
    shiny::uiOutput(
      outputId = ns("add_object")
    )
  )
}

add_object_box_ui <- function(...,
                          id,
                          title,
                          label,
                          placeholder,
                          collapsible = TRUE,
                          collapsed = TRUE
) {
  ns <- shiny::NS(id)

  if (!collapsible) collapsed <- FALSE

  ui <- bs4Dash::box(
    width = NULL,
    status = "primary",
    title = title,
    solidHeader = TRUE,
    collapsible = collapsible,
    collapsed = collapsed,
    shiny::textInput(
      inputId = ns("object_name"),
      label = label,
      placeholder = placeholder
    ),
    shiny::uiOutput(
      outputId = ns("wrong_name_length"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("name_taken"),
      class = "pfa-error"
    ),
    ...,
    shiny::uiOutput(
      outputId = ns("add_object"),
      class = "pfa-error"
    )
  )
}

add_object_server <- function(id,
                              .values,
                              settings,
                              db,
                              label
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      on_add_rv <- shiny::reactiveVal(0)

      output$wrong_name_length <- shiny::renderUI({
        if (name_too_short_r()) {
          return(.values$i18n$t(
            "err_min_chars",
            label$object_name_with_article,
            format_number(.values$settings[[settings$length_name]]$length$min)
          ))
        }

        if (name_too_long_r()) {
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

      output$add_object <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("add_object"),
              label = .values$i18n$t(label$add_label),
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("add_object"),
            label = .values$i18n$t(label$add_label),
            width = "100%"
          )
        }
      })

      name_too_short_r <- shiny::reactive({
        nchar(input$object_name) <
          .values$settings[[settings$length_name]]$length$min
      })

      name_too_long_r <- shiny::reactive({
        nchar(input$object_name) >
          .values$settings[[settings$length_name]]$length$max
      })

      name_taken_r <- shiny::reactive({
        db$func$has_object_name(.values$db, input$object_name)
      })

      add_object_allowed_r <- shiny::reactive({
        if (hasName(db$func, "add_object_allowed")) {
          db$func$add_object_allowed(.values$db, input$object_name)
        } else TRUE
      })

      error_r <- shiny::reactive({
        name_too_short_r() ||
          name_too_long_r() ||
          name_taken_r() ||
          !add_object_allowed_r()
      })

      shiny::observeEvent(input$add_object, {
        shiny::updateTextInput(
          session = session,
          inputId = "object_name",
          value = ""
        )

        success <- db$func$add_object(.values$db, input$object_name)

        if (success) {
          shiny::showNotification(
            ui = .values$i18n$t(
              "msg_object_added_successfully",
              label$object_with_article,
              input$object_name
            ),
            duration = 5,
            type = "warning"
          )
        } else {
          shiny::showNotification(
            ui = .values$i18n$t(
              "err_object_added_from_another_user",
              label$object_with_article,
              input$object_name
            ),
            duration = 5,
            type = "error"
          )
        }

        purrr::walk(settings$update_name, function(update_name) {
          .values$update[[update_name]](
            .values$update[[update_name]]() + 1
          )
        })

        on_add_rv(on_add_rv() + 1)
      })

      return_list <- list(
        on_add_r = shiny::reactive(on_add_rv())
      )

      return(return_list)
    }
  )
}
