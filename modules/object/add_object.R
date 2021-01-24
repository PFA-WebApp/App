add_object_ui <- function(id,
                          title,
                          label,
                          placeholder,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          ...
) {
  ns <- shiny::NS(id)

  shinydashboard::box(
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
      outputId = ns("wrong_name_length")
    ),
    shiny::uiOutput(
      outputId = ns("name_taken")
    ),
    ...,
    shiny::uiOutput(
      outputId = ns("add_object")
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

      output$add_object <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("add_object"),
              label = label$add_label,
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("add_object"),
            label = label$add_label,
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

        shiny::showNotification(
          ui = paste0(
            label$object_with_article,
            " \"",
            input$object_name,
            "\" wurde erfolgreich hinzugefügt."
          )
        )

        db$func$add_object(.values$db, input$object_name)

        .values$update[[settings$update_name]](
          .values$update[[settings$update_name]]() + 1
        )
      })

    }
  )
}
