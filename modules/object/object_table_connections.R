object_table_connections_ui <- function(id, object_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("connections") %_% object_id,
      label = NULL,
      icon = shiny::icon("cog"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          object_id: {object_id},
          nonce: Math.random()
        }});',
        inputId = ns("connections"),
        object_id = object_id
      )
    )
  )
}

object_table_connections_server <- function(id,
                                            .values,
                                            settings,
                                            db,
                                            label
) {
  required <- list(
    settings = "is_group_object",
    db = "func",
    func = c(
      "get_connections",
      "get_object_name",
      "get_possible_connections"
    ),
    label = c(
      "change_connections",
      "connections",
      "connection_modification"
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
        input$connections$object_id
      })

      object_connections_r <- shiny::reactive({
        .values$update$group_type()
        objects <- db$func$get_connections(.values$db, object_id_r())
      })

      shiny::observeEvent(input$connections, {
        title <- paste0(
          label$change_connections,
          " \"", db$func$get_object_name(.values$db, object_id_r()),
          "\""
        )

        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            title,
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          shiny::selectInput(
            inputId = ns("object_connections"),
            label = label$connections,
            selected = object_connections_r(),
            choices = db$func$get_possible_connections(.values$db),
            multiple = TRUE,
            selectize = .values$device$large
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_object_connections"),
            label = "Bestätigen"
          )
        ))
      })

      connections_r <- shiny::reactive({
        if (
          purrr::is_null(input$object_connections) ||
          input$object_connections == ""
        ) return(integer())

        as.integer(input$object_connections)
      })

      shiny::observeEvent(input$confirm_object_connections, {
        shiny::removeModal()

        if (settings$is_group_object) {
          db_set_group_type_by_group_id(
            .values$db,
            group_id = object_id_r(),
            type_ids = connections_r()
          )
        } else {
          db_set_group_type_by_type_id(
            .values$db,
            type_id = object_id_r(),
            group_ids = connections_r()
          )
        }

        shiny::showNotification(
          ui = paste0(
            label$connection_modification,
            " \"",
            db$func$get_object_name(.values$db, object_id_r()),
            "\" wurden erfolgreich bearbeitet."
          ),
          type = "warning",
          duration = 5
        )

        .values$update$group_type(.values$update$group_type() + 1)
      })
    }
  )
}
