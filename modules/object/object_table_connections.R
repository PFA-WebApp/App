object_table_connections_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("connections"),
      label = NULL,
      icon = shiny::icon("cog"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("connections")
      )
    )
  )
}

object_table_connections_server <- function(id,
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

      object_connections_r <- shiny::reactive({
        .values$update$group_type()
        objects <- db$func$get_connections(.values$db, object_id)
      })

      shiny::observeEvent(input$connections, {
        title <- paste0(
          label$change_connections,
          " \"", db$func$get_object_name(.values$db, object_id),
          "\""
        )

        shiny::showModal(shiny::modalDialog(
          title = title,
          easyClose = TRUE,
          shiny::selectInput(
            inputId = ns("object_connections"),
            label = label$connections,
            selected = object_connections_r(),
            choices = db$func$get_possible_connections(.values$db),
            multiple = TRUE
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
            group_id = object_id,
            type_ids = connections_r()
          )
        } else {
          db_set_group_type_by_type_id(
            .values$db,
            type_id = object_id,
            group_ids = connections_r()
          )
        }

        shiny::showNotification(
          ui = paste0(
            label$connection_modification,
            " \"",
            db$func$get_object_name(.values$db, object_id),
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
