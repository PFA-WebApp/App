object_table_ui <- function(id, title) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = title,
    solidHeader = TRUE,
    DT::dataTableOutput(
      outputId = ns("object_table")
    )
  )
}

object_table_server <- function(id,
                                .values,
                                settings,
                                db,
                                label
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      taken_object_types_rvs <- shiny::reactiveValues(
        connections = character(),
        name = character(),
        remove = character()
      )

      output$object_table <- DT::renderDataTable({
        .values$update[[settings$update_name]]()

        tbl <- db_get_table(.values$db, db$table)

        tbl$connections <- as.character(
          map_ui(
            object_ids = tbl$rowid,
            ui_func = object_table_connections_ui,
            server_func = object_table_connections_server,
            rvs = taken_object_types_rvs,
            rvs_slot = "connections",
            ns = ns,
            id_prefix = "connection",
            server_args = list(
              .values = .values,
              object_id = function(object_id) object_id,
              settings = settings,
              db = db,
              label = label
            )
          )
        )

        tbl$name <- as.character(
          map_ui(
            object_ids = tbl$rowid,
            ui_func = object_table_name_ui,
            server_func = object_table_name_server,
            rvs = taken_object_types_rvs,
            rvs_slot = "name",
            ns = ns,
            id_prefix = "name",
            ui_args = list(
              name = function(object_id) db$func$get_object_name(.values$db, object_id)
            ),
            server_args = list(
              .values = .values,
              object_id = function(object_id) object_id,
              settings = settings,
              db = db,
              label = label
            )
          )
        )

        tbl$remove <- as.character(
          map_ui(
            object_ids = tbl$rowid,
            ui_func = object_table_remove_object_ui,
            server_func = object_table_remove_object_server,
            rvs = taken_object_types_rvs,
            rvs_slot = "remove",
            ns = ns,
            id_prefix = "remove",
            server_args = list(
              .values = .values,
              object_id = function(object_id) object_id,
              settings = settings,
              db = db,
              label = label
            )
          )
        )

        x <- db$name_column
        tbl <- tbl %>%
          dplyr::select(name, connections, remove)

        tbl <- tbl[rev(seq_len(nrow(tbl))), , drop = FALSE]

        DT::datatable(
          data = tbl,
          options = list(
            columnDefs = list(
              list(
                className = 'dt-center',
                targets = 2:3
              )
            )
          ),
          escape = FALSE,
          colnames = label$colnames
        )
      })
    }
  )
}
