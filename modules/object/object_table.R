object_table_box_ui <- function(id, title) {
  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = title,
    solidHeader = TRUE,
    object_table_ui(
      id = id
    )
  )
}

object_table_ui <- function(id) {
  ns <- shiny::NS(id)

  DT::dataTableOutput(
    outputId = ns("object_table")
  )
}

object_table_server <- function(id,
                                .values,
                                settings,
                                db,
                                label
) {
  required_settings <- c(
    "is_group_object", "update_name", "length_name"
  )

  stopifnot(all(required_settings %in% names(settings)))

  default_settings <- list(
    show = c("name", "connections", "remove")
  )

  for (name in names(default_settings)) {
    if (!name %in% names(settings)) settings[[name]] <- default_settings[[name]]
  }

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

        if (!is.null(db$func$filter_table)) {
          tbl <- dplyr::filter(
            tbl,
            rowid %in% db$func$filter_table(.values$db)
          )
        }

        if ("connections" %in% settings$show) {
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
        }

        if ("name" %in% settings$show) {
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
        }

        if ("remove" %in% settings$show) {
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
        }

        x <- db$name_column
        tbl <- tbl[settings$show]

        targets <- which(settings$show != "name")

        tbl <- tbl[rev(seq_len(nrow(tbl))), , drop = FALSE]

        stopifnot(length(tbl) == length(label$colnames))

        DT::datatable(
          data = tbl,
          options = list(
            columnDefs = list(
              list(
                className = 'dt-center',
                targets = targets
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
