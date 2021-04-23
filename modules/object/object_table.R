object_table_box_ui <- function(id, title) {
  bs4Dash::box(
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
  required <- list(
    settings = c(
      "show",
      "update_name"
    ),
    db = character(),
    func = character(),
    label = "colnames"
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

      # call required servers --------------------------------------------------
      if ("connections" %in% settings$show) {
        object_table_connections_server(
          id = "object_table_connections",
          .values = .values,
          settings = settings,
          db = db,
          label = label
        )
      }

      if ("name" %in% settings$show) {
        object_table_name_server(
          id = "object_table_name",
          .values = .values,
          settings = settings,
          db = db,
          label = label
        )
      }

      if ("quantity" %in% settings$show) {
        object_table_quantity_server(
          id = "object_table_quantity",
          .values = .values,
          settings = settings,
          db = list(
            func = list(
              get_object_quantity = db$func$get_object_quantity,
              set_object_quantity = db$func$set_object_quantity
            )
          ),
          label = label
        )
      }

      if ("critical_quantity" %in% settings$show) {
        object_table_critical_quantity_server(
          id = "object_table_critical_quantity",
          .values = .values,
          settings = settings,
          db = list(
            func = list(
              get_object_quantity = db$func$get_object_critical_quantity,
              set_object_quantity = db$func$set_object_critical_quantity
            )
          ),
          label = label
        )
      }

      if ("remove" %in% settings$show) {
        object_table_remove_object_server(
          id = "object_table_remove_object",
          .values = .values,
          settings = settings,
          db = db,
          label = label
        )
      }

      # object table -----------------------------------------------------------
      output$object_table <- DT::renderDataTable({
        purrr::walk(settings$update_name, function(update_name) {
          .values$update[[update_name]]()
        })

        tbl <- db_get_table(.values$db, db$table)

        # order by name column
        tbl <- tbl[order(tbl[[2]]),]

        if (!is.null(db$func$filter_table)) {
          tbl <- dplyr::filter(
            tbl,
            rowid %in% db$func$filter_table(.values$db)
          )
        }

        if ("connections" %in% settings$show) {
          tbl$connections <- as.character(
            map_ui(
              id = ns("object_table_connections"),
              object_ids = tbl$rowid,
              ui_func = object_table_connections_ui
            )
          )
        }

        if ("name" %in% settings$show) {
          tbl$name <- as.character(
            map_ui(
              id = ns("object_table_name"),
              object_ids = tbl$rowid,
              ui_func = object_table_name_ui,
              ui_args = list(
                name = function(object_id) {
                  db$func$get_object_name(.values$db, object_id)
                }
              )
            )
          )
        }

        if ("quantity" %in% settings$show) {
          tbl$quantity <- as.character(
            map_ui(
              id = ns("object_table_quantity"),
              object_ids = tbl$rowid,
              ui_func = object_table_quantity_ui,
              ui_args = list(
                quantity = function(object_id) {
                  db$func$get_object_quantity(.values$db, object_id)
                }
              )
            )
          )
        }

        if ("critical_quantity" %in% settings$show) {
          tbl$critical_quantity <- as.character(
            map_ui(
              id = ns("object_table_critical_quantity"),
              object_ids = tbl$rowid,
              ui_func = object_table_critical_quantity_ui,
              ui_args = list(
                quantity = function(object_id) {
                  db$func$get_object_critical_quantity(.values$db, object_id)
                }
              )
            )
          )
        }

        if ("remove" %in% settings$show) {
          tbl$remove <- as.character(
            map_ui(
              id = ns("object_table_remove_object"),
              object_ids = tbl$rowid,
              ui_func = object_table_remove_object_ui
            )
          )
        }

        tbl <- tbl[settings$show]
        tbl$id <- seq_len(nrow(tbl))
        tbl <- dplyr::relocate(tbl, id)

        targets <- which(settings$show != "name")

        stopifnot(length(tbl) == length(label$colnames) + 1)

        DT::datatable(
          data = tbl,
          options = list(
            columnDefs = list(
              list(
                className = "dt-center",
                targets = targets
              ),
              list(
                className = "dt-left",
                targets = 0
              )
            ),
            language = list(
              url = .values$dt_language_r()
            )
          ),
          escape = FALSE,
          colnames = c("", label$colnames),
          rownames = FALSE
        )
      })
    }
  )
}
