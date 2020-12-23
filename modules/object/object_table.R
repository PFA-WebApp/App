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
        change_object_connections = character(),
        change_object_name = character(),
        remove = character()
      )

      output$object_table <- DT::renderDataTable({
        .values$update[[settings$update_name]]()

        tbl <- DB::db_get_table(.values$db, db$table)



        tbl$change_object_connections <- purrr::map_chr(
          tbl[[db$id_column]],
          function(object_id) {
            if (!object_id %in% taken_object_types_rvs$change_object_connections) {
              taken_object_types_rvs$change_object_connections <- c(
                taken_object_types_rvs$change_object_connections, object_id
              )

              object_table_change_object_connections_server(
                id = "object_table_change_object_connections" %_% object_id,
                .values = .values,
                object_id = object_id,
                settings = settings,
                db = db,
                label = label
              )
            }

            object_table_change_object_connections_ui(
              id = ns("object_table_change_object_connections" %_% object_id)
            )
          }
        )



        tbl$change_object_name <- purrr::map_chr(
          tbl[[db$id_column]],
          function(object_id) {
            if (!object_id %in% taken_object_types_rvs$change_object_name) {
              taken_object_types_rvs$change_object_name <- c(
                taken_object_types_rvs$change_object_name, object_id
              )

              object_table_change_object_name_server(
                id = "object_table_change_object_name" %_% object_id,
                .values = .values,
                object_id = object_id,
                settings = settings,
                db = db,
                label = label
              )
            }

            object_table_change_object_name_ui(
              id = ns("object_table_change_object_name" %_% object_id)
            )
          }
        )

        tbl$remove <- purrr::map_chr(
          tbl[[db$id_column]],
          function(object_id) {
            if (!object_id %in% taken_object_types_rvs$remove) {
              taken_object_types_rvs$remove <- c(
                taken_object_types_rvs$remove, object_id
              )

              object_table_remove_object_server(
                id = "object_table_remove_object" %_% object_id,
                .values = .values,
                object_id = object_id,
                settings = settings,
                db = db,
                label = label
              )
            }

            object_table_remove_object_ui(
              id = ns("object_table_remove_object" %_% object_id)
            )
          }
        )

        x <- db$name_column
        tbl <- tbl %>%
          dplyr::select({{x}}, change_object_name, change_object_connections, remove)

        tbl <- tbl[rev(seq_len(nrow(tbl))), , drop = FALSE]

        DT::datatable(
          data = tbl,
          options = list(
            language = list(
              url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'
            ),
            pageLength = 6,
            columnDefs = list(
              list(
                className = 'dt-center',
                targets = 2:4
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
