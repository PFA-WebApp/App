group_table_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = "Gruppentabelle",
    solidHeader = TRUE,
    DT::dataTableOutput(
      outputId = ns("group_table")
    )
  )
}

group_table_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      taken_group_ids_rvs <- shiny::reactiveValues(
        change_group_name = character(),
        remove = character()
      )

      output$group_table <- DT::renderDataTable({
        .values$update$group()

        tbl <- DB::db_get_table(.values$db, "groups")

        tbl$change_group_name <- purrr::map_chr(
          tbl$group_id,
          function(group_id) {
            if (!group_id %in% taken_group_ids_rvs$change_group_name) {
              taken_group_ids_rvs$change_group_name <- c(
                taken_group_ids_rvs$change_group_name, group_id
              )

              group_table_change_group_name_server(
                id = "group_table_change_group_name" %_% group_id,
                .values = .values,
                group_id = group_id
              )
            }

            group_table_change_group_name_ui(
              id = ns("group_table_change_group_name" %_% group_id)
            )
          }
        )

        tbl$remove <- purrr::map_chr(
          tbl$group_id,
          function(group_id) {
            if (!group_id %in% taken_group_ids_rvs$remove) {
              taken_group_ids_rvs$remove <- c(
                taken_group_ids_rvs$remove, group_id
              )

              group_table_remove_group_server(
                id = "group_table_remove_group" %_% group_id,
                .values = .values,
                group_id = group_id
              )
            }

            group_table_remove_group_ui(
              id = ns("group_table_remove_group" %_% group_id)
            )
          }
        )

        tbl <- tbl %>%
          dplyr::select(group_name, change_group_name, remove)

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
                targets = 2:3
              )
            )
          ),
          escape = FALSE,
          colnames = c("Gruppenname", "Gruppenname ändern", "Entfernen")
        )
      })
    }
  )
}
