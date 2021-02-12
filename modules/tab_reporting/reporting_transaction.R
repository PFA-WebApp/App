reporting_transaction_ui <- function(id) {
  ns <- shiny::NS(id)

  DT::dataTableOutput(
    outputId = ns("table")
  )
}

reporting_transaction_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      transaction_table_r <- shiny::reactive({
        .values$update$circulation()
        db_get_table(.values$db, "circulation")
      })

      formatted_transaction_table_r <- shiny::reactive({
        tbl <- transaction_table_r()

        if (.values$user$status() != "admin") {
          tbl <- dplyr::filter(tbl, user_id == .values$user$id())
        }

        tbl <- tbl %>%
          dplyr::arrange(desc(time)) %>%
          dplyr::mutate(
            type_id = db_get_type_id_by_subtype_id(.values$db, subtype_id),
            user_name = db_get_user_name(.values$db, user_id),
            type_name = db_get_type_name(.values$db, type_id),
            subtype_name = db_get_subtype_name(.values$db, subtype_id),
            quantity = -quantity
          ) %>%
          dplyr::select(
            Nutzer = user_name, Typ = type_name, Untertyp = subtype_name,
            Datum = time, Menge = quantity
          )

        if (.values$user$status() != "admin") {
          tbl <- dplyr::select(tbl, -Nutzer)
        }

        tbl
      })

      output$table <- DT::renderDataTable({
        DT::datatable(
          formatted_transaction_table_r()
        ) %>%
        DT::formatStyle(
          columns = "Menge",
          color = DT::styleInterval(
            cuts = 0,
            values = c("rgb(221, 75, 57)", "rgb(0, 166, 90)")
          )
        )
      })
    }
  )
}
