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
        transaction_table_r() %>%
          dplyr::arrange(desc(time)) %>%
          dplyr::mutate(
            type_id = db_get_type_id_by_subtype_id(.values$db, subtype_id),
            user_name = db_get_user_name(.values$db, user_id),
            type_name = db_get_type_name(.values$db, type_id),
            subtype_name = db_get_subtype_name(.values$db, subtype_id)
          ) %>%
          dplyr::select(
            Nutzer = user_name, Typ = type_name, Untertyp = subtype_name,
            Datum = time, Menge = quantity
          )
      })

      output$table <- DT::renderDataTable({
        DT::datatable(
          formatted_transaction_table_r()
        )
      })
    }
  )
}
