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
        .values$update$user()
        db_get_circulation_table(.values$db)
      })

      formatted_transaction_table_r <- shiny::reactive({
        tbl <- transaction_table_r()

        if (.values$user$status() != "admin") {
          tbl <- dplyr::filter(tbl, user_id == .values$user$id())
        }

        tbl <- tbl %>%
          dplyr::arrange(desc(time)) %>%
          dplyr::mutate(
            user_name = dplyr::case_when(
              as.logical(user_removed) ~ paste(user_name, "-", user_id, "(gelöscht)"),
              TRUE ~ user_name
            ),
            quantity = -quantity
          ) %>%
          dplyr::select(
            Nutzer = user_name, Typ = type_name, Untertyp = subtype_name,
            Datum = time, Menge = quantity, Entfernt = user_removed
          )

        if (.values$user$status() != "admin") {
          tbl <- dplyr::select(tbl, -Nutzer)
        }

        tbl
      })

      output$table <- DT::renderDataTable({
        tbl <- formatted_transaction_table_r()

        DT::datatable(
          formatted_transaction_table_r(),
          options = list(
            columnDefs = list(
              list(
                targets = which(names(tbl) == "Entfernt"),
                visible = FALSE
              )
            )
          )
        ) %>%  DT::formatStyle(
          columns = "Menge",
          color = DT::styleInterval(
            cuts = 0,
            values = c("rgb(221, 75, 57)", "rgb(0, 166, 90)")
          )
        ) %>% DT::formatStyle(
          columns = "Nutzer",
          valueColumns = "Entfernt",
          color = DT::styleInterval(
            cuts = 0,
            values = c("inherit", "rgb(221, 75, 57)")
          )
        )
      })
    }
  )
}
