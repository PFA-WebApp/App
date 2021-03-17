reporting_transaction_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::div(
      class = "flex-start color-boxes",
      htmltools::div(
        class = "flex",
        htmltools::div(
          class = "color-box green"
        ),
        htmltools::div(
          class = "color-text",
          "Zurückgegebene Menge"
        )
      ),
      htmltools::div(
        class = "flex",
        htmltools::div(
          class = "color-box red"
        ),
        htmltools::div(
          class = "color-text",
          "Ausgeliehene Menge"
        )
      ),
      htmltools::div(
        class = "flex",
        htmltools::div(
          class = "color-box orange"
        ),
        htmltools::div(
          class = "color-text",
          "Bestandsänderungen"
        )
      )
    ),
    DT::dataTableOutput(
      outputId = ns("table")
    )
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
              as.logical(user_removed) ~ paste0(user_name, ":", user_id, " (gelöscht)"),
              TRUE ~ user_name
            ),
            subtype_name = dplyr::case_when(
              as.logical(type_removed) ~ subtype_name,
              as.logical(subtype_removed) ~ paste0(subtype_name, ":", subtype_id, " (gelöscht)"),
              TRUE ~ subtype_name
            ),
            type_name = dplyr::case_when(
              as.logical(type_removed) ~ paste0(type_name, ":", type_id, " (gelöscht)"),
              TRUE ~ type_name
            ),
            quantity = dplyr::case_when(
              op_type == 1 ~ -quantity,
              TRUE ~ quantity
            ),
            quantity_color = dplyr::case_when(
              op_type == 1 & quantity >= 0 ~ "rgb(0, 166, 90)",
              op_type == 1 ~ "rgb(221, 75, 57)",
              TRUE ~ "rgb(255, 133, 27)"
            )
          ) %>%
          dplyr::select(
            Nutzer = user_name, Typ = type_name, Untertyp = subtype_name,
            Datum = time, Menge = quantity, user_removed, subtype_removed,
            type_removed, quantity_color
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
                targets = which(
                  names(tbl) %in% c(
                    "user_removed", "subtype_removed", "type_removed",
                    "quantity_color"
                  )
                ),
                visible = FALSE
              )
            )
          )
        ) %>%  DT::formatStyle(
          columns = "Menge",
          valueColumns = "quantity_color",
          color = DT::styleValue()
        ) %>% DT::formatStyle(
          columns = "Nutzer",
          valueColumns = "user_removed",
          color = DT::styleInterval(
            cuts = 0,
            values = c("inherit", "rgb(221, 75, 57)")
          )
        ) %>% DT::formatStyle(
          columns = "Untertyp",
          valueColumns = "subtype_removed",
          color = DT::styleInterval(
            cuts = 0,
            values = c("inherit", "rgb(221, 75, 57)")
          )
        ) %>% DT::formatStyle(
          columns = "Typ",
          valueColumns = "type_removed",
          color = DT::styleInterval(
            cuts = 0,
            values = c("inherit", "rgb(221, 75, 57)")
          )
        )
      })
    }
  )
}
