reporting_table_ui <- function(id) {
  ns <- shiny::NS(id)

  DT::dataTableOutput(
    outputId = ns("table")
  )
}

reporting_table_server <- function(id, .values, settings, object_id_r = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      borrow_summary_r <- shiny::reactive({
        .values$update$circulation()
        .values$update$subtype()
        .values$update$type()

        switch(
          settings$summary,
          "all" = db_get_borrow_summary(.values$db),
          "user" = db_get_borrow_summary_by_user_id(
            .values$db, object_id_r()
          ),
          "subtype" = db_get_borrow_summary_by_subtype_id(
            .values$db, object_id_r()
          )
        )
      })

      formatted_borrow_summary_r <- shiny::reactive({
        if (settings$summary %in% c("user", "all")) {
          tbl <- borrow_summary_r() %>%
            dplyr::mutate(
              type_id = db_get_type_id_by_subtype_id(.values$db, subtype_id),
              type_name = db_get_type_name(.values$db, type_id),
              subtype_name = db_get_subtype_name(.values$db, subtype_id)
            ) %>%
            dplyr::arrange(type_name, subtype_name) %>%
            dplyr::select(
              type_name, subtype_name, quantity, time
            )
        } else {
          borrow_summary_r() %>%
            dplyr::mutate(
              user_name = db_get_user_name(.values$db, user_id)
            ) %>%
            dplyr::arrange(user_name) %>%
            dplyr::select(
              user_name, quantity, time
            )
        }
      })

      tbl_names_r <- shiny::reactive({
        .values$language_rv()

        if (settings$summary %in% c("user", "all")) {
          c(
            i18n$t_chr("type"),
            i18n$t_chr("subtype"),
            i18n$t_chr("quantity"),
            i18n$t_chr("last_borrowed")
          )
        } else {
          c(
            i18n$t_chr("user_name"),
            i18n$t_chr("quantity"),
            i18n$t_chr("last_borrowed")
          )
        }
      })

      output$table <- DT::renderDataTable({
        tbl <- formatted_borrow_summary_r()

        names(tbl) <- tbl_names_r()

        DT::datatable(
          tbl,
          options = list(
            language = list(
              url = .values$dt_language_r()
            )
          )
        )
      })
    }
  )
}
