user_table_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    status = "primary",
    title = i18n$t("user_table"),
    solidHeader = TRUE,
    DT::dataTableOutput(
      outputId = ns("user_table")
    )
  )
}

user_table_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_table_change_status_server(
        id = "user_table_change_status",
        .values = .values
      )

      user_table_remove_user_server(
        id = "user_table_remove_user",
        .values = .values
      )

      user_table_reset_password_server(
        id = "user_table_reset_password",
        .values = .values
      )

      output$user_table <- DT::renderDataTable({
        .values$update$user()

        tbl <- db_get_table(.values$db, "user") %>%
          dplyr::filter(removed == 0)

        tbl$change_status <- purrr::map_chr(tbl$rowid, function(user_id) {
          user_table_change_status_ui(
            id = ns("user_table_change_status"),
            user_id = user_id
          )
        })

        tbl$remove <- purrr::map_chr(tbl$rowid, function(user_id) {
          user_table_remove_user_ui(
            id = ns("user_table_remove_user"),
            user_id = user_id
          )
        })

        tbl$reset_password <- purrr::map_chr(tbl$rowid, function(user_id) {
          user_table_reset_password_ui(
            id = ns("user_table_reset_password"),
            user_id = user_id
          )
        })

        .values$language_rv()

        tbl <- tbl %>%
          dplyr::select(name, status, change_status, remove, reset_password) %>%
          dplyr::mutate(status = .values$settings$status_dict_chr[status]) %>%
          dplyr::arrange(name)

        tbl <- tbl[, col_names_by_status_r()]

        DT::datatable(
          data = tbl,
          options = list(
            columnDefs = list(
              list(
                className = 'dt-center',
                targets = column_def_targets_r()
              )
            ),
            language = list(
              url = .values$dt_language_r()
            )
          ),
          escape = FALSE,
          colnames = col_display_names_by_status_r()
        )
      })

      col_names_by_status_r <- shiny::reactive({
        if (.values$user$status() == "admin") {
          c("name", "status", "change_status", "remove", "reset_password")
        } else {
          c("name", "status", "remove")
        }
      })

      col_display_names_by_status_r <- shiny::reactive({
        .values$language_rv()

        if (.values$user$status() == "admin") {
          c(
            .values$i18n$t_chr("user_name"),
            .values$i18n$t_chr("status"),
            .values$i18n$t_chr("edit_status"),
            .values$i18n$t_chr("remove"),
            .values$i18n$t_chr("reset_password")
          )
        } else {
          c(
            .values$i18n$t_chr("user_name"),
            .values$i18n$t_chr("status"),
            .values$i18n$t_chr("remove")
          )
        }
      })

      column_def_targets_r <- shiny::reactive({
        if (.values$user$status() == "admin") {
          3:5
        } else {
          3
        }
      })
    }
  )
}
