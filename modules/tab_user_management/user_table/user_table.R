user_table_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = "Nutzertabelle",
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

      taken_user_names_rvs <- shiny::reactiveValues(
        remove = character(),
        reset_password = character()
      )

      output$user_table <- DT::renderDataTable({
        .values$update$user()

        tbl <- DB::db_get_table(.values$db, "user")

        tbl$remove <- purrr::map2_chr(
          tbl$name, tbl$status,
          function(user_name, status) {
            if (!user_name %in% taken_user_names_rvs$remove) {
              taken_user_names_rvs$remove <- c(
                taken_user_names_rvs$remove, user_name
              )

              user_table_remove_button_server(
                id = "user_table_remove_button" %_% user_name,
                .values = .values,
                user_name = user_name,
                status = status
              )
            }

            user_table_remove_button_ui(
              id = ns("user_table_remove_button" %_% user_name)
            )
          }
        )

        tbl$reset_password <- purrr::map_chr(tbl$name, function(user_name) {
          if (!user_name %in% taken_user_names_rvs$reset_password) {
            taken_user_names_rvs$reset_password <- c(
              taken_user_names_rvs$reset_password, user_name
            )

            user_table_reset_password_server(
              id = "user_table_reset_password" %_% user_name,
              .values = .values,
              user_name = user_name,
              status = status
            )
          }

          user_table_reset_password_ui(
            id = ns("user_table_reset_password" %_% user_name)
          )
        })

        tbl <- tbl %>%
          dplyr::select(name, status, remove, reset_password) %>%
          dplyr::mutate(status = .values$settings$status_dict[status])

        tbl <- tbl[rev(seq_len(nrow(tbl))),]

        DT::datatable(
          data = tbl,
          options = list(
            language = list(
              url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'
            ),
            pageLength = 6
          ),
          escape = FALSE,
          colnames = c("Benutzername", "Status", "", "")
        )
      })
    }
  )
}
