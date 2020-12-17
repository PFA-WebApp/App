login_user_info_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("user_name")
    ),
    shiny::uiOutput(
      outputId = ns("user_last_logged")
    )
  )
}

login_user_info_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      is_logged_r <- shiny::reactive({
        .values$user$status() != "not_logged"
      })

      output$user_name <- shiny::renderUI({
        if (is_logged_r()) {
          shinydashboard::infoBox(
            title = .values$settings$status_dict[.values$user$status()],
            value = .values$user$name(),
            icon = shiny::icon("users"),
            color = "light-blue",
            width = NULL
          )
        }
      })

      diff_time_r <- shiny::reactive({
        .values$update$user()
        input$refresh_last_logged


        last_logged_time <- DB::db_get_user_last_logged(
          db = .values$db,
          name = .values$user$name()
        )

        last_logged_time <- lubridate::ymd_hms(last_logged_time)
        current_time <- lubridate::ymd_hms(Sys.time())

        diff_time <- current_time - last_logged_time
      })

      last_logged_r <- shiny::reactive({
        unit <- .values$settings$time_unit_dict[attr(diff_time_r(), "units")]
        value <- round(as.numeric(diff_time_r()))
        paste(value, unit)
      })

      output$user_last_logged <- shiny::renderUI({
        if (is_logged_r()) {
          shinydashboard::infoBox(
            title = "Eingeloggt seit",
            value = htmltools::div(
              class = "relative",
              last_logged_r(),
              shiny::actionButton(
                inputId = ns("refresh_last_logged"),
                label = NULL,
                icon = shiny::icon("redo-alt"),
                class = "refresh-user-last-logged-btn"
              )
            ),
            icon = shiny::icon("user-clock"),
            color = "light-blue",
            width = NULL
          )
        }
      })
    }
  )
}
