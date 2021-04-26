settings_password_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = i18n$t("edit_password"),
    shiny::passwordInput(
      inputId = ns("user_password_1"),
      label = i18n$t("new_password")
    ),
    shiny::uiOutput(
      outputId = ns("wrong_password_length"),
      class = "pfa-error"
    ),
    shiny::passwordInput(
      inputId = ns("user_password_2"),
      label = i18n$t("confirm_new_password")
    ),
    shiny::uiOutput(
      outputId = ns("non_matching_passwords"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("change_password")
    )
  )
}

settings_password_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$wrong_password_length <- shiny::renderUI({
        if (password_too_short_r()) {
          return(i18n$t(
            "err_min_chars",
            "${password_with_article}",
            format_number(.values$settings$password$length$min)
          ))
        }

        if (password_too_long_r()) {
          return(i18n$t(
            "err_max_chars",
            "${password_with_article}",
            format_number(.values$settings$password$length$max)
          ))
        }
      })

      output$non_matching_passwords <- shiny::renderUI({
        if (non_matching_passwords_r()) {
          i18n$t("err_non_matching_passwords")
        }
      })

      password_too_short_r <- shiny::reactive({
        nchar(input$user_password_1) < .values$settings$password$length$min
      })

      password_too_long_r <- shiny::reactive({
        nchar(input$user_password_1) > .values$settings$password$length$max
      })

      non_matching_passwords_r <- shiny::reactive({
        input$user_password_1 != input$user_password_2
      })

      error_r <- shiny::reactive({
        password_too_short_r() ||
          password_too_long_r() ||
          non_matching_passwords_r()
      })

      output$change_password <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("change_password"),
              label = i18n$t("edit_password"),
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("change_password"),
            label = i18n$t("edit_password"),
            width = "100%"
          )
        }
      })

      shiny::observeEvent(input$change_password, {
        shiny::updateTextInput(
          session = session,
          inputId = "user_password_1",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          inputId = "user_password_2",
          value = ""
        )

        if (.values$yaml$showcase && .values$user$id() %in% 1:3) {
          shiny::showNotification(
            ui = i18n$t(
              "err_edit_standard_user",
              "${password_with_article}"
            ),
            type = "error",
            duration = 5
          )

          return()
        }

        shiny::showNotification(
          ui = i18n$t("msg_password_change_successful"),
          type = "warning",
          duration = 5
        )

        db_set_password(
          db = .values$db,
          user_id = .values$user$id(),
          password = bcrypt::hashpw(input$user_password_1)
        )
      })
    }
  )
}
