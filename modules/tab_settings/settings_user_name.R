settings_user_name_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = i18n$t("edit_user_name"),
    shiny::uiOutput(
      outputId = ns("user_name")
    ),
    shiny::uiOutput(
      outputId = ns("wrong_user_name_length"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("user_name_taken"),
      class = "pfa-error"
    ),
    shiny::passwordInput(
      inputId = ns("password"),
      label = i18n$t("current_password")
    ),
    shiny::uiOutput(
      outputId = ns("change_user_name")
    )
  )
}

settings_user_name_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$user_name <- shiny::renderUI({
        shiny::textInput(
          inputId = ns("user_name"),
          label = i18n$t("new_user_name"),
          value = .values$user$name()
        )
      })

      output$wrong_user_name_length <- shiny::renderUI({
        if (user_name_too_short_r()) {
          return(i18n$t(
            "err_min_chars",
            "${user_name_with_article}",
            format_number(.values$settings$user_name$length$min)
          ))
        }

        if (user_name_too_long_r()) {
          return(i18n$t(
            "err_max_chars",
            "${user_name_with_article}",
            format_number(.values$settings$user_name$length$max)
          ))
        }
      })

      output$user_name_taken <- shiny::renderUI({
        if (user_name_taken_r()) {
          i18n$t(
            "err_name_taken",
            "${user_name_with_article}"
          )
        }
      })

      user_name_taken_r <- shiny::reactive({
        shiny::req(!is.null(input$user_name))
        if (input$user_name == .values$user$name()) return(FALSE)
        db_has_user_name(.values$db, input$user_name)
      })

      user_name_too_short_r <- shiny::reactive({
        nchar(input$user_name) < .values$settings$user_name$length$min
      })

      user_name_too_long_r <- shiny::reactive({
        nchar(input$user_name) > .values$settings$user_name$length$max
      })

      error_r <- shiny::reactive({
        user_name_taken_r() ||
          user_name_too_short_r() ||
          user_name_too_long_r()
      })

      output$change_user_name <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("change_user_name"),
              label = i18n$t("edit_user_name"),
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("change_user_name"),
            label = i18n$t("edit_user_name"),
            width = "100%"
          )
        }
      })

      shiny::observeEvent(input$change_user_name, {
        shiny::updateTextInput(
          inputId = "password",
          value = ""
        )

        if (.values$yaml$showcase && .values$user$id() %in% 1:3) {
          shiny::showNotification(
            ui = i18n$t(
              "err_edit_standard_user",
              "${user_name_with_article}"
            ),
            type = "error",
            duration = 3
          )

          return()
        }

        user_pwd <- db_get_password(
          db = .values$db,
          user_id = .values$user$id()
        )

        pwd_correct <- bcrypt::checkpw(input$password, user_pwd)

        if (!pwd_correct) {
          shiny::showNotification(
            ui = i18n$t("err_wrong_password"),
            type = "error",
            duration = 3
          )
        }

        if (pwd_correct) {
          success <- db_set_user_name(.values$db, .values$user$id(), input$user_name)

          if (success) {
            shiny::showNotification(
              ui = i18n$t(
                "msg_edit_user_name_successful",
                input$user_name
              ),
              duration = 5,
              type = "warning"
            )
          } else {
            shiny::showNotification(
              ui = i18n$t("err_edit_user_name_not_successful"),
              duration = 5,
              type = "error"
            )
          }

          .values$user$name(input$user_name)
          .values$update$user(.values$update$user() + 1)
        }
      })
    }
  )
}
