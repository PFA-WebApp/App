add_user_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    status = "primary",
    title = i18n$t("add_user"),
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    shiny::textInput(
      inputId = ns("user_name"),
      label = i18n$t("user_name"),
      placeholder = "Max Mustermann"
    ),
    shiny::uiOutput(
      outputId = ns("wrong_name_length"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("user_name_taken"),
      class = "pfa-error"
    ),
    shiny::passwordInput(
      inputId = ns("user_password_1"),
      label = i18n$t("password")
    ),
    shiny::uiOutput(
      outputId = ns("wrong_password_length"),
      class = "pfa-error"
    ),
    shiny::passwordInput(
      inputId = ns("user_password_2"),
      label = i18n$t("confirm_password")
    ),
    shiny::uiOutput(
      outputId = ns("non_matching_passwords"),
      class = "pfa-error"
    ),
    shiny::uiOutput(
      outputId = ns("status")
    ),
    shiny::uiOutput(
      outputId = ns("add_user")
    )
  )
}

add_user_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns



      choices_r <- shiny::reactive({
        if (.values$user$status() == "admin") {
          c(Administrator = "admin", Moderator = "mod", Benutzer = "user")
        } else {
          c(Benutzer = "user")
        }
      })

      output$status <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("user_status"),
          label = .values$i18n$t("status"),
          choices = choices_r(),
          selectize = .values$device$large
        )
      })

      output$wrong_name_length <- shiny::renderUI({
        if (user_name_too_short_r()) {
          return(.values$i18n$t(
            "err_min_chars",
            "${user_name_with_article}",
            format_number(.values$settings$user_name$length$min)
          ))
        }

        if (user_name_too_long_r()) {
          return(.values$i18n$t(
            "err_max_chars",
            "${user_name_with_article}",
            format_number(.values$settings$user_name$length$max)
          ))
        }
      })

      output$user_name_taken <- shiny::renderUI({
        if (user_name_taken_r()) {
          .values$i18n$t(
            "err_name_taken",
            "${user_name_with_article}"
          )
        }
      })

      output$wrong_password_length <- shiny::renderUI({
        if (password_too_short_r()) {
          return(.values$i18n$t(
            "err_min_chars",
            "${password_with_article}",
            format_number(.values$settings$password$length$min)
          ))
        }

        if (password_too_long_r()) {
          return(.values$i18n$t(
            "err_max_chars",
            "${password_with_article}",
            format_number(.values$settings$password$length$max)
          ))
        }
      })

      output$non_matching_passwords <- shiny::renderUI({
        if (non_matching_passwords_r()) {
          .values$i18n$t("err_non_matching_passwords")
        }
      })

      output$add_user <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("add_user"),
              label = .values$i18n$t("add_user"),
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("add_user"),
            label = .values$i18n$t("add_user"),
            width = "100%"
          )
        }
      })

      error_r <- shiny::reactive({
        user_name_too_short_r() ||
          user_name_too_long_r() ||
          user_name_taken_r() ||
          password_too_short_r() ||
          password_too_long_r() ||
          non_matching_passwords_r()
      })

      user_name_too_short_r <- shiny::reactive({
        nchar(input$user_name) < .values$settings$user_name$length$min
      })

      user_name_too_long_r <- shiny::reactive({
        nchar(input$user_name) > .values$settings$user_name$length$max
      })

      user_name_taken_r <- shiny::reactive({
        db_has_user_name(.values$db, input$user_name)
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

      shiny::observeEvent(input$add_user, {
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

        shiny::updateTextInput(
          session = session,
          inputId = "user_name",
          value = ""
        )

        success <- db_add_user(
          db = .values$db,
          name = input$user_name,
          status = input$user_status,
          password = bcrypt::hashpw(input$user_password_1),
          added_from = .values$user$id()
        )

        if (success) {
          shiny::showNotification(
            ui = .values$i18n$t(
              "msg_add_user_successful",
              input$user_name
            ),
            duration = 5,
            type = "warning"
          )
        } else {
          shiny::showNotification(
            ui = .values$i18n$t("err_add_user_not_successful"),
            duration = 5,
            type = "error"
          )
        }

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
