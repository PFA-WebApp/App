login_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        status = "primary",
        title = "Anmeldung",
        solidHeader = TRUE,
        shiny::uiOutput(
          outputId = ns("login")
        )
      )
    ),
    shiny::column(
      width = 6,
      login_user_info_ui(
        id = ns("login_user_info")
      )
    ),
    shiny::column(
      width = 6,
      # Only included in showcase mode
      shiny::uiOutput(
        outputId = ns("password_tbl_container")
      )
    )
  )
}

login_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$login <- shiny::renderUI({
        if (.values$user$status() == "not_logged") {
          login_r()
        } else {
          logout_r()
        }
      })

      login_r <- shiny::reactive({
        name_input <- if (.values$yaml$showcase) {
          shiny::selectInput(
            inputId = ns("user_name"),
            label = "Benutzername",
            choices = user_name_choices_r()
          )
        } else {
          shiny::textInput(
            inputId = ns("user_name"),
            label = "Benutzername"
          )
        }

        htmltools::tagList(
          name_input,
          shiny::passwordInput(
            inputId = ns("user_password"),
            label = "Passwort",
            placeholder = "Passwort"
          ),
          shiny::actionButton(
            inputId = ns("user_login"),
            label = "Anmelden",
            width = "100%"
          )
        )
      })

      shiny::observeEvent(input$user_login, {
        user_id <- db_get_user_id(.values$db, input$user_name)

        user_pwd <- db_get_password(
          db = .values$db,
          user_id = user_id
        )

        pwd_correct <- bcrypt::checkpw(input$user_password, user_pwd)

        if (pwd_correct) {
          .values$user$id(user_id)
          .values$user$name(input$user_name)
          .values$user$status(db_get_user_status(.values$db, .values$user$id()))
          .values$user$last_logged(
            db_get_user_last_logged(.values$db, .values$user$id())
          )
          db_log_user_in(.values$db, .values$user$id())
          .values$update$user(.values$update$user() + 1)

          type <- suppressWarnings(as.numeric(shiny::getQueryString()$type))
          if (length(type) && is.numeric(type) && !is.na(type)) {
            .values$query$type(type)
          }

          js$setCookie(
            cookie = "user",
            value = db_get_hash(.values$db, user_id),
            id = ns("cookie_user_hash")
          )

          shiny::showNotification(
            ui = "Du hast Dich erfolgreich angemeldet.",
            type = "default",
            duration = 3
          )
        } else {
          shiny::showNotification(
            ui = "Falsches Passwort! Bitte versuche es erneut.",
            type = "error",
            duration = 3
          )
        }

        shiny::updateTextInput(
          session = session,
          inputId = "user_password",
          value = ""
        )
      })

      logout_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("user_logout"),
          label = "Abmelden",
          width = "100%"
        )
      })

      shiny::observeEvent(input$user_logout, {
        db_log_user_out(.values$db, .values$user$id())

        .values$user$id(0L)
        .values$user$status("not_logged")
        .values$user$name("")
        .values$user$last_logged("")

        js$rmCookie(
          cookie = "user",
          id = ns("cookie_user_hash")
        )

        shiny::showNotification(
          ui = "Du hast Dich erfolgreich abgemeldet. Bis zum nächsten Mal.",
          type = "default",
          duration = 3
        )
      })

      user_name_choices_r <- shiny::reactive({
        .values$update$user()

        names(db_get_users(.values$db))
      })

      login_user_info_server(
        id = "login_user_info",
        .values = .values
      )

      shiny::observeEvent(TRUE, {
        js$getCookie(
          cookie = "user",
          id = ns("cookie_user_hash")
        )
      }, once = TRUE)

      shiny::observeEvent(input$cookie_user_hash, {
        if (is.null(input$cookie_user_hash)) return()

        user_id <- db_get_user_id_by_hash(.values$db, input$cookie_user_hash)
        # hash exists but is invalid (for example after reinitialisiton of database)
        if (!length(user_id)) return()

        if (.values$user$id() != user_id) {
          .values$user$id(user_id)
          user_name <- db_get_user_name(.values$db, user_id)
          .values$user$name(user_name)
          .values$user$status(db_get_user_status(.values$db, user_id))
          .values$user$last_logged(
            db_get_user_last_logged(.values$db, .values$user$id())
          )

          type <- suppressWarnings(as.numeric(shiny::getQueryString()$type))
          if (length(type) && is.numeric(type) && !is.na(type)) {
            .values$query$type(type)
          }
        }
      })

      output$password_tbl_container <- shiny::renderUI({
        if (.values$yaml$showcase) {
          bs4Dash::box(
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            title = "Passwörter",
            htmltools::p(
              "Diese Tabelle ist nur in der Testversion sichtbar"
            ),
            DT::dataTableOutput(outputId = ns("password_tbl"))
          )

        }
      })

      output$password_tbl <- DT::renderDataTable({
        password_tbl <- tibble::tibble(
          Name = c("Armin Admin", "Modesta Moderator", "Bernd Benutzer"),
          Passwort = c("admin", "mod", "user")
        )


        DT::datatable(password_tbl)
      })
    }
  )
}
