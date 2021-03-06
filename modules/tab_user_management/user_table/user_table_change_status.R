user_table_change_status_ui <- function(id, user_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("change_status"),
      label = NULL,
      icon = shiny::icon("edit"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          user_id: {user_id},
          nonce: Math.random()
        }});',
        inputId = ns("change_status"),
        user_id = user_id
      )
    )
  )
}

user_table_change_status_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_id_r <- shiny::reactive({
        shiny::req(input$change_status)$user_id
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, user_id_r())
      })

      status_r <- shiny::reactive({
        db_get_user_status(.values$db, user_id_r())
      })

      shiny::observeEvent(user_id_r(), {
        if (.values$yaml$showcase && user_id_r() %in% 1:3) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = htmltools::tagList(
              .values$i18n$t("err_access_denied"),
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            htmltools::p(
              .values$i18n$t(
                "err_edit_standard_user",
                "${status_with_article}"
              )
            ),
            footer = NULL
          ))

          return()
        }


        if (user_name_r() == .values$user$name()) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = htmltools::tagList(
              .values$i18n$t("err_access_denied"),
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            htmltools::p(
              .values$i18n$t("err_edit_own_status_admin")
            ),
            footer = NULL
          ))

          return()
        }

        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            .values$i18n$t("edit_status"),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          shiny::selectInput(
            inputId = ns("user_status"),
            label = .values$i18n$t("edit_status"),
            choices = c(
              Administrator = "admin",
              Moderator = "mod",
              Benutzer = "user"
            ),
            selected = status_r(),
            selectize = .values$device$large
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_status"),
            label = .values$i18n$t("confirm")
          )
        ))
      })

      shiny::observeEvent(input$confirm_status, {
        shiny::removeModal()

        success <- db_set_user_status(.values$db, user_id_r(), input$user_status)

        if (success) {
          shiny::showNotification(
            ui = .values$i18n$t(
              "msg_edit_status_successful",
              user_name_r(),
              .values$settings$status_dict[input$user_status]
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = .values$i18n$t(
              "err_edit_status_not_successful",
              user_name_r()
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
