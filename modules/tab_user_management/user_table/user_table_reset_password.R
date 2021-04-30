user_table_reset_password_ui <- function(id, user_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("reset_password"),
      label = NULL,
      icon = shiny::icon("eraser"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          user_id: {user_id},
          nonce: Math.random()
        }});',
        inputId = ns("reset_password"),
        user_id = user_id
      )
    )
  )
}

user_table_reset_password_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_id_r <- shiny::reactive({
        shiny::req(input$reset_password)$user_id
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, user_id_r())
      })

      shiny::observeEvent(user_id_r(), {
        if (.values$yaml$showcase && user_id_r() %in% 1:3) {
          shiny::showModal(shiny::modalDialog(
            title = htmltools::tagList(
              .values$i18n$t("err_access_denied"),
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            easyClose = TRUE,
            htmltools::p(
              .values$i18n$t("err_reset_password_standard_user")
            ),
            footer = NULL
          ))

          return()
        }

        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            .values$i18n$t("reset_password"),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          htmltools::p(
            .values$i18n$t(
              "msg_confirm_reset_password",
              user_name_r()
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_reset"),
            label = .values$i18n$t("confirm")
          )
        ))
      })

      shiny::observeEvent(input$confirm_reset, {
        shiny::removeModal()

        reset_pwd <- "1234"

        shiny::showNotification(
          ui = .values$i18n$t(
            "msg_reset_password_successful",
            user_name_r(),
            reset_pwd
          ),
          type = "warning",
          duration = 5
        )

        db_set_password(.values$db, user_id_r(), bcrypt::hashpw(reset_pwd))
      })
    }
  )
}
