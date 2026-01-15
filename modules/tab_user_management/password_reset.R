# Currently not used

password_reset_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    status = "primary",
    title = i18n$t("reset_password"),
    solidHeader = TRUE,
    shiny::uiOutput(
      outputId = ns("user_name")
    ),
    shiny::actionButton(
      inputId = ns("start_reset"),
      label = i18n$t("reset"),
      width = "100%"
    )
  )
}

password_reset_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_choices_r <- shiny::reactive({
        .values$update$user()

        db_get_users(.values$db)
      })

      output$user_name <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("user_id"),
          label = .values$i18n$t("user_name"),
          choices = user_choices_r(),
          selectize = .values$device$large
        )
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, input$user_id)
      })

      shiny::observeEvent(input$start_reset, {
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
          duration = NULL
        )

        db_set_password(.values$db, input$user_id, bcrypt::hashpw(reset_pwd))
      })

    }
  )
}
