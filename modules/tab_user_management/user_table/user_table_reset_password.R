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
              "Zugriff verweigert!",
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            easyClose = TRUE,
            htmltools::div(
              "Das Passwort der Standardnutzer kann in der Testversion nicht
              zurückgesetzt werden."
            ),
            footer = NULL
          ))

          return()
        }

        shiny::showModal(shiny::modalDialog(
          title = htmltools::tagList(
            "Passwort zurücksetzen",
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          easyClose = TRUE,
          htmltools::div(
            paste0(
              "Bist Du sicher, dass Du das Passwort für \"",
              user_name_r(),
              "\" zurücksetzen möchtest?"
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_reset"),
            label = "Ja"
          )
        ))
      })

      shiny::observeEvent(input$confirm_reset, {
        shiny::removeModal()

        reset_pwd <- "1234"

        shiny::showNotification(
          ui = paste0(
            "Das Passwort für \"",
            user_name_r(),
            "\" wurde erfolgreich auf \"",
            reset_pwd,
            "\" zurückgesetzt."
          ),
          type = "warning",
          duration = 5
        )

        db_set_password(.values$db, user_id_r(), bcrypt::hashpw(reset_pwd))
      })
    }
  )
}
