user_table_remove_user_ui <- function(id, user_id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("remove"),
      label = NULL,
      icon = shiny::icon("user-minus"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", {{
          user_id: {user_id},
          nonce: Math.random()
        }});',
        inputId = ns("remove"),
        user_id = user_id
      )
    )
  )
}

user_table_remove_user_server <- function(id,
                                          .values
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      user_id_r <- shiny::reactive({
        shiny::req(input$remove)$user_id
      })

      user_name_r <- shiny::reactive({
        db_get_user_name(.values$db, user_id_r())
      })

      added_from_r <- shiny::reactive({
        user_id <- db_get_adding_user(.values$db, user_id_r())
      })

      added_from_name_r <- shiny::reactive({
        .values$update$user()
        user_id <- added_from_r()
        db_get_user_name(.values$db, user_id)
      })

      shiny::observeEvent(user_id_r(), {
        # In showcase mode default users must not be removed
        if (.values$yaml$showcase && user_id_r() %in% 1:3) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = htmltools::tagList(
              i18n$t("err_access_denied"),
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            htmltools::p(
              i18n$t("err_remove_standard_user")
            ),
            footer = NULL
          ))

          return()
        }

        status <- .values$user$status()

        # Check that moderators can only remove users they added themselves
        if (
          status == "mod" && added_from_r() != .values$user$id()
        ) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = htmltools::tagList(
              i18n$t("err_access_denied"),
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            htmltools::p(
              i18n$t(
                "err_mod_remove_user",
                user_name_r(),
                added_from_name_r()
              )
            ),
            footer = NULL
          ))

          return()
        }


        # Check that admins can't remove themselves
        if (user_name_r() == .values$user$name()) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = htmltools::tagList(
              i18n$t("err_access_denied"),
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            htmltools::p(
              i18n$t("err_remove_self_admin")
            ),
            footer = NULL
          ))

          return()
        }

        borrowed <- db_get_total_borrowed_quantity_by_user_id(
          .values$db, user_id_r()
        )
        if (borrowed > 0) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = htmltools::tagList(
              i18n$t("err_access_denied"),
              shiny::modalButton(
                label = NULL,
                icon = shiny::icon("window-close")
              )
            ),
            htmltools::p(
              i18n$t("err_remove_user_borrowed")
            ),
            footer = NULL
          ))

          return()
        }

        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = htmltools::tagList(
            i18n$t("remove_user"),
            shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ),
          htmltools::div(
            i18n$t(
              "msg_confirm_remove_obj",
              "${user_with_small_article}",
              user_name_r()
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_remove"),
            label = i18n$t("confirm")
          )
        ))
      })

      shiny::observeEvent(input$confirm_remove, {
        shiny::removeModal()

        success <- db_remove_user(.values$db, user_id_r())

        if (success) {
          shiny::showNotification(
            ui = i18n$t(
              "msg_remove_successful",
              "${user_with_article} \"${p_[[2]]}\"",
              user_name_r()
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = i18n$t(
              "err_remove_not_successful",
              "${user_with_article} \"${p_[[2]]}\""
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
