user_table_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = "Nutzertabelle",
    solidHeader = TRUE,
    DT::dataTableOutput(
      outputId = ns("user_table")
    )
  )
}

user_table_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      taken_user_names_rvs <- shiny::reactiveValues(
        remove = character(),
        reset_password = character()
      )

      output$user_table <- DT::renderDataTable({
        .values$update$user()

        tbl <- DB::db_get_table(.values$db, "user")

        tbl$remove <- purrr::map2_chr(
          tbl$name, tbl$status,
          function(user_name, status) {
            if (!user_name %in% taken_user_names_rvs$remove) {
              taken_user_names_rvs$remove <- c(
                taken_user_names_rvs$remove, user_name
              )

              shiny::observeEvent(input[["remove" %_% user_name]], {
                if (.values$user$status() == "mod" && status != "user") {
                  shiny::showModal(shiny::modalDialog(
                    easyClose = TRUE,
                    title = "Zugriff verweigert!",
                    htmltools::div(
                      "Moderatoren dürfen nur Benutzer löschen."
                    ),
                    footer = shiny::modalButton(
                      label = NULL,
                      icon = shiny::icon("window-close")
                    )
                  ))

                  return()
                }

                shiny::showModal(shiny::modalDialog(
                  easyClose = TRUE,
                  title = "Benutzer löschen",
                  htmltools::div(
                    paste0(
                      "Bist du sicher, dass du den Benutzer \"",
                      user_name,
                      "\" löschen möchtest?"
                    )
                  ),
                  footer = shiny::actionButton(
                    inputId = ns("confirm_remove" %_% user_name),
                    label = "Ja"
                  )
                ))
              })

              shiny::observeEvent(input[["confirm_remove" %_% user_name]], {
                shiny::removeModal()

                shiny::showNotification(
                  ui = paste0(
                    "Der Benutzer \"",
                    user_name,
                    "\" wurde erfolgreich gelöscht."
                  ),
                  type = "warning",
                  duration = NULL
                )

                DB::db_remove_user(.values$db, user_name)

                .values$update$user(.values$update$user() + 1)
              })
            }

            as.character(
              shiny::actionButton(
                inputId = ns("remove" %_% user_name),
                label = NULL,
                icon = shiny::icon("user-alt-slash"),
                class = "primary",
                onclick = glue::glue(
                  'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
                  inputId = ns("remove" %_% user_name)
                )
              )
            )
          }
        )

        tbl$reset_password <- purrr::map_chr(tbl$name, function(user_name) {
          if (!user_name %in% taken_user_names_rvs$reset_password) {
            taken_user_names_rvs$reset_password <- c(
              taken_user_names_rvs$reset_password, user_name
            )

            shiny::observeEvent(input[["reset_password" %_% user_name]], {
              shiny::showModal(shiny::modalDialog(
                title = "Passwort zurücksetzen",
                easyClose = TRUE,
                htmltools::div(
                  paste0(
                    "Bist Du sicher, dass Du das Passwort für \"",
                    user_name,
                    "\" zurücksetzen möchtest?"
                  )
                ),
                footer = shiny::actionButton(
                  inputId = ns("confirm_reset" %_% user_name),
                  label = "Ja"
                )
              ))
            })

            shiny::observeEvent(input[["confirm_reset" %_% user_name]], {
              shiny::removeModal()

              reset_pwd <- "1234"

              shiny::showNotification(
                ui = paste0(
                  "Das Passwort für \"",
                  user_name,
                  "\" wurde erfolgreich auf \"",
                  reset_pwd,
                  "\" zurückgesetzt."
                ),
                type = "warning",
                duration = 5
              )

              DB::db_set_password(.values$db, user_name, bcrypt::hashpw(reset_pwd))
            })
          }

          as.character(
            shiny::actionButton(
              inputId = ns("reset_password" %_% user_name),
              label = "Passwort",
              icon = shiny::icon("eraser"),
              class = "primary",
              onclick = glue::glue(
                'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
                inputId = ns("reset_password" %_% user_name)
              )
            )
          )
        })

        tbl <- tbl %>%
          dplyr::select(name, status, remove, reset_password) %>%
          dplyr::mutate(status = .values$settings$status_mapper[status])

        DT::datatable(
          data = tbl,
          options = list(
            language = list(
              url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'
            ),
            pageLength = 6
          ),
          escape = FALSE,
          colnames = c("Benutzername", "Status", "", "")
        )
      })
    }
  )
}
