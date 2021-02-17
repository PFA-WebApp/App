operate_groups_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Gruppen des ausgewählten Typs",
    htmltools::p(
      "Klicke auf eine Gruppe, um weitere Typen dieser Gruppe anzuzeigen."
    ),
    shiny::uiOutput(
      outputId = ns("groups"),
      container = htmltools::tags$ul,
      class = "st-badges flex bordered"
    )
  )
}

operate_groups_server <- function(id, .values, type_id_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      groups_r <- shiny::reactive({
        .values$update$group_type()
        db_get_groups_by_type(.values$db, type_id_r())
      })

      clicked_group_index_rv <- shiny::reactiveVal(NULL)

      group_id_r <- shiny::reactive({
        groups_r()[shiny::req(clicked_group_index_rv())]
      })

      index_env <- new.env()
      index_env$indices <- numeric()

      output$groups <- shiny::renderUI({
        if (length(groups_r())) {
          purrr::map2(
            seq_along(groups_r()), names(groups_r()),
            function(index, group_name) {
              if (!index %in% index_env$indices) {
                index_env$indices <- c(index_env$indices, index)

                shiny::observeEvent(input[["group" %_% index]], {
                  clicked_group_index_rv(index)

                  shiny::showModal(shiny::modalDialog(
                    title = htmltools::tagList(
                      "Enthaltene Typen",
                      shiny::modalButton(
                        label = NULL,
                        icon = shiny::icon("window-close")
                      )
                    ),
                    easyClose = TRUE,
                    htmltools::p(
                      "Klicke auf einen Typen, um die Ausleihe bzw. die Rückgabe
                    vorzubereiten."
                    ),
                    htmltools::br(),
                    operate_group_types_ui(
                      id = ns("operate_group_types")
                    ),
                    footer = NULL
                  ))
                })
              }

              htmltools::tagList(
                htmltools::tags$li(
                  class = "st-badge bordered bg-lightgray",
                  shiny::actionLink(
                    inputId = ns("group" %_% index),
                    label = group_name
                  )
                )
              )
            }
          )
        } else {
          htmltools::tags$li(
            class = "st-badge bordered bg-lightgray",
            "Keine Gruppe vorhanden"
          )
        }
      })

      operate_group_types_return <- operate_group_types_server(
        id = "operate_group_types",
        .values = .values,
        group_id_r = group_id_r
      )

      return_list <- list(
        type_id_r = operate_group_types_return$type_id_r
      )
    }
  )
}
