subtypes_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Untertypen bearbeiten",
    shiny::uiOutput(
      outputId = ns("select_type")
    ),
    DT::dataTableOutput(
      outputId = ns("subtype_table")
    ),
    htmltools::br(),
    shiny::actionButton(
      inputId = ns("add_subtype"),
      label = "Untertyp hinzufügen",
      icon = shiny::icon("plus"),
      width = "100%"
    )
  )
}

subtypes_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$select_type <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("type"),
          label = "Typ",
          choices = db_get_types(.values$db)
        )
      })

      taken_object_types_rvs <- shiny::reactiveValues(
        name = character(),
        remove = character()
      )

      output$subtype_table <- DT::renderDataTable({
        .values$update$subtype()

        tbl <- db_get_subtype_table_by_type_id(.values$db, input$type)

        tbl$name <-purrr::map_chr(
          tbl$subtype_id,
          function(object_id) {
            if (!object_id %in% taken_object_types_rvs$name) {
              taken_object_types_rvs$name <- c(
                taken_object_types_rvs$name, object_id
              )

              object_table_name_server(
                id = "object_table_name" %_% object_id,
                .values = .values,
                object_id = object_id,
                settings = list(
                  update_name = "subtype",
                  length_name = "subtype_name"
                ),
                db = list(
                  func = list(
                    get_objects = db_get_subtypes,
                    has_object_name = db_has_subtype_name,
                    set_object_name = db_set_subtype_name
                  )
                ),
                label = list(
                  change_name = "Untertypenname bearbeiten",
                  new_name = "Neuer Untertypenname",
                  object_name_with_article = "Der Untertypenname"
                )
              )
            }

            object_table_name_ui(
              id = ns("object_table_name" %_% object_id),
              name = db_get_subtype_name(.values$db, object_id)
            )
          }
        )

        tbl$remove <- purrr::map_chr(
          tbl$subtype_id,
          function(object_id) {
            if (!object_id %in% taken_object_types_rvs$remove) {
              taken_object_types_rvs$remove <- c(
                taken_object_types_rvs$remove, object_id
              )

              object_table_remove_object_server(
                id = "object_table_remove_object" %_% object_id,
                .values = .values,
                object_id = object_id,
                settings = list(
                  update_name = "subtype"
                ),
                db = list(
                  func = list(
                    get_objects = db_get_subtypes,
                    has_object_id = db_has_subtype_id,
                    remove_objects = db_remove_subtype,
                    remove_allowed = remove_subtype_allowed
                  )
                ),
                label = list(
                  remove_btn_title = "Untertyp entfernen",
                  object_with_small_article = "den Untertypen",
                  object_with_article = "Der Untertyp"
                )
              )
            }

            object_table_remove_object_ui(
              id = ns("object_table_remove_object" %_% object_id)
            )
          }
        )

        tbl <- tbl %>%
          dplyr::select(
            Untertyp = name,
            Menge = quantity,
            Entfernen = remove
          )

        DT::datatable(
          tbl,
          options = list(
            columnDefs = list(
              list(
                className = 'dt-center',
                targets = 3
              )
            )
          ),
          escape = FALSE
        )
      })

      shiny::observeEvent(input$add_subtype, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          add_object_ui(
            id = ns("add_object"),
            title = "Untertyp hinzufügen",
            label = "Untertyp",
            placeholder = NULL,
            collapsible = FALSE
          ),
          footer = shiny::modalButton(
            label = "Abbrechen"
          )
        ))
      })

      add_object_server(
        id = "add_object",
        .values = .values,
        object_id = "subtype",
        object_name = "subtype_name",
        object_name_with_article = "Der Untertypenname",
        object_with_article =  "Der Untertyp",
        add_label = "Untertyp hinzufügen",
        add_object_func = db_add_subtype,
        add_object_func_args_r = shiny::reactive({
          list(
            type_id = input$type,
            quantity = 0
          )
        }),
        has_object_name_func = db_has_subtype_name
      )
    }
  )
}
