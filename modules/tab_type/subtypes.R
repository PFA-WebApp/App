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
          choices = db_get_types(db)
        )
      })

      taken_object_types_rvs <- shiny::reactiveValues(
        remove = character()
      )

      output$subtype_table <- DT::renderDataTable({
        .values$update$subtype()

        tbl <- db_get_subtype_table_by_type_id(db, input$type)

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
                    remove_objects = db_remove_subtype
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

        DT::datatable(
          tbl,
          escape = FALSE
        )
      })
    }
  )
}
