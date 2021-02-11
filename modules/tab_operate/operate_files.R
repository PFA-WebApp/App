operate_files_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabBox(
    id = ns("file_tabs"),
    width = NULL,
    title = "Dateien",
    shiny::tabPanel(
      title = shiny::uiOutput(
        outputId = ns("group_title"),
        inline = TRUE
      ),
      icon = shiny::icon("layer-group"),
      operate_file_manager_ui(
        id = ns("operate_file_manager_group")
      )
    ),
    shiny::tabPanel(
      title = shiny::uiOutput(
        outputId = ns("type_title"),
        inline = TRUE
      ),
      icon = shiny::icon("tags"),
      operate_file_manager_ui(
        id = ns("operate_file_manager_type")
      )
    ),
    shiny::tabPanel(
      title = shiny::uiOutput(
        outputId = ns("subtype_title"),
        inline = TRUE
      ),
      icon = shiny::icon("tag"),
      operate_file_manager_ui(
        id = ns("operate_file_manager_subtype")
      )
    )
  )
}

operate_files_server <- function(id,
                                 .values,
                                 type_id_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      group_ids_r <- shiny::reactive({
        .values$update$group_type()
        db_get_groups_by_type(.values$db, type_id_r())
      })

      subtype_ids_r <- shiny::reactive({
        .values$update$subtype()
        db_get_subtypes_by_type_id(.values$db, type_id_r())
      })

      output$group_title <- shiny::renderUI({
        format_file_title(
          "Gruppen",
          length(group_files_return$files_r())
        )
      })

      output$type_title <- shiny::renderUI({
        format_file_title(
          "Typ",
          length(type_files_return$files_r())
        )
      })

      output$subtype_title <- shiny::renderUI({
        format_file_title(
          "Untertypen",
          length(subtype_files_return$files_r())
        )
      })

      group_files_return <- operate_file_manager_server(
        id = "operate_file_manager_group",
        .values = .values,
        db = list(
          get_object_name = db_get_group_name
        ),
        settings = list(table_name = "group"),
        label = list(
          object_name = "Gruppe"
        ),
        object_ids_r = group_ids_r
      )

      type_files_return <- operate_file_manager_server(
        id = "operate_file_manager_type",
        .values = .values,
        db = list(
          get_object_name = db_get_type_name
        ),
        settings = list(table_name = "type"),
        label = list(
          object_name = "Typ"
        ),
        object_ids_r = type_id_r
      )

      subtype_files_return <- operate_file_manager_server(
        id = "operate_file_manager_subtype",
        .values = .values,
        db = list(
          get_object_name = db_get_subtype_name
        ),
        settings = list(table_name = "subtype"),
        label = list(
          object_name = "Untertyp"
        ),
        object_ids_r = subtype_ids_r
      )
    }
  )
}

format_file_title <- function(name, n) {
  glue::glue("{name} ({n})", name = name, n = n)
}
