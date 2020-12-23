type_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      # shinydashboard::box(
      #   width = NULL,
      #   status = "primary",
      #   title = "Sensorinformationen",
      #   solidHeader = TRUE,
      #   shiny::textInput(
      #     inputId = ns("type"),
      #     label = "Typ",
      #     placeholder = "S-Klasse"
      #   ),
      #   shiny::textInput(
      #     inputId = ns("subtype"),
      #     label = "Untertyp",
      #     placeholder = "W220"
      #   )
      # ),
      # shinydashboard::box(
      #   width = NULL,
      #   status = "success",
      #   title = "QR-Code generieren",
      #   solidHeader = TRUE,
      #   actionButton(
      #     inputId = ns("generate"),
      #     label = "Generieren",
      #     width = "100%"
      #   ),
      #   shiny::plotOutput(
      #     outputId = ns("code")
      #   )
      # )
      add_object_ui(
        id = ns("add_type"),
        title = "Typ hinzufügen",
        label = "Typname",
        placeholder = "PT 100"
      )
    ),
    shiny::column(
      width = 6,
      object_table_ui(
        id = ns("type_table"),
        title = "Typtabelle"
      )
    )
  )
}

type_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      link_r <- eventReactive(input$generate, {
        type <- input$type
        subtype <- input$subtype
        paste0("www.google.de?", type, "d", subtype)
      })

      output$code <- shiny::renderPlot({
        qrcode_gen(link_r())
      })

      add_object_server(
        id = "add_type",
        .values = .values,
        object_id = "type",
        object_name = "type_name",
        object_with_article = "Der Typ",
        add_label = "Typ hinzufügen",
        add_object_func = DB::db_add_type,
        has_object_name_func = DB::db_has_type_name
      )

      db <- list(
        table = "type",
        id_column = "type_id",
        name_column = "type_name",
        func = list(
          get_objects = DB::db_get_types,
          has_object_name = DB::db_has_type_name,
          set_object_name = DB::db_set_type_name,
          remove_object = DB::db_remove_type
        )
      )

      label <- list(
        colnames = c("Typname", "Typname ändern", "Entfernen"),
        change_name = "Typ ändern",
        new_name = "Neuer Typname",
        object_name_with_article = "Der Typname",
        object_with_article = "Der Typ",
        object_with_small_article = "der Typ",
        remove_btn_title = "Typ entfernen"
      )

      object_table_server(
        id = "type_table",
        .values = .values,
        .values_type = "type",
        .values_settings = "type_name",
        db = db,
        label = label
      )
    }
  )
}
