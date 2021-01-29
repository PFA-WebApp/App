operate_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 12,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Ausleihen & Zurückgeben",
        shiny::uiOutput(
          outputId = ns("type")
        ),
        shiny::uiOutput(
          outputId = ns("subtype")
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::actionButton(
              inputId = ns("borrow"),
              label = "Ausleihen",
              icon = shiny::icon("sign-out-alt"),
              width = "100%",
              class = "borrow-btn"
            )
          ),
          shiny::column(
            width = 6,
            shiny::actionButton(
              inputId = ns("return"),
              label = "Zurückgeben",
              icon = shiny::icon("sign-in-alt"),
              width = "100%",
              class = "return-btn"
            )
          )
        )
      )
    )
  )
}

operate_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      types_r <- shiny::reactive({
        .values$update$type()
        db_get_types(.values$db)
      })

      output$type <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("type"),
          label = "Typ",
          choices = types_r()
        )
      })

      subtypes_r <- shiny::reactive({
        .values$update$subtype()
        db_get_subtypes_by_type_id(.values$db, shiny::req(input$type))
      })

      output$subtype <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("subtype"),
          label = "Untertyp",
          choices = subtypes_r()
        )
      })

      object_quantity_input_server(
        id = "quantity",
        .values = .values
      )
    }
  )
}
