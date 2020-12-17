sensor_management_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        status = "primary",
        title = "Sensorinformationen",
        solidHeader = TRUE,
        shiny::textInput(
          inputId = ns("type"),
          label = "Typ",
          placeholder = "S-Klasse"
        ),
        shiny::textInput(
          inputId = ns("subtype"),
          label = "Untertyp",
          placeholder = "W220"
        ),
      ),
      shinydashboard::box(
        width = NULL,
        status = "success",
        title = "QR-Code generieren",
        solidHeader = TRUE,
        actionButton(
          inputId = ns("generate"),
          label = "Generieren",
          width = "100%"
        ),
        shiny::plotOutput(
          outputId = ns("code")
        )
      )
    )
  )
}

sensor_management_server <- function(id, .values) {
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
    }
  )
}
