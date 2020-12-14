qr_generator_ui <- function(id) {
  ns <- shiny::NS(id)

  fluidRow(
    column(width = 5,
           box(width = NULL, status = "primary", title = "Sensorinformationen", solidHeader = TRUE,
               textInput(inputId = ns("type"), label = "Typ", placeholder = "S-Klasse"),
               textInput(inputId = ns("subtype"), label = "Untertyp", placeholder = "W220"),
           ),
           box(width = NULL, status = "success", title = "QR-Code generieren", solidHeader = TRUE,
               actionButton(inputId = ns("generate"), label = "Generieren", width = "100%"),
               plotOutput(outputId = ns("code"), width = "100%", inline = FALSE)
           )
    )
  )
}

qr_generator_server <- function(id, .values) {
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
