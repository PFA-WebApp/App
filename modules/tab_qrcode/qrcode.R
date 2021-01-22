qrcode_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      shinydashboard::box(
        width = NULL,
        status = "success",
        title = "QR-Code generieren",
        solidHeader = TRUE,
        shiny::uiOutput(
          outputId = ns("type")
        ),
        shiny::plotOutput(
          outputId = ns("qrcode")
        )
      )
    )
  )
}

qrcode_server <- function(id, .values) {
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

      link_r <- shiny::reactive({
        "code" %_% shiny::req(input$type)
      })

      output$qrcode <- shiny::renderPlot({
        qrcode::qrcode_gen(link_r())
      })
    }
  )
}
