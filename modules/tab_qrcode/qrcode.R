qrcode_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        status = "success",
        title = "QR-Code generieren",
        solidHeader = TRUE,
        shiny::uiOutput(
          outputId = ns("type")
        ),
        shiny::uiOutput(
          outputId = ns("base_url")
        ),
        htmltools::div(
          class = "bordered",
          shiny::plotOutput(
            outputId = ns("qrcode")
          )
        ),
        shiny::uiOutput(
          outputId = ns("link")
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

      type_name_r <- shiny::reactive({
        db_get_type_name(.values$db, shiny::req(input$type))
      })

      output$base_url <- shiny::renderUI({
        shiny::textInput(
          inputId = ns("base_url"),
          label = "Serverdomäne",
          value = "http://127.0.0.1:1234"
        )
      })

      base_url_r <- shiny::reactive({
        url <- shiny::req(input$base_url)
        if (!stringr::str_detect(url, "/$")) url <- paste0(url, "/")
      })

      link_r <- shiny::reactive({
        paste0(base_url_r(), "?type=", shiny::req(input$type))
      })

      output$qrcode <- shiny::renderPlot({
        qrcode::qrcode_gen(link_r())
      })

      output$link <- shiny::renderUI({
        htmltools::div(
          class = "flex bordered bg-lightgray",
          htmltools::a(
            href = link_r(),
            link_r()
          ),
          shiny::downloadButton(
            outputId = ns("pdf"),
            label = NULL,
            icon = shiny::icon("file-pdf")
          ),
          rclipboard::rclipButton(
            inputId = ns("clip"),
            label = NULL,
            icon = shiny::icon("clipboard"),
            clipText = link_r()
          )
        )
      })

      file_name_r <- shiny::reactive({
        paste0(Sys.Date(), "_QR_", type_name_r(), ".pdf")
      })

      output$pdf <- shiny::downloadHandler(
        filename = file_name_r,
        content = function(file) {
          Cairo::Cairo(
            file = file,
            type = "pdf",
            units = "mm",
            width = width_r(),
            height = height_r()
          )
          qrcode::qrcode_gen(link_r())
          dev.off()
        },
        contentType = "application/pdf"
      )
    }
  )
}
