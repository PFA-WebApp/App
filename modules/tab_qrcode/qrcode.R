qrcode_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = 12,
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
    shiny::fluidRow(
      shiny::column(
        width = 6,
        object_quantity_input_ui(
          id = ns("width"),
          old_quantity = 210,
          label = "Breite (in mm)"
        )
      ),
      shiny::column(
        width = 6,
        object_quantity_input_ui(
          id = ns("height"),
          old_quantity = 297,
          label = "Höhe (in mm)"
        )
      )
    ),
    shiny::uiOutput(
      outputId = ns("download_area")
    ),
    shiny::uiOutput(
      outputId = ns("link")
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
          choices = types_r(),
          width = "100%"
        )
      })

      type_name_r <- shiny::reactive({
        .values$update$type()
        db_get_type_name(.values$db, shiny::req(input$type))
      })

      output$base_url <- shiny::renderUI({
        shiny::textInput(
          inputId = ns("base_url"),
          label = "Serverdomäne",
          value = .values$yaml$url
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
          rclipboard::rclipButton(
            inputId = ns("clip"),
            label = NULL,
            icon = shiny::icon("clipboard"),
            clipText = link_r()
          )
        )
      })

      file_type_r <- shiny::reactive({
        input$file_type
      })

      file_name_r <- shiny::reactive({
        paste0(Sys.Date(), "_QR_", type_name_r(), ".", file_type_r())
      })

      width_r <- shiny::reactive({
        width_return$quantity_r()
      })

      height_r <- shiny::reactive({
        height_return$quantity_r()
      })

      dpi_r <- shiny::reactive({
        if (file_type_r() == "png") 600 else "auto"
      })

      error_r <- shiny::reactive({
        width_return$error_r() || height_return$error_r()
      })

      output$download_area <- shiny::renderUI({
        if (!error_r()) {
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(
                inputId = ns("file_type"),
                label = NULL,
                choices = c(
                  PDF = "pdf",
                  PNG = "png",
                  SVG = "svg"
                )
              )
            ),
            shiny::column(
              width = 8,
              shiny::uiOutput(
                outputId = ns("download_button"),
                style = "margin-bottom: 16px;"
              )
            )
          )
        }
      })

      output$download_button <- shiny::renderUI({
        icon_name <- if (file_type_r() == "pdf") "file-pdf" else "file-image"

        shiny::downloadButton(
          outputId = ns("download"),
          label = paste("Als", toupper(file_type_r()), "herunterladen"),
          icon = shiny::icon(icon_name),
          style = "width: 100%"
        )
      })

      output$download <- shiny::downloadHandler(
        filename = file_name_r,
        content = function(file) {
          Cairo::Cairo(
            file = file,
            type = file_type_r(),
            units = "mm",
            width = width_r(),
            height = height_r(),
            dpi = dpi_r()
          )
          qrcode::qrcode_gen(link_r())
          dev.off()
        },
        contentType = NULL
      )

      min_width_r <- shiny::reactive(32)

      width_return <- object_quantity_input_server(
        id = "width",
        .values = .values,
        min_r = min_width_r,
        min_message_r = shiny::reactive({
          "Die minimale Seitenbreite muss mindestens 32 mm betragen.\n\n"
        }),
        object_label = "Die Seitenbreite"
      )

      min_height_r <- shiny::reactive(47)

      height_return <- object_quantity_input_server(
        id = "height",
        .values = .values,
        min_r = min_height_r,
        min_message_r = shiny::reactive({
          "Die minimale Seitenhöhe muss mindestens 47 mm betragen.\n\n"
        }),
        object_label = "Die Seitenbreite"
      )
    }
  )
}
