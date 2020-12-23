show_types_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = "Typen anzeigen",
    solidHeader = TRUE,
    shiny::uiOutput(
      outputId = ns("select_group")
    ),
    DT::dataTableOutput(
      outputId = ns("type_table")
    )
  )
}

show_types_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      groups_r <- shiny::reactive({
        DB::db_get_groups(.values$db)
      })

      output$select_group <- shiny::renderUI({
        .values$update$group()

        shiny::selectInput(
          inputId = ns("group"),
          label = "Gruppe",
          choices = groups_r(),
          selected = NA
        )
      })

      output$type_table <- DT::renderDataTable({
        types <- DB::db_get_types_by_group(.values$db, input$group)

        tbl <- tibble(types = names(types))

        DT::datatable(
          tbl,
          colnames = "Typname",
          options = list(
            preDrawCallback = DT::JS("
              function (settings) {
                console.log(1);
                pageScrollPos = $('body').scrollTop();
              }
            "),
            drawCallback = DT::JS("
              function (settings) {
                console.log(2);
                $('body').scrollTop(pageScrollPos);
              }
            ")
          )
        )
      })
    }
  )
}
