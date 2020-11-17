library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(stringr)

ui_server <- function(source_to_globalenv = FALSE) {
    # If source_to_global_env all sourced functions get added to the global
    # environment which takes some time after the app has stopped

    QWUtils::source_directory(
        # chdir makes it possible to use relative paths in source statements inside
        # these sourced files (for example DataStorage2.R)
        path = "modules", encoding = "UTF-8", modifiedOnly = FALSE,
        chdir = TRUE, recursive = TRUE,
        envir = if (source_to_globalenv) globalenv() else environment()
    )

    # Globals ------------------------------------------------------------------

    # Allow bigger file inputs
    options(shiny.maxRequestSize = 100*1024^2)

    options(.language = "de")

    # Enable scrolling for wide DT tables
    options(DT.options = list(dom = "lfptp", scrollX = TRUE))

    # UI -----------------------------------------------------------------------
    ui <- htmltools::div(
        tags$head(
            # Include custom css styles
            # shiny::includeCSS("www/css/styles.css")
        ),
        # ui_ui generates the UI which is displayed in the content_list,
        # viewer_data and viewer_plot
        module_ui_ui(
            id = "id_ui"
        ),
        # Enable shinyjs
        useShinyjs(),
        # Extend shinyjs with custom JavaScript
        # extendShinyjs("www/js/extend_shinyjs.js")
    )

    # SERVER -------------------------------------------------------------------

    server <- function(input, output, session) {

        # .VALUES ENVIRONMENT ------------------------------------------------

        # The .values environment is available to all modules so that arbitrary information
        # can be shared via this environment. Elements that underly reactive changes can be
        # stored as reactiveValues or reactiveVal
        .values <- new.env()
        # Set a value to .values$trigger$<value> inside a module and listen to its
        # change in some other module with observeEvent(.values$trigger$<value>, ...)
        .values$trigger <- shiny::reactiveValues()
        # Same purpose as above, but you must set the reactiveValues by yourself. This
        # is useful for modules that get reused multiple times and therefore can
        # store a trigger for each instance
        .values$trigger_list <- list()

        .values$logged <- shiny::reactiveVal(FALSE)

        # Connect to db
        # .values$db <- DBI::dbConnect(RSQLite::SQLite(), "./db/db.sqlite")

        # Use regex
        # RSQLite::initRegExp(.values$db)

        # CALL MODULE ----------------------------------------------------------
        shiny::callModule(
            module = module_ui,
            id = "id_ui",
            .values = .values
        )

        session$onSessionEnded(function() {
            # DBI::dbDisconnect(.values$db)
        })
    }

    return(list(ui = ui, server = server))
}

ui_server <- ui_server(source_to_globalenv = FALSE)

ui <- ui_server$ui
server <- ui_server$server

shinyApp(ui, server)
