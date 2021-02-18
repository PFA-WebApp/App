sidebar_menu_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::sidebarMenuOutput(
    outputId = ns("menu")
  )
}

sidebar_menu_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Name of menu items that are visible according to access right
      access_list <- list(
        not_logged = "login",
        admin = c(
          "login", "operate", "reporting", "user_management",
          "sensor_management", "settings"
        ),
        mod = c(
          "login", "operate", "reporting", "user_management", "settings"
        ),
        user = c("login", "operate", "reporting", "settings")
      )

      # List of all possible menu items. Extraction is done according to access
      # right
      menu_item_list <- function(selected = NULL) {
        list(
          login = bs4Dash::menuItem(
            text = "Anmeldung",
            tabName = "login",
            icon = shiny::icon("sign-in-alt")
          ),
          operate = bs4Dash::menuItem(
            text = "Ausleihen & Zurückgeben",
            tabName = "operate",
            icon = shiny::icon("shipping-fast"),
            selected = selected
          ),
          reporting = bs4Dash::menuItem(
            text = "Bestandsinformation",
            tabName = "reporting",
            icon = shiny::icon("chart-line")
          ),
          sensor_management = bs4Dash::menuItem(
            text = "Sensorverwaltung",
            icon = shiny::icon("temperature-low"),
            startExpanded = TRUE,
            bs4Dash::menuSubItem(
              text = "Gruppen",
              tabName = "group",
              icon = shiny::icon("layer-group")
            ),
            bs4Dash::menuSubItem(
              text = "Sensortypen",
              tabName = "type",
              icon = shiny::icon("tags")
            ),
            bs4Dash::menuSubItem(
              text = "Dateiverwaltung",
              tabName = "file_management",
              icon = shiny::icon("file-pdf")
            ),
            bs4Dash::menuSubItem(
              text = "QR-Code",
              tabName = "qrcode",
              icon = shiny::icon("qrcode")
            )
          ),
          user_management = bs4Dash::menuItem(
            text = "Nutzerverwaltung",
            tabName = "user_management",
            icon = shiny::icon("user-edit")
          ),
          settings = bs4Dash::menuItem(
            text = "Einstellungen",
            tabName = "settings",
            icon = shiny::icon("cog")
          )
        )
      }

      output$menu <- bs4Dash::renderMenu({
        sidebar_menu_r()
      })

      sidebar_menu_r <- shiny::reactive({
        selected <- if (.values$user$status() %in% c("user", "mod")) TRUE

        menu_items <- unname(
          menu_item_list(selected)[access_list[[.values$user$status()]]]
        )

        bs4Dash::sidebarMenu(id = ns("sidebar"), .list = menu_items)
      })

      # Register function for updating sidebar from other modules
      .values$update_sidebar <- function(tabName) {
        bs4Dash::updateTabItems(
          session = session,
          inputId = "sidebar",
          selected = tabName
        )
      }

      return_list <- list(
        sidebar_r = shiny::reactive(input$sidebar)
      )
    }
  )
}
