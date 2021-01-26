sidebar_menu_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::sidebarMenuOutput(
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
        user = c("login", "operate", "settings")
      )

      # List of all possible menu items. Extraction is done according to access
      # right
      menu_item_list <- list(
        login = shinydashboard::menuItem(
          text = "Anmeldung",
          tabName = "login",
          icon = shiny::icon("sign-in-alt")
        ),
        operate = shinydashboard::menuItem(
          text = "Ausleihen & Zurückgeben",
          tabName = "operate",
          icon = shiny::icon("shipping-fast")
        ),
        reporting = shinydashboard::menuItem(
          text = "Berichtswesen",
          tabName = "reporting",
          icon = shiny::icon("chart-line")
        ),
        sensor_management = shinydashboard::menuItem(
          text = "Sensorverwaltung",
          icon = shiny::icon("temperature-low"),
          shinydashboard::menuSubItem(
            text = "Gruppen",
            tabName = "group",
            icon = shiny::icon("layer-group")
          ),
          shinydashboard::menuSubItem(
            text = "Sensortypen",
            tabName = "type",
            icon = shiny::icon("tags")
          ),
          shinydashboard::menuSubItem(
            text = "Dateiverwaltung",
            tabName = "file_management",
            icon = shiny::icon("file-pdf")
          ),
          shinydashboard::menuSubItem(
            text = "QR-Code",
            tabName = "qrcode",
            icon = shiny::icon("qrcode")
          )
        ),
        user_management = shinydashboard::menuItem(
          text = "Nutzerverwaltung",
          tabName = "user_management",
          icon = shiny::icon("user-edit")
        ),
        settings = shinydashboard::menuItem(
          text = "Einstellungen",
          tabName = "settings",
          icon = shiny::icon("cog")
        )
      )

      output$menu <- shinydashboard::renderMenu({
        sidebar_menu_r()
      })

      sidebar_menu_r <- shiny::reactive({
        menu_items <- unname(menu_item_list[access_list[[.values$user$status()]]])

        shinydashboard::sidebarMenu(.list = menu_items)
      })
    }
  )
}
