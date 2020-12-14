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
          "sensor_management"
        ),
        moderator = c("login", "operate", "reporting", "user_management"),
        user = c("login", "operate")
      )

      # List of all possible menu items. Extraction is done according to access
      # right
      menu_item_list <- list(
        login = shinydashboard::menuItem(
          text = "Login",
          tabName = "login"
        ),
        operate = shinydashboard::menuItem(
          text = "Ausleihen & Zurückgeben",
          tabName = "operate"
        ),
        reporting = shinydashboard::menuItem(
          text = "Reporting",
          tabName = "reporting"
        ),
        sensor_management = shinydashboard::menuItem(
          text = "Sensorverwaltung",
          tabName = "sensor_management"
        ),
        user_management = shinydashboard::menuItem(
          text = "Nutzerverwaltung",
          tabName = "user_management"
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
