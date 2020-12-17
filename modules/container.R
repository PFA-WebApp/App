container_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = "Sensotheka"
    ),
    shinydashboard::dashboardSidebar(
      sidebar_menu_ui(
        id = ns("sidebar_menu")
      ),
      collapsed = FALSE
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "login",
          login_ui(
            id = ns("login")
          )
        ),
        shinydashboard::tabItem(
          tabName = "operate",
          operate_ui(
            id = ns("operate")
          )
        ),
        shinydashboard::tabItem(
          tabName = "reporting",
          reporting_ui(
            id = ns("reporting")
          )
        ),
        shinydashboard::tabItem(
          tabName = "user_management",
          user_management_ui(
            id = ns("user_management")
          )
        ),
        shinydashboard::tabItem(
          tabName = "sensor_management",
          sensor_management_ui(
            id = ns("sensor_management")
          )
        ),
        shinydashboard::tabItem(
          tabName = "settings",
          settings_ui(
            id = ns("settings")
          )
        )
      )
    )
  )
}

container_server <- function(id, .values) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Register function for updating sidebar from other modules
      .values$update_sidebar <- function(tabName) {
        shinydashboard::updateTabItems(
          session = session,
          inputId = "sidebar",
          selected = tabName
        )
      }

      sidebar_menu_server(
        id = "sidebar_menu",
        .values = .values
      )

      login_server(
        id = "login",
        .values = .values
      )

      operate_server(
        id = "operate",
        .values = .values
      )

      reporting_server(
        id = "reporting",
        .values = .values
      )

      user_management_server(
        id = "user_management",
        .values = .values
      )

      sensor_management_server(
        id = "sensor_management",
        .values = .values
      )

      settings_server(
        id = "settings",
        .values = .values
      )
    }
  )
}
