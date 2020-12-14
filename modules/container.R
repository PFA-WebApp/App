container_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = "QRTools"
    ),
    shinydashboard::dashboardSidebar(
      sidebar_menu_ui(
        id = ns("sidebar_menu")
      )
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
          tabName = "sensor_management",
          sensor_management_ui(
            id = ns("sensor_management")
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

      sensor_management_server(
        id = "sensor_management",
        .values = .values
      )
    }
  )
}
