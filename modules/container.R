container_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = "QRTools"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = ns("sidebar"),
        shinydashboard::menuItem(
          text = "Login",
          tabName = "login"
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "login"
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

      .values$update_sidebar <- function(tabName) {
        shinydashboard::updateTabItems(
          session = session,
          inputId = "sidebar",
          selected = tabName
        )
      }
    }
  )
}
