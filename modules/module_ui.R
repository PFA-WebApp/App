module_ui_ui <- function(id) {
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
        ),
        shinydashboard::menuItem(
          text = "QR-Generator",
          tabName = "qr_generator"
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "login"
        ),
        shinydashboard::tabItem(
          tabName = "qr_generator",
          qr_generator_ui(
            id = ns("id_qr_generator")
          )
        )
      )
    )
  )
}

module_ui <- function(
  input, output, session, .values
) {

  ns <- session$ns

  .values$update_sidebar <- function(tabName) {
    shinydashboard::updateTabItems(
      session = session,
      inputId = "sidebar",
      selected = tabName
    )
  }

  shiny::callModule(
    module = qr_generator,
    id = "id_qr_generator",
    .values = .values
  )
}
