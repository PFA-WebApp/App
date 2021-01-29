container_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::dashboardPage(
    bs4Dash::dashboardHeader(
      title = "Sensotheka",
      status = "primary"
    ),
    bs4Dash::dashboardSidebar(
      sidebar_menu_ui(
        id = ns("sidebar_menu")
      ),
      collapsed = FALSE,
      skin = "light"
    ),
    bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "login",
          login_ui(
            id = ns("login")
          )
        ),
        bs4Dash::tabItem(
          tabName = "operate",
          operate_ui(
            id = ns("operate")
          )
        ),
        bs4Dash::tabItem(
          tabName = "reporting",
          reporting_ui(
            id = ns("reporting")
          )
        ),
        bs4Dash::tabItem(
          tabName = "user_management",
          user_management_ui(
            id = ns("user_management")
          )
        ),
        bs4Dash::tabItem(
          tabName = "group",
          group_ui(
            id = ns("group")
          )
        ),
        bs4Dash::tabItem(
          tabName = "type",
          type_ui(
            id = ns("type")
          )
        ),
        bs4Dash::tabItem(
          tabName = "file_management",
          file_management_ui(
            id = ns("file_management")
          )
        ),
        bs4Dash::tabItem(
          tabName = "qrcode",
          qrcode_ui(
            id = ns("qrcode")
          )
        ),
        bs4Dash::tabItem(
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
        bs4Dash::updateTabItems(
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

      group_server(
        id = "group",
        .values = .values
      )

      type_server(
        id = "type",
        .values = .values
      )

      file_management_server(
        id = "file_management",
        .values = .values
      )

      qrcode_server(
        id = "qrcode",
        .values = .values
      )

      settings_server(
        id = "settings",
        .values = .values
      )
    }
  )
}
