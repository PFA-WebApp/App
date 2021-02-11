container_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::dashboardPage(
    freshTheme = fresh::create_theme(
      fresh::bs4dash_status(
        light = "#0073b7",
        dark = "#0073b7"
      ),
      fresh::bs4dash_vars(
        sidebar_dark_bg = "#343a40",
        sidebar_light_bg = "#343a40",
        sidebar_dark_color = "#C2C7D0",
        sidebar_light_color = "#C2C7D0",
        sidebar_light_hover_color = "#C2C7D0",
        sidebar_dark_hover_color = "#C2C7D0",
        sidebar_light_hover_bg = "hsla(100, 100%, 100%, 0.1)",
        sidebar_light_submenu_color = "#C2C7D0",
        sidebar_light_submenu_bg = "transparent",
        sidebar_light_submenu_hover_color = "#C2C7D0",
        sidebar_light_submenu_hover_bg = "hsla(100, 100%, 100%, 0.1)",
        sidebar_light_submenu_active_color = "#343a40",
        sidebar_light_submenu_active_bg = "hsla(100, 100%, 100%, 0.9)"
      )
    ),
    bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "Sensotheka",
        image = "img/sensotheka.png"
      )
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
