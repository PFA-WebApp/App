container_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::dashboardPage(
    title = "Sensotheka",
    freshTheme = fresh::create_theme(
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
        image = "img/sensotheka.png",
        href = "https://pfa-webapp.github.io/Docs/"
      ),
      rightUi = htmltools::tagList(
        htmltools::tags$li(
          # Fake dropdown
          class = "dropdown",
          htmltools::a(
            class = "github-link",
            href = "https://github.com/PFA-WebApp/App",
            shiny::icon("github")
          )
        ),
        htmltools::tags$li(
          # Fake dropdown
          class = "dropdown",
          shiny::actionLink(
            inputId = ns("refresh_db"),
            label = NULL,
            icon = shiny::icon("sync"),
            class = "refresh-link"
          )
        ),
        language_selector(
          inputId = ns("language")
        )
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

      sidebar_menu_return <- sidebar_menu_server(
        id = "sidebar_menu",
        .values = .values
      )

      shiny::observeEvent(input$language, {
        shiny.i18n::update_lang(.values$app_session, input$language)
        .values$i18n$set_language(input$language)
        .values$language_rv(input$language)
      })

      servers <- list(
        login = function() {
          login_server(
            id = "login",
            .values = .values
          )
        },
        operate = function() {
          operate_server(
            id = "operate",
            .values = .values
          )
        },
        reporting = function() {
          reporting_server(
            id = "reporting",
            .values = .values
          )
        },
        user_management = function() {
          user_management_server(
            id = "user_management",
            .values = .values
          )
        },
        group = function() {
          group_server(
            id = "group",
            .values = .values
          )
        },
        type = function() {
          type_server(
            id = "type",
            .values = .values
          )
        },
        file_management = function() {
          file_management_server(
            id = "file_management",
            .values = .values
          )
        },
        qrcode = function() {
          qrcode_server(
            id = "qrcode",
            .values = .values
          )
        },
        settings = function() {
          settings_server(
            id = "settings",
            .values = .values
          )
        }
      )

      called_rv <- shiny::reactiveVal(character())

      shiny::observeEvent(sidebar_menu_return$sidebar_r(), {
        call_modules(
          id = sidebar_menu_return$sidebar_r(),
          servers = servers,
          called_rv = called_rv
        )
      })

      shiny::observeEvent(input$refresh_db, {
        purrr::walk(.values$update, function(rv) {
          rv(rv() + 1)
        })

        shiny::showNotification(
          ui = .values$i18n$t("data_refreshed"),
          duration = 5,
          type = "warning"
        )
      })
    }
  )
}

call_modules <- function(id, servers, called_rv) {
  if (!id %in% called_rv()) {
    called_rv(c(called_rv(), id))
    # Call server function of this module
    servers[[id]]()
  }
}
