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

      output$menu <- shinydashboard::renderMenu({
        print(sidebar_menu_r())
      })

      sidebar_menu_r <- shiny::reactive({
        if (!.values$user$logged()) return(sidebar_menu_not_logged_r())

        switch(
          .values$user$type(),
          "admin" = sidebar_menu_admin_r(),
          "moderator" = sidebar_menu_moderator_r(),
          "user" = sidebar_menu_user_r()
        )
      })

      sidebar_menu_not_logged_r <- shiny::reactive({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            text = "Login",
            tabName = "login"
          )
        )
      })

      sidebar_menu_admin_r <- shiny::reactive({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            text = "Login",
            tabName = "login"
          ),
          shinydashboard::menuItem(
            text = "Admin",
            tabName = "admin"
          )
        )
      })

      sidebar_menu_moderator_r <- shiny::reactive({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            text = "Login",
            tabName = "login"
          ),
          shinydashboard::menuItem(
            text = "Moderator",
            tabName = "moderator"
          )
        )
      })

      sidebar_menu_user_r <- shiny::reactive({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            text = "Login",
            tabName = "login"
          ),
          shinydashboard::menuItem(
            text = "User",
            tabName = "user"
          )
        )
      })
    }
  )
}
