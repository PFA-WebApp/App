file_manager_download_btn <- function(href) {
  as.character(
    htmltools::a(
      href = href,
      target = "_blank",
      shiny::actionButton(
        inputId = "undefined",
        label = NULL,
        icon = shiny::icon("file-download"),
        class = "primary"
      )
    )
  )
}
