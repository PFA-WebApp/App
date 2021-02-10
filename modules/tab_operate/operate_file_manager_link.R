operate_file_manager_link <- function(name, href) {
  as.character(
    htmltools::tags$a(
      target = "_blank",
      href = href,
      name
    )
  )
}
