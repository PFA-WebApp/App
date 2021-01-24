file_manager_link <- function(name, href) {
  as.character(htmltools::a(
    name,
    download = NA,
    href = href
  ))
}
