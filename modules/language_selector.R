language_selector <- function(inputId, choices = c("de", "en", "es", "fr")) {
  htmltools::tags$li(
    id = inputId,
    class = "dropdown language-selector",
    `data-choices` = paste0(choices, collapse = ","),
    `data-selected` = choices[1],
    htmltools::tags$a(
      class = "dropdown-toggle dropdown-flag",
      `data-toggle` = "dropdown",
      `aria-haspopup` = "true",
      `aria-expanded` = "false"
    ),
    htmltools::div(
      class = "dropdown-menu"
    )
  )
}
