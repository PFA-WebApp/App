`%_%` <- function(x, y) paste0(x, "_", y)

`%||%` <- function(x, y) if (!is.null(x)) x else y

is_empty <- function(x) {
  is.null(x) || is.na(x)
}

format_number <- function(x) {
  dict <- c(
    "1"  = "${one}",
    "2"  = "${two}",
    "3"  = "${three}",
    "4"  = "${four}",
    "5"  = "${five}",
    "6"  = "${six}",
    "7"  = "${seven}",
    "8"  = "${eight}",
    "9"  = "${nine}",
    "10" = "${ten}",
    "11" = "${eleven}",
    "12" = "${twelve}"
  )

  if (x <= 12) {
    dict[as.character(x)]
  } else {
    x
  }
}
