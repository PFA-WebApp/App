`%_%` <- function(x, y) paste0(x, "_", y)

`%||%` <- function(x, y) if (!is.null(x)) x else y

as_german <- function(x) {
  dict <- c(
    "1"  = "ein",
    "2"  = "zwei",
    "3"  = "drei",
    "4"  = "vier",
    "5"  = "fünf",
    "6"  = "sechs",
    "7"  = "sieben",
    "8"  = "acht",
    "9"  = "neun",
    "10" = "zehn",
    "11" = "elf",
    "12" = "zwölf"
  )

  if (x <= 12) {
    dict[as.character(x)]
  } else {
    x
  }
}
