sort_translation <- function(path = "translation/translation.json") {
  x <- jsonlite::read_json(path)

  x <- purrr::map(x, function(el) {
    el[order(names(el))]
  })

  check_names(x)

  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE)
}

check_names <- function(x) {
  nms <- NULL
  for (i in seq_along(x)) {
    el <- x[[i]]
    if (is.null(nms)) {
      nms <- names(el)
    } else {
      if (!identical(nms, names(el))) {
        missing_old <- setdiff(names(el), nms)
        missing_new <- setdiff(nms, names(el))

        warn_old <- if (length(missing_old)) {
          paste0(
            "Names in ", names(x)[i], " but not in ",
            names(x)[1], ": ", missing_old
          )
        }

        warn_new <- if (length(missing_new)) {
          paste0(
            "Names in ", names(x)[1], " but not in ",
            names(x)[i], ": ", missing_new
          )
        }

        warning(warn_old, "\n", warn_new)
      }
    }
  }
}


