check_required <- function(required, settings, db, func, label) {
  purrr::walk2(names(required), required, function(name, reqs) {
    purrr::walk(reqs, function(req) {
      if (!hasName(get(name), req))
        stop("\"", name, "\" is missing slot \"", req, "\"")
    })
  })
}
