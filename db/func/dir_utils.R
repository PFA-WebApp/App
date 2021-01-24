dir_create <- function(name, id) {
  dir.create(file.path("files", name, id))
}

dir_remove <- function(name, id) {
  unlink(file.path("files", name, id), recursive = TRUE)
}
