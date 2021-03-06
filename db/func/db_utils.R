#' Determine the length of a table
#'
#' Determine the length of a table, i.e. the number of records.
#'
#' @template db
#' @template name
#'
#' @export
db_length <- function(db, name) {
  DBI::dbGetQuery(db, paste("SELECT COUNT(*) AS n FROM", name))$n
}


#' Wrapper to DBI::dbReadTable
#'
#' Get the table of \code{db} with name \code{name}.
#'
#' @template db
#' @template name
#'
#' @export
db_get_table <- function(db, name, include_removed = FALSE) {
  query <- paste(
    "SELECT * FROM",
    name
  )

  if (!include_removed && name %in% c("user", "groups", "type", "subtype")) {
    query <- paste(
      query,
      "WHERE removed = 0"
    )
  }

  DBI::dbGetQuery(
    db,
    query
  )
}
