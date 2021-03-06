#' Add Type to Type Table
#'
#' @template db
#' @template xxx-name
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_add_type <- function(db, type_name) {
  entry <- tibble::tibble(
    type_name = type_name,
    removed = 0
  )

  success <- tryCatch(
    DBI::dbAppendTable(db, "type", entry),
    `Rcpp::exception` = function(e) {
      if (stringr::str_detect(e$message, "UNIQUE.*type_name")) {
        return(0)
      }

      stop(e)
    }
  )

  id <- max(DBI::dbGetQuery(db, "SELECT rowid FROM type")$rowid)
  dir_create("type", id)

  success
}



#' Remove Type from Type Table
#'
#' @template db
#' @template xxx-name
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_remove_type <- function(db, type_id) {
  success <- DBI::dbExecute(
    db,
    "UPDATE type SET removed = 1 WHERE rowid = ?",
    params = list(type_id)
  )

  dir_remove("type", type_id)

  success
}



#' Set Type Name
#'
#' @inheritParams db_add_type
#' @template id
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_set_type_name <- function(db, type_id, type_name) {
  tryCatch(
    DBI::dbExecute(
      db,
      "UPDATE type SET type_name = ? WHERE rowid = ?",
      params = list(type_name, type_id)
    ),
    `Rcpp::exception` = function(e) {
      if (stringr::str_detect(e$message, "UNIQUE.*type_name")) {
        return(0)
      }

      stop(e)
    }
  )
}



#' Get Type Name
#'
#' Get the name of the type with ID \code{type_id}.
#'
#' @template db
#' @template id
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_get_type_name <- function(db, type_id) {
  DBI::dbGetQuery(
    db,
    "SELECT type_name FROM type WHERE rowid = ?",
    params = list(type_id)
  )$type_name
}



#' Get Type ID
#'
#' Get the ID of the type with name \code{type_name}.
#'
#' @template db
#' @template xxx-name
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_get_type_id <- function(db, type_name) {
  DBI::dbGetQuery(
    db,
    "SELECT rowid FROM type WHERE type_name = ? AND removed = 0",
    params = list(type_name)
  )$rowid
}



#' Check If Type Table Has Type Name
#'
#' @template db
#' @template xxx-name
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_has_type_name <- function(db, type_name) {
  type_name %in% names(db_get_types(db))
}



#' Check If Type Table Has Type ID
#'
#' @template db
#' @template id
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_has_type_id <- function(db, type_id) {
  type_id %in% db_get_types(db)
}



#' Get Type Names
#'
#' @template db
#'
#' @template return-named-vector
#' @templateVar key type
#'
#' @family type
#'
#' @export
db_get_types <- function(db, include_removed = FALSE) {
  tbl <- if (include_removed) {
    DBI::dbGetQuery(
      db,
      "SELECT rowid, type_name FROM type ORDER BY type_name ASC"
    )
  } else {
    DBI::dbGetQuery(
      db,
      "
      SELECT rowid, type_name FROM type WHERE removed = 0
      ORDER BY type_name ASC
      "
    )
  }

  x <- tbl$rowid
  names(x) <- tbl$type_name

  x
}
