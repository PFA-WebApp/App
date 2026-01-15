#' Add Subtype to Subtype Table
#'
#' @template db
#' @template id
#' @templateVar key type
#' @param subtype_name Subtype name.
#' @template quantity
#'
#' @family subtype
#'
#' @export
db_add_subtype <- function(db, type_id, subtype_name, quantity, critical_quantity) {
  entry <- tibble::tibble(
    type_id = type_id,
    subtype_name = subtype_name,
    quantity = quantity,
    critical_quantity = critical_quantity,
    removed = 0
  )

  success <- tryCatch(
    DBI::dbAppendTable(db, "subtype", entry),
    `Rcpp::exception` = function(e) {
      if (stringr::str_detect(e$message, "UNIQUE.*type_id.*subtype_name")) {
        return(0)
      }

      stop(e)
    }
  )

  id <- max(DBI::dbGetQuery(db, "SELECT rowid FROM subtype")$rowid)
  dir_create("subtype", id)

  success
}



#' Set Subtype Name
#'
#' @inheritParams db_add_subtype
#' @template id
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_set_subtype_name <- function(db, subtype_id, subtype_name) {
  tryCatch(
    DBI::dbExecute(
      db,
      "UPDATE subtype SET subtype_name = ? WHERE rowid = ?",
      params = list(subtype_name, subtype_id)
    ),
    `Rcpp::exception` = function(e) {
      if (stringr::str_detect(e$message, "UNIQUE.*type_id.*subtype_name")) {
        return(0)
      }

      stop(e)
    }
  )
}



#' Get Subtype Name
#'
#' @template db
#' @template id
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_get_subtype_name <- function(db, subtype_id) {
  DBI::dbGetQuery(
    db,
    "SELECT subtype_name FROM subtype WHERE rowid = ?",
    params = list(subtype_id)
  )$subtype_name
}



#' Get Subtype Max Quantity
#'
#' @template db
#' @template id
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_get_subtype_max_quantity <- function(db, subtype_id) {
  DBI::dbGetQuery(
    db,
    "SELECT quantity FROM subtype WHERE rowid = ?",
    params = list(subtype_id)
  )$quantity
}



#' Set Max Quantity of Subtype
#'
#' @template db
#' @template id
#' @templateVar key subtype
#' @template quantity
#'
#' @family subtype
#'
#' @export
db_set_subtype_max_quantity <- function(db, subtype_id, quantity) {
  DBI::dbExecute(
    db,
    "UPDATE subtype SET quantity = ? WHERE rowid = ?",
    params = list(quantity, subtype_id)
  )
}



#' Increment / Decrement Max Quantity of Subtype
#'
#' @template db
#' @template id
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_change_subtype_max_quantity <- function(db, subtype_id, amount) {
  tryCatch(
    DBI::dbExecute(
      db,
      "UPDATE subtype SET quantity = quantity + ? WHERE rowid = ?",
      params = list(amount, subtype_id)
    ),
    `Rcpp::exception` = function(e) {
      if (stringr::str_detect(e$message, "CHECK.*quantity")) {
        return(0)
      }

      stop(e)
    }
  )
}




#' Get Subtypes by Type ID
#'
#' @template db
#' @template id
#' @templateVar key type
#'
#' @family subtype
#'
#' @export
db_get_subtypes_by_type_id <- function(db, type_id) {
  tbl <- DBI::dbGetQuery(
    db,
    "
    SELECT rowid, subtype_name FROM subtype WHERE type_id = ? AND removed = 0
    ORDER BY subtype_name ASC
    ",
    params = list(type_id)
  )

  x <- tbl$rowid
  names(x) <- tbl$subtype_name

  x
}



#' Get Type ID by Subtype ID
#' @template db
#' @template id
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_get_type_id_by_subtype_id <- function(db, subtype_id) {
  DBI::dbGetQuery(
    db,
    "SELECT type_id FROM subtype WHERE rowid = ?",
    params = list(subtype_id)
  )$type_id
}



#' Get All Subtypes
#'
#' @template db
#'
#' @family subtype
#'
#' @export
db_get_subtypes <- function(db, include_removed = FALSE) {
  tbl <- if (include_removed) {
    DBI::dbGetQuery(
      db,
      "SELECT rowid, subtype_name FROM subtype ORDER BY subtype_name ASC"
    )
  } else {
    DBI::dbGetQuery(
      db,
      "
      SELECT rowid, subtype_name FROM subtype WHERE removed = 0
      ORDER BY subtype_name ASC
      "
    )
  }

  x <- tbl$rowid
  names(x) <- tbl$subtype_name

  x
}



#' Get Subtype ID by Subtype Name and Type ID
#'
#' @template db
#'
#' @family subtype
#'
#' @export
db_get_subtype_id <- function(db, type_id, subtype_name) {
  DBI::dbGetQuery(
    db,
    "SELECT rowid FROM subtype WHERE type_id = ? AND subtype_name = ?",
    params = list(type_id, subtype_name)
  )$rowid
}



#' Get Subtype Table by Type ID
#'
#' @template db
#' @template id
#' @templateVar key type
#'
#' @family subtype
#'
#' @export
db_get_subtype_table_by_type_id <- function(db, type_id) {
  DBI::dbGetQuery(
    db,
    "
    SELECT rowid AS subtype_id, subtype_name, quantity FROM subtype
    WHERE type_id = ? AND removed = 0
    ",
    params = list(type_id)
  )
}



#' Remove Subtype
#'
#' @template
#' @template id
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_remove_subtype <- function(db, subtype_id) {
  success <- DBI::dbExecute(
    db,
    "UPDATE subtype SET removed = 1 WHERE rowid = ?",
    params = list(subtype_id)
  )

  dir_remove("subtype", subtype_id)

  success
}




#' Remove Subtypes by Type Id
#'
#' @template db
#' @template id
#' @templateVar key type
#'
#' @family subtype
#'
#' @export
db_remove_subtypes_by_type_id <- function(db, type_id) {
  DBI::dbExecute(
    db,
    "UPDATE subtype SET removed = 1 WHERE type_id = ?",
    params = list(type_id)
  )
}



#' Check If Table Has Subtype ID
#'
#' @template db
#' @template id
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_has_subtype_id <- function(db, subtype_id) {
  subtype_id %in% db_get_subtypes(db)
}



#' Check If Table Has Subtype Name
#'
#' @template db
#' @template xxx-name
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_has_subtype_name <- function(db, subtype_name) {
  subtype_name %in% names(db_get_subtypes(db))
}



#' Check If Type Has Subtype Name
#'
#' @template db
#' @template xxx-name
#' @templateVar key subtype
#'
#' @family subtype
#'
#' @export
db_has_type_subtype_name <- function(db, type_id, name) {
  name %in% names(db_get_subtypes_by_type_id(db, type_id))
}



#' Set Critical Quantity
#'
#' @template db
#'
#' @family subtype
#'
#' @export
db_set_critical_quantity <- function(db, subtype_id, critical_quantity) {
  DBI::dbExecute(
    db,
    "UPDATE subtype SET critical_quantity = ? WHERE rowid = ?",
    params = list(critical_quantity, subtype_id)
  )
}



#' Get Critical Quantity
#'
#' @template db
#'
#' @family subtype
#'
#' @export
db_get_critical_quantity <- function(db, subtype_id) {
  DBI::dbGetQuery(
    db,
    "SELECT critical_quantity FROM subtype WHERE rowid = ?",
    params = list(subtype_id)
  )$critical_quantity
}
