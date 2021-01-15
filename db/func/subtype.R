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
db_add_subtype <- function(db, type_id, subtype_name, quantity) {
  entry <- tibble::tibble(
    type_id = type_id,
    subtype_name = subtype_name
  )

  DBI::dbAppendTable(db, "subtype", entry)
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
  DBI::dbExecute(
    db,
    "UPDATE subtype SET subtype_name = ? WHERE rowid = ?",
    params = list(subtype_name, subtype_id)
  )
}



#' Set Available Quantity of Subtype
#'
#' @template db
#' @template id
#' @templateVar key subtype
#' @template quantity
#'
#' @family subtype
#'
#' @export
db_set_subtype_quantity <- function(db, subtype_id, quantity) {
  DBI::dbExecute(
    db,
    "UPDATE subtype SET quantity = ? WHERE rowid = ?",
    params = list(quantity, subtype_id)
  )
}
