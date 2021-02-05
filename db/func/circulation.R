#' Add Circulation Entry
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_add_circulation <- function(db, user_id, subtype_id, quantity) {
  entry <- tibble::tibble(
    user_id = user_id,
    subtype_id = subtype_id,
    quantity = quantity,
    time = as.character(Sys.time())
  )

  DBI::dbAppendTable(
    db,
    "circulation",
    entry
  )
}



#' Get Borrowed Quantity
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_borrowed_quantity <- function(db, subtype_id) {
  borrowed <- DBI::dbGetQuery(
    db,
    "SELECT SUM(quantity) AS borrowed FROM circulation WHERE subtype_id = ?",
    params = list(subtype_id)
  )$borrowed

  if (is.na(borrowed)) 0 else borrowed
}




#' Get Available Quantity
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_available_quantity <- function(db, subtype_id) {
  max_quantity <- db_get_subtype_max_quantity(db, subtype_id)
  borrowed_quantity <- db_get_borrowed_quantity(db, subtype_id)
  max_quantity - borrowed_quantity
}



#' Get Borrowed Quantity From User
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_borrowed_quantity_by_user_id <- function(db, user_id, subtype_id) {
  borrowed <- DBI::dbGetQuery(
    db,
    "SELECT SUM(quantity) AS borrowed FROM circulation WHERE user_id = ? AND subtype_id = ?",
    params = list(user_id, subtype_id)
  )$borrowed

  if (is.na(borrowed)) 0 else borrowed
}
