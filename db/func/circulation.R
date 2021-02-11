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


#' Get Borrow Summary
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_borrow_summary <- function(db) {
  DBI::dbGetQuery(
    db,
    "SELECT subtype_id, quantity FROM circulation"
  ) %>%
    borrow_summary(sym("subtype_id"))
}



#' Get Borrow Summary By User ID
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_borrow_summary_by_user_id <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT subtype_id, quantity FROM circulation WHERE user_id = ?",
    params = list(user_id)
  ) %>%
    borrow_summary(sym("subtype_id"))
}



#' Get Borrow Summary By Subtype ID
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_borrow_summary_by_subtype_id <- function(db, subtype_id) {
  DBI::dbGetQuery(
    db,
    "SELECT user_id, quantity FROM circulation WHERE subtype_id = ?",
    params = list(subtype_id)
  ) %>%
    borrow_summary(sym("user_id"))
}




borrow_summary <- function(tbl, group_by) {
  tbl %>%
    dplyr::group_by(!!group_by) %>%
    dplyr::summarise(quantity = sum(quantity), .groups = "drop") %>%
    dplyr::filter(quantity > 0)
}
