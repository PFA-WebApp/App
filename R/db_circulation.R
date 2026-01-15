#' Add Circulation Entry
#'
#' @template db
#'
#' @param op_type 1: Borrow and Return. 2: Write-off and other quantity changes
#'
#' @family circulation
#'
#' @export
db_add_circulation <- function(db, user_id, subtype_id, quantity, op_type) {
  stopifnot(op_type %in% 1:2)

  entry <- tibble::tibble(
    user_id = user_id,
    subtype_id = subtype_id,
    quantity = quantity,
    time = as.character(Sys.time()),
    op_type = op_type
  )

  DBI::dbAppendTable(
    db,
    "circulation",
    entry
  )
}



#' Get Circulation Table
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_circulation_table <- function(db) {
  DBI::dbGetQuery(
    db,
    "
    SELECT
      circulation.rowid AS rowid,
      circulation.user_id AS user_id,
      circulation.subtype_id AS subtype_id,
      circulation.quantity AS quantity,
      circulation.time AS time,
      circulation.op_type AS op_type,
      user.removed AS user_removed,
      user.name AS user_name,
      subtype.subtype_name AS subtype_name,
      subtype.removed AS subtype_removed,
      type.type_name AS type_name,
      type.removed AS type_removed,
      type.rowid AS type_id
    FROM circulation
    INNER JOIN user ON circulation.user_id = user.rowid
    INNER JOIN subtype ON circulation.subtype_id = subtype.rowid
    INNER JOIN type ON subtype.type_id = type.rowid
    "
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
    "SELECT SUM(quantity) AS borrowed FROM circulation
    WHERE subtype_id = ? AND op_type = 1",
    params = list(subtype_id)
  )$borrowed

  ifelse(is.na(borrowed), 0, borrowed)
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
    "SELECT SUM(quantity) AS borrowed FROM circulation
    WHERE user_id = ? AND subtype_id = ? AND op_type = 1",
    params = list(user_id, subtype_id)
  )$borrowed

  ifelse(is.na(borrowed), 0, borrowed)
}



#' Get Total Borrowed Quantity From user
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_total_borrowed_quantity_by_user_id <- function(db, user_id) {
  borrowed <- DBI::dbGetQuery(
    db,
    "SELECT SUM(quantity) AS borrowed FROM circulation
    WHERE user_id = ? AND op_type = 1",
    params = list(user_id)
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
    "SELECT subtype_id, quantity, time FROM circulation WHERE op_type = 1"
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
    "SELECT subtype_id, quantity, time FROM circulation
    WHERE user_id = ? AND op_type = 1",
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
    "SELECT user_id, quantity, time FROM circulation
    WHERE subtype_id = ? AND op_type = 1",
    params = list(subtype_id)
  ) %>%
    borrow_summary(sym("user_id"))
}




borrow_summary <- function(tbl, group_by) {
  tbl %>%
    dplyr::group_by(!!group_by) %>%
    dplyr::summarise(
      quantity = sum(quantity),
      time = suppressWarnings(max(time, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(quantity > 0)
}



#' Get Available Summary
#'
#' @template db
#'
#' @family circulation
#'
#' @export
db_get_available_summary <- function(db, type_id) {
  DBI::dbWithTransaction(
    db, {
      subtype_ids <- db_get_subtypes_by_type_id(db, type_id)

      tibble::tibble(
        subtype_id = subtype_ids,
        quantity = db_get_available_quantity(db, subtype_ids),
        max_quantity = db_get_subtype_max_quantity(db, subtype_ids),
        critical_quantity = db_get_critical_quantity(db, subtype_ids)
      )
    }
  )
}
