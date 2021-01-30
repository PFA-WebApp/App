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
