#' Add Group to Group Table
#'
#' @template db
#' @template xxx-name
#' @templateVar key group
#'
#' @family group
#'
#' @export
db_add_group <- function(db, group_name) {
  entry <- tibble::tibble(
    group_name = group_name
  )

  DBI::dbAppendTable(db, "groups", entry)

  id <- max(DBI::dbGetQuery(db, "SELECT rowid FROM groups")$rowid)
  dir_create("group", id)
}



#' Remove Group from Group Table
#'
#' @template db
#' @template xxx-name
#' @templateVar key group
#'
#' @family group
#'
#' @export
db_remove_group <- function(db, group_id) {
  success <- DBI::dbExecute(
    db,
    "DELETE FROM groups WHERE rowid = ?",
    params = list(group_id)
  )

  dir_remove("group", group_id)

  success
}



#' Set Group Name
#'
#' Set the name of the group with ID \code{group_id} to \code{group_name}.
#'
#' @template db
#' @template id
#' @templateVar key group
#' @param group_name New group name.
#'
#' @family group
#'
#' @export
db_set_group_name <- function(db, group_id, group_name) {
  DBI::dbExecute(
    db,
    "UPDATE groups SET group_name = ? WHERE rowid = ?",
    params = list(group_name, group_id)
  )
}



#' Get Group Name
#'
#' Get the name of the group with ID \code{group_id}.
#'
#' @template db
#' @template id
#' @templateVar key group
#'
#' @family group
#'
#' @export
db_get_group_name <- function(db, group_id) {
  DBI::dbGetQuery(
    db,
    "SELECT group_name FROM groups WHERE rowid = ?",
    params = list(group_id)
  )
}




#' Check If Group Table Has Group Name
#'
#' @template db
#' @template xxx-name
#' @templateVar key group
#'
#' @family group
#'
#' @export
db_has_group_name <- function(db, group_name) {
  group_name %in% names(db_get_groups(db))
}



#' Check If Group Table Has Group ID
#'
#' @template db
#' @template id
#' @templateVar key group
#'
#' @family group
#'
#' @export
db_has_group_id <- function(db, group_id) {
  group_id %in% db_get_groups(db)
}



#' Get Group Names
#'
#' @template db
#'
#' @template return-named-vector
#' @templateVar key group
#'
#' @family group
#'
#' @export
db_get_groups <- function(db) {
  tbl <- DBI::dbGetQuery(db, "SELECT rowid AS group_id, group_name FROM groups")

  x <- tbl$group_id
  names(x) <- tbl$group_name

  x
}
