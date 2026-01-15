#' Add Type to a Group
#'
#' @template db
#' @template id
#' @templateVar key group
#' @template id
#' @templateVar key type
#'
#' @family group_type
#'
#' @export
db_add_group_type <- function(db, group_id, type_id) {
  entry <- tibble::tibble(
    group_id = group_id,
    type_id = type_id
  )

  DBI::dbAppendTable(db, "group_type", entry)
}



#' Remove Type from a Group
#'
#' @inheritParams db_add_group_type
#'
#' @family group_type
#'
#' @export
db_remove_group_type <- function(db, group_id, type_id) {
  DBI::dbExecute(
    db,
    "DELETE FROM group_type WHERE group_id = ? AND type_id = ?",
    params = list(group_id, type_id)
  )
}

db_remove_group_type_by_type_id <- function(db, type_id) {
  DBI::dbExecute(
    db,
    "DELETE FROM group_type WHERE type_id = ?",
    params = list(type_id)
  )
}

db_remove_group_type_by_group_id <- function(db, group_id) {
  DBI::dbExecute(
    db,
    "DELETE FROM group_type WHERE group_id = ?",
    params = list(group_id)
  )
}



#' Check If Type Is Part of a Group
#'
#' @inheritParams db_add_group_type
#'
#' @family group_type
#'
#' @export
db_has_group_type <- function(db, group_id, type_id) {
  n <- DBI::dbGetQuery(
    db,
    "SELECT COUNT(*) AS n FROM group_type WHERE group_id = ? AND type_id = ?",
    params = list(group_id, type_id)
  )$n

  as.logical(n)
}



#' Get All Groups of a Type
#'
#' @template db
#' @template id
#' @templateVar key type
#'
#' @template return-named-vector
#' @templateVar key group
#'
#' @family group_type
#'
#' @export
db_get_groups_by_type <- function(db, type_id) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT groups.rowid AS group_id, groups.group_name AS group_name
    FROM group_type
    INNER JOIN groups
    ON groups.rowid = group_type.group_id
    WHERE group_type.type_id = ?",
    params = list(type_id)
  )

  x <- tbl$group_id
  names(x) <- tbl$group_name

  x
}


#' Get All Types of a Group
#'
#' @template db
#' @template id
#' @templateVar key group
#'
#' @template return-named-vector
#' @templateVar key type
#'
#' @family group_type
#'
#' @export
db_get_types_by_group <- function(db, group_id) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT type.rowid AS type_id, type.type_name AS type_name
    FROM group_type
    INNER JOIN type
    ON type.rowid = group_type.type_id
    WHERE group_type.group_id = ?",
    params = list(group_id)
  )

  x <- tbl$type_id
  names(x) <- tbl$type_name

  x
}



#' Set Group Type By Group ID
#'
#' @template db
#' @template id
#' @templateVar key group
#' @param type_ids Type IDs
#'
#' @family group_type
#'
#' @export
db_set_group_type_by_group_id <- function(db, group_id, type_ids) {
  db_remove_group_type_by_group_id(db, group_id)
  db_add_group_type(db, rep(group_id, times = length(type_ids)), type_ids)
}



#' Set Group Type By Type ID
#'
#' @template db
#' @template id
#' @templateVar key type
#' @param group_ids Group IDs
#'
#' @family group_type
#'
#' @export
db_set_group_type_by_type_id <- function(db, type_id, group_ids) {
  db_remove_group_type_by_type_id(db, type_id)
  db_add_group_type(db, group_ids, rep(type_id, times = length(group_ids)))
}
