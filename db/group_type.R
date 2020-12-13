db_add_group_type <- function(db, group_id, type_id) {
  entry <- tibble::tibble(
    group_id = group_id,
    type_id = type_id
  )

  DBI::dbAppendTable(db, "group_type", entry)
}

db_remove_group_type <- function(db, group_id, type_id) {
  DBI::dbExecute(
    db,
    "DELETE FROM group_type WHERE group_id = ? AND type_id = ?",
    params = list(group_id, type_id)
  )
}

db_has_group_type <- function(db, group_id, type_id) {
  n <- DBI::dbGetQuery(
    db,
    "SELECT COUNT(*) AS has FROM group_type WHERE group_id = ? AND type_id = ?",
    params = list(group_id, type_id)
  )

  as.logical(n)
}

db_get_groups_by_type <- function(db, type_id) {
  DBI::dbGetQuery(
    db,
    "SELECT group_id FROM group_type WHERE type_id = ?",
    params = list(type_id)
  )$group_id
}

db_get_types_by_group <- function(db, group_id) {
  DBI::dbGetQuery(
    db,
    "SELECT type_id FROM group_type WHERE group_id = ?",
    params = list(group_id)
  )$type_id
}
