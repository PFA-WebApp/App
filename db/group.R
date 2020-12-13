db_add_group <- function(db, group_name) {
  group_id <- db_length(db, "group") + 1
  link <- group_id

  entry <- tibble::tibble(
    group_id = group_id,
    group_name = group_name,
    link = link
  )

  DBI::dbAppendTable(db, "group", entry)
}

db_remove_group <- function(db, group_name) {

}

db_set_group_name <- function(db, group_id, group_name) {
  DBI::dbExecute(
    db,
    "UPDATE group SET group_name = ? WHERE group_id = ?",
    params = list(group_name, group_id)
  )
}

db_has_group_name <- function(db, group_name) {
  group_name %in% db_get_group_names(db)
}

db_get_group_names <- function(db) {
  DBI::dbGetQuery(db, "SELECT group_name FROM group")$group_name
}
