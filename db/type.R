db_add_type <- function(db, type_name) {
  type_id <- db_length(db, "type") + 1
  link <- type_id

  entry <- tibble::tibble(
    type_id = type_id,
    type_name = type_name,
    link = link
  )

  DBI::dbAppendTable(db, "type", entry)
}

db_set_type_name <- function(db, type_id, type_name) {
  DBI::dbExecute(
    db,
    "UPDATE type SET type_name = ? WHERE type_id = ?",
    params = list(type_name, type_id)
  )
}
