db_add_subtype <- function(db, type_id, subtype_name, quantity) {
  subtype_id <- db_length(db, "subtype") + 1
  link <- subtype_id

  entry <- tibble::tibble(
    type_id = type_id,
    subtype_id = subtype_id,
    subtype_name = subtype_name,
    link = link
  )

  DBI::dbAppendTable(db, "subtype", entry)
}

db_set_subtype_name <- function(db, subtype_id, subtype_name) {
  DBI::dbExecute(
    db,
    "UPDATE subtype SET subtype_name = ? WHERE subtype_id = ?",
    params = list(subtype_name, subtype_id)
  )
}

db_set_subtype_quantity <- function(db, subtype_id, quantity) {
  DBI::dbExecute(
    db,
    "UPDATE subtype SET quantity = ? WHERE subtype_id = ?",
    params = list(quantity, subtype_id)
  )
}
