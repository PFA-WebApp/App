db_add_user <- function(db, name, status, password) {
  entry <- tibble::tibble(
    name = name,
    status = status,
    # Hashed password
    password = password
  )

  DBI::dbAppendTable(db, "user", entry)
}

db_set_user_status <- function(db, name, status) {
  DBI::dbExecute(
    db,
    "UPDATE user SET status = ? WHERE name = ?",
    params = list(status, name)
  )
}
