db_add_user <- function(db, name, status, password) {
  entry <- tibble::tibble(
    name = name,
    status = status,
    # Hashed password
    password = password
  )

  DBI::dbAppendTable(db, "user", entry)
}

db_get_user_status <- function(db, name) {
  DBI::dbGetQuery(
    db,
    "SELECT status FROM user WHERE name = ?",
    params = list(name)
  )$status
}

db_set_user_status <- function(db, name, status) {
  DBI::dbExecute(
    db,
    "UPDATE user SET status = ? WHERE name = ?",
    params = list(status, name)
  )
}

db_get_user_names <- function(db) {
  DBI::dbGetQuery(db, "SELECT name FROM user")$name
}

db_remove_user <- function(db, name) {
  DBI::dbExecute(
    db,
    "DELETE FROM user WHERE name = ?",
    params = list(name)
  )
}

db_get_password <- function(db, name) {
  pwd <- DBI::dbGetQuery(
    db,
    "SELECT password FROM user WHERE name = ?",
    params = list(name)
  )$password
}

db_set_password <- function(db, name, password) {
  DBI::dbExecute(
    db,
    "UPDATE user SET password = ? WHERE name = ?",
    params = list(password, name)
  )
}

db_has_user_name <- function(db, name) {
  name %in% db_get_user_names(db)
}
