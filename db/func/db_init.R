#' Initialise database
#'
#' Initialise SQLite database if not present.
#'
#' @param path Location at which database is created.
#'
#' @section Tables:
#' The database contains the following tables:
#'
#' * **User Table**
#'   * | **Column** | **Type** |
#'     | --- | --- |
#'     | name | character |
#'     | status | character |
#'     | password | character |
#'
#' * **Type Table**
#'   * | **Column** | **Type** |
#'     | --- | --- |
#'     | type_id | integer |
#'     | type_name | character |
#'
#' * **Subtype Table**
#'   * | **Column** | **Type** |
#'     | --- | --- |
#'     | subtype_id | integer |
#'     | type_id | integer |
#'     | subtype_name | character |
#'     | quantity | integer |
#'
#' * **Group Table**
#'   * | **Column** | **Type** |
#'     | --- | --- |
#'     | group_id | integer |
#'     | group_name | character |
#'
#' * **Group Type Table**
#'   * | **Column** | **Type** |
#'     | --- | --- |
#'     | group_id | integer |
#'     | type_id | integer |
#'
#' * **Circulation Table**
#'   * | **Column** | **Type** |
#'     | --- | --- |
#'     | name | character |
#'     | type_id | integer |
#'     | subtype_id | integer |
#'     | quantity | integer |
#'     | time | character |
#'
#' @md
#' @export
db_init <- function(path = "db/db.sqlite") {
  source_directory("./db/func")


  # Init DB
  db <- DBI::dbConnect(RSQLite::SQLite(), path)

  # create tables
  create_user_table(db)
  create_subtype_table(db)
  create_type_table(db)
  create_group_table(db)
  create_group_type_table(db)
  create_circulation_table(db)

  populate_user_table(db)

  DBI::dbDisconnect(db)
}



#' @export
create_user_table <- function(db) {
  DBI::dbExecute(
    db,
    "CREATE TABLE user (
      rowid INTEGER NOT NULL PRIMARY KEY,
      hash VARCHAR(255),
      name VARCHAR(255),
      status VARCHAR(255) CHECK(status IN ('admin', 'mod', 'user')),
      password VARCHAR(255),
      added_from INT,
      time_added VARCHAR(255),
      time_current_logged VARCHAR(255),
      time_previous_logged VARCHAR(255),
      times_logged INT CHECK(times_logged >= 0),
      removed INT CHECK(removed IN (0, 1))
    )"
  )

  DBI::dbExecute(
    db,
    "CREATE UNIQUE INDEX not_removed_names ON user(name) WHERE removed = 0"
  )
}



#' @export
populate_user_table <- function(db) {
  user_name <- c("Armin Admin", "Modesta Moderator", "Bernd Benutzer")
  user_status <- c("admin", "mod", "user")
  user_password <- c("admin", "mod", "user")
  user_password <- purrr::map_chr(user_password, ~ bcrypt::hashpw(.))

  purrr::pwalk(list(user_name, user_status, user_password), function(name, status, password) {
    db_add_user(db, name, status, password, added_from = 1L)
  })
}



#' @export
create_subtype_table <- function(db) {
  tbl <- tibble::tibble(
    type_id = integer(),
    subtype_name = character(),
    quantity = integer(),
    critical_quantity = integer(),
    removed = integer()
  )

  DBI::dbExecute(
    db,
    "CREATE TABLE subtype (
      rowid INTEGER NOT NULL PRIMARY KEY,
      type_id INT,
      subtype_name VARCHAR(255),
      quantity INT CHECK(quantity >= 0),
      critical_quantity INT CHECK(quantity >= 0),
      removed INT CHECK(removed IN (0, 1)),
      FOREIGN KEY(type_id) REFERENCES type(rowid)
    )"
  )

  DBI::dbExecute(
    db,
    "CREATE UNIQUE INDEX not_removed_subtype_names
    ON subtype(type_id, subtype_name)
    WHERE removed = 0"
  )
}



#' @export
create_type_table <- function(db) {
  DBI::dbExecute(
    db,
    "CREATE TABLE type (
      rowid INTEGER NOT NULL PRIMARY KEY,
      type_name VARCHAR(255),
      removed int CHECK(removed IN (0, 1))
    )"
  )

  DBI::dbExecute(
    db,
    "CREATE UNIQUE INDEX not_removed_type_names
    ON type(type_name) WHERE removed = 0"
  )
}



#' @export
create_group_table <- function(db) {
  DBI::dbExecute(
    db,
    "CREATE TABLE groups (
      rowid INTEGER NOT NULL PRIMARY KEY,
      group_name VARCHAR(255),
      removed int CHECK(removed IN (0, 1))
    )"
  )

  DBI::dbExecute(
    db,
    "CREATE UNIQUE INDEX not_removed_group_names
    ON groups(group_name) WHERE removed = 0"
  )
}



#' @export
create_group_type_table <- function(db) {
  DBI::dbExecute(
    db,
    "CREATE TABLE group_type (
      rowid INTEGER NOT NULL PRIMARY KEY,
      group_id INT,
      type_id INT,
      FOREIGN KEY(group_id) REFERENCES groups(rowid)
      FOREIGN KEY(type_id) REFERENCES type(rowid)
    )"
  )
}



#' @export
create_circulation_table <- function(db) {
  DBI::dbExecute(
    db,
    "CREATE TABLE circulation (
      rowid INTEGER NOT NULL PRIMARY KEY,
      user_id INT,
      subtype_id INT,
      quantity INT,
      time VARCHAR(255),
      op_type INT,
      FOREIGN KEY(user_id) REFERENCES user(rowid),
      FOREIGN KEY(subtype_id) REFERENCES subtype(rowid)
    )"
  )
}
