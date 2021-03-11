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
      name VARCHAR(255) UNIQUE,
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
      FOREIGN KEY(type_id) REFERENCES type(rowid),
      UNIQUE(type_id, subtype_name)
    )"
  )
}



#' @export
create_type_table <- function(db) {
  DBI::dbExecute(
    db,
    "CREATE TABLE type (
      rowid INTEGER NOT NULL PRIMARY KEY,
      type_name VARCHAR(255) UNIQUE,
      removed int CHECK(removed IN (0, 1))
    )"
  )
}



#' @export
create_group_table <- function(db) {
  tbl <- tibble::tibble(
    group_name = character(),
    removed = integer()
  )

  DBI::dbCreateTable(db, "groups", tbl)
}



#' @export
create_group_type_table <- function(db) {
  tbl <- tibble::tibble(
    group_id = integer(),
    type_id = integer()
  )

  DBI::dbCreateTable(db, "group_type", tbl)
}



#' @export
create_circulation_table <- function(db) {
  tbl <- tibble::tibble(
    user_id = integer(),
    subtype_id = integer(),
    quantity = integer(),
    time = character(),
    op_type = integer()
  )

  DBI::dbCreateTable(db, "circulation", tbl)
}
