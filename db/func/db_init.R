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
  tbl <- tibble::tibble(
    name = character(),
    status = character(),
    password = character(),
    added_from = character(),
    time_added = character(),
    time_logged = character(),
    times_logged = integer()
  )

  DBI::dbCreateTable(db, "user", tbl)
}



#' @export
populate_user_table <- function(db) {
  user_name <- c("Admin", "Moderator", "Benutzer")
  user_status <- c("admin", "mod", "user")
  user_password <- c("admin", "mod", "user")
  user_password <- purrr::map_chr(user_password, ~ bcrypt::hashpw(.))

  purrr::pwalk(list(user_name, user_status, user_password), function(name, status, password) {
    db_add_user(db, name, status, password)
  })
}



#' @export
create_subtype_table <- function(db) {
  tbl <- tibble::tibble(
    type_id = integer(),
    subtype_name = character(),
    quantity = integer()
  )

  DBI::dbCreateTable(db, "subtype", tbl)
}



#' @export
create_type_table <- function(db) {
  tbl <- tibble::tibble(
    type_name = character()
  )

  DBI::dbCreateTable(db, "type", tbl)
}



#' @export
create_group_table <- function(db) {
  tbl <- tibble::tibble(
    group_name = character()
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
    name = character(),
    type_id = integer(),
    subtype_id = integer(),
    quantity = integer(),
    time = character()
  )

  DBI::dbCreateTable(db, "circulation", tbl)
}
