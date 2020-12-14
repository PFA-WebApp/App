init_db <- function() {
  # Init DB
  db <- DBI::dbConnect(RSQLite::SQLite(), "db/db.sqlite")

  # create tables
  create_user_table(db)
  create_subtype_table(db)
  create_type_table(db)
  create_group_table(db)
  create_group_type_table(db)
  create_circulation_table(db)

  DBI::dbDisconnect(db)
}

create_user_table <- function(db) {
  tbl <- tibble::tibble(
    name = character(),
    status = character(),
    password = character()
  )

  DBI::dbCreateTable(db, "user", tbl)
}

create_subtype_table <- function(db) {
  tbl <- tibble::tibble(
    type_id = integer(),
    subtype_id = integer(),
    subtype_name = character(),
    quantity = integer()
  )

  DBI::dbCreateTable(db, "subtype", tbl)
}

create_type_table <- function(db) {
  tbl <- tibble::tibble(
    type_id = integer(),
    type_name = character()
  )

  DBI::dbCreateTable(db, "type", tbl)
}

create_group_table <- function(db) {
  tbl <- tibble::tibble(
    group_id = integer(),
    group_name = character()
  )

  DBI::dbCreateTable(db, "group", tbl)
}

create_group_type_table <- function(db) {
  tbl <- tibble::tibble(
    group_id = integer(),
    type_id = integer()
  )

  DBI::dbCreateTable(db, "group_type", tbl)
}

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
