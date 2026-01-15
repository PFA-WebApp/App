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
  create_type_table(db)
  create_subtype_table(db)
  create_group_table(db)
  create_group_type_table(db)
  create_circulation_table(db)

  populate_user_table(db)
  populate_type_table(db)
  populate_subtype_table(db)

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
  user_name <- c("Alex Admin", "Morgan Moderator", "Ulysses User")
  user_status <- c("admin", "mod", "user")
  user_password <- c("admin", "mod", "user")
  user_password <- purrr::map_chr(user_password, ~ bcrypt::hashpw(.))

  purrr::pwalk(list(user_name, user_status, user_password), function(name, status, password) {
    db_add_user(db, name, status, password, added_from = 1L)
  })
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
populate_type_table <- function(db, overwrite = FALSE, append = FALSE) {
  type_table <- tibble::as_tibble_col(c(
    "Screwdriver",
    "Hammer",
    "Pliers",
    "Measuring Tape",
    "Cordless Drill",
    "Safety Gloves",
    "Safety Goggles",
    "Extension Cable",
    "Multimeter",
    "Label Maker"
  ), column_name = "type_name") |>
    dplyr::mutate(removed = 0L)

  DBI::dbWriteTable(
    db,
    "type",
    type_table,
    overwrite = overwrite,
    append = append
  )
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
populate_subtype_table <- function(db, overwrite = FALSE, append = FALSE) {
  subtype_table <- tibble::tribble(
    ~type_name, ~subtype_name, ~quantity, ~critical_quantity, ~removed,
    # Screwdriver
    "Screwdriver", "Phillips #2 - Wera", 18, 5, 0,
    "Screwdriver", "Flathead 5.5mm - Stanley", 12, 4, 0,
    "Screwdriver", "Precision set (6 pcs) – iFixit", 6, 2, 0,
    # Hammer
    "Hammer", "Claw hammer 16oz – Estwing", 7, 2, 0,
    "Hammer", "Rubber mallet – Tekton", 5, 2, 0,
    # Pliers
    "Pliers", "Needle-nose pliers 6\" – Knipex", 9, 3, 0,
    "Pliers", "Slip-joint pliers 8\" – Irwin", 8, 3, 0,
    # Measuring Tape
    "Measuring Tape", "5m tape – Stanley PowerLock", 14, 5, 0,
    "Measuring Tape", "8m tape – Milwaukee", 6, 2, 0,
    # Cordless Drill
    "Cordless Drill", "18V drill/driver – Makita (2 batteries)", 4, 1, 0,
    "Cordless Drill", "20V drill/driver – DeWalt (tool only)", 3, 1, 0,
    # Safety Gloves
    "Safety Gloves", "Nitrile gloves (box of 100) – Medium", 10, 3, 0,
    "Safety Gloves", "Cut-resistant gloves – Large", 8, 3, 0,
    # Safety Goggles
    "Safety Goggles", "Clear safety goggles – Anti-fog", 15, 5, 0,
    "Safety Goggles", "Over-glasses safety goggles", 6, 2, 0,
    # Extension Cable
    "Extension Cable", "Extension cable 10m – Heavy duty", 5, 2, 0,
    "Extension Cable", "Extension cable 25m – Outdoor rated", 3, 1, 0,
    # Multimeter
    "Multimeter", "Digital multimeter – Fluke 117", 2, 1, 0,
    "Multimeter", "Basic multimeter – UNI-T UT33", 6, 2, 0,
    # Label Maker
    "Label Maker", "Label maker – Brother P-touch", 2, 1, 0,
    "Label Maker", "Label tape 12mm – Black on white", 12, 4, 0
  )

  type_table <- db_get_table(db, "type", include_rowid = TRUE) |>
    dplyr::rename(type_id = rowid) |>
    dplyr::select(-removed)

  subtype_table_2 <- subtype_table |>
    dplyr::left_join(type_table, dplyr::join_by(type_name)) |>
    dplyr::select(-type_name)

  DBI::dbWriteTable(
    db,
    "subtype",
    subtype_table_2,
    overwrite = overwrite,
    append = append
  )
  #   -- Measuring Tape
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, '5m tape – Stanley PowerLock', 14, 5, 0 FROM \"type\" WHERE type_name = 'Measuring Tape';
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, '8m tape – Milwaukee', 6, 2, 0 FROM \"type\" WHERE type_name = 'Measuring Tape';
  #
  #   -- Cordless Drill
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, '18V drill/driver – Makita (with 2 batteries)', 4, 1, 0 FROM \"type\" WHERE type_name = 'Cordless Drill';
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, '20V drill/driver – DeWalt (tool only)', 3, 1, 0 FROM \"type\" WHERE type_name = 'Cordless Drill';
  #
  #   -- Safety Gloves
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Nitrile gloves (box of 100) – Medium', 10, 3, 0 FROM \"type\" WHERE type_name = 'Safety Gloves';
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Cut-resistant gloves – Large', 8, 3, 0 FROM \"type\" WHERE type_name = 'Safety Gloves';
  #
  #   -- Safety Goggles
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Clear safety goggles – Anti-fog', 15, 5, 0 FROM \"type\" WHERE type_name = 'Safety Goggles';
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Over-glasses safety goggles', 6, 2, 0 FROM \"type\" WHERE type_name = 'Safety Goggles';
  #
  #   -- Extension Cable
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Extension cable 10m – Heavy duty', 5, 2, 0 FROM \"type\" WHERE type_name = 'Extension Cable';
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Extension cable 25m – Outdoor rated', 3, 1, 0 FROM \"type\" WHERE type_name = 'Extension Cable';
  #
  #   -- Multimeter
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Digital multimeter – Fluke 117', 2, 1, 0 FROM \"type\" WHERE type_name = 'Multimeter';
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Basic multimeter – UNI-T UT33', 6, 2, 0 FROM \"type\" WHERE type_name = 'Multimeter';
  #
  #   -- Label Maker
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Label maker – Brother P-touch', 2, 1, 0 FROM \"type\" WHERE type_name = 'Label Maker';
  #   INSERT INTO subtype (type_id, subtype_name, quantity, critical_quantity, removed)
  #   SELECT rowid, 'Label tape 12mm – Black on white', 12, 4, 0 FROM \"type\" WHERE type_name = 'Label Maker';
  #   COMMIT;
  #   "
  # )
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
