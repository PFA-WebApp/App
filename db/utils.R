db_length <- function(db, name) {
  DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM ?", params = list(name))$n
}

db_get_table <- function(db, name) {
  DBI::dbReadTable(db, name)
}
