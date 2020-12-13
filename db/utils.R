db_length <- function(db, name) {
  DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM ?", params = list(name))$n
}
