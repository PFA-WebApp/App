library(DBI)
library(RSQLite)

init_db <- function() {
  db <- DBI::dbConnect(RSQLite::SQLite(), "db/db.sqlite")

  DBI::dbDisconnect(db)
}
