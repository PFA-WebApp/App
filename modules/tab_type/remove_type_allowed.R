remove_type_allowed <- function(db, type_id) {
  allowed <- TRUE

  borrowed_subtypes <- db_get_borrowed_quantity(
    db,
    subtype_id = db_get_subtypes_by_type_id(db, type_id)
  )

  if (any(borrowed_subtypes > 0)) {
    allowed <- FALSE

    shiny::showNotification(
      ui = "Der Typ kann nicht gelöscht werden, solange Elemente eines Untertypen
      ausgeliehen sind",
      type = "error"
    )
  }

  allowed
}
