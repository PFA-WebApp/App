remove_subtype_allowed <- function(db, subtype_id) {
  type_id <- db_get_type_id_by_subtype_id(db, subtype_id)
  n <- length(db_get_subtypes_by_type_id(db, type_id))

  if (n > 1) return(TRUE)

  shiny::showNotification(
    ui = "Es wird mindestens ein Untertyp pro Typ benötigt.",
    type = "error"
  )

  return(FALSE)
}
