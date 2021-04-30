remove_subtype_allowed <- function(db, subtype_id) {
  type_id <- db_get_type_id_by_subtype_id(db, subtype_id)
  n <- length(db_get_subtypes_by_type_id(db, type_id))

  allowed <- TRUE

  if (n == 1) {
    allowed <- FALSE

    shiny::showNotification(
      ui = .values$i18n$t("err_at_least_one_subtype_per_type_required"),
      type = "error"
    )
  }

  if (db_get_borrowed_quantity(db, subtype_id) > 0) {
    allowed <- FALSE

    shiny::showNotification(
      ui = .values$i18n$t("err_subtype_borrowed"),
      type = "error"
    )
  }

  allowed
}
