map_ui <- function(id,
                   object_ids,
                   ui_func,
                   ui_args = NULL
) {
  purrr::map(object_ids, function(object_id) {
    # ui_args, that are functions are evaluated with their object_id
    for (i in seq_along(ui_args)) {
      if (is.function(ui_args[[i]])) ui_args[[i]] <- ui_args[[i]](object_id)
    }

    do.call(
      ui_func,
      c(
        list(
          id = id,
          object_id = object_id
        ),
        ui_args
      )
    )
  }
  )
}
