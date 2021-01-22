firstup <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

map_ui <- function(object_ids,
                   ui_func,
                   server_func,
                   rvs,
                   rvs_slot,
                   ns,
                   id_prefix = "object",
                   ui_args = NULL,
                   server_args = NULL
) {
  purrr::map(object_ids, function(object_id) {
      if (!object_id %in% rvs[[rvs_slot]]) {
        rvs[[rvs_slot]] <- c(
          rvs[[rvs_slot]], object_id
        )

        for (i in seq_along(server_args)) {
          if (is.function(server_args[[i]])) server_args[[i]] <- server_args[[i]](object_id)
        }

        do.call(
          server_func,
          c(
            list(
              id = id_prefix %_% object_id
            ),
            server_args
          )
        )
      }

      for (i in seq_along(ui_args)) {
        if (is.function(ui_args[[i]])) ui_args[[i]] <- ui_args[[i]](object_id)
      }

      do.call(
        ui_func,
        c(
          list(
            id = ns(id_prefix %_% object_id)
          ),
          ui_args
        )
      )
    }
  )
}
