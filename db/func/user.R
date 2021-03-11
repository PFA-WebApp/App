#' Add User to User Table
#'
#' Add a new user to the user table.
#'
#' @template db
#' @template xxx-name
#' @templateVar key user
#' @template user-status
#' @template user-password
#'
#' @family user
#'
#' @export
db_add_user <- function(db,
                        name,
                        status = c("admin", "mod", "user"),
                        password,
                        added_from
) {
  entry <- tibble::tibble(
    hash = bcrypt::hashpw(name),
    name = name,
    status = status,
    # Hashed password
    password = password,
    added_from = added_from,
    time_added = as.character(Sys.time()),
    time_current_logged = as.character(Sys.time()),
    time_previous_logged = as.character(Sys.time()),
    times_logged = 0,
    removed = 0
  )

  tryCatch(
    DBI::dbAppendTable(db, "user", entry),
    `Rcpp::exception` = function(e) {
      if (stringr::str_detect(e$message, "user.name")) {
        return(0)
      } else {
        stop(e)
      }
    }
  )
}



#' Get User ID
#'
#' @template db
#' @param user_name User name.
#'
#' @family user
#'
#' @export
db_get_user_id <- function(db, user_name) {
  DBI::dbGetQuery(
    db,
    "SELECT rowid FROM user WHERE name = ? AND removed = 0",
    params = list(user_name)
  )$rowid
}


#' Get User Name
#'
#' @template db
#' @param user_id User ID.
#'
#' @family user
#'
#' @export
db_get_user_name <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT name FROM user WHERE rowid = ?",
    params = list(user_id)
  )$name
}



#' Set User Name
#'
#' @template db
#' @param user_id User ID.
#' @param user_name User name.
#'
#' @family user
#'
#' @export
db_set_user_name <- function(db, user_id, user_name) {
  DBI::dbExecute(
    db,
    "UPDATE user SET name = ? WHERE rowid = ?",
    params = list(user_name, user_id)
  )
}



#' Get User Status
#'
#' @template db
#' @param user_id User ID.
#'
#' @family user
#'
#' @export
db_get_user_status <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT status FROM user WHERE rowid = ?",
    params = list(user_id)
  )$status
}



#' Set User Status
#'
#' @template db
#' @param user_id User Id.
#' @param status Status.
#'
#' @family user
#'
#' @export
db_set_user_status <- function(db, user_id, status) {
  current_status <- db_get_user_status(db, user_id)
  if (status != "admin" && current_status == "admin") {
    n_admins <- DBI::dbGetQuery(
      db,
      "SELECT COUNT(*) AS n_admins FROM user WHERE status = ?",
      params = list("admin")
    )$n_admins

    if (n_admins == 1) return(0)
  }

  DBI::dbExecute(
    db,
    "UPDATE user SET status = ? WHERE rowid = ?",
    params = list(status, user_id)
  )
}


#' Get User Names and IDs
#'
#' @template db
#'
#' @family user
#'
#' @export
db_get_users <- function(db, include_removed = FALSE) {
  tbl <- if (include_removed) {
    DBI::dbGetQuery(db, "SELECT rowid, name FROM user")
  } else {
    DBI::dbGetQuery(db, "SELECT rowid, name FROM user WHERE removed = 0")
  }

  x <- tbl$rowid
  names(x) <- tbl$name

  x
}



#' Remove User from User Table
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_remove_user <- function(db, user_id) {
  status <- db_get_user_status(db, user_id)

  if (status == "admin") {
    n_admins <- DBI::dbGetQuery(
      db,
      "SELECT COUNT(*) AS n_admins FROM user WHERE status = ?",
      params = list("admin")
    )$n_admins

    if (n_admins == 1) return(0)
  }

  DBI::dbExecute(
    db,
    "UPDATE user SET removed = 1 WHERE rowid = ?",
    params = list(user_id)
  )
}



#' Get Hashed Password From User
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_get_password <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT password FROM user WHERE rowid = ?",
    params = list(user_id)
  )$password
}



#' Set Hashed Password
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_set_password <- function(db, user_id, password) {
  DBI::dbExecute(
    db,
    "UPDATE user SET password = ? WHERE rowid = ?",
    params = list(password, user_id)
  )
}



#' Check If User Table Has User Name
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_has_user_name <- function(db, name) {
  name %in% names(db_get_users(db))
}



#' Log User In
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_log_user_in <- function(db, user_id) {
  DBI::dbExecute(
    db,
    "UPDATE user SET time_current_logged = ? WHERE rowid = ?",
    params = list(as.character(Sys.time()), user_id)
  )

  times_logged <- db_get_user_times_logged(db, user_id)

  DBI::dbExecute(
    db,
    "UPDATE user SET times_logged = ? WHERE rowid = ?",
    params = list(times_logged + 1, user_id)
  )
}



#' Log User Out
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_log_user_out <- function(db, user_id) {
  DBI::dbExecute(
    db,
    "UPDATE user SET time_previous_logged = time_current_logged WHERE rowid = ?",
    params = list(user_id)
  )
}



#' Get Number of Times a User Logged In
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_get_user_times_logged <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT times_logged FROM user WHERE rowid = ?",
    params = list(user_id)
  )$times_logged
}



#' Get Name of User Who Added User
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_get_adding_user <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT added_from FROM user WHERE rowid = ?",
    params = list(user_id)
  )$added_from
}



#' Get Time When User Logged
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_get_user_time_logged <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT time_current_logged FROM user WHERE rowid = ?",
    params = list(user_id)
  )$time_current_logged
}



#' Get Time When User Last Logged
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_get_user_last_logged <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT time_previous_logged FROM user WHERE rowid = ?",
    params = list(user_id)
  )$time_previous_logged
}



#' Get User Hash
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_get_hash <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT hash FROM user WHERE rowid = ?",
    params = list(user_id)
  )$hash
}



#' Get User ID by Hash
#'
#' @inheritParams db_add_user
#'
#' @family user
#'
#' @export
db_get_user_id_by_hash <- function(db, hash) {
  DBI::dbGetQuery(
    db,
    "SELECT rowid FROM user WHERE hash = ?",
    params = list(hash)
  )$rowid
}
