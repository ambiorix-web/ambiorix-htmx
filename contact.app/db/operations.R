box::use(
  DBI[
    dbBind,
    dbFetch,
    dbConnect,
    dbExecute,
    dbSendQuery,
    dbWriteTable,
    dbDisconnect,
    dbClearResult,
  ],
  generator[
    r_full_names,
    r_phone_numbers,
    r_email_addresses,
  ],
  RSQLite[SQLite],
  uuid[UUIDgenerate],
)

#' Generate fake data to use and seed it into the database
#'
#' @param n Integer. Number of rows to use for the fake
#' data. Default is 500.
#' @return `NULL`, invisibly.
#' @export
seed_db <- \(n = 500L) {
  set.seed(13L)
  nms <- r_full_names(n) |>
    strsplit(split = " ") |>
    lapply(as.list) |>
    lapply(as.data.frame, col.names = c("first_name", "last_name")) |>
    do.call(what = "rbind")

  set.seed(13L)
  contacts <- data.frame(
    id = UUIDgenerate(n = n),
    nms,
    phone_number = r_phone_numbers(n),
    email_address = r_email_addresses(n)
  )

  conn <- make_conn()
  on.exit(dbDisconnect(conn))

  dbWriteTable(
    conn = conn,
    name = "contacts",
    value = contacts,
    overwrite = TRUE,
    append = FALSE
  )

  invisible()
}

#' Delete the database
#'
#' @return [unlink()]
#' @export
delete_db <- \() {
  unlink(x = "contacts.sqlite")
}

#' Make connection to the database
#'
#' @return [DBI::dbConnect()]
#' @export
make_conn <- \() {
  dbConnect(drv = SQLite(), dbname = "contacts.sqlite")
}

#' Read all contacts
#'
#' Reads all contacts from the database which match the
#' specified pattern(s).
#' @param first_name_pattern String. Pattern to search for
#' in the first name.
#' @param last_name_pattern String. Pattern to search for
#' in the last name.
#' @param phone_number_pattern String. Pattern to search for
#' in the phone number.
#' @param email_address_pattern String. Pattern to search for
#' in the email address.
#' @param page Integer. Page number to read. Default is 1.
#' @parm page_length Integer. How many records per page?
#' Default is 15.
#' @return Named list:
#' - `filtered`: data.frame. The filtered records.
#' - `next_page`: Integer. Next page. `NULL` if at the end
#' of records.
#' @export
read_all_contacts <- \(
  first_name_pattern = "",
  last_name_pattern = "",
  phone_number_pattern = "",
  email_address_pattern = "",
  page = 1L,
  page_length = 15L
) {
  patterns <- list(
    first_name = first_name_pattern,
    last_name = last_name_pattern,
    phone_number = phone_number_pattern,
    email_address = email_address_pattern
  )

  where_conditions <- NULL
  where_values <- list()
  where_clause <- NULL

  for (i in seq_along(patterns)) {
    name <- names(patterns)[[i]]
    value <- patterns[[i]]

    is_empty <- identical(value, "") || identical(length(value), 0L)
    if (is_empty) {
      next()
    }

    statement <- paste0(name, " LIKE ?")
    where_conditions <- c(where_conditions, statement)
    where_values <- append(
      x = where_values,
      values = paste0("%", value, "%")
    )
  }

  query <- "
    SELECT
      id,
      first_name,
      last_name,
      phone_number,
      email_address
    FROM contacts
    "

  if (!is.null(where_conditions)) {
    where_clause <- paste(
      "WHERE",
      paste(where_conditions, collapse = " AND ")
    )
    query <- paste(query, where_clause)
  }

  conn <- make_conn()
  on.exit(dbDisconnect(conn))

  # pagination
  count_query <- paste(
    "SELECT COUNT(*) AS n FROM contacts",
    where_clause
  )
  res <- dbSendQuery(conn = conn, statement = count_query)
  if (!is.null(where_conditions)) {
    dbBind(res = res, params = where_values)
  }
  found <- dbFetch(res = res)
  dbClearResult(res)
  n <- as.integer(found[["n"]])

  start_idx <- (page - 1L) * page_length + 1L
  if (start_idx > n) {
    return(
      list(
        filtered = NULL,
        next_page = NULL
      )
    )
  }

  end_idx <- min(page * page_length, n) |> as.integer()
  is_last_page <- identical(end_idx, n) || n <= page_length
  next_page <- if (!is_last_page) page + 1L

  limit <- page_length
  offset <- start_idx - 1L
  query <- paste(query, "LIMIT", limit, "OFFSET", offset)

  res <- dbSendQuery(conn = conn, statement = query)
  if (!is.null(where_conditions)) {
    dbBind(res = res, params = where_values)
  }
  found <- dbFetch(res = res)
  dbClearResult(res)

  row.names(found) <- as.character(
    seq_len(nrow(found)) + offset
  )

  list(
    filtered = found,
    next_page = next_page
  )
}

#' Create new contact
#'
#' @param first_name String.
#' @param last_name String.
#' @param phone_number String.
#' @param email_address String.
#' @return data.frame containing details
#' of the new contact.
#' @export
create_contact <- \(
  first_name,
  last_name,
  phone_number,
  email_address
) {
  id <- UUIDgenerate(n = 1L)

  record <- data.frame(
    id = id,
    first_name = first_name,
    last_name = last_name,
    phone_number = phone_number,
    email_address = email_address
  )

  conn <- make_conn()
  on.exit(dbDisconnect(conn))

  dbWriteTable(
    conn = conn,
    name = "contacts",
    value = record,
    overwrite = FALSE,
    append = TRUE
  )

  read_contact(id = id)
}

#' Read a specific contact
#'
#' @param id String. Contact ID.
#' @return data.frame
#' @export
read_contact <- \(id) {
  params <- list(id)

  query <- "
    SELECT
      id,
      first_name,
      last_name,
      phone_number,
      email_address
    FROM contacts
    WHERE id = ?
    "

  conn <- make_conn()
  on.exit(dbDisconnect(conn))

  res <- dbSendQuery(conn = conn, statement = query)
  dbBind(res = res, params = params)
  found <- dbFetch(res = res)
  dbClearResult(res)

  found
}

#' Update an existing contact
#'
#' @param id String. Contact ID.
#' @param new_first_name String.
#' @param new_last_name String.
#' @param new_phone_number String.
#' @param new_email_address String.
#' @return data.frame containing details of
#' the updated contact.
#' @export
update_contact <- \(
  id,
  new_first_name = NULL,
  new_last_name = NULL,
  new_phone_number = NULL,
  new_email_address = NULL
) {
  set_statements <- c(
    if (!is.null(new_first_name)) {
      "first_name = ?"
    },
    if (!is.null(new_last_name)) {
      "last_name = ?"
    },
    if (!is.null(new_phone_number)) {
      "phone_number = ?"
    },
    if (!is.null(new_email_address)) {
      "email_address = ?"
    }
  )

  if (is.null(set_statements)) {
    return(
      read_contact(id = id)
    )
  }

  set_clause <- paste(
    "SET",
    paste(set_statements, collapse = ", ")
  )

  query <- "
    UPDATE contacts
    WHERE id = ?
    "
  query <- paste("UPDATE contacts", set_clause, "WHERE id = ?")

  params <- c(
    new_first_name,
    new_last_name,
    new_phone_number,
    new_email_address,
    id
  ) |>
    as.list()

  conn <- make_conn()
  on.exit(dbDisconnect(conn))

  dbExecute(conn = conn, statement = query, params = params)

  read_contact(id = id)
}

#' Delete a contact
#'
#' @param id Contact ID.
#' @return data.frame containing details of
#' the deleted contact.
#' @export
delete_contact <- \(id) {
  details <- read_contact(id = id)

  not_found <- identical(nrow(details), 0L)
  if (not_found) {
    return(details)
  }

  query <- "DELETE FROM contacts WHERE id = ?"
  params <- list(id)

  conn <- make_conn()
  on.exit(dbDisconnect(conn))

  dbExecute(conn = conn, statement = query, params = params)

  details
}
