box::use(
  DBI[
    dbBind,
    dbFetch,
    dbConnect,
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
#' @return data.frame
#' @export
read_all_contacts <- \(
  first_name_pattern = "",
  last_name_pattern = "",
  phone_number_pattern = "",
  email_address_pattern = ""
) {
  patterns <- list(
    first_name = first_name_pattern,
    last_name = last_name_pattern,
    phone_number = phone_number_pattern,
    email_address = email_address_pattern
  )

  where_conditions <- NULL
  where_values <- list()
  for (i in seq_along(patterns)) {
    name <- names(patterns)[[i]]
    value <- patterns[[i]]

    if (identical(value, "")) {
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

  res <- dbSendQuery(conn = conn, statement = query)
  if (!is.null(where_conditions)) {
    dbBind(res = res, params = where_values)
  }
  found <- dbFetch(res = res)
  dbClearResult(res)

  found
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
