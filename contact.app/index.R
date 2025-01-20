box::use(
  ambiorix[Ambiorix],
  . / utils[parse_req],
  . / store[
    home_page,
    contacts_table,
  ],
  . / db[
    seed_db,
    delete_db,
    read_contact,
    create_contact,
    update_contact,
    delete_contact,
    read_all_contacts,
  ]
)

# seed the database:
seed_db()

#' Handle GET at '/'
#'
#' @export
home_get <- \(req, res) {
  res$send(home_page())
}

#' Handle GET at '/contacts'
#'
#' @export
contact_get <- \(req, res) {
  query <- req$query
  page <- as.integer(query$page)

  data <- read_all_contacts(
    first_name_pattern = query[["first_name_pattern"]],
    last_name_pattern = query[["last_name_pattern"]],
    phone_number_pattern = query[["phone_number_pattern"]],
    email_address_pattern = query[["email_address_pattern"]],
    page = page
  )

  html <- contacts_table(
    data = data$filtered,
    next_page = data$next_page,
    type = "records"
  )

  # simulate a short delay so that spinner can be seen, lol:
  Sys.sleep(0.3)

  res$send(html)
}

#' Handle POST at '/contacts'
#'
#' @export
contact_post <- \(req, res) {
  new_field_names <- c("first_name", "last_name", "phone_number", "email_address")
  patterns <- paste0(new_field_names, "_pattern")
  fields_to_extract <- c(
    paste0("new_contact_", new_field_names),
    patterns
  )

  body <- parse_req(
    req = req,
    fields_to_extract = fields_to_extract,
    new_field_names = c(new_field_names, patterns)
  )

  create_contact(
    first_name = body[["first_name"]],
    last_name = body[["last_name"]],
    phone_number = body[["phone_number"]],
    email_address = body[["email_address"]]
  )

  data <- read_all_contacts(
    first_name_pattern = body[["first_name_pattern"]],
    last_name_pattern = body[["last_name_pattern"]],
    phone_number_pattern = body[["phone_number_pattern"]],
    email_address_pattern = body[["email_address_pattern"]],
    page = 1L
  )
  html <- contacts_table(
    data = data$filtered,
    next_page = data$next_page,
    type = "full"
  )

  res$send(html)
}

#' Handle PUT at '/contacts/:id'
#'
#' @export
contact_put <- \(req, res) {
  id <- req$params$id
  new_field_names <- c("first_name", "last_name", "phone_number", "email_address")
  patterns <- paste0(new_field_names, "_pattern")

  fields_to_extract <- c(
    paste0("edit_contact_", new_field_names, "_", id),
    patterns
  )

  body <- parse_req(
    req = req,
    fields_to_extract = fields_to_extract,
    new_field_names = c(new_field_names, patterns)
  )

  update_contact(
    id = id,
    new_first_name = body[["first_name"]],
    new_last_name = body[["last_name"]],
    new_phone_number = body[["phone_number"]],
    new_email_address = body[["email_address"]]
  )

  data <- read_all_contacts(
    first_name_pattern = body[["first_name_pattern"]],
    last_name_pattern = body[["last_name_pattern"]],
    phone_number_pattern = body[["phone_number_pattern"]],
    email_address_pattern = body[["email_address_pattern"]],
    page = 1L
  )
  html <- contacts_table(
    data = data$filtered,
    next_page = data$next_page,
    type = "full"
  )

  res$send(html)
}

#' Handle DELETE at '/contacts/:id'
#'
#' @export
contact_delete <- \(req, res) {
  id <- req$params$id
  query <- req$query

  delete_contact(id = id)

  data <- read_all_contacts(
    first_name_pattern = query[["first_name_pattern"]],
    last_name_pattern = query[["last_name_pattern"]],
    phone_number_pattern = query[["phone_number_pattern"]],
    email_address_pattern = query[["email_address_pattern"]],
    page = 1L
  )
  html <- contacts_table(
    data = data$filtered,
    next_page = data$next_page,
    type = "full"
  )

  res$send(html)
}

#' Handle POST at '/search-contacts'
#'
#' @export
contact_search <- \(req, res) {
  patterns <- paste0(
    c("first_name", "last_name", "phone_number", "email_address"),
    "_pattern"
  )

  body <- parse_req(req = req, fields_to_extract = patterns)

  data <- read_all_contacts(
    first_name_pattern = body[["first_name_pattern"]],
    last_name_pattern = body[["last_name_pattern"]],
    phone_number_pattern = body[["phone_number_pattern"]],
    email_address_pattern = body[["email_address_pattern"]],
    page = 1L
  )
  html <- contacts_table(
    data = data$filtered,
    next_page = data$next_page,
    type = "full"
  )

  res$send(html)
}

#' Global error handler for app
#'
#' @param req The request object.
#' @param res The response object.
#' @param error The error object. See [stop()].
#' @export
error_handler <- \(req, res, error) {
  message(conditionMessage(error))
  res$status <- 500L
  res$send("Internal Server Error :(")
}

app <- Ambiorix$new(port = 5000L)$
  set_error(error_handler)$
  static("public", "assets")$
  get("/", home_get)$
  get("/contacts", contact_get)$
  post("/contacts", contact_post)$
  put("/contacts/:id", contact_put)$
  delete("/contacts/:id", contact_delete)$
  post("/search-contacts", contact_search)

# delete the db when server closes:
app$on_stop <- \() {
  delete_db()
}

app$start()
