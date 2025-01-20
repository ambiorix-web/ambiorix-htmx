box::use(
  ambiorix[Ambiorix],
  . / utils[parse_req],
  . / store[
    home_page,
    contacts_table,
  ],
  . / db[
    seed_db,
    read_contact,
    create_contact,
    update_contact,
    delete_contact,
    read_all_contacts,
  ]
)

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
  page <- as.integer(req$query$page)
  data <- read_all_contacts(page = page)
  html <- contacts_table(
    data = data$filtered,
    next_page = data$next_page,
    type = "records"
  )

  # simulate a short delay so that spinner can be seen, lol:
  Sys.sleep(0.5)

  res$send(html)
}

#' Handle POST at '/contacts'
#'
#' @export
contact_post <- \(req, res) {
  new_field_names <- c("first_name", "last_name", "phone_number", "email_address")
  field_names <- paste0("new_contact_", new_field_names)

  body <- parse_req(
    req = req,
    fields_to_extract = field_names,
    new_field_names = new_field_names
  )

  create_contact(
    first_name = body[["first_name"]],
    last_name = body[["last_name"]],
    phone_number = body[["phone_number"]],
    email_address = body[["email_address"]]
  )

  data <- read_all_contacts(page = 1L)
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

Ambiorix$new(port = 5000L)$
  set_error(error_handler)$
  static("public", "assets")$
  get("/", home_get)$
  get("/contacts", contact_get)$
  post("/contacts", contact_post)$
  start()
