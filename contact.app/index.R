box::use(
  ambiorix[Ambiorix],
  generator[
    r_full_names,
    r_phone_numbers,
    r_email_addresses,
  ],
  tools[toTitleCase],
  htmltools[tags, tagList],
  . / utils[parse_req],
)

# generate some fake data to use:
n <- 500L
set.seed(13L)
nms <- r_full_names(n) |>
  strsplit(split = " ") |>
  lapply(as.list) |>
  lapply(as.data.frame, col.names = c("first_name", "last_name")) |>
  do.call(what = "rbind")

set.seed(13L)
contacts <- data.frame(
  nms,
  phone_number = r_phone_numbers(n),
  email_address = r_email_addresses(n)
)

#' Filter rows from `contacts` with pagination & pattern matching
#'
#' @param page Integer. Page number to retrieve. Default
#' is 1.
#' @param page_length Integer. Number of rows per page. Default
#' is 15.
#' @param first_name_pattern String.
#' @param last_name_pattern String.
#' @param phone_number_pattern String.
#' @param email_address_pattern String.
#' @return Named list:
#' - `filtered`: data.frame object containing the rows for
#' the requested page.
#' - `next_page`: The next page number as an integer, or `NULL`
#' if the current page is the last one.
#' @examples
#' # default:
#' filter_contacts()
#'
#' # retrieve the second page of results, with 10 rows per page:
#' filter_contacts(page = 2L, page_length = 10L)
#'
#' # attempt to retrieve a page beyond the available data:
#' filter_contacts(page = 1000000)
#' @export
filter_contacts <- \(
  first_name_pattern = "",
  last_name_pattern = "",
  phone_number_pattern = "",
  email_address_pattern = "",
  page = 1L,
  page_length = 15L
) {
  grepl(pattern = first_name_pattern, x = contacts[["first_name"]])
  filtered <- contacts[]
  start_idx <- (page - 1L) * page_length + 1L
  if (start_idx > n) {
    empty <- list(
      filtered = contacts[integer(), ],
      next_page = NULL
    )
    return(empty)
  }

  end_idx <- min(page * page_length, n)
  is_last_page <- identical(end_idx, n)
  next_page <- if (!is_last_page) page + 1L

  row_idx <- seq(from = start_idx, to = end_idx, by = 1L)

  list(
    filtered = contacts[row_idx, ],
    next_page = next_page
  )
}

#' Add a new contact
#'
#' Adds a new contact with the provided details
#' and modifies the object 'contacts' globally.
#'
#' @param first_name String. First name.
#' @param last_name String. Last name.
#' @param phone_number String. Phone number.
#' @param email_address String. Email address.
#' @return data.frame. The newly added record.
#' @export
add_new_contact <- \(
  first_name,
  last_name,
  phone_number,
  email_address
) {
  record <- data.frame(
    first_name = first_name,
    last_name = last_name,
    phone_number = phone_number,
    email_address = email_address
  )

  contacts <<- rbind(contacts, record)

  record
}


#' Create a generic HTML page
#'
#' @param head [htmltools::tagList()] Tag list containing
#' items to include in the head of the HTML document.
#' @param body [htmltools::tagList()] Tag list containing
#' items to include in the body of the HTML document.
#' @return [htmltools::tags$html]
#' @export
html_page <- \(head = NULL, body = NULL) {
  tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$title("Contact.app"),
      tags$link(
        rel = "stylesheet",
        href = "/assets/bootstrap-5.3.3.min.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "/assets/bootstrap-icons-1.11.3/bootstrap-icons.min.css"
      ),
      tags$script(src = "/assets/htmx-2.0.4.min.js"),
      tags$link(
        rel = "stylesheet",
        href = "/assets/styles.css"
      ),
      head
    ),
    tags$body(
      class = "raleway",
      `hx-encoding` = "multipart/form-data",
      body,
      tags$script(src = "/assets/bootstrap-5.3.3.bundle.min.js")
    )
  )
}

#' New contact form
#'
#' @export
new_contact_form <- \() {
  label <- tags$label(
    `for` = "n"
  )
  input <- tags$input(
    class = "form-control",
    type = "text",
  )

  labels <- c("First Name", "Last Name", "Phone Number", "Email Address")
  ids <- c("first_name", "last_name", "phone_number", "email_address")
  types <- c("text", "text", "tel", "email")
  placeholders <- c("John", "Doe", "+254712345678", "johndoe@mail.com")

  inputs <- Map(
    f = \(id, type, label, placeholder) {
      lbl <- tags$label(
        `for` = id,
        class = "form-label",
        label
      )

      input <- tags$input(
        class = "form-control",
        id = id,
        name = id,
        type = type,
        placeholder = placeholder
      )

      tags$div(
        class = "mb-3",
        lbl,
        input
      )
    },
    ids, types, labels, placeholders
  )

  btns <- tags$div(
    class = "d-flex justify-content-between",
    tags$button(
      type = "button",
      class = "btn btn-danger btn-sm",
      "Cancel"
    ),
    tags$button(
      type = "submit",
      class = "btn btn-success btn-sm",
      "Save Contact"
    )
  )

  tags$form(
    `hx-post` = "/contacts",
    `hx-target` = "#contacts_table",
    `hx-swap` = "outerHTML",
    `hx-on::after-request` = "this.reset()",
    inputs,
    btns
  )
}

#' Create search input fields for contacts table
#'
#' @export
create_search_fields <- \() {
  input_ids <- paste0(names(contacts), "_pattern")
  placeholders <- paste(
    "Search by",
    c(
      "first name",
      "last name",
      "phone number",
      "email address"
    )
  )

  inputs <- Map(
    f = \(id, placeholder) {
      hx_include <- paste0("#", setdiff(input_ids, id), collapse = ",")

      input <- tags$input(
        name = id,
        id = id,
        class = "form-control form-control-sm",
        type = "search",
        `hx-post` = "/search-contacts",
        `hx-include` = hx_include,
        `hx-target` = "#contacts_table",
        `hx-swap` = "outerHTML",
        `hx-trigger` = "input changed delay:500ms, search",
        placeholder = placeholder
      )

      tags$div(
        class = "col-12 col-md-6 col-lg-3",
        input
      )
    },
    input_ids,
    placeholders
  )

  tags$form(
    tags$div(
      class = "row",
      inputs
    )
  )
}

#' Create the contacts HTML table
#'
#' @param data data.frame object to use.
#' @param next_page Integer. The next page number.
#' Passed to `hx-get` of the last table row.
#' Default is NULL.
#' @param type Type of data to return. Either
#' "full" (default) to return the full html table,
#' or "records" to return the rows alone.
#' @return [htmltools::tags]
#' @export
contacts_table <- \(
  data,
  next_page = NULL,
  type = c("full", "records")
) {
  type <- match.arg(arg = type)

  nrows <- nrow(data)
  data[["Action"]] <- lapply(
    X = seq_len(nrows),
    FUN = \(row_idx) {
      tags$div(
        class = "btn-group btn-group-sm",
        role = "group",
        `aria-label` = "Action",
        tags$button(
          type = "button",
          class = "btn btn-outline-dark",
          tags$i(class = "bi bi-pencil-square"),
          "Edit"
        ),
        tags$button(
          type = "button",
          class = "btn btn-outline-danger",
          tags$i(class = "bi bi-trash3"),
          "Delete"
        )
      )
    }
  )

  ncols <- ncol(data)
  row_names <- rownames(data)

  table_records <- lapply(
    X = seq_len(nrows),
    FUN = \(row_idx) {
      cell_data <- lapply(
        X = seq_len(ncols),
        FUN = \(col_idx) {
          tags$td(
            data[[col_idx]][[row_idx]]
          )
        }
      )

      is_last_row <- identical(row_idx, nrows) && !is.null(next_page)
      hx_get <- if (is_last_row) paste0("/contacts?page=", next_page)

      tags$tr(
        class = "align-middle",
        `hx-get` = hx_get,
        `hx-trigger` = "revealed",
        `hx-swap` = "afterend",
        `hx-indicator` = "#contacts-loading-spinner",
        tags$th(
          scope = "row",
          row_names[[row_idx]]
        ),
        cell_data
      )
    }
  )

  if (identical(type, "records")) {
    return(table_records)
  }

  col_names <- c("#", names(data)) |>
    gsub(pattern = "_", replacement = " ") |>
    toTitleCase()

  table_head <- tags$thead(
    tags$tr(
      lapply(
        X = col_names,
        FUN = \(col_name) {
          tags$th(
            scope = "col",
            col_name
          )
        }
      )
    )
  )

  table_body <- tags$tbody(table_records)

  table <- tags$div(
    id = "contacts_table",
    class = "table-responsive",
    tags$table(
      class = "table table-sm table-hover table-bordered",
      table_head,
      table_body
    )
  )

  table
}

#' Contacts loading spinner
#'
#' @export
contacts_loading_spinner <- \() {
  tags$div(
    id = "contacts-loading-spinner",
    class = "d-flex justify-content-center htmx-indicator",
    tags$div(
      class = "spinner-border spinner-border-sm",
      role = "status",
      tags$span(
        class = "visually-hidden",
        "Loading..."
      )
    )
  )
}

#' Home page
#'
#' @export
home_page <- \() {
  first_fifteen <- filter_contacts()
  table <- contacts_table(
    data = first_fifteen$filtered,
    next_page = first_fifteen$next_page
  )

  body <- tagList(
    tags$header(
      class = "container",
      tags$h2("Contacts.app"),
      tags$p("Create, Read, Update & Delete Contacts")
    ),
    tags$main(
      class = "container",
      new_contact_form(),
      create_search_fields(),
      table,
      contacts_loading_spinner()
    ),
    tags$footer(
      class = "container",
      tags$p("ambiorix + htmx🚀")
    )
  )

  html_page(body = body)
}

#' Handle GET at '/'
#'
#' @export
home_get <- \(req, res) {
  res$send(home_page())
}

#' Handle GET at '/contacts'
#'
#' @export
get_contacts <- \(req, res) {
  page <- as.integer(req$query$page)
  data <- filter_contacts(page = page)
  html <- tagList(
    contacts_table(
      data = data$filtered,
      next_page = data$next_page,
      type = "records"
    )
  )

  # simulate a short delay so that spinner can be seen, lol:
  Sys.sleep(0.5)

  res$send(html)
}

#' Handle POST at '/contacts'
#'
#' @export
create_contact <- \(req, res) {
  field_names <- c("first_name", "last_name", "phone_number", "email_address")
  body <- parse_req(req, fields_to_extract = field_names)

  add_new_contact(
    first_name = body[["first_name"]],
    last_name = body[["last_name"]],
    phone_number = body[["phone_number"]],
    email_address = body[["email_address"]]
  )

  data <- filter_contacts(page = 1L)
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
  get("/contacts", get_contacts)$
  post("/contacts", create_contact)$
  start()
