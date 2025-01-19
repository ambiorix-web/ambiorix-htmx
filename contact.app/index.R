box::use(
  ambiorix[Ambiorix],
  generator[
    r_full_names,
    r_phone_numbers,
    r_email_addresses,
  ],
  htmltools[tags, tagList],
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
      tags$meta(
        name = "color-scheme",
        content = "light dark"
      ),
      tags$title("Contact.app"),
      tags$link(
        rel = "stylesheet",
        href = "/assets/styles.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "/assets/pico-2.0.6.min.css"
      ),
      tags$script(src = "/assets/htmx-2.0.4.min.js"),
      head
    ),
    tags$body(body)
  )
}

#' Create search input fields for contacts table
#'
#' @export
create_search_input_fields <- \() {
  input_ids <- paste0(names(contacts), "_pattern")
  placeholders <- c(
    "First Name",
    "Last Name",
    "Phone Number",
    "Email Address"
  )

  inputs <- Map(
    f = \(id, placeholder) {
      hx_include <- paste("#", setdiff(input_ids, id), collapse = ",")

      tags$input(
        type = "search",
        name = id,
        id = id,
        `hx-post` = "/search-contacts",
        `hx-include` = hx_include,
        `hx-target` = "#contacts_table",
        `hx-swap` = "outerHTML",
        `hx-trigger` = "input changed delay:500ms, search",
        placeholder = placeholder
      )
    },
    input_ids,
    placeholders
  )

  tags$form(
    tags$fieldset(
      class = "grid",
      inputs
    )
  )
}

#' Create an HTML table
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
html_table <- \(
  data,
  next_page = NULL,
  type = c("full", "records")
) {
  type <- match.arg(arg = type)
  nrows <- nrow(data)
  ncols <- ncol(data)
  row_names <- rownames(data)
  data <- as.list(data)


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

  col_names <- c("#", names(data))

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
    ),
    tags$tr(
      lapply(
        X = col_names,
        FUN = \(col_name) {
          if (identical(col_name, "#")) {
            return(tags$th())
          }
        }
      )
    )
  )

  table_body <- tags$tbody(table_records)

  table <- tags$table(
    id = "contacts_table",
    class = "striped",
    table_head,
    table_body
  )

  loading_spinner <- tags$div(
    id = "contacts-loading-spinner",
    class = "htmx-indicator",
    `aria-busy` = "true"
  )

  tags$article(
    tags$header("Contacts"),
    tags$main(
      create_search_input_fields(),
      table
    ),
    tags$footer(loading_spinner)
  )
}

#' Home page
#'
#' @export
home_page <- \() {
  first_fifteen <- filter_contacts()
  table <- html_table(
    data = first_fifteen$filtered,
    next_page = first_fifteen$next_page
  )

  body <- tagList(
    tags$header(
      class = "container",
      tags$hgroup(
        tags$h2("Contacts.app"),
        tags$p("Create, Read, Update & Delete Contacts")
      )
    ),
    tags$main(
      class = "container",
      table
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
    html_table(
      data = data$filtered,
      next_page = data$next_page,
      type = "records"
    )
  )

  # simulate a short delay so that spinner can be seen, lol:
  Sys.sleep(0.5)

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
  start()
