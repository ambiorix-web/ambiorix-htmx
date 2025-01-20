box::use(
  tools[toTitleCase],
  htmltools[tags, tagList],
  . / contact_table_action_btns[contact_table_action_btns],
)

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

  if (is.null(data)) {
    not_found <- tags$div(
      id = "contacts_table",
      tags$p(
        class = "mt-5 text-center fw-bold",
        "No matching contacts found :("
      )
    )
    return(not_found)
  }

  nrows <- nrow(data)
  data[["Action"]] <- lapply(
    X = seq_len(nrows),
    FUN = \(row_idx) {
      contact_table_action_btns(data = data[row_idx, ])
    }
  )

  data[["id"]] <- NULL

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
      hx_include <- if (is_last_row) {
        paste0(
          "#",
          c("first_name_pattern", "last_name_pattern", "phone_number_pattern", "email_address_pattern"),
          collapse = ","
        )
      }

      tags$tr(
        class = "align-middle fw-light",
        `hx-get` = hx_get,
        `hx-include` = hx_include,
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
    return(
      tagList(table_records)
    )
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
