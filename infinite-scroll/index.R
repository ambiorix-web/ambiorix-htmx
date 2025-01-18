box::use(
  babynames[babynames],
)

babynames <- as.data.frame(babynames)

#' Filter rows from `babynames` with pagination
#'
#' @param page Integer. Page number to retrieve. Default
#' is 1.
#' @param page_length Integer. Number of rows per page. Default
#' is 15.
#' @return Named list:
#' - `filtered`: data.frame object containing the rows for
#' the requested page.
#' - `next_page`: The next page number as an integer, or `NULL`
#' if the current page is the last one.
#' @examples
#' # default:
#' filter_babynames()
#'
#' # retrieve the second page of results, with 10 rows per page:
#' filter_babynames(page = 2L, page_length = 10L)
#'
#' # attempt to retrieve a page beyond the available data:
#' filter_babynames(page = 1000000)
#' @export
filter_babynames <- \(page = 1L, page_length = 15L) {
  n <- nrow(babynames)

  start_idx <- (page - 1L) * page_length + 1L
  if (start_idx > n) {
    empty <- list(
      filtered = babynames[integer(), ],
      next_page = NULL
    )
    return(empty)
  }

  end_idx <- min(page * page_length, n)
  is_last_page <- identical(end_idx, n)
  next_page <- if (!is_last_page) page + 1L

  row_idx <- seq(from = start_idx, to = end_idx, by = 1L)

  list(
    filtered = babynames[row_idx, ],
    next_page = next_page
  )
}
