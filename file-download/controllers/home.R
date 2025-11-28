box::use(
  dplyr,
  datasets,
  rlang[inject],
  htmltools[tags],
  # res$csv() requires {readr}. capture it as a dependency in {renv}:
  readr[format_csv],
  ambiorix[parse_multipart],
  .. / store / home[home_page],
  .. / store / components / card[card],
  .. /
    store /
    components /
    datatable[
      datatable,
      datatable_options,
    ],
  .. / helpers / create_href[create_href],
)

#' Handle GET at '/'
#'
#' @export
home_get <- \(req, res) {
  res$send(home_page())
}

#' Handle GET at '/dataset'
#'
#' @export
show_dataset <- \(req, res) {
  dataset_name <- req$query$dataset_name
  dataset <- get(x = dataset_name, envir = asNamespace(ns = "datasets"))

  html <- card(
    id = "dataset",
    title = dataset_name,
    `hx-swap` = "this", # for htmx transitions & animations
    inject(
      datatable(
        col_names = names(dataset),
        table_id = paste0(dataset_name, "data"),
        # send a GET request to "/data/:dataset_name":
        ajax = create_href(
          href = paste0(
            "/data/",
            dataset_name
          )
        ),
        !!!datatable_options
      )
    ),
    tags$a(
      href = create_href(
        href = paste0(
          "/download/",
          dataset_name
        )
      ),
      class = "btn btn-dark btn-sm rounded-1",
      tags$i(class = "bi bi-download"),
      "Download"
    )
  )

  res$send(html)
}

#' Handle GET at '/download/:name'
#'
#' @export
download_dataset <- \(req, res) {
  name <- req$params$name
  dataset <- get(x = name, envir = asNamespace(ns = "datasets"))

  res$csv(dataset, name)
}

#' Handle GET at '/data/:name'
#'
#' @export
get_data <- \(req, res) {
  name <- req$params$name
  dataset <- get(x = name, envir = asNamespace(ns = "datasets"))

  draw <- req$query$draw
  start_row <- as.integer(req$query$start) + 1L # datatables use 0-based indexing (JS)
  num_of_rows <- as.integer(req$query$length) - 1L
  end_row <- start_row + num_of_rows

  search_value <- req$query$`search[value]`

  # perform search:
  search_res <- dataset
  if (!is.null(search_value) && !is.na(search_value)) {
    # search the value in every column:
    found <- lapply(dataset, \(cl) {
      grepl(pattern = search_value, x = cl, ignore.case = TRUE)
    }) |>
      as.data.frame() |>
      rowSums(na.rm = TRUE)
    # filter only the rows with one or more occurrences of the value:
    found <- which(found > 0)
    search_res <- dataset |> dplyr$slice(found)
  }

  records_filtered <- nrow(search_res)

  # filter out the requested rows:
  row_inds <- seq(from = start_row, to = end_row, by = 1L)
  filtered <- search_res |> dplyr$slice(row_inds)

  # datatable expects json:
  response <- list(
    draw = draw,
    recordsTotal = nrow(dataset),
    recordsFiltered = records_filtered,
    data = filtered
  )

  res$json(response)
}
