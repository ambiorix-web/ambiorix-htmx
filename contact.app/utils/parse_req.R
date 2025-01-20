box::use(
  webutils[parse_http],
)

#' Parse HTTP request
#'
#' @description
#' Parses the body of an HTTP request based on its `Content-Type` header. This
#' function simplifies working with HTTP requests by extracting specific data
#' fields from the parsed body.
#'
#' @details
#' Supported `Content-Type` values include:
#' - `application/x-www-form-urlencoded`
#' - `multipart/form-data`
#' - `application/json`
#'
#' The `fields_to_extract` & `new_field_names` parameters are currently only
#' used for 'multipart/form-data'.
#'
#' **Limitations**:
#' - File uploads are not yet supported but could be added in future updates if
#' required.
#'
#' @param req A request object. The request must include a `CONTENT_TYPE` header
#' and a body accessible via `req$rook.input$read()`.
#' @param content_type String. 'Content-Type' of the request. See details for
#' valid values.
#' By default, this parameter is set to `NULL` and is inferred from the `req`
#' object during run time.
#' The only time you need to provide this argument is if `req$CONTENT_TYPE`
#' is different from how you want the request body to be parsed.
#' For example, `req$CONTENT_TYPE` gives "text/plain;charset=UTF-8" but you want
#' to parse the request body as "application/json".
#' @param fields_to_extract Character vector specifying the names of fields to
#' extract from the parsed request body. If missing, returns all
#' fields found after parsing of the HTTP request.
#' @param new_field_names Character vector of same length as
#' `fields_to_extract`. Specifies new names to assign to the extracted fields
#' in the returned list. Useful for renaming the fields for clarity or
#' consistency in the output. If not provided or empty (default), the
#' original names in `fields_to_extract` are used.
#' @return Named list containing the extracted fields and their associated
#' values. If no data is found or an error occurs, an empty list is returned.
#' @export
parse_req <- \(
  req,
  content_type = NULL,
  fields_to_extract = character(),
  new_field_names = character()
) {
  body <- req$rook.input$read()
  if (identical(body, raw())) {
    return(list())
  }

  content_type_choices <- c(
    "multipart/form-data",
    "application/json",
    "application/x-www-form-urlencoded"
  )
  content_type <- if (!is.null(content_type)) {
    match.arg(arg = content_type, choices = content_type_choices)
  } else {
    req$CONTENT_TYPE
  }

  parsed <- parse_http(body = body, content_type = content_type)

  if (identical(content_type, "application/json")) {
    return(parsed)
  }

  # -----form-data-----
  raw_to_char <- \(x) rawToChar(as.raw(x))

  values <- lapply(X = parsed, FUN = `[[`, "value") |>
    lapply(FUN = raw_to_char)

  if (identical(fields_to_extract, character())) {
    return(values)
  }

  required <- values[fields_to_extract]

  if (identical(new_field_names, character())) {
    return(required)
  }

  stopifnot(
    "'fields_to_extract' must have same length as 'new_field_names'" =
      identical(
        length(fields_to_extract),
        length(new_field_names)
      )
  )

  names(required) <- new_field_names
  required
}
