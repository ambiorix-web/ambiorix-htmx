#' Is app running in prod?
#'
#' @return Logical.
in_prod <- \() {
  identical(
    Sys.getenv("APP_ENV"),
    "prod"
  )
}
