#' Is app running in prod?
#'
#' @return Logical.
#' @export
in_prod <- function() {
  identical(
    Sys.getenv("APP_ENV"),
    "prod"
  )
}
