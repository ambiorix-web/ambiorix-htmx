box::use(
  htmltools[tags]
)

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
