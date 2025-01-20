box::use(
  htmltools[tags],
)

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
