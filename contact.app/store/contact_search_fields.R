box::use(
  htmltools[tags, tagList],
  . / create_href[create_href],
)

#' Create search input fields for contacts table
#'
#' @export
contact_search_fields <- \() {
  input_ids <- paste0(
    c(
      "first_name",
      "last_name",
      "phone_number",
      "email_address"
    ),
    "_pattern"
  )

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
        `hx-post` = create_href(href = "/search-contacts"),
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
