box::use(
  htmltools[tags, tagList],
  . / html_page[html_page],
  . / contacts_table[contacts_table],
  . / new_contact_form[new_contact_form],
  . / create_contact_btn[create_contact_btn],
  . / contact_search_fields[contact_search_fields],
  . / contacts_loading_spinner[contacts_loading_spinner],
  .. / db[read_all_contacts],
)

#' Home page
#'
#' @export
home_page <- \() {
  first_fifteen <- read_all_contacts()
  table <- contacts_table(
    data = first_fifteen$filtered,
    next_page = first_fifteen$next_page
  )

  body <- tagList(
    tags$header(
      class = "container",
      tags$h2("Contacts.app"),
      tags$p("Create, Read, Update & Delete Contacts")
    ),
    tags$main(
      class = "container",
      create_contact_btn(),
      contact_search_fields(),
      table,
      contacts_loading_spinner()
    ),
    tags$footer(
      class = "container",
      tags$p("ambiorix + htmx🚀")
    )
  )

  html_page(body = body)
}
