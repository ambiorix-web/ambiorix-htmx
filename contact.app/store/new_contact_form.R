box::use(
  htmltools[tags, tagList],
  . / create_href[create_href],
)

#' New contact form
#'
#' @export
new_contact_form <- \() {
  label <- tags$label(
    `for` = "n"
  )
  input <- tags$input(
    class = "form-control",
    type = "text",
  )

  labels <- c("First Name", "Last Name", "Phone Number", "Email Address")
  ids <- c("first_name", "last_name", "phone_number", "email_address")
  types <- c("text", "text", "tel", "email")
  placeholders <- c("John", "Doe", "+254712345678", "johndoe@mail.com")

  inputs <- Map(
    f = \(id, type, label, placeholder) {
      lbl <- tags$label(
        `for` = id,
        class = "form-label",
        label
      )

      input <- tags$input(
        class = "form-control",
        id = id,
        name = id,
        type = type,
        placeholder = placeholder
      )

      tags$div(
        class = "mb-3",
        lbl,
        input
      )
    },
    ids,
    types,
    labels,
    placeholders
  )

  btns <- tags$div(
    class = "d-flex justify-content-between",
    tags$button(
      type = "button",
      class = "btn btn-danger btn-sm",
      "Cancel"
    ),
    tags$button(
      type = "submit",
      class = "btn btn-success btn-sm",
      "Save Contact"
    )
  )

  tags$form(
    `hx-post` = create_href(href = "/contacts"),
    `hx-target` = "#contacts_table",
    `hx-swap` = "outerHTML",
    `hx-on::after-request` = "this.reset()",
    inputs,
    btns
  )
}
