box::use(
  htmltools[tags, tagList],
)

#' Create new contact button
#'
#' @return [htmltools::tagList()]. The button to
#' create a new contact and the associated modal
#' dialog.
#' @export
create_contact_btn <- \() {
  btn <- tags$button(
    class = "btn btn-sm btn-primary mb-3",
    `data-bs-toggle` = "modal",
    `data-bs-target` = "#modal_create_contact",
    tags$i(class = "bi bi-plus-lg"),
    "Add New Contact"
  )

  tagList(
    btn,
    modal_new_contact()
  )
}

#' Add new contact modal dialog
#'
#' @return [htmltools::tags]
modal_new_contact <- \() {
  header <- tags$div(
    class = "modal-header py-2",
    tags$h1(
      class = "modal-title fs-5 fw-bold",
      id = "modal_create_contact_header",
      "Add New Contact"
    ),
    tags$button(
      type = "button",
      class = "btn-close",
      `data-bs-dismiss` = "modal",
      `aria-label` = "Close"
    )
  )

  input_ids <- paste0(
    "new_contact_",
    c("first_name", "last_name", "phone_number", "email_address")
  )
  types <- c("text", "text", "tel", "email")
  labels <- c("First Name", "Last Name", "Phone Number", "Email Address")
  placeholders <- paste("Enter", labels)

  inputs <- Map(
    f = \(id, type, label, placeholder) {
      lbl <- tags$label(
        `for` = id,
        class = "form-label",
        label
      )

      input <- tags$input(
        type = type,
        id = id,
        name = id,
        class = "form-control",
        placeholder = placeholder
      )

      tags$div(
        class = "mb-3",
        lbl,
        input
      )
    },
    input_ids, types, labels, placeholders
  )

  btns <- tags$div(
    class = "d-flex justify-content-between w-100",
    tags$button(
      type = "button",
      class = "btn btn-sm btn-danger",
      `data-bs-dismiss` = "modal",
      "Cancel"
    ),
    tags$button(
      type = "submit",
      class = "btn btn-sm btn-success",
      `data-bs-dismiss` = "modal",
      "Add contact"
    )
  )

  body <- tags$div(
    class = "modal-body",
    tags$form(
      class = "mb-0",
      `hx-post` = "/contacts",
      `hx-target` = "#contacts_table",
      `hx-swap` = "outerHTML",
      `hx-on::after-request` = "this.reset()",
      inputs,
      btns
    )
  )

  tags$div(
    class = "modal fade",
    id = "modal_create_contact",
    tabindex = "-1",
    `aria-labelledby` = "modal_create_contact_header",
    `aria-hidden` = "true",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        header,
        body
      )
    )
  )
}
