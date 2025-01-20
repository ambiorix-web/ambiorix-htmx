box::use(
  htmltools[tags, tagList],
)

#' Edit contact button
#'
#' @param data data.frame. Data of the record to edit.
#' @return [htmltools::tagList()]. The button to
#' edit the contact and the associated modal
#' dialog.
#' @export
edit_contact_btn <- \(data) {
  data_id <- data[["id"]]
  modal_id <- paste0("modal_edit_contact_", data_id)

  btn <- tags$div(
    class = "btn-group btn-group-sm",
    role = "group",
    `aria-label` = "Action",
  )

  btn <- tags$button(
    type = "button",
    class = "btn btn-outline-dark",
    `data-bs-toggle` = "modal",
    `data-bs-target` = paste0("#", modal_id),
    tags$i(class = "bi bi-pencil-square"),
    "Edit"
  )


  tagList(
    btn,
    modal_edit_contact(data = data)
  )
}

#' Edit contact modal dialog
#'
#' @param data data.frame. Data of the record to edit.
#' @return [htmltools::tags]
modal_edit_contact <- \(data) {
  data_id <- data[["id"]]
  modal_id <- paste0("modal_edit_contact_", data_id)
  header_id <- paste0("modal_edit_contact_header_", data_id)

  header <- tags$div(
    class = "modal-header py-2",
    tags$h1(
      class = "modal-title fs-5 fw-bold",
      id = header_id,
      "Edit Contact"
    ),
    tags$button(
      type = "button",
      class = "btn-close",
      `data-bs-dismiss` = "modal",
      `aria-label` = "Close"
    )
  )

  base_ids <- c("first_name", "last_name", "phone_number", "email_address")
  input_ids <- paste0(
    "edit_contact_",
    base_ids,
    "_",
    data_id
  )
  values <- c(
    data[["first_name"]],
    data[["last_name"]],
    data[["phone_number"]],
    data[["email_address"]]
  )
  types <- c("text", "text", "tel", "email")
  labels <- c("First Name", "Last Name", "Phone Number", "Email Address")

  inputs <- Map(
    f = \(id, value, type, label) {
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
        required = NA,
        value = value
      )

      tags$div(
        class = "mb-3",
        lbl,
        input
      )
    },
    input_ids, values, types, labels
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
      "Save changes"
    )
  )

  hx_put <- paste0("/contacts/", data[["id"]])

  body <- tags$div(
    class = "modal-body",
    tags$form(
      class = "mb-0",
      `hx-put` = hx_put,
      `hx-include` = paste0("#", base_ids, "_pattern", collapse = ", "),
      `hx-target` = "#contacts_table",
      `hx-swap` = "outerHTML",
      inputs,
      btns
    )
  )

  tags$div(
    class = "modal fade",
    id = modal_id,
    tabindex = "-1",
    `aria-labelledby` = header_id,
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
