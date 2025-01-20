box::use(
  htmltools[tags, tagList],
)

#' Delete contact button
#'
#' @param data data.frame. Data of the record to delete.
#' @return [htmltools::tagList()]. The button to
#' delete a contact and the associated modal
#' dialog.
#' @export
delete_contact_btn <- \(data) {
  data_id <- data[["id"]]
  modal_id <- paste0("modal_delete_contact_", data_id)

  btn <- tags$button(
    type = "button",
    class = "btn btn-outline-danger",
    `data-bs-toggle` = "modal",
    `data-bs-target` = paste0("#", modal_id),
    tags$i(class = "bi bi-trash3"),
    "Delete"
  )


  tagList(
    btn,
    modal_delete_contact(data = data)
  )
}

#' Delete contact modal dialog
#'
#' @param data data.frame. Data of the record to delete.
#' @return [htmltools::tags]
modal_delete_contact <- \(data) {
  data_id <- data[["id"]]
  modal_id <- paste0("modal_delete_contact_", data_id)
  header_id <- paste0("modal_delete_contact_header_", data_id)

  header <- tags$div(
    class = "modal-header py-2",
    tags$h1(
      class = "modal-title fs-5 fw-bold",
      id = header_id,
      "Delete Contact"
    ),
    tags$button(
      type = "button",
      class = "btn-close",
      `data-bs-dismiss` = "modal",
      `aria-label` = "Close"
    )
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
      "Proceed"
    )
  )

  hx_delete <- paste0("/contacts/", data[["id"]])

  body <- tags$div(
    class = "modal-body",
    tags$form(
      class = "mb-0",
      `hx-delete` = hx_delete,
      `hx-target` = "#contacts_table",
      `hx-swap` = "outerHTML",
      tags$p(
        "Are you sure you want to delete",
        tags$span(
          class = "fw-bold",
          data[["first_name"]],
          data[["last_name"]]
        ),
        "?"
      ),
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
