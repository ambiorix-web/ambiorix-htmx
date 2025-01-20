box::use(
  htmltools[tags, tagList],
  . / edit_contact_btn[edit_contact_btn],
  . / delete_contact_btn[delete_contact_btn],
)

#' Contact table action buttons
#'
#' @param data data.frame. Row record.
#' @return [htmltools::tags]
#' @export
contact_table_action_btns <- \(data) {
  tags$div(
    class = "btn-group btn-group-sm",
    role = "group",
    `aria-label` = "Action",
    edit_contact_btn(data = data),
    delete_contact_btn(data = data)
  )
}
