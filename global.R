source("packages.R", local = T)

headerButton <- function(id, icon) {
  ns <- NS(id)
  tags$li(
    class = "dropdown header_button",
    id = ns("header_button"),
    icon(icon)
  )
}
