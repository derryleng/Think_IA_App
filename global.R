# Import required R packages
source("packages.R", local = T)

# Import settings
source("settings.R", local = T)

# Import mod_ui.R and mod_server.R files
invisible(lapply(list.files(path = "modules", pattern = "mod_ui.R|mod_server.R", recursive = T, full.names = T), source))

# ----------------------------------------------------------------------- #
# General Functions -------------------------------------------------------
# ----------------------------------------------------------------------- #

# Usage: x %!in% y; this is equivalent to !(x %in% y)
'%!in%' <- function(x, y) !('%in%'(x, y))

# Evaluate a string (or multiple comma-separated strings) as R code
evalParse <- function(...) {
  return(eval(parse(text = paste0(..., collapse = ","))))
}

# ----------------------------------------------------------------------- #
# Database Functions ------------------------------------------------------
# ----------------------------------------------------------------------- #

# Database connection pop-up dialogue
connection_dialogue <- function() {
  modalDialog(
    div(
      class = "centered",
      style = "margin: -6px",
      h4("Connect to Database")
    ),
    div(
      style = "margin-bottom: -15px",
      selectizeInput("db_driver", "Driver Name", db_defaults$driver, options = list(create = T), width = "100%")
    ),
    div(
      style = "margin-bottom: -15px",
      selectizeInput("db_server", "Server Name", db_defaults$server, options = list(create = T), width = "100%")
    ),
    div(
      style = "margin-bottom: -15px",
      selectizeInput("db_database", "Database Name", db_defaults$database, options = list(create = T), width = "100%")
    ),
    div(
      style = "margin-bottom: -10px",
      textInput("db_username", "Username", db_defaults$username, width = "100%")
    ),
    div(
      style = "margin-bottom: -5px",
      passwordInput("db_password", "Password", db_defaults$password, width = "100%")
    ),
    div(
      class = "centered",
      actionButton("db_connect", "Connect", icon("database"))
    ),
    uiOutput("db_status"),
    size = "s",
    footer = NULL,
    easyClose = T
  )
}

# Fetch database connection
get_db_connection <- function(str_driver, str_server, str_database, str_uid, str_pwd) {
  connection_string <- sprintf(
    "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
    str_driver, str_server, str_database, str_uid, str_pwd
  )
  return(odbcDriverConnect(connection=connection_string))
}

# ----------------------------------------------------------------------- #
# UI Wrapper Functions ----------------------------------------------------
# ----------------------------------------------------------------------- #

# Used to make buttons on the header bar
headerButtonUI <- function(id, icon_str) {
  tags$li(
    class = "dropdown header_button",
    id = id,
    icon(icon_str)
  )
}

sidebarTabUI <- function(id, text_str, icon_str) {
  menuItem(
    text = text_str,
    tabName = id,
    icon = icon(icon_str)
  )
}

tabContentUI <- function(id, ...) {
  ns <- NS(id)
  tabItem(tabName = id, ...)
}

# pickerInput function with customised styling and action boxes
pickerInput_customised <- function(
  inputId,
  label = NULL,
  choices = NULL,
  selected = NULL,
  multiple = T,
  options = list(`actions-box` = T, `live-search` = T),
  choicesOpt = NULL,
  width = "220px",
  inline = F,
  ...
) {
  pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = options,
    choicesOpt = choicesOpt,
    width = width,
    inline = inline,
    ...
  )
}

# datatable function with customised styling
datatable_customised_1 <- function(
  data,
  rownames = F,
  selection = "none",
  style = "bootstrap4",
  options = list(
    pageLength = 15,
    lengthMenu = seq(5, 100, 5),
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    scrollX = T,
    dom = '<"dataTables_row"lf>rt<"dataTables_row"ip>'
  ),
  ...
){
  datatable(
    data = data,
    rownames = rownames,
    selection = selection,
    style= style,
    options = options,
    ...
  )
}

# datatable function with customised styling and download buttons
datatable_customised_2 <- function(
  data,
  rownames = F,
  selection = "none",
  style = "bootstrap4",
  options = list(
    pageLength = 15,
    lengthMenu = seq(5, 100, 5),
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    scrollX = T,
    dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>',
    buttons = c('copy', 'csv', 'excel')
  ),
  extensions = c("Buttons"),
  ...
){
  datatable(
    data = data,
    rownames = rownames,
    selection = selection,
    style= style,
    options = options,
    extensions = extensions,
    ...
  )
}
