db_defaults <- list(
  driver = "SQL Server",
  server = "DESKTOP-U2P5V4F",
  database = c(
    "LVNL_UTMA_Validation",
    "NavCan_UTMA_Validation_V003a",
    "NavCan_UTMA_Validation_DB3",
    "NavCan_UTMA_Validation_DB2",
    "eTBS_UTMA_Validation_V002a",
    "RDS_UTMA_Validation"
  ),
  username = "ruser",
  password = "Th!nkruser"
)

connection_dialogue <- function() {
  modalDialog(
    div(
      class = "centered",
      h4("Connect to Database")
    ),
    selectizeInput("db_driver", "Driver Name", db_defaults$driver, options = list(create = T), width="100%"),
    selectizeInput("db_server", "Server Name", db_defaults$server, options = list(create = T), width="100%"),
    selectizeInput("db_database", "Database Name", db_defaults$database, options = list(create = T), width="100%"),
    textInput("db_username", "Username", db_defaults$username, width="100%"),
    passwordInput("db_password", "Password", db_defaults$password, width="100%"),
    div(
      class = "centered",
      actionButton("db_connect", "Connect", icon("database")),
      uiOutput("db_status")
    ),
    size = "s",
    footer = NULL,
    easyClose = T
  )
}

get_db_connection <- function(str_driver, str_server, str_database, str_uid, str_pwd) {
  connection_string <- sprintf(
    "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
    str_driver, str_server, str_database, str_uid, str_pwd
  )
  return(RODBC::odbcDriverConnect(connection=connection_string))
}
