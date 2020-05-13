# ----------------------------------------------------------------------- #
# Database Settings -------------------------------------------------------
# ----------------------------------------------------------------------- #

# Defines database connection dialogue parameters
db_defaults <- list(
  driver = "SQL Server",
  server = c(
    "DESKTOP-U2P5V4F",
    "192.168.1.11"
  ),
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

# ----------------------------------------------------------------------- #
# Sidebar Settings --------------------------------------------------------
# ----------------------------------------------------------------------- #

sidebarWidth <- "190px"

# Define names for sidebar tabs, e.g. Module_Name = c(name_str, icon_str):
#    Module_Name - corresponds to a folder name in the "modules" folder
#    name_str - is a string for the display name of a module
#    icon_str - is a string corresponding to an icon on fontawesome.com/icons
# NOTE: > Module will not be displayed unless specified here
#       > Order corresponds to order of appearance in app sidebar
#       > Use "" for icon_str to specify no icon
sidebarNameIcon <- list(
  example_1 = c("Example 1", "car"),
  db_explorer = c("Database Explorer", "database")
  # track_visualiser = "Track Visualiser",
)
