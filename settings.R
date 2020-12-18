# ----------------------------------------------------------------------- #
# Database Settings -------------------------------------------------------
# ----------------------------------------------------------------------- #

# Defines database connection dialogue parameters
db_defaults <- list(
  driver = "SQL Server",
  server = c(
    "Goose (192.168.1.39)" = "192.168.1.39",
    "Maverick (192.168.1.23)" = "192.168.1.23"
  ),
  database = if (file.exists("temp/db_names.csv")) fread("temp/db_names.csv", header = F)$V1,
  username = "ruser",
  password = "Th!nkruser"
)

# con <- odbcDriverConnect(connection="Driver={SQL Server};Server={192.168.1.23};Database={Think_App_Test};Uid={vbuser};Pwd={Th!nkvbuser};")
# dbi_con <- dbConnect(odbc::odbc(), .connection_string="Driver={SQL Server};Server={192.168.1.23};Database={Think_App_Test};Uid={vbuser};Pwd={Th!nkvbuser};")

# ----------------------------------------------------------------------- #
# Sidebar Settings --------------------------------------------------------
# ----------------------------------------------------------------------- #

sidebarWidth <- "160px"

# Define names for sidebar tabs, e.g. Module_Name = c(name_str, icon_str):
#    Module_Name - corresponds to a folder name in the "modules" folder
#    name_str - is a string for the display name of a module
#    icon_str - is a string corresponding to an icon on fontawesome.com/icons
# NOTE: > Module will not be displayed unless specified here
#       > Order corresponds to order of appearance in app sidebar
#       > Use "" for icon_str to specify no icon
sidebarSettings <- list(
  # example_1 = c("Example 1", "car"),
  # user_guide = c("User Guide", "question-circle"),
  data_loader = c("Data Loader", "database"),
  db_explorer = c("Database Explorer", "list-alt"),
  track_visualiser = c("Track Visualiser", "search-location"),
  optimised_runway_delivery = c("ORD", "plane-arrival")
)
