# Import packages
source("packages.R", local = T)

# Import settings
source("settings.R", local = T)

# Import resources
source("resources.R", local = T)

# Import modules
invisible(lapply(list.files(path = "modules", pattern = "mod_ui.R|mod_server.R", recursive = T, full.names = T), source))
