message("Think IA App - Copyright (C) 2020 Think Research Ltd.\nThis program comes with ABSOLUTELY NO WARRANTY.\nThis is free software, and you are welcome to redistribute it\nunder certain conditions; see LICENSE.md for details.")

load_packages <- function(req_file, silent = F) {
  # Input
  # req_file - .txt file containing list of required packages (one per line)
  #
  # Description
  # Loads required packages provided, automatically downloads and installs any
  # missing packages and additional requirements.
  req <- scan(req_file, character(), quiet = T)
  if (length(req) > 0) {
    missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages, repos = "https://cloud.r-project.org", dependencies = T)
    }
  }
  if (silent) {
    suppressPackageStartupMessages(invisible(lapply(req, library, character.only = T)))
  } else {
    lapply(req, library, character.only = T)
    message("\nA CAUTION TO DEVELOPERS --------------------------------")
    message("Beware of object masking! Check your package load order!")
  }
  if (!webshot::is_phantomjs_installed()) {
    webshot::install_phantomjs()
  }
}

# Loads packages in req.txt
load_packages("req.txt")
