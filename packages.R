load_packages <- function(req_file, silent = F) {
  # req_file - .txt file containing list of required packages (one per line)
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
  }
  if (!webshot::is_phantomjs_installed()) {
    webshot::install_phantomjs()
  }
}

# Loads packages in req.txt
load_packages("req.txt")
