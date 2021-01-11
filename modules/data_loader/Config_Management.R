import_configs <- function(configDir, dbi_con) {
  message("[",Sys.time(),"] ", "Import config from: ", configDir)
  sqlFiles <- list.files(configDir, pattern = ".sql", full.names = T)
  for (i in sqlFiles) {
    message("[",Sys.time(),"] ", "Executing ", basename(i))
    x_i <- read_SQL_File(i)
    if (grepl("\\$\\$PARAMETER\\$\\$", x_i)) {
      #dirname(i)
      # configTablePath <- gsub("/", "\\", "D:/Config/Schiphol_IA_Config_Dev", fixed = T)
      # dbGetQuery(dbi_con, gsub("$$PARAMETER$$", configTablePath, x_i, fixed = T))
      # sprintf(gsub("\\$\\$PARAMETER\\$\\$.*.csv", "%s", test), paste0(testdir, "\\bleh.csv"))
      # dbGetQuery(dbi_con, gsub("\\$\\$PARAMETER\\$\\$.*.csv", gsub(".sql$", ".csv", i), x_i))
      # dbGetQuery(dbi_con, sprintf(gsub("\\$\\$PARAMETER\\$\\$", "%s", x_i), paste(unlist(strsplit(configDir, "\\\\")), collapse = "\\")))
      # dbGetQuery(dbi_con, gsub("\\$\\$PARAMETER\\$\\$", paste0(configDir, "/"), x_i))
    } else {
      dbGetQuery(dbi_con, x_i)
    }
  }
  message("[",Sys.time(),"] ", "Finished importing new configuration data.")
}

# list(
#   ord_runways = list(
#     ord_runway = list(
#       runway_name = "R06",
#       max_dtt = "30.00",
#       four_hundred_ft_aal = "1.04",
#       six_hundred_ft_aal = "1.67",
#       thousand_ft_gate = "2.93",
#       gust_adjustment = "0.0"
#     ),
#     ord_runway = list(
#       runway_name = "R09",
#       max_dtt = "30.00",
#       four_hundred_ft_aal = "1.05",
#       six_hundred_ft_aal = "1.68",
#       thousand_ft_gate = "2.94",
#       gust_adjustment = "0.0"
#     )
#     # more ord_runway = list(...) here
#   ),
#   ord_wake_categories = list(
#     ord_wake_category = list(
#       # ...
#     ),
#     ord_wake_category = list(
#       # ...
#     ),
#     ord_wake_category = list(
#       # ...
#     )
#   ),
#   ord_aircraft_types = list(
#     ord_aircraft_type = list(
#       # ...
#     ),
#     ord_aircraft_type = list(
#       # ...
#     ),
#     ord_aircraft_type = list(
#       # ...
#     )
#   ),
#   ord_tbs_table_values = list(
#     ord_tbs_table_value = list(
#       #
#     ),
#     ord_tbs_table_value = list(
#       #
#     ),
#     ord_tbs_table_value = list(
#       #
#     )
#   ),
#   wind_effect_segment_entries = list(
#     wind_effect_segment_entry = list(
#       #
#     ),
#     wind_effect_segment_entry = list(
#       #
#     ),
#     wind_effect_segment_entry = list(
#       #
#     )
#   )
# )

