# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  ORD Error Visualisation                               #
# Version No.    |  3.0                                                   #
# Date Modified  |  23/04/2021                                            #
# Author(s)      |  Michael Cowham                                        #
# Project        |  Toronto IA                                            #
# Purpose        |  To investigate large ORD Errors                       #
#                |                                                        #
# ----------------------------------------------------------------------- #

rm(list = ls())

library(data.table)
library(RODBC)
library(dplyr)
library(ggplot2)
library(smooth)
library(modelr)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(getPass)

# ----------------------------------------------------------------------- #
# 1. Configuration ------------------------------------------------------
# ----------------------------------------------------------------------- #



#Use a version number derived from date or define manually
version <- paste0(Sys.Date(), " ","V1.0 (AH)")
# version <- "2021-05-04 V1.0 (AH)"

use_same_input_version <- T

if (use_same_input_version == T) {
  input_version <- version
} else if (use_same_input_version == F) {
  input_version <- "2021-05-04 V1.0 (AH)"   #Manually define this if you want different input output version numbers
}

#Set server  with IP then tied to this
Server <- "Maverick" #or Goose

if (Server == "Maverick") {ip <- "192.168.1.23"}
if (Server == "Goose") {ip <- "192.168.1.39"}

#Set the database name for SQL connection
database <- "NavCan_TBS_V3"

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Calibration & Validation"
Script_out <- "Validation"
# OutputFolder <- paste(ModuleFolder, Script_out, version, sep = "/")
# --------------------------------------------------------------------------- #

#Set to 1 when in git structure

FileFlag <- c("global.R", "GlobalPlaceholder.txt")[1]
ResourcesFolder <- c("resources", "GlobalFunctionsPlaceholder")[1]
AlgoResourcesFolder <- c("algorithm_functions", "AlgoFunctionsPlaceholder")[1]
ModulesFolder <- c("modules", "ModulesPlaceholder")[1]

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  Base_Dir <- getwd()
  Global_Dir <- Base_Dir
  Script_Dir <- file.path(Base_Dir)
  while (!file.exists(file.path(Global_Dir, FileFlag))){
    Global_Dir <- file.path(Global_Dir, "..")
  }
} else {
  Global_Dir <- getwd()
  Script_Dir <- file.path(Global_Dir, ModulesFolder, ModuleFolder, ModuleSubfolder)
}

Global_Dir <- file.path(Global_Dir, ResourcesFolder)
Algo_Func_Dir <- file.path(Global_Dir, AlgoResourcesFolder)

# Global Functions, imports & parameters
source(file.path(Global_Dir, "imports.R"), local = F)
source(file.path(Global_Dir, "unit conversions.R"), local = F)
source(file.path(Global_Dir, "functions.R"), local = F)
source(file.path(Algo_Func_Dir, "ORD Functions.R"), local = F)

project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3", noblank = FALSE, forcemask = FALSE))

#IorO for "Ouputs" or "Inputs" directory, must be a string as used in directory
Base_Dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

#Directory for reference data
# inputs_dir <- file.path(project_dir, "Inputs")
# input <- file.path(inputs_dir, "GWCS_Input", version)

inputs_dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Inputs")
ref_data <- file.path(inputs_dir, "Reference Data")

# out_data <- Base_Dir
ord_dir <- Base_Dir

ref_data <- file.path(ord_dir, "Validation", version)

prof_data <- file.path(ord_dir, "Speed Profiles", input_version)

out_data <- file.path(ord_dir, "Validation", version)

plots_data <- file.path(out_data, "ORD Plots")
if (!dir.exists(plots_data)) dir.create(plots_data)


#Get database connection
con <- Get_RODBC_Database_Connection(IP = ip, Database = database)





# Config Parameters

time_offset_position_plot <- 30

m_to_ft <- 3.28084

# Use database (= T) or previously calculated results file

use_database <- T

# Filter for IAS

filter_ias <- T
ias_max <- 180
ias_min <- 150

# Allowed Path Legs

Allowed_Path_Legs <- c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept")

# ----------------------------------------------------------------------- #
# 2. Load Data ----------------------------------------------------------
# -----------------------------------------------------------------------

# Load the main ORD data set

ord_data <- fread(file.path(ref_data, "Validation Data Post SepN Accuracy.csv"))

prof_data <- fread(file.path(prof_data, "Approach_Speed_Profiles.csv"))

#ord_data <- left_join(ord_data, prof_data, by = c("Leader_Flight_Plan_ID" = "Follower_Flight_Plan_ID"))

# Filter for large errors (currently exclusing large wind errors assuming some these due the Range to Threshold Issue)

#ord_large_errors <- ord_data[Thresh_Accuracy_Perfect_05 == "[0.5NM, Inf)"][order(ORD_Compression_Error, decreasing = T)]
ord_large_errors <- ord_data[ORD_Compression_Error_Rounded >= 0.2 & Forecast_Mean_Follower_Wind_Effect_Error <= 15][order(ORD_Compression_Error, decreasing = T)]

# ----------------------------------------------------------------------- #
# 3. Functions ----------------------------------------------------------
# ----------------------------------------------------------------------- #


plot_ord_profile_ll <- function(lp_id){

  #lp_id <- 118746
  #include_gspd <- T

  # Get the ORD data line

  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)

  title_text <- paste0("Leader IAS Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("ORD Error = ", ord_line$ORD_Compression_Error_Rounded, "NM ", "Leader IAS Error = ", sprintf("%.0f", ord_line$ORD_Leader_IAS_Error), "kt")

  p1 <- tryCatch(plot_single_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, F , title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  title_text <- paste0("Leader GSPD Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("Lead Wind Effect Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error), "kt Follower Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error), "kt")

  p2 <- tryCatch(plot_single_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, T , title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  grid.arrange(p1, p2, nrow = 1)

}

plot_ord_profile_we <- function(lp_id){

  #lp_id <- 118746
  #include_gspd <- T

  # Get the ORD data line

  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)

  title_text <- paste0("Leader WE Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("ORD Error = ", ord_line$ORD_Compression_Error_Rounded, "NM ", "Leader WE Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error), "kt")

  p1 <- tryCatch(plot_single_wind_effect(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, "L", 0, 4.5, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  title_text <- paste0("Follower WE Profile for ", ord_line$Follower_Callsign, " ",ord_line$Follower_Aircraft_Type)
  subtitle_text <- paste0("Follower Wind Effect Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error))

  p2 <- tryCatch(plot_single_wind_effect(ord_line$Follower_Flight_Plan_ID, ord_line$Landing_Pair_ID, "F", ord_line$Follower_Forecast_Start_Distance, ord_line$Follower_Forecast_End_Distance, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  grid.arrange(p1, p2, nrow = 1)

}


plot_ord_profile_lf <- function(lp_id, include_gspd){

  #lp_id <- 855810
  #include_gspd <- T

  # Get the ORD data line

  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)

  title_text <- paste0("Leader IAS Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("IAS Error = ", sprintf("%.0f", ord_line$ORD_Leader_IAS_Error), " Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error))

  p1 <- tryCatch(plot_ord_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, "L", include_gspd, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  title_text <- paste0("Follower IAS Speed Profile for ", ord_line$Follower_Callsign, " ",ord_line$Follower_Aircraft_Type)
  subtitle_text <- paste0("IAS Error = ", sprintf("%.0f", ord_line$ORD_Follower_IAS_Error), " Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error))

  p2 <- tryCatch(plot_ord_aircraft(ord_line$Follower_Flight_Plan_ID, ord_line$Landing_Pair_ID, "F", include_gspd, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  grid.arrange(p1, p2, nrow = 1)

}

plot_ord_aircraft <- function(fp_id, lp_id, l_or_f, include_gspd, title_text, subtitle_text){

  #fp_id <- 247329
  #lp_id <- 855810
  #include_gspd <- F
  #l_or_f <- "L"

  leader_track <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))

  # Get the ORD Profile for leader and follower

  ord_profile <- sqlQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = '", l_or_f, "'"))

  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852,
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852)


  if (include_gspd == T){
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD, color = "GSPD"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_GS, color = "ORD GSPD"))+

      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "GSPD", "ORD IAS", "ORD GSPD"),
                         values = c("IAS" = "black", "GSPD" = "blue", "ORD IAS" = "grey", "ORD GSPD" = "light blue"))+
      theme_bw()+
      theme(legend.position = "bottom")

  } else {
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "ORD IAS"),
                         values = c("IAS" = "black", "ORD IAS" = "grey"))+
      theme_bw()+
      theme(legend.position = "bottom")
  }

  return(p1)

}


plot_single_aircraft <- function(fp_id, lp_id, gspd, title_text, subtitle_text){

  #fp_id <- 31169
  #lp_id <- 118746
  #lp_id <- 855810
  #gspd <- F
  #l_or_f <- "L"

  leader_track <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))

  # Get the ORD Profile for leader and follower

  ord_profile <- sqlQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = 'L'"))

  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852,
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852)


  if (gspd == T){
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD, color = "GSPD"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_GS, color = "ORD GSPD"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("GSPD", "ORD GSPD"),
                         values = c("GSPD" = "blue", "ORD GSPD" = "light blue"))+
      theme_bw()+
      theme(legend.position = "bottom")

  } else {
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "ORD IAS"),
                         values = c("IAS" = "black", "ORD IAS" = "grey"))+
      theme_bw()+
      theme(legend.position = "bottom")
  }

  return(p1)

}


plot_single_wind_effect <- function(fp_id, lp_id, role, min, max, title_text, subtitle_text){

#  fp_id <- 31170
#  lp_id <- 118746
#  role <- "F"
  #lp_id <- 855810
  #gspd <- F
  #l_or_f <- "L"

 # min <- 3.5
#  max <- 8.5



  leader_track <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))

  leader_track <- filter(leader_track, Path_Leg_Type %in% Allowed_Path_Legs)

  min_y = 10 * floor(min(leader_track$Wind_Effect_IAS) / 10)
  max_y = 10 * floor(max(leader_track$Wind_Effect_IAS) / 10) + 10

  # Get the ORD Profile for leader and follower

  ord_profile <- sqlQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = '", role, "'"))

  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852,
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852,
                        Start_WE = Start_GS - Start_IAS,
                        End_WE = End_GS - End_IAS)

  p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = ILS_Locus_RTT, y = Wind_Effect_IAS, color = "Wind Effect"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_WE, color = "ORD WE"))+
      xlim(0, 10)+
      ylim(min_y, max_y)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Wind Effect (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("Wind Effect", "ORD WE"),
                         values = c("Wind Effect" = "blue", "ORD WE" = "grey"))+
      geom_vline(xintercept = min, color = "grey")+
      geom_vline(xintercept = max, color = "grey")+
      theme_bw()+
      theme(legend.position = "bottom")



  return(p1)

}


# ----------------------------------------------------------------------- #
# 4. Analysis  ----------------------------------------------------------
# ----------------------------------------------------------------------- #


# Plot the set of large errors

for (i in 1:nrow(ord_large_errors)){
# for (i in 1:100){

  # i <- 1
  le_line <- ord_large_errors[i, ]

  plot_name <- paste0(i, "-",le_line$Leader_Flight_Plan_ID,"-", le_line$Leader_Callsign, "-", le_line$Follower_Callsign, ".png")
  png(filename = file.path(plots_data, plot_name), width = 900, height = 600)
  plot_ord_profile_ll(le_line$Landing_Pair_ID)
  dev.off()

}


#for (i in 1:nrow(ord_large_errors)){
  for (i in 1:20){

  #i <- 1
  le_line <- ord_large_errors[i, ]

  plot_name <- paste0("WE-", i, "-",le_line$Leader_Flight_Plan_ID,"-", le_line$Leader_Callsign, "-", le_line$Follower_Callsign, ".png")
  png(filename = file.path(plots_data, plot_name), width = 900, height = 600)
  plot_ord_profile_we(le_line$Landing_Pair_ID)
  dev.off()

}


# Plotting largest ORD large error per aircraft type

largest_error_by_type <- ord_data %>% group_by(Leader_Aircraft_Type) %>% filter(ORD_Compression_Error == max(ORD_Compression_Error))

for (i in 1:nrow(largest_error_by_type)){
  # for (i in 1:10){

  #i <- 1
  le_line <- largest_error_by_type[i, ]

  plot_name <- paste0(i, "-",le_line$Leader_Flight_Plan_ID,"-", le_line$Leader_Callsign, "-", le_line$Follower_Callsign, ".png")
  png(filename = file.path(plots_data, plot_name), width = 900, height = 600)
  plot_ord_profile_ll(le_line$Landing_Pair_ID)
  dev.off()

}
