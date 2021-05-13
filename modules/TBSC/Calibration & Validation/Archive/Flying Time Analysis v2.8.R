# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Wind Conditions Analysis                              #
# Version No.    |  2.8                                                   #
# Date Modified  |  03/12/2020                                            #
# Author(s)      |  George Clark, Catherine Mason                         #
# Project        |  NavCan IA                                             #
# Purpose        |  To assess the wind conditions variability             #
#                |                                                        #
# ----------------------------------------------------------------------- #

#-------------------------------------------------------------------------#
# Version History --------------------------------------------------------#
#-------------------------------------------------------------------------#

# Version 1.0 - Original script produced based from NAV - not operational
# Version 1.1 - not operational
# Version 1.2 - not operational
# Version 1.3 - Operational. Gives Average IAS/Flying Times grouped by 
#               Wake Cat/Runway and Wake Cat
# Version 1.4 - Restructure of previous version. Should give same outputs
# Version 1.5 - Added investigations into Outliers
# Version 1.6 - Test for Surface Wind Sample Size
# Version 2.0 - Restructure of entire program. Removed outlier investigations
#               and added filter count analysis and achieved separation filter.
#               Configured to run for a given start dist, but comparison functions
#               not present.
# Version 2.0 mc - Updated the SQL Query to look for surface wind +/- 300s (replacing 30)
#                - Updated the Apply_New_Fields function to re-calculate the Diff_Mode_S_To_Radar_GSPD_Flag
#                  and Global_Flag values. New values tests +/- 10kt from the surveillance GSPD
#                - Updated the Apply_New_Fields function to calculate the surface wind speed groups
#                - Used a filter value of -5 to 5kt for surface headwind for results
#                - Renamed the data set from the SQL query so as not to overwrite other data
# Version 2.8 - Updating LVNL script for Toronto
# (min IAS reduced to 80, min GSPD reduced to 100, added 3.5 to TBS table values)
# (Sep Acc Max adjusted to 20 for now, 'Apply_Achieved_Separation_Filter' temp commenting out of one section due to null data)
# Version 2.8 AH - Added a 3.5 separation distance to the out plots
#-------------------------------------------------------------------------#

rm(list = ls())

#-----------------------------------------------------------------------------------------------------------#
# Imports --------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#

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
library(tidyr)

#-----------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------#
# Config ---------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#
user <- "Andy Hyde"

# Choose the database you would like to use (IMPORTANT) and choose if you would like to take data from it (T)
#database <- "NavCan_UTMA_Validation_DB2"
#database <- "NavCan_TBS_Analysis_UTMA_Validation"
database <- "NavCan_TBS_V3"

get_connection <- T
use_database <- T
output_main_data <- T

# Establishes a database connection (192.168.1.39 or 192.168.1.23)
if (get_connection == T){
  con <- RODBC::odbcDriverConnect(connection=paste0("Driver={SQL Server};Server={192.168.1.23};Database={",database,"};Uid={ruser};Pwd={Th!nkruser};"))
}

# Version (determines output directory - must be string).
version <- "5.1.0 January and February Data"

# Set working directory.
# if (rstudioapi::isAvailable()) {
#   setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# } else {
#   wd <- Sys.glob(file.path("C:", "Users", "*", "Dropbox (Think Research)", "NATS Projects", "NATS LVNL Schiphol", "Phase 2", "20. LVNL Scripts", "Flying_Time_Analysis"))
#   while (any(!dir.exists(wd), length(wd) == 0)) wd <- readline("Please specify working directory: ")
#   setwd(wd)
# }
# base_dir <- getwd()
setwd(paste0("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis"))
base_dir <- getwd()

# Reference data directory
ref_data <- file.path(base_dir, "Inputs/Flying_Time_Analysis_Input")

# Output directory
out_data <- file.path(base_dir, "Outputs\\Flying_Time_Analysis_Output", paste0("v", version))
if (!dir.exists(out_data)) dir.create(out_data)
setwd(out_data)

# Output Adaptation Data?
output_ref_data <- T #Output adaptation
output_legacy <- T #Output Legacy Adaptation (output_ref_data must also be on)
adap_set <- 1 #1 for Surface wind group, 2 for Surface Headwind safety case

#-----------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------#
# Data Loading ---------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#

SQL_Query <- "SELECT * FROM vw_Flying_Time_Analysis"
RW_Query <- "SELECT * FROM tbl_Runway"
AF_Query <- "SELECT Airfield_Name FROM tbl_Airfield"
Wake_Query <- "SELECT * FROM tbl_Reference_Recat_Separation_Dist"
ROT_Query <- "SELECT * FROM tbl_Reference_ROT_Spacing_Dist"
Wake_Query_AC <- "SELECT * FROM tbl_Reference_ACTP_Wake_Separation_Dist"
ROT_Query_AC <- "SELECT * FROM tbl_Reference_ACTP_ROT_Spacing_Dist"
Wake_Query_OP <- "SELECT * FROM tbl_Reference_AC_Operator_Wake_Separation_Dist"
ROT_Query_OP <- "SELECT * FROM tbl_"


# This section loads the data locally if use_database = FALSE or from the above 
# SQL query if use_database = TRUE.
# MC 15/11 commented out the database here

#if (use_database == T){
  
  # Runway Data
  rw_sql <- sqlQuery(con, sprintf(RW_Query))
  #fwrite(rw_sql, file.path(ref_data, "rw_data_sql.csv"))
  
  # Airfield Data
  Airfield <- sqlQuery(con, sprintf(AF_Query))
  #fwrite(Airfield, file.path(ref_data, "Airfield.csv"))
  
  # Wake Distances Data
  wake_sep <- sqlQuery(con, sprintf(Wake_Query))
  #wake_sep_ac <- sqlQuery(con, Wake_Query_AC, stringsAsFactors = F)
  #wake_sep_op <- sqlQuery(con, Wake_Query_OP, stringsAsFactors = F)
  #fwrite(wake_sep, file.path(ref_data, "reference_wake_separation_dist.csv"))
  
  # ROT Distances Data
  ROT_sep <- sqlQuery(con, sprintf(ROT_Query))
  #ROT_sep_ac <- sqlQuery(con, ROT_Query_AC, stringsAsFactors = F)
  #ROT_sep_op <- sqlQuery(con, ROT_Query_OP, stringsAsFactors = F)
  #fwrite(ROT_sep, file.path(ref_data, "reference_ROT_separation_dist.csv"))
  
#}

#ROT_sep <- fread(file.path(ref_data, "ROT_Distances.csv"))

#rw_sql <- as.data.frame(fread(file.path(ref_data, "rw_data_sql.csv")))


#Airfield <- fread(file.path(ref_data, "Airfield.csv"))
Airfield$Airfield_Name <- as.character(Airfield$Airfield_Name)
Airfield <- as.character(Airfield)

#wake_sep <- as.data.frame(fread(file.path(ref_data, "reference_wake_separation_dist.csv")))
#ROT_sep <- as.data.frame(fread(file.path(ref_data, "reference_ROT_separation_dist.csv")))



tryCatch(
  if (use_database == T) {
    if (exists("con")) {
      data1 <- sqlQuery(con, sprintf(SQL_Query))
    }
  } else {
    data1 <- fread(file.path(ref_data, "vw_Flying_Time_Analysis.csv"))
  },
  error = function(e) stop("Validation view not found!")
)

if (output_main_data == T){
  fwrite(data1, file.path(ref_data, "vw_Flying_Time_Analysis.csv"))
}

# Loads the ICAO4 Reference Data 
#Aircraft_ICAO4 <- as.data.frame(fread(file.path(ref_data, "reference_aircraft_wake_icao4.csv")))
#wake_sep_ICAO4 <- as.data.frame(fread(file.path(ref_data, "reference_wake_separation_dist_icao4.csv")))
Aircraft_ICAO4 <- unique(sqlQuery(con, sprintf("SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy")))
names(Aircraft_ICAO4)[2] <- "ICAO_WTC"
wake_sep_ICAO4 <- sqlQuery(con, sprintf("SELECT * FROM tbl_DBS_Wake_Turbulence"))
names(wake_sep_ICAO4) <- c("Leader_ICAO_WTC","Follower_ICAO_WTC","Reference_Wake_Separation_Distance")
wake_sep_ICAO4$Reference_Wake_Separation_Distance <- wake_sep_ICAO4$Reference_Wake_Separation_Distance /1852
# Ensure data1 is a data table

data1 <- as.data.table(data1)

# Create a copy of the original dataset
data1_original <- data1

# Change direction of surface wind from "direction from" to "direction to"
#data1 <- mutate(data1, Surface_Wind_HDG = (Surface_Wind_HDG + 180)%%360)

#-----------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------#
# Functions ------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#

# Adaptation writing Function
Write_Reference_Adaptation <- function(Data, Table, Path, Airfield){
  
  Full_Path <- file.path(Path, paste0("Populate_", Table, "_", Airfield))
  fwrite(Data, Full_Path)
  
}

#-----------------------------------------------------------------------------------------------------------#
# Pre-processing Functions
#-----------------------------------------------------------------------------------------------------------#

# This function applies the relevant new fields (and updates others) to the originaul data. 
# This function should be used on the ORIGINAL dataset for consistency.
### Add to this function (or create new one) to manipulate GWCS stage 2 filtering (MC already started)

Apply_New_Fields <- function(df) {
  df <- rename(df, Runway = Landing_Runway)
  df <- left_join(df, rw, by = c("Runway"))
  df <- mutate(df, Surface_Headwind = calculate_headwind_component(Heading, Surface_Wind_HDG, Surface_Wind_SPD))
  df <- mutate(df, Surface_Headwind_Gp = cut(Surface_Headwind, breaks = hw_groups),
               Surface_Wind_Gp = cut(Surface_Wind_SPD, breaks = sw_groups))
  return(df)
}


# This function converts degrees into radians.

radians <- function(degrees){
  radians <- degrees * pi / 180
  return(radians)
}

# This function converts radians into degrees.

degrees <- function(radians){
  degrees <- radians * 180 / pi
  return(degrees)
}

# This function calculates the difference between two angles in degrees.

angle_diff <- function(angle_a, angle_b) {
  angle_diff <- atan2(sin(radians(angle_a) - radians(angle_b)), cos(radians(angle_a) - radians(angle_b)))
  return(degrees(angle_diff))
}

# This function returns the headwind component along the runway heading
# Runway_Heading is the runway heading in degrees, relating to the heading flown by aircraft as they land
# Wind_Heading is the wind heading from the database in degrees and is in direction "from".
# Wind_Speed is in kts

calculate_headwind_component <- function(runway_hdg, wind_hdg, wind_spd){
  
  runway_hdg <- as.numeric(runway_hdg)
  wind_hdg <- as.numeric(wind_hdg)
  wind_spd <- as.numeric(wind_spd)
  head_diff <- angle_diff(runway_hdg, wind_hdg)
  
  headwind_component <- wind_spd * cos(radians(head_diff)) 
  return(headwind_component)
}


# This function matches L-F pairs to their ICAO4 WTCs and calculates
# the difference between observed/required ICAO4 separation distances.
# A filter is then applied to remove pairs with differences > Max_Diff.
# This filter is to gain a dataset for pressured operations.
### Want to generalise this to any wake scheme for future clients

Apply_Achieved_Separation_Filter <- function(df, sep_accuracy_max, sep_accuracy_max_a380){
  
  df <- left_join(df, Aircraft_ICAO4, by=c("Leader_Aircraft_Type"="Aircraft_Type")) %>% 
    rename(Leader_ICAO4_WTC=ICAO_WTC)
  
  df <- left_join(df, Aircraft_ICAO4, by=c("Aircraft_Type"="Aircraft_Type")) %>% 
    rename(Follower_ICAO4_WTC=ICAO_WTC)
  
  ref_wake_icao4_join <- wake_sep_ICAO4 %>% mutate(ICAO4_Leader_Follower_Pair = 
                                                     paste0(Leader_ICAO_WTC, Follower_ICAO_WTC)) %>% 
                                            select(-c(Leader_ICAO_WTC, Follower_ICAO_WTC))
  
  df <- mutate(df, ICAO4_Leader_Follower_Pair = paste0(Leader_ICAO4_WTC, Follower_ICAO4_WTC))
  df <- left_join(df, ref_wake_icao4_join,by=c("ICAO4_Leader_Follower_Pair")) %>% select(-c("ICAO4_Leader_Follower_Pair"))
  
  Min_Rdr <- MRS
  
  df <- rename(df, Ref_Wake_Separation_ICAO4=Reference_Wake_Separation_Distance)
  
  df <- mutate(df, Ref_All_Separation_ICAO4 = ifelse(is.na(Ref_Wake_Separation_ICAO4), Min_Rdr, Ref_Wake_Separation_ICAO4))
  df <- mutate(df, ICAO_Sep_Accuracy_0DME = (Delivered_0DME_Separation/1852) - Ref_All_Separation_ICAO4)
  # Temp commented out - issue with data ICAO_Sep_Accuracy_0DME has a lot of null values
  #df <- rbind(filter(df, Leader_Aircraft_Type == "A388" & df$ICAO_Sep_Accuracy_0DME <= sep_accuracy_max_a380), 
  #            filter(df, Leader_Aircraft_Type != "A388" & df$ICAO_Sep_Accuracy_0DME <= sep_accuracy_max))
  
  return(df)
}








#-----------------------------------------------------------------------------------------------------------#
# Main Processing Functions
#-----------------------------------------------------------------------------------------------------------#

# This function takes the data after spreading IAS by DME_Seg, cuts DataFrame for IAS
# between start and dist, and averages these values

avg_spd2 <-  function(df, start, dist) {
  df <- df[,(start+2):(dist+start+1)]
  result <- apply(df, 1, mean, na.rm=FALSE)
  return(result)
}

# This function operates AFTER the avg_spd function and calculates averages between
# two successive avg_spd output values (xDME - x+1DME) for half mile average IAS

half_mile2 <- function(df, start, dist, min, max){
  half <- max+start+3+dist-min
  two <- df[half:(half+1)]
  result <- apply(two, 1, mean, na.rm=FALSE)
  return(result)
}

# This function loops between a min and max DME segment to provide average IAS values for
# (start - kDME) where k is in the range min:max. This also provides the averages between
# each successive IAS - giving half mile separation values for certain VFR/ROT/Wake scenarios 
# Currently gives warning message for melt function. Change to pivot_longer in next version  

Get_Speeds <- function(df, start, min, max, halves){
  
  message("Getting required flight information...")
  dfn <- select(df, Flight_Plan_ID, Wake_Cat, Follower_ICAO4_WTC, Aircraft_Type, 
                Runway, Surface_Headwind_Gp, Surface_Wind_Gp) %>% unique()
  message("Isolated filtering details for each flight.")
  
  dft <- select(df, Flight_Plan_ID, DME_Seg, Ave_SPD) %>% filter(DME_Seg<=(max_dist + start))
  dft_piv <- pivot_wider(dft, names_from = DME_Seg, values_from = Ave_SPD, names_prefix = "Speed_DME_") 
  
  for (dist in min_dist:max_dist){
    speeds <- as.data.frame(avg_spd2(dft_piv, start, dist))
    names(speeds) <- paste0(dist)
    dft_piv <- as.data.frame(cbind(dft_piv, speeds))
    message("Bound Average Speeds for Separation of ", dist, ", From ", start, "DME to ", (dist+start), "DME")
  }
  
  for (dist in halves){
    speeds <- as.data.frame(half_mile2(dft_piv, start, dist, min_dist, max_dist))
    names(speeds) <- paste0((dist+0.5))
    dft_piv <- as.data.frame(cbind(dft_piv, speeds))
    message("Bound Average Speeds for Separation of ", dist+0.5, ", From ", start, "DME to ", (dist+start+0.5), "DME")
  }
  
  id <- dft_piv[1:1]
  dft_piv2 <- dft_piv[(max_dist+start+3):ncol(dft_piv)]
  dft_piv2 <- cbind(id, dft_piv2)
  
  speed_output <- pivot_longer(dft_piv2, cols=2:ncol(dft_piv2), names_to="Separation_Distance", values_to="Ave_SPD")
  speed_output$Separation_Distance <- as.numeric(speed_output$Separation_Distance)
  
  speed_output <- right_join(speed_output, dfn, by=c("Flight_Plan_ID"))
  speed_values <- filter(speed_output, !is.na(Ave_SPD))
  
  return(speed_values)
  
}
#-----------------------------------------------------------------------------------------------------------#
# Summary & Output Functions
#-----------------------------------------------------------------------------------------------------------#

# This function creates the "Flying_Time_Analysis.csv" output.
# Includes all necessary separation distances for each follower WTC 
# and each surface wind group.

Create_Summary_Main <- function(df, start) {
  summary <- summarise(df,
                       Count = sum(!is.na(Ave_SPD)),
                       Median_Speed = median(Ave_SPD, na.rm=TRUE),
                       Pc05_Speed = quantile(Ave_SPD, 0.05, na.rm = TRUE),
                       Pc95_Speed = quantile(Ave_SPD, 0.95, na.rm = TRUE)) %>%
    mutate(DME_Start = as.numeric(start),
           DME_End = Separation_Distance+start) %>% filter(Count>0)
  return(summary)
}

Create_ICAO_seps <- function(summary){
  
  seps <- right_join(summary, wake_sep_ICAO4, by=c("Follower_ICAO4_WTC"="Follower_ICAO_WTC",
                                                                                   "Separation_Distance"="Reference_Wake_Separation_Distance")) %>%
    select(Leader_ICAO_WTC, everything()) %>% rename(Leader_ICAO4_WTC = Leader_ICAO_WTC) %>%
    mutate(Flying_Time = 3600 * Separation_Distance/Median_Speed)
  return(seps)
  
}

Create_seps <- function(summary, wake_sep, ROT_sep, level){
  wake_sep1 <- rename(wake_sep, Ref_Distance = Reference_Wake_Separation_Distance) %>%
    mutate(Ref_Distance = Ref_Distance/1852,
           Constraint = "Wake")
  ROT_sep1 <- rename(ROT_sep, Ref_Distance = Reference_ROT_Spacing_Distance) %>%
    mutate(Ref_Distance = Ref_Distance,
           Constraint = "ROT")
  for (i in 1:length(rw$Runway)){
    rwwake <- mutate(wake_sep1, Runway = rw$Runway[i])
    if (i == 1){
      wake_full <- rwwake
    }
    else{wake_full <- rbind(wake_full, rwwake)}
  }
  wake_full <- select(wake_full, Runway, everything())
  
  all_seps <- rbind(wake_full, ROT_sep1)
  if (level == "Wake"){seps <- right_join(summary, all_seps, by=c("Wake_Cat" = "Follower_WTC", "Separation_Distance" = "Ref_Distance"))}
  if (level == "Aircraft"){seps <- right_join(summary, all_seps, by=c("Aircraft_Type" = "Follower_Aircraft_Type", "Separation_Distance" = "Ref_Distance"))}
  if (level == "Operator"){seps <- right_join(summary, all_seps, by=c("Operator" = "Follower_Operator", "Aircraft_Type" = "Follower_Aircraft_Type", "Separation_Distance" = "Ref_Distance"))}
  seps <- seps %>%
    select(Runway, Leader_WTC, everything()) %>% rename(Follower_WTC = Wake_Cat) %>%
    mutate(Flying_Time = 3600 * Separation_Distance/Median_Speed)
  seps <- seps[order(seps$Runway, seps$Separation_Distance),]
  seps <- arrange(seps, Runway, Separation_Distance)
  return(seps)
  
}

Create_Wake_Adaptation <- function(seps, level){
  wake <- filter(seps, Constraint == "Wake") %>% select(-c("Runway", "Pc05_Speed", "Pc95_Speed",
                                                           "Count", "DME_Start", "DME_End", "Constraint")) %>% unique() %>%
    rename(Reference_Wake_Separation_Distance = Separation_Distance,
           Assumed_Wake_Separation_IAS = Median_Speed,
           Reference_Wake_Separation_Time = Flying_Time) %>%
    mutate(Assumed_Wake_Separation_IAS = round(Assumed_Wake_Separation_IAS, 0),
           Reference_Wake_Separation_Time= round(Reference_Wake_Separation_Time, 0))
  if (level == "Wake"){wake <- arrange(wake, Leader_WTC, Follower_WTC)}
  if (level == "Aircraft"){wake <- arrange(wake, Leader_Aircraft_Type, Follower_Aircraft_Type)}
  if (level == "Operator"){wake <- arrange(wake, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator)}
  return(wake)
}

Create_ROT_Adaptation <- function(seps, level){
  rot <- filter(seps, Constraint == "ROT") %>% select(-c("Pc05_Speed", "Pc95_Speed",
                                                           "Count", "DME_Start", "DME_End", "Constraint")) %>%
    rename(Reference_ROT_Spacing_Distance = Separation_Distance,
           Assumed_ROT_Spacing_IAS = Median_Speed,
           Reference_ROT_Spacing_Time = Flying_Time) %>%
    mutate(Assumed_ROT_Spacing_IAS = round(Assumed_ROT_Spacing_IAS, 0),
           Reference_ROT_Spacing_Time= round(Reference_ROT_Spacing_Time, 0))
  if (level == "Wake"){rot <- arrange(rot, Runway, Leader_WTC, Follower_WTC)}
  if (level == "Aircraft"){rot <- arrange(rot, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type)}
  if (level == "Operator"){rot <- arrange(rot, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator)}
  return(rot)
}

Create_TBS_Adaptation <- function(seps){
  seps <- filter(seps, Separation_Distance %in% TBS_Values)
  tbs <- seps %>% group_by(Separation_Distance) %>% summarise(
    Reference_Wake_Separation_Time = weighted.mean(Flying_Time, Count),
    Assumed_Wake_Separation_IAS = weighted.mean(Median_Speed, Count)) %>%
    rename(Reference_Wake_Separation_Distance = Separation_Distance) %>%
    mutate(Reference_Wake_Separation_Time = round(Reference_Wake_Separation_Time, 0),
           Assumed_Wake_Separation_IAS = round(Assumed_Wake_Separation_IAS, 0))
  return(tbs)
}

Create_Legacy_Adaptation <- function(seps){
  seps <- select(seps, -c("Pc05_Speed", "Pc95_Speed",
                          "Count", "DME_Start", "DME_End")) %>% 
    rename(Assumed_WT_IAS = Median_Speed,
           Leader_WVI = Leader_ICAO4_WTC,
           Follower_WVI = Follower_ICAO4_WTC,
           WT_Separation_Distance = Separation_Distance,
           WT_Separation_Time = Flying_Time) %>%
    mutate(Assumed_WT_IAS = round(Assumed_WT_IAS, 0),
           WT_Separation_Time = round(WT_Separation_Time, 0))
}

Use_Speed_Parameter <- function(data, speed_type){
  if (speed_type == "IAS"){input <- data1 %>% mutate(Ave_SPD = Ave_Mode_S_IAS)}
  if (speed_type =="GSPD"){input <- data1 %>% mutate(Ave_SPD = Ave_Mode_S_GSPD)}
  if (speed_type =="TAS"){input <- data1 %>% mutate(Ave_SPD = Ave_Mode_S_TAS)}
  if (speed_type =="WE"){input <- data1 %>% mutate(Ave_SPD = Ave_Wind_Effect_IAS)}
  if (speed_type =="TRACK"){input <- data1 %>% mutate(Ave_SPD = Ave_Track_SPD)}
  if (speed_type == "DERIVED TRACK"){input <- mutate(data1, Ave_SPD = ifelse(abs(Max_Track_Time - Min_Track_Time) > 0,
                                                                       3600 * (Max_RTT - Min_RTT) / (Max_Track_Time - Min_Track_Time), NA))}
  
  return(input)
}

# Updates to date/time


#-----------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------#
# Processing -----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#
speed_type <- 'IAS'   # Not yet functional, intend on bringing over GSPD functionality for universality among clients
#-----------------------------------------------------------------------------------------------------------#
# Parameters (Adaptable)
#-----------------------------------------------------------------------------------------------------------#
safe_hw_lower <- -5 #Lower bound of Safety Case Surface Headwind
safe_hw_upper <- 5 #Upper bound of Safety Case Surface Headwind
safe_sw <- 3 #Upper bound of ideal case Surface Wind Speed
MRS <- 3 #Generic Minimum Radar Separation value

hw_groups <- c(-Inf, -15, safe_hw_lower, safe_hw_upper, 10, 20, 30, Inf)
sw_groups <- c(0, safe_sw, 6, 10, 20, 30, Inf)

start <- 0 #Delivery Point in DME
min_dist <- 2 #Minimum Separation distance, use 2 if 2.5 needed
max_dist <- 8 #Maximum Separation distance
halves <- c(2, 3, 4) #Put all half mile distances in here as their integer, e.g. 3 for 3.5 ROT distances

sep_accuracy_max <- 20 # 3 for LVNL (set to 20 for now)
sep_accuracy_max_a380 <- 20 # 6 for LVNL (set to 20 for now)
sep_accuracy_max_tight <- 1.5
sep_accuracy_max_a380_tight <- 3
sep_accuracy_max_lean <- 5
sep_accuracy_max_a380_lean <- 8

max_ias <- 200
min_ias <- 80 # 100 originally
max_gspd <- 200
min_gspd <- 100 # 200 originally

TBS_Values <- c(3, 3.5, 4, 5, 6, 7, 8) #For LVNL. NAV require 3.5
Plot_Values <- c(3,3.5, 4, 5, 6, 7, 8)
#-----------------------------------------------------------------------------------------------------------#
# Pre-processing
#-----------------------------------------------------------------------------------------------------------#
# This prepares the runway data in the required format
rw <- rw_sql %>% select(Runway = Runway_Name, Runway_Group, Heading) %>% mutate(Heading = degrees(Heading), Min_Rdr = MRS)
#rm(rw_sql)

# First remove all duplicate wind segs
data1 <- data1_original
data1 <- data1[!duplicated(data1[,c('Mode_S_Wind_Seg_ID')]),]
data1 <- data1[!duplicated(data1),]

# Add Surface wind and runway fields with Apply_New_Fields
data1 <- Apply_New_Fields(data1)

# Initial Filters to remove Not in Trail pairs and Flagged segments
data1 <- filter(data1, Landing_Pair_Type != "Not_In_Trail") # For LVNL only
data1 <- filter(data1, Global_Flag == F)
data1 <- filter(data1, !is.na(Wake_Cat), !is.na(Surface_Headwind_Gp), !is.na(Surface_Wind_Gp), Wake_Cat != "")

# Filter for queued arrivals (pressured operations)
data1 <- Apply_Achieved_Separation_Filter(data1, sep_accuracy_max, sep_accuracy_max_a380)
#data1 <- Apply_Achieved_Separation_Filter(data1, sep_accuracy_max_lean, sep_accuracy_max_a380_lean)
#data1 <- Apply_Achieved_Separation_Filter(data1, sep_accuracy_max_tight, sep_accuracy_max_a380_tight)

# Extra filtering if necessary
data1 <- filter(data1, Ave_Mode_S_IAS <= max_ias || Ave_Mode_S_IAS >= min_ias) #IAS extreme values
data1 <- filter(data1, Ave_Mode_S_GSPD <= max_gspd || Ave_Mode_S_GSPD >= min_gspd) #GSPD extreme values
#-----------------------------------------------------------------------------------------------------------#
# Main processing
#-----------------------------------------------------------------------------------------------------------#
# Choose What speed you want to analyse. Use IAS for LVNL
input <- data1 %>% mutate(Ave_SPD = Ave_Mode_S_IAS)

# Get Average speed values across separation distances
IAS_values <- Get_Speeds(input, 0, min_dist, max_dist, halves)

# Summary tables by RECAT-EU and ICAO4 categories & Surface Wind/HW Groups 
IAS_Summary_Recat_SW <- IAS_values %>% group_by(Wake_Cat, Separation_Distance, Surface_Wind_Gp) %>%
  Create_Summary_Main(start) %>% ungroup()

IAS_Summary_Recat_SHW <- IAS_values %>% group_by(Wake_Cat, Separation_Distance, Surface_Headwind_Gp) %>%
  Create_Summary_Main(start) %>% ungroup()

# IAS_Summary_ACTP_SW <- IAS_values %>% group_by(Aircraft_Type, Separation_Distance, Surface_Wind_Gp) %>%
#   Create_Summary_Main(start) %>% ungroup()
# 
# IAS_Summary_ACTP_SHW <- IAS_values %>% group_by(Aircraft_Type, Separation_Distance, Surface_Headwind_Gp) %>%
#   Create_Summary_Main(start) %>% ungroup()
# 
# IAS_Summary_AC_Operator_SW <- IAS_values %>% group_by(Aircraft_Type, Operator, Separation_Distance, Surface_Wind_Gp) %>%
#   Create_Summary_Main(start) %>% ungroup()
# 
# IAS_Summary_AC_Operator_SHW <- IAS_values %>% group_by(Aircraft_Type, Operator, Separation_Distance, Surface_Headwind_Gp) %>%
#   Create_Summary_Main(start) %>% ungroup()

# IAS_Summary_ICAO_SW <- IAS_values %>% group_by(Follower_ICAO4_WTC, Separation_Distance, Surface_Wind_Gp) %>%
#   Create_Summary_Main(start) %>% ungroup()
# 
# IAS_Summary_ICAO_SHW <- IAS_values %>% group_by(Follower_ICAO4_WTC, Separation_Distance, Surface_Headwind_Gp) %>%
#   Create_Summary_Main(start) %>% ungroup()

# MC Additional For Landing Runway

IAS_Summary_Recat_SHW_Runway <- IAS_values %>% group_by(Wake_Cat, Runway, Separation_Distance, Surface_Headwind_Gp) %>%
  Create_Summary_Main(start) %>% ungroup()

# Summary tables for all separation constraints for each scheme (RECAT: Wake & ROT by Runway, ICAO4: Wake)
IAS_Summary_Recat_SW_Seps <- Create_seps(IAS_Summary_Recat_SW, wake_sep, ROT_sep, level = "Wake") %>% filter(Surface_Wind_Gp == "(0,3]")
IAS_Summary_Recat_SHW_Seps <- Create_seps(IAS_Summary_Recat_SHW, wake_sep, ROT_sep, level = "Wake") %>% filter(Surface_Headwind_Gp == "(-5,5]")
# IAS_Summary_ACTP_SW_Seps <- Create_seps(IAS_Summary_Recat_SW, wake_sep_ac, ROT_sep_ac, level = "Aircraft") %>% filter(Surface_Wind_Gp == "(0,3]")
# IAS_Summary_ACTP_SHW_Seps <- Create_seps(IAS_Summary_Recat_SHW, wake_sep_ac, ROT_sep_ac, level = "Aircraft") %>% filter(Surface_Headwind_Gp == "(-5,5]")
# IAS_Summary_AC_Operator_SW_Seps <- Create_seps(IAS_Summary_Recat_SW, wake_sep_op, ROT_sep_op, level = "Operator") %>% filter(Surface_Wind_Gp == "(0,3]")
# IAS_Summary_AC_Operator_SHW_Seps <- Create_seps(IAS_Summary_Recat_SHW, wake_sep_op, ROT_sep_op, level = "Operator") %>% filter(Surface_Headwind_Gp == "(-5,5]")
# IAS_Summary_ICAO_SW_Seps <- Create_ICAO_seps(IAS_Summary_ICAO_SW) %>% filter(Surface_Wind_Gp == "(0,3]")
# IAS_Summary_ICAO_SHW_Seps <- Create_ICAO_seps(IAS_Summary_ICAO_SHW) %>% filter(Surface_Headwind_Gp == "(-5,5]")

# Some additional Analysis by landing runway

IAS_Summary_Recat_SHW_Runway <- inner_join(IAS_Summary_Recat_SHW_Runway, select(IAS_Summary_Recat_SHW, Wake_Cat, Separation_Distance, Surface_Headwind_Gp, Count, Median_Speed), by = c("Wake_Cat", "Separation_Distance", "Surface_Headwind_Gp")) %>%
    mutate(Median_Diff = Median_Speed.x - Median_Speed.y)
fwrite(IAS_Summary_Recat_SHW_Runway, "Flying_Time_Analysis_Runway.csv")


#----------------------------------------------,-------------------------------------------------------------#
# Adaptation Production
# (1) - Surface Wind 0-3kts, (2) - Surface Headwind -5-5 kts
#-----------------------------------------------------------------------------------------------------------#

# TBS Table Adaptation

TBS_base_1 <- Create_TBS_Adaptation(IAS_Summary_Recat_SW_Seps)
TBS_base_2 <- Create_TBS_Adaptation(IAS_Summary_Recat_SHW_Seps)

TBS_times_1 <- select(TBS_base_1, Reference_Wake_Separation_Distance, Reference_Wake_Separation_Time)
TBS_speeds_1 <- select(TBS_base_1, Reference_Wake_Separation_Distance, Assumed_Wake_Separation_IAS)

TBS_times_2 <- select(TBS_base_2, Reference_Wake_Separation_Distance, Reference_Wake_Separation_Time)
TBS_speeds_2 <- select(TBS_base_2, Reference_Wake_Separation_Distance, Assumed_Wake_Separation_IAS)

# RECAT Adaptation

wake_base_1 <- Create_Wake_Adaptation(IAS_Summary_Recat_SW_Seps, "Wake") %>% select(-c("Surface_Wind_Gp"))
wake_base_2 <- Create_Wake_Adaptation(IAS_Summary_Recat_SHW_Seps, "Wake") %>% select(-c("Surface_Headwind_Gp"))

wake_times_1 <- select(wake_base_1, Leader_WTC, Follower_WTC, Reference_Wake_Separation_Time)
wake_dists_1 <- select(wake_base_1, Leader_WTC, Follower_WTC, Reference_Wake_Separation_Distance)
wake_speeds_1 <- select(wake_base_1, Leader_WTC, Follower_WTC, Assumed_Wake_Separation_IAS)

wake_times_2 <- select(wake_base_2, Leader_WTC, Follower_WTC, Reference_Wake_Separation_Time)
wake_dists_2 <- select(wake_base_2, Leader_WTC, Follower_WTC, Reference_Wake_Separation_Distance)
wake_speeds_2 <- select(wake_base_2, Leader_WTC, Follower_WTC, Assumed_Wake_Separation_IAS)

ROT_base_1 <- Create_ROT_Adaptation(IAS_Summary_Recat_SW_Seps, "Wake") %>% select(-c("Surface_Wind_Gp"))
ROT_base_2 <- Create_ROT_Adaptation(IAS_Summary_Recat_SHW_Seps, "Wake") %>% select(-c("Surface_Headwind_Gp"))

ROT_times_1 <- select(ROT_base_1, Runway, Leader_WTC, Follower_WTC, Reference_ROT_Spacing_Time)
ROT_dists_1 <- select(ROT_base_1, Runway, Leader_WTC, Follower_WTC, Reference_ROT_Spacing_Distance)
ROT_speeds_1 <- select(ROT_base_1, Runway, Leader_WTC, Follower_WTC, Assumed_ROT_Spacing_IAS)

ROT_times_2 <- select(ROT_base_2, Runway, Leader_WTC, Follower_WTC, Reference_ROT_Spacing_Time)
ROT_dists_2 <- select(ROT_base_2, Runway, Leader_WTC, Follower_WTC, Reference_ROT_Spacing_Distance)
ROT_speeds_2 <- select(ROT_base_2, Runway, Leader_WTC, Follower_WTC, Assumed_ROT_Spacing_IAS)

# Aircraft Adaptation

# wake_base_3 <- Create_Wake_Adaptation(IAS_Summary_ACTP_SW_Seps, "Aircraft") %>% select(-c("Surface_Wind_Gp"))
# wake_base_4 <- Create_Wake_Adaptation(IAS_Summary_ACTP_SHW_Seps, "Aircraft") %>% select(-c("Surface_Headwind_Gp"))
# 
# wake_times_3 <- select(wake_base_3, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_Wake_Separation_Time)
# wake_dists_3 <- select(wake_base_3, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_Wake_Separation_Distance)
# wake_speeds_3 <- select(wake_base_3, Leader_Aircraft_Type, Follower_Aircraft_Type, Assumed_Wake_Separation_IAS)
# 
# wake_times_4 <- select(wake_base_4, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_Wake_Separation_Time)
# wake_dists_4 <- select(wake_base_4, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_Wake_Separation_Distance)
# wake_speeds_4 <- select(wake_base_4, Leader_Aircraft_Type, Follower_Aircraft_Type, Assumed_Wake_Separation_IAS)
# 
# ROT_base_3 <- Create_ROT_Adaptation(IAS_Summary_ACTP_SW_Seps, "Aircraft") %>% select(-c("Surface_Wind_Gp"))
# ROT_base_4 <- Create_ROT_Adaptation(IAS_Summary_ACTP_SHW_Seps, "Aircraft") %>% select(-c("Surface_Headwind_Gp"))
# 
# ROT_times_3 <- select(ROT_base_3, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_ROT_Spacing_Time)
# ROT_dists_3 <- select(ROT_base_3, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_ROT_Spacing_Distance)
# ROT_speeds_3 <- select(ROT_base_3, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Assumed_ROT_Spacing_IAS)
# 
# ROT_times_4 <- select(ROT_base_4, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_ROT_Spacing_Time)
# ROT_dists_4 <- select(ROT_base_4, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_ROT_Spacing_Distance)
# ROT_speeds_4 <- select(ROT_base_4, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Assumed_ROT_Spacing_IAS)

# Operator Adaptation

# wake_base_5 <- Create_Wake_Adaptation(IAS_Summary_AC_Operator_SW_Seps, "Operator") %>% select(-c("Surface_Wind_Gp"))
# wake_base_6 <- Create_Wake_Adaptation(IAS_Summary_AC_Operator_SHW_Seps, "Operator") %>% select(-c("Surface_Headwind_Gp"))
# 
# wake_times_5 <- select(wake_base_5, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_Wake_Separation_Time)
# wake_dists_5 <- select(wake_base_5, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_Wake_Separation_Distance)
# wake_speeds_5 <- select(wake_base_5, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Assumed_Wake_Separation_IAS)
# 
# wake_times_6 <- select(wake_base_6, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_Wake_Separation_Time)
# wake_dists_6 <- select(wake_base_6, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_Wake_Separation_Distance)
# wake_speeds_6 <- select(wake_base_6, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Assumed_Wake_Separation_IAS)
# 
# ROT_base_5 <- Create_ROT_Adaptation(IAS_Summary_AC_Operator_SW_Seps, "Aircraft") %>% select(-c("Surface_Wind_Gp"))
# ROT_base_6 <- Create_ROT_Adaptation(IAS_Summary_AC_Operator_SHW_Seps, "Aircraft") %>% select(-c("Surface_Headwind_Gp"))
# 
# ROT_times_5 <- select(ROT_base_5, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_ROT_Spacing_Time)
# ROT_dists_5 <- select(ROT_base_5, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_ROT_Spacing_Distance)
# ROT_speeds_5 <- select(ROT_base_5, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Assumed_ROT_Spacing_IAS)
# 
# ROT_times_6 <- select(ROT_base_6, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_ROT_Spacing_Time)
# ROT_dists_6 <- select(ROT_base_6, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_ROT_Spacing_Distance)
# ROT_speeds_6 <- select(ROT_base_6, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Assumed_ROT_Spacing_IAS)

# Legacy Adaptation

# ICAO_wake_base_1 <- Create_Legacy_Adaptation(IAS_Summary_ICAO_SW_Seps) %>% select(-c("Surface_Wind_Gp"))
# ICAO_wake_base_2 <- Create_Legacy_Adaptation(IAS_Summary_ICAO_SHW_Seps) %>% select(-c("Surface_Headwind_Gp"))
# 
# ICAO_wake_times_1 <- select(ICAO_wake_base_1, Leader_WVI, Follower_WVI, WT_Separation_Time)
# ICAO_wake_times_2 <- select(ICAO_wake_base_2, Leader_WVI, Follower_WVI, WT_Separation_Time)
# ICAO_wake_dists_1 <- select(ICAO_wake_base_1, Leader_WVI, Follower_WVI, WT_Separation_Distance)
# ICAO_wake_dists_2 <- select(ICAO_wake_base_2, Leader_WVI, Follower_WVI, WT_Separation_Distance)
# ICAO_wake_speeds_1 <- select(ICAO_wake_base_1, Leader_WVI, Follower_WVI, Assumed_WT_IAS)
# ICAO_wake_speeds_2 <- select(ICAO_wake_base_2, Leader_WVI, Follower_WVI, Assumed_WT_IAS)

# Output adaptation files



if (output_ref_data == T){
  if (adap_set == 1){
    fwrite(TBS_speeds_1, "Populate_tbl_Assumed_TBS_Table_IAS_EHAM.csv")
    fwrite(TBS_times_1, "Populate_tbl_Reference_TBS_Table_Time_EHAM.csv")
    
    fwrite(wake_times_1, "Populate_tbl_Reference_Recat_Separation_Time_EHAM.csv")
    fwrite(wake_speeds_1, "Populate_tbl_Assumed_Recat_Separation_IAS_EHAM.csv")
    fwrite(ROT_dists_1, "Populate_tbl_Reference_ROT_Spacing_Dist_EHAM.csv")
    fwrite(ROT_times_1, "Populate_tbl_Reference_ROT_Spacing_Time_EHAM.csv")
    fwrite(ROT_speeds_1, "Populate_tbl_Assumed_ROT_Spacing_IAS_EHAM.csv")
    
    # fwrite(wake_times_3, "Populate_tbl_Reference_ACTP_Wake_Separation_Time_EHAM.csv")
    # fwrite(wake_speeds_3, "Populate_tbl_Assumed_ACTP_Wake_Separation_IAS_EHAM.csv")
    # fwrite(ROT_dists_3, "Populate_tbl_Reference_ACTP_ROT_Spacing_Dist_EHAM.csv")
    # fwrite(ROT_times_3, "Populate_tbl_Reference_ACTP_ROT_Spacing_Time_EHAM.csv")
    # fwrite(ROT_speeds_3, "Populate_tbl_Assumed_ACTP_ROT_Spacing_IAS_EHAM.csv")
    # 
    # fwrite(wake_times_5, "Populate_tbl_Reference_AC_Operator_Wake_Separation_Time_EHAM.csv")
    # fwrite(wake_speeds_5, "Populate_tbl_Assumed_AC_Operator_Wake_Separation_IAS_EHAM.csv")
    # fwrite(ROT_dists_5, "Populate_tbl_Reference_AC_Operator_ROT_Spacing_Dist_EHAM.csv")
    # fwrite(ROT_times_5, "Populate_tbl_Reference_AC_Operator_ROT_Spacing_Time_EHAM.csv")
    # fwrite(ROT_speeds_5, "Populate_tbl_Assumed_AC_Operator_ROT_Spacing_IAS_EHAM.csv")
    
    # if (output_legacy == T){
    #   fwrite(ICAO_wake_times_1, "Populate_tbl_TBS_Wake_Turbulence.csv")
    #   fwrite(ICAO_wake_dists_1, "Populate_tbl_DBS_Wake_Turbulence.csv")
    #   fwrite(ICAO_wake_speeds_1, "Populate_tbl_IAS_Wake_Turbulence.csv")
    # }
  }
  if (adap_set == 2){
    fwrite(wake_times_2, "Populate_tbl_Reference_Recat_Separation_Time_EHAM.csv")
    fwrite(wake_speeds_2, "Populate_tbl_Assumed_Recat_Separation_IAS_EHAM.csv")
    fwrite(TBS_speeds_2, "Populate_tbl_Assumed_TBS_Table_IAS_EHAM.csv")
    fwrite(TBS_times_2, "Populate_tbl_Reference_TBS_Table_Time_EHAM.csv")
    fwrite(ROT_dists_2, "Populate_tbl_Reference_ROT_Spacing_Dist_EHAM.csv")
    fwrite(ROT_times_2, "Populate_tbl_Reference_ROT_Spacing_Time_EHAM.csv")
    fwrite(ROT_speeds_2, "Populate_tbl_Assumed_ROT_Spacing_IAS_EHAM.csv")
    # if (output_legacy == T){
    #   fwrite(ICAO_wake_times_2, "Populate_tbl_TBS_Wake_Turbulence.csv")
    #   fwrite(ICAO_wake_dists_2, "Populate_tbl_DBS_Wake_Turbulence.csv")
    #   fwrite(ICAO_wake_speeds_2, "Populate_tbl_IAS_Wake_Turbulence.csv")
    # }
  }
  
}

fwrite(IAS_Summary_Recat_SHW_Seps, "Flying_Time_Analysis_Seps.csv")
fwrite(IAS_Summary_Recat_SHW, "Flying_Time_Analysis.csv")


#-----------------------------------------------------------------------------------------------------------#
# Generate Plots
#-----------------------------------------------------------------------------------------------------------#

# Case = 0: All Wind
# Case = 1: (-5, 5)kts Surface Headwind
# Case = 2: (0, 3)kts Surface Wind
FT_Boxplot_General <- function(speeds, start, case, facet, low_bound, high_bound){
  
  plot_data <- speeds
  plot_data <- plot_data %>% filter(Separation_Distance %in% c(3, 3.5, 4, 5, 6, 7, 8))
  plot_data$Sep_Dist_Factor = factor(plot_data$Separation_Distance)
  plot_data <- mutate(plot_data, Flying_Time = 3600 * (Separation_Distance / Ave_SPD))
  
  if (facet == "Wake"){facet_lab <- "RECAT-EU Follower WTC"}
  if (facet == "Runway"){facet_lab <- "Runway"}
  
  if (case == 0){case_lab <- "(All Wind)"}
  if (case == 1){case_lab <- "((-5, 5) kts Surface Headwind)"
                 filt <- "(-5,5]"
                 plot_data <- filter(plot_data, Surface_Headwind_Gp == filt)}
  if (case == 2){case_lab <- "((0, 3) kts Surface Wind)"
                 filt <- "(0,3]"
                 plot_data <- filter(plot_data, Surface_Wind_Gp == filt)}
  
  plot_title <- paste0("Flying Time Boxplot over Separation Distance by ", facet_lab, " ", case_lab, " (", start, "DME)")
  
  plot <- ggplot(data = plot_data,
                 mapping = aes(x = Sep_Dist_Factor, y = Flying_Time)) +
    geom_boxplot(aes(group=Sep_Dist_Factor, fill = Sep_Dist_Factor)) +
    labs(x = "Separation Distance (NM)", y = "Flying Time (s)",
         title = plot_title) +
    theme(text = element_text(size=16), legend.position = "none") +
    ylim(low_bound, high_bound)
    
  if (facet == "Wake"){plot <- plot + facet_wrap(~Wake_Cat, nrow=2)} 
  if (facet == "Runway"){plot <- plot + facet_wrap(~Runway, nrow=3)} 
  
  return(plot)
}

# IAS by surface headwind group
png("IAS by Separation Distance and Surface Headwind Group.png")
ggplot(data = filter(IAS_values_0, Separation_Distance %in% Plot_Values), mapping = aes(x = factor(Separation_Distance), y = Ave_SPD)) + facet_wrap(~Surface_Headwind_Gp) +
  geom_boxplot(aes(group = factor(Separation_Distance), fill = factor(Separation_Distance))) +
  labs(x = "Separation Distance (NM)", y = "IAS (kts)", title = "IAS by Separation Distance and Surface Headwind Group") +
  theme(text = element_text(size=8), legend.position = "none")
dev.off()

IAS_values_0 <- Get_Speeds(input, 0, min_dist, max_dist, halves)
fwrite(IAS_values_0, "Speeds_0DME.csv")
IAS_Values_1 <- Get_Speeds(input, 1, min_dist, max_dist, halves)
fwrite(IAS_Values_1, "Speeds_1DME.csv")

All_0DME_Wake <- FT_Boxplot_General(IAS_values_0, 0, 0, "Wake", 50, 250)
SHW_0DME_Wake <- FT_Boxplot_General(IAS_values_0, 0, 1, "Wake", 50, 250)
SW_0DME_Wake <- FT_Boxplot_General(IAS_values_0, 0, 2, "Wake", 50, 250)
SHW_1DME_Wake <- FT_Boxplot_General(IAS_Values_1, 1, 1, "Wake", 50, 250)
SHW_0DME_Runway <- FT_Boxplot_General(IAS_values_0, 0, 1, "Runway", 50, 250)

print(All_0DME_Wake)

png("Flying Time Boxplot All Wind v Low Wind (0DME, (-5-5)kts).png",  width = 1100, height = 960)
grid.arrange(All_0DME_Wake, SHW_0DME_Wake)
dev.off()

png("Flying Time Boxplot (-5-5)kts SHW v (0,3)kts SW (0DME).png",  width = 1100, height = 960)
grid.arrange(SHW_0DME_Wake, SW_0DME_Wake)
dev.off()

png("Flying Time Boxplot 0DME v 1DME (Low Wind).png",  width = 1100, height = 960)
grid.arrange(SHW_0DME_Wake, SHW_1DME_Wake)
dev.off()

png("Flying Time Boxplot by Runway (Low Wind, 0DME).png",  width = 1100, height = 600)
print(SHW_0DME_Runway)
dev.off()

# boxplot function for single RECAT EU types
bplot <- function(data, wake, low){
  data <- filter(data, Wake_Cat == wake)
  data <- filter(data, Separation_Distance %in% Plot_Values)
  if (low == 1){data <- filter(data, Surface_Headwind_Gp == "(-5,5]")}
  plot <- ggplot(data, mapping = aes(x = factor(Separation_Distance), y = Ave_SPD)) + 
    geom_boxplot(mapping = aes(group = factor(Separation_Distance), fill = factor(Separation_Distance))) 
  if (low == 0){lab <- "(All Wind)"} else {lab <- "(-5 to 5 kts)"}
  plot <- plot + labs(x = "Separation Distance (NM)", y = "IAS (kts)", title = paste0("IAS Values for Category ", wake, " ", lab)) +
    theme(text = element_text(size=12), legend.position = "none") + ylim(100, 200)
  return(plot)
}

# use bplot
for (wake in c("A", "B", "C", "D", "E", "F")){
  a <- bplot(IAS_values_0, wake, 0)
  b <- bplot(IAS_values_0, wake, 1)
  png(paste0("IAS Boxplot ", wake, ".png"))
  grid.arrange(a, b)
  dev.off()
}

#-----------------------------------------------------------------------------------------------------------#
# Sample Size Checks
#-----------------------------------------------------------------------------------------------------------#
data1 <- data1_original

# First remove all duplicate wind segs
data1 <- data1[!duplicated(data1[,c('Mode_S_Wind_Seg_ID')]),]
data1 <- data1[!duplicated(data1)]
sum1a <- nrow(data1)
sum1b <- length(unique(data1$Landing_Pair_ID))

# Add Surface wind and runway fields with Apply_New_Fields
data1 <- Apply_New_Fields(data1)

# Initial Filters to remove Not in Trail pairs and Flagged segments
data1 <- filter(data1, Landing_Pair_Type != "Not_In_Trail")# For LVNL only
sum2a <- nrow(data1)
sum2b <- length(unique(data1$Landing_Pair_ID))
data1 <- filter(data1, Global_Flag == F)
sum3a <- nrow(data1)
sum3b <- length(unique(data1$Landing_Pair_ID))
data1 <- filter(data1, !is.na(Wake_Cat), !is.na(Surface_Headwind_Gp), !is.na(Surface_Wind_Gp), Wake_Cat != "")
sum4a <- nrow(data1)
sum4b <- length(unique(data1$Landing_Pair_ID))

# Filter for queued arrivals (pressured operations)
data1 <- Apply_Achieved_Separation_Filter(data1, sep_accuracy_max, sep_accuracy_max_a380)
sum5a <- nrow(data1)
sum5b <- length(unique(data1$Landing_Pair_ID))
#data1 <- Apply_Achieved_Separation_Filter(data1, sep_accuracy_max_lean, sep_accuracy_max_a380_lean)
#data1 <- Apply_Achieved_Separation_Filter(data1, sep_accuracy_max_tight, sep_accuracy_max_a380_tight)

# Extra filtering if necessary
data1 <- filter(data1, Ave_Mode_S_IAS <= max_ias || Ave_Mode_S_IAS >= min_ias) #IAS extreme values
data1 <- filter(data1, Ave_Mode_S_GSPD <= max_gspd || Ave_Mode_S_GSPD >= min_gspd) #GSPD extreme values
sum6a <- nrow(data1)
sum6b <- length(unique(data1$Landing_Pair_ID))

sample_size <- data.frame(c("Original", "In Trail Only", "Passed Stage 2", "Valid Surface Wind/Wake", "Queued Pairs", "Within IAS/GSPD Bounds"),
                          c(sum1a, sum2a, sum3a, sum4a, sum5a, sum6a),
                          c(sum1b, sum2b, sum3b, sum4b, sum5b, sum6b))
names(sample_size) <- c("Filter_Stage", "Number_Of_Segments", "Number_Of_Flights")

fwrite(sample_size, "Sample Sizes.csv")

# Table for requirements, count per wake category
wake_count <- as.data.table(table(data1[data1$DME_Seg == '4',]$Wake_Cat))
names(wake_count) <- c("Wake_Cat","Count")
fwrite(wake_count, "Wake_count_4DME.csv")

#-----------------------------------------------------------------------------------------------------------#
#Summary Statistics
#-----------------------------------------------------------------------------------------------------------#


# Summary tables by RECAT-EU and ICAO4 categories & Surface Wind/HW Groups 
IAS_Summary_Recat_SW_0 <- IAS_values_0 %>% group_by(Wake_Cat, Separation_Distance, Surface_Wind_Gp) %>%
Create_Summary_Main(0) %>% ungroup()
 
IAS_Summary_Recat_SHW_0 <- IAS_values_0 %>% group_by(Wake_Cat, Separation_Distance, Surface_Headwind_Gp) %>%
 Create_Summary_Main(0) %>% ungroup()
 
IAS_Summary_Recat_All_0 <- IAS_values_0 %>% group_by(Wake_Cat, Separation_Distance) %>%
 Create_Summary_Main(0) %>% ungroup()
 
IAS_Summary_Recat_SHW_1 <- IAS_Values_1 %>% group_by(Wake_Cat, Separation_Distance, Surface_Headwind_Gp) %>%
 Create_Summary_Main(1) %>% ungroup()
 
IAS_Summary_Recat_SW_0_Seps <- Create_seps(IAS_Summary_Recat_SW_0) %>% filter(Surface_Wind_Gp == "(0,3]") %>% 
 filter(Constraint == "Wake") %>% select(Leader_WTC, Follower_WTC, Count, Flying_Time) %>% mutate(Flying_Time = round(Flying_Time, 0))
IAS_Summary_Recat_SW_0_Seps <- rename(IAS_Summary_Recat_SW_0_Seps,
                                        Median_Wake_Time_SWCase_0DME = Flying_Time,
                                        SWCase_0DME_Count = Count)
 
 
IAS_Summary_Recat_SHW_0_Seps <- Create_seps(IAS_Summary_Recat_SHW_0) %>% filter(Surface_Headwind_Gp == "(-5,5]") %>% 
   filter(Constraint == "Wake") %>% select(Leader_WTC, Follower_WTC, Count, Flying_Time) %>% mutate(Flying_Time = round(Flying_Time, 0))
IAS_Summary_Recat_SHW_0_Seps <- rename(IAS_Summary_Recat_SHW_0_Seps,
                                        Median_Wake_Time_SHWCase_0DME = Flying_Time,
                                        SHWCase_0DME_Count = Count)
 
 
IAS_Summary_Recat_All_0_Seps <- Create_seps(IAS_Summary_Recat_All_0) %>% 
   filter(Constraint == "Wake") %>% select(Leader_WTC, Follower_WTC, Count, Flying_Time) %>% mutate(Flying_Time = round(Flying_Time, 0))
IAS_Summary_Recat_All_0_Seps <- rename(IAS_Summary_Recat_All_0_Seps,
                                        Median_Wake_Time_All_0DME = Flying_Time,
                                        All_0DME_Count = Count)
 
IAS_Summary_Recat_SHW_1_Seps <- Create_seps(IAS_Summary_Recat_SHW_1) %>% filter(Surface_Headwind_Gp == "(-5,5]") %>% 
   filter(Constraint == "Wake") %>% select(Leader_WTC, Follower_WTC, Count, Flying_Time) %>% mutate(Flying_Time = round(Flying_Time, 0))
IAS_Summary_Recat_SHW_1_Seps <- rename(IAS_Summary_Recat_SHW_1_Seps,
                                        Median_Wake_Time_SHWCase_1DME = Flying_Time,
                                        SHWCase_1DME_Count = Count)
 
All_v_Low_Summary <- left_join(IAS_Summary_Recat_All_0_Seps, IAS_Summary_Recat_SHW_0_Seps, by=c("Leader_WTC", "Follower_WTC")) %>% 
   mutate(Time_Difference = Median_Wake_Time_SHWCase_0DME - Median_Wake_Time_All_0DME)
 
DME1_v_DME0_Summary <- left_join(IAS_Summary_Recat_SHW_1_Seps, IAS_Summary_Recat_SHW_0_Seps, by=c("Leader_WTC", "Follower_WTC")) %>% 
   mutate(Time_Difference = Median_Wake_Time_SHWCase_0DME - Median_Wake_Time_SHWCase_1DME)
 
SHW_v_SW_Summary <- left_join(IAS_Summary_Recat_SW_0_Seps, IAS_Summary_Recat_SHW_0_Seps, by=c("Leader_WTC", "Follower_WTC")) %>% 
   mutate(Time_Difference = Median_Wake_Time_SWCase_0DME - Median_Wake_Time_SHWCase_0DME)
 
fwrite(All_v_Low_Summary, "AllvLow Summary Comparison.csv")
fwrite(DME1_v_DME0_Summary, "1v0DME Summary Comparison.csv")
fwrite(SHW_v_SW_Summary, "HWvSHW Summary Comparison.csv")

