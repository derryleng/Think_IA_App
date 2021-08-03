# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Radar ORD Analysis                                    #
# Version No.    |  1.0                                                   #
# Date Modified  |  28/03/2021                                            #
# Author(s)      |  George Clark, Andy Hyde                               #
# Project        |  Toronto IA                                            #
# Purpose        |  Generate an ORD profile from radar track points and   #
#                |  visualise different error types by groupings          #
#                |                                                        #
#                |                                                        #
# ----------------------------------------------------------------------- #

rm(list = ls())

library(data.table)
library(dplyr)
library(officer)
library(ggplot2)
library(gridExtra)
# library(RODBC)
library(lubridate)
library(ggforce)
library(getPass)
library(tidyverse)


#Use a version number derived from date or define manually
version <- paste0(Sys.Date(), " ","V1.0 (AH)")
version <- "2021-07-26 V1.0 (DB)"

use_same_input_version <- F

if (use_same_input_version == T) {
  input_version <- version
} else if (use_same_input_version == F) {
  input_version <- "2021-07-16 V1.0 (AH)"   #Manually define this if you want different input output version numbers
}

#Set server  with IP then tied to this
Server <- "Maverick" #or Goose

if (Server == "Maverick") {ip <- "192.168.1.23"}
if (Server == "Goose") {ip <- "192.168.1.39"}

#Set the database name for SQL connection
database <- "EGLL_PWS"

#Airport Code
Airport_Code <- "EGLL"

Operation <- "PWS"

# If True uses [tbl_ORD_Aircraft_Profile] if False uses the adaptation
Use_ORD_Profile <- T

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Calibration & Validation"
Script_out <- "Radar ORD Analysis"
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


project <- GetProjectID()

#IorO for "Ouputs" or "Inputs" directory, must be a string as used in directory
Base_Dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

# out_data <- Base_Dir
ord_dir <- Base_Dir

out_data <- file.path(ord_dir, "Radar ORD Analysis", version)
Create_Directory(out_data)

plots_data <- file.path(out_data, "Plots")
Create_Directory(plots_data)

plots_data_ac_type <- file.path(plots_data, "AC Type")
Create_Directory(plots_data_ac_type)

plots_data_lss_type <- file.path(plots_data, "LSS Type")
Create_Directory(plots_data_lss_type)

speed_prof_dir <- file.path(ord_dir, "Speed Profiles", input_version)

con <- Get_DBI_Connection(IP = ip, Database = database)

###############################################################################
# Initialisation

RTT_Bin_Width <-  0.5
Wind_Bin_Width <- 5
Baro_Bin_Width <- 5
Gust_Bin_Width <- 5

Max_RTT <- 6

#set to T if wanting to run the linear regression model by AC/LSS type respectively
ML_Model_AC <- F
ML_Model_LSS <- F

#if T, filters Radar data to only flights with >200s track duration per segment (as done in GWCS Performance)
flight_greater_200_filter <- T

###############################################################################
# Functions

# Function to return the ceiling to 1dp - unused as its unnecessary
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# the "not in" function
"%!in%" <- function(x,y) !("%in%"(x,y))


Generate_ORD_Profile_Radar_Point_Error <- function(Radar, GS_Profile, LorF, Include_Intercept_ILS){

  #LorF <- "Follower"
  #Include_Intercept_ILS <- T

  # Do we want to transform RTT values based on Intercept legs?
  if (Include_Intercept_ILS){

    # Adaptation Data
    Allowed_Path_Legs <- c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept")
    Max_Range_To_ILS <- 4

    # Change Range to Threshold value based on intercept ILS criteria
    Radar <- mutate(Radar,
                    ILS_Intercept_Flag = ifelse(is.na(Range_To_Threshold) & Path_Leg_Type %in% Allowed_Path_Legs & Range_To_ILS <= Max_Range_To_ILS, 1, 0),
                    ILS_Intercept_Flag = ifelse(is.na(ILS_Intercept_Flag), 0, ILS_Intercept_Flag),
                    Range_To_Threshold = ifelse(ILS_Intercept_Flag == 1, ILS_Locus_RTT, Range_To_Threshold))

  }

  # Filter out invalid RTT values
  Radar <- filter(Radar, !is.na(Range_To_Threshold))

  ## NOTE: GS Profile will need to be only In-Trail Pairs to stop duplication.

  # Get This_Pair_Role
  LorF1 <- substr(LorF, 1, 1)
  FPID_Keep <- ifelse(LorF1 == "L", "Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID")
  FPID_Lose <- ifelse(LorF1 == "F", "Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID")
  GS_Profile <- filter(GS_Profile, This_Pair_Role == LorF1) %>%
    select(-c("This_Pair_Role", !!sym(FPID_Lose)))


  # Rolling Join Radar onto GS Profile
  # --------------------------------------------------- #
  Radar <- Radar %>%
    mutate(Join_ID = Flight_Plan_ID,
           Join_Distance = Range_To_Threshold)

  GS_Profile <- GS_Profile %>%
    mutate(Join_ID := !!sym(FPID_Keep),
           Join_Distance = Start_Dist)

  Radar <- as.data.table(Radar)
  GS_Profile <- as.data.table(GS_Profile)

  setkey(Radar, Join_ID, Join_Distance)
  setkey(GS_Profile, Join_ID, Join_Distance)

  Radar <- GS_Profile[Radar, roll = -Inf]

  Radar <- as.data.frame(Radar)
  GS_Profile <- as.data.frame(GS_Profile)

  Radar <- select(Radar, -c("Join_ID",
                            "Join_Distance"))

  Radar <- mutate(Radar, Mode_S_GSPD = Mode_S_IAS + Wind_Effect_IAS)
  # --------------------------------------------------- #

  # Adjust Predicted Speed Based on Range to Threshold
  Radar <- mutate(Radar,
                  IAS_Difference = Start_IAS - End_IAS,
                  GS_Difference = Start_GS - End_GS,
                  Distance_Ratio = (Range_To_Threshold - End_Dist)/(Start_Dist - End_Dist),
                  Forecast_IAS = End_IAS + (Distance_Ratio * IAS_Difference),
                  Forecast_GS = End_GS + (Distance_Ratio * GS_Difference),
                  Forecast_WE = Forecast_GS - Forecast_IAS,
                  Forecast_IAS_Error = Mode_S_IAS - Forecast_IAS,
                  Forecast_GS_Error = Mode_S_GSPD - Forecast_GS,
                  Forecast_WE_Error = (Mode_S_GSPD - Mode_S_IAS) - Forecast_WE)

  # Remove Unrequired Fields
  Radar <- Radar %>%
    select(-c("IAS_Difference",
              "GS_Difference",
              "Distance_Ratio",
              !!sym(FPID_Keep)))

  # # Remove GS Profile Fields
  # Radar <- Radar %>%
  #   select(-c("Start_IAS",
  #             "Start_GS",
  #             "Start_Dist",
  #             "End_IAS",
  #             "End_GS",
  #             "End_Dist"))

  # Rename Fields for Leader/Follower metrics
  Radar <- Radar %>%
    rename(!!sym(paste0(LorF, "_Forecast_IAS")) := Forecast_IAS,
           !!sym(paste0(LorF, "_Forecast_GS")) := Forecast_GS,
           !!sym(paste0(LorF, "_Forecast_WE")) := Forecast_WE,
           !!sym(paste0(LorF, "_Forecast_IAS_Error")) := Forecast_IAS_Error,
           !!sym(paste0(LorF, "_Forecast_GS_Error")) := Forecast_GS_Error,
           !!sym(paste0(LorF, "_Forecast_WE_Error")) := Forecast_WE_Error)

  return(Radar)

}

Bin_Radar_Data <- function(Radar_Data, Grouping, BinWidth) {
  ##should this not be Radar_Data rather than Radar1?
  #segments <- seq(floor(min(Radar1[[Grouping]], na.rm = T)), ceiling(max(Radar1[[Grouping]], na.rm = T)), BinWidth)
  segments <- seq(floor(min(Radar_Data[[Grouping]], na.rm = T)), ceiling(max(Radar_Data[[Grouping]], na.rm = T)), BinWidth)
  
  
  
  Radar_Binned <- cut(Radar_Data[[Grouping]],
                      na.bucket = NA,
                      breaks = segments,
                      include.lowest = TRUE,
                      right = FALSE) %>%
    tibble(.)

  Radar_Data[[paste0(Grouping, "_Bins")]] <- Radar_Binned$.

  return(Radar_Data)

}

plot_error_by_group <- function(dat, Error_Var, Grouping_Var, Max_Error_Val) {

  png(file.path(out_data, paste0(Error_Var, "_by_", Grouping_Var, ".png")), width = 1920, height = 1080)
  m <- ggplot(dat %>% filter(abs(!!sym(Error_Var)) < Max_Error_Val)) +
    geom_boxplot(aes(x = Range_To_Threshold_Bins, y = !!sym(Error_Var))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(as.formula(paste("~", Grouping_Var)))
  print(m)
  dev.off()

}

plot_error_all_ac <- function(dat, Error_Var, Max_Error_Val, height, width) {

  n_page <- ceiling(nrow(Radar1 %>% select(Aircraft_Type) %>% distinct()) / (height * width))

  for (i in 1:n_page) {

    png(file.path(out_data, paste0("All_aircraft_", Error_Var, "_page_", i, ".png")), width = 1920, height = 1080)
    m <- ggplot(dat %>% filter(abs(!!sym(Error_Var)) < Max_Error_Val)) +
      geom_boxplot(aes(x = Range_To_Threshold_Bins, y = !!sym(Error_Var))) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      facet_wrap_paginate(~Aircraft_Type, ncol = width, nrow = height, page = i)
    print(m)
    dev.off()

  }
}

plot_error_specific_ac_type <- function(dat, Error_Var, Max_Error_Val, AC_Type) {

  png(file.path(out_data, paste0(AC_Type, "_", Error_Var, ".png")), width = 1920, height = 1080)
  m <- ggplot(dat %>% filter(Aircraft_Type == AC_Type & abs(!!sym(Error_Var)) < Max_Error_Val)) +
    geom_boxplot(aes(x = Range_To_Threshold_Bins, y = !!sym(Error_Var))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
  print(m)
  dev.off()

}

###############################################################################
# Load Raw data


### GC ADD 0803: Get Radar Data
Radar_Query <- "SELECT
RTPD.Flight_Plan_ID,
Track_Time,
Range_To_Threshold,
ILS_Locus_RTT,
Range_To_ILS,
Path_Leg_Type,
Wind_Effect_IAS,
Mode_S_IAS
FROM vw_Radar_Track_Point_Derived RTPD
LEFT JOIN tbl_Flight_Plan FP
ON RTPD.Flight_Plan_ID = FP.Flight_Plan_ID
LEFT JOIN tbl_Flight_Plan_Derived FPD
ON RTPD.Flight_Plan_ID = FPD.Flight_Plan_ID
LEFT JOIN tbl_Runway R1
ON R1.Runway_Name = RTPD.Mode_S_Wind_Localiser_Capture
LEFT JOIN tbl_Runway R2
ON R2.Runway_Name = FP.Landing_Runway
WHERE FP.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
OR FPD.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
OR R1.Runway_Group = R2.Runway_Group
OR RTPD.Mode_S_Wind_Localiser_Capture IS NULL
ORDER BY Flight_Plan_ID, Track_Time"

GS_Query <- "SELECT
LP.Leader_Flight_Plan_ID,
LP.Follower_Flight_Plan_ID,
GS.This_Pair_Role,
GS.End_IAS,
GS.Start_IAS,
GS.End_GS,
GS.Start_GS,
GS.End_Dist,
GS.Start_Dist
FROM vw_ORD_GS_Profile GS
INNER JOIN tbl_Landing_Pair LP
ON LP.Landing_Pair_ID = GS.Landing_Pair_ID
WHERE LP.Landing_Pair_Type != 'Not_In_Trail'"

# Load Data
Radar <- dbGetQuery(con, Radar_Query, stringsAsFactors = F)
GS_Profile <- dbGetQuery(con, GS_Query, stringsAsFactors = F)

## added 22/07/2021 - adds option to filter out flights with track duration >200s long
if(flight_greater_200_filter){
  rawsegs <- as.data.table(dbGetQuery(con, "SELECT Mode_S_Wind_Seg_ID, FP_Date, Min_Track_Time, Max_Track_Time, Callsign FROM vw_Mode_S_Wind_Seg"))
  rawsegs_FPID <-  unique(as.data.table(dbGetQuery(con, "SELECT [Flight_Plan_ID], [Mode_S_Wind_Seg_ID] FROM tbl_Mode_S_Wind_Seg")))
  
  rawsegs <- merge(rawsegs,rawsegs_FPID, by = "Mode_S_Wind_Seg_ID" )
  
  rawsegs$FP_Date <- as.character(rawsegs$FP_Date)
  
  rawsegs$track_dur <- rawsegs$Max_Track_Time - rawsegs$Min_Track_Time
  #rawsegs_copy <- rawsegs
  
  # find all flights with anomaly (difference in track min, max > 200)
  flights_greater_200 <- unique(rawsegs[track_dur > 200, c("Flight_Plan_ID", "FP_Date","Callsign")])
  Radar <- Radar %>% filter(!(Flight_Plan_ID %in% flights_greater_200$Flight_Plan_ID))
  GS_Profile <- GS_Profile %>% filter(!(Leader_Flight_Plan_ID %in% flights_greater_200$Flight_Plan_ID)) %>%
                               filter(!(Follower_Flight_Plan_ID %in% flights_greater_200$Flight_Plan_ID))
}


# keep this commented out, just wastes memory
# Radar_o <- Radar
# GS_o <- GS_Profile
#
#
# # Revert
# Radar <- Radar_o
# GS_Profile <- GS_o

#Splits data into leaders and followers

Radar1 <- Generate_ORD_Profile_Radar_Point_Error(Radar, GS_Profile, LorF = "Leader", Include_Intercept_ILS = T) %>%
          filter(!is.na(Leader_Forecast_IAS_Error)) %>%
          filter(Range_To_Threshold <= Max_RTT)
Radar2 <- Generate_ORD_Profile_Radar_Point_Error(Radar, GS_Profile, LorF = "Follower", Include_Intercept_ILS = T) %>%
          filter(!is.na(Follower_Forecast_IAS_Error)) %>%
          filter(Range_To_Threshold <= Max_RTT)

# ----------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------- #

# Use fpid to get aircraft type and wake categories from db

# FPIDS <- Radar %>% select(Flight_Plan_ID) %>% distinct() %>% pull(Flight_Plan_ID)
# FPID_String <- toString(sprintf("'%s'", FPIDS))
# 
# Type_Query <- "SELECT Flight_Plan_ID, Aircraft_Type FROM tbl_Flight_Plan WHERE Flight_Plan_ID IN (%s)"
# 
# Type_Query <- sprintf(Type_Query, FPID_String)
# 
# Aircraft_Types <- dbGetQuery(con, Type_Query)

Aircraft_Types <- dbGetQuery(con, "SELECT Flight_Plan_ID, Aircraft_Type FROM tbl_Flight_Plan")

Wake_Categories <- dbGetQuery(con, "SELECT Aircraft_Type, Wake FROM tbl_Aircraft_Type_To_Wake")

Aircraft_Types <- left_join(Aircraft_Types, Wake_Categories, by = "Aircraft_Type")

Radar1 <- left_join(Radar1, Aircraft_Types, by = "Flight_Plan_ID")
Radar2 <- left_join(Radar2, Aircraft_Types, by = "Flight_Plan_ID")

Radar1 <- Bin_Radar_Data(Radar1, "Range_To_Threshold", RTT_Bin_Width)
Radar2 <- Bin_Radar_Data(Radar2, "Range_To_Threshold", RTT_Bin_Width)
Radar1 <- Bin_Radar_Data(Radar1, "Wind_Effect_IAS", Wind_Bin_Width)
Radar2 <- Bin_Radar_Data(Radar2, "Wind_Effect_IAS", Wind_Bin_Width)


# ----------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------- #

# Load in speed profiles

Speed_Prof <- fread(file.path(speed_prof_dir, "Approach_Speed_Profiles.csv"))

# Gets a vector containing all FPIDs in Speed_Prof
Prof_FPIDS <- Speed_Prof %>% select(Follower_Flight_Plan_ID) %>% 
                             distinct() %>% 
                             pull(Follower_Flight_Plan_ID) 


###### This section of code dosnt work for very large tables SQL cant deal with the amount of subqueries, need to fetch the whole table instead.

# # converts FPID vector to a character vector
# Prof_FPIDS_String <- FPID_String <- toString(sprintf("'%s'", Prof_FPIDS))
# 
# # Initialises query to select FP_Time for desired FPIDs assembles query and fetches from database
# FPID_Query <- "SELECT Flight_Plan_ID, FP_Time FROM tbl_Flight_Plan WHERE Flight_Plan_ID IN (%s)"
# FPID_Query <- sprintf(FPID_Query, Prof_FPIDS_String)
# FP_Times <- dbGetQuery(con, FPID_Query)

FP_Times <- dbGetQuery(con, "SELECT main.Flight_Plan_ID, der.Time_At_1DME FROM tbl_Flight_Plan as main LEFT JOIN tbl_Flight_Plan_Derived as der ON main.Flight_Plan_ID = der.Flight_Plan_ID")

#Joins FP_Time onto Speed Prof by FPID
Speed_Prof <- left_join(Speed_Prof, FP_Times, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))

# rm(Prof_FPIDS, Prof_FPIDS_String, FPID_Query, FP_Times)
rm(FP_Times)

#Getting all Barometer data to join QNH on as a grouping for errors
tbl_Baro <- dbGetQuery(con, "SELECT Baro_Date, Baro_Time, Baro_Pressure FROM tbl_Baro")

#Rolling join, exact match on date, nearest match on time
Speed_Prof <- rolling_join(Speed_Prof, tbl_Baro, c("FP_Date", "Time_At_1DME"), c("Baro_Date", "Baro_Time"), "nearest")

#bins QNH
Speed_Prof <- Bin_Radar_Data(Speed_Prof, "Baro_Pressure", Baro_Bin_Width)

#Getting all Gusting data to join Gusting on as a grouping for errors
tbl_Gusting <- dbGetQuery(con, "SELECT Gust_Date, Gust_Time, Gust, Gust_Valid, Runway FROM tbl_Gusting")

tbl_Gusting <- tbl_Gusting %>% mutate(Gust_Date = gsub("/20","/",Gust_Date)) %>% #crude way to replace 20xx dates with just xx (to match FP_Date)
                               mutate(Gust_Date_Runway = paste(Gust_Date,Runway, sep=":")) #creates a date/runway variable for use in rolling join
Speed_Prof <- Speed_Prof %>% mutate(FP_Date_Runway = paste(FP_Date,Landing_Runway,sep=":"))

#Rolling join gusting data
Speed_Prof <- rolling_join(Speed_Prof, tbl_Gusting, c("FP_Date_Runway", "Time_At_1DME"), c("Gust_Date_Runway", "Gust_Time"), "nearest")
Speed_Prof <- Bin_Radar_Data(Speed_Prof,"Gust",Gust_Bin_Width)

# Filtering the speed profiles fitted parameters, replaces out of range
# values with NA so the flight can be used if other parameters are good

a1_min <- 75; a1_max <- 220

a2_min <- 75; a2_max <- 220

b_min <- 100; b_max <- 180

n1_min <- 0; n1_max <- 12

n2_min <- 0; n2_max <- 14

d_min <- 0; d_max <- 50

AC_Type_To_Wake <- dbGetQuery(con, "SELECT Aircraft_Type, Wake FROM tbl_Aircraft_Type_To_Wake")

Speed_Prof <- Speed_Prof %>% mutate(a1 = ifelse(a1 >= a1_min & a1 <= a1_max, a1, NA), 
                                    a2 = ifelse(a2 >= a2_min & a2 <= a2_max, a2, NA),
                                    b = ifelse(b >= b_min & b <= b_max, b, NA),
                                    n1 = ifelse(n1 >= n1_min & n1 <= n1_max, n1, NA),
                                    n2 = ifelse(n2 >= n2_min & n2 <= n2_max, n2, NA),
                                    d = ifelse(d >= d_min & d <= d_max, d, NA)) %>%
                                    select(-wake) %>%
                                    left_join(AC_Type_To_Wake, by = c("Follower_Aircraft_Type" = "Aircraft_Type"))

#add Carrier Column to Speed_Prof
Speed_Prof <- Speed_Prof %>% mutate(Follower_Carrier = substr(Follower_Callsign, start = 1, stop = 3))


###############################################################################
# Calculate errors
# e1: Landing Speed Error
# e2: Deceleration End Point Error
# e3: Deceleration Rare Error
# e4: Deceleration Start Point Error
# e5: Procedural Speed Error


# Legacy and PWS operation have different adaptation tables (adjustable end decel point not fixed at the 1000ft gate)

# Might want to make this a column in the data so it can be done on a runway specific basis. 3 is wrong, all runways
# have a different value around 2.9, depend on runway alt
thousand_ft_gate <- 3

if (Use_ORD_Profile == F) {
  
  tbl_ORD_Wake_Adaptation <- Auto_Unit_Conversion(dbGetQuery(con, "SELECT * FROM tbl_ORD_Wake_Adaptation"), "SI_to_Aviation")
  
  tbl_ORD_Aircraft_Adaptation <- Auto_Unit_Conversion(dbGetQuery(con, "SELECT * FROM tbl_ORD_Aircraft_Adaptation"), "SI_to_Aviation")
  
  PWS_Type_Join <- c("Aircraft_Type",
                     "Min_Safe_Landing_Speed_Follower",
                     "Local_Stabilisation_Distance_Follower",
                     "Steady_Procedural_Speed_Follower",
                     "Final_Deceleration_Follower",
                     "End_Initial_Deceleration_Distance_Follower",
                     "Initial_Procedural_Speed_Follower",
                     "End_Final_Deceleration_Distance_Follower",
                     "Initial_Deceleration_Follower"
                     )
  
  PWS_Wake_Join <- c("Wake_Cat",
                     "Min_Safe_Landing_Speed_Follower",
                     "Local_Stabilisation_Distance_Follower",
                     "Steady_Procedural_Speed_Follower",
                     "Final_Deceleration_Follower",
                     "End_Initial_Deceleration_Distance_Follower",
                     "Initial_Procedural_Speed_Follower",
                     "End_Final_Deceleration_Distance_Follower",
                     "Initial_Deceleration_Follower"
                     )
  
  Legacy_Type_Join <- c("Aircraft_Type",
                        "Min_Safe_Landing_Speed_Follower",
                        "Local_Stabilisation_Distance_Follower",
                        "Steady_Procedural_Speed_Follower",
                        "Final_Deceleration_Follower",
                        "End_Initial_Deceleration_Distance_Follower",
                        "Initial_Procedural_Speed_Follower",
                        "Initial_deceleration_follower"
                        )
  
  Legacy_Wake_Join <- c("Wake_Cat",
                        "Min_Safe_Landing_Speed_Follower",
                        "Local_Stabilisation_Distance_Follower",
                        "Steady_Procedural_Speed_Follower",
                        "Final_Deceleration_Follower",
                        "End_Initial_Deceleration_Distance_Follower",
                        "Initial_Procedural_Speed_Follower",
                        "Initial_deceleration_Follower"
                        )
  
  # Takes all tracks that have an aircraft type in the type specififc adaptation and joins on these parameters
  Speed_Prof_Type <- Speed_Prof %>% filter(Follower_Aircraft_Type %in% tbl_ORD_Aircraft_Adaptation$Aircraft_Type) %>%
                                    left_join(select(tbl_ORD_Aircraft_Adaptation, if (Operation == "PWS") {PWS_Type_Join} else {Legacy_Type_Join}),
                                              by = c("Follower_Aircraft_Type" = "Aircraft_Type"))
  
  # Renames the Follower deceleration to match the wake naming, ty RW for the inconsistency
  if (Operation == "Legacy") {Speed_Prof_Type <- Speed_Prof_Type %>% rename(Initial_deceleration_Follower = Initial_deceleration_follower)}
  
  #Takes all tracks that arent in the type specific adaptation and joins on the wake adaptation
  
  Speed_Prof_Wake <- Speed_Prof %>% filter(!(Follower_Aircraft_Type %in% tbl_ORD_Aircraft_Adaptation$Aircraft_Type)) %>%
                                    left_join(select(tbl_ORD_Wake_Adaptation, if (Operation == "PWS") {PWS_Wake_Join} else {Legacy_Wake_Join}),
                                              by = c("wake" = "Wake_Cat"))
  
  # The observed deceleration rate is calculated here, the input from Approach Speed Profiling outputs decel as a wake cat value
  # Would probably be best to implement this into the approach speed profiling script instead of recalculating here
  
  
  
  
  Speed_Prof_Errors <- rbind(Speed_Prof_Type, Speed_Prof_Wake) %>%
                       mutate(d = (b - a1)/(n2 - n1),
                              Start_Final_Deceleration_Follower = (Steady_Procedural_Speed_Follower - Min_Safe_Landing_Speed_Follower) /
                              Final_Deceleration_Follower + ifelse(Operation == "PWS", End_Final_Deceleration_Distance_Follower, thousand_ft_gate)) %>%
                       mutate(e1 = a1 - Min_Safe_Landing_Speed_Follower,
                              e2 = n1 - ifelse(Operation == "PWS", End_Final_Deceleration_Distance_Follower, thousand_ft_gate),
                              e3 = d - Final_Deceleration_Follower,
                              e4 = n2 - Start_Final_Deceleration_Follower,
                              e5 = b - Steady_Procedural_Speed_Follower
                       )
}

if (Use_ORD_Profile == T) {
  
  if (Operation != "PWS") {
  
    ORD_Profile_Query <- "SELECT ORD.[Landing_Pair_ID]
                                ,ORD.[This_Pair_Role]
                                ,ORD.[Aircraft_Type]
                                ,ORD.[Wake_Cat]
                                ,ORD.[VRef]
                                ,ORD.[Apply_Gusting]
                                ,ORD.[Landing_Stabilisation_Speed_Type]
                                ,ORD.[Local_Stabilisation_Distance]
                                ,ORD.[Landing_Stabilisation_Speed]
                                ,ORD.[Final_Deceleration]
                                ,ORD.[Final_Deceleration_Distance]
                                ,ORD.[Steady_Procedural_Speed]
                                ,ORD.[End_Initial_Deceleration_Distance]
                                ,ORD.[Start_Initial_Deceleration_Distance]
                                ,ORD.[Initial_Procedural_Speed]
                          	    ,LP.[Follower_Flight_Plan_ID]
                          FROM [tbl_ORD_Aircraft_Profile] ORD
                          LEFT JOIN tbl_Landing_Pair LP
                          ON ORD.Landing_Pair_ID = LP.Landing_Pair_ID
                          WHERE This_Pair_Role = 'F' "
  } else {
    
    ORD_Profile_Query <- "SELECT ORD.[Landing_Pair_ID]
                                ,ORD.[This_Pair_Role]
                                ,ORD.[Aircraft_Type]
                                ,ORD.[Operator]
                                ,ORD.[Wake_Cat]
                                ,ORD.[Runway]
                                ,ORD.[VRef]
                                ,ORD.[Apply_Gusting]
                                ,ORD.[Landing_Stabilisation_Speed_Type]
                                ,ORD.[Local_Stabilisation_Distance]
                                ,ORD.[Compression_Commencement_Threshold]
                                ,ORD.[Landing_Stabilisation_Speed]
                                ,ORD.[Final_Deceleration]
                                ,ORD.[End_Final_Deceleration_Distance]
                                ,ORD.[Start_Final_Deceleration_Distance]
                                ,ORD.[Steady_Procedural_Speed]
                                ,ORD.[Initial_Deceleration]
                                ,ORD.[End_Initial_Deceleration_Distance]
                                ,ORD.[Start_Initial_Deceleration_Distance]
                                ,ORD.[Initial_Procedural_Speed]
                          	    ,LP.[Follower_Flight_Plan_ID]
                          FROM [tbl_ORD_Aircraft_Profile] ORD
                          LEFT JOIN tbl_Landing_Pair LP
                          ON ORD.Landing_Pair_ID = LP.Landing_Pair_ID
                          WHERE This_Pair_Role = 'F' "
    
  }
  tbl_ORD_Aircraft_Profile <- Auto_Unit_Conversion(dbGetQuery(con, ORD_Profile_Query), "SI_to_Aviation")
  
  Speed_Prof_Errors <- inner_join(Speed_Prof, tbl_ORD_Aircraft_Profile, by = "Follower_Flight_Plan_ID") %>%
                       mutate(d = (b - a1)/(n2 - n1),
                              Start_Final_Deceleration = (Steady_Procedural_Speed - VRef) /
                              Final_Deceleration + ifelse(Operation == "PWS", End_Final_Deceleration_Distance, thousand_ft_gate)) %>%
                       mutate(e1 = a1 - VRef,
                              e2 = n1 - ifelse(Operation == "PWS", End_Final_Deceleration_Distance, thousand_ft_gate),
                              e3 = d - Final_Deceleration,
                              e4 = n2 - Start_Final_Deceleration,
                              e5 = b - Steady_Procedural_Speed
                       )
  
}




##############################
# Recalculate these performance metrics using the ORD profile that is calculated on the DB (Contains wind adjustments)








#-----------------------------------------------------------------------------#
## Create Plots ---------------------------------------------------------------
#-----------------------------------------------------------------------------#

# The aggregates we want to plot by

# o	The errors by different aircraft types / carriers
# o	The errors by different landing stabilisation speed types
# o	The errors by different surface wind conditions
# o	The errors by different GWCS profile conditions
# o	The errors at different ranges to threshold
# o	The errors by different gusting conditions

# Some of the errors will require the full data set (rtp at all points),
# some will require the approach speed profiles data (speed profile for the flight)








# Plot an error type with a grouping faceted
# plot_error_by_group(dat, Error_Var, Grouping_Var, Max_Error_Val)
# If no filtering wanted, set Max_Error_Val to Inf

plot_error_by_group(Radar1, "Leader_Forecast_IAS_Error", "Wake", 150)

# Plots an error type for all aircraft types paginated facet wrap
# NB. takes a long time to execute
# plot_error_all_ac(dat, Error_Var, Max_Error_Val, height, width)
# height and width refer to the number of plots per page

plot_error_all_ac(Radar1, "Leader_Forecast_IAS_Error", 25, 3, 3)

# Plot an error type for an specific aircraft type
# plot_error_specific_ac_type(dat, Error_Var, Max_Error_Val, AC_Type)

plot_error_specific_ac_type(Radar1, "Leader_Forecast_IAS_Error", Inf, "A333")

Plot_profile_error <- function(dat, Error_Var, Grouping_Var, Max_Error_Val, Facet) {
  #filters out NA values from graphs
  dat <- dat %>% filter(!is.na(!!sym(Grouping_Var)))%>%
    group_by(!!sym(Grouping_Var))%>%
    #filter(n()>10)%>%
    ungroup()
  
  m <- ggplot(dat %>% filter(abs(!!sym(Error_Var)) < Max_Error_Val)) +
    geom_boxplot(aes(x = !!sym(Grouping_Var), y = !!sym(Error_Var))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(paste(Error_Var_Name(Error_Var,F), " by ", Grouping_Var_Name(Grouping_Var,F), " (",count(dat %>% filter(abs(!!sym(Error_Var)) < Max_Error_Val))," observations)", sep = "")) +
    labs(x=Grouping_Var_Name(Grouping_Var,T), y=Error_Var_Name(Error_Var,T))
  
  if (!missing(Facet)) {
    m <- m + facet_wrap(as.formula(paste("~", Facet)), scales = "free")
  }
  
  return(m)
  
}

Plot_profile_error_no_group <- function(dat, Error_Var, Grouping_Var, Max_Error_Val, Facet) {
  #filters out NA values from graphs
  if(Grouping_Var != "NONE"){
    dat <- dat %>% filter(!is.na(!!sym(Grouping_Var))) %>%
      group_by(!!sym(Grouping_Var))%>%
      #filter(n()>10)%>%
      ungroup()
  }
  m <- ggplot(dat %>% filter(abs(!!sym(Error_Var)) < Max_Error_Val)) +
    geom_boxplot(aes(x = ifelse(Grouping_Var == "NONE", factor(0), !!sym(Grouping_Var)), y = !!sym(Error_Var))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(paste(Error_Var_Name(Error_Var,F), "(",count(dat %>% filter(abs(!!sym(Error_Var)) < Max_Error_Val))," observations)", sep = "")) +
    labs(y=Error_Var_Name(Error_Var,T))
  
  if (!missing(Facet)) {
    m <- m + facet_wrap(as.formula(paste("~", Facet)), scales = "free")
  }
  
  return(m)
  
}

#Changes var name for readability
Error_Var_Name <- function(ev, units){
  if(units){
    ifelse(ev=="e1","Landing Speed Prediction Error (kts)", ifelse(ev=="e2","Deceleration End Point Error(nm)",ifelse(ev=="e3","Deceleration Rate Error",ifelse(ev=="e4","Deceleration Start Point Error(nm)",ifelse(ev=="e5","Procedural Speed Error",ev)))))
  }else{
    ifelse(ev=="e1","Landing Speed Prediction Error", ifelse(ev=="e2","Deceleration End Point Error",ifelse(ev=="e3","Deceleration Rate Error",ifelse(ev=="e4","Deceleration Start Point Error",ifelse(ev=="e5","Procedural Speed Error",ev)))))
  }
}

#Changes var name for readability
Grouping_Var_Name <- function(gv, units){
  if(units){
    ifelse(gv=="Follower_Carrier","Aircraft Carrier",ifelse(gv=="Gust_Bins","Gust Value (kts)",ifelse(gv=="Gust_Valid","Gusting Flag",ifelse(gv=="Follower_Aircraft_Type","Aircraft Type",ifelse(gv=="Baro_Pressure_Bins","QNH Value (mbar)",ifelse(gv=="Wake","Wake Category",gv))))))
  } else{
    ifelse(gv=="Follower_Carrier","Aircraft Carrier",ifelse(gv=="Gust_Bins","Gust Value",ifelse(gv=="Gust_Valid","Gusting Flag",ifelse(gv=="Follower_Aircraft_Type","Aircraft Type",ifelse(gv=="Baro_Pressure_Bins","QNH Value",ifelse(gv=="Wake","Wake Category",gv))))))
  }
}

# Here you can define the facet or leave argument blank (last argument of the function).
# Set filter to Inf if you dont want a filter.

Plot_profile_error(Speed_Prof_Errors, "e1", "Follower_Aircraft_Type", 50, "Wake")
Plot_profile_error_no_group(Speed_Prof_Errors, "e3", "NONE", 5, "lss_type")
# 
Plot_profile_error(Speed_Prof_Errors, "e1", "lss_type", 50)


error_types <- c("e1", "e2", "e4","e3", "e5")
# Filter values c(e1, e2, e3, e4, e5)
filters <- c(25, 3, 20, 3, 30)

ac_types <- unique(Speed_Prof$Follower_Aircraft_Type)
lss_types <- unique(Speed_Prof$lss_type)

Speed_Prof_Errors <- mutate(Speed_Prof_Errors, `LSS Type` = as.character(lss_type))


# Plot_profile_error(Speed_Prof_Errors, "e3", "Follower_Aircraft_Type", 50, "lss_type")

for (i in 1:length(error_types)) {
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_type.png")), width = 1920, height = 1080)
  m <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Follower_Aircraft_Type", filters[i], "Wake")
  print(m)
  dev.off()
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_wake.png")), width = 1920, height = 1080)
  n <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Wake", filters[i])
  print(n)
  dev.off()
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_lss_type_and_ac_type.png")), width = 1920, height = 1080)
  n <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Follower_Aircraft_Type", filters[i], "lss_type")
  print(n)
  dev.off()
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_lss_type.png")), width = 1920, height = 1080)
  n <- Plot_profile_error_no_group(Speed_Prof_Errors, error_types[i], "NONE", filters[i], "lss_type")
  print(n)
  dev.off()
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_carrier_facet.png")), width = 1920, height = 1080)
  n <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Follower_Carrier", filters[i], "Wake")
  print(n)
  dev.off()  
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_carrier.png")), width = 1920, height = 1080)
  n <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Follower_Carrier", filters[i])
  print(n)
  dev.off()
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_QNH_and_LSS.png")), width = 1920, height = 1080)
  n <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Baro_Pressure_Bins", filters[i], "lss_type")
  print(n)
  dev.off()
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_Gusting.png")), width = 1920, height = 1080)
  n <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Gust_Bins", filters[i])
  print(n)
  dev.off()
  
  png(filename = file.path(plots_data, paste0(error_types[i], "_by_Gusting_Valid.png")), width = 1920, height = 1080)
  n <- Plot_profile_error(Speed_Prof_Errors, error_types[i], "Gust_Valid", filters[i])
  print(n)
  dev.off()
  
  #plot by AC type for gusting, recommended to leave this off unless definitely needed, otherwise creates 780 new plots
  by_AC_type <- F
  if (by_AC_type){
    for (j in 1:length(ac_types)){
      Speed_Prof_Errors_AC <- Speed_Prof_Errors %>% filter(Follower_Aircraft_Type==ac_types[j])
      
      png(filename = file.path(plots_data_ac_type, paste0(error_types[i], "_by_Gusting_",ac_types[j],".png")), width = 1920, height = 1080)
      n <- Plot_profile_error(Speed_Prof_Errors_AC, error_types[i], "Gust_Bins", filters[i])
      print(n)
      dev.off()
      
      png(filename = file.path(plots_data_ac_type, paste0(error_types[i], "_by_Gusting_Valid",ac_types[j],".png")), width = 1920, height = 1080)
      n <- Plot_profile_error(Speed_Prof_Errors_AC, error_types[i], "Gust_Valid", filters[i])
      print(n)
      dev.off()
    }
  }
  
  #plot by LSS Type for gusting
  by_LSS_type <- T
  if (by_LSS_type){
    for (j in 1:length(lss_types)){
      Speed_Prof_Errors_LSS <- Speed_Prof_Errors %>% filter(lss_type==lss_types[j])
      
      png(filename = file.path(plots_data_lss_type, paste0(error_types[i], "_by_Gusting_LSS_",lss_types[j],".png")), width = 1920, height = 1080)
      n <- Plot_profile_error(Speed_Prof_Errors_LSS, error_types[i], "Gust_Bins", filters[i])
      print(n)
      dev.off()
      
      png(filename = file.path(plots_data_lss_type, paste0(error_types[i], "_by_Gusting_Valid_LSS_",lss_types[j],".png")), width = 1920, height = 1080)
      n <- Plot_profile_error(Speed_Prof_Errors_LSS, error_types[i], "Gust_Valid", filters[i])
      print(n)
      dev.off()
      
    }
  }
}




###############################


#######################################
# Create a plot based on QNH... need data for this (PWS database when ORD is run)
# ie... 
# Plot_profile_error(Speed_Prof_Errors, "e1", "QNH_bins", 1000, "Wake_Cat")
####
# Do the same for gusting ~~~~~~~ I need to load this data still



###############################
# Creating a summary table of error metric

make_summary_table <- function(dat, Error_Var) {
  
  dat_summary <- dat %>% group_by(Follower_Aircraft_Type) %>%
                         summarise('5%' = quantile(!!sym(Error_Var), 0.05, na.rm = T),
                                   'median' = quantile(!!sym(Error_Var), 0.50, na.rm = T),
                                   '95%' = quantile(!!sym(Error_Var), 0.95, na.rm = T),
                                   'Standard Dev' = sd(!!sym(Error_Var), na.rm = T),
                                   'Mean' = mean(!!sym(Error_Var), na.rm = T),
                                   'Sample Size' = sum(!is.na(!!sym(Error_Var))))
                        
}

E1_Summary <- make_summary_table(Speed_Prof_Errors, "e1")
E2_Summary <- make_summary_table(Speed_Prof_Errors, "e2")
E3_Summary <- make_summary_table(Speed_Prof_Errors, "e3")
E4_Summary <- make_summary_table(Speed_Prof_Errors, "e4")
E5_Summary <- make_summary_table(Speed_Prof_Errors, "e5")

fwrite(E1_Summary, file.path(out_data, "E1 Summary.csv"))
fwrite(E2_Summary, file.path(out_data, "E2 Summary.csv"))
fwrite(E3_Summary, file.path(out_data, "E3 Summary.csv"))
fwrite(E4_Summary, file.path(out_data, "E4 Summary.csv"))
fwrite(E5_Summary, file.path(out_data, "E5 Summary.csv"))


##############################
# Creating a summary for mean ORD IAS error, removing all tracks where RTT > Decel distance
# so error is only across the deceleration 

ORD_Profile_Query_Lead <- "SELECT ORD.[Landing_Pair_ID]
                                 ,ORD.[Start_Final_Deceleration_Distance]
                            	   ,LP.[Leader_Flight_Plan_ID]
                           FROM [tbl_ORD_Aircraft_Profile] ORD
                           LEFT JOIN tbl_Landing_Pair LP
                           ON ORD.Landing_Pair_ID = LP.Landing_Pair_ID
                           WHERE This_Pair_Role = 'L' "


ORD_Profile_Query_Foll <- "SELECT ORD.[Landing_Pair_ID]
                                 ,ORD.[Start_Final_Deceleration_Distance]
                            	   ,LP.[Follower_Flight_Plan_ID]
                           FROM [tbl_ORD_Aircraft_Profile] ORD
                           LEFT JOIN tbl_Landing_Pair LP
                           ON ORD.Landing_Pair_ID = LP.Landing_Pair_ID
                           WHERE This_Pair_Role = 'F' "

ORD_Prof_Lead <- Auto_Unit_Conversion(dbGetQuery(con, ORD_Profile_Query_Lead), "SI_to_Aviation")
ORD_Prof_Foll <- Auto_Unit_Conversion(dbGetQuery(con, ORD_Profile_Query_Foll), "SI_to_Aviation")

Mean_IAS_Lead <- left_join(Radar1, ORD_Prof_Lead, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID")) %>% 
                 filter(Start_Final_Deceleration_Distance >= Range_To_Threshold) %>%
                 group_by(Aircraft_Type) %>%
                 summarise(Mean_Lead_IAS_Error = mean(Leader_Forecast_IAS_Error),
                           Std_Dev_Lead_IAS_Error = sd(Leader_Forecast_IAS_Error),
                           Sample_Size_Lead_IAS_Error = sum(!is.na(Leader_Forecast_IAS_Error)))
                
Mean_IAS_Foll <- left_join(Radar2, ORD_Prof_Foll, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID")) %>% 
                 filter(Start_Final_Deceleration_Distance >= Range_To_Threshold) %>%
                 group_by(Aircraft_Type) %>%
                 summarise(Mean_Foll_IAS_Error = mean(Follower_Forecast_IAS_Error),
                           Std_Def_Foll_IAS_Error = sd(Follower_Forecast_IAS_Error),
                           Sample_Size_Foll_IAS_Error = sum(!is.na(Follower_Forecast_IAS_Error)))

Mean_IAS_Error <- full_join(Mean_IAS_Lead, Mean_IAS_Foll, by = "Aircraft_Type")

fwrite(Mean_IAS_Error, file.path(out_data, "Mean_IAS_Error_By_Aircraft_Type.csv"))



#-----------------------------------------------------------------------------#
## Plot aircraft specific ORD profiles against median profile -----------------
#-----------------------------------------------------------------------------#

All_AC_median <- Radar1 %>% group_by(Range_To_Threshold_Bins, Aircraft_Type) %>%
  summarise(Med_Forecast_IAS = median(Leader_Forecast_IAS), Med_Obs_IAS = median(Leader_Forecast_IAS + Leader_Forecast_IAS_Error), med_RTT = median(Range_To_Threshold))

n_page <- ceiling(nrow(All_AC_median %>% select(Aircraft_Type) %>% distinct()) / 9)

for (i in 1:n_page) {

  png(filename = file.path(plots_data, paste0("All_aircraft_median_ORD_", i, ".png")), width = 1920, height = 1080)
  m <- ggplot(All_AC_median) +
    geom_line(aes(med_RTT, Med_Forecast_IAS)) +
    geom_point(aes(med_RTT, Med_Obs_IAS)) +
    facet_wrap_paginate(~Aircraft_Type, ncol = 3, nrow = 3, page = i)
  print(m)
  dev.off()

}


#-----------------------------------------------------------------------------#
## ML Model ------------------------------------------------------------------#
####-- Model trains per category specified (AC Type or LSS) and outputs how --#
####-- Significant each variable is (QNH, Gust, Carrier, Surface Headwind) ---#
#-----------------------------------------------------------------------------#


#function to create linear regression model based on the data
AC_Model <- function(df) {
  
  #checks for if values are applicable in lm model
  Gust_lm <- F
  Baro_lm <- F
  Carrier_lm <- F
  Surface_Headwind_lm <- F
  
  if(length(unique(df$Gust))>1){Gust_lm <- T} #Gust check
  if(length(unique(df$Baro_Pressure))>1){Baro_lm <- T} #Baro check
  if(length(unique(df$Follower_Carrier))>1){Carrier_lm <- T} #Carrier check
  if(length(unique(df$Surface_Headwind))>1){Surface_Headwind_lm <- T} #Headwind check
  
  #makes the model if there is enough data to not cause factoring issues
  if(Gust_lm & Baro_lm & Carrier_lm & Surface_Headwind_lm){
    lm(e1 ~ Gust + Baro_Pressure + Follower_Carrier + Surface_Headwind, data=df)
  }
}

#gets the significance levels for a given model (this is output as a % that variance seen over this variable was down to random chance, i.e. lower=more significant)
Get_P_Values <- function(model) {
  if(!is_null(model)){
    coef(summary(model))[,"Pr(>|t|)"]
  }
}

#gets the significance levels for a given model (this is output as a % that variance seen over this variable was down to random chance, i.e. lower=more significant)
Get_Coeff_Values <- function(model) {
  if(!is_null(model)){
    model$coefficients
  }
}

#linear regression model by AC type
if(ML_model_AC){
  #nests data by aircraft type
  AC_Speed_Profile_Errors <- Speed_Prof_Errors %>% group_by(Follower_Aircraft_Type) %>% filter(!is.na(Gust)) %>% nest()
  
  #creates lm model for each aircraft type
  AC_Speed_Profile_Errors <- AC_Speed_Profile_Errors %>%
    mutate(model = map(data,AC_Model))
  
  #gets significance values for each model
  AC_Speed_Profile_Errors <- AC_Speed_Profile_Errors %>%
    mutate(P_Values = map(model, Get_P_Values))
  
  #gets coefficients for each model
  AC_Speed_Profile_Errors <- AC_Speed_Profile_Errors %>%
    mutate(Co_Effs = map(model, Get_Coeff_Values))
  
  #collates the relevant data from the models
  P_Value_Output <- AC_Speed_Profile_Errors %>% select(Follower_Aircraft_Type, P_Values) %>% unnest_wider(P_Values) %>% relocate(Surface_Headwind, .after=Baro_Pressure)
  Co_Eff_Value_Output <- AC_Speed_Profile_Errors %>% select(Follower_Aircraft_Type, Co_Effs) %>% unnest_wider(Co_Effs) %>% relocate(Surface_Headwind, .after=Baro_Pressure)
  
  
  #gets counts of each aircraft
  Aircraft_Counts <- Speed_Prof_Errors %>% select(Follower_Aircraft_Type) %>% group_by(Follower_Aircraft_Type) %>% mutate(Sample_Size = n()) %>% distinct(Follower_Aircraft_Type, Sample_Size)
  
  #joins the counts onto the significance values
  P_Value_Output <- left_join(P_Value_Output, Aircraft_Counts, by=c("Follower_Aircraft_Type"="Follower_Aircraft_Type")) %>%
    relocate(Sample_Size, .after=Follower_Aircraft_Type)
  Co_Eff_Value_Output <- left_join(Co_Eff_Value_Output, Aircraft_Counts, by=c("Follower_Aircraft_Type"="Follower_Aircraft_Type")) %>%
    relocate(Sample_Size, .after=Follower_Aircraft_Type)
  
  #writes to file
  fwrite(P_Value_Output, file.path(out_data, "P_values_AC_Type.csv"))
  fwrite(Co_Eff_Value_Output, file.path(out_data, "Coefficients_AC_Type.csv"))
}

#linear regression model by LSS type
if(ML_model_LSS){
  #nests data by lss type
  AC_Speed_Profile_Errors <- Speed_Prof_Errors %>% group_by(lss_type) %>% filter(!is.na(Gust)) %>% nest()
  
  #creates lm model for each lss type
  AC_Speed_Profile_Errors <- AC_Speed_Profile_Errors %>%
    mutate(model = map(data,AC_Model))
  
  #gets significance values for each model
  AC_Speed_Profile_Errors <- AC_Speed_Profile_Errors %>%
    mutate(P_Values = map(model, Get_P_Values))
  
  #gets coefficients for each model
  AC_Speed_Profile_Errors <- AC_Speed_Profile_Errors %>%
    mutate(Co_Effs = map(model, Get_Coeff_Values))
  
  #collates the relevant data from the models
  P_Value_Output <- AC_Speed_Profile_Errors %>% select(lss_type, P_Values) %>% unnest_wider(P_Values) %>% relocate(Surface_Headwind, .after=Baro_Pressure)
  Co_Eff_Value_Output <- AC_Speed_Profile_Errors %>% select(lss_type, Co_Effs) %>% unnest_wider(Co_Effs) %>% relocate(Surface_Headwind, .after=Baro_Pressure)
  
  
  #gets counts of each lss type
  LSS_Counts <- Speed_Prof_Errors %>% select(lss_type) %>% group_by(lss_type) %>% mutate(Sample_Size = n()) %>% distinct(lss_type, Sample_Size)
  
  #joins the counts onto the significance values
  P_Value_Output <- left_join(P_Value_Output, LSS_Counts, by=c("lss_type"="lss_type")) %>%
    relocate(Sample_Size, .after=lss_type)
  Co_Eff_Value_Output <- left_join(Co_Eff_Value_Output, LSS_Counts, by=c("lss_type"="lss_type")) %>%
    relocate(Sample_Size, .after=lss_type)
  
  #writes to file
  fwrite(P_Value_Output, file.path(out_data, "P_values_LSS_Type.csv"))
  fwrite(Co_Eff_Value_Output, file.path(out_data, "Coefficients_LSS_Type.csv"))
}

###############################################################################
# Unused manual plots

p <- ggplot(Radar1) +
  geom_boxplot(aes(x=Wind_Effect_IAS_Bins, y=Leader_Forecast_IAS_Error))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
p

p <- ggplot(Radar2) +
  geom_boxplot(aes(x=Wind_Effect_IAS_Bins, y=Follower_Forecast_IAS_Error))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
p


for (i in Distinct_Types) {
  for (j in Error_Type) {

    k_temp <- k %>% filter(Aircraft_Type == i)

    if (nrow(k_temp) > 0) {

      png(filename = file.path(plots_data, paste0(i,"_", j, ".png")), width = 1600, height = 900)
      q <- ggplot(k_temp) +
        geom_boxplot(aes(x = RTT_Bins, y = k_temp[[j]])) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = paste0(i, "_", j, "by Range To Threshold"), x = "Range to Threshold", y = paste0(j))
      print(q)
      dev.off()

    }
  }
}

q <- ggplot(Radar1) +
  geom_boxplot(aes(x = RTT_Bins, y = Leader_Forecast_IAS_Error)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "IAS Error by Range To Threshold in 0.5NM increments", x = "Range to Threshold", y = "Leader Forcast IAS Error")
q

s <- ggplot(Radar1) +
  geom_boxplot(aes(x = RTT_Bins, y = Leader_Forecast_WE_Error)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Wind Error by Range To Threshold in 0.5NM increments", x = "Range to Threshold", y = "Leader Forcast Wind Effect Error")
s





p <- ggplot(Radar2) +
  geom_boxplot(aes(x = RTT_Bins, y = Follower_Forecast_IAS_Error)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "IAS Error by Range To Threshold in 0.5NM increments", x = "Range to Threshold", y = "Follower Forcast IAS Error")
p


r <- ggplot(Radar2) +
  geom_boxplot(aes(x = RTT_Bins, y = Follower_Forecast_WE_Error)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Wind Error by Range To Threshold in 0.5NM increments", x = "Range to Threshold", y = "Follower Forcast Wind Effect Error")
r

#----------------------------------------------------------------------------#
# By aircraft type ----------------------------------------------------------#




## Boxplots of IAS/WE Error By 0.5NM RTT increments.

# Overall, Aircraft Type, Wind Conditions, etc

# Do for Both Leader and Follower: Create functions and reuse.

# Use !!sym(Variable Name Here) to save reusing the same plot in multiple functions i.e.

## ggplot(data <- Data) + geom_boxplot(x = Binned_RTT, y = !!sym(Error_Var)) + facet_wrap(~!!sym(Grouping_Var))




# Finding Precedural speed error

radar1_precedural <- Radar1 %>% filter(Range_To_Threshold > 5)

MD11 <- Radar1 %>% filter(Aircraft_Type == "MD11" & Flight_Plan_ID == 1362)# %>% group_by(Flight_Plan_ID)
MD11 <- Radar1 %>% filter(Aircraft_Type == "C68A" )# %>% group_by(Flight_Plan_ID)

ggplot(MD11) +
  geom_line(aes(Range_To_Threshold, Leader_Forecast_IAS)) +
  geom_point(aes(Range_To_Threshold, Leader_Forecast_IAS + Leader_Forecast_IAS_Error))
  facet_wrap(~Flight_Plan_ID)

MD11_median_vland <- MD11 %>% filter(Range_To_Threshold < 0.5)

MD11_median <- MD11 %>% group_by(Range_To_Threshold_Bins) %>% summarise(Med_Forecast_IAS = median(Leader_Forecast_IAS), Med_Obs_IAS = median(Leader_Forecast_IAS + Leader_Forecast_IAS_Error), med_RTT = median(Range_To_Threshold))

ggplot(MD11_median) +
  geom_line(aes(med_RTT, Med_Forecast_IAS)) +
  geom_point(aes(med_RTT, Med_Obs_IAS))



All_AC_median <- Radar1 %>% group_by(Range_To_Threshold_Bins, Aircraft_Type) %>%
  summarise(Med_Forecast_IAS = median(Leader_Forecast_IAS), Med_Obs_IAS = median(Leader_Forecast_IAS + Leader_Forecast_IAS_Error), med_RTT = median(Range_To_Threshold)) %>%
  ungroup()



n_page <- ceiling(nrow(All_AC_median %>% select(Aircraft_Type) %>% distinct()) / 9)

for (i in 1:n_page) {

  png(filename = file.path(plots_data, paste0("All_aircraft_median_ORD_", i, ".png")), width = 1920, height = 1080)
 m <- ggplot(All_AC_median) +
    geom_line(aes(med_RTT, Med_Forecast_IAS)) +
    geom_point(aes(med_RTT, Med_Obs_IAS)) +
    facet_wrap_paginate(~Aircraft_Type, ncol = 3, nrow = 3, page = i)
 print(m)
 dev.off()

}
