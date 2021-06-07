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

#Airport Code
Airport_Code <- "CYYZ"

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

project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3", noblank = FALSE, forcemask = FALSE))

#IorO for "Ouputs" or "Inputs" directory, must be a string as used in directory
Base_Dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

# out_data <- Base_Dir
ord_dir <- Base_Dir

out_data <- file.path(ord_dir, "Radar ORD Analysis", version)
Create_Directory(out_data)

plots_data <- file.path(out_data, "Plots")
Create_Directory(plots_data)


con <- Get_DBI_Connection(IP = ip, Database = database)



# Function to return the ceiling to 1dp - unused as its unnecessary
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# the "not in" function
"%!in%" <- function(x,y) !("%in%"(x,y))

# Get Database Connection (RODBC)
Get_RODBC_Database_Connection <- function(IP, Database){
  User <- getPass(msg = "Username: ", noblank = FALSE, forcemask = FALSE)
  Pass <- getPass(msg = "Password: ", noblank = FALSE, forcemask = TRUE)
  con <- RODBC::odbcDriverConnect(connection=paste0("Driver={SQL Server};
                                  Server={",IP,"};Database={", Database, "};
                                  Uid={",User,"};Pwd={",Pass,"};"))
  return(con)
}

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

  segments <- seq(floor(min(Radar1[[Grouping]], na.rm = T)), ceiling(max(Radar1[[Grouping]], na.rm = T)), BinWidth)

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
  m <- ggplot(dat %>% filter(!!sym(Error_Var) < Max_Error_Val)) +
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
    m <- ggplot(dat %>% filter(!!sym(Error_Var) < Max_Error_Val)) +
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
  m <- ggplot(dat %>% filter(Aircraft_Type == AC_Type & !!sym(Error_Var) < Max_Error_Val)) +
    geom_boxplot(aes(x = Range_To_Threshold_Bins, y = !!sym(Error_Var))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
  print(m)
  dev.off()

}

# ----------------------------------------------------------------------------------------------------- #


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

Radar_o <- Radar
GS_o <- GS_Profile

# ----------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------- #

# Revert
Radar <- Radar_o
GS_Profile <- GS_o

Radar1 <- Generate_ORD_Profile_Radar_Point_Error(Radar, GS_Profile, LorF = "Leader", Include_Intercept_ILS = T) %>%
  filter(!is.na(Leader_Forecast_IAS_Error))
Radar2 <- Generate_ORD_Profile_Radar_Point_Error(Radar, GS_Profile, LorF = "Follower", Include_Intercept_ILS = T) %>%
  filter(!is.na(Follower_Forecast_IAS_Error))

# ----------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------- #

# Use fpid to get aircraft type and wake categories from db

FPIDS <- Radar %>% select(Flight_Plan_ID) %>% distinct() %>% pull(Flight_Plan_ID)
FPID_String <- toString(sprintf("'%s'", FPIDS))

Type_Query <- "SELECT Flight_Plan_ID, Aircraft_Type FROM tbl_Flight_Plan WHERE Flight_Plan_ID IN (%s)"

Type_Query <- sprintf(Type_Query, FPID_String)

Aircraft_Types <- dbGetQuery(con, Type_Query)

Wake_Categories <- dbGetQuery(con, "SELECT Aircraft_Type, Wake FROM tbl_Aircraft_Type_To_Wake")

Aircraft_Types <- left_join(Aircraft_Types, Wake_Categories, by = "Aircraft_Type")

Radar1 <- left_join(Radar1, Aircraft_Types, by = "Flight_Plan_ID")
Radar2 <- left_join(Radar2, Aircraft_Types, by = "Flight_Plan_ID")

Radar1 <- Bin_Radar_Data(Radar1, "Range_To_Threshold", 0.5)
Radar2 <- Bin_Radar_Data(Radar2, "Range_To_Threshold", 0.5)
Radar1 <- Bin_Radar_Data(Radar1, "Wind_Effect_IAS", 5)
Radar2 <- Bin_Radar_Data(Radar2, "Wind_Effect_IAS", 5)




#-----------------------------------------------------------------------------#
## Create Plots ---------------------------------------------------------------
#-----------------------------------------------------------------------------#

# Plot an error type with a grouping faceted
# plot_error_by_group(dat, Error_Var, Grouping_Var, Max_Error_Val)
# If no filtering wanted, set Max_Error_Val to Inf

plot_error_by_group(Radar1, "Leader_Forecast_IAS_Error", "Wake", 150)

# Plots an error type for all aircraft types paginated facet wrap
# NB. takes a long time to execute
# plot_error_all_ac(dat, Error_Var, Max_Error_Val, height, width)
# height and width refer to the number of plots per page

plot_error_all_ac(Radar1, "Leader_Forecast_IAS_Error", 200, 3, 3)

# Plot an error type for an specific aircraft type
# plot_error_specific_ac_type(dat, Error_Var, Max_Error_Val, AC_Type)

plot_error_specific_ac_type(Radar1, "Leader_Forecast_IAS_Error", Inf, "A333")


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
