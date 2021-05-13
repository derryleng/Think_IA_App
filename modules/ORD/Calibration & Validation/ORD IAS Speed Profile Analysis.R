# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Glidslope  Analysis                                   #
# Version No.    |  1.0                                                   #
# Date Modified  |  02/07/2020                                            #
# Author(s)      |  Michael Cowham                                        #
# Project        |  Toronto IA                                            #
# Purpose        |  To assess the glideslope variability                  #
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

#Airport Code
Airport_Code <- "CYYZ"

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Calibration & Validation"
Script_out <- "ILS Profile"
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
# source(file.path(Global_Dir, "Imports.R"), local = F) #There is a package in here conflicting with shift()
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

inputs_dir <- GetSaveDirectory(Project = project, Algorithm = "Flying_Time_Analysis_Input", IorO = "Inputs")
# ref_data <- file.path(inputs_dir, "Reference Data")

# out_data <- Base_Dir
ord_dir <- Base_Dir

out_dir <- file.path(ord_dir, "ILS Profile", version)
Create_Directory(out_dir)




# #Base project directory
# project_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "23 Data Analysis")
#
# #Directory for all script outputs
# base_dir <- file.path(project_dir, "Outputs")
#
# #Directory for reference data
# inputs_dir <- file.path(project_dir, "Inputs")
#
# #Database specific folders for NavCan especially where we might use the old
# #non-mode S data sometimes for comparison.
# database_dir <- file.path(base_dir, database)
# if (!dir.exists(database_dir)) dir.create(database_dir)
#
# ord_dir <- file.path(database_dir, "ORD")
# if (!dir.exists(ord_dir)) dir.create(ord_dir)
#
# out_data <- file.path(ord_dir, "ILS Profile", version)
# if (!dir.exists(out_data)) dir.create(out_data)
#
# setwd(out_data)





# Whether to use the database connection or a locally stored .csv
use_database <- T
# The database connection
if (use_database == T) {
  con <- Get_RODBC_Database_Connection(IP = ip, Database = database)
}
# Reference file direcory
if (use_database == F) {
  ref_data <- GetSaveDirectory(Project = project, Algorithm = "Flying_Time_Analysis_Input", IorO = "Inputs")
}









# Reference data directory
#ref_data <- file.path(base_dir, "Inputs")

# Output directory
#out_data <- file.path(base_dir, "Outputs", paste0("v", version))
#if (!dir.exists(out_data)) dir.create(out_data)
#setwd(out_data)

# Filtering Parameters

max_track_spd_diff <- 15  # The maximum difference allowed between the observation and smoothed groundspeed
min_time <- 5 # Time filter - not used
max_time <- 22 # Time filter - not used

min_gspd <- 50 # The absolute minimum groundspeed to keep out large errors
max_gspd <- 250 # The absolute maximum groundspeed to keep out large errors

m_to_ft <- 3.28084

#wake_cats <- c("A", "B", "C", "D", "E", "F")
#wake_cats <- as.data.frame(wake_cats)
#names(wake_cats) <- c("Wake_Cat")

min_obs <- 10  # Minimum number of observations in a day / DME_Seg / Runway Group for smoothing
obs_per_hour <- 1 # Minimum observations per hour for consideration

max_diff_smoothed_trend <- 15


rw_main <- c("R05", "R06L", "R06R", "R23", "R24L", "R24R")
rw_other <- c("R33L", "R33R", "R15L", "R15R")
output_ref_data <- F

# ----------------------------------------------------------------------- #
# 1. Functions ----------------------------------------------------------
# ----------------------------------------------------------------------- #



calculate_glideslope_altitude <- function(rwy, dtt_m){

  # Threshold Elevation = 3 degrees on the touchdown offset

  # rwy <- "R27"
  rwy_data <- filter(rwy_adaptation, Runway_Name == rwy)

  thresh_elev <- rwy_data$Touchdown_Offset * tan(rwy_data$Glideslope_Angle) + rwy_data$Elevation
  glideslope_altitude <- m_to_ft * (thresh_elev + dtt_m * tan(rwy_data$Glideslope_Angle))

  if(is.na(dtt_m)){
    return(NA)}
  else {
    return(glideslope_altitude)
  }
}

calculate_glideslope_altitude_vec <- Vectorize(calculate_glideslope_altitude)

calculate_glideslope_altitude("R05", 10)
# ----------------------------------------------------------------------- #
# 2. Data Load ----------------------------------------------------------
# ----------------------------------------------------------------------- #

tryCatch(
  if (use_database == T) {
    if (exists("con")) {
      data1 <- sqlQuery(con, sprintf("SELECT * FROM vw_Flying_Time_Analysis"))
    }
  } else {
    data1 <- fread(file.path("C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\4. TBS Calculations\\Inputs", "vw_Flying_Time_Analysis.csv"))
    # data1 <- fread(file.path(ref_data, "vw_Flying_Time_Analysis.csv"))
  },
  error = function(e) stop("Validation view not found!")
)

# Load the reference data

rw <- sqlQuery(con, "SELECT Runway_Name, Runway_Group FROM tbl_Runway") %>% rename(Landing_Runway = Runway_Name)
rwy_adaptation <- sqlQuery(con, "SELECT * FROM tbl_Runway")

# ----------------------------------------------------------------------- #
# 2. Graph Functions-----------------------------------------------------
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# 3. Data Processing ----------------------------------------------------
# ----------------------------------------------------------------------- #

data1 <- inner_join(data1, rw, by = c("Landing_Runway"))

# Add new fields

data1 <- mutate(data1, Surface_Wind_Speed_Group = cut(Surface_Wind_SPD, breaks = c(0, 3, 6, 10, 15, 20, Inf)))

data1 <- arrange(data1, Landing_Runway, FP_Date, DME_Seg, Max_Track_Time)
data1 <- group_by(data1, Landing_Runway, FP_Date, DME_Seg) %>%
  mutate(Follower_Time_Rw = shift(Max_Track_Time, n = -1),
         Follower_Ave_Mode_C_Rw = shift(Ave_Corrected_Mode_C, n = -1),
         Follower_Ave_Wind_Effect_Rw = shift(Ave_Wind_Effect_IAS, n = -1))


data1 <- inner_join(data1, select(rwy_adaptation, Runway_Name, Touchdown_Offset, Elevation, Glideslope_Angle), by = c("Landing_Runway" = "Runway_Name"))
data1 <- mutate(data1, Glideslope_Alititude = m_to_ft * (Touchdown_Offset * tan(Glideslope_Angle) + Elevation + 1852 * (DME_Seg + 0.5) * tan(Glideslope_Angle)),
                Glideslope_Deviation = Ave_Corrected_Mode_C - Glideslope_Alititude)


data1 <- arrange(data1, Runway_Group, FP_Date, DME_Seg, Max_Track_Time)
data1 <- group_by(data1, Runway_Group, FP_Date, DME_Seg) %>%
  mutate(Follower_Time_Gp = shift(Max_Track_Time, n = -1),
         Follower_Ave_Mode_C_Gp = shift(Ave_Corrected_Mode_C, n = -1),
         Follower_Ave_Wind_Effect_Gp = shift(Ave_Wind_Effect_IAS, n = -1))

data1 <- mutate(data1, Time_Diff_rw = Follower_Time_Rw - Max_Track_Time,
                Alt_Diff_rw = Follower_Ave_Mode_C_Rw - Ave_Corrected_Mode_C,
                WE_Diff_rw = Follower_Ave_Wind_Effect_Rw - Ave_Wind_Effect_IAS,
                Time_Diff_gp = Follower_Time_Gp - Max_Track_Time,
                Alt_Diff_gp = Follower_Ave_Mode_C_Gp - Ave_Corrected_Mode_C,
                WE_Diff_gp = Follower_Ave_Wind_Effect_Gp - Ave_Wind_Effect_IAS)

#data1 <- inner_join(data1, rename(icao_categories, Follower_ICAO4 = ICAO_WTC), by = c("Aircraft_Type" = "Aircraft_Type"))
#data1 <- inner_join(data1, rename(icao_categories, Leader_ICAO4 = ICAO_WTC), by = c("Leader_Aircraft_Type" = "Aircraft_Type"))

#data1 <- left_join(data1, icao_sep, by = c("Follower_ICAO4" = "Follower_ICAO_WTC", "Leader_ICAO4" = "Leader_ICAO_WTC"))

#data1 <- mutate(data1,  Delivered_4DME_Separation = Delivered_4DME_Separation / 1852,
#                        ifelse(is.na(Reference_Wake_Separation_Distance), 3, Reference_Wake_Separation_Distance),
#                        Separation_Accuracy = Delivered_4DME_Separation - Reference_Wake_Separation_Distance)


data2 <- filter(data1, Landing_Runway %in% rw_main, Landing_Pair_Type != "Not_In_Trail")
data3 <- filter(data1, Landing_Runway %in% rw_other, Landing_Pair_Type != "Not_In_Trail")


data2_total <- group_by(filter(data2, DME_Seg == 0), Landing_Runway) %>%
  summarise(Total_Count_Rw = n())

data2_summary <- group_by(data2, Landing_Runway, DME_Seg) %>%
  summarise(Total_Count_DME = n())

data2_total <- group_by(data2_summary, Landing_Runway) %>%
  summarise(Total_Count_Rw = max(Total_Count_DME, na.rm = T))

data2_summary <- inner_join(data2_summary, data2_total, by = c("Landing_Runway")) %>%
  mutate(DME_Pc = as.numeric(Total_Count_DME) / as.numeric(Total_Count_Rw))

# (Corrected) Altitude Distribution Plot

png(file.path(out_dir, "Altitude Profile.png"),  width = 600, height = 500)
ggplot(data = data2, aes(x = DME_Seg, y = Ave_Corrected_Mode_C))+
  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg), outlier.shape = NA)+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(0, 8000)+
  labs(title = "Boxplot of Localiser Alititude", x = "DME", y = "Glideslope Alt (ft)")+
  facet_wrap(~Landing_Runway)
dev.off()
# Glideslope Deviation Plots

png(file.path(out_dir, "Glideslope Deviation Main.png"),  width = 600, height = 500)
ggplot(data = data2, aes(x = DME_Seg, y = Glideslope_Deviation))+
  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(-4000, 4000)+
  labs(title = "Boxplot of Localiser Alititude Deviation", x = "DME", y = "Glideslope Deviation (ft)")+
  facet_wrap(~Landing_Runway)
dev.off()

png(file.path(out_dir, "Glideslope Deviation Other.png"),  width = 600, height = 500)
ggplot(data = data3, aes(x = DME_Seg, y = Glideslope_Deviation))+
  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(-4000, 4000)+
  labs(title = "Boxplot of Localiser Alititude Deviation", x = "DME", y = "Glideslope Deviation (ft)")+
  facet_wrap(~Landing_Runway)
dev.off()


ggplot(data = data2_summary, aes(x = DME_Seg, y = DME_Pc))+
  geom_bar(stat="identity", fill="steelblue")+
  labs(title = "Proportion of Observations by Runway", x = "DME", y = "Proportion")+
  facet_wrap(~Landing_Runway)

ggplot(data = filter(data2, Time_Diff_rw <= 300), aes(x = DME_Seg, y = Alt_Diff_rw))+
  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg), outlier.shape = NA)+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(-2000, 2000)+
  labs(title = "Boxplot of Localiser Alititude Difference (Runway)", x = "DME", y = "Glideslope Alt Diff (ft)")+
  facet_wrap(~Landing_Runway)

ggplot(data = filter(data2, Time_Diff_gp <= 300), aes(x = DME_Seg, y = Alt_Diff_gp))+
  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg), outlier.shape = NA)+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(-2000, 2000)+
  labs(title = "Boxplot of Localiser Alititude Difference (Runway Group)", x = "DME", y = "Glideslope Alt Diff (ft)")+
  facet_wrap(~Landing_Runway)

# Wind Effect Difference Plots

ggplot(data = filter(data2, Time_Diff_rw <= 300), aes(x = DME_Seg, y = WE_Diff_rw))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg), outlier.shape = NA)+

  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(-10, 10 )+
  labs(title = "Boxplot of Wind Effect Difference (Runway)", x = "DME", y = "Wind Effect Difference (kt)")+
  facet_wrap(~Landing_Runway)

ggplot(data = filter(data2, Time_Diff_gp <= 300), aes(x = DME_Seg, y = WE_Diff_gp))+
  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg), outlier.shape = NA)+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(-10, 10 )+
  labs(title = "Boxplot of Wind Effect Difference (Runway Group)", x = "DME", y = "Wind Effect Difference (kt)")+
  facet_wrap(~Landing_Runway)


# Wind Effect Difference Plots

ggplot(data = filter(data2, Time_Diff_rw <= 300), aes(x = DME_Seg, y = WE_Diff_rw))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg), outlier.shape = NA)+

  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_label(stat="count",aes(label=..count..), y = 7000)+
  ylim(-10, 10 )+
  labs(title = "Boxplot of Wind Effect Difference (Runway)", x = "DME", y = "Wind Effect Difference (kt)")+
  facet_wrap(~Surface_Wind_Speed_Group)

# IAS Speed Profiles

ggplot(data = data2, aes(x = DME_Seg, y = Ave_Mode_S_IAS))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_boxplot()+
  geom_boxplot(aes(group=DME_Seg))+
  ylim(100, 200)+
  xlim(0, 10)+
  labs(title = "Boxplot of IAS Profile (By Surface Wind)", x = "DME", y = "IAS (kt)")+
  facet_wrap(~Surface_Wind_Speed_Group)

ggplot(data = data2, aes(x = DME_Seg, y = Ave_Mode_S_IAS))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_boxplot()+
  geom_boxplot(aes(group=DME_Seg))+
  ylim(100, 200)+
  xlim(0, 10)+
  labs(title = "Boxplot of IAS Profile (By Runway)", x = "DME", y = "IAS (kt)")+
  facet_wrap(~Landing_Runway)

ggplot(data = filter(data2, DME_Seg <= 10), aes(x = DME_Seg, y = Ave_Mode_S_IAS))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_boxplot()+
  geom_boxplot(aes(group=DME_Seg), outlier.shape = NA)+
  geom_hline(yintercept = 170)+
  scale_y_continuous(breaks = seq(100,200, 20), limits=c(100,200))+
  scale_x_continuous(breaks = seq(0, 10, 1))+
  # ylim(100, 200)+
  #  xlim(0, 10)+
  labs(title = "Boxplot of IAS Profile", x = "DME", y = "IAS (kt)")+
  facet_wrap(~Landing_Runway)


# Overall Plot

ggplot(data = filter(data2, DME_Seg <= 10, Separation_Accuracy <= 3), aes(x = DME_Seg, y = Ave_Mode_S_IAS))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_boxplot()+
  geom_boxplot(aes(group=DME_Seg))+
  geom_hline(yintercept = 160)+
  scale_y_continuous(breaks = seq(100,200, 20), limits=c(100,200))+
  scale_x_continuous(breaks = seq(0, 10, 1))+
  # ylim(100, 200)+
  #  xlim(0, 10)+
  labs(title = "Boxplot of IAS Profile", x = "DTT (NM)", y = "IAS (kt)")


# By Landing Runway

ggplot(data = filter(data2, DME_Seg <= 10, Separation_Accuracy <= 3), aes(x = DME_Seg, y = Ave_Mode_S_IAS))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_boxplot()+
  geom_boxplot(aes(group=DME_Seg))+
  geom_hline(yintercept = 160)+
  scale_y_continuous(breaks = seq(100,200, 20), limits=c(100,200))+
  scale_x_continuous(breaks = seq(0, 10, 1))+
  # ylim(100, 200)+
  #  xlim(0, 10)+
  labs(title = "Boxplot of IAS Profile", x = "DTT (NM)", y = "IAS (kt)")+
  facet_wrap(~Landing_Runway)

ggplot(data = filter(data3, DME_Seg <= 10, Separation_Accuracy <= 3), aes(x = DME_Seg, y = Ave_Mode_S_IAS))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_boxplot()+
  geom_boxplot(aes(group=DME_Seg))+
  geom_hline(yintercept = 160)+
  scale_y_continuous(breaks = seq(100,200, 20), limits=c(100,200))+
  scale_x_continuous(breaks = seq(0, 10, 1))+
  # ylim(100, 200)+
  #  xlim(0, 10)+
  labs(title = "Boxplot of IAS Profile", x = "DTT (NM)", y = "IAS (kt)")+
  facet_wrap(~Landing_Runway)

# By Wake Category

ggplot(data = filter(data2, DME_Seg <= 10, Separation_Accuracy <= 3), aes(x = DME_Seg, y = Ave_Mode_S_IAS))+
  #  geom_boxplot(aes(group=DME_Seg, fill = DME_Seg))+
  #  geom_boxplot()+
  geom_boxplot(aes(group=DME_Seg))+
  geom_hline(yintercept = 160)+
  scale_y_continuous(breaks = seq(100,200, 20), limits=c(100,200))+
  scale_x_continuous(breaks = seq(0, 10, 1))+
  # ylim(100, 200)+
  #  xlim(0, 10)+
  labs(title = "Boxplot of IAS Profile", x = "DTT (NM)", y = "IAS (kt)")+
  facet_wrap(~Wake_Cat)


ggplot(data = filter(data1, DME_Seg == 4, Landing_Pair_Type != "Not_In_Trail"))+
  geom_histogram(mapping = aes(x = Separation_Accuracy, y = ..density..))
