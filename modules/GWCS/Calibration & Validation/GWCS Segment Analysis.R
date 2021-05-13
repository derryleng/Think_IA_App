# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  IA GWCS Segment Exclusion Analysis                    #
#                |                                                        #
# Version No.    |  1.2                                                   #
#                |                                                        #
# Date Modified  |  12/01/2021                                            #
#                |                                                        #
# Author(s)      |  Michael Cowham                                        #
#                |                                                        #
# Project        |  IA (NavCan)                                           #
#                |                                                        #
# Purpose        |  GWCS Analysis - check segment exclusion               #
#                |                                                        #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
#
# 1 Initial Script
#
# v1.2 Updated to support new filestructure ready for git integration
#
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# Load Packages ----------------------------------------------------------
# ----------------------------------------------------------------------- #

rm(list = ls())

library(RODBC)
library(ggplot2)
library(lattice)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(data.table)
library(openair)
library(lubridate)
library(getPass)

# ----------------------------------------------------------------------- #
# Parameters   ----------------------------------------------------------
# ----------------------------------------------------------------------- #



# ----------------------------------------------------------------------- #
# Initialisation --------------------------------------------------------
# ----------------------------------------------------------------------- #

#Use a version number derived from date or define manually
version <- paste0(Sys.Date(), " ","V1.0 (AH)")
# version <- "2021-05-04 V1.0 (AH)"

#Not applicatable to this script uses database only
# use_same_input_version <- T
#
# if (use_same_input_version == T) {
#   input_version <- version
# } else if (use_same_input_version == F) {
#   input_version <- "2021-05-04 V1.0 (AH)"   #Manually define this if you want different input output version numbers
# }

#Set server  with IP then tied to this
Server <- "Maverick" #or Goose

if (Server == "Maverick") {ip <- "192.168.1.23"}
if (Server == "Goose") {ip <- "192.168.1.39"}

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "GWCS"
ModuleSubfolder <- "Calibration & Validation"
Script_out <- "Segment Analysis"
OutputFolder <- paste(ModuleFolder, Script_out, version, sep = "/")
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

Base_Dir <- GetSaveDirectory(Algorithm =  OutputFolder, IorO = "Outputs")
Create_Directory(Base_Dir)


# input <- GetSaveDirectory(Algorithm = paste(ModuleFolder, version, sep = "/"), IorO = "Inputs")

out_data <- Base_Dir

#Set the database name for SQL connection
database <- "NavCan_TBS_V3"

con <- Get_RODBC_Database_Connection(IP = ip, Database = database)

# ----------------------------------------------------------------------- #
# Load Data      --------------------------------------------------------
# ----------------------------------------------------------------------- #

wind_seg <- sqlQuery(con, "SELECT * FROM vw_Mode_S_Wind_Seg")
wind_adaptation <- sqlQuery(con, "SELECT * FROM vw_Mode_S_Wind_Adaptation")

# ----------------------------------------------------------------------- #
# Analysis      --------------------------------------------------------
# ----------------------------------------------------------------------- #

# Create new fields

wind_seg <- mutate(wind_seg, Diff_Mode_S_To_Radar_GSPD = Ave_Track_SPD - Ave_Mode_S_GSPD)

# Rate of Segment Exclusions by DME Overall

ex_dme <- wind_seg %>% group_by(DME_Seg) %>% summarise(TotalN = n(), Total_Invalid = sum(Global_Flag, na.rm = T)) %>% ungroup() %>% mutate(PC_Invalid = 100 * Total_Invalid / TotalN)

# Rate of Segment Exclusions by Flags

ex_dme_flag <- wind_seg %>% group_by(DME_Seg) %>% summarise(TotalN = n(), Total_Seg_Duration = sum(Seg_Duration_Flag, na.rm = T),
                                                                          Total_Track_To_Runway_HDG = sum(Diff_Track_To_Runway_HDG_Flag, na.rm = T),
                                                                          Total_HDG_To_Runway_HDG = sum(Diff_HDG_To_Runway_HDG_Flag, na.rm = T),
                                                                          Total_Mode_S_To_Radar_Track = sum(Diff_Mode_S_To_Radar_Track_Flag, na.rm = T),
                                                                          Total_Mode_S_To_Radar_GSPD = sum(Diff_Mode_S_To_Radar_GSPD_Flag, na.rm = T),
                                                                          Total_Max_Wind_Effect = sum(Max_Wind_Effect_Flag, na.rm = T),
                                                                          Total_Max_Wind_SPD = sum(Max_Wind_SPD_Flag, na.rm = T)) %>% ungroup() %>%
                                                  mutate(PC_Seg_Duration = 100 * Total_Seg_Duration / TotalN,
                                                         PC_Track_To_Runway_HDG = 100 * Total_Track_To_Runway_HDG / TotalN,
                                                         PC_HDG_To_Runway_HDG = 100 * Total_HDG_To_Runway_HDG / TotalN,
                                                         PC_Mode_S_To_Radar_Track = 100 *  Total_Mode_S_To_Radar_Track / TotalN,
                                                         PC_Mode_S_To_Radar_GSPD = 100 *  Total_Mode_S_To_Radar_GSPD / TotalN,
                                                         PC_Total_Max_Wind_Effect = 100 * Total_Max_Wind_Effect / TotalN,
                                                         PC_Max_Wind_SPD = 100 * Total_Max_Wind_SPD / TotalN)

fwrite(ex_dme_flag, file.path(out_data, "flag_counts.csv"))


# Rate of Segment Exclusions by Flags and Runway

ex_dme_flag_rw <- wind_seg %>% group_by(Landing_Runway, DME_Seg) %>% summarise(TotalN = n(), Total_Seg_Duration = sum(Seg_Duration_Flag, na.rm = T),
                                                            Total_Track_To_Runway_HDG = sum(Diff_Track_To_Runway_HDG_Flag, na.rm = T),
                                                            Total_HDG_To_Runway_HDG = sum(Diff_HDG_To_Runway_HDG_Flag, na.rm = T),
                                                            Total_Mode_S_To_Radar_Track = sum(Diff_Mode_S_To_Radar_Track_Flag, na.rm = T),
                                                            Total_Mode_S_To_Radar_GSPD = sum(Diff_Mode_S_To_Radar_GSPD_Flag, na.rm = T),
                                                            Total_Max_Wind_Effect = sum(Max_Wind_Effect_Flag, na.rm = T),
                                                            Total_Max_Wind_SPD = sum(Max_Wind_SPD_Flag, na.rm = T)) %>% ungroup() %>%
  mutate(PC_Seg_Duration = 100 * Total_Seg_Duration / TotalN,
         PC_Track_To_Runway_HDG = 100 * Total_Track_To_Runway_HDG / TotalN,
         PC_HDG_To_Runway_HDG = 100 * Total_HDG_To_Runway_HDG / TotalN,
         PC_Mode_S_To_Radar_Track = 100 *  Total_Mode_S_To_Radar_Track / TotalN,
         PC_Mode_S_To_Radar_GSPD = 100 *  Total_Mode_S_To_Radar_GSPD / TotalN,
         PC_Total_Max_Wind_Effect = 100 * Total_Max_Wind_Effect / TotalN,
         PC_Max_Wind_SPD = 100 * Total_Max_Wind_SPD / TotalN)

fwrite(ex_dme_flag_rw, file.path(out_data, "flag_counts_rw.csv"))

# ----------------------------------------------------------------------- #
# Ad-Hoc Analysis   -----------------------------------------------------
# ----------------------------------------------------------------------- #

DME_Min <- 0
DME_Max <- 9

# Check the Mode S GSPD to Radar Track Speed Differences
# Not too much evidence of problems in Seg 0 - coasting etc.

png(file.path(out_data, "Mode S to radar GSPD diff.png"), width = 900, height = 900)
m <- ggplot(data = filter(wind_seg, DME_Seg >= DME_Min, DME_Seg < DME_Max))+
       geom_histogram(mapping = aes(x = Diff_Mode_S_To_Radar_GSPD, y = ..density..), binwidth = 1, color = "black", fill = "blue")+
       geom_vline(xintercept = -wind_adaptation$Diff_Mode_S_To_Radar_GSPD_Max)+
       geom_vline(xintercept = wind_adaptation$Diff_Mode_S_To_Radar_GSPD_Max)+
       facet_wrap(~DME_Seg)+
       theme_classic()
print(m)
dev.off()

# Check the Segment Duration versus Flown Distance

png(file.path(out_data, "RTT against Track time.png"), width = 900, height = 900)
m <- ggplot(data = filter(wind_seg, DME_Seg < DME_Max))+
       geom_point(mapping = aes(x = Max_RTT - Min_RTT, y = Max_Track_Time - Min_Track_Time, color = as.factor(Seg_Duration_Flag)))+
       ylim(0, 30)+
       facet_wrap(~DME_Seg)+
       theme_classic()
print(m)
dev.off()

# Check the Segment Duration Distribution

png(file.path(out_data, "Segment Duration Histogram.png"), width = 900, height = 900)
m <- ggplot(data = filter(wind_seg, DME_Seg < DME_Max))+
  geom_histogram(mapping = aes(x = Max_Track_Time - Min_Track_Time, y = ..density..), binwidth = 1, color = "black", fill = "blue")+
 # ylim(0, 30)+
  xlim(0, 40)+
  facet_wrap(~DME_Seg)+
  theme_classic()
print(m)
dev.off()

png(file.path(out_data, "Observation Count.png"), width = 900, height = 900)
m <- ggplot(data = filter(wind_seg, DME_Seg < DME_Max, Global_Flag == 0))+
  geom_histogram(mapping = aes(x = Obs_Count, y = ..density..), binwidth = 1, color = "black", fill = "blue")+
  # ylim(0, 30)+
  xlim(0, 10)+
  geom_vline(xintercept = 1)+
  facet_wrap(~DME_Seg)+
  theme_classic()
print(m)
dev.off()

# Check ths Min_RTT in segment 0
# Looks worse for all except R24L/R.  Did Jason say coverage not as good for R33?

ggplot(data = filter(wind_seg, DME_Seg == 0))+
  geom_histogram(mapping = aes(x = Min_RTT, y = ..density..), binwidth = 0.1, color = "black", fill = "blue")+
  facet_wrap(~Landing_Runway)+
  theme_classic()

ggplot(ex_dme_flag) +
  geom_col(aes(DME_Seg, Total_Seg_Duration))
ggplot(ex_dme_flag) +
  geom_col(aes(DME_Seg, PC_Seg_Duration))

# Recommendations

# Decrease the minimum segment duration to 9s as per LVNL.  Improve rate for Seg0
# Increase the Diff_Mode_S_To_Radar_GSPD value to 10kt as per LVNL as well.
#

# Plots of the PC differences by flag and runway

ggplot(ex_dme_flag_rw)+
  geom_line(aes(x = DME_Seg, y = PC_Seg_Duration, color = Landing_Runway))+
  geom_point(aes(x = DME_Seg, y = PC_Seg_Duration, color = Landing_Runway))+
  theme_bw()

ggplot(ex_dme_flag_rw)+
  geom_line(aes(x = DME_Seg, y = PC_Track_To_Runway_HDG, color = Landing_Runway))+
  geom_point(aes(x = DME_Seg, y = PC_Track_To_Runway_HDG, color = Landing_Runway))+
  theme_bw()

ggplot(ex_dme_flag_rw)+
  geom_line(aes(x = DME_Seg, y = PC_HDG_To_Runway_HDG, color = Landing_Runway))+
  geom_point(aes(x = DME_Seg, y = PC_HDG_To_Runway_HDG, color = Landing_Runway))+
  theme_bw()

ggplot(ex_dme_flag_rw)+
  geom_line(aes(x = DME_Seg, y = PC_Mode_S_To_Radar_Track, color = Landing_Runway))+
  geom_point(aes(x = DME_Seg, y = PC_Mode_S_To_Radar_Track, color = Landing_Runway))+
  theme_bw()

ggplot(ex_dme_flag_rw)+
  geom_line(aes(x = DME_Seg, y = PC_Mode_S_To_Radar_GSPD, color = Landing_Runway))+
  geom_point(aes(x = DME_Seg, y = PC_Mode_S_To_Radar_GSPD, color = Landing_Runway))+
  theme_bw()

# Max Wind Speed

max_wind_speed <- filter(wind_seg, Max_Wind_SPD_Flag == 1)

# ----------------------------------------------------------------------- #
# Attempt a Wind Rose             ---------------------------------------
# ----------------------------------------------------------------------- #

wr_data <- filter(wind_seg, Global_Flag == 0, DME_Seg < 6)
wr_data <- mutate(wr_data, Ave_Wind_HDG = (Ave_Wind_HDG + 180) %% 360, FP_Date_2 = dmy(FP_Date))
wr_data_cav <- filter(wr_data, FP_Date %in% c("19/11/2020","23/11/2020","28/11/2020", "01/12/2020", "07/12/2020"))

#windRose(wr_data, ws = "Ave_Wind_SPD", wd = "Ave_Wind_HDG", paddle = F, breaks = c(0, 10, 20, 30, 40, 50, 60, Inf))

png(file.path(out_data, "Wind_Rose_Date.png"),  width = 600, height = 500)
windRose(wr_data, ws = "Ave_Wind_SPD", wd = "Ave_Wind_HDG", type = "FP_Date_2", paddle = F, breaks = c(0, 10, 20, 30, 40,50), key.footer = "kt", annotate = F, auto.text = F)
dev.off()

png(file.path(out_data, "Wind_Rose_Runway.png"),  width = 700, height = 500)
windRose(wr_data, ws = "Ave_Wind_SPD", wd = "Ave_Wind_HDG", type = "Landing_Runway", paddle = F, breaks = c(0, 10, 20, 30, 40, 50), key.footer = "kt", annotate = F, auto.text = F)
dev.off()

max(wr_data_cav$Ave_Wind_SPD)


ggplot(data = wr_data)+
  geom_histogram(mapping = aes(x = Ave_Wind_SPD, y = ..density..), binwidth = 5)

ggplot(data = wr_data)+
  geom_histogram(mapping = aes(x = Ave_Wind_HDG, y = ..density..), binwidth = 15)+
  facet_wrap(~Landing_Runway)
