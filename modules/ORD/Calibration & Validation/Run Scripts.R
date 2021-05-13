# ----------------------------------------------------------------------- #
# 0. Project Configuration ------------------------------------------------
# ----------------------------------------------------------------------- #
# Run this section first before running any other sections! ------------- #
# ----------------------------------------------------------------------- #

rm(list = ls())

library(RODBC)
library(MASS)
library(BMS)
library(zoo)
library(pastecs)
library(plyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(RColorBrewer)
library(zip)
library(xlsx) # Requires JRE
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
# Script_out <- "Performance Analysis"
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

con <- Get_RODBC_Database_Connection(IP = ip, Database = database)

# ----------------------------------------------------------------------- #
# 1. Approach Speed Profiling ---------------------------------------------
# ----------------------------------------------------------------------- #

# Name of output folder in project directory ---------------------------- #
outdir_approach_speed_profiling <- paste0("Speed Profiles/", version)
# outdir_approach_speed_profiling <- paste0("Speed Profiles/", "2021-05-04")

# Reference LSS type file ----------------------------------------------- #
ref_lss_type_table <- "01a ORD Configuration Output Type v1.8.csv"

# Reference wake lookup file -------------------------------------------- #
ref_wake_aircraft_table <- "UK and RECAT WTC Lookup.csv"

# Reference default wake adaptation file -------------------------------- #
ref_wake_adaptation <- "01c ORD Configuration Output Wake v1.4.csv"

# WTC type -------------------------------------------------------------- #
# Options:
#   "REF_DATA"
#   "DATABASE"
wake_type <- "DATABASE"

# Speed type ------------------------------------------------------------ #
# Options:
#   "Mode_S_IAS" - preferred speed type
#   "Track_Speed" - use only when Mode S IAS not available
#   "Calculated_Speed" - use only when above options unsuitable
speed_type <- "Mode_S_IAS"

# Airport altitude (ft asl) --------------------------------------------- #
# NB: Used when speed_type != "Mode_S_IAS"
airport_alt <- 550

# Set speed filtering parameters ---------------------------------------- #
# NB: Used when speed_type == "Calculated_Speed"
# For each flight, filters out calculated speeds which are not within
#   [max(min(Track_Speed)*(speed_filter_perc/100), speed_filter_limit_low),
#    min(max(Track_Speed)*(1+speed_filter_perc/100), speed_filter_limit_high)]
speed_filter_perc <- 50
speed_filter_limit_low <- 50
speed_filter_limit_high <- 200

# Start day number ------------------------------------------------------ #
# Set to 1 to run script for all days
start_day_num <- 1

# Run Approach Speed Profiling ------------------------------------------ #
source(file.path(Script_Dir, "Approach Speed Profiling.R"), local = F)




# ----------------------------------------------------------------------- #
# 2. Parameter Summary ----------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of speed profile folder to use ----------------------------------- #
# Using Date generated version number
speed_profile_folder <- paste0("Speed Profiles/", input_version)
#Alternatively Manually Specify
# speed_profile_folder <- paste0("Speed Profiles/", "2021-05-04")

# Name of output folder in project directory ---------------------------- #
outdir_parameter_summary <- paste0("Adaptation/", version)

# Reference LSS type file ----------------------------------------------- #
ref_lss_type_table <- "01a ORD Configuration Output Type v1.8.csv"

# Wake Pairings (from GWCS sample weighting)----------------------------- #
Wake_Pair_Counts_input <- "Wake Pair Counts.csv"

# Generate new validation list? (Defaults to TRUE if list not found) ---- #
validation_generation <- F

# Probability of dates being reserved for validation -------------------- #
# validation_threshold <- 0.20
validation_threshold <- 0

# Setting a threshold for number of observations to be modeled --------- #
observation_threshold <- 5
empirical_threshold <- 30

# Parameter filter settings (*_filter variable must be set to TRUE) ----- #
a1_filter <- T
a1_min <- 75
a1_max <- 220

a2_filter <- T
a2_min <- 75
a2_max <- 220

b_filter <- F
b_min <- 100
b_max <- 180

n1_filter <- T
n1_min <- 0
n1_max <- 12

n2_filter <- T
n2_min <- 0
n2_max <- 14

d_filter <- F
d_min <- 0
d_max <- 50

use_vref_for_decel <- T
min_vref <- 10
max_vref <- 10


type_adaptation_input_table <- data.table(
  Aircraft_Type = "ALL",
  Compression_Commencement_Threshold = 10,
  Landing_Stabilisation_Speed_Type_Lead = NA,
  Landing_Stabilisation_Speed_Type_Follower = NA,
  Min_Safe_Landing_Speed_Lead = NA,
  Min_Safe_Landing_Speed_Follower = NA,
  Apply_Gusting_Lead = 1,
  Apply_Gusting_Follower = 1,
  Local_Stabilisation_Distance_Lead = 4.5,
  Local_Stabilisation_Distance_Follower = 4.5,
  Steady_Procedural_Speed_Lead = 170,
  Steady_Procedural_Speed_Follower = 172,
  Final_Deceleration_Lead = NA,
  Final_Deceleration_Follower = NA,
  End_Initial_Deceleration_Distance_Lead = 12,
  End_Initial_Deceleration_Distance_Follower = 12,
  Initial_Procedural_Speed_Lead = 170,
  Initial_Procedural_Speed_Follower = 172,
  Initial_Deceleration_Lead = 10,
  Initial_Deceleration_Follower = 10
)

wake_adaptation_input_table <- data.table(
  Wake_Cat = "ALL",
  Compression_Commencement_Threshold = 10,
  Landing_Stabilisation_Speed_Type_Lead = 0,
  Landing_Stabilisation_Speed_Type_Follower = 0,
  Min_Safe_Landing_Speed_Lead = NA,
  Min_Safe_Landing_Speed_Follower = NA,
  Apply_Gusting_Lead = 1,
  Apply_Gusting_Follower = 1,
  Local_Stabilisation_Distance_Lead = 4.5,
  Local_Stabilisation_Distance_Follower = 4.5,
  Steady_Procedural_Speed_Lead = 170,
  Steady_Procedural_Speed_Follower = 172,
  Final_Deceleration_Lead = NA,
  Final_Deceleration_Follower = NA,
  End_Initial_Deceleration_Distance_Lead = 12,
  End_Initial_Deceleration_Distance_Follower = 12,
  Initial_Procedural_Speed_Lead = 170,
  Initial_Procedural_Speed_Follower = 172,
  Initial_Deceleration_Lead = 10,
  Initial_Deceleration_Follower = 10
)


dbs_adaptation_input_table <- data.table(
  DBS_Distance = "ALL",
  Compression_Commencement_Threshold = 10,
  Landing_Stabilisation_Speed_Type_Lead = 0,
  Landing_Stabilisation_Speed_Type_Follower = 0,
  Min_Safe_Landing_Speed_Lead = NA,
  Min_Safe_Landing_Speed_Follower = NA,
  Apply_Gusting_Lead = 1,
  Apply_Gusting_Follower = 1,
  Local_Stabilisation_Distance_Lead = 4.5,
  Local_Stabilisation_Distance_Follower = 4.5,
  Steady_Procedural_Speed_Lead = 170,
  Steady_Procedural_Speed_Follower = 172,
  Final_Deceleration_Lead = NA,
  Final_Deceleration_Follower = NA,
  End_Initial_Deceleration_Distance_Lead = 12,
  End_Initial_Deceleration_Distance_Follower = 12,
  Initial_Procedural_Speed_Lead = 170,
  Initial_Procedural_Speed_Follower = 172,
  Initial_Deceleration_Lead = 10,
  Initial_Deceleration_Follower = 10
)

additional_aircraft_to_output <- c("A388", "A339")

thousand_ft_gate <- 5321.9/1852
reference_wind <- 5
max_decel <- 50

# Use Wake Separation Weighted Average for TBS Table Output
use_weighted_average <- T

# Parameter to use additional time base Vref/decel adjustmenet (To account for
# 1000ft gate limitation)

use_Vref_Adjust <- T

# Run Parameter Summary ------------------------------------------------- #
# source("Parameter Summary.R", local = T)
source(file.path(Script_Dir, "Parameter Summary.R"), local = F)




# ----------------------------------------------------------------------- #
# Vref Comparison ---------------------------------------------------------
# ----------------------------------------------------------------------- #

# Directory of Populate_tbl_ORD_Aircraft_Adaptation_*.csv --------------- #
adaptation_folder <- paste0("Adaptation/", version)
# adaptation_folder <- "Adaptation 24_02_21 Andy 5 THRESH"

# adaptation_file <- "Populate_tbl_ORD_Aircraft_Adaptation_EHAM New Config.csv"
adaptation_file <- "Populate_tbl_ORD_Aircraft_Adaptation_CYYZ.csv"

# Code for comparison airport - set to EGLL or EHAM
Comparison_Airport_Code <- "EGLL"

# Run Vref Comparison --------------------------------------------------- #
source(file.path(Script_Dir, "Vref Comparison.R"), local = F)




# ----------------------------------------------------------------------- #
# 3. Validation Analysis --------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of output folder in project directory ---------------------------- #
outdir_validation_analysis <- paste0("Validation/", version)
# outdir_validation_analysis <- "Validation 26-04-21 AH TEST"

# Validation View source ------------------------------------------------ #
# File path to backed up ORD_Validation_View.csv or set as NA for database
validation_view_source <- NA
# validation_view_source <- file.path(Project_Directory, outdir_validation_analysis, "ORD_Validation_View.csv")

# eTBS Performance Model View source ------------------------------------ #
# File path to backed up eTBS_Performance_View.csv or set as NA for database
performance_model_source <- NA
# performance_model_source <- file.path(Project_Directory, outdir_validation_analysis, "eTBS_Performance_View.csv")

# Reference ICAO 4 aircraft wake lookup --------------------------------- #
# ref_aircraft_wake_icao4 <- "reference_wake_category_icao.csv"

# Reference ICAO 4 wake separation table -------------------------------- #
# ref_ref_wake_icao4 <- "reference_wake_separation_dist_icao.csv"

# Performance Measure --------------------------------------------------- #
operational_hour_multiplier_wake <- 3.6
operational_hour_multiplier_all <- 21.1

# Compression Metric ---------------------------------------------------- #
# Choose "New" for the updated Compression calcs, anything else for DB values
ORD_Compression_Type <- "New"

# Use Validation_Date_List.csv to subset data? -------------------------- #
val <- F
# Validation_Date_List.csv directory (Parameter Summary folder)
# or specify file path inside project folder
# or specify full file path
# valset_folder <- "v3.0"
# valset_folder <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Reference Data\\DB2 New Dates.csv"
# valset_folder <- "C:\\Users\\George Clark\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Reference Data\\DB2 New Dates.csv"
# valset_folder <- "C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD\\Adaptation - Sprint 13 - IA-187 v2\\Validation_Date_List.csv"


# T/F Delivery to ROT Indicator/Wake Indicator (tighter) ---------------- #
delivery_to_rot <- T

# Actual behaviour of the leader and follower --------------------------- #
obs_lead_ias_min <- 80
obs_lead_ias_max <- 180
obs_follow_ias_min <- 100
obs_follow_ias_max <- 250
obs_follow_ias_max_tight <- 200

# ORD behaviours (to exclude current issues with algorithm) ------------- #
ord_lead_ias_min <- 80
ord_lead_ias_max <- 180
ord_follow_ias_min <- 100
ord_follow_ias_max <- 200

# Separation Accuracy: Entire Dataset ----------------------------------- #
sep_accuracy_max <- 3
sep_accuracy_max_a380 <- 6

# Separation Accuracy: Tight Dataset ------------------------------------ #
sep_accuracy_max_tight <- 1
sep_accuracy_max_a380_tight <- 4

# Aircraft types to report performance metrics -------------------------- #
# NB: Leave blank to use all aircraft types in validation view
report_performance_actypes <- c(

)

# MC 26/02
# Flag on whether to calculate the separation adjustment
separation_adjustment <- F

# Adjustments for Separation Accuracy ----------------------------------- #
# MC Add 16/9.  Separation Adjustment Parameters for modelled accuracy
#sep_adjust_file <- "separation_adjustment_parameters_v2.csv"
#egll_mean <- 0.55
#egll_sd <- 0.46
#pc_under <- 0.03
# End Add


# Run Validation Analysis ----------------------------------------------- #
source(file.path(Script_Dir, "Validation Analysis.R"), local = F)
