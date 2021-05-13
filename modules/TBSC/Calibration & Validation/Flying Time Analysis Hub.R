# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Flying Time Analysis Hub
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v3.0
#
# Authors: George Clark
#
# Description: Script to Analyse Speeds/Flying Times for SASAI Adaptation Calibration.
#
# Use:
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# v3.0:
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# -------------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------------- #
# REQUIRED: Database/Initial Setup
# -------------------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
ModuleFolder <- "TBSC"
ModuleSubfolder <- "Calibration & Validation"
# OutputFolder <- file.path("Flying_Time_Analysis_Output", "New Format")
OutputFolder <- file.path("Flying Time Analysis")

# --------------------------------------------------------------------------- #

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
# source(file.path(Global_Dir, "Imports.R"), local = F)
# source(file.path(Global_Dir, "Global Functions.R"), local = F)
# source(file.path(Global_Dir, "Global Parameters.R"), local = F)

Base_Dir <- GetSaveDirectory(Algorithm = OutputFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

# -------------------------------------------------------------------------------------------------- #
# REQUIRED: All Adaptation Parameters
# -------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------- #
# Version Control
# ------------------------------------------------------------- #

# Script Version
Script_Version <- "3.0"

# Operation to Run (IA/IA PWS)
Operation <- "IA" # Want this to be in DB

# Data/Output Versions (Move Adap to DB?)
Adap_Version <-  "7.0"
Adap_Iteration <- "1"
Local_Iteration <- "3"

# Version Comparisons
Do_Version_Comparison <- T
Compare_Airfield <- "CYYZ"
Compare_Version <- "7.0-1-1"

# Database Choice
IP <- c("192.168.1.23", "192.168.1.39")[1]
Database <- c("NavCan_TBS_V2", "NavCan_TBS_V3", "NavCan_UTMA_Validation_DB2")[2]

# ------------------------------------------------------------- #
# Table Names
# ------------------------------------------------------------- #

# Reference Data
ACTP_Wake_Dist_Name <- "tbl_Reference_ACTP_Wake_Separation_Dist"
AC_Operator_Wake_Dist_Name <- "tbl_Reference_AC_Operator_Wake_Separation_Dist"
Legacy_Wake_Dist_Name <- "tbl_DBS_Wake_Turbulence"
Recat_Wake_Dist_Name <- "tbl_Reference_Recat_Separation_Dist"
Legacy_AC_To_Wake_Name <- "tbl_Aircraft_Type_To_Wake_Legacy"
Runway_Name <- "tbl_Runway"

# Input Data
FTA_View_Name <- "vw_Flying_Time_Analysis"
Recat_ROT_Dist_Name <- "tbl_Reference_ROT_Spacing_Dist"

# ------------------------------------------------------------- #
# Filters & Processing Control
# ------------------------------------------------------------- #

# Local Input Data Parameters
FTA_View_Local <- F
Recat_ROT_Dist_Local <- F
FTA_View_Version_Alt <- "7.0-0"
Recat_ROT_Dist_Version_Alt <- "5.0-0"

# Speed Type
Speed_Type <- c("IAS", "GSPD", "TAS", "WE", "TRACK", "DERIVED TRACK")[1]

# Distances !!NOTE: Should Automate Displacement/Half Distances for PWS.
Delivery_Primary <- 0
Delivery_Secondary <- 1

# Filter Values - IAS/GSPD
Use_Filter_IAS <- T
Use_Filter_GSPD <- T
IAS_Min <- 80
IAS_Max <- 220
GSPD_Min <- 60
GSPD_Max <- 250

# Filter Values - Sep Accuracy
MRS <- 3
Legacy_Delivery <- 0 #Controls what Separation Accuracy to use
Use_Filter_Separation_Accuracy <- F
Sep_Accuracy_Max <- 3
Sep_Accuracy_Alt_Max <- 20
Sep_Accuracy_Max_Tight <- 20
Sep_Accuracy_Alt_Max_Tight <- 20
Sep_Accuracy_Max_Used <- "Standard" # Either "Standard" or "Tight"

# Surface Wind/Headwind Groups
Ref_Wind_SHW_Max <- 5
Ref_Wind_SWS_Max <- 3
SHW_Groups <- c(-Inf, -15, -Ref_Wind_SHW_Max, Ref_Wind_SHW_Max, 10, 20, 30, Inf)
SWS_Groups <- c(0, Ref_Wind_SWS_Max, 6, 10, 20, 30, Inf)

# Misc Filter Switches
Remove_Not_In_Trail <- T
Remove_Global_Flags <- T

# ------------------------------------------------------------- #
# Adaptation Output & Weightings
# ------------------------------------------------------------- #

# Output Control
ORD_Profile_TBS_Calcs <- F
Output_Adaptation <- T
Safety_Case <- 2
Use_Secondary_Adaptation <- F

# Weighting (Recat Wake/ROT)
Use_Weighted_Wake <- T
Weighting_Local <- T
Weighting_Version_Alt <- "6.0-0-0"
Weighting_Method <- 2
Min_Weighting_Obs <- 0
Weighted_Types <- c("B", "C", "D", "E", "F", "G")

# Weighting (TBS Table)
Counts_Local <- T
Counts_Version_Alt <- "6.0-0-0"

# ------------------------------------------------------------- #
# PWS ONLY
# ------------------------------------------------------------- #

# PWS Exclusive:
Operator_Enabled <- F
Obs_Removal_At_Level <- T
Min_Obs_Operator <- 10
Min_Obs_Aircraft <- 10

# ACTP ROT Distances
Aircraft_ROT_Dist_Name <- "tbl_Reference_ACTP_ROT_Spacing_Dist"
Aircraft_ROT_Dist_Local <- F
Aircraft_ROT_Dist_Version_Alt <- "5.0-0"
Aircraft_ROT_Dist_Query <- paste0("SELECT * FROM ", Aircraft_ROT_Dist_Name)

# AC Operator ROT Distances
Operator_ROT_Dist_Name <- "tbl_Reference_AC_Operator_ROT_Spacing_Dist"
Operator_ROT_Dist_Local <- F
Operator_ROT_Dist_Version_Alt <- "5.0-0"
Operator_ROT_Dist_Query <- paste0("SELECT * FROM ", Operator_ROT_Dist_Name)

# -------------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------------- #
# REQUIRED: Resource Gathering & Directory Management
# -------------------------------------------------------------------------------------------------- #

# Algorithm Functions
source(file.path(Algo_Func_Dir, "TBSC Functions.R"), local = T)

# Get Connection
con <- Get_RODBC_Database_Connection(IP, Database)
Airfield <- as.character(Load_Adaptation_Table(con, "tbl_Airfield")$Airfield_Name)

# Full Version
Adap_Iteration_Version <- paste0(Adap_Version, "-", Adap_Iteration)
Local_Iteration_Version <- paste0(Adap_Iteration_Version, "-", Local_Iteration)

# -------------------------------------------------------------------------------------------------- #
# Create Directory Network
source(GetScriptPath(Script_Dir, Airfield, "Flying Time Analysis Setup.R"), local = T)

Compare_Airfield_Dir <- Airfield_Dir
# -------------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------------- #
# Data Loading
# -------------------------------------------------------------------------------------------------- #

# Data Individual Versions
FTA_View_Version <- ifelse(FTA_View_Local, FTA_View_Version_Alt, Adap_Iteration_Version)
Recat_ROT_Dist_Version <- ifelse(Recat_ROT_Dist_Local, Recat_ROT_Dist_Version_Alt, Adap_Iteration_Version)
Aircraft_ROT_Dist_Version <- ifelse(Aircraft_ROT_Dist_Local, Aircraft_ROT_Dist_Version_Alt, Adap_Iteration_Version)
Operator_ROT_Dist_Version <- ifelse(Operator_ROT_Dist_Local, Operator_ROT_Dist_Version_Alt, Adap_Iteration_Version)

## Reference Data (Need to see if Operator Wake Distance would change)
Recat_Wake_Dist <- Load_Reference_Data(con, Recat_Wake_Dist_Name, Airfield_Dir, Adap_Version)
Legacy_Wake_Dist <- Load_Reference_Data(con, Legacy_Wake_Dist_Name, Airfield_Dir, Adap_Version)
Legacy_AC_To_Wake <- Load_Reference_Data(con, Legacy_AC_To_Wake_Name, Airfield_Dir, Adap_Version) %>% unique()
Runway <- Load_Reference_Data(con, Runway_Name, Airfield_Dir, Adap_Version)
Aircraft_Wake_Dist <- ifelse(Operation == "IA PWS", Load_Reference_Data(con, Aircraft_Wake_Dist_Name, Airfield_Dir, Adap_Version), NA)
Operator_Wake_Dist <- ifelse(Operation == "IA PWS" & Operator_Enabled, Load_Reference_Data(con, Operator_Wake_Dist_Name, Airfield_Dir, Adap_Version), NA)

# Calibrated Adaptation
Recat_ROT_Dist <- Load_DB_Input_Data(con, Recat_ROT_Dist_Name, Airfield_Dir, Adap_Iteration_Version, Recat_ROT_Dist_Version)

Aircraft_ROT_Dist <- ifelse(Operation == "IA PWS",
                            Load_DB_Input_Data(con, Aircraft_ROT_Dist_Name, Airfield_Dir, Adap_Iteration_Version, Aircraft_ROT_Dist_Version),
                            NA)

Operator_ROT_Dist <- ifelse(Operation == "IA PWS" & Operator_Enabled,
                            Load_DB_Input_Data(con, Operator_ROT_Dist_Name, Airfield_Dir, Adap_Iteration_Version, Operator_ROT_Dist_Version),
                            NA)

# Data
FTA_View <- Load_DB_Input_Data(con, FTA_View_Name, Airfield_Dir, Adap_Iteration_Version, FTA_View_Version)

# -------------------------------------------------------------------------------------------------- #
# Speed/Time Processing/Pre-Processing
# -------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------- #
# Run Processing
source(GetScriptPath(Script_Dir, Airfield, "Flying Time Analysis Processing.R"), local = T)
# -------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------- #
## Run Adaptation Generation/Comparison
source(GetScriptPath(Script_Dir, Airfield, "Flying Time Analysis Adaptation.R"), local = T)
# -------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------- #
## Run Analysis Scripts
source(GetScriptPath(Script_Dir, Airfield, "Flying Time Analysis.R"), local = T)
# -------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------------------------- #
