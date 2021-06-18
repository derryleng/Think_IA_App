# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Performance Modelling Hub
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v1.0
#
# Authors: George Clark
#
# Description: Script to Model Overall IA Performance.
#
# Use:
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# v1.0:
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
ModuleFolder <- "Performance Modelling"
ModuleSubfolder <- "Calibration & Validation"
# OutputFolder <- "Performance_Model_Output"
OutputFolder <- "Performance Modelling"
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

Project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3", noblank = FALSE, forcemask = FALSE))
Base_Dir <- GetSaveDirectory(Project, Algorithm = OutputFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

# -------------------------------------------------------------------------------------------------- #
# REQUIRED: All Adaptation Parameters
# -------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------- #
# Version Control
# ------------------------------------------------------------- #

# Script Version
Script_Version <- "1.0"

# Operation to Run (IA/IA PWS)
Operation <- "IA" # Want this to be in DB

# Data/Output Versions (Move Adap to DB?)
Output_Version <- "1.5"
Input_Collection <- "PM 15-06-21"
GWCS_Input_Version <- "2021-05-04 V1.0 (AH)"

# Database Choice
IP <- c("192.168.1.23", "192.168.1.39")[1]
Database <- c("NavCan_TBS_V2", "NavCan_TBS_V3", "NavCan_UTMA_Validation_DB2")[2]

## COMING SOON: COMPARISON FUNCTIONALITY

# ---------------------------------------------------- #
# Airport Config
# ---------------------------------------------------- #

# Minimum Radar Separation Assumption (Legacy Separation)
Min_Radar_Sep <- 3

# Parameter "Caps" for Recat
Min_Wake_Delivery <- 3
Min_Wake_FAF <- 3
Min_ROT_Delivery <- 3
Min_ROT_FAF  <- 3

# ---------------------------------------------------- #
# Filters
# ---------------------------------------------------- #

Use_Filter_Recat_Wake <- F
Use_Filter_Go_Arounds <- T
Use_Filter_Average_IAS <- T

Filter_Average_IAS_Max_DME <- 4.5
Filter_Average_IAS_Max_SPD <- 200
Filter_Average_IAS_Min_SPD <- 80

# ---------------------------------------------------- #
# Sample Bolstering
# ---------------------------------------------------- #

# The RECAT Wake Scheme for Current Airport
Wake_Scheme <- "ICAO7"

# Do we need to Bolster Samples for Specific RECAT Wake Categories?
Bolster_Main <- T
Bolster_Main_Type <- "Manual"
Bolster_Main_Manual_Leaders <- c("A")
Bolster_Main_Manual_Followers <- c("F")
Bolster_Main_Automatic_Min_Obs <- 50
Bolster_Main_Automatic_Min_Obs <- 1000

# Do we need to Bolster samples for specific ROT Pairs/Distances?
Bolster_ROT <- F

# ---------------------------------------------------- #
# Time Separation Calcs Configuration
# ---------------------------------------------------- #

# Config Parameters for Time Spacing Calculations (NAV)
# Separation_Type <- c("Perfect", "Perfect", "Perfect", "Perfect", "Perfect", "Perfect", "Perfect", "Perfect")
# Separation_Delivery <- c(1, 1, 1, 1, 0, 0, 0, 0)
# Separation_Distance <- c("Recat_eTBS_0DME_Wake_Separation_Distance", "Recat_eTBS_0DME_Wake_Separation_Distance", "Ref_Recat_Wake_Separation_Distance", "Ref_Recat_Wake_Separation_Distance",
#                          "Recat_eTBS_0DME_Wake_Separation_Distance", "Recat_eTBS_0DME_Wake_Separation_Distance", "Ref_Recat_Wake_Separation_Distance", "Ref_Recat_Wake_Separation_Distance")
# Separation_Time <- c("Perfect_1DME_Wake_Separation_Time_TBS", "Perfect_1DME_Wake_Separation_Time_TBS_US05", "Perfect_1DME_Wake_Separation_Time_DBS", "Perfect_1DME_Wake_Separation_Time_DBS_US05",
#                      "Perfect_0DME_Wake_Separation_Time_TBS", "Perfect_0DME_Wake_Separation_Time_TBS_US05", "Perfect_0DME_Wake_Separation_Time_DBS", "Perfect_0DME_Wake_Separation_Time_DBS_US05")
# Separation_Under <- c(0, 0.5, 0, 0.5, 0, 0.5, 0, 0.5)
Separation_Type <- c("Perfect", "Perfect", "Perfect", "Perfect")
Separation_Delivery <- c(1, 1, 1, 1)
Separation_Distance <- c("Recat_eTBS_0DME_Wake_Separation_Distance", "Recat_eTBS_0DME_Wake_Separation_Distance", "Ref_Recat_Wake_Separation_Distance", "Ref_Recat_Wake_Separation_Distance")
Separation_Time <- c("Perfect_1DME_Wake_Separation_Time_TBS", "Perfect_1DME_Wake_Separation_Time_TBS_US05", "Perfect_1DME_Wake_Separation_Time_DBS", "Perfect_1DME_Wake_Separation_Time_DBS_US05")
Separation_Under <- c(0, 0.5, 0, 0.5)

# Grouping Variable type (Use "Landing_Pair_ID" if both outputs from DB) TODO: Automatically Determine Based on File Versions
Grouping_Type <- "Landing_Pair_ID"

# Leader Start Variable (Only Required for Actual Separations)
Leader_Start_Var <- "Leader_Local_Stabilisation_Threshold"

# Are all segments required within a distance range?
All_Segs_Required <- T

# Max DME Gap between succesive Segments (Only used if All_Segs_Required = T)
Use_Max_DME_Gap <- T
Max_DME_Gap <- 2

# --------------------------------------------------------------------------------- #
# Connection / Directory Config
# --------------------------------------------------------------------------------- #

# Database Choice
IP <- c("192.168.1.23", "192.168.1.39")[1]
Database <- c("NavCan_TBS_V2", "NavCan_TBS_V3", "NavCan_UTMA_Validation_DB2")[2]

# Get Directories
Out_Dir <- file.path(Base_Dir, paste0("v", Output_Version))
Inp_Dir <- file.path(Base_Dir, "..", "..", "Inputs", "Performance_Model_Input", paste0(Input_Collection))

# Make Directories if Necessary
Create_Directory(Out_Dir)
Create_Directory(Inp_Dir)

# Algorithm Functions
source(file.path(Algo_Func_Dir, "PM Functions.R"), local = T)

# -------------------------------------------------------------------------------------------------- #
# REQUIRED: Data Loading
# -------------------------------------------------------------------------------------------------- #

# Get Connection
con <- Get_DBI_Connection(IP, Database)
Airfield <- as.character(Load_Adaptation_Table(con, "tbl_Airfield")$Airfield_Name)

# --------------------------------------------------------------------------------- #
# Data Loading
# --------------------------------------------------------------------------------- #

Segment_Query <- paste0(
  "SELECT
  Landing_Pair_ID,
  Landing_Pair_Type,
  Landing_Pair_Date AS FP_Date,
  DME_Seg = LWS.DME_Seg / dbo.fnc_GI_Nm_To_M() ,
  Leader_Flight_Plan_ID,
  Leader_Wind_Segment_ID = LWS.Mode_S_Wind_Seg_ID,
  Leader_Average_GSPD = LWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec(),
  Leader_Average_IAS = LWS.Ave_Mode_S_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec(),
  Follower_Flight_Plan_ID,
  Follower_Average_GSPD = FWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec(),
  Follower_Average_IAS = FWS.Ave_Mode_S_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec()
FROM tbl_Landing_Pair LP

INNER JOIN tbl_Mode_S_Wind_Seg LWS
ON LP.Leader_Flight_Plan_ID = LWS.Flight_Plan_ID

INNER JOIN tbl_Mode_S_Wind_Seg FWS
ON LP.Follower_Flight_Plan_ID = FWS.Flight_Plan_ID AND LWS.DME_Seg = FWS.DME_Seg

ORDER BY Landing_Pair_ID, DME_Seg")

# Load Main Data Sources.

## Performance Model View.
if (!file.exists(file.path(Inp_Dir, "Performance Model View.csv"))){
  Performance_Model <- dbGetQuery(con, "SELECT * FROM vw_eTBS_Performance_Model", stringsAsFactors = F)
  fwrite(Performance_Model, file.path(Inp_Dir, "Performance Model View.csv"))
} else {
  Performance_Model <- fread(file.path(file.path(Inp_Dir, "Performance Model View.csv")))
}

## Segments Data.
if (!file.exists(file.path(Inp_Dir, "Performance Model Segment View.csv"))){
  Segments <- dbGetQuery(con, Segment_Query, stringsAsFactors = F)
  fwrite(Segments, file.path(Inp_Dir, "Performance Model Segment View.csv"))
} else {
  Segments <- fread(file.path(file.path(Inp_Dir, "Performance Model Segment View.csv")))
}

## GWCS Data.
# Load in the 6 GWCS data sets
SepDists <- GetGWCSSepDistances(Airfield)
for (i in 1:length(SepDists)){
  GWCS_Dist <- GetModeSWindForecastFile(Project, GWCS_Input_Version, SepDists[i]) %>%
    mutate(Sep_Dist = SepDists[i])
  if (i == 1){
    GWCS_Data <- GWCS_Dist
  } else {
    GWCS_Data <- rbind(GWCS_Data, GWCS_Dist)
  }
  rm(GWCS_Dist)
}

# QC Flag for GWCS
GWCS_Data <- mutate(GWCS_Data, QC_Flag = 0)

# Select relevant fields.
GWCS_Data <- select(GWCS_Data, FP_Date, Time_At_4DME, Callsign, Forecast_Wind_Effect_IAS, Sep_Dist)
LP_Times <- dbGetQuery(con, "SELECT lp.Landing_Pair_ID, lp.Leader_Flight_Plan_ID, lp.Follower_Flight_Plan_ID, FPDL.Time_At_4DME AS Leader_Time_At_4DME,
              FPDF.Time_At_4DME AS Follower_Time_At_4DME FROM tbl_Landing_Pair lp 
              LEFT JOIN tbl_Flight_Plan_Derived FPDL 
              ON lp.Leader_Flight_Plan_ID = FPDL.Flight_Plan_ID
              LEFT JOIN tbl_Flight_Plan_Derived FPDF
              ON lp.Follower_Flight_Plan_ID = FPDF.Flight_Plan_ID")
Performance_Model <- left_join(Performance_Model, LP_Times, by = c("Landing_Pair_ID"))
rm(LP_Times)

# gwcs_data <- mutate(gwcs_data,  Sep_Dist = Forecast_Seg_Max - Forecast_Seg_Min + 1, 
#                     qc_flag = ifelse(Observed_Max_RTT >= (Forecast_Seg_Min + Sep_Dist - 2) & Observed_Flying_Time >= (Sep_Dist * 20 - 40) & abs(Observed_Ave_Mode_S_GSPD - Observed_Track_GSPD) <= 20, 1, 0))

# 

# Load Adaptation Sources. TODO: Enable Local Files.
GWCS_Adaptation <- dbGetQuery(con, "SELECT * FROM tbl_Mode_S_Wind_Adaptation", stringsAsFactors = F)
Recat_Wake_Dist <- dbGetQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Dist", stringsAsFactors = F) %>%
  mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852)
Recat_Wake_Speed <- dbGetQuery(con, "SELECT * FROM tbl_Assumed_Recat_Separation_IAS", stringsAsFactors = F) %>%
  mutate(Assumed_Wake_Separation_IAS = Assumed_Wake_Separation_IAS / (1852/3600))
Recat_Wake_Time <- dbGetQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Time", stringsAsFactors = F)
Legacy_Wake_Dist <- dbGetQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence", stringsAsFactors = F) %>%
  mutate(WT_Separation_Distance = WT_Separation_Distance / 1852)
Recat_AC_To_Wake <- dbGetQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake", stringsAsFactors = F)
Legacy_AC_To_Wake <- dbGetQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy", stringsAsFactors = F) %>% unique()
Recat_ROT_Dist <- dbGetQuery(con, "SELECT * FROM tbl_Reference_ROT_Spacing_Dist", stringsAsFactors = F) %>%
  mutate(Reference_ROT_Spacing_Distance = Reference_ROT_Spacing_Distance / 1852)
Recat_ROT_Time <- dbGetQuery(con, "SELECT * FROM tbl_Reference_ROT_Spacing_Time", stringsAsFactors = F)
Recat_ROT_Speed <- dbGetQuery(con, "SELECT * FROM tbl_Assumed_ROT_Spacing_IAS", stringsAsFactors = F) %>%
  mutate(Assumed_ROT_Spacing_IAS = Assumed_ROT_Spacing_IAS / (1852/3600))

# --------------------------------------------------------------------------------- #
# Initial Setup
# --------------------------------------------------------------------------------- #

# Default Segment Size Parameter (Should get from DB)
Seg_Size <- as.numeric(GWCS_Adaptation$DME_Seg_Size/1852)

# Set Original Values
Performance_Model_Original <- Performance_Model
Segments_Original <- Segments

# Join on RECAT Wake/ROT Adaptation for Complete Table.
Recat_Wake <- Recat_Wake_Dist %>%
  left_join(Recat_Wake_Time, by = c("Leader_WTC", "Follower_WTC")) %>%
  left_join(Recat_Wake_Speed, by = c("Leader_WTC", "Follower_WTC"))

Recat_ROT <- Recat_ROT_Dist %>%
  left_join(Recat_ROT_Time, by = c("Runway", "Leader_WTC", "Follower_WTC")) %>%
  left_join(Recat_ROT_Speed, by = c("Runway", "Leader_WTC", "Follower_WTC"))

# Optional: Combine Two Together to get full distance set,

# Local Adaptation #######################################################################################

# ---------------------------------------------------- #
# Adaptation Table Construction
# ---------------------------------------------------- #

# Create Local Adaptation Components
Local_Variables <- c()
Local_Values <- c()

# Create Local Adaptation Table
Local_Adaptation <- data.frame(
  Variable = Local_Variables,
  Value = Local_Values
)

# Store Local Adaptation Table
# fwrite(Local_Adaptation, file.path(Out_Dir, "PM Local Adaptation.csv"))

# Pre-Processing #########################################################################################

# Revert to Original Values if re-running
Performance_Model <- Performance_Model_Original
Segments <- Segments_Original

# Run Pre-Processing.
source(GetScriptPath(Script_Dir, Airfield, "IA Performance Model Pre-Processing.R"), local = T)

# Allocate Pre-Processed Version
Performance_Model_PP <- Performance_Model

# Processing #############################################################################################

# Revert to Original Values if re-running
Performance_Model <- Performance_Model_PP
Segments <- Segments_Original

# Run Processing.
source(GetScriptPath(Script_Dir, Airfield, "IA Performance Model Processing.R"), local = T)

##########################################################################################################
