# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Run ORD (Validation)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0A (Alpha)
#
# Authors: George Clark
#
# Description: A hub that runs all the necessary scripts for ORD. Assumes Landing Pair generation is moved to pre-processing.
#
#
# Use Guide Section:
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# v0A: First Integration within the Think IA App Git Environment.
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
#library(float)
# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Database Processing"
# --------------------------------------------------------------------------- #
#
FileFlag <- "global.R"
ResourcesFolder <- "resources"
AlgoResourcesFolder <- "algorithm_functions"
ModulesFolder <- "modules"

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
DB_Module_Dir <- file.path(Script_Dir, "Database Modules")

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Source Global Configuration Files (Requires Shiny Integration)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path(Global_Dir, "imports.R"), local = T)
source(file.path(Global_Dir, "unit conversions.R"), local = T)
source(file.path(Global_Dir, "functions.R"), local = T)

# Start Time
Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Source Function Files
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path(Algo_Func_Dir, "ORD Functions (DB).R"), local = T) # ORD Functions
source(file.path(Algo_Func_Dir, "TBSC Functions (PWS).R"), local = T)
#source(file.path(Algo_Func_Dir, "GWCS Functions (DB).R"), local = T) # GWCS Functions for Wind Forecasting
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path(DB_Module_Dir, "All Pair Reference Data.R"), local = T) # All Pair Reference Data Functions
source(file.path(DB_Module_Dir, "ORD-WAD Observation.R"), local = T) # ORD Observation Functions
source(file.path(DB_Module_Dir, "ORD Aircraft Profile.R"), local= T) # ORD Aircraft Profile Functions
source(file.path(DB_Module_Dir, "ORD IAS-GSPD Profile.R"), local= T) # ORD Segment/IAS/GS Profile Functions
source(file.path(DB_Module_Dir, "ORD-WAD Prediction.R"), local= T) # ORD Prediction Functions
source(file.path(DB_Module_Dir, "Setup IA Performance Model.R"), local= T) # Setup Performance Model Functions
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Configuration (## TODO: SETUP FOR SHINY)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Database <- "PWS_Prototyping"
IP <- "192.168.1.23"
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Testing <- T
PROC <- 1
PROC_Period <- c("Day", "Month", "All")[PROC]
PROC_Criteria <- c("01/06/2019", "06/2019", NA)[PROC]
# ----------------------------------------------------------------------------------------------------------------------------------------- #
con <- Get_DBI_Connection(IP, Database)
LP_Primary_Key <- Get_LP_Primary_Key("Validation")
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Load Common Adaptation
# ----------------------------------------------------------------------------------------------------------------------------------------- #
ADAP_Config <- Load_Adaptation_Table(con, "tbl_Adaptation_Data")
WAD_Enabled <- as.logical(ADAP_Config$Include_Tailwinds_Aloft)
Combined_ORD <- F

# Get Seg Size
GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")
Seg_Size <- GWCS_Adaptation$DME_Seg_Size

# PWS "Level" Options (Currently only ORD Operator used!)
Full_Level_Precedence <- c("Operator", "Aircraft", "20Cat", "14Cat", "Wake")

# Levels for New Operation 
ORD_Levels <- c(F, T, F, F, T)
TBS_Wake_Levels <- c(F, T, F, F, T)
TBS_ROT_Levels <- c(T, F, F, F, T)

# Levels for Legacy Operation
ORD_Levels_Legacy <- c(F, F, F, F, T)
TBS_Wake_Levels_Legacy <- c(F, F, F, F, T)
TBS_ROT_Levels_Legacy <- c(F, F, F, F, T)

# - PWS Options ((Recat))
UseLSSAdaptation <- F # If True, uses LSS Adaptation table that can be adapted for testing. False is existing operations.
FlexibleGusting <- F # If True, creates standardised "flexible" gusting for airbus types. False is existing ops. UseLSSAdaptation must be turned on to use and should be turned on for Use_EFDD.
Use_EFDD <- F # Use the adaptable End Final Deceleration Distance. False for existing operations.
ORDBuffers <- F # FALSE for Original Parameters. TRUE for Median Parameters. TRUE must be used for TBSC.
Use_Variable_Decel <- F # Use Deceleration that varies by wind. False for existing operations.

# - PWS TBSC Options
TBSCBuffers <- F # Use Time buffers for TBS Cacls. False for existing operations. Needs fixing!
TTB_Type <- "Original" # Type of TBSC Calculation: "Original"|"ORD"|"T2F" (T2F still in development) - Original is existing operations.

# - Other IA Options
Constraints <- c("Wake", "Non_Wake", "ROT", "Runway_Dependent") # Need to Add Runway Dependency Compatibility
Legacy_Constraints <- c("Wake", "Non_Wake", "ROT", "Runway_Dependent")
Forecast_Compression_Type <- 1 # 1: Traditional Forecast Compression, 2: Use of Forecast Distance, Speed/WE
Observed_Compression_Type <- 1 # Same as above.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Data Loading
# ----------------------------------------------------------------------------------------------------------------------------------------- #
INP_Radar <- Load_Radar_Data_ORD_Validation(con, PROC_Period, PROC_Criteria)
INP_Flight_Plan <- Load_Flight_Data_ORD_Validation(con, PROC_Period, PROC_Criteria)
INP_Segments <- Load_Stage_2_Segment_Data(con, PROC_Period, PROC_Criteria)
INP_Surface_Wind <- Load_Surface_Wind_Data(con, PROC_Period, PROC_Criteria)
# ----------------------------------------------------------------------------------------------------------------------------------------- #

## Clear Exisiting and create new Landing Pair to Get correct LPIDs.
if (!Testing){
  INP_Landing_Pair <- Generate_Landing_Pair(INP_Flight_Plan)
  Clear_Landing_Pair(con, PROC_Period, PROC_Criteria)
  PopulateSQLTable(con, "tbl_Landing_Pair", select(INP_Landing_Pair, -Landing_Pair_ID))
}

INP_Landing_Pair <- Load_Landing_Pair_Data(con, PROC_Period, PROC_Criteria)

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Processing
# ----------------------------------------------------------------------------------------------------------------------------------------- #

INT_Landing_Pairs <- Generate_All_Pair_Reference_Data(con, LP_Primary_Key, INP_Landing_Pair, INP_Radar, INP_Flight_Plan, INP_Surface_Wind, Full_Level_Precedence, TBS_Wake_Levels, TBS_ROT_Levels, TBSCBuffers)
INT_Landing_Pairs <- Generate_ORD_Observation(con, LP_Primary_Key, INT_Landing_Pairs, INP_Radar, INP_Surface_Wind)
INT_Full_GWCS_Forecast <- Generate_Full_ORD_GWCS_Forecast(con, LP_Primary_Key, INP_Segments, INT_Landing_Pairs, Time_Key = "Prediction_Time")

INT_Aircraft_Profile <- Generate_ORD_Aircraft_Profile(con, LP_Primary_Key, INT_Landing_Pairs, ORDBuffers, Use_EFDD, Full_Level_Precedence, ORD_Levels, LegacyorRecat = "Recat")
INT_IAS_Profile <- Generate_ORD_IAS_Profile(con, LP_Primary_Key, INT_Aircraft_Profile, INT_Landing_Pairs, INT_Full_GWCS_Forecast)
INT_GSPD_Profile <- Generate_ORD_GSPD_Profile(con, LP_Primary_Key, INT_IAS_Profile, INT_Full_GWCS_Forecast, Seg_Size)
INT_Landing_Pairs <- Generate_ORD_Prediction(con, LP_Primary_Key, INT_Landing_Pairs, INT_GSPD_Profile, INT_Full_GWCS_Forecast, INP_Radar,
                                             Constraints, TTB_Type, Forecast_Compression_Type, Observed_Compression_Type,
                                             Use_EFDD, ORD_Levels[1], LegacyorRecat = "Recat")

INT_Aircraft_Profile_Leg <- Generate_ORD_Aircraft_Profile(con, LP_Primary_Key, INT_Landing_Pairs, ORDBuffers = F, Use_EFDD = F, Full_Level_Precedence, ORD_Levels_Legacy, LegacyorRecat = "Legacy")
INT_IAS_Profile_Leg <- Generate_ORD_IAS_Profile(con, LP_Primary_Key, INT_Aircraft_Profile_Leg, INT_Landing_Pairs, INT_Full_GWCS_Forecast)
INT_GSPD_Profile_Leg <- Generate_ORD_GSPD_Profile(con, LP_Primary_Key, INT_IAS_Profile_Leg, INT_Full_GWCS_Forecast, Seg_Size)
INT_Landing_Pairs <- Generate_ORD_Prediction(con, LP_Primary_Key, INT_Landing_Pairs, INT_GSPD_Profile_Leg, INT_Full_GWCS_Forecast, INP_Radar,
                                             Legacy_Constraints, TTB_Type = "Original", Forecast_Compression_Type, Observed_Compression_Type,
                                             Use_EFDD = F, F, LegacyorRecat = "Legacy")

INT_Landing_Pairs <- INT_Landing_Pairs %>% mutate(Performance_Flag = 1)
INT_Landing_Pairs <- Generate_IA_Performance_Model_Setup(con, LP_Primary_Key, INT_Landing_Pairs, INP_Radar, INT_Full_GWCS_Forecast)

#if (WAD_Enabled & !Combined_ORD){
#  INT_Landing_Pairs <- Generate_WAD_Observation(con, LP_Primary_Key, INT_Landing_Pairs, INP_Radar, INP_Flight_Plan)
#  INT_Landing_Pairs <- Generate_WAD_Prediction(con, LP_Primary_Key, INT_Landing_Pairs, INT_GSPD_Profile)
#}

rm(INP_Radar)
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Construction
# ----------------------------------------------------------------------------------------------------------------------------------------- #

OUTP_All_Pair_Reference_Data <- Construct_All_Pair_Reference_Data(LP_Primary_Key, INT_Landing_Pairs)
OUTP_ORD_Observation <- Construct_ORD_Observation(LP_Primary_Key, INT_Landing_Pairs)
OUTP_ORD_Aircraft_Profile <- Construct_ORD_Aircraft_Profile(LP_Primary_Key, INT_Aircraft_Profile)
OUTP_ORD_IAS_Profile <- Construct_ORD_IAS_Profile(LP_Primary_Key, INT_IAS_Profile)
OUTP_ORD_GSPD_Profile <- Construct_ORD_GSPD_Profile(LP_Primary_Key, INT_GSPD_Profile)
OUTP_ORD_Prediction <- Construct_ORD_Prediction(LP_Primary_Key, INT_Landing_Pairs)
OUTP_IA_Performance_Model_Setup <- Construct_IA_Performance_Model_Setup(LP_Primary_Key, INT_Landing_Pairs)
rm(INT_GSPD_Profile)

#if (WAD_Enabled){
#  OUTP_WAD_Observation <- Construct_WAD_Observation(INT_Landing_Pairs, LP_Primary_Key)
#  OUTP_WAD_Prediction <- Construct_WAD_Prediction(INT_Landing_Pairs, LP_Primary_Key)
#}

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Testing
# ----------------------------------------------------------------------------------------------------------------------------------------- #

if (Testing){

  ZCOMP_All_Pair_Reference_Data <- Compare_All_Pair_Reference_Data(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Landing_Pairs)
  ZCOMP_ORD_Observation <- Compare_ORD_Observation(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Landing_Pairs)
  ZCOMP_ORD_Aircraft_Profile <- Compare_ORD_Aircraft_Profile(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Aircraft_Profile)
  ZCOMP_ORD_IAS_Profile <- Compare_ORD_IAS_Profile(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_IAS_Profile)
  ZCOMP_ORD_GSPD_Profile <- Compare_ORD_GSPD_Profile(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_GSPD_Profile)
  ZCOMP_ORD_Prediction <- Compare_ORD_Prediction(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Landing_Pairs)
  ZCOMP_IA_Performance_Model_Setup <- Compare_IA_Performance_Model_Setup(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Landing_Pairs)

  ZSTAT_All_Pair_Reference_Data <- Summary_All_Pair_Reference_Data(LP_Primary_Key, ZCOMP_All_Pair_Reference_Data) ##
  ZSTAT_ORD_Observation <- Summary_ORD_Observation(LP_Primary_Key, ZCOMP_ORD_Observation) ##
  ZSTAT_ORD_Aircraft_Profile <- Summary_ORD_Aircraft_Profile(LP_Primary_Key, ZCOMP_ORD_Aircraft_Profile) ##
  ZSTAT_ORD_IAS_Profile <- Summary_ORD_IAS_Profile(LP_Primary_Key, ZCOMP_ORD_IAS_Profile) ##
  ZSTAT_ORD_GSPD_Profile <- Summary_ORD_GSPD_Profile(LP_Primary_Key, ZCOMP_ORD_GSPD_Profile) ##
  ZSTAT_ORD_Prediction <- Summary_ORD_Prediction(LP_Primary_Key, ZCOMP_ORD_Prediction) ##
  ZSTAT_IA_Performance_Model_Setup <- Summary_IA_Performance_Model_Setup(LP_Primary_Key, ZCOMP_IA_Performance_Model_Setup) ##

  #if (WAD_Enabled){
  #  ZCOMP_WAD_Observation <- Compare_WAD_Observation(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Landing_Pairs) ##
  #  ZCOMP_WAD_Prediction <- Compare_WAD_Prediction(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Landing_Pairs) ##
  #
  #  ZSTAT_WAD_Observation <- Summary_WAD_Observation(LP_Primary_Key, ZCOMP_WAD_Observation) ##
  #  ZSTAT_WAD_Prediction <- Summary_WAD_PRediction(LP_Primary_Key, ZCOMP_WAD_Prediction) ##
  #}
}
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Database Interaction
# ----------------------------------------------------------------------------------------------------------------------------------------- #

if (!Testing){

  # Populating
  PopulateSQLTable(con, "tbl_All_Pair_Reference_Data", OUTP_All_Pair_Reference_Data)
  PopulateSQLTable(con, "tbl_ORD_Observation", OUTP_ORD_Observation)
  PopulateSQLTable(con, "tbl_ORD_Aircraft_Profile", OUTP_ORD_Aircraft_Profile)
  PopulateSQLTable(con, "tbl_ORD_Prediction", OUTP_ORD_Prediction)
  PopulateSQLTable(con, "tbl_eTBS_Performance_Model", OUTP_IA_Performance_Model_Setup)
  PopulateSQLTable(con, "tbl_ORD_IAS_Profile", OUTP_ORD_IAS_Profile)
  PopulateSQLTable(con, "tbl_ORD_GS_Profile", OUTP_ORD_GSPD_Profile)
  
  # if (WAD_Enabled){
  #   dbAppendTable(con, "tbl_WAD_Observation", OUTP_WAD_Observation)
  #   dbAppendTable(con, "tbl_WAD_Prediction", OUTP_WAD_Prediction
  # }

}
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# How long did it take?
Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Completed ORD Validation Process for ", PROC_Period, " of ", PROC_Criteria, " in ",
               seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #

### TEMP: NEW PERFORMANCE MODEL TABLE

# test <- select(INT_Landing_Pairs, Landing_Pair_ID, Recat_FAF_All_Separation_Distance, Recat_FAF_Max_Constraint, Legacy_FAF_All_Separation_Distance, Legacy_FAF_Max_Constraint)
# 
# New_Table <- INT_Landing_Pairs %>%
#   select(Landing_Pair_ID, Landing_Pair_Date,
#          Leader_Aircraft_Type, Leader_Callsign, Leader_Operator, Leader_Recat_Wake_Cat, Leader_Legacy_Wake_Cat, Leader_Time_At_4DME, Leader_Landing_Runway,
#          Follower_Aircraft_Type, Follower_Callsign, Follower_Operator, Follower_Recat_Wake_Cat, Follower_Legacy_Wake_Cat, Follower_Time_At_4DME, Follower_Landing_Runway,
#          Reference_Recat_Wake_Separation_Distance, Reference_Recat_Wake_Separation_Time, Reference_Recat_Wake_Separation_IAS,
#          Reference_Recat_ROT_Spacing_Distance, Reference_Recat_ROT_Spacing_Time, Reference_Recat_ROT_Spacing_IAS,
#          Reference_Legacy_Wake_Separation_Distance, Reference_Legacy_Wake_Separation_Time, Reference_Legacy_Wake_Separation_IAS,
#          Reference_Legacy_ROT_Spacing_Distance, Reference_Legacy_ROT_Spacing_Time, Reference_Legacy_ROT_Spacing_IAS,
#          Non_Wake_Separation_Distance, Non_Wake_RNAV_Separation_Distance,
#          Recat_Threshold_Wake_Separation_Distance, Recat_Threshold_Wake_Separation_IAS, Recat_Threshold_Wake_Separation_Wind_Effect,
#          Recat_Threshold_ROT_Spacing_Distance, Recat_Threshold_ROT_Spacing_IAS, Recat_Threshold_ROT_Spacing_Wind_Effect,
#          Recat_Threshold_Non_Wake_Separation_Distance,
#          Recat_Threshold_All_Separation_Distance, Recat_Threshold_Max_Constraint, Recat_Follower_Assumed_IAS, Recat_Follower_Forecast_Wind_Effect,
#          Recat_Forecast_ORD_Compression, Recat_FAF_Wake_Separation_Distance, Recat_FAF_ROT_Spacing_Distance, Recat_FAF_Non_Wake_Separation_Distance,
#          Recat_FAF_All_Separation_Distance, Recat_FAF_Max_Constraint,
#          Legacy_Threshold_Wake_Separation_Distance, Legacy_Threshold_Wake_Separation_IAS, Legacy_Threshold_Wake_Separation_Wind_Effect,
#          Legacy_Threshold_ROT_Spacing_Distance, Legacy_Threshold_ROT_Spacing_IAS, Legacy_Threshold_ROT_Spacing_Wind_Effect,
#          Legacy_Threshold_Non_Wake_Separation_Distance,
#          Legacy_Threshold_All_Separation_Distance, Legacy_Threshold_Max_Constraint, Legacy_Follower_Assumed_IAS, Legacy_Follower_Forecast_Wind_Effect,
#          Legacy_Forecast_ORD_Compression, Legacy_FAF_Wake_Separation_Distance, Legacy_FAF_ROT_Spacing_Distance, Legacy_FAF_Non_Wake_Separation_Distance,
#          Legacy_FAF_All_Separation_Distance, Legacy_FAF_Max_Constraint, Observed_AGI_Surface_Headwind,
# Observed_AGI_Surface_Wind_SPD,
# Observed_AGI_Surface_Wind_HDG, Observed_ORD_Compression, Observed_Threshold_Separation_Distance = Delivered_Threshold_Separation, Observed_FAF_Separation_Distance = Delivered_FAF_Separation,
# Recat_Threshold_Separation_Accuracy, Recat_FAF_Separation_Accuracy, Legacy_Threshold_Separation_Accuracy, Legacy_FAF_Separation_Accuracy, Recat_Observed_Follower_IAS, Recat_Observed_Follower_Wind_Effect,
# Legacy_Observed_Follower_IAS, Legacy_Observed_Follower_Wind_Effect