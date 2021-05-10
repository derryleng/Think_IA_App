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

# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # this will need editing to find wd outside of RStudio

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Source Global Configuration Files (Requires Shiny Integration)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path("0. Global Functions & Parameters", "Imports.R"), local = T)
source(file.path("0. Global Functions & Parameters", "Global Parameters.R"), local = T)
source(file.path("0. Global Functions & Parameters", "Global Functions.R"), local = T)

# Start Time
Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Source Function Files
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path("3. ORD Processing", "ORD Functions.R"), local = T) # ORD Functions
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path("3. ORD Processing", "All Pair Reference Data.R"), local = T) # All Pair Reference Data Functions
source(file.path("3. ORD Processing", "ORD-WAD Observation.R"), local = T) # ORD Observation Functions
source(file.path("3. ORD Processing", "ORD Aircraft Profile.R"), local= T) # ORD Aircraft Profile Functions
source(file.path("3. ORD Processing", "ORD IAS-GSPD Profile.R"), local= T) # ORD Segment/IAS/GS Profile Functions
source(file.path("3. ORD Processing", "ORD-WAD Prediction.R"), local= T) # ORD Prediction Functions
source(file.path("3. ORD Processing", "Setup IA Performance Model.R"), local= T) # Setup Performance Model Functions
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Configuration
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Database_Type <- "Validation"
Database <- "NavCan_TBS_V2_Test"
IP <- "192.168.1.23"
con <- Get_RODBC_Database_Connection(IP, Database)
PROC_Period <- "Month"
PROC_Criteria <- "08/2020"
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Testing <- T
LP_Primary_Key <- Get_LP_Primary_Key(Database_Type)
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Load Common Adaptation
# ----------------------------------------------------------------------------------------------------------------------------------------- #
ADAP_Config <- Load_Adaptation_Table(con, "tbl_Adaptation_Data")
WAD_Enabled <- as.logical(ADAP_Config$Include_Tailwinds_Aloft)
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Data Loading
# ----------------------------------------------------------------------------------------------------------------------------------------- #
INP_Radar <- Load_Radar_Data_ORD_Validation(con, PROC_Period, PROC_Criteria)
INP_Flight_Plan <- Load_Flight_Data_ORD_Validation(con, PROC_Period, PROC_Criteria)
INP_Landing_Pair <- Load_Landing_Pair_Data(con, PROC_Period, PROC_Criteria)
INP_Segments <- Load_Stage_2_Segment_Data(con, PROC_Period, PROC_Criteria)
INP_Surface_Wind <- Load_Surface_Wind_Data(con, PROC_Period, PROC_Criteria)
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Processing
# ----------------------------------------------------------------------------------------------------------------------------------------- #
INT_Landing_Pairs <- Generate_All_Pair_Reference_Data(con, LP_Primary_Key, INP_Landing_Pair, INP_Radar, INP_Flight_Plan, INP_Surface_Wind)
INT_Landing_Pairs <- Generate_ORD_Observation(con, LP_Primary_Key, INT_Landing_Pairs, INP_Radar, INP_Surface_Wind)
INT_Aircraft_Profile <- Generate_ORD_Aircraft_Profile(con, LP_Primary_Key, INT_Landing_Pairs)
INT_Full_GWCS_Forecast <- Generate_Full_ORD_GWCS_Forecast(con, LP_Primary_Key, INP_Segments, INT_Landing_Pairs, Time_Key = "Prediction_Time")
INT_IAS_Profile <- Generate_ORD_IAS_Profile(con, LP_Primary_Key, INT_Aircraft_Profile, INT_Landing_Pairs, INT_Full_GWCS_Forecast)
INT_GSPD_Profile <- Generate_ORD_GSPD_Profile(con, LP_Primary_Key, INT_IAS_Profile, INT_Full_GWCS_Forecast)
INT_Landing_Pairs <- Generate_ORD_Prediction(con, LP_Primary_Key, INT_Landing_Pairs, INT_GSPD_Profile)
INT_Landing_Pairs <- Generate_IA_Performance_Model_Setup(con, LP_Primary_Key, INT_Landing_Pairs, INP_Radar, INT_Full_GWCS_Forecast)

#if (WAD_Enabled){
#  INT_Landing_Pairs <- Generate_WAD_Observation(con, LP_Primary_Key, INT_Landing_Pairs, INP_Radar, INP_Flight_Plan)
#  INT_Landing_Pairs <- Generate_WAD_Prediction(con, LP_Primary_Key, INT_Landing_Pairs, INT_GSPD_Profile)
#}
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Construction
# ----------------------------------------------------------------------------------------------------------------------------------------- #
#OUTP_All_Pair_Reference_Data <- Construct_All_Pair_Reference_Data(LP_Primary_Key, INT_Landing_Pairs)
#OUTP_ORD_Observation <- Construct_ORD_Observation(LP_Primary_Key, INT_Landing_Pairs)
#OUTP_ORD_Aircraft_Profile <- Construct_ORD_Aircraft_Profile(LP_Primary_Key, INT_Aircraft_Profile)
#OUTP_ORD_IAS_Profile <- Construct_ORD_IAS_Profile(LP_Primary_Key, INT_IAS_Profile)
#OUTP_ORD_GSPD_Profile <- Construct_ORD_GSPD_Profile(LP_Primary_Key, INT_GSPD_Profile)
#OUTP_ORD_Prediction <- Construct_ORD_Prediction(LP_Primary_Key, INT_Landing_Pairs)
#OUTP_IA_Performance_Model_Setup <- Construct_IA_Performance_Model_Setup(LP_Primary_Key, INT_Landing_Pairs)

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

  # Clearing
  Clear_All_Pair_Reference_Data(con, PROC_Period, PROC_Criteria)
  Clear_ORD_Observation(con, PROC_Period, PROC_Criteria)
  Clear_ORD_Aircraft_Profile(con, PROC_Period, PROC_Criteria)
  Clear_ORD_IAS_Profile(con, PROC_Period, PROC_Criteria)
  Clear_ORD_GSPD_Profile(con, PROC_Period, PROC_Criteria)
  Clear_ORD_Prediction(con, PROC_Period, PROC_Criteria)
  
  if (WAD_Enabled){
    Clear_WAD_Observation(con, PROC_Period, PROC_Criteria)
    Clear_WAD_Prediction(con, PROC_Period, PROC_Criteria)
  }
  
  # Populating
  Populate_All_Pair_Reference_Data(con, OUTP_All_Pair_Reference_Data)
  Populate_ORD_Observation(con, OUTP_ORD_Observation)
  Populate_ORD_Aircraft_Profile(con, OUTP_ORD_Aircraft_Profile)
  Populate_ORD_IAS_Profile(con, OUTP_ORD_IAS_Profile)
  Populate_ORD_GSPD_Profile(con, OUTP_ORD_GSPD_Profile)
  Populate_ORD_Prediction(con, OUTP_ORD_Prediction)
  
  if (WAD_Enabled){
    Populate_WAD_Observation(con, OUTP_WAD_Observation)
    Populate_WAD_Prediction(con, OUTP_WAD_Prediction)
  }

}
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# How long did it take?
Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Completed ORD Validation Process for ", PROC_Period, " of ", PROC_Criteria, " in ",
               seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #

