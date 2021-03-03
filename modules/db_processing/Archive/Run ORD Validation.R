# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Run ORD (Validation)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
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
# v0: 
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Directory Management
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Get the Program Start Time
Initial_Time_ORD <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))

# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # this will need editing to find wd outside of RStudio

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Load Globals
# ------------------------------------------------------------------------------------------------------------------------------------------ #

message("Loading Imports, Global Functions and Global Parameters...")

# Load Required Packages - 0.0.
source(file.path("0. Global Functions & Parameters", "0.0. Imports.R"))

# Load Global Parameters (Unit Conversions etc) - 0.1.
source(file.path("0. Global Functions & Parameters", "0.1. Global Parameters.R"))

# Load Global Functions (MAths Functions etc) - 0.2.
source(file.path("0. Global Functions & Parameters", "0.2. Global Functions.R"))

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Setup Database Connection (RODBC for now)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

message("Connecting to Microsoft SQL Server...")

# Specify the Database IP/Computer Name (This should be integrated as a reactive in shiny)
IP <- "192.168.1.39"

# Specify the SQL Database Name (This should be integrated as a reactive in shiny)
Database <- "LVNL_UTMA_Validation"

# Get the Database Connection
con <- Get_RODBC_Database_Connection(IP, Database)

message(paste0("Connected to SQL Server Database ", Database, " on IP ", IP, "."))

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Configuration - Should be Controlled by the Shiny App
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Test Mode Active. This stops uploads and clears, and runs testing procedures. 
Test_Mode <- T

# Debug Mode Active. This ensures all Intermdiary data inputs are from SQL - purely to test for Functionaility.
Debug_Mode <- F

# The Processing Period. Controls how much data is processed ("All", "Month", "Day")
Processing_Period <- "Day"

# Processing Date if "Day" is selected.
Processing_Date <- "18/11/2018"

# Processing Month if "Month" is selected.
Processing_Month <- "04/2019"

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# X. Config Loading (Function in Global Functions)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

message("Loading Adaptation Tables...")

# Adaptation Data (Will Require Restructure)
Adaptation <- Load_Adaptation_Table(con, "tbl_Adaptation_Data")

# Airfield Data (Will Require Additions: Legacy/New Operations, Delivery Distances - Hardcode Above for Now)
Airfield <- Load_Adaptation_Table(con, "tbl_Airfield")

# GWCS Adaptation (For Segment Extrapolation)
GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")

# Runway Adaptation
Runway <- Load_Adaptation_Table(con, "tbl_Runway")

# Path Leg Adaptation
Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")

# ORD Adaptation (Runway, Aircraft Type, Wake Category and DBS Distance Config)
ORD_Runway <- Load_Adaptation_Table(con, "tbl_ORD_Runway_Adaptation")
ORD_Aircraft <- Load_Adaptation_Table(con, "tbl_ORD_Aircraft_Adaptation")
ORD_Wake <- Load_Adaptation_Table(con, "tbl_ORD_Wake_Adaptation")
ORD_DBS <- Load_Adaptation_Table(con, "tbl_ORD_DBS_Adaptation")

# Default Wind Segments
Default_Wind <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Default_Wind_Effect_Segments")

# Aircraft Type to Wake Mapping (RECAT, New Operation)
AC_To_Wake <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake")

# Aircraft Type to Wake Mapping (Legacy, Old Operation)
AC_To_Wake_Legacy <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake_Legacy")

# Legacy Wake Separation Distances
Legacy_Wake_Distance <- Load_Adaptation_Table(con, "tbl_DBS_Wake_Turbulence") #Should change this

# RECAT Wake Distances, Times, IAS
RECAT_Wake_Distance <- Load_Adaptation_Table(con, "tbl_Reference_Recat_Separation_Dist")
RECAT_Wake_Time <- Load_Adaptation_Table(con, "tbl_Reference_Recat_Separation_Time")
RECAT_Wake_IAS <- Load_Adaptation_Table(con, "tbl_Assumed_Recat_Separation_IAS")

# RECAT ROT Distances, Times, IAS
RECAT_ROT_Distance <- Load_Adaptation_Table(con, "tbl_Reference_ROT_Spacing_Dist")
RECAT_ROT_Time <- Load_Adaptation_Table(con, "tbl_Reference_ROT_Spacing_Time")
RECAT_ROT_IAS <- Load_Adaptation_Table(con, "tbl_Assumed_ROT_Spacing_IAS")

# More SASAI Tables for Pairwise Performance Modelling (Will not be using yet)
# Legacy_Wake_Time <- Load_Adaptation_Table(con, "tbl_Legacy_Wake_Separation_Time")
# Legacy_Wake_IAS <- Load_Adaptation_Table(con, "tbl_Assumed_Legacy_Separation_IAS")
# Legacy_ROT_Distance <- Load_Adaptation_Table(con, "tbl_Legacy_ROT_Spacing_Dist")
# Legacy_ROT_Time <- Load_Adaptation_Table(con, "tbl_Legacy_ROT_Spacing_Time")
# Legacy_ROT_IAS <- Load_Adaptation_Table(con, "tbl_Assumed_Legacy_ROT_IAS")

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# X. Run Configuration Parameters
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# --- Hardcoded Delivery Distance/Operations to be included in tbl_Airfield

# Operation we are validating:
New_Operation <- "IA"
# New_Operation <- as.character(Airfield$New_Operation)

# Delivery Point under Proposed New Operations:
New_Delivery <- 0 * NM_to_m
# New_Delivery <- as.numeric(Airfield$New_Delivery_Point)

# Existing Operation:
Old_Operation <- "DBS"
# New_Operation <- as.character(Airfield$Old_Operation)

# Old Operation Delivery Point
Legacy_Delivery <- 4 * NM_to_m
# New_Delivery <- as.numeric(Airfield$Old_Delivery_Point)

# --- ORD Config Parameters

# Select how to run ORD. Can be Aircraft_Type, Wake_Only, TBS_Table
ORD_Profile_Selection <- as.character(Adaptation$ORD_Profile_Selection)

# Flag to remove all non-wake pairs.
Wake_Pairs_Only <- as.logical(Adaptation$Process_Wake_Pairs_Only)

# Flag to remove Not-In_Trail pairs. 
In_Trail_Only <- as.logical(Adaptation$Process_In_Trail_Pairs_Only)

# Flag to process WAD.
WAD_Enabled <- as.logical(Adaptation$Include_Tailwinds_Aloft)

# --- GWCS Config Parameters (For Extrapolation Algorithm)

# GWCS Wind Type Selection. 
GWCS_Wind_Selection <- as.character(Adaptation$GWCS_Wind_Selection)

# Get the Individual Segment Size
Seg_Size <- as.numeric(GWCS_Adaptation$DME_Seg_Size)

# Get the Final Valid Forecast Segment.
Forecast_Seg_Max <- as.numeric(GWCS_Adaptation$Forecast_Seg_Max)

# Get the Forecast Segment Stale Time.
Stale_Time <- as.numeric(GWCS_Adaptation$Forecast_Stale_Time)

# The maximum number of adjacent segments away a TBS extrapolation can occur from (in either direction)
Max_Seg_Extrapolation <- as.numeric(GWCS_Adaptation$Max_Seg_Extrapolation)

# The minimum segment that can be extrapolated. Segments below this cannot be included in extrapolation
Extrapolation_Seg_Min <- as.numeric(GWCS_Adaptation$Extrapolation_Seg_Min)

# The final segment that can be extrapolated using TBS extrapolation.
Separation_Forecast_Seg_Max <- as.numeric(GWCS_Adaptation$Separation_Forecast_Seg_Max)

# Time taken to Load Adaptation
ORD_Adaptation_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Adaptation Data Loaded in ", seconds_to_period(ORD_Adaptation_Time - Initial_Time_ORD), "."))

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Y. Data Loading
# ------------------------------------------------------------------------------------------------------------------------------------------ #

Radar_Query        <- "SELECT 
                         rtp.Flight_Plan_ID,
                         rtp.Track_Time,
                         rtpd.Range_To_Threshold,
                         rtpd.Mode_S_Wind_Localiser_Capture,
                         rtp.Mode_S_IAS,
                         rtpd.Wind_Effect_IAS,
                         rtpd.Path_Leg
                       FROM tbl_Radar_Track_Point rtp
                       LEFT JOIN tbl_Radar_Track_Point_Derived rtpd
                       ON rtp.Radar_Track_Point_ID = rtpd.Radar_Track_Point_ID "

Surface_Wind_Query <- "SELECT
                         Landing_Runway,
                         Anemo_Date,
                         Anemo_Time,
                         Anemo_SPD,
                         Anemo_HDG
                       FROM tbl_Anemometer"  
      
Flight_Plan_Query  <- "SELECT
                         FP.Flight_Plan_ID,
                         FP_Time,
                         Aircraft_Type,
                         Callsign,
                         FP.Landing_Runway,
                         Time_At_4DME
                       FROM tbl_Flight_Plan FP
                       LEFT JOIN tbl_Flight_Plan_Derived FPD
                       ON FP.Flight_Plan_ID = FPD.Flight_Plan_ID"

Landing_Pair_Query <- "SELECT 
                         *
                       FROM tbl_Landing_Pair" 

Forecast_Segs_Query <- "SELECT 
                         *
                        FROM tbl_Mode_S_Wind_Seg_Forecast"

# Adjust queries based on Processing Period.
if (Processing_Period == "Day"){
  Radar_Query <- paste0(Radar_Query, " WHERE Track_Date = '", Processing_Date, "'")
  Flight_Plan_Query <- paste0(Flight_Plan_Query, " WHERE FP_Date = '", Processing_Date, "'")
  Landing_Pair_Query <- paste0(Landing_Pair_Query, " WHERE Landing_Pair_Date = '", Processing_Date, "'")
  Surface_Wind_Query <- paste0(Surface_Wind_Query, " WHERE Anemo_Date = '", Processing_Date, "'")
  Forecast_Segs_Query <- paste0(Forecast_Segs_Query, " WHERE Forecast_Date = '", Processing_Date, "'")
}

if (Processing_Period == "Month"){
  Radar_Query <- paste0(Radar_Query, " WHERE Track_Date LIKE '%", Processing_Month, "%'")
  Flight_Plan_Query <- paste0(Flight_Plan_Query, " WHERE FP_Date LIKE '%", Processing_Month, "%'")
  Landing_Pair_Query <- paste0(Landing_Pair_Query, " WHERE Landing_Pair_Date LIKE '%", Processing_Month, "%'")
  Surface_Wind_Query <- paste0(Surface_Wind_Query, " WHERE Anemo_Date LIKE '%", Processing_Month, "%'") 
  Forecast_Segs_Query <- paste0(Forecast_Segs_Query, " WHERE Forecast_Date LIKE '%", Processing_Month, "%'")
}

# Load in the Raw/Generated Data
message("Loading in Raw Data. This will take a while...")
ORD_Load_Time_1 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))

# Radar Data
Radar <- sqlQuery(con, Radar_Query, stringsAsFactors = F)
ORD_Load_Time_2 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Radar Data Loaded in ", seconds_to_period(ORD_Load_Time_2 - ORD_Load_Time_1), "."))

# Flight Plan Data
Flight_Plan <- sqlQuery(con, Flight_Plan_Query, stringsAsFactors = F)
ORD_Load_Time_3 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Flight Plan Data Loaded in ", seconds_to_period(ORD_Load_Time_3 - ORD_Load_Time_2), "."))

# Landing Pair Data
Landing_Pair <- sqlQuery(con, Landing_Pair_Query, stringsAsFactors = F)
ORD_Load_Time_4 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Landing Pair Data Loaded in ", seconds_to_period(ORD_Load_Time_4 - ORD_Load_Time_3), "."))

# Surface Wind Data
Surface_Wind <- sqlQuery(con, Surface_Wind_Query, stringsAsFactors = F)
ORD_Load_Time_5 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Surface Wind Data Loaded in ", seconds_to_period(ORD_Load_Time_5 - ORD_Load_Time_4), "."))

# Forecast Segment Data
Forecast_Seg <- sqlQuery(con, Forecast_Segs_Query, stringsAsFactors = F)
ORD_Load_Time_6 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Forecast Segment Data Loaded in ", seconds_to_period(ORD_Load_Time_6 - ORD_Load_Time_5), "."))

# Total Time Taken
message(paste0("All Raw Data Loaded in ", seconds_to_period(ORD_Load_Time_6 - ORD_Load_Time_1), "."))

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 0. Clearing (Functions found in Global Functions)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

message("Clearing Required ORD Data...")

if (!Test_Mode){
  
  # Clear All (If Processing_Period = "All")
  if (Processing_Period == "All"){
    #SQL_Clear_ORD_Validation(con)
  }
  
  # Clear by Month (If Processing_Period = "Month")
  if (Processing_Period == "Month"){
    SQL_Clear_ORD_Validation_By_Month(con, Processing_Month)
  }
  
  # Clear by Day (If Processing_Period = "Day")
  if (Processing_Period == "Day"){
    SQL_Clear_ORD_Validation_By_Day(con, Processing_Day)
  }
  
}

# Find Completion Time of Clearing Data
ORD_Clear_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Required ORD Data Cleared in ", seconds_to_period(ORD_Clear_Time - ORD_Load_Time_5), "."))

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 1. Processing
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# -- Load ORD Functions
message("Loading ORD Functions...")
source(file.path("5. Algorithm Processing", "5.2. ORD", "ORD Functions.R"))
ORD_Functions_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Loaded ORD Functions in ", seconds_to_period(ORD_Functions_Time - ORD_Clear_Time), "."))

# -- Generate Landing Pair Reference Data
message("Generating Landing Pair Reference Data...")
source(file.path("4. Algorithm Setup", "4.2. Landing Pairs", "4.2.1. Generate All Pair Reference Data.R"))
ORD_Processing_Time_1 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Generated Landing Pair Reference Data in ", seconds_to_period(ORD_Processing_Time_1 - ORD_Functions_Time), "."))

# -- Generate ORD/WAD Observation
message("Generating ORD Observed Parameters...")
source(file.path("5. Algorithm Processing", "5.2. ORD", "5.2.2. Generate ORD Observation.R"))
ORD_Processing_Time_2 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Generated ORD Observation in ", seconds_to_period(ORD_Processing_Time_2 - ORD_Processing_Time_1), "."))

# -- Generate ORD Aircraft Profiles
message("Generating ORD Aircraft Profile Parameter Data...")
source(file.path("5. Algorithm Processing", "5.2. ORD", "5.2.1. Generate ORD Aircraft Profile.R"))
ORD_Processing_Time_3 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Generated ORD Aircraft Profile Parameters in ", seconds_to_period(ORD_Processing_Time_3 - ORD_Processing_Time_2), "."))

# -- Generate ORD Segment Extrapolation
message("Generating ORD Forecast Segment Data...")
source(file.path("5. Algorithm Processing", "5.2. ORD", "5.2.3. Generate Wind Segment Forecast ORD.R"))
ORD_Processing_Time_4 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Generated ORD Forecast Segments in ", seconds_to_period(ORD_Processing_Time_4 - ORD_Processing_Time_3), "."))

# -- Generate ORD IAS Profile
message("Generating ORD Forecast IAS/GS Profiles...")
source(file.path("5. Algorithm Processing", "5.2. ORD", "5.2.4. Generate ORD IAS Profile.R"))
ORD_Processing_Time_5 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Generated ORD IAS Profile in ", seconds_to_period(ORD_Processing_Time_5 - ORD_Processing_Time_4), "."))

# -- Generate ORD GS Profile
#message("Generating ORD Forecast GSPD Profile...")
#source(file.path("5. Algorithm Processing", "5.2. ORD", "5.2.1. Generate ORD GSPD Profile.R"))
ORD_Processing_Time_6 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
#message(paste0("Generated ORD GSPD Profile in ", seconds_to_period(ORD_Processing_Time_6 - ORD_Processing_Time_5), "."))

# -- Generate ORD/WAD Prediction
message("Generating ORD Prediction Parameters...")
source(file.path("5. Algorithm Processing", "5.2. ORD", "5.2.6. Generate ORD Prediction.R"))
ORD_Processing_Time_7 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Generated ORD Prediction in ", seconds_to_period(ORD_Processing_Time_7 - ORD_Processing_Time_6), "."))

# -- Generate Performance Model Setup
message("Generating Performance Model Setup...")
source(file.path("5. Algorithm Processing", "5.2. ORD", "5.2.8. Setup Performance Model.R"))
ORD_Processing_Time_Final <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
#message(paste0("Generated Performance Model Parameters in ", seconds_to_period(ORD_Processing_Time_Final - ORD_Processing_Time_7), "."))

# Final time taken for Processing:
message(paste0("All ORD Processing complete in ", seconds_to_period(ORD_Processing_Time_Final - ORD_Functions_Time), "."))


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 2. Table Formatting (Functions found in ORD Functions)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ORD Observation
#ORD_Observation <- Prepare_ORD_Observation(Landing_Pair_Reference)

# ORD Prediction
#ORD_Prediction <- Prepare_ORD_Prediction(Landing_Pair_Reference)

# Performance Model
#Performance_Model <- Prepare_Performance_Model(Landing_Pair_Reference)

# WAD Observation & Prediction
if (WAD_Enabled){
  WAD_Observation <- Prepare_WAD_Observation(Landing_Pair_Reference)
  WAD_Prediction <- Prepare_WAD_Prediction(Landing_Pair_Reference)
}

# All Pair Reference Data
#All_Pair_Reference_Data <- Prepare_All_Pair_Reference_Data(Landing_Pair_Reference)

# Preparation Time
ORD_Table_Prep_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("All ORD tables prepared in ", Convert_Seconds_to_Time_String(ORD_Table_Prep_Time - ORD_Processing_Time_Final), "."))

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 2. Testing (Only if Test_Mode is on)
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 3. Uploading (Function found in Global Functions)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

if (!Test_Mode){

  message(paste0("Loading Processed Data back into ", Database, "."))
  
  # ORD Aircraft Profile
  message("Loading data into tbl_ORD_Aircraft_Profile...")
  SQL_Load(con, ORD_Aircraft_Profile, "tbl_ORD_Aircraft_Profile")
  
  # ORD Observation
  message("Loading data into tbl_ORD_Observation...")
  SQL_Load(con, ORD_Observation, "tbl_ORD_Observation")
  
  # ORD Segment Forecast
  message("Loading data into tbl_ORD_Segment_Forecast...")
  SQL_Load(con, ORD_Segments, "tbl_ORD_Segment_Forecast")
  
  # ORD IAS Profile
  message("Loading data into tbl_ORD_IAS_Profile...")
  SQL_Load(con, ORD_IAS_Profile, "tbl_ORD_IAS_Profile")
  
  # ORD GSPD Profile
  message("Loading data into tbl_ORD_GS_Profile...")
  SQL_Load(con, ORD_GS_Profile, "tbl_ORD_GS_Profile")
  
  # Performance Model
  message("Loading data into tbl_eTBS_Performance_Model...")
  SQL_Load(con, Performance_Model, "tbl_eTBS_Performance_Model") # Should be changed - perhaps to IA?
  
  # All Pair Reference Data
  message("Loading data into tbl_All_Pair_Reference_Data...")
  SQL_Load(con, All_Pair_Reference_Data, "tbl_All_Pair_Reference_Data")
  
  # WAD Tables
  if (WAD_Enabled){
    SQL_Load(con, WAD_Observation, "tbl_WAD_Observation")
    SQL_Load(con, WAD_Prediction, "tbl_WAD_Prediction")
  }

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 3. Tidying
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Get the Final Completion Time.
Final_Time_ORD <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Full ORD run Complete in ", Convert_Seconds_to_Time_String(Final_Time_ORD - Initial_Time_ORD), "."))

# Create Table of Times Taken


# Remove all unecessary variables/objects
rm(Initial_Time_ORD, Final_Time_ORD)

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
