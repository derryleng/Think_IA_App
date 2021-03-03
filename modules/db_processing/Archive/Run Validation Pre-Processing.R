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
library(lubridate)
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Directory Management
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Get the Program Start Time
Initial_Time_PP <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))

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

# The Processing Period. Controls how much data is processed ("All", "Month", "Day")
Processing_Period <- "Day"

# Processing Date if "Day" is selected.
Processing_Date <- "01/11/2018"

# Processing Month if "Month" is selected.
Processing_Month <- "11/2018"

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

# Localiser Adaptation
Localisers <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Localiser_Capture")

# -- PLT Adaptation (3 Sets)

# Path Leg Adaptation
Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")
#Path_Legs_2 <- Load_Adaptation_Table(con, "tbl_Path_Leg_2")
#Path_Legs_3 <- Load_Adaptation_Table(con, "tbl_Path_Leg_3")

# Path Leg Transition Adaptation
Path_Leg_Transitions <- Load_Adaptation_Table(con, "tbl_Path_Leg_Transition")
#Path_Leg_Transitions_2 <- Load_Adaptation_Table(con, "tbl_Path_Leg_Transition_2")
#Path_Leg_Transitions_3 <- Load_Adaptation_Table(con, "tbl_Path_Leg_Transition_3")

# Polygons
Polygons <- Load_Adaptation_Table(con, "tbl_Polygon")
#Polygons_2 <- Load_Adaptation_Table(con, "tbl_Polygon_2")
#Polygons_3 <- Load_Adaptation_Table(con, "tbl_Polygon_3")

# Volumes
Volumes <- Load_Adaptation_Table(con, "tbl_Volume")
#Volumes_2 <- Load_Adaptation_Table(con, "tbl_Volume_2")
#Volumes_3 <- Load_Adaptation_Table(con, "tbl_Volume_3")

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# X. Run Configuration Parameters
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# --- Config

# Magnetic Variation
Mag_Var <- as.numeric(Adaptation$Mag_Var)

# --- Stage 1 GWCS Filtering
Mode_S_GSPD_Min <- as.numeric(GWCS_Adaptation$Mode_S_GSPD_Min)
Mode_S_GSPD_Max <- as.numeric(GWCS_Adaptation$Mode_S_GSPD_Max)
Mode_S_IAS_Min <- as.numeric(GWCS_Adaptation$Mode_S_IAS_Min)
Mode_S_IAS_Max <- as.numeric(GWCS_Adaptation$Mode_S_IAS_Max)
Mode_S_TAS_Min <- as.numeric(GWCS_Adaptation$Mode_S_TAS_Min)
Mode_S_TAS_Max <- as.numeric(GWCS_Adaptation$Mode_S_TAS_Max)
Mode_S_Roll_Angle_Max <- as.numeric(GWCS_Adaptation$Mode_S_Roll_Angle_Max)
Time_Of_Day_Min <- as.integer(GWCS_Adaptation$Time_Of_Day_Min)
Time_Of_Day_Max <- as.integer(GWCS_Adaptation$Time_Of_Day_Max)
Altitude_Tolerance <- as.numeric(GWCS_Adaptation$Altitude_Tolerance)
Max_Wind_Effect <- as.numeric(GWCS_Adaptation$Max_Wind_Effect)
Max_Wind_SPD <- as.numeric(GWCS_Adaptation$Max_Wind_SPD)

# --- Stage 2 GWCS Filtering
DME_Seg_Min <- as.numeric(GWCS_Adaptation$DME_Seg_Min)
DME_Seg_Max <- as.numeric(GWCS_Adaptation$DME_Seg_Min)
Seg_Duration_Min <- as.numeric(GWCS_Adaptation$Seg_Duration_Min)
Seg_Duration_Max <- as.numeric(GWCS_Adaptation$Seg_Duration_Max)
Diff_Track_To_Runway_HDG_Max <- as.numeric(GWCS_Adaptation$Diff_Track_To_Runway_HDG_Max)
Diff_HDG_To_Runway_HDG_Max <- as.numeric(GWCS_Adaptation$Diff_HDG_To_Runway_HDG_Max)
Diff_Mode_S_To_Radar_Track_Max <- as.numeric(GWCS_Adaptation$Diff_Mode_S_To_Radar_Track_Max)
Diff_Mode_S_To_Radar_GSPD_Max <- as.numeric(GWCS_Adaptation$Diff_Mode_S_To_Radar_GSPD_Max)


# Time taken to Load Adaptation
PP_Adaptation_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Adaptation Data Loaded in ", seconds_to_period(PP_Adaptation_Time - Initial_Time_PP), "."))

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Y. Data Loading
# ------------------------------------------------------------------------------------------------------------------------------------------ #

Radar_Query        <- "SELECT 
                         Radar_Track_Point_ID,
                         Flight_Plan_ID,
                         Track_Date,
                         Track_Time,
                         X_Pos,
                         Y_Pos,
                         Mode_C,
                         Track_SPD,
                         Track_HDG,
                         --Mode_S_Address,
                         Mode_S_GSPD,
                         Mode_S_IAS,
                         Mode_S_HDG,
                         Mode_S_TAS,
                         Mode_S_Track_HDG,
                         Mode_S_Roll_Angle
                       FROM tbl_Radar_Track_Point"


Flight_Plan_Query  <- "SELECT
                         Flight_Plan_ID,
                         FP_Time,
                         Aircraft_Type,
                         Callsign,
                         Landing_Runway
                       FROM tbl_Flight_Plan"

Baro_Query <- "SELECT
                 Baro_Date,
                 Baro_Time,
                 Baro_Pressure
               FROM tbl_Baro"


# Adjust queries based on Processing Period.
if (Processing_Period == "Day"){
  Radar_Query <- paste0(Radar_Query, " WHERE Track_Date = '", Processing_Date, "'")
  Flight_Plan_Query <- paste0(Flight_Plan_Query, " WHERE FP_Date = '", Processing_Date, "'")
  Baro_Query <- paste0(Baro_Query, " WHERE Baro_Date = '", Processing_Date, "'")
}

if (Processing_Period == "Month"){
  Radar_Query <- paste0(Radar_Query, " WHERE Track_Date LIKE '%", Processing_Month, "%'")
  Flight_Plan_Query <- paste0(Flight_Plan_Query, " WHERE FP_Date LIKE '%", Processing_Month, "%'")
  Baro_Query <- paste0(Baro_Query, " WHERE Baro_Date LIKE '%", Processing_Date, "%'")
}

# Load in the Raw/Generated Data
message("Loading in Raw Data. This will take a while...")
PP_Load_Time_1 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))

# Radar Data
Radar <- sqlQuery(con, Radar_Query, stringsAsFactors = F)
PP_Load_Time_2 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Radar Data Loaded in ", seconds_to_period(PP_Load_Time_2 - PP_Load_Time_1), "."))

# Flight Plan Data
Flight_Plan <- sqlQuery(con, Flight_Plan_Query, stringsAsFactors = F)
PP_Load_Time_3 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Flight Plan Data Loaded in ", seconds_to_period(PP_Load_Time_3 - PP_Load_Time_2), "."))

# Baro Data
Baro <- sqlQuery(con, Baro_Query, stringsAsFactors = F)
PP_Load_Time_4 <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
message(paste0("Landing Pair Data Loaded in ", seconds_to_period(PP_Load_Time_4 - PP_Load_Time_3), "."))

# Total Time Taken
message(paste0("All Raw Data Loaded in ", seconds_to_period(PP_Load_Time_4 - PP_Load_Time_1), "."))

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
    #SQL_Clear_ORD_Validation_By_Month(con, Processing_Month)
  }
  
  # Clear by Day (If Processing_Period = "Day")
  if (Processing_Period == "Day"){
    #SQL_Clear_ORD_Validation_By_Day(con, Processing_Day)
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


# Final time taken for Processing:
message(paste0("All ORD Processing complete in ", seconds_to_period(ORD_Processing_Time_Final - ORD_Functions_Time), "."))


