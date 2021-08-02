# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# IAC TBS Hub
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v1.0
#
# Authors: George Clark
#
# Description: All TBS Processing elements for the UTMA Validation Database.
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
Database <- "EGLL_PWS"
IP <- "192.168.1.23"
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Database Connection
con <- Get_DBI_Connection(IP, Database)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# BEGIN
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Time1 <- Sys.time()
Testing <- F # If TRUE, Database is not altered.
PROC <- 1
PROC_Period <- c("Day", "Month", "All")[PROC]
PROC_Criteria <- c("01/06/2019", "06/2019", NA)[PROC]
Populate_Legacy <- T # Populate Legacy ORD Tables
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Date_List <- dbGetQuery(con, "SELECT DISTINCT(FP_Date) FROM tbl_Flight_Plan")
Date_List <- Date_List$FP_Date
#Date_List <- Date_List[1]
#Date <- Date_List
#for (PROC_Criteria in Date_List$Landing_Pair_Date){
if (!Testing){Clear_Landing_Pair(con, PROC_Period = "All", NA)}
for (Date in Date_List){
  message("Generating All TBS Data for, ", Date, "...")
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 0. Adaptation ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# These should be set in Adaptation Tables. But for easy prototyping, keep internal.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# Primary Key in use. Use Observation_ID to distibuish between Bolstered Samples with same LP.
LP_Primary_Key <- "Observation_ID"

# Bolstering: Turn on bolstering methods to increase sample size via different methods. Only WTC active for now.
Bolster_WTC <- F
Bolster_WTC_Pairs <- c() # Fill with "A-A" format pairs.
Bolster_Sep <- F
Bolster_Sep_Pairs <- c() # Not active.

# Level Precedences. Determine priority of Adaptation Grab.
Full_Level_Precedence <- c("Operator", "Aircraft", "Wake")

# ------------------------------------------------------------------------------------------------------------ #
# 0.1. "New" Adaptation (RECAT) ----
# ------------------------------------------------------------------------------------------------------------ #

# "New" Wake scheme.
New_Wake_Scheme <- "RECAT-EU"

# Applicable Constraints for TBS Distance calculations. (Wake, ROT, Non_Wake, Runway_Dependent Supported)
New_Constraints <- c("Wake", "ROT", "Non_Wake", "Runway_Dependent")

# Levels to use for "New" Operation ORD, SASAI Wake and ROT adaptation grab. Must all be same length as Full_Level_Precedence.
ORD_Levels <- c(F, T, T)
TBS_Wake_Levels <- c(F, T, T)
TBS_ROT_Levels <- c(F, F, T)

# Switch for use of TBSC Time Buffers (If TRUE, Buffer in time adaptation files added to reference)
TBSCBuffersNew <- F

# Switch for use of Variable End Final Deceleration Distance, now in ORD Adaptation. eTBS/old IA should have this OFF.
UseEFDDNew <- T

# Decide the method of calculating TBS Distances. ("Original" | "ORD" | "T2F")
TTBTypeNew <- "T2F"

# Fix the Delivery Point for New Operations. Currently only supports one per operation, not one per constraint.
New_Delivery_Point <- 0 * 1852

# Choose the method for calculating FORECAST Compression. 1 = Traditional Follower - Leader Flying Distance, 2 = Speed and WE Construction
ForecastCompressionTypeNew <- 2

# Choose the method for calculating OBSERVED Compression. 1 = Traditional Follower - Leader Flying Distance, 2 = Speed and WE Construction (Allows for loss of procedural speed dependencies)
ObservedCompressionTypeNew <- 2

# Do we want to use the Follower IAS Adjustment (Removes the difference in L/F Set Procedural Speeds in Forecast Average Speed When calculating Type 2 Observed Compressions)
UseAdjustmentNew <- T

# Adjustment of Observed Range to Threshold/Wind Effect values to increase sample size.
AdjustWEsNew <- F # Use the below parameters to adjust the wind effect/RTT Values
MaxRangeToILSNew <- 3 * NM_to_m # The maximum distance from the ILS to consider alternate RTT/WE values for Observed Calcs.
AllowedPathLegsNew <- c("Intercept", "Ext_Intercept", "ILS") # Allowed Path legs for Radar tracks to be considered for alternate RTT/WE Values.
MaxUnderRepNew <- 2 * NM_to_m # The maximum gap between the Max Range to Threshold and the Max Required Range to Threshold for Observed Calcs.

# WAD Related (Not used)
CombinedORDNew <- T
WADEnabledNew <- T

# Do we want to use Bounded Times for Observed Calcs or Distances? ("Time" | "Range")
TimeorRangeNew <- "Time" 

# Under-Separation Distance Values for Performance Model (Recat Times)
UnderSepsNew <- c(0, 0.5 * NM_to_m, 1.0 * NM_to_m)


# ------------------------------------------------------------------------------------------------------------ #
# 0.1. "Old" Adaptation (LEGACY) ----
# ------------------------------------------------------------------------------------------------------------ #

# "Old" Wake scheme.
Old_Wake_Scheme <- "RECAT-EU"

# Applicable Constraints for TBS Distance calculations. (Wake, ROT, Non_Wake, Runway_Dependent Supported)
Old_Constraints <- c("Wake", "ROT", "Non_Wake", "Runway_Dependent")

# Levels to use for "Old" Operation ORD, SASAI Wake and ROT adaptation grab. Must all be same length as Full_Level_Precedence.
ORD_Levels_Legacy <- c(F, T, T)
TBS_Wake_Levels_Legacy <- c(F, T, T)
TBS_ROT_Levels_Legacy <- c(F, F, T)

# Switch for use of TBSC Time Buffers (If TRUE, Buffer in time adaptation files added to reference)
TBSCBuffersOld <- F

# Switch for use of Variable End Final Deceleration Distance, now in ORD Adaptation. eTBS/old IA should have this OFF.
UseEFDDOld <- T

# Decide the method of calculating TBS Distances. ("Original" | "ORD" | "T2F")
TTBTypeOld <- "ORD"

# Fix the Delivery Point for New Operations. Currently only supports one per operation, not one per constraint.
Old_Delivery_Point <- 0 * 1852

# Choose the method for calculating FORECAST Compression. 1 = Traditional Follower - Leader Flying Distance, 2 = Speed and WE Construction
ForecastCompressionTypeOld <- 2

# Choose the method for calculating OBSERVED Compression. 1 = Traditional Follower - Leader Flying Distance, 2 = Speed and WE Construction (Allows for loss of procedural speed dependencies)
ObservedCompressionTypeOld <- 2

# Do we want to use the Follower IAS Adjustment (Removes the difference in L/F Set Procedural Speeds in Forecast Average Speed When calculating Type 2 Observed Compressions)
UseAdjustmentOld <- T

# Adjustment of Observed Range to Threshold/Wind Effect values to increase sample size.
AdjustWEsOld <- F # Use the below parameters to adjust the wind effect/RTT Values
MaxRangeToILSOld <- 3 * NM_to_m # The maximum distance from the ILS to consider alternate RTT/WE values for Observed Calcs.
AllowedPathLegsOld <- c("Intercept", "Ext_Intercept", "ILS") # Allowed Path legs for Radar tracks to be considered for alternate RTT/WE Values.
MaxUnderRepOld <- 2 * NM_to_m # The maximum gap between the Max Range to Threshold and the Max Required Range to Threshold for Observed Calcs.

# WAD Related (Not used)
CombinedORDOld <- F
WADEnabledOld <- F

# Do we want to use Bounded Times for Observed Calcs or Distances? ("Time" | "Range")
TimeorRangeOld <- "Time"

# Under-Separation Distance Values for Performance Model (Legacy Times)
UnderSepsOld <- c(0, 0.5 * NM_to_m, 1.0 * NM_to_m)


# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 1. Data Loading ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Loading of Data from SQL.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------------------------------------------------------ #
# 1.1. Adaptation/Reference Data ----
# ------------------------------------------------------------------------------------------------------------ #

ACTW <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake") %>% select(-Aircraft_Class) # Recat Aircraft Type To Wake
ACTWL <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake_Legacy") %>% select(-Aircraft_Class) # Legacy Aircraft Type to Wake
Runways <- Load_Adaptation_Table(con, "tbl_Runway") # Runway Adaptation
ORDRunways <- Load_Adaptation_Table(con, "tbl_ORD_Runway_Adaptation") # ORD Runway Adaptation
Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg") # Path Leg Data
Recat_Wake_Distances <- Load_Adaptation_Table(con, "tbl_Reference_Recat_Separation_Dist") # Reference RECAT WTC Pair Distances, for Bolstering
Seg_Size <- as.numeric(Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")$DME_Seg_Size) # GWCS Segment Size
Delivery_Points <- Load_Adaptation_Table(con, "tbl_Delivery_Points") %>% rename(SASAI_Constraint = Constraint) # Constraint Delivery Point Information 

# ------------------------------------------------------------------------------------------------------------ #
# 1.2. Raw/Processed Data ----
# ------------------------------------------------------------------------------------------------------------ #

Radar <- Load_Radar_Data_ORD_Validation(con, PROC_Period, Date) # Radar Data
FP <- Load_Flight_Data_ORD_Validation(con, PROC_Period, Date) # Flight Plan (Aircraft Level) Data
Segments <- Load_Stage_2_Segment_Data(con, PROC_Period, Date) # GWCS Forecast Segment Data
SW <- Load_Surface_Wind_Data(con, PROC_Period, Date) # Surface Wind data

# If not in Testing Mode, Generate LAnding Pairs for this day and load to SQL
if (!Testing){
  LP <- Generate_Landing_Pair(FP)
  #Clear_Landing_Pair(con, PROC_Period, Date)
  PopulateSQLTable(con, "tbl_Landing_Pair", select(LP, -Landing_Pair_ID))
}

# Load Landing Pair data from SQL.
LP <- Load_Landing_Pair_Data(con, PROC_Period, Date)


# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 2. Adaptation Grabbing and Pre-maniuplation ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Grab relevant adaptation based on the Levels argumments, and get some preliminary important data.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# Gather Aircraft Reference Data & Original Prediction Time ----
LP <- Get_LF_Ref_Parameters(LP, FP, Runways, ACTW, ACTWL, LorF = "Leader")
LP <- Get_LF_Ref_Parameters(LP, FP, Runways, ACTW, ACTWL, LorF = "Follower")
LP <- Get_ORD_Prediction_Time(LP, Radar, Path_Legs)

# Find Aircraft Max RTT on Glideslope (and other distances) ----
MaxRTTs <- select(FP, Flight_Plan_ID) %>% Get_Max_RTTs(Radar, "Range_To_Threshold")
LP <- left_join(LP, MaxRTTs, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>% rename(Leader_Max_RTT = Max_RTT)
LP <- left_join(LP, MaxRTTs, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>% rename(Follower_Max_RTT = Max_RTT)

# ------------------------------------------------------------------------------------------------------------ #
# 2.1. Bolstering Data ----
# ------------------------------------------------------------------------------------------------------------ #

# Produce Bolstered Samples (Wake Pair, Runway/ROT Pair) ----
Bolster_Columns <- c("Bolster_Flag_WTC", "Bolster_Flag_Sep")

# Initalize Bolster Columns.
LP <- LP %>%
  mutate(Bolster_Flag_WTC = 0,
         Bolster_Flag_Sep = 0)

# Bolster by Leader WTC if enabled.
if (Bolster_WTC){
  LP <- Bolster_Landing_Pair_By_Leader_WTC(LP, New_Wake_Scheme, Old_Wake_Scheme, Recat_Wake_Distances, Bolster_WTC_Pairs)
}

# Bolster by Separation Distance if enabled.
if (Bolster_Sep){}

# Set the Observation ID, the new ID column that distibguishes between different bolstered samples of the same pair.
LP <- mutate(LP, Observation_ID = row_number()) %>% select(Observation_ID, everything())

# Create a column to identify any "Fake" observations of any bolstering type
LP <- mutate(LP, Fake_Observation = ifelse(psum(!!!syms(Bolster_Columns)) > 0, 1, 0))

# ------------------------------------------------------------------------------------------------------------ #
# 2.2. Pair Reference Data ----
# ------------------------------------------------------------------------------------------------------------ #

# Get the Leader and Follower Operators.
LP <- LP %>%
  mutate(Leader_Operator = substr(Leader_Callsign, 1, 3),
         Follower_Operator = substr(Follower_Callsign, 1, 3))

# Get Reference "Recat" Pair Parameters. Currently includes Wake and ROT adaptation.
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, TBS_Wake_Levels, Param_Type = "Distance", TBSCBuffersNew, "Recat")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, TBS_Wake_Levels, Param_Type = "Time", TBSCBuffersNew, "Recat")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, TBS_Wake_Levels, Param_Type = "Speed", TBSCBuffersNew, "Recat")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, TBS_ROT_Levels, Param_Type = "Distance", TBSCBuffersNew, "Recat")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, TBS_ROT_Levels, Param_Type = "Time", TBSCBuffersNew, "Recat")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, TBS_ROT_Levels, Param_Type = "Speed", TBSCBuffersNew, "Recat")

# Get Reference "Legacy" Pair Parameters. Currently includes Wake and ROT adaptation.
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, TBS_Wake_Levels_Legacy, Param_Type = "Distance", TBSCBuffersOld, "Legacy")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, TBS_Wake_Levels_Legacy, Param_Type = "Time", TBSCBuffersOld, "Legacy")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, TBS_Wake_Levels_Legacy, Param_Type = "Speed", TBSCBuffersOld, "Legacy")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, TBS_ROT_Levels_Legacy, Param_Type = "Distance", TBSCBuffersOld, "Legacy")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, TBS_ROT_Levels_Legacy, Param_Type = "Time", TBSCBuffersOld, "Legacy")
LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, TBS_ROT_Levels_Legacy, Param_Type = "Speed", TBSCBuffersOld, "Legacy")

# PRepare TBSC Extras (RNAV Flag, Non-Wake Spacing, Runway Dep Separation, TBS Service Level)
LP <- LP %>% Get_RNAV_Flag() %>% Get_Non_Wake_Spacing() %>% Get_Runway_Dependent_Separation()
LP <- Get_TBS_Service_Level(con, LP, LP_Primary_Key)

# Get the DBS All Sep Distance - ## Wake only for TBS Table for existing operations.
LP <- mutate(LP, 
             Recat_DBS_All_Sep_Distance = pmax(Reference_Recat_ROT_Spacing_Distance, Reference_Recat_Wake_Separation_Distance, Non_Wake_Separation_Distance, na.rm = T),
             Legacy_DBS_All_Sep_Distance = pmax(Reference_Legacy_ROT_Spacing_Distance, Reference_Legacy_Wake_Separation_Distance, Non_Wake_Separation_Distance, na.rm = T))

# Get the Forecast Surface Wind for the Leader
LP <- Get_Surface_Wind(LP, SW, Runways,
                       Prefix = "Forecast_AGI",
                       ID_Var = "Observation_ID",
                       Date_Var = "Landing_Pair_Date",
                       Time_Var = "Prediction_Time",
                       Runway_Var = "Leader_Landing_Runway")

# Find New Prediction Times ----

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 3. Intermediary Table Generation ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Create the New/Old ORD Profile tables and the GWCS Forecast.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# Generate the Full GWCS Forecast based on the Prediction Times
GWCS_Forecast <- Generate_Full_ORD_GWCS_Forecast(con, "Observation_ID", Segments, LP, Time_Key = "Prediction_Time")

# Get the "New" Aircraft Profile, IAS Profile and GSPD Profile for ORD.
ACProfile <- Generate_ORD_Aircraft_Profile(con, LP_Primary_Key, LP, ORDBuffers = F, UseEFDDNew, Full_Level_Precedence, ORD_Levels, LegacyorRecat = "Recat")
IASProfile <- Generate_ORD_IAS_Profile(con, LP_Primary_Key, ACProfile, LP, GWCS_Forecast)
GSProfile <- Generate_ORD_GSPD_Profile(con, LP_Primary_Key, IASProfile, GWCS_Forecast, Seg_Size)

# Get the "Old" Aircraft Profile, IAS Profile and GSPD Profile for ORD.
ACProfileLegacy <- Generate_ORD_Aircraft_Profile(con, LP_Primary_Key, LP, ORDBuffers = F, UseEFDDOld, Full_Level_Precedence, ORD_Levels_Legacy, LegacyorRecat = "Legacy")
IASProfileLegacy <- Generate_ORD_IAS_Profile(con, LP_Primary_Key, ACProfileLegacy, LP, GWCS_Forecast)
GSProfileLegacy <- Generate_ORD_GSPD_Profile(con, LP_Primary_Key, IASProfileLegacy, GWCS_Forecast, Seg_Size)

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 4. More ORD Reference Data ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Gets the Times, Distances and related parameters for ORD elements and Fixed. 
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# Attach the delivery points to the LP data.
LP <- LP %>%
  mutate(New_Delivery = New_Delivery_Point,
         Old_Delivery = Old_Delivery_Point)

# Get the Compression Commencement Threshold Distance/RTT/Time Values. (Landing Pair Level)
LP <- LP %>%
  Get_CCT_Distances(ACProfile, LP_Primary_Key, LegacyorRecat = "Recat") %>%
  Get_CCT_Distances(ACProfileLegacy, LP_Primary_Key, LegacyorRecat = "Legacy") %>%
  Get_CCT_RTTs(Radar, "Recat") %>%
  Get_CCT_RTTs(Radar, "Legacy") %>%
  Get_CC_RTTs("Recat") %>%
  Get_CC_RTTs("Legacy") %>%
  Get_CC_Distances("Recat") %>%
  Get_CC_Distances("Legacy")

# Get the Delivery and FAF Distance/RTTs (Aircraft Level)
FP_Original <- select(FP, Flight_Plan_ID)
FP <- FP %>%
  Get_FAF_Distances(ORDRunways) %>%
  Get_FAF_RTTs(Radar, N = 1) %>%
  Get_FAF_RTTs(Radar, N = 2) %>%
  Get_Delivery_RTTs(Radar, New_Delivery_Point, "Recat") %>%
  Get_Delivery_RTTs(Radar, Old_Delivery_Point, "Legacy") %>%
  select(-c(FP_Date, FP_Time, Aircraft_Type, Time_At_4DME, Callsign, Landing_Runway)) %>%
  rename(Leader_FAF_Distance = FAF_Distance)

# Rename for Follower parameters.
FP2 <- select(FP, Flight_Plan_ID, Follower_FAF_RTT = Leader_FAF_2_RTT, Follower_FAF_Time = Leader_FAF_2_Time,
              Recat_Follower_Delivery_RTT = Recat_Leader_Delivery_RTT, Recat_Follower_Delivery_Time = Recat_Leader_Delivery_Time,
              Legacy_Follower_Delivery_RTT = Legacy_Leader_Delivery_RTT, Legacy_Follower_Delivery_Time = Legacy_Leader_Delivery_Time)

# Get the fixed RTT/Times for 0DME, 1DME and 4DME for legacy IA.
FP1 <- FP_Original %>%
  Get_Fixed_RTTs(Radar, DME = 0) %>%
  Get_Fixed_RTTs(Radar, DME = 1) %>%
  Get_Fixed_RTTs(Radar, DME = 4)

# Join on the Leader FAF and Delivery Parameters.
LP <- left_join(LP, FP, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))

# Rename for the Leader fixed RTTs.
FP1a <- FP1 %>% 
  rename(Leader_0DME_RTT = `0DME_RTT`, Leader_0DME_Time = `0DME_Time`,
         Leader_1DME_RTT = `1DME_RTT`, Leader_1DME_Time = `1DME_Time`,
         Leader_4DME_RTT = `4DME_RTT`, Leader_4DME_Time = `4DME_Time`)

# Rename for the Follower fixed RTTs.
FP1b <- FP1 %>%
  rename(Follower_0DME_RTT = `0DME_RTT`, Follower_0DME_Time = `0DME_Time`,
         Follower_1DME_RTT = `1DME_RTT`, Follower_1DME_Time = `1DME_Time`,
         Follower_4DME_RTT = `4DME_RTT`, Follower_4DME_Time = `4DME_Time`)

# Join on the Leader/Follower Fixed RTTs and the Follower FAF/Delivery parameters.
LP <- LP %>%
  left_join(FP1a, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
  left_join(FP1b, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
  left_join(FP2, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))

# Get the Follower Interpolated RTTs at the Leader 0ME, 1DME and 4DME times for Legacy IA.
for (Trail in c("Not_In_Trail", "In_Trail", "In_Trail_Non_Sequential")){
  
  LP_Trail <- filter(LP, Landing_Pair_Type == Trail)
  
  DME0 <- Get_Follower_Interpolated_RTTs(LP_Trail, Radar, Leader_Time_Var = "Leader_0DME_Time") %>% rename(Follower_Int_0DME_RTT = Interp_Distance)
  DME1 <- Get_Follower_Interpolated_RTTs(LP_Trail, Radar, Leader_Time_Var = "Leader_1DME_Time") %>% rename(Follower_Int_1DME_RTT = Interp_Distance)
  DME4 <- Get_Follower_Interpolated_RTTs(LP_Trail, Radar, Leader_Time_Var = "Leader_4DME_Time") %>% rename(Follower_Int_4DME_RTT = Interp_Distance)
  
  LP_Trail <- LP_Trail %>%
    left_join(DME0, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
    left_join(DME1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
    left_join(DME4, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  if (exists("LP_Full")){LP_Full <- rbind(LP_Full, LP_Trail)} else {LP_Full <- LP_Trail}
  
}

LP <- LP_Full
rm(LP_Full, DME0, DME1, DME4)

# Calculate the Observed Separation Distance/Times at 0DME, 1DME and 4DME.
LP <- LP %>%
  mutate(Observed_0DME_Separation_Distance = Follower_Int_0DME_RTT - Leader_0DME_RTT,
         Observed_1DME_Separation_Distance = Follower_Int_1DME_RTT - Leader_1DME_RTT,
         Observed_4DME_Separation_Distance = Follower_Int_4DME_RTT - Leader_4DME_RTT,
         Observed_0DME_Separation_Time = Follower_0DME_Time - Leader_0DME_Time,
         Observed_1DME_Separation_Time = Follower_1DME_Time - Leader_1DME_Time,
         Observed_4DME_Separation_Time = Follower_4DME_Time - Leader_4DME_Time)

# Calculate the Observed Leader Flying Times for Observed ORD Type 1.
LP <- LP %>%
  mutate(Recat_Observed_Leader_JustORD_Flying_Distance = Leader_FAF_1_RTT - Recat_Leader_Delivery_RTT,
         Recat_Observed_Leader_WAD_Flying_Distance = Recat_Leader_CC_RTT - Leader_FAF_2_RTT,
         Recat_Observed_Leader_ORD_Flying_Distance = Recat_Leader_CC_RTT - Recat_Leader_Delivery_RTT,
         Legacy_Observed_Leader_JustORD_Flying_Distance = Leader_FAF_1_RTT - Legacy_Leader_Delivery_RTT,
         Legacy_Observed_Leader_WAD_Flying_Distance = Legacy_Leader_CC_RTT - Leader_FAF_2_RTT,
         Legacy_Observed_Leader_ORD_Flying_Distance = Legacy_Leader_CC_RTT - Legacy_Leader_Delivery_RTT)

LPOrig <- LP

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 5. All ORD and TBS Calculations ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# For both RECAT/LEGACY, Calculate the TBS Distances, Observed and Forecast ORD Metrics. Calculate the Actual/Perfect Speeds/Time Spacings.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# LP <- LPOrig
# LegacyorRecat <- "Recat"

for (LegacyorRecat in c("Recat", "Legacy")){
  
  All_Dist_Values <- c()
  
  if (LegacyorRecat == "Legacy"){
    Delivery_Column <- "Old_Delivery"
    Use_EFDD <- UseEFDDOld
    Wake_Levels_Used <- TBS_Wake_Levels_Legacy
    ROT_Levels_Used <- TBS_ROT_Levels_Legacy
    TTB_Type <- TTBTypeOld
    Constraints <- Old_Constraints
    ORD_GS_Profile <- GSProfileLegacy
    ORD_AC_Profile <- ACProfileLegacy
    Forecast_Compression_Type = ForecastCompressionTypeOld
    Observed_Compression_Type = ObservedCompressionTypeOld
    Use_Adjustment <- UseAdjustmentOld
    TimeorRange <- TimeorRangeOld 
    AdjustWEs <- AdjustWEsOld
    MaxRangeToILS <- MaxRangeToILSOld
    AllowedPathLegs <- AllowedPathLegsOld
    MaxUnderRep <- MaxUnderRepOld
    UnderSeps <- UnderSepsOld
  } else {
      Delivery_Column <- "New_Delivery"
      Use_EFDD <- UseEFDDNew
      Wake_Levels_Used <- TBS_Wake_Levels
      ROT_Levels_Used <- TBS_ROT_Levels
      TTB_Type <- TTBTypeNew
      Constraints <- New_Constraints
      ORD_GS_Profile <- GSProfile
      ORD_AC_Profile <- ACProfile
      Forecast_Compression_Type = ForecastCompressionTypeNew
      Observed_Compression_Type = ObservedCompressionTypeNew
      Use_Adjustment <- UseAdjustmentNew
      TimeorRange <- TimeorRangeNew
      AdjustWEs <- AdjustWEsNew
      MaxRangeToILS <- MaxRangeToILSNew
      AllowedPathLegs <- AllowedPathLegsNew
      MaxUnderRep <- MaxUnderRepNew
      UnderSeps <- UnderSepsNew
    }
  
  Dist_Values <- c()
  
  # Setup the Profiles for Wake and ROT Constraints.
  TBSC_Profiles_Wake <- Generate_TBSC_Profiles(con, LP, GWCS_Forecast, LP_Primary_Key, TTB_Type, Use_EFDD, Full_Level_Precedence, Wake_Levels_Used, LegacyorRecat)
  TBSC_Profiles_ROT <- Generate_TBSC_Profiles(con, LP, GWCS_Forecast, LP_Primary_Key, TTB_Type, Use_EFDD, Full_Level_Precedence, ROT_Levels_Used, LegacyorRecat)
  
  LP <- mutate(LP, Delivery_Distance = !!sym(Delivery_Column))
  
  ## Generate the Distance for all Constraints at Threshold
  for (Constraint in Constraints){
    if (Constraint == "ROT"){TBSC_Profiles <- TBSC_Profiles_ROT} else {TBSC_Profiles <- TBSC_Profiles_Wake}
    Constraint_Delivery <- filter(Delivery_Points, SASAI_Constraint == Constraint) %>% select(!!sym(Delivery_Column)) %>% as.character()
    LP <- Generate_Constraint_Spacing_All(con, LPR = LP, TBSC_Profile = TBSC_Profiles,
                                                    TTB_Type, Constraint_Type = Constraint,
                                                    Constraint_Delivery, Evaluated_Delivery = "THRESHOLD",
                                                    ID_Var = LP_Primary_Key,
                                                    Time_Var = Get_Reference_Variable_Name(Constraint, "Time", LegacyorRecat),
                                                    Speed_Var = Get_Reference_Variable_Name(Constraint, "IAS", LegacyorRecat),
                                                    Seg_Size, LegacyorRecat)
    Dist_Name_Var <- paste0(Get_Constraint_Prefix(Constraint, "THRESHOLD", "TBS", LegacyorRecat), "_Distance")
    if (Dist_Name_Var %in% names(LP)){
      Dist_Values <- append(Dist_Values, Dist_Name_Var)
    }
    rm(Dist_Name_Var)
  }
  
  All_Sep_Var <- paste0(LegacyorRecat, "_Threshold_All_Separation_Distance")
  Thresh_Max_Constraint_Var <- paste0(LegacyorRecat, "_Threshold_Max_Constraint")
  
  LP <- LP %>%
    mutate(!!sym(All_Sep_Var) := pmax(!!!syms(Dist_Values), na.rm=T))
  
  # Get the Max Constraint (Threshold)
  LP <- mutate(LP, !!sym(Thresh_Max_Constraint_Var) := NA)
  for (c in 1:length(Constraints)){
    Dist_Name_Var <- paste0(Get_Constraint_Prefix(Constraints[c], "THRESHOLD", "TBS", LegacyorRecat), "_Distance")
    if (Dist_Name_Var %in% Dist_Values){
      LP <- LP %>% 
        mutate(!!sym(Thresh_Max_Constraint_Var) := ifelse(!is.na(!!sym(Dist_Name_Var)) & abs(!!sym(Dist_Name_Var) - !!sym(All_Sep_Var)) < 0.01, Constraints[c], !!sym(Thresh_Max_Constraint_Var)))
    }
  } 
  
  rm(Dist_Name_Var)
  
  # Get the "Winning" Assumed IAS and Forecast WE
  Wake_Prefix <- Get_Constraint_Prefix("Wake", "THRESHOLD", "TBS", LegacyorRecat)
  ROT_Prefix <- Get_Constraint_Prefix("ROT", "THRESHOLD", "TBS", LegacyorRecat)
  LP <- LP %>%
    mutate(!!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS")) := NA,
           !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect")) := NA,
           !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "Wake", !!sym(paste0(Wake_Prefix, "_IAS")), !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS"))),
           !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "ROT", !!sym(paste0(ROT_Prefix, "_IAS")), !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS"))),
           !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "Wake", !!sym(paste0(Wake_Prefix, "_Wind_Effect")), !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect"))),
           !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "ROT", !!sym(paste0(ROT_Prefix, "_Wind_Effect")), !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect"))))
  
  Dist_Values <- append(Dist_Values, All_Sep_Var)
  All_Dist_Values <- append(All_Dist_Values, Dist_Values)
  
  # The whole of ORD is in this one line!!
  LP <- Generate_Forecast_ORD_Results(LP, ORD_GS_Profile, LP_Primary_Key, Forecast_Compression_Type, LegacyorRecat)
  
  # Now get the Distances + Compression (If viable!)
  Dist_Values <- c()
  for (Constraint in Constraints){
    if (Constraint == "ROT"){TBSC_Profiles <- TBSC_Profiles_ROT} else {TBSC_Profiles <- TBSC_Profiles_Wake}
    Constraint_Delivery <- filter(Delivery_Points, SASAI_Constraint == Constraint) %>% select(!!sym(Delivery_Column)) %>% as.character()
    LP <- Generate_Constraint_Spacing_All(con, LPR = LP, TBSC_Profile = TBSC_Profiles,
                                                    TTB_Type, Constraint_Type = Constraint,
                                                    Constraint_Delivery, Evaluated_Delivery = "FAF",
                                                    ID_Var = LP_Primary_Key,
                                                    Time_Var = Get_Reference_Variable_Name(Constraint, "Time", LegacyorRecat),
                                                    Speed_Var = Get_Reference_Variable_Name(Constraint, "IAS", LegacyorRecat),
                                                    Seg_Size, LegacyorRecat)
    if (paste0(Get_Constraint_Prefix(Constraint, "FAF", "TBS", LegacyorRecat), "_Distance") %in% names(LP)){
      Dist_Values <- append(Dist_Values, paste0(Get_Constraint_Prefix(Constraint, "FAF", "TBS", LegacyorRecat), "_Distance"))
    }
  }
  
  ## Get the Maximum Distance FAF Constraint
  All_Sep_Var <- paste0(LegacyorRecat, "_FAF_All_Separation_Distance")
  FAF_Max_Constraint_Var <- paste0(LegacyorRecat, "_FAF_Max_Constraint")
  LP <- LP %>%
    mutate(!!sym(All_Sep_Var) := pmax(!!!syms(Dist_Values), na.rm=T))
  
  Dist_Values <- append(Dist_Values, All_Sep_Var)
  All_Dist_Values <- append(All_Dist_Values, Dist_Values)
  
  # Get the Max Constraint (FAF)
  LP <- mutate(LP, !!sym(FAF_Max_Constraint_Var) := NA)
  for (c in 1:length(Constraints)){
    Dist_Name_Var <- paste0(Get_Constraint_Prefix(Constraints[c], "FAF", "TBS", LegacyorRecat), "_Distance")
    if (Dist_Name_Var %in% Dist_Values){
      LP <- LP %>% 
        mutate(!!sym(FAF_Max_Constraint_Var) := ifelse(!is.na(!!sym(Dist_Name_Var)) & !!sym(Dist_Name_Var) == !!sym(All_Sep_Var), Constraints[c], !!sym(FAF_Max_Constraint_Var)))
    }
    
  }
  rm(Dist_Name_Var)
  
  # All Observed ORD Calculations
  LP <- Generate_Observed_ORD_Results(LP, Radar, ORD_AC_Profile, TimeorRange, Metric_Type, Use_Adjustment, LegacyorRecat, AdjustWEs, MaxRangeToILS, AllowedPathLegs, MaxUnderRep)
  
  # Get ORD Errors
  LP <- Get_ORD_Error_Variables(LP, LegacyorRecat)
  
  # Some Separation Accuracy Stuff 
  Act_Dist_Values <- All_Dist_Values
  
  # Hardcoded Distances to speed things up
  All_Dist_Values <- c(paste0(LegacyorRecat, "_Threshold_Wake_Separation_Distance"), paste0(LegacyorRecat, "_Threshold_All_Separation_Distance"))
  Act_Dist_Values <- c(paste0("Actual_", LegacyorRecat, "_FAF_Wake_Separation_Distance"), paste0("Actual_", LegacyorRecat, "_FAF_All_Separation_Distance"))
  
  # Hardcoded Separation Accuracy for now
  if (LegacyorRecat == "Legacy"){LP <- mutate(LP, Observed_4DME_Separation_Accuracy = Observed_4DME_Separation_Distance - Legacy_FAF_Wake_Separation_Distance)}
  RSA <- 0.2 * NM_to_m
  Prototyping <- T
  LP <- LP %>% mutate(!!sym(paste0(LegacyorRecat, "_Required_Separation_Accuracy")) := ifelse(!Prototyping & LegacyorRecat == "Legacy", Observed_4DME_Separation_Accuracy, RSA))
  LP <- LP %>% mutate(!!sym(paste0("Actual_", LegacyorRecat, "_FAF_Wake_Separation_Distance")) := !!sym(paste0(LegacyorRecat, "_FAF_Wake_Separation_Distance")) + !!sym(paste0(LegacyorRecat, "_Required_Separation_Accuracy")))
  LP <- LP %>% mutate(!!sym(paste0("Actual_", LegacyorRecat, "_FAF_All_Separation_Distance")) := !!sym(paste0(LegacyorRecat, "_FAF_All_Separation_Distance")) + !!sym(paste0(LegacyorRecat, "_Required_Separation_Accuracy")))
  
  # Get Observed Speed/IAS
  for (Trail in c("Not_In_Trail", "In_Trail", "In_Trail_Non_Sequential")){
  
    #Trail <- "Not_In_Trail"
    LP_Trail <- filter(LP, Landing_Pair_Type == Trail)
    
    for (UnderSep in UnderSeps){
      
      USString <- ""
      if (UnderSep != 0){USString <- paste0("_US", UnderSep / NM_to_m)}
      
      All_PM_Values_Perf <- paste0("Observed_Perfect_", gsub("_Distance", USString, All_Dist_Values))
      All_Time_Values_Perf <- paste0("Perfect_", gsub("_Distance", paste0(USString, "_Time"), All_Dist_Values))
      All_PM_Values_Act <- paste0("Observed_Foll_", gsub("_Distance", USString, Act_Dist_Values))
      All_Time_Values_Act <- paste0("", gsub("_Distance", paste0(USString, "_Time"), Act_Dist_Values))
      
      for (i in 1:length(All_Dist_Values)){
        
        Time_Var <- All_Time_Values_Perf[i]
        IAS_Var <- paste0(All_PM_Values_Perf[i], "_IAS")
        WE_Var <- paste0(All_PM_Values_Perf[i], "_Wind_Effect")
        Dist_Var <- All_Dist_Values[i]
        
        LP_Trail <- mutate(LP_Trail, Start_Var := !!sym(Delivery_Column) + !!sym(All_Dist_Values[i]) - UnderSep, End_Var := !!sym(Delivery_Column))
        
        LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                             Prefix = "NULL",
                                                             LorF = "Follower",
                                                             TimeorRange = "Range",
                                                             Start_Var = "Start_Var",
                                                             End_Var = "End_Var",
                                                             LegacyorRecat,
                                                             MaxUnderRep,
                                                             LP_Primary_Key,
                                                             All_PM_Values_Perf[i])
        
        LP_Trail <- mutate(LP_Trail, !!sym(Time_Var) := (!!sym(Dist_Var) - UnderSep) / (!!sym(IAS_Var) + !!sym(WE_Var)))
        
      }
      
      for (i in 1:length(Act_Dist_Values)){
        
        Time_Var <- All_Time_Values_Act[i]
        Foll_IAS_Var <- paste0(All_PM_Values_Act[i], "_IAS")
        Foll_WE_Var <- paste0(All_PM_Values_Act[i], "_Wind_Effect")
        Foll_Dist_Var <- Act_Dist_Values[i]
        Lead_IAS_Var <- paste0(LegacyorRecat, "_Observed_Leader_ORD_IAS")
        Lead_WE_Var <- paste0(LegacyorRecat, "_Observed_Leader_ORD_Wind_Effect")
        Lead_Dist_Var <- paste0(LegacyorRecat, "_Observed_Leader_ORD_Flying_Distance")
        
        LP_Trail <- mutate(LP_Trail, Start_Var := !!sym(paste0(LegacyorRecat, "_Leader_CC_Distance")) + !!sym(Act_Dist_Values[i]) - UnderSep, End_Var := !!sym(Delivery_Column))
        
        LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                             Prefix = "NULL",
                                                             LorF = "Follower",
                                                             TimeorRange = "Range",
                                                             Start_Var = "Start_Var",
                                                             End_Var = "End_Var",
                                                             LegacyorRecat,
                                                             MaxUnderRep,
                                                             LP_Primary_Key,
                                                             All_PM_Values_Act[i])
        
        LP_Trail <- LP_Trail %>%
          mutate(!!sym(Time_Var) := ((!!sym(paste0(LegacyorRecat, "_Leader_CC_Distance")) + !!sym(Act_Dist_Values[i]) - UnderSep) /
                                       (!!sym(Foll_IAS_Var) + !!sym(Foll_WE_Var))) - (!!sym(Lead_Dist_Var) / (!!sym(Lead_IAS_Var) + !!sym(Lead_WE_Var))))
        
      }
      
      
      
      
    }
    
    if (!exists("LP_New")){LP_New <- LP_Trail} else {LP_New <- rbind(LP_New, LP_Trail)}
      
  }
  
  LP <- LP_New
  rm(LP_New)
    
    
}

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 6. Final Data Extras ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Last extras to be added on - Observed Leader/Follower Surface Wind.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

LP <- Get_Surface_Wind(LP, SW, Runways,
                       Prefix = "Observed_AGI_Follower",
                       ID_Var = "Observation_ID",
                       Date_Var = "Landing_Pair_Date",
                       Time_Var = "Follower_0DME_Time",
                       Runway_Var = "Follower_Landing_Runway")

LP <- Get_Surface_Wind(LP, SW, Runways,
                       Prefix = "Observed_AGI",
                       ID_Var = "Observation_ID",
                       Date_Var = "Landing_Pair_Date",
                       Time_Var = "Recat_Leader_Delivery_Time",
                       Runway_Var = "Leader_Landing_Runway")

LP <- mutate(LP, Forecast_AGI_Surface_Headwind_Error = Observed_AGI_Surface_Headwind - Forecast_AGI_Surface_Headwind)

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 7. Table Construction & Population ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Select relevant data for the SQL tables and populate if not in testing mode.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# Get the ORiginal only results for ORD tables
LP_ORD <- filter(LP, Fake_Observation == 0) %>% arrange(Landing_Pair_ID)

# Get the full list of LPIDs to attach to the profile tables
LP_IDs <- select(LP, Observation_ID, Landing_Pair_ID)

## EXISTING TABLES
ORD_Observation <- LP_ORD %>%
  select(Landing_Pair_ID,
         Observed_Compression = Recat_Observed_JustORD_Compression,
         Observed_Mean_Leader_IAS = Recat_Observed_Leader_JustORD_IAS,
         Observed_Mean_Follower_IAS = Recat_Observed_Follower_JustORD_IAS,
         Observed_Mean_Leader_Wind_Effect = Recat_Observed_Leader_JustORD_Wind_Effect,
         Observed_Mean_Follower_Wind_Effect = Recat_Observed_Follower_JustORD_Wind_Effect,
         Observed_AGI_Surface_Headwind,
         Observation_Date = Landing_Pair_Date,
         Leader_FAF_Time = Leader_FAF_1_Time,
         Leader_0DME_Time = Recat_Leader_Delivery_Time,
         Leader_FAF_RTT = Leader_FAF_1_RTT,
         Leader_0DME_RTT = Recat_Leader_Delivery_RTT,
         Follower_Start_RTT = Recat_Follower_Int_FAF_1_RTT,
         Follower_Stop_RTT = Recat_Follower_Int_Delivery_RTT,
         Leader_Callsign,
         Follower_Callsign,
         Follower_0DME_Time = Recat_Follower_Delivery_RTT,
         Follower_0DME_RTT = Recat_Follower_Delivery_Time,
         Follower_Time_At_4DME,
         Landing_Runway = Leader_Landing_Runway,
         Delivered_FAF_Separation = Recat_Observed_FAF_Separation_Distance)

ORD_Prediction <- LP_ORD %>%
  select(Landing_Pair_ID,
         ORD_Compression = Recat_Forecast_JustORD_Compression,
         ORD_Compression_Error = Recat_JustORD_Compression_Error,
         ORD_Mean_Leader_IAS = Recat_Forecast_Leader_JustORD_IAS,
         ORD_Leader_IAS_Error = Recat_Forecast_Leader_JustORD_IAS_Error,
         ORD_Mean_Follower_IAS = Recat_Forecast_Follower_JustORD_IAS,
         ORD_Follower_IAS_Error = Recat_Forecast_Follower_JustORD_IAS_Error,
         Forecast_Mean_Leader_Wind_Effect = Recat_Forecast_Leader_JustORD_Wind_Effect,
         Forecast_Mean_Leader_Wind_Effect_Error = Recat_Forecast_Leader_JustORD_Wind_Effect_Error,
         Forecast_Mean_Follower_Wind_Effect = Recat_Forecast_Follower_JustORD_Wind_Effect,
         Forecast_Mean_Follower_Wind_Effect_Error = Recat_Forecast_Follower_JustORD_Wind_Effect_Error,
         Forecast_AGI_Surface_Headwind,
         Forecast_AGI_Surface_Headwind_Error,
         Prediction_Time,
         Leader_Distance_To_Threshold = Leader_FAF_Distance,
         ORD_Separation_Distance = Recat_ORD_Separation_Distance,
         DBS_All_Sep_Distance = Recat_DBS_All_Sep_Distance)

ORD_Observation_Legacy <- LP_ORD %>%
  select(Landing_Pair_ID,
         Observed_Compression = Legacy_Observed_JustORD_Compression,
         Observed_Mean_Leader_IAS = Legacy_Observed_Leader_JustORD_IAS,
         Observed_Mean_Follower_IAS = Legacy_Observed_Follower_JustORD_IAS,
         Observed_Mean_Leader_Wind_Effect = Legacy_Observed_Leader_JustORD_Wind_Effect,
         Observed_Mean_Follower_Wind_Effect = Legacy_Observed_Follower_JustORD_Wind_Effect,
         Observed_AGI_Surface_Headwind,
         Observation_Date = Landing_Pair_Date,
         Leader_FAF_Time = Leader_FAF_1_Time,
         Leader_0DME_Time = Legacy_Leader_Delivery_Time,
         Leader_FAF_RTT = Leader_FAF_1_RTT,
         Leader_0DME_RTT = Legacy_Leader_Delivery_RTT,
         Follower_Start_RTT = Legacy_Follower_Int_FAF_1_RTT,
         Follower_Stop_RTT = Legacy_Follower_Int_Delivery_RTT,
         Leader_Callsign,
         Follower_Callsign,
         Follower_0DME_Time = Legacy_Follower_Delivery_RTT,
         Follower_0DME_RTT = Legacy_Follower_Delivery_Time,
         Follower_Time_At_4DME,
         Landing_Runway = Leader_Landing_Runway,
         Delivered_FAF_Separation = Legacy_Observed_FAF_Separation_Distance)

ORD_Prediction_Legacy <- LP_ORD %>%
  select(Landing_Pair_ID,
         ORD_Compression = Legacy_Forecast_JustORD_Compression,
         ORD_Compression_Error = Legacy_JustORD_Compression_Error,
         ORD_Mean_Leader_IAS = Legacy_Forecast_Leader_JustORD_IAS,
         ORD_Leader_IAS_Error = Legacy_Forecast_Leader_JustORD_IAS_Error,
         ORD_Mean_Follower_IAS = Legacy_Forecast_Follower_JustORD_IAS,
         ORD_Follower_IAS_Error = Legacy_Forecast_Follower_JustORD_IAS_Error,
         Forecast_Mean_Leader_Wind_Effect = Legacy_Forecast_Leader_JustORD_Wind_Effect,
         Forecast_Mean_Leader_Wind_Effect_Error = Legacy_Forecast_Leader_JustORD_Wind_Effect_Error,
         Forecast_Mean_Follower_Wind_Effect = Legacy_Forecast_Follower_JustORD_Wind_Effect,
         Forecast_Mean_Follower_Wind_Effect_Error = Legacy_Forecast_Follower_JustORD_Wind_Effect_Error,
         Forecast_AGI_Surface_Headwind,
         Forecast_AGI_Surface_Headwind_Error,
         Prediction_Time,
         Leader_Distance_To_Threshold = Leader_FAF_Distance,
         ORD_Separation_Distance = Legacy_ORD_Separation_Distance,
         DBS_All_Sep_Distance = Legacy_DBS_All_Sep_Distance)

WAD_Observation <- LP_ORD %>%
  select(Landing_Pair_ID,
         Observed_Compression = Recat_Observed_WAD_Compression,
         Observed_Mean_Leader_IAS = Recat_Observed_Leader_WAD_IAS,
         Observed_Mean_Follower_IAS = Recat_Observed_Follower_WAD_IAS,
         Observed_Mean_Leader_Wind_Effect = Recat_Observed_Leader_WAD_Wind_Effect,
         Observed_Mean_Follower_Wind_Effect = Recat_Observed_Follower_WAD_Wind_Effect,
         Observed_AGI_Surface_Headwind,
         Observation_Date = Landing_Pair_Date,
         Leader_CCT_Time = Recat_Leader_CC_Time,
         Leader_FAF_Time = Leader_FAF_2_Time,
         Leader_CCT_RTT = Recat_Leader_CC_RTT,
         Leader_FAF_RTT = Leader_FAF_2_RTT,
         Follower_Start_RTT = Recat_Follower_Int_CC_RTT,
         Follower_Stop_RTT = Recat_Follower_Int_FAF_2_RTT,
         Leader_Callsign,
         Follower_Callsign,
         Follower_FAF_Time,
         Follower_FAF_RTT,
         Follower_Time_At_4DME,
         Landing_Runway = Leader_Landing_Runway,
         Delivered_CCT_Separation = Recat_Observed_CC_Separation_Distance)

WAD_Prediction <- LP_ORD %>%
  select(Landing_Pair_ID,
         WAD_Compression = Recat_Forecast_WAD_Compression,
         WAD_Compression_Error = Recat_WAD_Compression_Error,
         WAD_Mean_Leader_IAS = Recat_Forecast_Leader_WAD_IAS,
         WAD_Leader_IAS_Error = Recat_Forecast_Leader_WAD_IAS_Error,
         WAD_Mean_Follower_IAS = Recat_Forecast_Follower_WAD_IAS,
         WAD_Follower_IAS_Error = Recat_Forecast_Follower_WAD_IAS_Error,
         Forecast_Mean_Leader_Wind_Effect = Recat_Forecast_Leader_WAD_Wind_Effect,
         Forecast_Mean_Leader_Wind_Effect_Error = Recat_Forecast_Leader_WAD_Wind_Effect_Error,
         Forecast_Mean_Follower_Wind_Effect = Recat_Forecast_Follower_WAD_Wind_Effect,
         Forecast_Mean_Follower_Wind_Effect_Error = Recat_Forecast_Follower_WAD_Wind_Effect_Error,
         Forecast_AGI_Surface_Headwind,
         Forecast_AGI_Surface_Headwind_Error,
         Prediction_Time,
         Leader_Distance_To_Threshold = Recat_Leader_CC_Distance,
         WAD_Separation_Distance = Recat_WAD_Separation_Distance,
         DBS_All_Sep_Distance = Recat_DBS_All_Sep_Distance)


Aircraft_Profile <- left_join(ACProfile, LP_IDs, by = c("Observation_ID")) %>%
  filter(Observation_ID %in% LP_ORD$Observation_ID) %>%
  select(Landing_Pair_ID,
         This_Pair_Role,
         Aircraft_Type,
         Operator,
         Wake_Cat,
         Runway = Landing_Runway,
         VRef,
         Apply_Gusting,
         Landing_Stabilisation_Speed_Type,
         Local_Stabilisation_Distance,
         Compression_Commencement_Threshold,
         Landing_Stabilisation_Speed,
         Final_Deceleration,
         End_Final_Deceleration_Distance,
         Start_Final_Deceleration_Distance,
         Steady_Procedural_Speed,
         Initial_Deceleration,
         End_Initial_Deceleration_Distance,
         Start_Initial_Deceleration_Distance,
         Initial_Procedural_Speed)

Aircraft_Profile_Legacy <- left_join(ACProfileLegacy, LP_IDs, by = c("Observation_ID")) %>%
  filter(Observation_ID %in% LP_ORD$Observation_ID) %>%
  select(Landing_Pair_ID,
         This_Pair_Role,
         Aircraft_Type,
         Operator,
         Wake_Cat,
         Runway = Landing_Runway,
         VRef,
         Apply_Gusting,
         Landing_Stabilisation_Speed_Type,
         Local_Stabilisation_Distance,
         Compression_Commencement_Threshold,
         Landing_Stabilisation_Speed,
         Final_Deceleration,
         End_Final_Deceleration_Distance,
         Start_Final_Deceleration_Distance,
         Steady_Procedural_Speed,
         Initial_Deceleration,
         End_Initial_Deceleration_Distance,
         Start_Initial_Deceleration_Distance,
         Initial_Procedural_Speed)

IAS_Profile <- left_join(IASProfile, LP_IDs, by = c("Observation_ID")) %>%
  filter(Observation_ID %in% LP_ORD$Observation_ID) %>%
  select(Landing_Pair_ID,
         This_Pair_Role,
         Section_Number,
         Profile_Section,
         Profile_Type,
         Start_IAS,
         End_IAS,
         Start_Dist,
         End_Dist)

IAS_Profile_Legacy <- left_join(IASProfileLegacy, LP_IDs, by = c("Observation_ID")) %>%
  filter(Observation_ID %in% LP_ORD$Observation_ID) %>%
  select(Landing_Pair_ID,
         This_Pair_Role,
         Section_Number,
         Profile_Section,
         Profile_Type,
         Start_IAS,
         End_IAS,
         Start_Dist,
         End_Dist)

GSPD_Profile <- left_join(GSProfile, LP_IDs, by = c("Observation_ID")) %>%
  filter(Observation_ID %in% LP_ORD$Observation_ID) %>%
  select(Landing_Pair_ID,
         This_Pair_Role,
         Section_Number,
         Profile_Section,
         Profile_Type,
         Start_IAS,
         End_IAS,
         Start_GS,
         End_GS,
         Start_Dist,
         End_Dist)

GSPD_Profile_Legacy <- left_join(GSProfileLegacy, LP_IDs, by = c("Observation_ID")) %>%
  filter(Observation_ID %in% LP_ORD$Observation_ID) %>%
  select(Landing_Pair_ID,
         This_Pair_Role,
         Section_Number,
         Profile_Section,
         Profile_Type,
         Start_IAS,
         End_IAS,
         Start_GS,
         End_GS,
         Start_Dist,
         End_Dist)

All_Pair_Reference_Data <- LP_ORD %>%
  select(Landing_Pair_ID,
         FP_Date = Landing_Pair_Date,
         Leader_Callsign,
         Leader_Aircraft_Type,
         Leader_UK_Wake_Cat = Leader_Legacy_Wake_Cat,
         Leader_Recat_Wake_Cat,
         Follower_Callsign,
         Follower_Aircraft_Type,
         Follower_UK_Wake_Cat = Follower_Legacy_Wake_Cat,
         Follower_Recat_Wake_Cat,
         UK6Cat_Separation_Distance = Reference_Legacy_Wake_Separation_Distance,
         UK6Cat_Separation_Time = Reference_Legacy_Wake_Separation_Time,
         Ref_Recat_Wake_Separation_Distance = Reference_Recat_Wake_Separation_Distance,
         Ref_ROT_Spacing_Distance = Reference_Recat_ROT_Spacing_Distance,
         Leader_4DME_Time,
         Follower_0DME_Time,
         Follower_0DME_RTT,
         Follower_Time_At_4DME,
         Observed_AGI_Surface_Headwind = Observed_AGI_Follower_Surface_Headwind,
         Observed_AGI_Surface_Wind_SPD = Observed_AGI_Follower_Surface_Wind_SPD,
         Observed_AGI_Surface_Wind_HDG = Observed_AGI_Follower_Surface_Wind_HDG,
         Delivered_4DME_Separation = Observed_4DME_Separation_Distance,
         Landing_Runway = Follower_Landing_Runway)

Old_Performance_Model <- LP_ORD %>%
  select(Landing_Pair_ID,
         FP_Date = Landing_Pair_Date,
         Leader_Callsign,
         Leader_Aircraft_Type,
         Leader_UK_Wake_Cat = Leader_Legacy_Wake_Cat,
         Leader_Recat_Wake_Cat,
         Follower_Callsign,
         Follower_Aircraft_Type,
         Follower_UK_Wake_Cat = Follower_Legacy_Wake_Cat,
         Follower_Recat_Wake_Cat,
         UK6Cat_Separation_Distance = Reference_Legacy_Wake_Separation_Distance,
         Ref_Recat_Wake_Separation_Distance = Reference_Recat_Wake_Separation_Distance,
         Ref_ROT_Spacing_Distance = Reference_Recat_ROT_Spacing_Distance,
         UK6Cat_Separation_Time = Reference_Legacy_Wake_Separation_Time,
         Ref_Recat_Wake_Separation_Time = Reference_Recat_Wake_Separation_Time,
         Ref_ROT_Spacing_Time = Reference_Recat_ROT_Spacing_Time,
         Follower_Ass_Recat_Separation_IAS = Reference_Recat_Wake_Separation_IAS,
         Follower_Ass_ROT_Spacing_IAS = Reference_Recat_ROT_Spacing_IAS,
         Follower_Ass_IAS = Recat_Follower_Assumed_IAS,
         Follower_Forecast_TBS_Wind_Effect = Legacy_Follower_Forecast_Wind_Effect,
         Follower_Forecast_eTBS_Wind_Effect = Recat_Follower_Forecast_Wind_Effect,
         Observed_AGI_Surface_Headwind = Observed_AGI_Follower_Surface_Headwind,
         Observed_AGI_Surface_Wind_SPD = Observed_AGI_Follower_Surface_Wind_SPD,
         Observed_AGI_Surface_Wind_HDG = Observed_AGI_Follower_Surface_Wind_HDG,
         UK6Cat_TBS_4DME_Wake_Separation_Distance = Legacy_FAF_Wake_Separation_Distance,
         Recat_eTBS_0DME_Wake_Separation_Distance = Recat_Threshold_Wake_Separation_Distance,
         Recat_eTBS_0DME_ROT_Spacing_Distance = Recat_Threshold_ROT_Spacing_Distance,
         Recat_eTBS_0DME_All_Separation_Distance = Recat_Threshold_All_Separation_Distance,
         Forecast_ORD_TBS_Compression = Legacy_Forecast_JustORD_Compression,
         Forecast_ORD_eTBS_Compression = Recat_Forecast_JustORD_Compression,
         Recat_eTBS_4DME_Wake_Separation_Distance = Recat_FAF_Wake_Separation_Distance,
         Recat_eTBS_4DME_ROT_Spacing_Distance = Recat_FAF_ROT_Spacing_Distance,
         Recat_eTBS_4DME_All_Separation_Distance = Recat_FAF_All_Separation_Distance,
         Leader_0DME_RTT,
         Observed_0DME_Separation_Distance,
         Observed_1DME_Separation_Distance,
         Observed_4DME_Separation_Distance,
         Observed_4DME_Separation_Accuracy,
         Leader_4DME_Time,
         Observed_0DME_Separation_Time,
         Observed_1DME_Separation_Time,
         Observed_4DME_Separation_Time,
         Observed_Follower_eTBS_IAS = Observed_Perfect_Recat_Threshold_All_Separation_IAS,
         Observed_Follower_TBS_Wind_Effect = Observed_Perfect_Legacy_Threshold_Wake_Separation_Wind_Effect,
         Observed_Follower_eTBS_Wind_Effect = Observed_Perfect_Recat_Threshold_All_Separation_Wind_Effect,
         Observed_Compression = Recat_Observed_JustORD_Compression)

## NEW TABLES

New_Performance_Model <- LP %>%
  select(Observation_ID,
         Landing_Pair_ID,
         Fake_Observation,
         FP_Date = Landing_Pair_Date,
         Leader_Callsign,
         Leader_Operator,
         Leader_Aircraft_Type,
         Leader_Legacy_Wake_Cat,
         Leader_Recat_Wake_Cat,
         Leader_Landing_Runway,
         Follower_Callsign,
         Follower_Operator,
         Follower_Aircraft_Type,
         Follower_Legacy_Wake_Cat,
         Follower_Recat_Wake_Cat,
         Follower_Landing_Runway,
         Reference_Legacy_Wake_Separation_Distance,
         Reference_Legacy_Wake_Separation_IAS,
         Reference_Legacy_Wake_Separation_Time,
         Reference_Recat_Wake_Separation_Distance,
         Reference_Recat_Wake_Separation_IAS,
         Reference_Recat_Wake_Separation_Time,
         Reference_Legacy_ROT_Spacing_Distance,
         Reference_Legacy_ROT_Spacing_IAS,
         Reference_Legacy_ROT_Spacing_Time,
         Reference_Recat_ROT_Spacing_Distance,
         Reference_Recat_ROT_Spacing_Time,
         Legacy_Threshold_Non_Wake_Separation_Distance,
         Legacy_Threshold_Runway_Dependent_Separation_Distance,
         Legacy_Threshold_Wake_Separation_Distance,
         Legacy_Threshold_Wake_Separation_IAS,
         Legacy_Threshold_Wake_Separation_Wind_Effect,
         Legacy_Threshold_ROT_Spacing_Distance,
         Legacy_Threshold_ROT_Spacing_IAS,
         Legacy_Threshold_ROT_Spacing_Wind_Effect,
         Legacy_Threshold_All_Separation_Distance,
         Legacy_Follower_Assumed_IAS,
         Legacy_Follower_Forecast_Wind_Effect,
         Legacy_Threshold_Max_Constraint,
         Recat_Threshold_Non_Wake_Separation_Distance,
         Recat_Threshold_Runway_Dependent_Separation_Distance,
         Recat_Threshold_Wake_Separation_Distance,
         Recat_Threshold_Wake_Separation_IAS,
         Recat_Threshold_Wake_Separation_Wind_Effect,
         Recat_Threshold_ROT_Spacing_Distance,
         Recat_Threshold_ROT_Spacing_IAS,
         Recat_Threshold_ROT_Spacing_Wind_Effect,
         Recat_Threshold_All_Separation_Distance,
         Recat_Follower_Assumed_IAS,
         Recat_Follower_Forecast_Wind_Effect,
         Recat_Threshold_Max_Constraint,
         Legacy_Forecast_Compression = Legacy_Forecast_JustORD_Compression,
         Legacy_FAF_Non_Wake_Separation_Distance,
         Legacy_FAF_Runway_Dependent_Separation_Distance,
         Legacy_FAF_Wake_Separation_Distance,
         Legacy_FAF_ROT_Spacing_Distance,
         Legacy_FAF_All_Separation_Distance,
         Legacy_FAF_Max_Constraint,
         Recat_Forecast_Compression = Recat_Forecast_JustORD_Compression,
         Recat_FAF_Non_Wake_Separation_Distance,
         Recat_FAF_Runway_Dependent_Separation_Distance,
         Recat_FAF_Wake_Separation_Distance,
         Recat_FAF_ROT_Spacing_Distance,
         Recat_FAF_All_Separation_Distance,
         Recat_FAF_Max_Constraint,
         Observed_0DME_Separation_Distance,
         Observed_1DME_Separation_Distance,
         Observed_4DME_Separation_Distance,
         Observed_0DME_Separation_Time,
         Observed_1DME_Separation_Time,
         Observed_4DME_Separation_Time,
         Legacy_Observed_Compression = Legacy_Observed_JustORD_Compression,
         Recat_Observed_Compression = Recat_Observed_JustORD_Compression,
         Observed_AGI_Surface_Headwind = Observed_AGI_Follower_Surface_Headwind,
         Observed_AGI_Surface_Wind_SPD = Observed_AGI_Follower_Surface_Wind_SPD,
         Observed_AGI_Surface_Wind_HDG = Observed_AGI_Follower_Surface_Wind_HDG,
         Observed_AGI_Surface_Crosswind = Observed_AGI_Follower_Surface_Crosswind,
         Legacy_Required_Separation_Accuracy,
         Recat_Required_Separation_Accuracy,
         Actual_Legacy_FAF_Wake_Separation_Distance,
         Actual_Legacy_FAF_All_Separation_Distance,
         Actual_Recat_FAF_Wake_Separation_Distance,
         Actual_Recat_FAF_All_Separation_Distance,
         Observed_Perfect_Recat_Threshold_Wake_Separation_IAS,
         Observed_Perfect_Recat_Threshold_Wake_Separation_Wind_Effect,
         Perfect_Recat_Threshold_Wake_Separation_Time,
         Perfect_Recat_Threshold_Wake_Separation_US0.5_Time,
         Perfect_Recat_Threshold_Wake_Separation_US1_Time,
         Observed_Perfect_Recat_Threshold_All_Separation_IAS,
         Observed_Perfect_Recat_Threshold_All_Separation_Wind_Effect,
         Perfect_Recat_Threshold_All_Separation_Time,
         Perfect_Recat_Threshold_All_Separation_US0.5_Time,
         Perfect_Recat_Threshold_All_Separation_US1_Time,
         Observed_Perfect_Legacy_Threshold_Wake_Separation_IAS,
         Observed_Perfect_Legacy_Threshold_Wake_Separation_Wind_Effect,
         Perfect_Legacy_Threshold_Wake_Separation_Time,
         Perfect_Legacy_Threshold_Wake_Separation_US0.5_Time,
         Perfect_Legacy_Threshold_Wake_Separation_US1_Time,
         Observed_Perfect_Legacy_Threshold_All_Separation_IAS,
         Observed_Perfect_Legacy_Threshold_All_Separation_Wind_Effect,
         Perfect_Legacy_Threshold_All_Separation_Time,
         Perfect_Legacy_Threshold_All_Separation_US0.5_Time,
         Perfect_Legacy_Threshold_All_Separation_US1_Time,
         Observed_Foll_Actual_Recat_FAF_Wake_Separation_IAS,
         Observed_Foll_Actual_Recat_FAF_Wake_Separation_Wind_Effect,
         Actual_Recat_FAF_Wake_Separation_Time,
         Actual_Recat_FAF_Wake_Separation_US0.5_Time,
         Actual_Recat_FAF_Wake_Separation_US1_Time,
         Observed_Foll_Actual_Recat_FAF_All_Separation_IAS,
         Observed_Foll_Actual_Recat_FAF_All_Separation_Wind_Effect,
         Actual_Recat_FAF_All_Separation_Time,
         Actual_Recat_FAF_All_Separation_US0.5_Time,
         Actual_Recat_FAF_All_Separation_US1_Time,
         Observed_Foll_Actual_Legacy_FAF_Wake_Separation_IAS,
         Observed_Foll_Actual_Legacy_FAF_Wake_Separation_Wind_Effect,
         Actual_Legacy_FAF_Wake_Separation_Time,
         Actual_Legacy_FAF_Wake_Separation_US0.5_Time,
         Actual_Legacy_FAF_Wake_Separation_US1_Time,
         Observed_Foll_Actual_Legacy_FAF_All_Separation_IAS,
         Observed_Foll_Actual_Legacy_FAF_All_Separation_Wind_Effect,
         Actual_Legacy_FAF_All_Separation_Time,
         Actual_Legacy_FAF_All_Separation_US0.5_Time,
         Actual_Legacy_FAF_All_Separation_US1_Time
         ) %>% select(-Observation_ID)


# Populate Tables.
if (!Testing){
  
  PopulateSQLTable(con, SQLTable = "tbl_ORD_Observation", Table = ORD_Observation) 
  PopulateSQLTable(con, SQLTable = "tbl_ORD_Prediction", Table = ORD_Prediction)
  PopulateSQLTable(con, SQLTable = "tbl_All_Pair_Reference_Data", Table = All_Pair_Reference_Data)
  PopulateSQLTable(con, SQLTable = "tbl_eTBS_Performance_Model", Table = Old_Performance_Model)
  
  PopulateSQLTable(con, SQLTable = "tbl_ORD_Aircraft_Profile", Table = Aircraft_Profile)
  PopulateSQLTable(con, SQLTable = "tbl_ORD_IAS_Profile", Table = IAS_Profile)
  PopulateSQLTable(con, SQLTable = "tbl_ORD_GS_Profile", Table = GSPD_Profile)
  
  if (Populate_Legacy){
    PopulateSQLTable(con, SQLTable = "tbl_ORD_Observation_Legacy", Table = ORD_Observation_Legacy) 
    PopulateSQLTable(con, SQLTable = "tbl_ORD_Prediction_Legacy", Table = ORD_Prediction_Legacy)
    PopulateSQLTable(con, SQLTable = "tbl_ORD_Aircraft_Profile_Legacy", Table = Aircraft_Profile_Legacy)
    PopulateSQLTable(con, SQLTable = "tbl_ORD_IAS_Profile_Legacy", Table = IAS_Profile_Legacy)
    PopulateSQLTable(con, SQLTable = "tbl_ORD_GS_Profile_Legacy", Table = GSPD_Profile_Legacy)
  }
  
  PopulateSQLTable(con, SQLTable = "tbl_WAD_Observation", Table = WAD_Observation)
  PopulateSQLTable(con, SQLTable = "tbl_WAD_Prediction", Table = WAD_Prediction)
  
  PopulateSQLTable(con, SQLTable = "tbl_IA_Performance_Model", Table = New_Performance_Model)
  
}         

}

Time2 <- Sys.time()
Time2-Time1

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 8. Sense Checking ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Temporary Section for result visualisation and sense checking.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

Analysis <- T
if (Analysis){
  
  
  
}




