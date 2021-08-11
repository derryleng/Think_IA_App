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
source(file.path(Script_Dir, "New TBS Generalisation.R"), local=T)
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Configuration (## TODO: SETUP FOR SHINY)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Database <- "EGLL_PWS"
IP <- "192.168.1.23"
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Database Connection
con <- Get_DBI_Connection(IP, Database)

# Start Time
Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# BEGIN
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Time1 <- Sys.time()
Testing <- T # If TRUE, Database is not altered.
PROC <- 1
PROC_Period <- c("Day", "Month", "All")[PROC]
PROC_Criteria <- c("01/06/2019", "06/2019", NA)[PROC]
Populate_Legacy <- T # Populate Legacy ORD Tables
Clear_All <- F
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# 0. Adaptation ----
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# These should be set in Adaptation Tables. But for easy prototyping, keep internal.
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# Are we joining on Gusting Data?
Join_Gusting <- T

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
# 0.1. Variant Based Adaptation
# ------------------------------------------------------------------------------------------------------------ #

# Thje names of the variants and their operations (operations not used yet)
Variants <- c("Recat", "Legacy")
VariantOps <- c("PWS", "eTBS")

# If adaptation doesn't exist for a variant, can it use a default variant's config? Where from?
DefaultVariant <- Variants[1]
UseDefaultVariantNonLocal <- c(F, F)
UseDefaultVariantLocal <- c(F, F)

# The Operation variant based output tables, and switches for population.
OpTables <- c("tbl_ORD_Observation", "tbl_ORD_Prediction", "tbl_ORD_Aircraft_Profile", "tbl_ORD_IAS_Profile", "tbl_ORD_GS_Profile",
              "tbl_WAD_Observation", "tbl_WAD_Prediction")

PopTablesList <- list(c(T, T, T, T, T, T, T), c(T, T, T, T, T, F, F))

# Local adaptation lists
WakeSchemesList <- list("RECAT-EU", "RECAT-EU")
ConstraintsList <- list(c("Wake", "ROT", "Non_Wake", "Runway_Dependent"), c("Wake", "ROT", "Non_Wake", "Runway_Dependent"))
ORDLevelsList <- list(c(F, T, T), c(F, T, T))
TBSWakeLevelsList <- list(c(F, T, T), c(F, T, T))
TBSROTLevelsList <- list(c(F, F, T), c(F, F, T))
SymmetryWakeList <- list(T, T)
ROTTimeTypeList <- list("ROT", "ROT")
TBSCBuffersList <- list(T, T)
UseEFDDList <- list(T, T)
TTBTypesList <- list("ORD", "ORD")
UseGustingDataList <- list(T, F)
DeliveryPointsList <- list(0*NM_to_m, 0*NM_to_m)
DeliveryColumnsList <- list("New_Delivery", "Old_Delivery")
ForecastCompressionTypeList <- list(2, 2)
ObservedCompressionTypeList <- list(2, 2)
UseAdjustmentList <- list(T, T)
AdjustWEList <- list(F, F)
MaxRangeToILSList <- list(3*NM_to_m, 3*NM_to_m)
AllowedPathLegsList <- list(c("Intercept", "Ext_Intercept", "ILS"), c("Intercept", "Ext_Intercept", "ILS"))
MaxUnderRepList <- list(2*NM_to_m, 2*NM_to_m)
TimeorRangeList <- list("Time", "Time")
UnderSepsList <- list(c(0, 0.5 * NM_to_m, 1.0 * NM_to_m), c(0, 0.5 * NM_to_m, 1.0 * NM_to_m))
UseLSSModelList <- list(F, F)
UseLSSModelTTBList <- list(F, F)

# Performance Model adaptation lists
ActDistInputsList <- list(c("Recat_FAF_Wake_Separation_Distance", "Recat_FAF_All_Separation_Distance"),
                          c("Legacy_FAF_Wake_Separation_Distance", "Legacy_FAF_All_Separation_Distance"))
ActDistOutputsList <- list(c("Actual_Recat_FAF_Wake_Separation_Distance", "Actual_Recat_FAF_All_Separation_Distance"),
                           c("Actual_Legacy_FAF_Wake_Separation_Distance", "Actual_Legacy_FAF_All_Separation_Distance"))
ActTimeOutputsList <- list(c("Actual_Recat_FAF_Wake_Separation_Time", "Actual_Recat_FAF_All_Separation_Time"),
                           c("Actual_Legacy_FAF_Wake_Separation_Time", "Actual_Legacy_FAF_All_Separation_Time"))
ActSpeedOutputsList <- list(c("Observed_Foll_Actual_Recat_FAF_Wake_Separation", "Observed_Foll_Actual_Recat_FAF_All_Separation"),
                           c("Observed_Foll_Actual_Legacy_FAF_Wake_Separation", "Observed_Foll_Actual_Legacy_FAF_All_Separation"))
PerfDistInputsList <- list(c("Recat_Threshold_Wake_Separation_Distance", "Recat_Threshold_All_Separation_Distance"),
                           c("Legacy_Threshold_Wake_Separation_Distance", "Legacy_Threshold_All_Separation_Distance"))
PerfTimeOutputsList <- list(c("Perfect_Recat_Threshold_Wake_Separation_Time", "Perfect_Recat_Threshold_All_Separation_Time"),
                           c("Perfect_Legacy_Threshold_Wake_Separation_Time", "Perfect_Legacy_Threshold_All_Separation_Time"))
PerfSpeedOutputsList <- list(c("Observed_Perfect_Recat_Threshold_Wake_Separation", "Observed_Perfect_Recat_Threshold_All_Separation"),
                           c("Observed_Perfect_Legacy_Threshold_Wake_Separation", "Observed_Perfect_Legacy_Threshold_All_Separation"))

# Get all the Dates 
Date_List <- dbGetQuery(con, "SELECT DISTINCT(FP_Date) FROM tbl_Flight_Plan")$FP_Date

# If testing, only use one date
if (Testing){Date_List <- Date_List[1:5]}
#Date_List <- Date_List[1]

# If valid clear entire LP table at once instead of it by bit (resets seeds)
if (!Testing & Clear_All){Clear_Landing_Pair(con, PROC_Period = "All", NA)}

# Perform the calculations!!
for (DayCount in 1:length(Date_List)){
  
  Date <- Date_List[DayCount]
  CriteriaStartTime <- Sys.time()
  CriteriaStartTime <- Convert_Time_String_to_Seconds(substr(CriteriaStartTime, 12, 19))
  message("Generating All TBS Data for, ", Date, " (", DayCount, " out of ", length(Date_List), ")...")
  
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # 1. Data Loading ----
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # Loading of Data from SQL.
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  
  # ------------------------------------------------------------------------------------------------------------ #
  # 1.1. Adaptation/Reference Data ----
  # ------------------------------------------------------------------------------------------------------------ #
  message("----------------------------")
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
  if (Join_Gusting){Gust <- Load_Gust_Data(con, PROC_Period, Date)} # Gust Data
  Baro <- Load_Baro_Data_ORD_Validation(con, PROC_Period, Date) # Baro Data
  
  # If not in Testing Mode, Generate LAnding Pairs for this day and load to SQL
  if (!Testing){
    message("Generating the Landing Pair Data for ", Date, "...")
    LP <- Generate_Landing_Pair(FP)
    if (!Clear_All){Clear_Landing_Pair(con, PROC_Period, Date)}
    PopulateSQLTable(con, "tbl_Landing_Pair", select(LP, -Landing_Pair_ID))
  }
  
  # Load Landing Pair data from SQL.
  LP <- Load_Landing_Pair_Data(con, PROC_Period, Date)
  message("----------------------------")
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # 2. Adaptation Grabbing and Pre-maniuplation ----
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # Grab relevant adaptation based on the Levels argumments, and get some preliminary important data. This needs updating by operation.
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  
  # Gather Aircraft Reference Data & Original Prediction Time: LF REF FUNCTION NEEDS UPDATING
  message("Obtaining the Leader/Follower reference parameter data for ", Date, "...")
  LP <- Get_LF_Ref_Parameters(LP, FP, Runways, ACTW, ACTWL, LorF = "Leader")
  LP <- Get_LF_Ref_Parameters(LP, FP, Runways, ACTW, ACTWL, LorF = "Follower")
  LP <- Get_ORD_Prediction_Time(LP, Radar, Path_Legs)
  
  # ------------------------------------------------------------------------------------------------------------ #
  # 2.1. Bolstering Data (Initial) ----
  # ------------------------------------------------------------------------------------------------------------ #
  
  message("Bolstering the samples for ", Date, "...")
  
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
  
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # 3. Intermediary Table Generation (Operation Variant Independent) ----
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # Create the Intermediary Data that is not dependent on Operation. Currently only GWCS Forecasting - which could become independent.
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  
  # Generate the Full GWCS Forecast based on the Prediction Times
  message("Generating the GWCS Full Wind Forecast data for ", Date, "...")
  GWCS_Forecast <- Generate_Full_ORD_GWCS_Forecast(con, LP_Primary_Key, Segments, LP, Time_Key = "Prediction_Time")

  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # 4. More ORD Reference Data ----
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # Gets the Times, Distances and related parameters for ORD elements and Fixed. 
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  
  message("Generating the remaining pair reference data for ", Date, "...")
  # Get the Leader and Follower Operators.
  LP <- LP %>%
    mutate(Leader_Operator = substr(Leader_Callsign, 1, 3),
           Follower_Operator = substr(Follower_Callsign, 1, 3))
  
  # Prepare TBSC Extras (RNAV Flag, Non-Wake Spacing, Runway Dep Separation, TBS Service Level)
  LP <- LP %>% Get_RNAV_Flag() %>% Get_Non_Wake_Spacing() %>% Get_Runway_Dependent_Separation()
  LP <- Get_TBS_Service_Level(con, LP, LP_Primary_Key)
  
  # Get the Fixed DME Observed Parameters.
  message("Generating the Observed Fixed DME Input data for ", Date, "...")
  LP <- Generate_Observed_DME_Parameters(LP, FP, Radar, DMEs = c(0, 1, 4))
  
  # Get the Fixed ORD Observed Input Parameters (FAF Distance, Max RTTs)
  message("Generating the fixed Operation ORD Input data for ", Date, "...")
  LP <- Generate_ORD_Input_Parameters_Operation_Independent(LP, FP, Radar, ORDRunways)
  
  # Get the Forecast Surface Wind for the Leader
  message("Generating the Predicted Leader, Observed Leader and Observed Follower Surface Wind data for ", Date, "...")
  LP <- Get_Surface_Wind(LP, SW, Runways,
                         Prefix = "Forecast_AGI",
                         ID_Var = "Observation_ID",
                         Date_Var = "Landing_Pair_Date",
                         Time_Var = "Prediction_Time",
                         Runway_Var = "Leader_Landing_Runway")
  
  # Get the Observed Surface Widn for the Follower
  LP <- Get_Surface_Wind(LP, SW, Runways,
                         Prefix = "Observed_AGI_Follower",
                         ID_Var = "Observation_ID",
                         Date_Var = "Landing_Pair_Date",
                         Time_Var = "Follower_0DME_Time",
                         Runway_Var = "Follower_Landing_Runway")
  
  # Get the Observed Surface Wind for the Leader
  LP <- Get_Surface_Wind(LP, SW, Runways,
                         Prefix = "Observed_AGI",
                         ID_Var = "Observation_ID",
                         Date_Var = "Landing_Pair_Date",
                         Time_Var = "Leader_0DME_Time",
                         Runway_Var = "Leader_Landing_Runway")
  
  # Get the Forecast Surface Wind Error as the Leader Observed - Forecast
  LP <- mutate(LP, Forecast_AGI_Surface_Headwind_Error = Observed_AGI_Surface_Headwind - Forecast_AGI_Surface_Headwind)
  
  # Get the Forecast Surface Gust for the Follower Runway.
  if (Join_Gusting){
    
    # Join on the Gust Values
    LP <- LP %>%
      rolling_join(Gust, c("Landing_Pair_Date", "Follower_Landing_Runway", "Prediction_Time"), c("Gust_Date", "Runway", "Gust_Time"), Roll = +Inf) %>%
      rename(Forecast_AGI_Surface_Gust = Gust)
    
  }
  
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # 5. All ORD and TBS Calculations ----
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # For both RECAT/LEGACY, Calculate the TBS Distances, Observed and Forecast ORD Metrics. Calculate the Actual/Perfect Speeds/Time Spacings.
  # This could be generalised to include more variants!
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  
  # Do the majority of TBS by Operational Variant.
  for (VariantIndex in 1:length(Variants)){
    message("----------------------------")
    
    # # TEST: Just RECAT
    # VariantIndex <- 1
    # LP <- LPO
    
    # Are we doing Legacy or Recat operations? Potential to generalise to multiple prototypes here!
    LegacyorRecat <- Variants[VariantIndex]
    
    # Set the Local Adaptation used for this variant. Will soon to be available from the database.
    ORD_Levels_Used <- ORDLevelsList[[VariantIndex]]
    DeliveryPoint <- DeliveryPointsList[[VariantIndex]]
    Delivery_Column <- DeliveryColumnsList[[VariantIndex]]
    Use_EFDD <- UseEFDDList[[VariantIndex]]
    Wake_Levels_Used <- TBSWakeLevelsList[[VariantIndex]]
    ROT_Levels_Used <- TBSROTLevelsList[[VariantIndex]]
    TTB_Type <- TTBTypesList[[VariantIndex]]
    Constraints <- ConstraintsList[[VariantIndex]]
    Forecast_Compression_Type = ForecastCompressionTypeList[[VariantIndex]]
    Observed_Compression_Type = ObservedCompressionTypeList[[VariantIndex]]
    Use_Adjustment <- UseAdjustmentList[[VariantIndex]]
    TimeorRange <- TimeorRangeList[[VariantIndex]]
    AdjustWEs <- AdjustWEList[[VariantIndex]]
    MaxRangeToILS <- MaxRangeToILSList[[VariantIndex]]
    AllowedPathLegs <- AllowedPathLegsList[[VariantIndex]]
    MaxUnderRep <- MaxUnderRepList[[VariantIndex]]
    UnderSeps <- UnderSepsList[[VariantIndex]]
    SymmetryWake <- SymmetryWakeList[[VariantIndex]]
    Use_Gust_Data <- UseGustingDataList[[VariantIndex]]
    TBSCBuffers <- TBSCBuffersList[[VariantIndex]]
    ROTTimeType <- ROTTimeTypeList[[VariantIndex]]
    UseLSSModel <- UseLSSModelList[[VariantIndex]]
    UseLSSModelTTB <- UseLSSModelTTBList[[VariantIndex]]
    PopTables <- PopTablesList[[VariantIndex]]
    ActDistInput <- ActDistInputsList[[VariantIndex]]
    ActDistOutput <- ActDistOutputsList[[VariantIndex]]
    ActTimeOutput <- ActTimeOutputsList[[VariantIndex]]
    ActSpeedOutput <- ActSpeedOutputsList[[VariantIndex]]
    PerfDistInput <- PerfDistInputsList[[VariantIndex]]
    PerfTimeOutput <- PerfTimeOutputsList[[VariantIndex]]
    PerfSpeedOutput <- PerfSpeedOutputsList[[VariantIndex]]
    
    # ============================================================================================================================================================== #
    # 5.1. Pair Reference Parameters ----
    # Get Reference Pair Parameters for this operation. Currently includes Wake and ROT adaptation. DBS All Sep assumes same Non-Wake.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant SASAI Reference data for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, Wake_Levels_Used, Param_Type = "Distance", TBSCBuffers, LegacyorRecat)
    LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, Wake_Levels_Used, Param_Type = "Time", TBSCBuffers, LegacyorRecat)
    LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "Wake", Full_Level_Precedence, Wake_Levels_Used, Param_Type = "Speed", TBSCBuffers, LegacyorRecat)
    LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, ROT_Levels_Used, Param_Type = "Distance", TBSCBuffers, LegacyorRecat)
    LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = ROTTimeType, Full_Level_Precedence, ROT_Levels_Used, Param_Type = "Time", TBSCBuffers, LegacyorRecat)
    LP <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, LP, Use = "ROT", Full_Level_Precedence, ROT_Levels_Used, Param_Type = "Speed", TBSCBuffers, LegacyorRecat)
    LP <- mutate(LP, !!sym(paste0(LegacyorRecat, "_DBS_All_Sep_Distance")) := pmax(!!sym(paste0("Reference_", LegacyorRecat, "_Wake_Separation_Distance")), Non_Wake_Separation_Distance, na.rm = T))
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.2. ORD Profile Data ----
    # Generate the Aircraft, IAS and GSPD profiles for ORD only for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant ORD Aircraft, IAS and GSPD Profile data for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    ORD_AC_Profile <- Generate_ORD_Aircraft_Profile(con, LP_Primary_Key, LP, Baro, ORDBuffers = F, Use_EFDD, Use_Gust_Data, UseLSSModel, Full_Level_Precedence, ORD_Levels_Used, LegacyorRecat)
    ORD_IAS_Profile <- Generate_ORD_IAS_Profile(con, LP_Primary_Key, ORD_AC_Profile, LP, GWCS_Forecast)
    ORD_GS_Profile <- Generate_ORD_GSPD_Profile(con, LP_Primary_Key, ORD_IAS_Profile, GWCS_Forecast, Seg_Size)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.3. ORD Input Parameters ----
    # Generate the Distance/Time Input Parameters for Observed/Forecast calculations for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant ORD Input Parameter data for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Generate_ORD_Input_Parameters_Operation_Dependent(LP, FP, Radar, ORD_AC_Profile, DeliveryPoint, LegacyorRecat)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.4. TBSC Profile Data ----
    # Generate the TTB Speed Profiles for Wake and ROT constraints independently for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant ", TTB_Type, " TTB Wake/ROT Speed Profile data for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    TBSC_Profiles_Wake <- Generate_TBSC_Profiles(con, LP, Baro, GWCS_Forecast, LP_Primary_Key, TTB_Type, Use_EFDD, Use_Gust_Data, UseLSSModelTTB, Full_Level_Precedence, Wake_Levels_Used, LegacyorRecat, SymmetryWake)
    TBSC_Profiles_ROT <- Generate_TBSC_Profiles(con, LP, Baro, GWCS_Forecast, LP_Primary_Key, TTB_Type, Use_EFDD, Use_Gust_Data, UseLSSModelTTB, Full_Level_Precedence, ROT_Levels_Used, LegacyorRecat, Symmetry = F)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.5. TBSC Distances (Threshold)
    # Generate the Indicator Distances at Threshold (and the average speed/wind effects if applicable) using the TBS Profiles and provided constraints for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant TBS Distances at Threshold for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Generate_TBS_Indicator_Distances_Threshold(LP, Delivery_Points, Delivery_Column, TBSC_Profiles_ROT, TBSC_Profiles_Wake, Seg_Size, LegacyorRecat)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.6. Forecast ORD Parameters ----
    # Generate the Forecast ORD, WAD and Full Section Results based on the Maximum Threshold Indicator Distance and Forecast Compression Metric for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant Forecast ORD/WAD/Full Section Parameters for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Generate_Forecast_ORD_Results(LP, ORD_GS_Profile, LP_Primary_Key, Forecast_Compression_Type, LegacyorRecat)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.7. TBSC Distances (FAF) ----
    # Generate the Indicator Distances at the FAF (and the average speed/wind effects if applicable) using the TBS Profiles and provided constraints for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant TBS Distances at FAF for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Generate_TBS_Indicator_Distances_FAF(LP, Delivery_Points, Delivery_Column, TBSC_Profiles_ROT, TBSC_Profiles_Wake, Seg_Size, LegacyorRecat)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.8. Observed ORD Parameters ----
    # Generate the Observed ORD, WAD and Full Section results based on Radar data and the Observed Compression Metric for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant Observed ORD/WAD/Full Section Parameters for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Generate_Observed_ORD_Results(LP, Radar, ORD_AC_Profile, TimeorRange, Metric_Type, Use_Adjustment, LegacyorRecat, AdjustWEs, MaxRangeToILS, AllowedPathLegs, MaxUnderRep)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.9. ORD Error Calculations ----
    # Generate the Errors for ORD, WAD and Full Section ORD as the Observed value minus the Forecast for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant ORD/WAD/Full Section Parameter O-F Errors for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Get_ORD_Error_Variables(LP, LegacyorRecat)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.10. Performance Modelling ----
    # Calculate the Observed Speeds, Wind Effects and Flying Times across Provided Separation Distances in Perfect Indicator Delivery and Actual Delivery scenarios.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Generating the ", LegacyorRecat, " variant Performance Modelling Parameters for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LP <- Generate_TBS_Assumed_Separation_Accuracy(LP, LegacyorRecat)
    LP <- Generate_TBS_Actual_Separation_Distances(LP, ActDistInput, ActDistOutput, LegacyorRecat)
    LP <- Generate_TBS_Separation_Scenario_Parameters(LP, Delivery_Column, UnderSeps, ORDInterval = "ORD", LegacyorRecat,
                                                      PerfDistInput, PerfTimeOutput, PerfSpeedOutput, ActDistOutput, ActTimeOutput, ActSpeedOutput)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #
    
    # ============================================================================================================================================================== #
    # 5.11. Operation Table Population
    # Organise results into tables to populate the SQL database for this operation.
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    message("Formatting the ", LegacyorRecat, " variant SQL tables for ", Date, "...")
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    LPORD <- filter(LP, Fake_Observation == 0) %>% arrange(Landing_Pair_ID)
    LP_IDs <- select(LP, Observation_ID, Landing_Pair_ID)
    ORDObservation <- ConstructORDObservation(con, LPORD, LegacyorRecat, Testing, OpTables, PopTables)
    ORDPrediction <- ConstructORDPrediction(con, LPORD, LegacyorRecat, Testing , OpTables, PopTables)
    WADObservation <- ConstructWADObservation(con, LPORD, LegacyorRecat, Testing, OpTables, PopTables)
    WADPrediction <- ConstructWADPrediction(con, LPORD, LegacyorRecat, Testing, OpTables, PopTables)
    ORDAircraftProfile <- ConstructORDACProfile(con, ORD_AC_Profile, LPORD, LP_IDs, LegacyorRecat, Testing, OpTables, PopTables)
    ORDIASProfile <- ConstructORDIASProfile(con, ORD_IAS_Profile, LPORD, LP_IDs, LegacyorRecat, Testing, OpTables, PopTables)
    ORDGSProfile <- ConstructORDGSProfile(con, ORD_GS_Profile, LPORD, LP_IDs, LegacyorRecat, Testing, OpTables, PopTables)
    # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # -- # - # - # - # - # - # - # - # - # - # - #
    # ============================================================================================================================================================== #

  }
  
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # 7. Table Construction & Population ----
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  # Select relevant data for the Variant Independent SQL tables and populate if not in testing mode.
  # ----------------------------------------------------------------------------------------------------------------------------------------- #
  
  message("Formatting the Other SQL tables for ", Date, "...")
  
  # Get the ORiginal only results for ORD tables
  LP_ORD <- filter(LP, Fake_Observation == 0) %>% arrange(Landing_Pair_ID)
  
  # Get the full list of LPIDs to attach to the profile tables
  LP_IDs <- select(LP, Observation_ID, Landing_Pair_ID)
  
  
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
  
  if (!Testing){
    
    PopulateSQLTable(con, SQLTable = "tbl_All_Pair_Reference_Data", Table = All_Pair_Reference_Data)
    PopulateSQLTable(con, SQLTable = "tbl_eTBS_Performance_Model", Table = Old_Performance_Model)
    PopulateSQLTable(con, SQLTable = "tbl_IA_Performance_Model", Table = New_Performance_Model)
    
  } 
  
  # How long did this day take to run?
  CriteriaEndTime <- Sys.time()
  CriteriaEndTime <- Convert_Time_String_to_Seconds(substr(CriteriaEndTime, 12, 19))
  CriteriaCompTime <- CriteriaEndTime - CriteriaStartTime
  message("----------------------------")
  message("TBS Processing for ", Date, " complete in ", seconds_to_period(CriteriaCompTime), ".")
  
  # Attempt at estimating time remaining/percentage completion.
  DaysRemaining <- length(Date_List) - DayCount
  PercentComplete <- round(100 * (DayCount / length(Date_List)), 1)
  if (DayCount != length(Date_List)){
    message("Estimated time remaining: ", seconds_to_period(DaysRemaining*CriteriaCompTime), " (", PercentComplete, "% Complete)")
  }
  
  message("----------------------------")
  
}

# How long did the Entire Processing take?
message("Entire TBS Processing complete in ", seconds_to_period(CriteriaEndTime - Proc_Initial_Time), ".")
