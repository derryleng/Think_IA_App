# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 4.2.1 Generate All Pair Reference Data
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Functions for Populating tbl_All_Pair_Reference_Data.
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
# GENERATE: All Pair Reference Data
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_All_Pair_Reference_Data <- function(con, LP_Primary_Key, Landing_Pair, Radar, Flight_Plan, Surface_Wind){

  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating All Pair Reference Data...")
  
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  
  # Load Relevant Adaptation Tables
  AC_To_Wake <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake")
  AC_To_Wake_Legacy <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake_Legacy")
  Runway <- Load_Adaptation_Table(con, "tbl_Runway")
  Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")
  
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
  
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  
  # Get reduced forms of Aircraft/Wake Reference Tables.
  AC_To_Wake_Reduced <- select(AC_To_Wake, Aircraft_Type, Wake) %>% unique()
  AC_To_Wake_Legacy_Reduced <- select(AC_To_Wake_Legacy, Aircraft_Type, Wake) %>% unique()
  
  # Get Reference Parameters for Leader and Follower Aircraft
  Landing_Pair <- Get_LF_Ref_Parameters(Landing_Pair, Flight_Plan, Runway, AC_To_Wake_Reduced, AC_To_Wake_Legacy_Reduced, "Leader")
  Landing_Pair <- Get_LF_Ref_Parameters(Landing_Pair, Flight_Plan, Runway, AC_To_Wake_Reduced, AC_To_Wake_Legacy_Reduced, "Follower")
  
  # Get Reference Parameters for Aircraft Pair
  
  # - RECAT
  Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = RECAT_Wake_Distance, RecatorLegacy = "Recat", Param_Type = "Distance", Constraint = "Wake_Separation")
  Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = RECAT_Wake_Time, RecatorLegacy = "Recat", Param_Type = "Time", Constraint = "Wake_Separation")
  Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = RECAT_Wake_IAS, RecatorLegacy = "Recat", Param_Type = "IAS", Constraint = "Wake_Separation")
  Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = RECAT_ROT_Distance, RecatorLegacy = "Recat", Param_Type = "Distance", Constraint = "ROT_Spacing")
  Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = RECAT_ROT_Time, RecatorLegacy = "Recat", Param_Type = "Time", Constraint = "ROT_Spacing")
  Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = RECAT_ROT_IAS, RecatorLegacy = "Recat", Param_Type = "IAS", Constraint = "ROT_Spacing")
  
  # - Legacy
  Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = Legacy_Wake_Distance, RecatorLegacy = "Legacy", Param_Type = "Distance", Constraint = "Wake_Separation")
  #Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = Legacy_Wake_Time, RecatorLegacy = "Legacy", Param_Type = "Time", Constraint = "Wake_Separation")
  #Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = Legacy_Wake_IAS, RecatorLegacy = "Legacy", Param_Type = "IAS", Constraint = "Wake_Separation")
  #Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = Legacy_ROT_Distance, RecatorLegacy = "Legacy", Param_Type = "Distance", Constraint = "ROT_Spacing")
  #Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = Legacy_ROT_Time, RecatorLegacy = "Legacy", Param_Type = "Time", Constraint = "ROT_Spacing")
  #Landing_Pair <- Get_Pair_Ref_Parameter(Landing_Pair, Ref_Source = Legacy_ROT_IAS, RecatorLegacy = "Legacy", Param_Type = "IAS", Constraint = "ROT_Spacing")
  
  # Fix All DBS Not-in-trail Distances (Not Operational in SQL yet)
  # Landing_Pair <- Get_Dependent_Runway_Offset_Changes(Landing_Pair, Runway_Offsets)
  
  # Get the DBS All Sep Distance
  Landing_Pair <- mutate(Landing_Pair, DBS_All_Sep_Distance = Reference_Recat_Wake_Separation_Distance)
  
  # Get the Prediction Time
  Landing_Pair <- Get_ORD_Prediction_Time(Landing_Pair, Radar)
  
  # Get ORD Forecast Surface Wind
  Landing_Pair <- Get_Surface_Wind(Landing_Pair, Surface_Wind, Runway, 
                                   Prefix = "Forecast_AGI", 
                                   ID_Var = "Landing_Pair_ID", 
                                   Date_Var = "Landing_Pair_Date", 
                                   Time_Var = "Prediction_Time", 
                                   Runway_Var = "Follower_Landing_Runway")
  
  # Get Leader/Follower Times/RTTs at 0/1/4 DME
  Landing_Pair <- Get_Time_At_Fixed_DME(Landing_Pair, Radar, DME = 0, LorF = "Leader", OrderBy = "Range")
  Landing_Pair <- Get_Time_At_Fixed_DME(Landing_Pair, Radar, DME = 1, LorF = "Leader", OrderBy = "Range")
  Landing_Pair <- Get_Time_At_Fixed_DME(Landing_Pair, Radar, DME = 4, LorF = "Leader", OrderBy = "Range")
  Landing_Pair <- Get_Time_At_Fixed_DME(Landing_Pair, Radar, DME = 0, LorF = "Follower", OrderBy = "Range")
  Landing_Pair <- Get_Time_At_Fixed_DME(Landing_Pair, Radar, DME = 1, LorF = "Follower", OrderBy = "Range")
  Landing_Pair <- Get_Time_At_Fixed_DME(Landing_Pair, Radar, DME = 4, LorF = "Follower", OrderBy = "Range")
  
  # Get Observed Separation Distances at 0/1/4 DME
  Landing_Pair <- Get_Observed_Separation_Time_At_DME(Landing_Pair, DME = 0)
  Landing_Pair <- Get_Observed_Separation_Time_At_DME(Landing_Pair, DME = 1)
  Landing_Pair <- Get_Observed_Separation_Time_At_DME(Landing_Pair, DME = 4)
  
  # Get Observed Separation Distances at 0/1/4 DME (Need to Separate In-Trail and Not-In_Trail Pairs)
  Landing_Pair_I <- Get_In_Trail(Landing_Pair)
  Landing_Pair_I <- Get_Interpolated_Separation_At_DME(Landing_Pair_I, Radar, DME = 0)
  Landing_Pair_I <- Get_Interpolated_Separation_At_DME(Landing_Pair_I, Radar, DME = 1)
  Landing_Pair_I <- Get_Interpolated_Separation_At_DME(Landing_Pair_I, Radar, DME = 4)
  
  Landing_Pair_N <- Get_Not_In_Trail(Landing_Pair)
  Landing_Pair_N <- Get_Interpolated_Separation_At_DME(Landing_Pair_N, Radar, DME = 0)
  Landing_Pair_N <- Get_Interpolated_Separation_At_DME(Landing_Pair_N, Radar, DME = 1)
  Landing_Pair_N <- Get_Interpolated_Separation_At_DME(Landing_Pair_N, Radar, DME = 4)
  
  Landing_Pair <- rbind(Landing_Pair_I, Landing_Pair_N)
  Landing_Pair <- Order_Landing_Pairs(Landing_Pair, "Landing_Pair_ID")
  rm(Landing_Pair_I, Landing_Pair_N)
  
  # Get Observed Follower Surface Wind
  Landing_Pair <- Get_Surface_Wind(Landing_Pair, Surface_Wind, Runway, 
                                   Prefix = "Observed_AGI_Follower", 
                                   ID_Var = "Landing_Pair_ID", 
                                   Date_Var = "Landing_Pair_Date", 
                                   Time_Var = "Follower_0DME_Time", 
                                   Runway_Var = "Follower_Landing_Runway")
  
  # Setup Flag for Observations That Will be Filtered: Go-Arounds and NA Aircraft Types.
  Landing_Pair <- Create_Filter_Flag_Reference(Landing_Pair, Radar, Path_Legs)
  Landing_Pair <- filter(Landing_Pair, Reference_Flag == 0)
  
  # Remove Intermediary Data
  rm(AC_To_Wake_Reduced, AC_To_Wake_Legacy_Reduced)
  
  # TEMP: Add the TBS Wind Effect as NA as we don't need it
  Landing_Pair <- mutate(Landing_Pair, Follower_Forecast_TBS_All_Wind_Effect = NA,
                         UK6Cat_Separation_Time = NA)
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Generated All Pair Reference Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Landing_Pair)

}  
  
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: All Pair Reference Data
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Compare_All_Pair_Reference_Data <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, Landing_Pair){
  
  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Building Comparison of All Pair Reference Data...")
  
  ### ------------------  SQL Data Query ------------------------------------------------
  LPR_Query <- paste0("SELECT ",
                    LP_Primary_Key,
                    ", Delivered_4DME_Separation AS SQL_Observed_4DME_Separation_Distance,
                    Observed_AGI_Surface_Headwind AS SQL_Observed_AGI_Follower_Surface_Headwind,
                    Leader_Aircraft_Type AS SQL_Leader_Aircraft_Type,
                    Follower_Aircraft_Type AS SQL_Follower_Aircraft_Type,
                    Leader_Recat_Wake_Cat AS SQL_Leader_Recat_Wake_Cat,
                    Follower_Recat_Wake_Cat AS SQL_Follower_Recat_Wake_Cat,
                    Ref_Recat_Wake_Separation_Distance AS SQL_Reference_Recat_Wake_Separation_Distance
                   FROM tbl_All_Pair_Reference_Data")
  
  if (PROC_Period == "Day"){
    LPR_Query <- paste0(LPR_Query, " WHERE FP_Date = '", PROC_Criteria, "'")
  }
  
  if (PROC_Period == "Month"){
    LPR_Query <- paste0(LPR_Query, " WHERE FP_Date LIKE '%", PROC_Criteria, "%'")
  }
  ### ---------------------  Get R & SQL Data  ------------------------------------------
  
  Landing_Pair_Reference_SQL <- sqlQuery(con, LPR_Query, stringsAsFactors = F)
  
  Landing_Pair_Reference_R <- select(Landing_Pair, 
                                     !!sym(LP_Primary_Key),
                                     Landing_Pair_Date,
                                     Observed_4DME_Separation_Distance,
                                     Observed_AGI_Follower_Surface_Headwind,
                                     Leader_Aircraft_Type,
                                     Follower_Aircraft_Type,
                                     Leader_Recat_Wake_Cat,
                                     Follower_Recat_Wake_Cat,
                                     Reference_Recat_Wake_Separation_Distance)
  
  ### -------------------- Create Comparison Table --------------------------------------
  
  # Perform the Join to create the table (FULL JOIN: WE NEED TO SEE WHAT IS MISSING)
  ZCOMP_Landing_Pair_Reference <- full_join(Landing_Pair_Reference_SQL, Landing_Pair_Reference_R, by = setNames(LP_Primary_Key, LP_Primary_Key))
  
  # Use Testing Function To Add Parameters
  ZCOMP_Landing_Pair_Reference <- ZCOMP_Landing_Pair_Reference %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_4DME_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_AGI_Follower_Surface_Headwind", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Leader_Aircraft_Type", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Follower_Aircraft_Type", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Leader_Recat_Wake_Cat", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Follower_Recat_Wake_Cat", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Reference_Recat_Wake_Separation_Distance", Tolerance = 0.001)
  
  ### -----------------------------------------------------------------------------------
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Built Comparison of All Pair Reference Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(ZCOMP_Landing_Pair_Reference)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: All Pair Reference Data
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Summary_All_Pair_Reference_Data <- function(LP_Primary_Key, ZCOMP_All_Pair_Reference_Data){
  
  # Create Statistic Table
  ZSTAT_Landing_Pair_Reference <- group_by(ZCOMP_All_Pair_Reference_Data, Landing_Pair_Date) %>%
    summarise(CNT_Observed_4DME_Separation_Distance = sum(FLAG_Observed_4DME_Separation_Distance, na.rm = T),
              CNT_Observed_AGI_Follower_Surface_Headwind = sum(FLAG_Observed_AGI_Follower_Surface_Headwind, na.rm = T),
              CNT_Leader_Aircraft_Type = sum(FLAG_Leader_Aircraft_Type, na.rm = T),
              CNT_Follower_Aircraft_Type = sum(FLAG_Follower_Aircraft_Type, na.rm = T),
              CNT_Leader_Recat_Wake_Cat = sum(FLAG_Leader_Recat_Wake_Cat, na.rm = T),
              CNT_Follower_Recat_Wake_Cat = sum(FLAG_Follower_Recat_Wake_Cat, na.rm = T),
              CNT_Reference_Recat_Wake_Separation_Distance = sum(FLAG_Reference_Recat_Wake_Separation_Distance, na.rm = T))
  
  return(ZSTAT_Landing_Pair_Reference)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: All Pair Reference Data
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #



Construct_All_Pair_Reference_Data <- function(LP_Primary_Key, Landing_Pair){
  
  # Filter Observations
  Landing_Pair <- filter(Landing_Pair, Reference_Flag == 0)
  
  # Select Relevant Fields
  All_Pair_Ref_Data <- select(Landing_Pair,
                              !!sym(LP_Primary_Key),
                              FP_Date = Landing_Pair_Date,
                              Leader_Callsign,
                              Leader_UK_Wake_Cat = Leader_Legacy_Wake_Cat,
                              Leader_Recat_Wake_Cat,
                              Follower_Callsign,
                              Follower_UK_Wake_Cat = Follower_Legacy_Wake_Cat,
                              Follower_Recat_Wake_Cat,
                              UK6Cat_Separation_Distance = Reference_Legacy_Wake_Separation_Distance,
                              UK6Cat_Separation_Time,
                              Ref_Recat_Wake_Separation_Distance = Reference_Recat_Wake_Separation_Distance,
                              Ref_ROT_Spacing_Distance = Reference_Recat_ROT_Spacing_Distance,
                              Leader_4DME_Time,
                              Follower_0DME_Time,
                              Follower_0DME_RTT,
                              Follower_Time_At_4DME,
                              Observed_AGI_Surface_Headwind = Observed_AGI_Follower_Surface_Headwind,
                              Observed_AGI_Surface_Wind_SPD = Observed_AGI_Follower_Surface_Wind_SPD,
                              Observed_AGI_Surface_Wind_HDG = Observed_AGI_Follower_Surface_Wind_HDG,
                              Follower_Forecast_TBS_Wind_Effect = Follower_Forecast_TBS_All_Wind_Effect,
                              Delivered_4DME_Separation = Observed_4DME_Separation_Distance,
                              Landing_Runway = Follower_Landing_Runway)
  
  return(All_Pair_Ref_Data)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: All Pair Reference Data
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Clear_All_Pair_Reference_Data <- function(con, PROC_Period, PROC_Criteria){
  
  # Build SQL Query
  Query <- "DELETE FROM tbl_All_Pair_Reference_Data"
  
  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE FP_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE FP_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Execute Query
  sqlQuery(con, Query)
  
}
  

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: All Pair Reference Data
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Populate_All_Pair_Reference_Data <- function(con, TableToAdd){
  
  # Add to Table
  
  
} 

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

