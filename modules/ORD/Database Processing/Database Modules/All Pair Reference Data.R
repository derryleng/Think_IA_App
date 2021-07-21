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
# Description: Functions for Populating tbl_All_Pair_Reference_Data and tbl_Landing_Pair
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
# GENERATE: Landing_Pair
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Generate_Landing_Pair: Takes Flight Plan Data. Generates Not-in-trail pairs using Runway "Indexes" that currently consist of the Runway Direction.
# For each flight joins on the next flight and assigns a landing pair. If runways are different, not in trail, if the same, in trail. Does a second
# pass with Runway (Not runway index) and all non-existent in-trail pairs are classified as in-trail-non-sequential. Works well for Heathrow, but
# in-trail/not-in-trail defintions are incorrect. Has no concept of wake dependent runways. Assumes all differing runway pairs behave the same.
# Creates table with the Landing pair type (as above) and the leader/follower flight plan IDs. This is used as the basis of all TBS related calculations.
# ------------------------------------------------------------------------------------------------------------------------------------------ #

Generate_Landing_Pair <- function(FP){
  
  # Local Adaptation
  Max_Time_Diff <- 1200 #20 Mins
  
  # Get Relevant Flight Plan Data
  FP <- FP %>%
    select(Flight_Plan_ID, FP_Date, Landing_Runway, Time_At_4DME) %>%
    mutate(Runway_Index = substr(Landing_Runway, 2, 3))
  
  # Remove all Void Observations and arrange by Runway groupings and Time
  FP <- filter(FP, !is.na(Landing_Runway) & !is.na(Time_At_4DME)) %>%
    arrange(FP_Date, Runway_Index, Time_At_4DME)
  
  # Pass 1: Detect In-Trail and Not-In-Trail pairs.
  FP1 <- FP %>% 
    arrange(FP_Date, Runway_Index, Time_At_4DME) %>%
    group_by(FP_Date, Runway_Index) %>% 
    mutate(Previous_Flight_Plan_ID = lag(Flight_Plan_ID),
           Previous_Landing_Runway = lag(Landing_Runway),
           Previous_Time_At_4DME = lag(Time_At_4DME)) %>%
    ungroup() %>%
    mutate(Time_Difference = Time_At_4DME - Previous_Time_At_4DME,
           Is_Valid_Pair = ifelse(Time_Difference < Max_Time_Diff & Time_Difference > 0, 1, 0)) %>%
    filter(Is_Valid_Pair == 1) %>% 
    mutate(Landing_Pair_Type = ifelse(Landing_Runway == Previous_Landing_Runway, "In_Trail", "Not_In_Trail")) %>%
    mutate(FPID_Pair = paste0(Flight_Plan_ID, "-", Previous_Flight_Plan_ID))
  
  # Pass 2: Detect Non-Sequential In-Trail Pairs.
  FP2 <- FP %>% 
    arrange(FP_Date, Landing_Runway, Time_At_4DME) %>%
    group_by(FP_Date, Landing_Runway) %>% 
    mutate(Previous_Flight_Plan_ID = lag(Flight_Plan_ID),
           Previous_Landing_Runway = lag(Landing_Runway),
           Previous_Time_At_4DME = lag(Time_At_4DME)) %>%
    ungroup() %>%
    mutate(Time_Difference = Time_At_4DME - Previous_Time_At_4DME,
           Is_Valid_Pair = ifelse(Time_Difference < Max_Time_Diff & Time_Difference > 0, 1, 0)) %>%
    filter(Is_Valid_Pair == 1) %>% 
    mutate(Landing_Pair_Type = "In_Trail_Non_Sequential") %>%
    mutate(FPID_Pair = paste0(Flight_Plan_ID, "-", Previous_Flight_Plan_ID)) %>%
    filter(FPID_Pair %!in% FP1$FPID_Pair)
  
  # Join Datasets together.
  LP <- rbind(FP1, FP2) %>%
    mutate(Landing_Pair_ID = NA) %>%
    select(Landing_Pair_ID,
           Landing_Pair_Date = FP_Date,
           Leader_Flight_Plan_ID = Previous_Flight_Plan_ID,
           Follower_Flight_Plan_ID = Flight_Plan_ID,
           Landing_Pair_Type)
  
  
  return(LP)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: Landing_Pair (Updated - IN PROGRESS!)
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Updated version of Generate_Landing_Pair (NOT CURRENTLY OPERATIONAL!). Instead of using a Runway "Index" that assumes dependent runways must
# have same runway name (e.g. 27L and 27R has Index 27) This version uses the Runway_Pair_Rule table to assign pairs based on runway pairings
# allowed via adaptation. For example, for Toronto Pearson airport, Runways 23 and 24L are "Dependent" but are not picked up as a not-in-trail 
# pair at the moment. The function first uses Bolster_Runway_Pair_Rule to get the information for every possible runway pair with constraints 
# including the same runway twice for all runways (in-trail). The outer loop loops across each leader runway, and the inner loop across all follower
# runways that have constraints. All Flights landing at both runways are grabbed and joined together with a rolling join on a given Time argument
# (prev Time_At_4DME) such that each flight on the leader runway corresponds to a single flight on the follower runway. The output table
# is the same as before but with two extra columns: Runway Pair Index, an integer representing the index value of the runway pair 
# (each pair of identical runways has it's own index too, and swapping of leader and follower runways correspond to different indexes) and 
# the pair dependence (i.e. in-trail, wake dependent, runway dependent)
# ------------------------------------------------------------------------------------------------------------------------------------------ #

Generate_Landing_Pair_New <- function(FP, Runway, Runway_Pair_Rule, Time_Used, Max_Time_Diff){
  
  # This adds the reverse Runway Pairs and also the Single Runways as a double Pair
  Runway_Pair_Rule <- Bolster_Runway_Pair_Rule(Runway_Pair_Rule, Runway_Rule)
  
  # Get Relevant Flight Plan Data
  FP <- FP %>%
    select(Flight_Plan_ID, FP_Date, Landing_Runway, !!sym(Time_Used))
  
  # Remove all Void Observations and arrange by Date and Time
  FP <- filter(FP, !is.na(Landing_Runway) & !is.na(!!sym(Time_Used))) %>%
    arrange(FP_Date, !!sym(Time_Used))
  
  # Loop 1: By Runway
  for (Runway in unique(Runway_Rule$Runway_Name)){
    
    # Filter for Aircraft on this Runway
    FP_Runway <- filter(FP, Landing_Runway == Runway)
    
    # Rename Flight Plan ID to Leader
    FP_Runway <- rename(FP_Runway, Leader_Flight_Plan_ID = Flight_Plan_ID, Leader_Landing_Runway = Landing_Runway)
    
    # Filter Runway Pair Rule for Rules with this Runway as Runway 1
    RPR_Runway <- filter(Runway_Pair_Rule, Runway_1 == Runway)
    
    # Loop 2: Loop across all Runway 2 Values to get the different allowed Landing Pairs
    for (Runway2 in RPR_Runway$Runway_2){
      
      # Filter FP for Aircraft with the second Landing Runway. 
      FP_Runway2 <- filter(FP, Landing_Runway == Runway2)
      
      # Rename Flight Plan ID to Leader
      FP_Runway2 <- rename(FP_Runway2, Follower_Flight_Plan_ID = Flight_Plan_ID, Follower_Landing_Runway = Landing_Runway)
      
      # Remove 0.0001 seconds to the Time value to stop aircraft matching with themselves
      FP_Runway2 <- mutate(FP_Runway2, !!sym(Time_Used) := !!sym(Time_Used) - 0.0001)
      
      # Do a rolling join on the Times
      FP_Runways <- rolling_join(FP_Runway, FP_Runway2, c("FP_Date", Time_Used), c("FP_Date", Time_Used), Roll = Max_Time_Diff)
      
      # Bind to new LP Dataset
      if (!exists("LP")){LP <- FP_Runways} else {LP <- rbind(LP, FP_Runways)}
      
    }
    
  }
  
  return(LP)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: Landing_Pair
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# This is an SQL Script constructor that deletes entries from tbl_Landing_Pair based on the Processing Period (Day, Month) and its value.
# This is a cascade delete operation: Any tables with a Landing_Pair_ID foreign key have their corresponding entries removed too. Note that
# if All Landing Pair Data is deleted (not a single day or month) then the Landing_Pair_ID primary key seed is reset back to 1.
# ------------------------------------------------------------------------------------------------------------------------------------------ #


Clear_Landing_Pair <- function(con, PROC_Period, PROC_Criteria){
  
  # Main Query.
  Query <- "DELETE FROM tbl_Landing_Pair"
  
  # Delete single Day/Month if Period is "Day"/""Month" and non-NA criteria provided.
  # Delete All if "All" Period selected or NA Criteria provided. (& Reset Primary Key)
  if (PROC_Period == "Day" & !is.na(PROC_Criteria)){
    Query <- paste0(Query, " WHERE Landing_Pair_Date = '", PROC_Criteria, "'")
  } else if (PROC_Period == "Month" & !is.na(PROC_Criteria)){
    Query <- paste0(Query, " WHERE Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")
  } else {
    Query <- paste0(Query, "\nIF IDENT_CURRENT('tbl_Landing_Pair') >= 1
  DBCC CHECKIDENT (tbl_Landing_Pair, RESEED, 0)")
  }
  
  dbExecute(con, Query)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: All Pair Reference Data (DEGENERATE)
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# This is a table with a collection of landing pair reference data that has traditionally been used to provide extra data for ORD 
# calibration. In IAC TBS v0.0 this has been recycled as a basis for all preceding ORD/TBS Processing, gathering the necessary reference data
# for new/old operations for performance modelling and ORD Predictions. THIS FUNCTION WILL BECOME OBSOLETE IN IAC TBS v1.0.
# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_All_Pair_Reference_Data <- function(con, LP_Primary_Key, Landing_Pair, Radar, Flight_Plan, Surface_Wind, Adaptation_Levels, Level_Switches_Wake, Level_Switches_ROT, TBSCBuffers){

  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating All Pair Reference Data...")
  
  ## Get the Level Switches.

  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #

  # Load Relevant Adaptation Tables
  AC_To_Wake <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake")
  AC_To_Wake_Legacy <- Load_Adaptation_Table(con, "tbl_Aircraft_Type_To_Wake_Legacy")
  Runway <- Load_Adaptation_Table(con, "tbl_Runway")
  Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")

  # TBS Calcs Related tables
  #Runway_Pair_Rule <- Load_Adaptation_Table(con, "tbl_Runway_Pair_Rule")
  #Runway_Rule <- Load_Adaptation_Table(con, "tbl_Runway_Rule")
  #Wake_Separation_Min <- Load_Adaptation_Table(con, "tbl_Wake_Separation_Min")
  #Constraint_Precedence <- Load_Adaptation_Table(con, "tbl_Constraint_Precedence_List")
  #Delivery_Points <- Load_Adaptation_Table(con, "tbl_Delivery_Points")

  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------------------------------------ #

  # Get reduced forms of Aircraft/Wake Reference Tables.
  AC_To_Wake_Reduced <- select(AC_To_Wake, Aircraft_Type, Wake) %>% unique()
  AC_To_Wake_Legacy_Reduced <- select(AC_To_Wake_Legacy, Aircraft_Type, Wake) %>% unique()

  # Get Reference Parameters for Leader and Follower Aircraft
  Landing_Pair <- Get_LF_Ref_Parameters(Landing_Pair, Flight_Plan, Runway, AC_To_Wake_Reduced, AC_To_Wake_Legacy_Reduced, "Leader")
  Landing_Pair <- Get_LF_Ref_Parameters(Landing_Pair, Flight_Plan, Runway, AC_To_Wake_Reduced, AC_To_Wake_Legacy_Reduced, "Follower")
  
  # Get the Leader and Follower Operators.
  Landing_Pair <- Landing_Pair %>%
    mutate(Leader_Operator = substr(Leader_Callsign, 1, 3),
           Follower_Operator = substr(Follower_Callsign, 1, 3))

  # - RECAT (IA PWS Update) ### NOTE: Does not distinguish between nit pairs - but does remove nit ROT contraints.
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "Wake", Adaptation_Levels, Level_Switches_Wake, Param_Type = "Distance", TBSCBuffers, "Recat")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "Wake", Adaptation_Levels, Level_Switches_Wake, Param_Type = "Time", TBSCBuffers, "Recat")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "Wake", Adaptation_Levels, Level_Switches_Wake, Param_Type = "Speed", TBSCBuffers, "Recat")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "ROT", Adaptation_Levels, Level_Switches_ROT, Param_Type = "Distance", TBSCBuffers, "Recat")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "ROT", Adaptation_Levels, Level_Switches_ROT, Param_Type = "Time", TBSCBuffers, "Recat")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "ROT", Adaptation_Levels, Level_Switches_ROT, Param_Type = "Speed", TBSCBuffers, "Recat")

  # - Legacy
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "Wake", Adaptation_Levels, Level_Switches_Wake, Param_Type = "Distance", TBSCBuffers, "Legacy")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "Wake", Adaptation_Levels, Level_Switches_Wake, Param_Type = "Time", TBSCBuffers, "Legacy")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "Wake", Adaptation_Levels, Level_Switches_Wake, Param_Type = "Speed", TBSCBuffers, "Legacy")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "ROT", Adaptation_Levels, Level_Switches_ROT, Param_Type = "Distance", TBSCBuffers, "Legacy")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "ROT", Adaptation_Levels, Level_Switches_ROT, Param_Type = "Time", TBSCBuffers, "Legacy")
  Landing_Pair <- Get_Reference_SASAI_Parameters_In_Precedence(con, LP_Primary_Key, Landing_Pair, Use = "ROT", Adaptation_Levels, Level_Switches_ROT, Param_Type = "Speed", TBSCBuffers, "Legacy")
  
  # Fix All DBS Not-in-trail Distances (Not Operational in SQL yet)
  # Landing_Pair <- Get_Dependent_Runway_Offset_Changes(Landing_Pair, Runway_Offsets)

  # Get the DBS All Sep Distance - ## Wake only for TBS Table for existing operations.
  Landing_Pair <- mutate(Landing_Pair, 
                         Recat_DBS_All_Sep_Distance = Reference_Recat_Wake_Separation_Distance,
                         Legacy_DBS_All_Sep_Distance = Reference_Legacy_Wake_Separation_Distance)
  
  # Get the Prediction Time
  Landing_Pair <- Get_ORD_Prediction_Time(Landing_Pair, Radar, Path_Legs)

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
# COMPARE: All Pair Reference Data (DEGENERATE)
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# This function was created in IAC TBS v0.0 to compare the outputs of tbl_All_Pair_Reference_Data created in IAC TBS v0.0 to the UTMA Validation
# Database. The SQL table is called and the relevant fields in IAC TBS gathered and the values of these fields are compared with "Difference" 
# variables for each field and "Flag" variables signifiying if the two systems have equal values to an agreed tolerance.
# Thorough verification for non-core IA alogirthm componenets has been ruled unecessary so this function is now obsolete.
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

  Landing_Pair_Reference_SQL <- dbGetQuery(con, LPR_Query, stringsAsFactors = F)

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
# SUMMARY: All Pair Reference Data (DEGENERATE)
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# a companion to Compare_All_Pair_Reference_Data. This summarises the number of discrpeancies per field per day to see where differences 
# originate. Thorough verification for non-core IA alogirthm componenets has been ruled unecessary so this function is now obsolete.
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
# CONSTRUCT: All Pair Reference Data (DEGENERATE)
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# This is the constructor function for tbl_All_Pair_Reference_Data. This selects the relevant Landing Pair fields generated in IAC TBS v0.0
# and arranges them in the format of the UTMA Validation Database table. IAC TBS v1.0 creates these fields differently so this will soon
# become obsolete.
# ------------------------------------------------------------------------------------------------------------------------------------------ #



Construct_All_Pair_Reference_Data <- function(LP_Primary_Key, Landing_Pair){

  # Filter Observations
  Landing_Pair <- filter(Landing_Pair, Reference_Flag == 0)

  # Select Relevant Fields
  All_Pair_Ref_Data <- select(Landing_Pair,
                              !!sym(LP_Primary_Key),
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
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
