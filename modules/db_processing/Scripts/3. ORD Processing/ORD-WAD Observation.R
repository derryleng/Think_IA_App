# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.2. ORD/WAD Observations
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Uses tbl_All_Pair_Reference_Data, tbl_ORD_Aircraft_Profile and a Radar Track ~Point/Derived/Flight Plan hybrid
#              to find the RTT's/Times at the first track point above Delivery Point and FAF (ORD) and the second track point
#              above FAF - to not double count any track points - and the first point above CCT - if not the furthest point 
#              where Follower has joined the ILS (WAD). Mean Leader/Follower IAS/WE across these sections are calculated too.
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

# Updates Required/Theorized
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# - !!!Are we using Runway FAF or ORD LST??
# - Save to SQL/Local
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: ORD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_ORD_Observation <- function(con, LP_Primary_Key, Landing_Pair, Radar, Surface_Wind){
  
  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating ORD Observation Data...")
  
  # ----------------------------------------------- #
  # X.X.X Get Adaptation
  # ----------------------------------------------- #
  # 
  # ----------------------------------------------- #
  
  # ORD Adaptation
  ORD_Operator <- NA ## Added for PWS
  ORD_Aircraft <- Load_Adaptation_Table(con, "tbl_ORD_Aircraft_Adaptation")
  ORD_Wake <- Load_Adaptation_Table(con, "tbl_ORD_Wake_Adaptation")
  ORD_DBS <- Load_Adaptation_Table(con, "tbl_ORD_DBS_Adaptation")
  
  # ORD Profile Selection Type.  
  ORD_Profile_Selection <- as.character(ADAP_Config$ORD_Profile_Selection)
  
  # Flag to remove all non-wake pairs.
  Wake_Pairs_Only <- as.logical(ADAP_Config$Process_Wake_Pairs_Only)
  
  # Flag to remove Not-In_Trail pairs. 
  In_Trail_Only <- as.logical(ADAP_Config$Process_In_Trail_Pairs_Only)
  
  # Path Leg Data
  Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")
  
  # Runway Adaptation
  Runway <- Load_Adaptation_Table(con, "tbl_Runway")
  ORD_Runway <- Load_Adaptation_Table(con, "tbl_ORD_Runway_Adaptation")
  
  # Get Delivery Point
  New_Delivery <- 0 * NM_to_m
    
  # ----------------------------------------------- #
  # X.X.X Get Data
  # ----------------------------------------------- #
  # 
  # ----------------------------------------------- #
  
  # Get the Local Stabilisation Thresholds
  LSTs <- Get_Compression_Distances(Landing_Pair, ORD_Operator, ORD_Aircraft, ORD_Wake, ORD_DBS, ORD_Profile_Selection, "LST") %>% 
    select(-Leader_Flight_Plan_ID)
  
  # Temporary Hardcode to match Validation Outputs (Will Remove)
  #LSTs <- mutate(LSTs, Local_Stabilisation_Distance = 4 * NM_to_m)
  
  # Join on the CCTs and the LSTs
  Landing_Pair <- left_join(Landing_Pair, LSTs, by = c("Landing_Pair_ID"))
  Landing_Pair <- mutate(Landing_Pair, Delivery_Distance = New_Delivery)
  
  
  # ----------------------------------------------- #
  # X.X.X Filtering Criteria & Pre Calcs
  # ----------------------------------------------- #
  # 
  # ----------------------------------------------- #
  
  # Create a Filter Flag for ORD/WAD Observation tables. These Currently have the same filters. 
  Landing_Pair <- Create_Filter_Flag_Observation(Landing_Pair, ORD_Aircraft, ORD_Profile_Selection, Wake_Pairs_Only, In_Trail_Only)
  
  # Get Follower ILS Join Time - we need ascending time order to choose first point. Used for WAD - Currently Uses GWCS ILS.
  Landing_Pair <- Get_Follower_ILS_Join_Time(Landing_Pair, Radar, Path_Legs, PLTorGWCS = "GWCS")
  
  # Get Observed Leader Surface Wind Parameters
  Landing_Pair <- Get_Surface_Wind(Landing_Pair, Surface_Wind, Runway,
                                   Prefix = "Observed_AGI",
                                   ID_Var = "Landing_Pair_ID",
                                   Date_Var = "Landing_Pair_Date",
                                   Time_Var = "Leader_0DME_Time",
                                   Runway_Var = "Leader_Landing_Runway")
  
  # ----------------------------------------------- #
  # X.X.X ORD
  # ----------------------------------------------- #
  # 
  # ----------------------------------------------- #
  
  # Get Leader Delivery Distance (1st Point after New Delivery, Ordered By Increasing Distance - Same as 0DME Currently)
  Landing_Pair <- Get_Nth_Time_At_LP_Distance(Landing_Pair, Radar, N = 1, 
                                              Prefix = "Delivery",
                                              Filter_Var = "Range_To_Threshold",
                                              Crit_Var = "Delivery_Distance",
                                              LorF = "Leader", 
                                              OrderBy = "Range Ascending")
  
  # Get Leader ORD FAF Time (1st Point after Local Stabilisation Distance, Ordered By Increasing Distance).
  Landing_Pair <- Get_Nth_Time_At_LP_Distance(Landing_Pair, Radar, N = 1, 
                                              Prefix = "ORD_FAF",
                                              Filter_Var = "Range_To_Threshold",
                                              Crit_Var = "Local_Stabilisation_Distance",
                                              LorF = "Leader", 
                                              OrderBy = "Range Ascending")
  
  # TEMP: Substitute NA for FAF & CC Times/RTTs if using Non-ORD Supported Runway
  Landing_Pair <- mutate(Landing_Pair,
                                   Leader_ORD_FAF_Time = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_ORD_FAF_Time, NA),
                                   Leader_ORD_FAF_RTT = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_ORD_FAF_RTT, NA))
  
  
  # Split up Data into In-Trail and Not-In-Trail for Observed Speed/WE Calcs
  Landing_Pair_I <- Get_In_Trail(Landing_Pair)
  Landing_Pair_N <- Get_Not_In_Trail(Landing_Pair)
  
  # Get Observed Leader Parameters (In Trail)
  Landing_Pair_I <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_I, Radar, 
                                                           Prefix = "ORD",
                                                           LorF = "Leader",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_ORD_FAF_Time",
                                                           End_Var = "Leader_Delivery_Time")
  
  # Get Observed Follower Parameters (In Trail)
  Landing_Pair_I <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_I, Radar, 
                                                           Prefix = "ORD",
                                                           LorF = "Follower",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_ORD_FAF_Time",
                                                           End_Var = "Leader_Delivery_Time")
  
  # Get Observed Leader Parameters (Not In Trail)
  Landing_Pair_N <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_N, Radar, 
                                                           Prefix = "ORD",
                                                           LorF = "Leader",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_ORD_FAF_Time",
                                                           End_Var = "Leader_Delivery_Time")
  
  # Get Observed Follower Parameters (Not In Trail)
  Landing_Pair_N <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_N, Radar, 
                                                           Prefix = "ORD",
                                                           LorF = "Follower",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_ORD_FAF_Time",
                                                           End_Var = "Leader_Delivery_Time")
  
  
  # Get the Interpolated IT Follower Distances for ORD. (Between Leader Delivery and FAF Time) 
  Landing_Pair_I <- Get_Interpolated_Compression_Distances(Landing_Pair_I, Radar,
                                                         ORDorWAD = "ORD", 
                                                         Start_Time_Var = "Leader_ORD_FAF_Time",
                                                         End_Time_Var = "Leader_Delivery_Time")
  
  # Get the Interpolated NIT Follower Distances for ORD. (Between Leader Delivery and FAF Time) 
  Landing_Pair_N <- Get_Interpolated_Compression_Distances(Landing_Pair_N, Radar,
                                                         ORDorWAD = "ORD", 
                                                         Start_Time_Var = "Leader_ORD_FAF_Time",
                                                         End_Time_Var = "Leader_Delivery_Time")
  
  # Get the ORD Separation Distance IT
  Landing_Pair_I <- Get_Nth_Time_At_LP_Distance(Landing_Pair_I, Radar, N = 1, 
                                              Prefix = "ORD_Separation",
                                              Filter_Var = "Track_Time",
                                              Crit_Var = "Leader_0DME_Time",
                                              LorF = "Follower", 
                                              OrderBy = "Time Ascending")
  
  # Get the ORD Separation Distance NIT
  Landing_Pair_N <- Get_Nth_Time_At_LP_Distance(Landing_Pair_N, Radar, N = 1, 
                                                Prefix = "ORD_Separation",
                                                Filter_Var = "Track_Time",
                                                Crit_Var = "Leader_0DME_Time",
                                                LorF = "Follower", 
                                                OrderBy = "Time Ascending")
  
  
  # Bind Datasets Together and Reorder
  Landing_Pair <- rbind(Landing_Pair_I, Landing_Pair_N)
  Landing_Pair <- Order_Landing_Pairs(Landing_Pair, "Landing_Pair_ID")
  rm(Landing_Pair_I, Landing_Pair_N)
  
  # Tidy Up ORD Separation Distance Calcs
  Landing_Pair <- select(Landing_Pair, -c("Follower_ORD_Separation_Time")) %>% 
    rename(ORD_Separation_Distance = Follower_ORD_Separation_RTT)
  
  # Get the Observed ORD Compression 
  Landing_Pair <- Get_Observed_Compression_Values(Landing_Pair, ORDorWAD = "ORD")
  
  # TEMP: Delivered FAF Separation
  Landing_Pair <- mutate(Landing_Pair, Delivered_FAF_Separation = NA)
  
  # Get the ORD Prediction Filter Flags
  Landing_Pair <- Create_Filter_Flag_Prediction(Landing_Pair, "ORD")
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Generated ORD Observation Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Landing_Pair)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: ORD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Compare_ORD_Observation <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, Landing_Pair){
  
  ### ------------------  SQL Data Query ------------------------------------------------
  OO_Query <- "SELECT
  OO.Landing_Pair_ID,
  OO.Observed_Compression AS SQL_Observed_ORD_Compression,
  OO.Observed_Mean_Leader_IAS AS SQL_Observed_Leader_ORD_IAS,
  OO.Observed_Mean_Follower_IAS AS SQL_Observed_Follower_ORD_IAS,
  OO.Observed_Mean_Leader_Wind_Effect AS SQL_Observed_Leader_ORD_Wind_Effect,
  OO.Observed_Mean_Follower_Wind_Effect AS SQL_Observed_Follower_ORD_Wind_Effect,
  OO.Observed_AGI_Surface_Headwind AS SQL_Observed_AGI_Surface_Headwind,
  OO.Leader_FAF_Time AS SQL_Leader_ORD_FAF_Time,
  OO.Leader_0DME_Time AS SQL_Leader_Delivery_Time,
  OO.Leader_FAF_RTT AS SQL_Leader_ORD_FAF_RTT,
  OO.Leader_0DME_RTT AS SQL_Leader_Delivery_RTT,
  OO.Follower_Start_RTT AS SQL_Follower_ORD_Start_RTT,
  OO.Follower_Stop_RTT AS SQL_Follower_ORD_Stop_RTT
  FROM tbl_ORD_Observation OO"
  
  if (PROC_Period == "Day"){
    OO_Query <- paste0(OO_Query, " WHERE OO.Observation_Date = '", PROC_Criteria, "'")
  }
  
  if (PROC_Period == "Month"){
    OO_Query <- paste0(OO_Query, " WHERE OO.Observation_Date LIKE '%", PROC_Criteria, "%'")
  }
  ### ---------------------  Get R & SQL Data  ------------------------------------------
  
  # Set 0 Values to NA
  ORD_Observation_SQL <- sqlQuery(con, OO_Query, stringsAsFactors = F) %>%
    mutate(SQL_Observed_Leader_ORD_IAS = ifelse(SQL_Observed_Leader_ORD_IAS == 0, NA, SQL_Observed_Leader_ORD_IAS),
           SQL_Observed_Follower_ORD_IAS = ifelse(SQL_Observed_Follower_ORD_IAS == 0, NA, SQL_Observed_Follower_ORD_IAS),
           SQL_Observed_Leader_ORD_Wind_Effect = ifelse(SQL_Observed_Leader_ORD_Wind_Effect == 0, NA, SQL_Observed_Leader_ORD_Wind_Effect),
           SQL_Observed_Follower_ORD_Wind_Effect = ifelse(SQL_Observed_Follower_ORD_Wind_Effect == 0, NA, SQL_Observed_Follower_ORD_Wind_Effect))
  
  ORD_Observation_R <- select(Landing_Pair,
                              Landing_Pair_ID,
                              Landing_Pair_Date,
                              Observation_Flag,
                              Observed_ORD_Compression,
                              Observed_Leader_ORD_IAS,
                              Observed_Follower_ORD_IAS,
                              Observed_Leader_ORD_Wind_Effect,
                              Observed_Follower_ORD_Wind_Effect,
                              Observed_AGI_Surface_Headwind,
                              Leader_ORD_FAF_Time,
                              Leader_Delivery_Time,
                              Leader_ORD_FAF_RTT,
                              Leader_Delivery_RTT,
                              Follower_ORD_Start_RTT,
                              Follower_ORD_Stop_RTT)
  
  ORD_Observation_R <- filter(ORD_Observation_R, Observation_Flag == 0)
  
  ### -------------------- Create Comparison Table --------------------------------------
  
  # Full Join two Datasets
  ZCOMP_ORD_Observation <- full_join(ORD_Observation_SQL, ORD_Observation_R, by = c("Landing_Pair_ID"))
  
  ZCOMP_ORD_Observation <- ZCOMP_ORD_Observation %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_ORD_Compression", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Leader_ORD_IAS", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Follower_ORD_IAS", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Leader_ORD_Wind_Effect", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Follower_ORD_Wind_Effect", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_AGI_Surface_Headwind", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_ORD_FAF_RTT", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_ORD_FAF_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_Delivery_RTT", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_Delivery_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Follower_ORD_Start_RTT", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Follower_ORD_Stop_RTT", Tolerance = 0.01)
  
  ### -----------------------------------------------------------------------------------
  
  return(ZCOMP_ORD_Observation)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: ORD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Summary_ORD_Observation <- function(LP_Primary_Key, ZCOMP_ORD_Observation){
  
  # Summary Table
  ZSTAT_ORD_Observation <- group_by(ZCOMP_ORD_Observation, Landing_Pair_Date) %>% summarise(
    CNT_Observed_ORD_Compression = sum(FLAG_Observed_ORD_Compression, na.rm=T),
    CNT_Observed_Leader_ORD_IAS = sum(FLAG_Observed_Leader_ORD_IAS, na.rm=T),
    CNT_Observed_Follower_ORD_IAS = sum(FLAG_Observed_Follower_ORD_IAS, na.rm=T),
    CNT_Observed_Leader_ORD_Wind_Effect = sum(FLAG_Observed_Leader_ORD_Wind_Effect, na.rm=T),
    CNT_Observed_Follower_ORD_Wind_Effect = sum(FLAG_Observed_Follower_ORD_Wind_Effect, na.rm=T),
    CNT_Observed_AGI_Surface_Headwind = sum(FLAG_Observed_AGI_Surface_Headwind, na.rm=T),
    CNT_Leader_ORD_FAF_Time = sum(FLAG_Leader_ORD_FAF_Time, na.rm=T),
    CNT_Leader_Delivery_Time = sum(FLAG_Leader_Delivery_Time, na.rm=T),
    CNT_Leader_ORD_FAF_RTT = sum(FLAG_Leader_ORD_FAF_RTT, na.rm=T),
    CNT_Leader_Delivery_RTT = sum(FLAG_Leader_Delivery_RTT, na.rm=T),
    CNT_Follower_ORD_Start_RTT = sum(FLAG_Follower_ORD_Start_RTT, na.rm=T),
    CNT_Follower_ORD_Stop_RTT = sum(FLAG_Follower_ORD_Stop_RTT, na.rm=T)) %>%
    ungroup()

  return(ZSTAT_ORD_Observation)
  
}
  
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: ORD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Construct_ORD_Observation <- function(LP_Primary_Key, Landing_Pair){
  
  # Filter Observations
  Landing_Pair <- filter(Landing_Pair, Observation_Flag == 0)
  
  # Select Relevant Fields
  ORD_Observation <- select(Landing_Pair,
                            !!sym(LP_Primary_Key),
                            Observed_Compression = Observed_ORD_Compression,
                            Observed_Mean_Leader_IAS = Observed_Leader_ORD_IAS,
                            Observed_Mean_Follower_IAS = Observed_Follower_ORD_IAS,
                            Observed_Mean_Leader_Wind_Effect = Observed_Leader_ORD_Wind_Effect,
                            Observed_Mean_Follower_Wind_Effect = Observed_Follower_ORD_Wind_Effect,
                            Observed_AGI_Surface_Headwind,
                            Observation_Date = Landing_Pair_Date,
                            Leader_FAF_Time = Leader_ORD_FAF_Time,
                            Leader_0DME_Time = Leader_Delivery_Time,
                            Leader_FAF_RTT = Leader_ORD_FAF_RTT,
                            Leader_0DME_RTT = Leader_Delivery_RTT,
                            Follower_Start_RTT = Follower_ORD_Start_RTT,
                            Follower_Stop_RTT = Follower_ORD_Stop_RTT,
                            Leader_Callsign,
                            Follower_Callsign,
                            Follower_0DME_Time,
                            Follower_0DME_RTT,
                            Follower_Time_At_4DME,
                            Landing_Runway = Follower_Landing_Runway,
                            Delivered_FAF_Separation)
  
  return(ORD_Observation)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: ORD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Clear_ORD_Observation <- function(con, PROC_Period, PROC_Criteria){
  
  # Build SQL Query
  Query <- "DELETE FROM tbl_ORD_Observation"
  
  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE Observation_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE Observation_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Execute Query
  sqlQuery(con, Query)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: ORD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Populate_ORD_Observation <- function(con, ORD_Observation){
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: WAD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_WAD_Observation <- function(con, LP_Primary_Key, Landing_Pair, Radar){
  
  # ----------------------------------------------- #
  # X.X.X Get Adaptation
  # ----------------------------------------------- #
  # 
  # ----------------------------------------------- #
  
  # ORD Adaptation
  ORD_Aircraft <- Load_Adaptation_Table(con, "tbl_ORD_Aircraft_Adaptation")
  ORD_Wake <- Load_Adaptation_Table(con, "tbl_ORD_Wake_Adaptation")
  ORD_DBS <- Load_Adaptation_Table(con, "tbl_ORD_DBS_Adaptation")
  
  # ORD Profile Selection Type.  
  ORD_Profile_Selection <- as.character(ADAP_Config$ORD_Profile_Selection)
  
  # Flag to remove all non-wake pairs.
  Wake_Pairs_Only <- as.logical(ADAP_Config$Process_Wake_Pairs_Only)
  
  # Flag to remove Not-In_Trail pairs. 
  In_Trail_Only <- as.logical(ADAP_Config$Process_In_Trail_Pairs_Only)
  
  # Path Leg Data
  Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")
  
  # Runway Adaptation
  Runway <- Load_Adaptation_Table(con, "tbl_Runway")
  ORD_Runway <- Load_Adaptation_Table(con, "tbl_ORD_Runway_Adaptation")
  
  # ----------------------------------------------- #
  # X.X.X Get Data
  # ----------------------------------------------- #
  # 
  # ----------------------------------------------- #
  
  # Get the Compression Commencement Thresholds
  CCTs <- Get_Compression_Distances(Landing_Pair, ORD_Aircraft, ORD_Wake, ORD_DBS, ORD_Profile_Selection, "CCT") %>%
    select(-Leader_Flight_Plan_ID)
  
  # Join on the CCTs and the LSTs
  Landing_Pair <- left_join(Landing_Pair, CCTs, by = c("Landing_Pair_ID"))
  
  # Get Leader WAD FAF Time (2nd Point after Local Stabilisation Distance, Ordered By Increasing Distance).
  Landing_Pair <- Get_Nth_Time_At_LP_Distance(Landing_Pair, Radar, N = 2, 
                                              Prefix = "WAD_FAF",
                                              Filter_Var = "Range_To_Threshold",
                                              Crit_Var = "Local_Stabilisation_Distance",
                                              LorF = "Leader", 
                                              OrderBy = "Range Ascending")
  
  # Get Leader Delivery Distance (1st Point after New Delivery, Ordered By Increasing Distance - Same as 0DME Currently)
  Landing_Pair <- Get_Compression_Commencement_Distances(Landing_Pair, Radar)
  
  # Get the Interpolated Follower Distances for ORD. (Between Leader Delivery and FAF Time) 
  Landing_Pair <- Get_Interpolated_Compression_Distances(Landing_Pair, Radar,
                                                         ORDorWAD = "WAD", 
                                                         Start_Time_Var = "Leader_CC_Time",
                                                         End_Time_Var = "Leader_WAD_FAF_Time")
  
  # TEMP: Remove FAF/CC Data if Not on ORD Supported Runway
  if (WAD_Enabled){
    Landing_Pair <- mutate(Landing_Pair,
                           Leader_WAD_FAF_Time = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_WAD_FAF_Time, NA),
                           Leader_WAD_FAF_RTT = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_WAD_FAF_RTT, NA),
                           Leader_CC_RTT = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_CC_RTT, NA),
                           Leader_CC_Time = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_CC_Time, NA))
  }
  
  
  # Get the Observed ORD Compression 
  Landing_Pair <- Get_Observed_Compression_Values(Landing_Pair, ORDorWAD = "WAD")
  
  # Split up Data into In-Trail and Not-In-Trail for Observed Speed/WE Calcs
  Landing_Pair_I <- Get_In_Trail(Landing_Pair)
  Landing_Pair_N <- Get_Not_In_Trail(Landing_Pair)
  
  # Get Observed Leader Parameters (In Trail)
  Landing_Pair_I <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_I, Radar, 
                                                           Prefix = "WAD",
                                                           LorF = "Leader",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_CC_Time",
                                                           End_Var = "Leader_WAD_FAF_Time")
  
  # Get Observed Follower Parameters (In Trail)
  Landing_Pair_I <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_I, Radar, 
                                                           Prefix = "WAD",
                                                           LorF = "Follower",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_CC_Time",
                                                           End_Var = "Leader_WAD_FAF_Time")
  
  # Get Observed Leader Parameters (Not In Trail)
  Landing_Pair_N <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_N, Radar, 
                                                           Prefix = "WAD",
                                                           LorF = "Leader",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_CC_Time",
                                                           End_Var = "Leader_WAD_FAF_Time")
  
  # Get Observed Follower Parameters (Not In Trail)
  Landing_Pair_N <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_N, Radar, 
                                                           Prefix = "WAD",
                                                           LorF = "Follower",
                                                           TimeorRange = "Time",
                                                           Start_Var = "Leader_CC_Time",
                                                           End_Var = "Leader_WAD_FAF_Time")
  
  # Bind Datasets Together and Reorder
  Landing_Pair <- rbind(Landing_Pair_I, Landing_Pair_N)
  Landing_Pair <- Order_Landing_Pairs(Landing_Pair, "Landing_Pair_ID")
  rm(Landing_Pair_I, Landing_Pair_N)
  
  return(Landing_Pair)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: WAD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Compare_WAD_Observation <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, INT_Landing_Pairs){
  
  ### ------------------  SQL Data Query ------------------------------------------------
  OO_Query <- "SELECT
  OO.Landing_Pair_ID,
  OO.Observed_Compression AS SQL_Observed_WAD_Compression,
  OO.Observed_Mean_Leader_IAS AS SQL_Observed_Leader_WAD_IAS,
  OO.Observed_Mean_Follower_IAS AS SQL_Observed_Follower_WAD_IAS,
  OO.Observed_Mean_Leader_Wind_Effect AS SQL_Observed_Leader_WAD_Wind_Effect,
  OO.Observed_Mean_Follower_Wind_Effect AS SQL_Observed_Follower_WAD_Wind_Effect,
  OO.Leader_CCT_Time AS SQL_Leader_CC_Time,
  OO.Leader_FAF_Time AS SQL_Leader_WAD_FAF_Time,
  OO.Leader_CCT_RTT AS SQL_Leader_CC_RTT,
  OO.Leader_FAF_RTT AS SQL_Leader_WAD_FAF_RTT,
  OO.Follower_Start_RTT AS SQL_Follower_ORD_Start_RTT,
  OO.Follower_Stop_RTT AS SQL_Follower_ORD_Stop_RTT
  FROM tbl_WAD_Observation OO"
  
  if (PROC_Period == "Day"){
    OO_Query <- paste0(OO_Query, " WHERE OO.Observation_Date = '", PROC_Criteria, "'")
  }
  
  if (PROC_Period == "Month"){
    OO_Query <- paste0(OO_Query, " WHERE OO.Observation_Date LIKE '%", PROC_Criteria, "%'")
  }
  ### ---------------------  Get R & SQL Data  ------------------------------------------
  
  # Set 0 Values to NA
  WAD_Observation_SQL <- sqlQuery(con, OO_Query, stringsAsFactors = F) %>%
    mutate(SQL_Observed_Leader_WAD_IAS = ifelse(SQL_Observed_Leader_WAD_IAS == 0, NA, SQL_Observed_Leader_WAD_IAS),
           SQL_Observed_Follower_WAD_IAS = ifelse(SQL_Observed_Follower_WAD_IAS == 0, NA, SQL_Observed_Follower_WAD_IAS),
           SQL_Observed_Leader_WAD_Wind_Effect = ifelse(SQL_Observed_Leader_WAD_Wind_Effect == 0, NA, SQL_Observed_Leader_WAD_Wind_Effect),
           SQL_Observed_Follower_WAD_Wind_Effect = ifelse(SQL_Observed_Follower_WAD_Wind_Effect == 0, NA, SQL_Observed_Follower_WAD_Wind_Effect))
  
  WAD_Observation_R <- select(Landing_Pair,
                              Landing_Pair_ID,
                              Landing_Pair_Date,
                              Observation_Flag,
                              Observed_WAD_Compression,
                              Observed_Leader_WAD_IAS,
                              Observed_Follower_WAD_IAS,
                              Observed_Leader_WAD_Wind_Effect,
                              Observed_Follower_WAD_Wind_Effect,
                              Leader_WAD_FAF_Time,
                              Leader_CC_Time,
                              Leader_WAD_FAF_RTT,
                              Leader_CC_RTT,
                              Follower_WAD_Start_RTT,
                              Follower_WAD_Stop_RTT)
  
  WAD_Observation_R <- filter(WAD_Observation_R, Observation_Flag == 0)
  
  ### -------------------- Create Comparison Table --------------------------------------
  
  # Full Join two Datasets
  ZCOMP_WAD_Observation <- full_join(WAD_Observation_SQL, WAD_Observation_R, by = c("Landing_Pair_ID"))
  
  ZCOMP_WAD_Observation <- ZCOMP_WAD_Observation %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_WAD_Compression", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Leader_WAD_IAS", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Follower_WAD_IAS", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Leader_WAD_Wind_Effect", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Follower_WAD_Wind_Effect", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_WAD_FAF_RTT", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_WAD_FAF_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_CC_RTT", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Leader_CC_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Follower_WAD_Start_RTT", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Follower_WAD_Stop_RTT", Tolerance = 0.01)
  
  ### -----------------------------------------------------------------------------------
  
  return(ZCOMP_WAD_Observation)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: WAD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Summary_WAD_Observation <- function(LP_Primary_Key, ZCOMP_WAD_Observation){
  
  # Summary Table
  ZSTAT_WAD_Observation <- group_by(ZCOMP_WAD_Observation, Landing_Pair_Date) %>% summarise(
    CNT_Observed_WAD_Compression = sum(FLAG_Observed_WAD_Compression, na.rm=T),
    CNT_Observed_Leader_WAD_IAS = sum(FLAG_Observed_Leader_WAD_IAS, na.rm=T),
    CNT_Observed_Follower_WAD_IAS = sum(FLAG_Observed_Follower_WAD_IAS, na.rm=T),
    CNT_Observed_Leader_WAD_Wind_Effect = sum(FLAG_Observed_Leader_WAD_Wind_Effect, na.rm=T),
    CNT_Observed_Follower_WAD_Wind_Effect = sum(FLAG_Observed_Follower_WAD_Wind_Effect, na.rm=T),
    CNT_Leader_WAD_FAF_Time = sum(FLAG_Leader_WAD_FAF_Time, na.rm=T),
    CNT_Leader_CC_Time = sum(FLAG_Leader_CC_Time, na.rm=T),
    CNT_Leader_WAD_FAF_RTT = sum(FLAG_Leader_WAD_FAF_RTT, na.rm=T),
    CNT_Leader_CC_RTT = sum(FLAG_Leader_CC_RTT, na.rm=T),
    CNT_Follower_WAD_Start_RTT = sum(FLAG_Follower_WAD_Start_RTT, na.rm=T),
    CNT_Follower_WAD_Stop_RTT = sum(FLAG_Follower_WAD_Stop_RTT, na.rm=T)) %>%
    ungroup()
  
  return(ZSTAT_WAD_Observation)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: WAD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Construct_WAD_Observation <- function(LP_Primary_Key, Landing_Pair){
  
  # Filter Observations
  Landing_Pair <- filter(Landing_Pair, Observation_Flag == 0)
  
  # Select Relevant Fields
  WAD_Observation <- select(Landing_Pair,
                            !!sym(LP_Primary_Key),
                            Observed_Compression = Observed_WAD_Compression,
                            Observed_Mean_Leader_IAS = Observed_Leader_WAD_IAS,
                            Observed_Mean_Follower_IAS = Observed_Follower_WAD_IAS,
                            Observed_Mean_Leader_Wind_Effect = Observed_Leader_WAD_Wind_Effect,
                            Observed_Mean_Follower_Wind_Effect = Observed_Follower_WAD_Wind_Effect,
                            Observed_AGI_Surface_Headwind,
                            Observation_Date = Landing_Pair_Date,
                            Leader_CCT_Time = Leader_CC_Time,
                            Leader_FAF_Time = Leader_WAD_FAF_Time,
                            Leader_CCT_RTT = Leader_CC_RTT,
                            Leader_FAF_RTT = Leader_WAD_FAF_RTT,
                            Follower_Start_RTT = Follower_WAD_Start_RTT,
                            Follower_Stop_RTT = Follower_WAD_Stop_RTT,
                            Leader_Callsign,
                            Follower_Callsign,
                            Follower_FAF_Time,
                            Follower_FAF_RTT,
                            Follower_Time_At_4DME,
                            Landing_Runway = Follower_Landing_Runway,
                            Delivered_CCT_Separation)
  
  return(WAD_Observation)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: WAD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Clear_WAD_Observation <- function(con, PROC_Period, PROC_Criteria){
  
  # Build SQL Query
  Query <- "DELETE FROM tbl_WAD_Observation"
  
  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE Observation_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE Observation_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Execute Query
  sqlQuery(con, Query)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: WAD Observation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Populate_WAD_Observation <- function(con, WAD_Observation){
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
