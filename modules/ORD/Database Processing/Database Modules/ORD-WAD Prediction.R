# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.6 Generate ORD/WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Uses tbl_ORD_GS_Profile, tbl_ORD_Aircraft_Profile, tbl_All_Pair_Reference_Data, tbl_ORD_Observation, 
#              tbl_WAD_Observation and potentially new table tbl_ORD_TBS_Distances. Using the GS table as a base,
#              this procedure 1) finds the leader time flown 2) finds the follower distance flown in leader time 
#              3) calculates compression = follower distance - leader distance and Mean L/F IAS/WE across flown sections
#              using trapezium rule.
#               
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# v0: Constructed unsorted working tmemplate. Calculates the same ORD_Compression, ORD_Mean_IAS, Forecast_Mean_Wind_Effects as
#     the database. Requires update to account for configurable Leader delivery time, and partial final leader section, similarly
#     to the follower method.
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Updates Required/Theorized
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# - !!!Are we using TBS distances or Observed Delivery Separation?
# - General tidyup. (Commenting, Variable names, Sectioning) - DONE
# - Take account for configurable delivery distance. Partial final leader section. - DONE
# - Take account for partial initial leader section (FAF). (Just in case Runway FAF is used) - DONE
# - Linking. Link to rest of procedures for seamless runflow.
# - Further modularise and place functions in ORD Functions.R, change to run functions
# - Create WAD version. (Leader: FAF - CC, Follower: FAF + ORD_Compression + ORD_Separation_Distance - Follower_WAD_End)
# - Populate Error Variables
# - Join on relevant variables from other tables
# - Get format of ORD/WAD Prediction
# - Save to SQL/Local
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: ORD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_ORD_Prediction <- function(con, LP_Primary_Key, Landing_Pair, ORD_GS_Profile){
  
  LPID_Var <- LP_Primary_Key
  
  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating ORD Predicted Parameter Data...")

  # Order ORD GS Profile 
  ORD_GS_Profile <- arrange(ORD_GS_Profile, !!sym(LPID_Var), desc(This_Pair_Role), Section_Number)
  
    # First Calculate Compression for ORD
    Landing_Pair <- Get_Forecast_ORD_Parameters(ORD_GS_Profile, Landing_Pair, LP_Primary_Key, 
                                                Prefix = "ORD",
                                                Comp_End_Var = "Delivery_Distance",
                                                Comp_Start_Var = "Local_Stabilisation_Distance",
                                                Sep_Dist_Var = "ORD_Separation_Distance")
    
    # Calculate Error Variables
    Landing_Pair <- Get_Prediction_Error_Variables(Landing_Pair, "ORD")
    
    # Calculate Surface Wind Error
    Landing_Pair <- mutate(Landing_Pair, Forecast_AGI_Surface_Headwind_Error = Observed_AGI_Surface_Headwind - Forecast_AGI_Surface_Headwind)
  
    # How long did it take?
    Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
    message(paste0("Generated ORD Predicted Parameter Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
    
  return(Landing_Pair)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: ORD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #

Construct_ORD_Prediction <- function(LP_Primary_Key, Landing_Pair){
  
  # Filter for Type 0 Prdiction Flag
  Landing_Pair <- filter(Landing_Pair, ORD_Prediction_Flag == 0)
  
  # Select Relevant Fields for SQL Table
  ORD_Prediction <- select(Landing_Pair,
                           !!sym(LP_Primary_Key),
                           ORD_Compression = Forecast_ORD_Compression,
                           ORD_Compression_Error,
                           ORD_Mean_Leader_IAS = Forecast_Leader_ORD_IAS,
                           ORD_Leader_IAS_Error = Forecast_Leader_ORD_IAS_Error,
                           ORD_Mean_Follower_IAS = Forecast_Follower_ORD_IAS,
                           ORD_Follower_IAS_Error= Forecast_Follower_ORD_IAS_Error,
                           Forecast_Mean_Leader_Wind_Effect = Forecast_Leader_ORD_Wind_Effect,
                           Forecast_Mean_Leader_Wind_Effect_Error = Forecast_Leader_ORD_Wind_Effect_Error,
                           Forecast_Mean_Follower_Wind_Effect = Forecast_Follower_ORD_Wind_Effect,
                           Forecast_Mean_Follower_Wind_Effect_Error = Forecast_Follower_ORD_Wind_Effect_Error,
                           Forecast_AGI_Surface_Headwind,
                           Forecast_AGI_Surface_Headwind_Error,
                           Prediction_Time,
                           Leader_Distance_To_Threshold = Local_Stabilisation_Distance,
                           ORD_Separation_Distance,
                           DBS_All_Sep_Distance)
  
  return(ORD_Prediction)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: ORD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Compare_ORD_Prediction <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, Landing_Pair){
  
  # Filter for ORD PRediction Flag
  Landing_Pair <- filter(Landing_Pair, Observation_Flag == 0)
  
  # Setup Queries for ORD/WAD Prediction
  ORD_Prediction_SQL_Query <- "SELECT 
                                 OP.Landing_Pair_ID,
                                 LP.Landing_Pair_Date,
                                 OP.ORD_Compression AS SQL_Forecast_ORD_Compression,
                                 OP.ORD_Mean_Leader_IAS AS SQL_Forecast_Leader_ORD_IAS,
                                 OP.ORD_Mean_Follower_IAS AS SQL_Forecast_Follower_ORD_IAS,
                                 OP.Forecast_Mean_Leader_Wind_Effect AS SQL_Forecast_Leader_ORD_Wind_Effect,
                                 OP.Forecast_Mean_Follower_Wind_Effect AS SQL_Forecast_Follower_ORD_Wind_Effect,
                                 OP.DBS_All_Sep_Distance AS SQL_DBS_All_Sep_Distance,
                                 OP.Prediction_Time AS SQL_Prediction_Time,
                                 OP.Forecast_AGI_Surface_Headwind AS SQL_Forecast_AGI_Surface_Headwind,
                                 OP.ORD_Separation_Distance AS SQL_ORD_Separation_Distance
                               FROM tbl_ORD_Prediction OP
                               INNER JOIN tbl_Landing_Pair LP
                               ON LP.Landing_Pair_ID = OP.Landing_Pair_ID"
  
  
  if (PROC_Period == "Day"){
    ORD_Prediction_SQL_Query <- paste0(ORD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date = '", PROC_Criteria, "'")
  }
  
  if (PROC_Period == "Month"){
    ORD_Prediction_SQL_Query <- paste0(ORD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")
  }
  
  # Load Required Test ORD PRediction Data
  ORD_Prediction_SQL <- sqlQuery(con, ORD_Prediction_SQL_Query, stringsAsFactors = F)
  
  # Select Relevant Fields from R Calculation
  ORD_Prediction_R <- select(Landing_Pair,
                             Landing_Pair_ID,
                             Forecast_ORD_Compression,
                             Forecast_Leader_ORD_IAS,
                             Forecast_Follower_ORD_IAS,
                             Forecast_Leader_ORD_Wind_Effect,
                             Forecast_Follower_ORD_Wind_Effect,
                             DBS_All_Sep_Distance,
                             Prediction_Time,
                             Forecast_AGI_Surface_Headwind,
                             ORD_Separation_Distance)
  
  # Join Sets Together
  ZCOMP_ORD_Prediction <- full_join(ORD_Prediction_SQL, ORD_Prediction_R, by = c("Landing_Pair_ID"))
  
  # Add Comparison Fields
  ZCOMP_ORD_Prediction <- ZCOMP_ORD_Prediction %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_ORD_Compression", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Leader_ORD_IAS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Follower_ORD_IAS", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Leader_ORD_Wind_Effect", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Follower_ORD_Wind_Effect", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "DBS_All_Sep_Distance", Tolerance = 0.0001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Prediction_Time", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_AGI_Surface_Headwind", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "ORD_Separation_Distance", Tolerance = 0.01)
  
  return(ZCOMP_ORD_Prediction)
    
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: ORD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Summary_ORD_Prediction <- function(LP_Primary_Key, ZCOMP_ORD_Prediction){

  # Create Summary
  ZSTAT_ORD_Prediction <- group_by(ZCOMP_ORD_Prediction, Landing_Pair_Date) %>% summarise(
    CNT_Forecast_Leader_ORD_IAS = sum(FLAG_Forecast_Leader_ORD_IAS, na.rm = T),
    CNT_Forecast_Follower_ORD_IAS = sum(FLAG_Forecast_Follower_ORD_IAS, na.rm = T),
    CNT_Forecast_Leader_ORD_Wind_Effect = sum(FLAG_Forecast_Leader_ORD_Wind_Effect, na.rm = T),
    CNT_Forecast_Follower_ORD_Wind_Effect = sum(FLAG_Forecast_Follower_ORD_Wind_Effect, na.rm = T),
    CNT_Forecast_ORD_Compression = sum(FLAG_Forecast_ORD_Compression, na.rm = T),
    CNT_DBS_All_Sep_Distance = sum(FLAG_DBS_All_Sep_Distance, na.rm = T),
    CNT_Prediction_Time = sum(FLAG_Prediction_Time, na.rm = T),
    CNT_Forecast_AGI_Surface_Headwind = sum(FLAG_Forecast_AGI_Surface_Headwind, na.rm = T),
    CNT_ORD_Separation_Distance = sum(FLAG_ORD_Separation_Distance, na.rm = T)) %>% ungroup()
  
  return(ZSTAT_ORD_Prediction)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: ORD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Clear_ORD_Prediction <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria){
  
  #PROC_Period <- "Month"
  #PROC_Criteria <- "11/2018"
  #LP_Primary_Key <- "ORD_Tool_Calculation_ID"

  # Build SQL Query
  Query <- "DELETE OP "
  
  # If Done by Month/Day
  Query <- paste0(Query, "FROM tbl_ORD_Prediction OP LEFT JOIN tbl_Landing_Pair LP ON OP.", LP_Primary_Key, " = LP.", LP_Primary_Key)
  
  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE LP.Landing_Pair_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE LP.Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Execute Query
  sqlQuery(con, Query)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: ORD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Populate_ORD_Prediction <- function(con, ORD_Prediction){
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Need to add Delivered_CC_Separation


# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_WAD_Prediction <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, Landing_Pair){
    
    # If WAD is Enabled, Use Results to Get WAD Separation Distance
    Landing_Pair <- mutate(Landing_Pair, WAD_Separation_Distance = ORD_Separation_Distance + Forecast_ORD_Compression)
    
    # Get the Leader WAD Start Distance based on the Min Value of CCT and Leader_CC_RTT (so for a CCT of 10, Max 10NM)
    Landing_Pair <- Get_Min_Valid_Value_2Var(Landing_Pair, "Leader_WAD_Start_Distance", "Compression_Commencement_Threshold", "Leader_CC_RTT")
    
    # Get the WAD Compression using these Parameters
    Landing_Pair <- Get_Forecast_Compression(ORD_GS_Profile, Landing_Pair, LPID_Var, 
                                             Prefix = "WAD",
                                             Comp_End_Var = "Local_Stabilisation_Distance",
                                             Comp_Start_Var = "Leader_WAD_Start_Distance",
                                             Sep_Dist_Var = "WAD_Separation_Distance")
    
    # Get WAD Prediction Error Variables
    Landing_Pair <- Get_Prediction_Error_Variables(Landing_Pair, "WAD")
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Compare_WAD_Prediction <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, Landing_Pair){

  WAD_Prediction_SQL_Query <- "SELECT 
                                   WP.Landing_Pair_ID,
                                   LP.Landing_Pair_Date,
                                   WP.WAD_Compression AS SQL_Forecast_WAD_Compression,
                                   WP.WAD_Mean_Leader_IAS AS SQL_Forecast_Leader_WAD_IAS,
                                   WP.WAD_Mean_Follower_IAS AS SQL_Forecast_Follower_WAD_IAS,
                                   WP.Forecast_Mean_Leader_Wind_Effect AS SQL_Forecast_Leader_WAD_Wind_Effect,
                                   WP.Forecast_Mean_Follower_Wind_Effect AS SQL_WAD_Forecast_Follower_WAD_Wind_Effect
                                 FROM tbl_WAD_Prediction WP
                                 INNER JOIN tbl_Landing_Pair LP
                                 ON LP.Landing_Pair_ID = WP.Landing_Pair_ID"
  
  if (PROC_Period == "Day"){
    WAD_Prediction_SQL_Query <- paste0(WAD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date = '", PROC_Critera, "'")
  }
  
  if (PROC_Period == "Month"){
    WAD_Prediction_SQL_Query <- paste0(WAD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")
  }
  
  
  # Load Required Test WAD PRediction Data
  WAD_Prediction_SQL <- sqlQuery(con, WAD_Prediction_SQL_Query, stringsAsFactors = F)
  
  # Select Relevant Fields from R Calculation
  WAD_Prediction_R <- select(Landing_Pair_Reference,
                             Landing_Pair_ID,
                             Forecast_Leader_WAD_IAS,
                             Forecast_Follower_WAD_IAS,
                             Forecast_Leader_WAD_Wind_Effect,
                             Forecast_Follower_WAD_Wind_Effect,
                             Forecast_WAD_Compression)
  
  # Join Sets Together
  ZCOMP_WAD_Prediction <- full_join(WAD_Prediction_SQL, WAD_Prediction_R, by = c("Landing_Pair_ID"))
  
  # Add Comparison Fields
  ZCOMP_WAD_Prediction <- ZCOMP_WAD_Prediction %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_WAD_Compression", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Leader_WAD_IAS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Follower_WAD_IAS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Leader_WAD_Wind_Effect", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Forecast_Follower_WAD_Wind_Effect", Tolerance = 0.001) 
  
  return(ZCOMP_WAD_Prediction)

}



# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Summary_WAD_Prediction <- function(LP_Primary_Key, ZCOMP_WAD_Prediction){
  
  # Create Summary
  ZSTAT_WAD_Prediction <- group_by(ZCOMP_WAD_Prediction, Landing_Pair_Date) %>%
    summarise(ZCOMP_WAD_Prediction,
              CNT_Forecast_Leader_WAD_IAS = sum(FLAG_Forecast_Leader_WAD_IAS, na.rm = T),
              CNT_Forecast_Follower_WAD_IAS = sum(FLAG_Forecast_Follower_WAD_IAS, na.rm = T),
              CNT_Forecast_Leader_WAD_Wind_Effect = sum(FLAG_Forecast_Leader_WAD_Wind_Effect, na.rm = T),
              CNT_Forecast_Follower_WAD_Wind_Effect = sum(FLAG_Forecast_Follower_WAD_Wind_Effect, na.rm = T),
              CNT_Forecast_WAD_Compression = sum(FLAG_Forecast_WAD_Compression, na.rm = T))
  
  return(ZSTAT_WAD_Prediction)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Clear_WAD_Prediction <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria){
  
  #PROC_Period <- "Month"
  #PROC_Criteria <- "11/2018"
  #LP_Primary_Key <- "ORD_Tool_Calculation_ID"
  
  # Build SQL Query
  Query <- "DELETE WP "
  
  # If Done by Month/Day
  Query <- paste0(Query, "FROM tbl_WAD_Prediction WP LEFT JOIN tbl_Landing_Pair LP ON WP.", LP_Primary_Key, " = LP.", LP_Primary_Key)
  
  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE LP.Landing_Pair_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE LP.Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Execute Query
  sqlQuery(con, Query)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Populate_WAD_Prediction <- function(con, WAD_Prediction){
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: ORD Calculation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Generate_ORD_Calculation <- function(){

  # Verification Procedure
  if (LP_Primary_Key == Get_LP_Primary_Key("Verification")){
    
    # Get the Delivery Distance for the Compression End
    Landing_Pair <- mutate(Landing_Pair, Delivery_Distance = New_Delivery)
    
    # Get the Forecast ORD For Verification
    Landing_Pair <- Get_Forecast_Compression(ORD_GS_Profile, Landing_Pair, LP_Primary_Key, 
                                             Prefix = "ORD", 
                                             Comp_End_Var = "Delivery_Distance", 
                                             Comp_Start_Var = "Leader_Distance_To_Threshold", 
                                             Sep_Dist_Var = "ORD_Separation_Distance")
    
    # Landing Pair Reference must be made Verification friendly as Prediction Time/SW Needed
    # for Aircraft Profile
  
  }

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: ORD Calculation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: ORD Calculation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: ORD Calculation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: ORD Calculation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: ORD Calculation
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #



