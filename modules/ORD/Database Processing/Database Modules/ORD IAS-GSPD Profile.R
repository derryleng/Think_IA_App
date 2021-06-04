# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 0.1. Global Parameters
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
#
# Description: Resource file for global parameters for the
#              tool.
#
# Use Guide Section:
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# v0: Added aviation unit conversion parameters: Time, speed, distance, acceleration, pressure
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: Full ORD GWCS Forecast
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_Full_ORD_GWCS_Forecast <- function(con, LP_Primary_Key, Forecast_Seg, Landing_Pair, Time_Key){

  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating GWCS Forecast Segment Data for ORD...")

  # Load Adaptation Tables
  GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")
  MAIN_Adaptation <- ADAP_Config
  Default_Wind <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Default_Wind_Effect_Segments")

  # Load Adaptation Values
  Seg_Size <- GWCS_Adaptation$DME_Seg_Size
  Forecast_Seg_Max <- GWCS_Adaptation$Forecast_Seg_Max
  Stale_Time <- GWCS_Adaptation$Forecast_Stale_Time
  Max_Seg_Extrapolation <- GWCS_Adaptation$Max_Seg_Extrapolation
  Extrapolation_Seg_Min <- GWCS_Adaptation$Extrapolation_Seg_Min
  Separation_Forecast_Seg_Max <- GWCS_Adaptation$Separation_Forecast_Seg_Max
  GWCS_Wind_Selection <- MAIN_Adaptation$GWCS_Wind_Selection

  # Prepare the Segment Data
  ORD_Segment_Forecast <- Prepare_Segment_Data("ORD", Landing_Pair, Seg_Size, Forecast_Seg_Max, 0)

  # Get the Non-Stale Segments
  ORD_Segment_Forecast <- Get_Non_Stale_Segments(ORD_Segment_Forecast, Forecast_Seg, Stale_Time)

  # Perform TBS Extrapolation
  ORD_Segment_Forecast <- TBS_Extrapolate_Segments(ORD_Segment_Forecast, Max_Seg_Extrapolation, Extrapolation_Seg_Min, Separation_Forecast_Seg_Max)

  # Perform ORD Extrapolation
  ORD_Segment_Forecast <- ORD_Extrapolate_Segments(ORD_Segment_Forecast, Forecast_Seg_Max, Separation_Forecast_Seg_Max)

  # Update Wind Effect Values based on GWCS Wind Selection.
  ORD_Segment_Forecast <- Treat_Default_Wind_Segments(ORD_Segment_Forecast, Default_Wind, GWCS_Wind_Selection)

  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Generated GWCS Segment Data for ORD in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))

  return(ORD_Segment_Forecast)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: ORD IAS Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_ORD_IAS_Profile <- function(con, LP_Primary_Key, Aircraft_Profile, Landing_Pair, ORD_Segment_Forecast){

  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating ORD IAS Profile Data...")

  # For the purposes of IAS Profile, Surface Headwind must be min 10kts.
  Aircraft_Profile <- mutate(Aircraft_Profile, Surface_Headwind = ifelse(Surface_Headwind < 10 * kts_To_mps, 10 * kts_To_mps, Surface_Headwind))

  # Get the Wind Effect Values at 0-4DME for the Airbus Sections
  Aircraft_Profile <- Get_Forecast_Wind_Effect_At_DME(ORD_Segment_Forecast, Aircraft_Profile, LP_Primary_Key, DME = 0)
  Aircraft_Profile <- Get_Forecast_Wind_Effect_At_DME(ORD_Segment_Forecast, Aircraft_Profile, LP_Primary_Key, DME = 1)
  Aircraft_Profile <- Get_Forecast_Wind_Effect_At_DME(ORD_Segment_Forecast, Aircraft_Profile, LP_Primary_Key, DME = 2)
  Aircraft_Profile <- Get_Forecast_Wind_Effect_At_DME(ORD_Segment_Forecast, Aircraft_Profile, LP_Primary_Key, DME = 3)
  Aircraft_Profile <- Get_Forecast_Wind_Effect_At_DME(ORD_Segment_Forecast, Aircraft_Profile, LP_Primary_Key, DME = 4)

  # Get the Leader/Follower Aircraft Profiles.
  Aircraft_Profile_Leader <- Split_Leader_Follower(Aircraft_Profile, "Leader")
  Aircraft_Profile_Follower <- Split_Leader_Follower(Aircraft_Profile, "Follower")

  # Get the Runway Variables and Adjusted Airbus Speeds/Decelerations.
  ## NOTE: Validation uses Leader runway for both - Verification does not.
  Aircraft_Profile_Leader <- Get_ORD_Runway_Parameters(Aircraft_Profile_Leader, Landing_Pair, LP_Primary_Key, LorFRunway = "Leader")
  if (LP_Primary_Key == Get_LP_Primary_Key("Validation")){
    Aircraft_Profile_Follower <- Get_ORD_Runway_Parameters(Aircraft_Profile_Follower, Landing_Pair, LP_Primary_Key, LorFRunway = "Follower")} else {
      Aircraft_Profile_Follower <- Get_ORD_Runway_Parameters(Aircraft_Profile_Follower, Landing_Pair, LP_Primary_Key, LorFRunway = "Follower")
    }

  # ----------------------------------------------- #
  # 0.1.1 Build the IAS Profile
  # ----------------------------------------------- #
  # IAS Profile Functions in ORD Functions.R
  # ----------------------------------------------- #

  # Build all IAS Leader Profiles
  IAS_Profile_Leader <- Build_Full_IAS_Profile(Aircraft_Profile_Leader, LP_Primary_Key)

  # Build all IAS Follower Profiles
  IAS_Profile_Follower <- Build_Full_IAS_Profile(Aircraft_Profile_Follower, LP_Primary_Key)

  # Bind together and order.
  IAS_Profile <- rbind(IAS_Profile_Leader, IAS_Profile_Follower) %>%
    arrange(!!sym(LP_Primary_Key), desc(This_Pair_Role), End_Dist)

  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Generated ORD IAS Profile Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))

  return(IAS_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: ORD IAS Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Compare_ORD_IAS_Profile <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, ORD_IAS_Profile){

  #ORD_IAS_Profile_R <- filter(ORD_IAS_Profile, Landing_Pair_ID %in% ORD_Prediction_IDs)
  ORD_IAS_Profile_R <- ORD_IAS_Profile

  IAS_Query <- "SELECT *
                 FROM tbl_ORD_IAS_Profile GS
                 INNER JOIN tbl_Landing_Pair LP
                 ON LP.Landing_Pair_ID = GS.Landing_Pair_ID"


  if (PROC_Period == "Day"){
    IAS_Query <- paste0(IAS_Query, " WHERE LP.Landing_Pair_Date = '", PROC_Criteria, "'")
  }

  if (PROC_Period == "Month"){
    IAS_Query <- paste0(IAS_Query, " WHERE LP.Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")
  }

  ORD_IAS_Profile_SQL <- dbGetQuery(con, IAS_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                      "Landing_Pair_Type"))

  ORD_IAS_Profile_SQL <- select(ORD_IAS_Profile_SQL,
                                !!sym(LP_Primary_Key),
                                This_Pair_Role,
                                Section_Number,
                                Landing_Pair_Date,
                                SQL_Profile_Section = Profile_Section,
                                SQL_Profile_Type = Profile_Type,
                                SQL_Start_IAS = Start_IAS,
                                SQL_End_IAS = End_IAS,
                                SQL_Start_Dist = Start_Dist,
                                SQL_End_Dist = End_Dist)

  # Join Together
  ZCOMP_ORD_IAS_Profile <- full_join(ORD_IAS_Profile_SQL, ORD_IAS_Profile_R, by = c("Landing_Pair_ID", "This_Pair_Role", "Section_Number"))

  # Add Comparison Fields
  ZCOMP_ORD_IAS_Profile <- ZCOMP_ORD_IAS_Profile %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Profile_Section", Tolerance = NA) %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Profile_Type", Tolerance = NA) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Start_IAS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "End_IAS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Start_Dist", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "End_Dist", Tolerance = 0.01)

  return(ZCOMP_ORD_IAS_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: ORD IAS Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Summary_ORD_IAS_Profile <- function(LP_Primary_Key, ZCOMP_ORD_IAS_Profile){

  # Make Summary Stats
  ZSTAT_ORD_IAS_Profile <- group_by(ZCOMP_ORD_IAS_Profile, Landing_Pair_Date) %>%
    summarise(CNT_Profile_Section = sum(FLAG_Profile_Section, na.rm = T),
              CNT_Profile_Type = sum(FLAG_Profile_Type, na.rm = T),
              CNT_Start_IAS = sum(FLAG_Start_IAS, na.rm = T),
              CNT_End_IAS = sum(FLAG_End_IAS, na.rm = T),
              CNT_Start_Dist = sum(FLAG_Start_Dist, na.rm = T),
              CNT_End_Dist = sum(FLAG_End_Dist, na.rm = T))

  return(ZSTAT_ORD_IAS_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: ORD IAS Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Construct_ORD_IAS_Profile <- function(LP_Primary_Key, IAS_Profile){

  ORD_IAS_Profile <- IAS_Profile

  return(ORD_IAS_Profile)

}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: ORD IAS Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Clear_ORD_IAS_Profile <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria){

  #PROC_Period <- "Month"
  #PROC_Criteria <- "11/2018"
  #LP_Primary_Key <- "ORD_Tool_Calculation_ID"

  # Get the Variables (If Validation)
  if (Get_LP_Primary_Key("Validation") == LP_Primary_Key){
    Date_Var <- "Landing_Pair_Date"
    Table_Name <- "tbl_Landing_Pair"
  }

  # Get the Variables (If Verification)
  if (Get_LP_Primary_Key("Verification") == LP_Primary_Key){
    Date_Var <- "Log_Date"
    Table_Name <- "tbl_ORD_Tool_Calculation"
  }

  # Build SQL Query
  Query <- "DELETE IAS "

  # If Done by Month/Day
  Query <- paste0(Query, "FROM tbl_ORD_IAS_Profile IAS LEFT JOIN ", Table_Name, " LP ON IAS.", LP_Primary_Key, " = LP.", LP_Primary_Key)

  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE LP.", Date_Var,  " = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE LP.", Date_Var, " LIKE '%", PROC_Criteria, "%'")}

  # Execute Query
  dbGetQuery(con, Query)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: ORD IAS Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Populate_ORD_IAS_Profile <- function(con, ORD_IAS_Profile){

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: ORD GSPD Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# We need to join the ORD wind segments to the IAS
# profile. The GS profile has a section for each
# wind segment and "partial" sections for the
# boundaries of the IAS profile that are within segment
# boundaries. First, we loop through each type of section
# filter the IAS profile for this section and join on
# IAS values/distances to Wind segments. We then find
# if a wind segment is "within" this section, or "on top"
# or "on bottom" using flags. If within, these segments are
# used directly. If on topo/bottom, the start/end distances
# of segment are adjusted to match IAS profile boundaries
# and added too. IAS speeds are then adjusted by using
# y = mx + c. Start/End GS is then calculated by adding
# the Forecast Wind Effect to Start/End IAS.
# ------------------------------------------------------------------------------------------------------------------------------------------ #

Generate_ORD_GSPD_Profile <- function(con, LP_Primary_Key, IAS_Profile, ORD_Segment_Forecast, Seg_Size){
  
  LPID_Var <- LP_Primary_Key

  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating ORD GSPD Profile Data...")

  # Make the start and end dist from DME_Seg + Seg_Size
  ORD_Segment_Forecast <- rename(ORD_Segment_Forecast, End_Dist_Wind = DME_Seg) %>%
    mutate(Start_Dist_Wind = End_Dist_Wind + Seg_Size)

  # Get the Leader/Follower IAS Profiles
  IAS_Profile_Leader <- Split_Leader_Follower(IAS_Profile, "Leader")
  IAS_Profile_Follower <- Split_Leader_Follower(IAS_Profile, "Follower")

  # Attempt at building
  GS_Profile_Leader <- Build_GSPD_Profile(IAS_Profile_Leader, ORD_Segment_Forecast, LPID_Var)
  GS_Profile_Follower <- Build_GSPD_Profile(IAS_Profile_Follower, ORD_Segment_Forecast, LPID_Var)

  # Build complete GSPD and Order
  ORD_GS_Profile <- rbind(GS_Profile_Leader, GS_Profile_Follower) %>%
    arrange(!!sym(LPID_Var), desc(This_Pair_Role), Section_Number)

  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Generated ORD GSPD Profile Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))

  return(ORD_GS_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: ORD GSPD Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Compare_ORD_GSPD_Profile <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, GSPD_Profile){

  # ----------------------------------------------- #
  # Get the GS Profile R Data
  #ORD_GS_Profile_R <- filter(GSPD_Profile, Landing_Pair_ID %in% ORD_Prediction_IDs)
  ORD_GS_Profile_R <- GSPD_Profile
  # ----------------------------------------------- #

  # ----------------------------------------------- #
  # Get the GS Profile From SQL
  GS_Query <- "SELECT *
                 FROM tbl_ORD_GS_Profile GS
                 INNER JOIN tbl_Landing_Pair LP
                 ON LP.Landing_Pair_ID = GS.Landing_Pair_ID"


  if (PROC_Period == "Day"){
    GS_Query <- paste0(GS_Query, " WHERE LP.Landing_Pair_Date = '", PROC_Criteria, "'")
  }

  if (PROC_Period == "Month"){
    GS_Query <- paste0(GS_Query, " WHERE LP.Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")
  }

  ORD_GS_Profile_SQL <- dbGetQuery(con, GS_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                    "Landing_Pair_Type"))

  ORD_GS_Profile_SQL <- select(ORD_GS_Profile_SQL,
                               Landing_Pair_ID,
                               This_Pair_Role,
                               Section_Number,
                               Landing_Pair_Date,
                               SQL_Profile_Section = Profile_Section,
                               SQL_Profile_Type = Profile_Type,
                               SQL_Start_IAS = Start_IAS,
                               SQL_End_IAS = End_IAS,
                               SQL_Start_GS = Start_GS,
                               SQL_End_GS = End_GS,
                               SQL_Start_Dist = Start_Dist,
                               SQL_End_Dist = End_Dist)
  # ----------------------------------------------- #

  # Join Together
  ZCOMP_ORD_GS_Profile <- full_join(ORD_GS_Profile_SQL, ORD_GS_Profile_R, by = c("Landing_Pair_ID", "This_Pair_Role", "Section_Number"))

  ZCOMP_ORD_GS_Profile <- ZCOMP_ORD_GS_Profile %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Profile_Section", Tolerance = NA) %>%
    Add_Test_Variable(Type = "Categoric", Parameter = "Profile_Type", Tolerance = NA) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Start_IAS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "End_IAS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Start_GS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "End_GS", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Start_Dist", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "End_Dist", Tolerance = 0.01)



  return(ZCOMP_ORD_GS_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: ORD GSPD Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Summary_ORD_GSPD_Profile <- function(LP_Primary_Key, ZCOMP_ORD_GSPD_Profile){

  # Make Summary Stats
  ZSTAT_ORD_GS_Profile <- group_by(ZCOMP_ORD_GSPD_Profile, Landing_Pair_Date) %>%
    summarise(CNT_Profile_Section = sum(FLAG_Profile_Section, na.rm = T),
              CNT_Profile_Type = sum(FLAG_Profile_Type, na.rm = T),
              CNT_Start_IAS = sum(FLAG_Start_IAS, na.rm = T),
              CNT_End_IAS = sum(FLAG_End_IAS, na.rm = T),
              CNT_Start_GS = sum(FLAG_Start_GS, na.rm = T),
              CNT_End_GS = sum(FLAG_End_GS, na.rm = T),
              CNT_Start_Dist = sum(FLAG_Start_Dist, na.rm = T),
              CNT_End_Dist = sum(FLAG_End_Dist, na.rm = T))

  return(ZSTAT_ORD_GS_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: ORD GSPD Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Construct_ORD_GSPD_Profile <- function(LP_Primary_Key, GSPD_Profile){

  ORD_GSPD_Profile <- GSPD_Profile

  return(ORD_GSPD_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: ORD GSPD Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Clear_ORD_GSPD_Profile <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria){

  #PROC_Period <- "Month"
  #PROC_Criteria <- "11/2018"
  #LP_Primary_Key <- "ORD_Tool_Calculation_ID"

  # Get the Variables (If Validation)
  if (Get_LP_Primary_Key("Validation") == LP_Primary_Key){
    Date_Var <- "Landing_Pair_Date"
    Table_Name <- "tbl_Landing_Pair"
  }

  # Get the Variables (If Verification)
  if (Get_LP_Primary_Key("Verification") == LP_Primary_Key){
    Date_Var <- "Log_Date"
    Table_Name <- "tbl_ORD_Tool_Calculation"
  }

  # Build SQL Query
  Query <- "DELETE GS "

  # If Done by Month/Day
  Query <- paste0(Query, "FROM tbl_ORD_GS_Profile GS LEFT JOIN ", Table_Name, " LP ON GS.", LP_Primary_Key, " = LP.", LP_Primary_Key)

  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE LP.", Date_Var,  " = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE LP.", Date_Var, " LIKE '%", PROC_Criteria, "%'")}

  # Execute Query
  dbGetQuery(con, Query)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: ORD GSPD Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Populate_ORD_GSPD_Profile <- function(con, ORD_GSPD_Profile){

}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
