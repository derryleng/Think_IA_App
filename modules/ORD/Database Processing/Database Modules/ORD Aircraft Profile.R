# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.2 Generate ORD Aircraft Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
#
# Description: This script generates the ORD Aircraft Profile table and uploads to SQL, as well as saving a local
#              copy for further use in ORD. This table contains all the necessary adaptation/derived parameters to build
#              the IAS/GSPD profiles. It begins by joining on relevant ORD adaptation. This is dependent on the ORD_Profile_Type
#              in tbl_Adaptation_Data. If set to Aircraft_Type, all aircraft with ORD calibrated aircraft types
#              (in tbl_ORD_Aircraft_Adaptation) are matched, and the rest default to wake parameters (tbl_ORD_Wake_Adaptation).
#              The reference wake distances (and MRS if none such apply) are then matched such that if ORD_Profile_Type is set to
#              TBS_Table then we can use these as reference for joining on the DBS adaptation (tbl_ORD_DBS_Adaptation). These parameters
#              are used to calculate the Landing_Stabilisation_Speed, Final_Deceleration_Distance and Start_Initial_Deceleration_Distance
#              and all of these fields are combined to form the end table.
#
#
# Assumptions: The following data is assumed to be loaded: tbl_Adaptation_Data (Adaptation),
#              tbl_All_Pair_Reference_Data (Landing_Pair_Reference),
#
#
# Use Guide Section:
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
#
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: ORD Aircraft Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_ORD_Aircraft_Profile <- function(con, LP_Primary_Key, Landing_Pair, ORDBuffers, Use_EFDD, Use_Gust_Data, UseLSSModel, Precedences, ORD_Levels, LegacyorRecat){

  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  #message("Generating ORD Aircraft Profile Data...")

  # ----------------------------------------------- #
  # Get Adaptation
  # ----------------------------------------------- #
  #
  # ----------------------------------------------- #
  
  # Runway Adaptation
  ORD_Runway <- Load_Adaptation_Table(con, "tbl_ORD_Runway_Adaptation")
  
  # ORD Profile Selection.
  Adaptation_Main <- Load_Adaptation_Table(con, "tbl_Adaptation_Data")
  ORD_Profile_Selection <- as.character(Adaptation_Main$ORD_Profile_Selection)
  ORD_Use_All_Pairs <- T 
  
  # ----------------------------------------------- #
  # Add Filter Flags
  # ----------------------------------------------- #
  #
  # ----------------------------------------------- #
  
  # Copy of LP Key to save renaming
  LPID_Var <- LP_Primary_Key
  
  # Decide whether to perform prediction for all pairs or only pairs with observed compression.
  if (LP_Primary_Key == Get_LP_Primary_Key("Validation")){

    # Create List of IDs To Keep for Prediction
    if (ORD_Use_All_Pairs){Landing_Pair <- filter(Landing_Pair, Observation_Flag == 0)} else {Landing_Pair <- filter(Landing_Pair, ORD_Prediction_Flag == 0)}

  }

  # ----------------------------------------------- #
  # Build Aircraft Profile
  # ----------------------------------------------- #
  #
  # ----------------------------------------------- #
  
  # Build Leader Aircraft Profile
  Aircraft_Profile_Leader <- Build_Aircraft_Profile(Landing_Pair, LPID_Var, LorF = "Leader", ORD_Profile_Selection, ORDBuffers, UseLSSModel, Precedences, ORD_Levels, ORD_Runway, Use_EFDD, Use_Gust_Data, LegacyorRecat) %>%
    Calculate_Landing_Stabilisation_Speed(LPID_Var, UseLSSModel) %>%
    Calculate_Start_Initial_Decel_Distance() %>%
    Calculate_Final_Decel_Distance()
  
  # Build Follower Aircraft Profile
  Aircraft_Profile_Follower <- Build_Aircraft_Profile(Landing_Pair, LPID_Var, LorF = "Follower", ORD_Profile_Selection, ORDBuffers, UseLSSModel, Precedences, ORD_Levels, ORD_Runway, Use_EFDD, Use_Gust_Data, LegacyorRecat) %>%
    Calculate_Landing_Stabilisation_Speed(LPID_Var, UseLSSModel) %>%
    Calculate_Start_Initial_Decel_Distance() %>%
    Calculate_Final_Decel_Distance()

  # ----------------------------------------------- #
  # Setup
  # ----------------------------------------------- #
  #
  # ----------------------------------------------- #

  # Bind Data Together and Order
  Aircraft_Profile <- rbind(Aircraft_Profile_Leader, Aircraft_Profile_Follower) %>%
    arrange(!!sym(LPID_Var), desc(This_Pair_Role))

  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  #message(paste0("Generated Aircraft Profile Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))

  return(Aircraft_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: ORD Aircraft Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Construct_ORD_Aircraft_Profile <- function(LP_Primary_Key, Aircraft_Profile){
  
  ### THIS NEEDS SOME SELECT ALL_OF!!!!!
  
  # Select Appropriate Fields for Table
  ORD_Aircraft_Profile <- select(Aircraft_Profile,
                                 !!sym(LP_Primary_Key),
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

  # TEMP: Validation doesn't have CCT yet
  # if (LP_Primary_Key == Get_LP_Primary_Key("Validation")){ORD_Aircraft_Profile <- select(ORD_Aircraft_Profile,
  #                                                                                        -Compression_Commencement_Threshold)}

  return(ORD_Aircraft_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: ORD Aircraft Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Compare_ORD_Aircraft_Profile <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, Aircraft_Profile){

  if (LP_Primary_Key == Get_LP_Primary_Key("Validation")){LP_Date_Key <- "Landing_Pair_Date"}
  if (LP_Primary_Key == Get_LP_Primary_Key("Verification")){LP_Date_Key <- "Log_Date"}

  # Get tbl_ORD_Aircraft_Profile from SQL
  AP_Query <- "SELECT *
                  FROM tbl_ORD_Aircraft_Profile AP
                  INNER JOIN tbl_Landing_Pair LP
                  ON LP.Landing_Pair_ID = AP.Landing_Pair_ID"

  if (PROC_Period == "Day"){
    AP_Query <- paste0(AP_Query, " WHERE LP.Landing_Pair_Date = '", PROC_Criteria, "'")
  }

  if (PROC_Period == "Month"){
    AP_Query <- paste0(AP_Query, " WHERE LP.Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")
  }

  ORD_Aircraft_Profile_SQL <- dbGetQuery(con, AP_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                          "Landing_Pair_Type"))

  # Assume only testing Calculated Fields for now
  ORD_Aircraft_Profile_SQL <- select(ORD_Aircraft_Profile_SQL,
                                     !!sym(LP_Primary_Key),
                                     !!sym(LP_Date_Key),
                                     This_Pair_Role,
                                     SQL_Landing_Stabilisation_Speed = Landing_Stabilisation_Speed,
                                     SQL_Final_Deceleration_Distance = Final_Deceleration_Distance,
                                     SQL_Start_Initial_Deceleration_Distance = Start_Initial_Deceleration_Distance)

  # Get the Required Testing Fields from R Output
  ORD_Aircraft_Profile_R <- select(Aircraft_Profile,
                                   Landing_Pair_ID,
                                   This_Pair_Role,
                                   Landing_Stabilisation_Speed,
                                   Final_Deceleration_Distance = Start_Final_Deceleration_Distance,
                                   Start_Initial_Deceleration_Distance)

  # Join on R and SQL results ready for comparing
  ZCOMP_ORD_Aircraft_Profile <- full_join(ORD_Aircraft_Profile_R, ORD_Aircraft_Profile_SQL, by = c(setNames(LP_Primary_Key, LP_Primary_Key),
                                                                                                    setNames("This_Pair_Role", "This_Pair_Role")))

  # Make Difference and Flag variables
  ZCOMP_ORD_Aircraft_Profile <- ZCOMP_ORD_Aircraft_Profile %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Landing_Stabilisation_Speed", Tolerance = 0.001) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Final_Deceleration_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Start_Initial_Deceleration_Distance", Tolerance = 0.01)

  return(ZCOMP_ORD_Aircraft_Profile)

}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: ORD Aircraft Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Summary_ORD_Aircraft_Profile <- function(LP_Primary_Key, ZCOMP_ORD_Aircraft_Profile){

  # Make quick summary table by Landing Pair Date
  ZSTAT_ORD_Aircraft_Profile <- group_by(ZCOMP_ORD_Aircraft_Profile, Landing_Pair_Date) %>%
    summarise(CNT_Landing_Stabilisation_Speed = sum(FLAG_Landing_Stabilisation_Speed, na.rm = T),
              CNT_Final_Deceleration_Distance = sum(FLAG_Final_Deceleration_Distance, na.rm = T),
              CNT_Start_Initial_Deceleration_Distance = sum(FLAG_Start_Initial_Deceleration_Distance, na.rm = T))

  return(ZSTAT_ORD_Aircraft_Profile)

}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

