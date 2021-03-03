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

# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # this will need editing to find wd outside of RStudio

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Start Time
#Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Source Global Configuration Files (Requires Shiny Integration)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path("0. Global Functions & Parameters", "0.0. Imports.R"))
source(file.path("0. Global Functions & Parameters", "0.1. Global Parameters.R"))
source(file.path("0. Global Functions & Parameters", "0.2. Global Functions.R"))
# ----------------------------------------------------------------------------------------------------------------------------------------- #
#--------------------------------------------------------------------------------------------------------------------------------- #
# Configuration
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Database_Type <- "Validation"
Database <- "LVNL_UTMA_Validation"
IP <- "192.168.1.39"
con <- Get_RODBC_Database_Connection(IP, Database)
PROC_Period <- "Day"
PROC_Criteria <- "19/07/2019"
# ----------------------------------------------------------------------------------------------------------------------------------------- #

Airfield <- "EHAM"

Radar_Query <- "SELECT 
RTP.Radar_Track_Point_ID,
Flight_Plan_ID,
Track_Date,
Track_Time,
Mode_S_Wind_Localiser_Capture AS Approach_Path,
Range_To_Threshold,
Wind_Effect_IAS
FROM tbl_Radar_Track_Point RTP
JOIN tbl_Radar_Track_Point_Derived RTPD
ON RTP.Radar_Track_Point_ID = RTPD.Radar_Track_Point_ID"

Radar_Query <- paste0(Radar_Query, " WHERE Track_Date = '", PROC_Criteria, "'")

Radar <- sqlQuery(con, Radar_Query, stringsAsFactors = F)
FP <- sqlQuery(con, "SELECT Flight_Plan_ID, Time_At_4DME FROM tbl_Flight_Plan_Derived")
Radar <- left_join(Radar, FP, by="Flight_Plan_ID")

GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")



# ----------------------------------------------------------------------------------------------------------------------------------------- #

## Global Flag procedure (Stage 1 filtering)

Generate_GWCS_Stage_1_Filters <- function(con, Radar, Operation, Activity){
  
  # Get number of columns in Radar. To generalise Global Flag.
  Radar_Col_Before <- ncol(Radar)
  
  # -- Operation/Activity Dependent -- #
  # Firstly, load the GWCS adaptation table
  GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")
  Adaptation <- Load_Adaptation_Table(con, "tbl_Adaptation_Data")
  
  # Get the Max Mode S Data Age
  Max_Age <- as.numeric(Adaptation$Max_Mode_S_Data_Age)
  # ---------------------------------- # 
  
  # Get flags for Stage 1 Filtering Criteria
  Radar <- Check_Stage_1_Filter_Mode_S_GSPD(Radar, GWCS_Adaptation, Operation, Activity)
  Radar <- Check_Stage_1_Filter_Mode_S_IAS(Radar, GWCS_Adaptation, Operation, Activity)
  Radar <- Check_Stage_1_Filter_Mode_S_TAS(Radar, GWCS_Adaptation, Operation, Activity)
  Radar <- Check_Stage_1_Filter_Mode_S_Roll_Angle(Radar, GWCS_Adaptation, Operation, Activity)
  Radar <- Check_Stage_1_Filter_Altitude_Difference(Radar, GWCS_Adaptation, Operation, Activity)
  
  # If necessary, get flags for Age criteria (Currently done in the Loading process for Validation)
  Radar <- Check_Mode_S_Data_Age(Radar, Max_Age, "Mode_S_GSPD", Operation, Activity)
  Radar <- Check_Mode_S_Data_Age(Radar, Max_Age, "Mode_S_IAS", Operation, Activity)
  Radar <- Check_Mode_S_Data_Age(Radar, Max_Age, "Mode_S_TAS", Operation, Activity)
  Radar <- Check_Mode_S_Data_Age(Radar, Max_Age, "Mode_S_Roll_Angle", Operation, Activity)
  Radar <- Check_Mode_S_Data_Age(Radar, Max_Age, "Mode_S_BPS", Operation, Activity)
  
  # Get number of columns in Radar after adding flags.
  Radar_Col_After <- ncol(Radar)
  
  # Add the Global Flag by checking if the sum of each flag is greater than 1 across each observation.
  Radar <- mutate(Radar, Global_Flag = ifelse(rowSums((Radar_Col_Before+1):Radar_Col_After) >= 1, 1, 0))
  
  return(Radar)
  
}

Summarise_GWCS_Stage_1_Filters <- function(){}



Check_Mode_S_Data_Age <- function(Radar, Max_Age, Parameter, Operation, Activity){
  
  # If Validation Activity, exit function as currently filtering done in data loading process
  if (Activity == "Validation"){return(Radar)} else {
    
    # Get the Age/Flag parameter names
    Age_Var <- paste0(Parameter, "_Age")
    Flag_Var <- paste0(Age_Var, "_Flag")
    
    # Add the Flag
    Radar <- mutate(Radar, !!sym(Flag_Var) := ifelse(!!sym(Age_Var) > Max_Age | is.na(!!sym(Age_Var)), 1, 0))
    
    return(Radar)
    
  }
  
}

Check_Stage_1_Filter_Two_Bound <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter, Min, Max){
  
  # Get Min & Max Values
  Min <- as.numeric(select(GWCS_Adaptation, !!sym(Min)))
  Max <- as.numeric(select(GWCS_Adaptation, !!sym(Max)))
  
  # Get Flag Variable name
  Flag_Var <- paste0(Parameter, "_Flag")
  
  # Add the Flag based on being between these criteria
  Radar <- mutate(Radar, !!sym(Flag_Var) := ifelse((!!sym(Parameter) <= Max & !!sym(Parameter) >= Min) | is.na(Parameter), 0, 1))
  
  return(Radar)
  
}

Check_Stage_1_Filter_Min_Bound <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter, Min){
  
  # Get Min Value
  Min <- as.numeric(select(GWCS_Adaptation, !!sym(Min)))
  
  # Get Flag Variable name
  Flag_Var <- paste0(Parameter, "_Flag")
  
  # Add the Flag based on being above criteria
  Radar <- mutate(Radar, !!sym(Flag_Var) := ifelse(!!sym(Parameter) >= Min | is.na(Parameter), 0, 1))
  
  return(Radar)
  
}

Check_Stage_1_Filter_Max_Bound <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter, Max){
  
  # Get Max Value
  Max <- as.numeric(select(GWCS_Adaptation, !!sym(Min)))
  
  # Get Flag Variable name
  Flag_Var <- paste0(Parameter, "_Flag")
  
  # Add the Flag based on being below this criteria
  Radar <- mutate(Radar, !!sym(Flag_Var) := ifelse(!!sym(Parameter) <= Max | is.na(Parameter), 0, 1))
  
  return(Radar)
  
}

Check_Stage_1_Filter_Mode_S_GSPD <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter){
  
  # Currently No bounds by Operation/Activity
  Radar <- Check_Stage_1_Filter_Two_Bound(Radar, Operation, Activity, Parameter = "Mode_S_GSPD", Min = "Mode_S_GSPD_Min", Max = "Mode_S_GSPD_Max")
  
  return(Radar)
  
}

Check_Stage_1_Filter_Mode_S_IAS <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter){
  
  # Currently No bounds by Operation/Activity
  Radar <- Check_Stage_1_Filter_Two_Bound(Radar, Operation, Activity, Parameter = "Mode_S_IAS", Min = "Mode_S_IAS_Min", Max = "Mode_S_IAS_Max")
  
  return(Radar)
  
}

Check_Stage_1_Filter_Mode_S_TAS <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter){
  
  # Currently No bounds by Operation/Activity
  Radar <- Check_Stage_1_Filter_Two_Bound(Radar, Operation, Activity, Parameter = "Mode_S_TAS", Min = "Mode_S_TAS_Min", Max = "Mode_S_TAS_Max")
  
  return(Radar)
  
}

Check_Stage_1_Filter_Mode_S_Roll_Angle <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter){
  
  # Currently No bounds by Operation/Activity
  Radar <- Check_Stage_1_Filter_Max_Bound(Radar, Operation, Activity, Parameter = "Mode_S_Roll_Angle", Max = "Mode_S_Roll_Angle_Max")
  
  return(Radar)
  
}

Check_Stage_1_Filter_Altitude_Difference <- function(Radar, GWCS_Adaptation, Operation, Activity, Parameter){
  
  # Get the Altitude Difference
  Radar <- mutate(Radar, Absolute_Altitude_Difference = abs(Corrected_Mode_C - Glideslope_Altitude))
  
  # Currently No bounds by Operation/Activity
  Radar <- Check_Stage_1_Filter_Max_Bound(Radar, Operation, Activity, Parameter = "Altitude_Tolerance", Max = "Altitude_Tolerance")
  
  # Remove the absolute difference to retain column number
  Radar <- select(Radar, -Absolute_Altitude_Difference)
  
  # Get the Max RTT NULL Derived QNH
  Max_NULL_QNH_RTT <- as.numeric(GWCS_Adaptation$Max_RTT_Null_Derived_QNH) 
  
  # Check Altitude Tolerance Flag again
  Radar <- mutate(Radar, Altitude_Tolerance_Flag = ifelse(is.na(Derived_QNH) & Range_To_Threshold > Max_NULL_QNH_RTT, 1, Altitude_Tolerance_Flag))
  
  return(Radar)
  
}

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# Assume Global Flag, Is Assosciated Flag and Intention have been populated.
Generate_GWCS_Observed_Segments <- function(con, Radar, Operation, Activity){
  
  ##### VERIFICATION/VALIDATION EXCLUSIVES #####
  Radar_Date_Var <- ifelse(Activity == "Validation", "Track_Date", "Log_Date")
  Seg_Date_Var <- ifelse(Activity == "Validation", "FP_Date", "Log_Date")
  FPID_Var <- ifelse(Activity == "Validation", "Flight_Plan_ID", "Aman_Flight_Plan_ID")
  Seg_ID_Var <- ifelse(Activity == "Validation", "Mode_S_Wind_Seg_ID", "Seg_ID")
  ###############################################################################
  
  # Get the GWCS Adaptation
  GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")
  
  # Get the Intention Code valid for GWCS segments
  Required_Intention <- as.character(Load_Adaptation_Table(con, "tbl_Airfield")$Intention_Code)
  
  # Get the Seg Size
  Seg_Size <- as.numeric(GWCS_Adaptation$DME_Seg_Size)
  
  # Get the Buffer time to mitigate as many rounding errors as possible (VER only)
  Buffer <- ifelse(Activity == "Verification", 0.000426 * Seg_Size, 0)
  
  # Order Radar Data by Flight Plan ID and Track Time
  Radar <- arrange(Radar, Flight_Plan_ID, Track_Time)
  
  ##### STEP TO GET FP LANDING RUNWAY AND REMOVE AIRCRAFT WITH NO 4DME TIME #####
  # This will be removed in the final version and is for Validation only
  #
  ###############################################################################
  
  # Filter for no Global Flag - This doesn't include Desequences or Approach Path Deviations - just invalid points
  Radar <- filter(Radar, Global_Flag == 0 | is.na(Global_Flag)) %>% select(-Global_Flag)
  
  # Get the Segment Range to Threshold (DME_Seg, Seg_Size dependent)
  Radar <- mutate(Radar, Seg_Range_To_Threshold = floor((Range_To_Threshold + Buffer) / Seg_Size) * Seg_Size)
  
  # --- Split Datasets into ALL RADAR with no Aggreagte Parameters (Radar_All) and SEGMENT PRODUCTION RADAR for only Segment Radar with Aggregates. (Radar_Agg)
  Radar_All <- select(Radar, -c("Mode_S_IAS", "Mode_S_GSPD", "Mode_S_TAS", "Wind_Effect_IAS", "Track_SPD", "Track_HDG", "Mode_S_Track_HDG", "Mode_S_HDG", "Wind_SPD", "Wind_HDG", "Corrected_Mode_C"))
  Radar_Agg <- filter(Radar, !is.na(Approach_Path) & Intention == Airfield & Is_Assosciated_Flag == 0)
  
  # Get the "Track ID" for each Radar point of each aircraft as ascending in Time. (For Radar_All)
  Radar_All <- Radar_All %>% group_by(Flight_Plan_ID) %>% mutate(Track_ID = row_number()) %>% ungroup()
  
  # Use this Track ID to create a dataset with the NEXT Radar Track point from this one. (New Track ID -> Track ID - 1)
  Radar_All_Next <- Radar2 %>% mutate(Track_ID = Track_ID - 1) %>% 
    rename(Next_Radar_Track_Point_ID = Radar_Track_Point_ID,
           Next_Track_Time = Track_Time,
           Next_Seg_Range_To_Threshold = Seg_Range_To_Threshold,
           Next_Approach_Path = Approach_Path,
           Next_Is_Assosciated_Flag = Is_Assosciated_Flag,
           Next_Intention = Intention)
  
  # Join on Radar_All_Next to Radar_All to begin the comparison of successive radar points. NOTE: Assumes Flight Plan ID generated.
  Radar_Compare <- left_join(Radar_All, Radar_All_Next, by = setNames(FPID_Var, Radar_Date_Var, "Track_ID"))
  
  # Get the Number of Columns for Radar_Compare (Will use for a Publish Flag)
  Radar_Compare_Cols_Before <- ncol(Radar_Compare)
  
  # Add Flags for Radar Points that signify a Segment to Publish (Flight Plan Desequence, Left Approach Path, Intention Change, Segment Change)
  Radar_Compare <- Radar_Compare %>%
    mutate(Desequence = ifelse(Is_Assosciated_Flag == 1 & Next_IA_Flag == 0, 1, 0)) %>%
    mutate(Intention_Change = ifelse(Intention == Airfield & Next_Intention != Intention, 1, 0)) %>%
    mutate(Last_Track = ifelse(!is.na(Seg_Range_To_Threshold) & is.na(Next_Seg_Range_To_Threshold) & is.na(Next_Radar_Track_Point_ID), 1, 0)) %>%
    mutate(Left_Approach = ifelse(Approach_Path != Next_Approach_Path | (!is.na(Approach_Path) & is.na(Next_Approach_Path)), 1, 0)) %>%
    mutate(Left_Approach = ifelse(is.na(Left_Approach), 0, Left_Approach)) %>%
    mutate(Segment_Change = ifelse(Seg_Range_To_Threshold != Next_Seg_Range_To_Threshold, 1, 0)) %>%
    mutate(Segment_Change = ifelse(is.na(Segment_Change), 0, Segment_Change))
  
  # Get the Number of Columns for Radar_Compare After Flags.
  Radar_Compare_Cols_After <- ncol(Radar_Compare)
  
  # Create a Global Publish Flag that is positive if any publishing criteria is met.
  Radar_Compare <- Radar_Compare %>%
    mutate(Publish_Flag = ifelse(rowSums((Radar_Compare_Cols_Before+1):Radar_Compare_Cols_After) >= 1, 1, 0))
  
  # Filter for Radar track point pairs that lead to a published segment.
  Published_Segments <- filter(Radar_Compare, Publish_Flag == 1)
  
  # Assign a Publish Time (or Update_Time) to each of these segments, based on their publush conditions.(Either next valid radar point or 9041 desequence time, temp left to NA)==
  Published_Segments <- mutate(Published_Segments, Update_Time = ifelse(Desequence == 1 | Intention_Change == 1 | Last_Track == 1, NA, Next_Track_Time))====
  
  # Generate a Segment Identifier variable which will then be matched by closest time to Radar_Agg
  Published_Segments <- mutate(Published_Segments, !!sym(Seg_ID_Var) := row_number())
  
  # Select the relevant required fields to link the Update Time/Seg ID to Radar_Agg
  Published_Segments <- select(Published_Segments, !!sym(FPID_Var), Seg_Range_To_Threshold, Update_Time, Seg_ID)
  
  # Perform a rolling join using the Identifier variables and The Update Time
  Radar_Agg <- rolling_join(Radar_Agg, Published_Segments, ID_Vars = c(!!sym(FPID_Var), "Seg_Range_To_Threshold"), Rolling_Var = c("Update_Time"="Track_Time"))
  
  # Perform the necessary field aggregation using the Segment Identification to group variables
  Observed_Segments <- Aggregate_GWCS_Segment_Radar(Radar_Agg, GWCS_Adaptation, Operation, Activity)
  
  return(Observed_Segments)

}
  

Aggregate_GWCS_Segment_Radar <- function(Radar, Segments, GWCS_Adaptation, Operation, Activity){
  
  # Get the Required Column names/variables
  Seg_ID_Var <- Get_Var_Segment_ID(Operation, Activity)
  Track_Date_Var <- Get_Var_Radar_Date(Operation, Activity)
  Obs_Seg_Date_Var <- Get_Var_Observed_Segment_Date(Operation, Activity)
  Flight_Plan_ID_Var <- Get_Var_Flight_Plan_ID(Operation, Activity)
  MS_True_HDG_Var <- Get_Var_MS_True_HDG(Operation, Activity)
  MS_Track_HDG_Var <- Get_Var_MS_Track_HDG(Operation, Activity)
  Track_HDG_Var <- Get_Var_Track_HDG_Var(Operation, Activity)
  Wind_HDG_Var <- Get_Var_Wind_HDG(Operation, Activity)
  Track_Time_Var <- Get_Var_Radar_Track_Time(Operation, Activity)
  Wind_Effect_Var <- Get_Var_Wind_Effect(Operation, Activity)
  Wind_SPD_Var <- Get_Var_Wind_SPD(Operation, Activity)
  Corrected_Alt_Var <- Get_Var_Corrected_Altitude(Operation, Acitivity) 
  
  # Segments should already be ordered in time, but do so by Segment ID too.
  Radar <- arrange(Radar, !!sym(Seg_ID_Var), !!sym(Track_Time_Var))
  
  # Currently for all Versions: Decompose all Headings.
  Radar <- Radar %>%
    Decompose_Heading_XY(Parameter = !!sym(MS_True_HDG_Var)) %>%
    Decompose_Heading_XY(Parameter = !!sym(MS_Track_HDG_Var)) %>%
    Decompose_Heading_XY(Parameter = !!sym(Track_HDG_Var)) %>%
    Decompose_Heading_XY(Parameter = !!sym(Wind_HDG_Var))
  
  # Setup Trapezium Rule averages for Relevant Parameters
  Radar <- Radar %>%
    Setup_Trapezium_Rule(Scale_Var = !!sym(Track_Time_Var), Parameter = !!sym(Mode_S_IAS_Var)) %>%
    Setup_Trapezium_Rule(Scale_Var = !!sym(Track_Time_Var), Parameter = !!sym(Mode_S_GSPD_Var)) %>%
    Setup_Trapezium_Rule(Scale_Var = !!sym(Track_Time_Var), Parameter = !!sym(Mode_S_TAS_Var)) %>%
    Setup_Trapezium_Rule(Scale_Var = !!sym(Track_Time_Var), Parameter = !!sym(Wind_Effect_Var)) %>%
    Setup_Trapezium_Rule(Scale_Var = !!sym(Track_Time_Var), Parameter = !!sym(Wind_SPD_Var)) %>%
    Setup_Trapezium_Rule(Scale_Var = !!sym(Track_Time_Var), Parameter = !!sym(Track_SPD_Var)) %>%
    Setup_Trapezium_Rule(Scale_Var = !!sym(Track_Time_Var), Parameter = !!sym(Corrected_Alt_Var))
    
  # Get the required averages/trapezium rule averages
  Aggregate_Data <- Radar %>% group_by(!!sym(Seg_ID_Var)) %>%
    summarise(!!sym(paste0("Ave_", Mode_S_IAS_Var)) = sum(!!sym(paste0("Trap_", Mode_S_IAS_Var))),
              !!sym(paste0("Ave_", Mode_S_GSPD_Var)) = sum(!!sym(paste0("Trap_", Mode_S_GSPD_Var))),
              !!sym(paste0("Ave_", Mode_S_TAS_Var)) = sum(!!sym(paste0("Trap_", Mode_S_TAS_Var))),
              !!sym(paste0("Ave_", Wind_Effect_Var)) = sum(!!sym(paste0("Trap_", Wind_Effect_Var))),
              !!sym(paste0("Ave_", Wind_SPD_Var)) = sum(!!sym(paste0("Trap_", Wind_SPD_Var))),
              !!sym(paste0("Ave_", Track_SPD_Var)) = sum(!!sym(paste0("Trap_", Track_SPD_Var))),
              !!sym(paste0("Ave_", Corrected_Alt_Var)) = sum(!!sym(paste0("Trap_", Corrected_Alt_Var))),
              !!sym(paste0("Ave_", MS_True_HDG_Var, "_X")) = mean(!!sym(paste0(MS_True_HDG_Var, "_X"))),
              !!sym(paste0("Ave_", MS_True_HDG_Var, "_Y")) = mean(!!sym(paste0(MS_True_HDG_Var, "_Y"))),
              !!sym(paste0("Ave_", MS_Track_HDG_Var, "_X")) = mean(!!sym(paste0(MS_Track_HDG_Var, "_X"))),
              !!sym(paste0("Ave_", MS_Track_HDG_Var, "_Y")) = mean(!!sym(paste0(MS_Track_HDG_Var, "_Y"))),
              !!sym(paste0("Ave_", Track_HDG_Var, "_X")) = mean(!!sym(paste0(Track_HDG_Var, "_X"))),
              !!sym(paste0("Ave_", Track_HDG_Var, "_Y")) = mean(!!sym(paste0(Track_HDG_Var, "_Y"))),
              !!sym(paste0("Ave_", Wind_HDG_Var, "_X")) = mean(!!sym(paste0(Wind_HDG_Var, "_X"))),
              !!sym(paste0("Ave_", Wind_HDG_Var, "_Y")) = mean(!!sym(paste0(Wind_HDG_Var, "_Y"))))
  
  
              
}


