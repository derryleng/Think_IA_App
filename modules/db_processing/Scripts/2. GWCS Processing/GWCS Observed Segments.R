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


Generate_RTPD_Glideslope_Altitude <- function(RTPD, Runway, ValorVer){
  
  # Get the Name of Apprach Path Variable (Dependent on VAL/VER)
  Approach_Path_Var <- Get_Name_Approach_Path(ValorVer)
  
  # Select the Glideslope Angle and Touchdown Offsets
  Runway <- select(Runway, Runway_Name, Glideslope_Angle, Touchdown_Offset)
  
  # Join on the Runway Data
  RTPD <- left_join(RTPD, Runway, by = setNames(Approach_Path_Var, "Runway_Name"))
  
  # Get the Glideslope Altitude
  RTPD <- RTPD %>% 
    mutate(Glideslope_Alt = (Range_To_Threshold + Touchdown_Offset) * tan(Glideslope_Angle))
  
  # Select the Glideslope Altitude
  RTPD <- RTPD %>%
    select(Radar_Track_Point_ID, Glideslope_Alt)
  
  return(RTPD)
  
}

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

# ----------------------------------------------------------------------------------------------------------------------------------------- #

Get_Name_Approach_Path <- function(ValorVer){
  
  if (ValorVer == "Val"){return("Mode_S_Wind_Localiser_Capture")}
  else {return("Approach_Volume")}
  
}

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
  Stale_Time <- as.numeric(GWCS_Adaptation$Forecast_Stale_Time)
  
  # Get the Buffer time to mitigate as many rounding errors as possible (VER only)
  Buffer <- ifelse(Activity == "Verification", 0.000426 * Seg_Size, 0)
  
  # Order Radar Data by Flight Plan ID and Track Time
  Radar <- arrange(Radar, Flight_Plan_ID, Track_Time)
  
  ##### STEP TO GET FP LANDING RUNWAY AND REMOVE AIRCRAFT WITH NO 4DME TIME #####
  # This will be removed in the final version and is for Validation only
  #
  ###############################################################################
  
  # Filter for no Global Flag - This doesn't include Desequences or Approach Path Deviations - just invalid points
  Radar <- filter(Radar, GlobalFlag == 0 | is.na(GlobalFlag)) %>% select(-GlobalFlag)
  
  # To enable consistency with Validation and Verification, add the VER only fields - assuming all remaining radar is valid.
  RadarCols <- names(Radar)
  if (Intention %!in% RadarCols){Radar <- mutate(Radar, Intention = Required_Intention)}
  if (Is_Assosciated_Flag %!in% RadarCols){Radar <- mutate(Radar, Is_Assosciated_Flag = 0)}
  
  # Get the Segment Range to Threshold (DME_Seg, Seg_Size dependent)
  Radar <- mutate(Radar, Seg_Range_To_Threshold = floor((Range_To_Threshold + Buffer) / Seg_Size) * Seg_Size)
  
  # --- Split Datasets into ALL RADAR with no Aggreagte Parameters (Radar_All) and SEGMENT PRODUCTION RADAR for only Segment Radar with Aggregates. (Radar_Agg)
  Radar_All <- select(Radar, -c("Mode_S_IAS", "Mode_S_GSPD", "Mode_S_TAS", "Wind_Effect_IAS", "Track_SPD", "Track_HDG", "Mode_S_Track_HDG", "Mode_S_HDG", "Wind_SPD", "Wind_HDG", "Corrected_Mode_C"))
  Radar_Agg <- filter(Radar, !is.na(Approach_Path) & Intention == Airfield & Is_Assosciated_Flag == 0)
  
  # Get the "Track ID" for each Radar point of each aircraft as ascending in Time. (For Radar_All)
  Radar_All <- Radar_All %>% group_by(Flight_Plan_ID) %>% mutate(Track_ID = row_number()) %>% ungroup()
  
  # Use this Track ID to create a dataset with the NEXT Radar Track point from this one. (New Track ID -> Track ID - 1)
  Radar_All_Next <- Radar_All %>% mutate(Track_ID = Track_ID - 1) %>% 
    rename(Next_Radar_Track_Point_ID = Radar_Track_Point_ID,
           Next_Track_Time = Track_Time,
           Next_Seg_Range_To_Threshold = Seg_Range_To_Threshold,
           Next_Approach_Path = Approach_Path,
           Next_Is_Assosciated_Flag = Is_Assosciated_Flag,
           Next_Intention = Intention)
  
  # Join on Radar_All_Next to Radar_All to begin the comparison of successive radar points. NOTE: Assumes Flight Plan ID generated.
  Radar_Compare <- left_join(Radar_All, Radar_All_Next, by = setNames(FPID_Var, Radar_Date_Var, "Track_ID"))
  
  # Add Flags for Radar Points that signify a Segment to Publish (Flight Plan Desequence, Left Approach Path, Intention Change, Segment Change)
  Radar_Compare <- Radar_Compare %>%
    mutate(Desequence = ifelse(Is_Assosciated_Flag == 1 & Next_Is_Assosciated_Flag == 0, 1, 0)) %>%
    mutate(Intention_Change = ifelse(Intention == Airfield & Next_Intention != Intention, 1, 0)) %>%
    mutate(Last_Track = ifelse(!is.na(Seg_Range_To_Threshold) & is.na(Next_Seg_Range_To_Threshold) & is.na(Next_Radar_Track_Point_ID), 1, 0)) %>%
    mutate(Left_Approach = ifelse(Approach_Path != Next_Approach_Path | (!is.na(Approach_Path) & is.na(Next_Approach_Path)), 1, 0)) %>%
    mutate(Left_Approach = ifelse(is.na(Left_Approach), 0, Left_Approach)) %>%
    mutate(Segment_Change = ifelse(Seg_Range_To_Threshold != Next_Seg_Range_To_Threshold, 1, 0)) %>%
    mutate(Segment_Change = ifelse(is.na(Segment_Change), 0, Segment_Change))
  
  # Create list of columns that affect the publishing of a segment
  PublishCols <- c("Desequence", "Intention_Change", "Last_Track", "Left_Approach", "Segment_Change")
  
  # Create a Global Publish Flag that is positive if any publishing criteria is met.
  Radar_Compare <- Radar_Compare %>%
    mutate(Publish_Flag = ifelse(psum(!!!syms(PublishCols)) >= 1, 1, 0))
  
  # Filter for Radar track point pairs that lead to a published segment.
  Published_Segments <- filter(Radar_Compare, Publish_Flag == 1)
  
  # Assign a Publish Time (or Update_Time) to each of these segments, based on their publush conditions.(Either next valid radar point or 9041 desequence time, temp left to NA)
  Published_Segments <- mutate(Published_Segments, Update_Time = ifelse(Desequence == 1 | Intention_Change == 1 | Last_Track == 1, NA, Next_Track_Time))
  
  # Generate a Segment Identifier variable which will then be matched by closest time to Radar_Agg
  Published_Segments <- mutate(Published_Segments, !!sym(Seg_ID_Var) := row_number())
  
  # Select the relevant required fields to link the Update Time/Seg ID to Radar_Agg
  Published_Segments <- select(Published_Segments, !!sym(FPID_Var), Seg_Range_To_Threshold, Update_Time, !!sym(Seg_ID_Var))
  
  # Perform a rolling join using the Identifier variables and The Update Time to assign each valid radar point a segment ID
  Radar_Agg <- rolling_join(Radar_Agg, Published_Segments, c(FPID_Var, "Seg_Range_To_Threshold", "Update_Time"), c(FPID_Var, "Seg_Range_To_Threshold", "Track_Time"), Roll = +Inf)
  
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






























# Testing ----

Database <- "PWS_Prototyping"
con <- Get_DBI_Connection(IP, Database)
Date_List_Query <- "SELECT DISTINCT(Track_Date) FROM tbl_Radar_Track_Point"
Dates <- dbGetQuery(con, Date_List_Query)
GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")
Date <- Dates$Track_Date[1]
RTP <- dbGetQuery(con, paste0("SELECT * FROM tbl_Radar_Track_Point WHERE Track_Date = '", Date, "'"))

StatsColumns <- c("Mode_S_GSPD_Missing", "Mode_S_GSPD_Unrealistic", "Mode_S_IAS_Missing", "Mode_S_IAS_Unrealistic", "Mode_S_TAS_Missing", "Mode_S_TAS_Unrealistic",
                  "Mode_S_Roll_Angle_Missing", "!Mode_S_Roll_Angle_Reject", "Mode_S_HDG_Missing", "Mode_S_Track_HDG_Missing", "Glideslope_Error")

AllColumns <- append(StatsColumns, c("Wind_Effect_Missing", "Wind_HDG_Missing", "Wind_SPD_Missing"))

RTP <- RTP %>%
  Generate_RTPD_Glideslope_Altitude(RTP, Runways, ValorVer = "Val")

RTP <- RTP %>%
  mutate(Mode_S_GSPD_Missing = ifelse(is.na(Mode_S_GSPD), 1, 0),
         Mode_S_GSPD_Unrealistic = ifelse(Mode_S_GSPD_Missing == 0 & (Mode_S_GSPD > Mode_S_GSPD_Max | Mode_S_GSPD < Mode_S_GSPD_Min), 1, 0), 
         Mode_S_IAS_Missing = ifelse(is.na(Mode_S_IAS), 1, 0),
         Mode_S_IAS_Unrealistic = ifelse(Mode_S_IAS_Missing == 0 & (Mode_S_IAS > Mode_S_IAS_Max | Mode_S_IAS < Mode_S_IAS_Min), 1, 0), 
         Mode_S_TAS_Missing = ifelse(is.na(Mode_S_TAS), 1, 0),
         Mode_S_TAS_Unrealistic = ifelse(Mode_S_TAS_Missing == 0 & (Mode_S_TAS > Mode_S_TAS_Max | Mode_S_TAS < Mode_S_TAS_Min), 1, 0),
         Mode_S_Roll_Angle_Missing = ifelse(is.na(Mode_S_Roll_Angle), 1, 0),
         Mode_S_Roll_Angle_Reject = ifelse(Mode_S_Roll_Angle_Missing == 0 & abs(Mode_S_Roll_Angle) > Mode_S_Roll_Angle_Max, 1, 0),
         Mode_S_HDG_Missing = ifelse(is.na(Mode_S_HDG), 1, 0),
         Mode_S_Track_HDG_Missing = ifelse(is.na(Mode_S_Track_HDG), 1, 0),
         Glideslope_Error = ifelse(abs(Corrected_Mode_C - Glideslope_Altitude) > Altitude_Tolerance, 1, 0),
         Wind_Effect_Missing = ifelse(is.na(Wind_Effect_IAS), 1, 0),
         Wind_SPD_Missing = ifelse(is.na(Wind_SPD), 1, 0),
         Wind_HDG_Missing = ifelse(is.na(Wind_HDG), 1, 0),
         GlobalFlag = ifelse(psum(!!!syms(AllColumns)) > 0, 1, 0))

FilterStats <- RTP %>%
  group_by(Flight_Plan_ID) %>%
  summarise(Mode_S_GSPD_Missing = sum(Mode_S_GSPD_Missing),
            Mode_S_GSPD_Unrealistic = sum(Mode_S_GSPD_Unrealistic),
            Mode_S_IAS_Missing = sum(Mode_S_IAS_Missing),
            Mode_S_IAS_Unrealistic = sum(Mode_S_IAS_Unrealistic),
            Mode_S_TAS_Missing = sum(Mode_S_TAS_Missing),
            Mode_S_TAS_Unrealistic = sum(Mode_S_TAS_Unrealistic),
            Mode_S_Roll_Angle_Missing = sum(Mode_S_Roll_Angle_Missing),
            Mode_S_Roll_Angle_Reject = sum(Mode_S_Roll_Angle_Reject),
            Mode_S_HDG_Missing = sum(Mode_S_HDG_Missing),
            Mode_S_Track_HDG_Missing = sum(Mode_S_Track_HDG_Missing),
            Glideslope_Error = sum(Glideslope_Error),
            Obs_Count = n()) %>% ungroup()

# Filter for flags
RTP <- filter(RTP, GlobalFlag == 0)

# Validation Only.
RTP <- filter(RTP, !is.na(Mode_S_Wind_Localiser_Capture))
RTP <- filter(RTP, !is.na(Range_To_Threshold))









