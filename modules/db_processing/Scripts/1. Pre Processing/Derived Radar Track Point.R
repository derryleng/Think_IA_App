# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Generate Radar Track Point Derived
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v1.0
#
# Authors: George Clark
# 
# Description: Script to Generate Radar Track Point Derived Fields.
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

Get_Name_Approach_Path <- function(ValorVer){
  
  if (ValorVer == "Val"){return("Mode_S_Wind_Localiser_Capture")}
  else {return("Approach_Volume")}
  
}

Generate_RTPD_Range_To_Threshold <- function(RTP, RTPD, Runway, Method, ValorVer){
  
  # Get the Name of Apprach Path Variable (Dependent on VAL/VER)
  Approach_Path_Var <- Get_Name_Approach_Path(ValorVer)
  
  # Get the Track X/Y
  RTP <- select(RTP, Radar_Track_Point_ID, X_Pos, Y_Pos)
  
  # Use Mode S Wind Localiser by Default
  RTPD <- select(RTPD, Radar_Track_Point_ID, !!sym(Approach_Path_Var))
  
  # Select Runway X/Y Co-ordinates
  Runway <- select(Runway, Runway_Name, Threshold_X_Pos, Threshold_Y_Pos)
  
  # Join on the tables
  RTPD <- RTPD %>% 
    left_join(RTP, by = c("Radar_Track_Point_ID")) %>%
    left_join(Runway, by = setNames("Runway_Name", Approach_Path_Var))
  
  # Get the Range to Threshold based on Localiser
  RTPD <- RTPD %>%
    mutate(Range_To_Threshold = Get_2D_Range(X_Pos, Y_Pos, Threshold_X_Pos, Threshold_Y_Pos)) %>%
    select(Radar_Track_Point_ID, Range_To_Threshold)
  
  return(RTPD)
  
}


Generate_RTPD_Wind_Parameters <- function(RTP, Adaptation){
  
  # Get the Mag_Var
  Mag_Var <- as.numeric(Adaptation$Mag_Var)
  
  # First of all, Find the Ground Track Heading and True HDG
  RTP <- RTP %>%
    mutate(Ground_Track_Heading = ifelse(!is.na(Mode_S_Track_HDG), Mode_S_Track_HDG, ifelse(!is.na(Track_HDG), Track_HDG, NA)),
           True_Heading = Mode_S_HDG + Mag_Var)
  
  # Create Flag for "Sanity Check" of Values
  RTP <- RTP %>%
    mutate(Sanity_Check = ifelse(Ground_Track_Heading >= 0 & Mode_S_GSPD > 0 & Mode_S_IAS > 0 & Mode_S_HDG >= 0 & Mode_S_TAS > 0, 1, 0)) %>%
    mutate(Sanity_Check = ifelse(is.na(Sanity_Check), 0, Sanity_Check))
  
  # Filter only for Tracks that pass sanity check
  RTP <- filter(RTP, Sanity_Check == 1)
  
  # Get the X, Y Components of GSPD and TAS
  RTP <- RTP %>%
    mutate(GSPD_X = Get_2D_Vx(Mode_S_GSPD, Ground_Track_Heading),
           GSPD_Y = Get_2D_Vy(Mode_S_GSPD, Ground_Track_Heading),
           TAS_X = Get_2D_Vx(Mode_S_TAS, True_Heading),
           TAS_Y = Get_2D_Vy(Mode_S_TAS, True_Heading))
  
  # Calculate the X, Y Components of the Wind Vector
  RTP <- RTP %>%
    mutate(Wind_X = GSPD_X - TAS_X,
           Wind_Y = GSPD_Y - TAS_Y)
  
  # Calculate the Wind Speed, Heading, and Headwind
  RTP <- RTP %>% 
    mutate(Wind_SPD = Get_2D_Amplitude(Wind_X, Wind_Y),
           Wind_HDG = Get_2D_Angle(Wind_X, Wind_Y),
           Headwind_SPD = Get_2D_Scalar_Product(Wind_SPD, Wind_HDG, 1, Ground_Track_Heading))
  
  # Calculate the Wind Effect 
  RTP <- RTP %>%
    mutate(Wind_Effect_IAS = Mode_S_GSPD - Mode_S_IAS)
  
  # Select Wind Fields
  RTP <- RTP %>%
    select(Radar_Track_Point_ID, Wind_SPD, Wind_HDG, Headwind_SPG, Wind_Effect_IAS)
  
  return(RTP)
  
}


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


Calculate_Intercept_Position_X <- function(){}
Calculate_Intercept_Position_Y <- function(){}
Calculate_DME_Position_X <- function(){}

Generate_RTPD_ILS_Relative_Fields <- function(RTP, FP, Runway, GWCS_Adaptation){
  
  # Get the Diff_Mode_S_To_Radar_Track_Max for Ground_Track_Heading
  Max_Heading_Diff <- as.numeric(GWCS_Adaptation$Diff_Mode_S_To_Radar_Track_Max)
  
  # Get the Relevant Fields from Flight Plan
  FP <- select(FP, Flight_Plan_ID, Landing_Runway)
  
  # Select Runway X/Y Co-ordinates
  Runway <- select(Runway, Runway_Name, Threshold_X_Pos, Threshold_Y_Pos, Runway_Heading = Heading)
  
  # Join on the FP/Runway Data
  RTP <- left_join(RTP, FP, by = c("Flight_Plan_ID")) %>%
    left_join(Runway, by = c("Landing_Runway" = "Runway_Name"))
  
  # Get the Ground Track Heading
  RTP <- RTP %>%
    mutate(Ground_Track_Heading = ifelse(Get_Heading_Difference(Mode_S_Track_HDG, Track_HDG) <= Max_Heading_Diff, Mode_S_Track_HDG, Track_HDG),
           Ground_Track_Heading = ifelse(is.na(Ground_Track_Heading) & !is.na(Mode_S_Track_HDG), Mode_S_Track_HDG, Ground_Track_Heading),
           Ground_Track_Heading = ifelse(is.na(Ground_Track_Heading) & !is.na(Track_HDG), Track_HDG, Ground_Track_Heading))
  
  # Get the Direct Intercept Positions (Current Heading to ILS)
  RTP <- RTP %>%
    mutate(Intercept_X = Calculate_Intercept_Position_X(Threshold_X_Pos, Threshold_Y_Pos, Runway_Heading, X_Pos, Y_Pos, Ground_Track_Heading),
           Intercept_Y = Calculate_Intercept_Position_Y(Threshold_X_Pos, Threshold_Y_Pos, Runway_Heading, X_Pos, Y_Pos, Ground_Track_Heading),
           Intercept_Dir_From_4DME = ifelse(Intercept_X >= Calculate_DME_Position_X(Landing_Runway, (4.0 * NM_to_m))), "E", "W")
  
}



