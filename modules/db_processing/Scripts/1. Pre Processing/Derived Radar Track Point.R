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
Calculate_DME_Position_Y <- function(){}
Get_Heading_Difference <- function(HDG1, HDG2){return(HDG1)}
Is_In_Heading_Range <- function(HDG, HDG1, HDG2){return(T)}
Is_In_Polygon <- function(RTP, X_Pol, Y_Pol){
  RTP <- mutate(RTP, In_Polygon = point.in.polygon(X_Pos, Y_Pos, X_Pol, Y_Pol)) %>%
    mutate(In_Polygon = ifelse(In_Polygon >= 1, 1, 0))
  return(RTP)
}

Generate_RTPD_Ground_Track_Heading <- function(RTP){
  RTP <- RTP %>%
    mutate(Ground_Track_Heading = ifelse(Get_Heading_Difference(Mode_S_Track_HDG, Track_HDG) <= Max_Heading_Diff, Mode_S_Track_HDG, Track_HDG),
           Ground_Track_Heading = ifelse(is.na(Ground_Track_Heading) & !is.na(Mode_S_Track_HDG), Mode_S_Track_HDG, Ground_Track_Heading),
           Ground_Track_Heading = ifelse(is.na(Ground_Track_Heading) & !is.na(Track_HDG), Track_HDG, Ground_Track_Heading))
  return(RTP)
}

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
  
  # Get the 4DME X/Y Co-oridnates
  RTP <- RTP %>%
    mutate(Runway_4DME_X = Calculate_DME_Position_X(Landing_Runway, 4*NM_to_m),
           Runway_4DME_Y = Calculate_DME_Position_Y(Landing_Runway, 4*NM_to_m))
  
  # Get the Ground Track Heading
  RTP <- RTP %>%
    Generate_RTPD_Ground_Track_Heading()
  
  # Get the Direct Intercept Positions (Current Heading to ILS)
  RTP <- RTP %>%
    mutate(Intercept_X = Calculate_Intercept_Position_X(Threshold_X_Pos, Threshold_Y_Pos, Runway_Heading, X_Pos, Y_Pos, Ground_Track_Heading),
           Intercept_Y = Calculate_Intercept_Position_Y(Threshold_X_Pos, Threshold_Y_Pos, Runway_Heading, X_Pos, Y_Pos, Ground_Track_Heading),
           Intercept_Dir_From_4DME = ifelse(Intercept_X >= Runway_4DME_X, "E", "W"))
  
  # Get the ILS Locus point (Intercept of ILS perpendicular to ILS) and it's range to threshold.
  RTP <- RTP %>%
    mutate(Runway_Heading_Perp = Runway_Heading + (pi/2),
           ILS_Locus_X = Calculate_Intercept_Position_X(Threshold_X_Pos, Threshold_Y_Pos, Runway_Heading, X_Pos, Y_Pos, Runway_Heading_Perp),
           ILS_Locus_Y = Calculate_Intercept_Position_Y(Threshold_X_Pos, Threshold_Y_Pos, Runway_Heading, X_Pos, Y_Pos, Runway_Heading_Perp),
           ILS_Locus_RTT = Get_2D_Range(ILS_Locus_X, ILS_LocusY, Threshold_X_Pos, Threshold_Y_Pos),
           Direction_From_ILS = ifelse(Y_Pos >= ILS_Locus_Y, "N", "S"))
  
  # Get the Range to ILS.
  RTP <- RTP %>%
    mutate(Range_To_ILS = Get_2D_Range(X_Pos, Y_Pos, ILS_Locus_X, ILS_Locus_Y))
  
  # Get the intercept of 4DME perpendicular and line from current position parallel to ILS.
  RTP <- RTP %>%
    mutate(Perpendicular_4DME_Locus_X = Calculate_Intercept_Position_X(Runway_4DME_X, Runway_4DME_Y, Runway_Heading_Perp, X_Pos, Y_Pos, Runway_Heading),
           Direction_From_4DME = ifelse(X_Pos >= Perpendicular_4DME_Locus_X, "E", "W"))
  
  # Select relevant fields.
  RTP <- RTP %>%
    select(Radar_Track_Point_ID,
           Intercept_X,
           Intercept_Y,
           Direction_From_ILS,
           Direction_From_4DME,
           Range_To_ILS,
           Intercept_Dir_From_4DME,
           ILS_Locus_RTT)
  
  return(RTP)
  
}

#library(sp)

RTP <- sqlQuery(con, "SELECT TOP(100) * FROM tbl_Radar_Track_Point", stringsAsFactors = F)
RTPD <- sqlQuery(con, "SELECT TOP(100) * FROM tbl_Radar_Track_Point_Derived", stringsAsFactors = F)
FP <- sqlQuery(con, "SELECT * FROM tbl_Flight_Plan", stringsAsFactors = F)
Runway <- Load_Adaptation_Table(con, "tbl_Runway")
Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")
Path_Leg_Transitions <- Load_Adaptation_Table(con, "tbl_Path_Leg_Transition")
Volumes <- Load_Adaptation_Table(con, "tbl_Volume")
Polygons <- Load_Adaptation_Table(con, "tbl_Polygon")
Adaptation <- Load_Adaptation_Table(con, "tbl_Adaptation_Data")

#Generate_RTPD_Path_Leg <- function(RTP, RTPD, FP, Runway, Path_Legs, Path_Leg_Transitions, Volumes, Polygons, Adaptation, VarNo){
  
  VarNo <- 1
  
  # Initialise Path Leg Variable Name
  Var <- "Path_Leg"
  if (VarNo != 1){Var <- paste0(Var, "_", VarNo)}
  
  # Get Max Heading Difference
  Max_Heading_Diff <- as.numeric(Adaptation$Diff_Mode_S_To_Radar_Track_Max)
  
  # Arrange RTP by Flight Plan ID and Track Time
  RTP <- RTP %>%
    arrange(Flight_Plan_ID, Track_Time)
  
  # Select Relevant Fields from Main Tables 
  RTP <- select(RTP, Radar_Track_Point_ID, Flight_Plan_ID, X_Pos, Y_Pos, Track_HDG, Mode_S_Track_HDG)
  RTPD <- select(RTPD, Radar_Track_Point_ID, Corrected_Mode_C, Min_Sustained_RoCD)
  FP <- select(FP, Flight_Plan_ID, Landing_Runway)
  RTP <- RTP %>%
    left_join(RTPD, by = c("Radar_Track_Point_ID")) %>%
    left_join(FP, by = c("Flight_Plan_ID"))
  
  # Calculate Ground Track Heading
  RTP <- RTP %>%
    Generate_RTPD_Ground_Track_Heading()
  
  # Generate Relative Track ID to flight.
  RTP <- RTP %>%
    group_by(Flight_Plan_ID) %>%
    mutate(Track_ID = row_number()) %>%
    ungroup()
  
  # Get the Path Leg Transition File in the correct format. (PLT_ID, Volume_Name, Landing_Runway, Difference_Runway, Min_Sustained_RoCD, )
  Path_Leg_Transitions <- select(Path_Leg_Transitions, PLT_ID, Current_Path_Leg, New_Path_Leg, Airfield_Name, Min_Heading, Max_Heading, Volume_Name, Min_Sustained_RoCD, Runway_Name, Difference_Runway)
  Polygons <- filter(Polygons, Point_Sequence <= 4) %>% select(Volume_Name, Point_X, Point_Y)
  Volumes <- select(Volumes, Volume_Name, Min_Altitude, Max_Altitude)
  Path_Leg_Transitions <- Path_Leg_Transitions %>%
    left_join(Volumes, by = c("Volume_Name")) %>%
    left_join(Polygons, by = c("Volume_Name"))
  
  ## Loop 1: By Runway. ASsumes Constant Landing Runway for a single flight.
  Runways <- unique(Runway$Runway_Name)
  
  for (i in 1:length(Runways)){
    
    # Filter for Flights in this Runway
    RTP_Runway <- filter(RTP, Landing_Runway == Runways[i])
    
    # Find the Maximum number of points on a flight
    Max_Points <- max(RTP$Track_ID)
    
    # Filter Path Leg Transitions
    Transitions_Runway <- filter(Path_Leg_Transitions, (Runway_Name == Runways[i] | is.na(Runway_Name)) & (Difference_Runway != Runways[i] | is.na(Difference_Runway)))
    
    # SECOND LOOP: Over Track ID
    for (j in 1:Max_Points){
      
      # Filter for This Track ID
      RTP_Runway_ID <- filter(RTP_Runway, Track_ID == j)
      
      # Get the Current Path Leg
      if (j == 1){RTP_Runway_ID_PL <- select(RTP_Runway_ID, Flight_Plan_ID) %>% mutate(Current_Path_Leg = NA)} else 
      {RTP_Runway_ID_PL <- filter(RTP_Runway_ID_Comp, Track_ID == (j-1)) %>% select(Flight_Plan_ID, Current_Path_Leg = Value)}
      RTP_Runway_ID <- left_join(RTP_Runway_ID, RTP_Runway_ID_PL, by = c("Flight_Plan_ID"))
      
      
      Unique_PL <- unique(RTP_Runway_ID_PL$Current_Path_Leg)
      
      # THIRD LOOP: NULL AND NO NULL CURRENT PATH LEGS
      for (k in 1:2){
        
        if (k == 1){
          RTP_Runway_ID_Tran <- filter(RTP_Runway_ID, is.na(Current_Path_Leg))
          Transitions_Runway_ID_Tran <- filter(Transitions_Runway, is.na(Current_Path_Leg))
        } else {
          RTP_Runway_ID_Tran <- filter(RTP_Runway_ID, !is.na(Current_Path_Leg))
          Transitions_Runway_ID_Tran <- filter(Transitions_Runway, Current_Path_Leg %in% Unique_PL)
        }
        
        if (nrow(RTP_Runway_ID_Tran) > 0 & nrow(Transitions_Runway_ID_Tran) > 0){
         
          # FINAL LOOP: TRANSITION COMPARISON
          for (l in 1:max(Transitions_Runway_ID_Tran$PLT_ID)){
            
            # Get the Transition Values
            Trans <- filter(Transitions_Runway_ID_Tran, PLT_ID == l)
            New_Path_Leg <- Trans[1,]$New_Path_Leg
            Min_Heading <- Trans[1,]$Min_Heading
            Max_Heading <- Trans[1,]$Max_Heading
            Min_Alt <- Trans[1,]$Min_Altitude
            Max_Alt <- Trans[1,]$Max_Altitude
            Min_RoCD <- Trans[1,]$Min_Sustained_RoCD
            X_Values <- Trans$Point_X
            Y_Values <- Trans$Point_Y
            
            # Now perform the checks
            RTP_Runway_ID_Tran <- RTP_Runway_ID_Tran %>%
              mutate(Flag = ifelse(Is_In_Heading_Range(Ground_Track_Heading, Min_Heading, Max_Heading), 1, 0),
                     Flag = ifelse(is.na(Min_Heading) | is.na(Max_Heading), Flag, 0),
                     Flag = ifelse(Corrected_Mode_C >= Min_Alt & Corrected_Mode_C <= Max_Alt, Flag, 0),
                     Flag = ifelse(Min_Sustained_RoCD >= Min_RoCD | is.na(Min_RoCD), Flag, 0))
            
            # Perform Check on Polygon
            # RTP_Runway_ID_Tran <- RTP_Runway_ID_Tran %>%
            #   Is_In_Polygon(X_Values, Y_Values) %>%
            #   mutate(Flag = ifelse(In_Polygon == 1, Flag, 0)) %>%
            #   select(-In_Polygon)
            
            # Split Valid and Invalid
            Valid <- filter(RTP_Runway_ID_Tran, Flag == 1) %>% select(-Flag) %>% mutate(Value = New_Path_Leg)
            RTP_Runway_ID_Tran <- filter(RTP_Runway_ID_Tran, Flag == 0) %>% select(-Flag)
            
            # Add to Completed 
            if (!exists("RTP_Runway_ID_Comp")){RTP_Runway_ID_Comp <- Valid} else {
              RTP_Runway_ID_Comp <- rbind(RTP_Runway_ID_Comp, Valid)
            }
            
            
          
          }
        
        }
        
        if (nrow(RTP_Runway_ID_Tran) > 0){
          RTP_Runway_ID_Tran <- mutate(RTP_Runway_ID_Tran, Value = Current_Path_Leg)
        }
        
        if (!exists("RTP_Runway_ID_Comp")){RTP_Runway_ID_Comp <- RTP_Runway_ID_Tran} else {
          RTP_Runway_ID_Comp <- rbind(RTP_Runway_ID_Comp, RTP_Runway_ID_Tran)
        }
      
      }
      
        
    }
    
    if (!exists("RTP_Comp")){RTP_Comp <- RTP_Runway_ID_Comp} else {
      RTP_Comp <- rbind(RTP_Comp, RTP_Runway_ID_Comp)}
      
    rm(RTP_Runway_ID_Comp)
    
  }
  
  RTP_Comp <- rename(RTP_Comp, !!sym(Var) := Value)
  
  #return(select(RTP_Comp, Radar_Track_Point_ID, !!sym(Var)))
  
#}
  



