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

library(sp)

Get_Name_Approach_Path <- function(ValorVer){
  
  if (ValorVer == "Val"){return("Mode_S_Wind_Localiser_Capture")}
  else {return("Approach_Volume")}
  
}

Get_Heading_Difference <- function(Data, HDG1Var, HDG2Var){
  
  Data <- mutate(Data, HDG1 := !!sym(HDG1Var), HDG2 := !!sym(HDG2Var))
  
  Data <- Data %>%
    mutate(Diff1 = ifelse(HDG1 - HDG2 < 0, HDG1 - HDG2 + 2*pi, HDG1 - HDG2),
           Diff2 = ifelse(HDG2 - HDG1 < 0, HDG2 - HDG1 + 2*pi, HDG2 - HDG1),
           HeadingDifference = ifelse(Diff1 <= Diff2, Diff1, Diff2)) %>%
    select(-Diff1, -Diff2, -HDG1, -HDG2)
  
  return(Data)
}


Is_In_Heading_Range <- function(Data, HDG, HDG1, HDG2){
  Data <- Data %>%
    mutate(InHeadingRange = ifelse((
      !!sym(HDG) >= !!sym(HDG1) &
        !!sym(HDG) <= !!sym(HDG2)) |
        (!!sym(HDG) - 2*pi >= !!sym(HDG1) &
           !!sym(HDG) - 2*pi <= !!sym(HDG2)), 
      1, 0)
    )
  return(Data)
  
}


Generate_RTPD_Ground_Track_Heading <- function(RTP, Max_Heading_Diff){
  RTP <- RTP %>%
    Get_Heading_Difference("Mode_S_Track_HDG", "Track_HDG") %>%
    mutate(Ground_Track_Heading = ifelse(HeadingDifference <= Max_Heading_Diff, Mode_S_Track_HDG, Track_HDG),
           Ground_Track_Heading = ifelse(is.na(Ground_Track_Heading) & !is.na(Mode_S_Track_HDG), Mode_S_Track_HDG, Ground_Track_Heading),
           Ground_Track_Heading = ifelse(is.na(Ground_Track_Heading) & !is.na(Track_HDG), Track_HDG, Ground_Track_Heading)) %>%
    select(-HeadingDifference)
  return(RTP)
}


rolling_join <- function(Data1, Data2, Vars1, Vars2, Roll, Direction){
 
  String1 <- "setkey(Data1"
  for (i in 1:(length(Vars1))){
    String1 <- paste0(String1, ", ", Vars1[i])
    if (i == length(Vars1)){String1 <- paste0(String1, ")")}
  }
  
  String2 <- "setkey(Data2"
  for (i in 1:(length(Vars2))){
    Data2 <- Data2 %>%
      rename(!!sym(Vars1[i]) := !!sym(Vars2[i])) 
    String2 <- paste0(String2, ", ", Vars1[i])
    if (i == length(Vars2)){String2 <- paste0(String2, ")")}
  }
  
  Data1 <- as.data.table(Data1)
  Data2 <- as.data.table(Data2)
  
  eval(str2lang(String1))
  eval(str2lang(String2))
  
  Data <- Data2[Data1, roll=Roll]
  Data <- as.data.frame(Data)
  
  Data <- Data %>%
    select(all_of(names(Data1)), everything())
  
  return(Data)
}

Load_Radar_Data_ORD_Pre_Processing <- function(con, PROC_Period, PROC_Criteria){
  
  # Get Start Time
  message(paste0("Loading Radar data for ORD Pre-Processing for the ", PROC_Period, " of ", PROC_Criteria, "..."))
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  
  # Original Radar Data Query
  Radar_Query <- "SELECT 
                    rtp.Radar_Track_Point_ID,
                    rtp.Flight_Plan_ID,
                    rtp.Track_Date,
                    rtp.Track_Time,
                    rtp.Mode_C
                  FROM tbl_Radar_Track_Point rtp"
  
  # Edit Based on Data Loading Criteria
  if (PROC_Period == "Day"){
    Radar_Query <- paste0(Radar_Query, " WHERE Track_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){
    Radar_Query <- paste0(Radar_Query, " WHERE Track_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Acquire the Data
  Radar <- sqlQuery(con, Radar_Query, stringsAsFactors = F)
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Completed ORD Validation Radar Loading for the ", PROC_Period, " of ", PROC_Criteria, " in ",
                 seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Radar)
  
}

Load_Baro_Data <- function(con, PROC_Period, PROC_Criteria){
  
  Query <- "SELECT * FROM tbl_Baro"
  
  # Edit Based on Data Loading Criteria
  if (PROC_Period == "Day"){
    Query <- paste0(Query, " WHERE Baro_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){
    Query <- paste0(Query, " WHERE Baro_Date LIKE '%", PROC_Criteria, "%'")}
  
  Baro <- sqlQuery(con, Query, stringsAsFactors = F)
  
  return(Baro)
  
}

Pivot_Polygons <- function(Polygons){
  
  Poly <- select(Polygons, Volume_Name, Point_Sequence, Px = Point_X, Py = Point_Y) %>%
    pivot_wider(names_from = "Point_Sequence", values_from = c("Px", "Py")) 
  
  return(Poly)
  
}

Generate_RTPD_Full_Path_Leg_Transition_Table <- function(Path_Leg_Transitions, Volumes, Polygons){
  
  Poly <- Pivot_Polygons(Polygons)
  
  Vol <- filter(Volumes, Volume_Type == "PLT") %>%
    select(Volume_Name, Min_Altitude, Max_Altitude)
  
  PLT <- select(Path_Leg_Transitions, 
                PLT_ID, Current_Path_Leg, New_Path_Leg, Min_Heading, Max_Heading, Volume_Name, Min_RoCD = Min_Sustained_RoCD, Runway_Name, Difference_Runway) %>%
    mutate(Current_Path_Leg = ifelse(is.na(Current_Path_Leg), "NULL", Current_Path_Leg),
           New_Path_Leg = ifelse(is.na(New_Path_Leg), "NULL", New_Path_Leg),
           Volume_Name = ifelse(is.na(Volume_Name), "NULL", Volume_Name),
           Runway_Name = ifelse(is.na(Runway_Name), "NULL", Runway_Name),
           Difference_Runway = ifelse(is.na(Difference_Runway), "NULL", Difference_Runway))
  
  PLT <- PLT %>%
    left_join(Poly, by = c("Volume_Name")) %>%
    left_join(Vol, by = c("Volume_Name"))
  
  return(PLT)
  
}

# assumes polygon data mtm joined to RTP
Is_Inside_Polygon <- function(RTP){
  
  RTP <- RTP %>%
    mutate(Px_old = !!sym(paste0("Px_", 5)),
           Py_old = !!sym(paste0("Py_", 5)),
           Is_Inside = -1)
  
  for (i in 1:5){
    
    RTP <- RTP %>%
      mutate(Px_new = !!sym(paste0("Px_", i)),
             Py_new = !!sym(paste0("Py_", i))) %>%
      mutate(Px_a = ifelse(Px_new > Px_old, Px_old, Px_new),
             Py_a = ifelse(Px_new > Px_old, Py_old, Py_new),
             Px_b = ifelse(Px_new > Px_old, Px_new, Px_old),
             Py_b = ifelse(Px_new > Px_old, Py_new, Py_old))
    
    
    RTP <- RTP %>%
      mutate(Is_Inside = ifelse((((Px_new < X_Pos & X_Pos <= Px_old) | (!(Px_new < X_Pos) & !(X_Pos <= Px_old))) & 
                                   ((Y_Pos - Py_a)*(Px_b - Px_a)) < ((Py_b - Py_a)*(X_Pos - Px_a))), -Is_Inside, Is_Inside))
    
    RTP <- RTP %>%
      mutate(Px_old = Px_new, Py_old = Py_new)
    
  }
  
  RTP <- RTP %>%
    mutate(Is_Inside = ifelse(Is_Inside == -1, 0, 1)) %>%
    select(-c("Px_a", "Px_b", "Py_a", "Py_b", "Px_old", "Py_old", "Px_new", "Py_new"))
  
  return(RTP)
  
}

Is_Inside_Single_Polygon <- function(Data, PolX, PolY){
  
  Data <- Data %>%
    mutate(Is_Inside = point.in.polygon(X_Pos, Y_Pos, PolX, PolY),
           Is_Inside = ifelse(Is_Inside >= 1, 1, 0))
  
  return(Data)
  
}

Calculate_Single_DME_Position_X <- function(Runway_X, DME, Runway_Heading){
  
  return(Runway_X + DME*sin(Runway_Heading - pi))
  
}

Calculate_Single_DME_Position_Y <- function(Runway_Y, DME, Runway_Heading){
  
  return(Runway_Y + DME*cos(Runway_Heading - pi))
  
}

Calculate_Intercept_Position_XY <- function(RTP, Prefix, X1Var, Y1Var, H1Var, X2Var, Y2Var, H2Var){
  IntX <- paste0(Prefix, "_X")
  IntY <- paste0(Prefix, "_Y")
  RTP <- RTP %>%
    mutate(X1 = !!sym(X1Var), Y1 = !!sym(Y1Var), H1 = !!sym(H1Var),
           X2 = !!sym(X2Var), Y2 = !!sym(Y2Var), H2 = !!sym(H2Var)) %>%
    mutate(!!sym(IntX) := ifelse(H1 != H2, (sin(H1)*(X2*cos(H2) - Y2*sin(H2)) - sin(H2)*(X1*cos(H1) - Y1*sin(H1))) / sin(H1-H2), NA),
           !!sym(IntY) := ifelse(H1 != H2, (cos(H1)*(X2*cos(H2) - Y2*sin(H2)) - cos(H2)*(X1*cos(H1) - Y1*sin(H1))) / sin(H1-H2), NA)) %>%
    select(-c("X1", "X2", "Y1", "Y2", "H1", "H2"))
  return(RTP)

}



Calculate_DME_Positions <- function(RTP, RXVar, RYVar, RHdgVar, DME){
  X_Var <- paste0("Runway_X_", DME/NM_to_m, "DME")
  Y_Var <- paste0("Runway_Y_", DME/NM_to_m, "DME")
  RTP <- RTP %>%
    mutate(!!sym(X_Var) := !!sym(RXVar) + DME*sin(!!sym(RHdgVar) - pi),
           !!sym(Y_Var) := !!sym(RYVar) + DME*cos(!!sym(RHdgVar) - pi))
  return(RTP)
}




#################################################################################################################################

#################################################################################################################################

#library(sp)



#################################################################################################################################

#################################################################################################################################

Generate_RTPD_Range_To_Threshold <- function(RTP, Runway, Method, ValorVer){
  
  # Get the Name of Apprach Path Variable (Dependent on VAL/VER)
  Approach_Path_Var <- Get_Name_Approach_Path(ValorVer)
  
  # Get the Track X/Y
  RTP <- select(RTP, Radar_Track_Point_ID, X_Pos, Y_Pos, !!sym(Approach_Path_Var))
  
  # Select Runway X/Y Co-ordinates
  Runway <- select(Runway, Runway_Name, Threshold_X_Pos, Threshold_Y_Pos)
  
  # Join on the tables
  RTP <- RTP %>% 
    left_join(Runway, by = setNames("Runway_Name", Approach_Path_Var))
  
  # Get the Range to Threshold based on Localiser
  RTP <- RTP %>%
    mutate(Range_To_Threshold = sqrt((X_Pos-Threshold_X_Pos)^2 + (Y_Pos-Threshold_Y_Pos)^2)) %>%
    select(Radar_Track_Point_ID, Range_To_Threshold)
  
  return(RTP)
  
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
    select(Radar_Track_Point_ID, Wind_SPD, Wind_HDG, Headwind_SPD, Wind_Effect_IAS)
  
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


Generate_RTPD_Corrected_Mode_C <- function(RTP, Baro){
  
  # Baro Update Window
  Update_Window <- 7200 # Not used.
  
  # Define Constants
  P0 <- 101325 # Sea Level Standard Pressure (Pa)
  T0 <- 288.15 # Sea Level Standard Temperature (K)
  g <- 9.80665 # Earth Gravitational Acceleration (ms-2)
  M <- 0.0289644 # Molar mass of Dry Air (kgmol-1)
  R <- 8.31447 # Universal Gas Constant (Jmol-1K-1)
  Exponent <- (g * M) / (R * T0)
  
  # Join on the Baro Pressure with a rolling join.
  RTP <- rolling_join(RTP, Baro, c("Track_Date", "Track_Time"), c("Baro_Date", "Baro_Time"), Roll = Inf)
  
  # Current SQL Solution - Set Pressure to SLS if NULL.
  RTP <- RTP %>%
    mutate(Baro_Pressure = ifelse(is.na(Baro_Pressure), P0, Baro_Pressure))
  
  # Calculate the Uncorrected Pressure.
  RTP <- RTP %>%
    mutate(Mode_C_Pressure = P0 * exp(-Exponent*Mode_C),
           Corrected_Mode_C_Pressure = Mode_C_Pressure + (P0 - Baro_Pressure),
           Corrected_Mode_C = log((Corrected_Mode_C_Pressure / P0)) / -Exponent) %>%
    select(Radar_Track_Point_ID, Corrected_Mode_C)
  
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
  RTP <- RTP %>% Calculate_DME_Positions("Threshold_X_Pos", "Threshold_Y_Pos", "Runway_Heading", DME = 4*NM_to_m)
  
  # Get the Ground Track Heading
  RTP <- RTP %>%
    Generate_RTPD_Ground_Track_Heading(Max_Heading_Diff)
  
  # Get the Direct Intercept Positions (Current Heading to ILS)
  RTP <- RTP %>%
    Calculate_Intercept_Position_XY(Prefix = "Intercept", "Threshold_X_Pos", "Threshold_Y_Pos", "Runway_Heading", "X_Pos", "Y_Pos", "Ground_Track_Heading") %>%
    mutate(Intercept_Dir_From_4DME = ifelse(Intercept_X >= Runway_X_4DME, "E", "W"))
  
  # Get the ILS Locus point (Intercept of ILS perpendicular to ILS) and it's range to threshold.
  RTP <- RTP %>%
    mutate(Runway_Heading_Perp = Runway_Heading + (pi/2)) %>%
    Calculate_Intercept_Position_XY(Prefix = "ILS_Locus", "Threshold_X_Pos", "Threshold_Y_Pos", "Runway_Heading", "X_Pos", "Y_Pos", "Runway_Heading_Perp") %>%
    mutate(ILS_Locus_RTT = sqrt((ILS_Locus_X - Threshold_X_Pos)^2 + (ILS_Locus_Y - Threshold_Y_Pos)^2 ),
           Direction_From_ILS = ifelse(Y_Pos >= ILS_Locus_Y, "N", "S"))
  
  # Get the Range to ILS.
  RTP <- RTP %>%
    mutate(Range_To_ILS = sqrt((ILS_Locus_X - X_Pos)^2 + (ILS_Locus_Y - Y_Pos)^2 ))
  
  # Get the intercept of 4DME perpendicular and line from current position parallel to ILS.
  RTP <- RTP %>%
    Calculate_Intercept_Position_XY(Prefix = "Perpendicular_4DME_Locus", "Runway_X_4DME", "Runway_Y_4DME", "Runway_Heading_Perp", "X_Pos", "Y_Pos", "Runway_Heading") %>%
    mutate(Direction_From_4DME = ifelse(X_Pos >= Perpendicular_4DME_Locus_X, "E", "W"))
  
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


Generate_RTPD_Min_Sustained_RoCD <- function(RTP, Adaptation){
  
  # Are we using the Vertical_Rate parameter?
  Use_Vertical_Rate <- F
  #Use_Vertical_Rate <- Adaptation$Use_Vertical_Rate
  
  Minimum_Period <- 20
  
  # Get the Radar Update Period
  Update_Period <- Adaptation$Radar_Update_Period
  Update_Period <- 6
  
  # Arrange by Flight Plan ID and Track Time
  RTP <- arrange(RTP, Flight_Plan_ID, Track_Time)
  
  # Generate the RoCD
  if (!Use_Vertical_Rate){
    RTP <- RTP %>%
      group_by(Flight_Plan_ID) %>%
      mutate(Previous_Mode_C = lag(Mode_C)) %>%
      ungroup() %>%
      mutate(RoCD = (Mode_C - Previous_Mode_C)/Update_Period)
  } else {
    RTP <- RTP %>%
      mutate(RoCD = Vertical_Rate)
  }
  
  # Get the number of Updates to consider
  Update_Count <- ceiling(Minimum_Period/Update_Period)
  
  Params <- c("RoCD")
  
  RTP <- group_by(RTP, Flight_Plan_ID)
  for (i in 1:(Update_Count-1)){
    Param <- paste0("RoCD_", i)
    Params <- append(Params, Param)
    RTP <- RTP %>%
      mutate(!!sym(Param) := lag(RoCD, n = i))
  }
  RTP <- RTP %>% ungroup() %>% 
    mutate(Min_Sustained_RoCD = pmin(!!!syms(Params), na.rm=T)) %>%
    select(Radar_Track_Point_ID, RoCD, Min_Sustained_RoCD)
  
  return(RTP)
}
  

Generate_RTPD_Localiser_Capture <- function(RTP, Runways, Localisers, Volumes, Polygons, Adaptation){
  
  # RTPO <- RTP
  # 
  # 
  # RTP <- RTPO
  
  Max_Heading_Diff <- Adaptation$Diff_Mode_S_To_Radar_Track_Max
  
  # Iinitialize The Mode S Localiser Capture as NA
  RTP <- RTP %>%
    mutate(Mode_S_Wind_Localiser_Capture = NA) %>%
    Generate_RTPD_Ground_Track_Heading(Max_Heading_Diff)
  
  # Loop across all Runways in alphabetical order
  # NOTE: Only returns the first localiser in capture region.
  for (i in 1:nrow(Runways)){
    
    #i <- 1
    
    # Get the ith Runway
    Runway <- Runways$Runway_Name[i]
    message(paste0("Attempting to match Localisers for Runway ", Runway, "."))
    
    # Get the Headings for this Runway
    RunwayHdgs <- filter(Localisers, Runway_Name == Runway)
    Min_HDG <- RunwayHdgs$Min_Heading
    Max_HDG <- RunwayHdgs$Max_Heading
    VolName <- RunwayHdgs$Volume_Name
    
    # Get the Polygon Points
    Pols <- filter(Polygons, Point_Sequence <= 4) %>%
      filter(Volume_Name == VolName)
    
    PolX <- Pols$Point_X
    PolY <- Pols$Point_Y
    
    # Get the Altitudes for this Runway
    RunwayAlts <- filter(Volumes, Volume_Name == VolName)
    Min_Alt <- RunwayAlts$Min_Alt
    Max_Alt <- RunwayAlts$Max_Alt
    
    # Calculate In_Volume
    RTP <-RTP %>% Is_Inside_Single_Polygon(PolX, PolY) %>% mutate(Min_Heading = Min_HDG, Max_Heading = Max_HDG) %>%
      Is_In_Heading_Range("Ground_Track_Heading", "Min_Heading", "Max_Heading") %>%
      select(-Min_Heading, -Max_Heading)
    
    # Generate a flag based on all criteria, update localiser if not already done so and all criteria met.
    RTP <- RTP %>%
      mutate(Flag = ifelse(Corrected_Mode_C >= Min_Alt & Corrected_Mode_C <= Max_Alt, 1, 0),
             Flag = ifelse(Is_Inside == 1, Flag, 0),
             Flag = ifelse(InHeadingRange == 1, Flag, 0),
             Mode_S_Wind_Localiser_Capture = ifelse(is.na(Mode_S_Wind_Localiser_Capture) & Flag == 1, Runway, Mode_S_Wind_Localiser_Capture)) %>%
      select(-Flag, -Is_Inside, -InHeadingRange)
    
  }
  
  RTP <- RTP %>%
    select(Radar_Track_Point_ID, Mode_S_Wind_Localiser_Capture)
  
  return(RTP)
  
}


Generate_RTPD_Path_Leg <- function(RTP, FP, Runway, Path_Legs, Path_Leg_Transitions, Volumes, Polygons, Adaptation, VarNo){

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
  RTP <- select(RTP, Radar_Track_Point_ID, Flight_Plan_ID, X_Pos, Y_Pos, Track_HDG, Mode_S_Track_HDG, Corrected_Mode_C, Min_Sustained_RoCD)
  FP <- select(FP, Flight_Plan_ID, Landing_Runway)
  RTP <- RTP %>%
    left_join(FP, by = c("Flight_Plan_ID"))
  
  # Calculate Ground Track Heading
  RTP <- RTP %>%
    Generate_RTPD_Ground_Track_Heading(Max_Heading_Diff)
  
  # Generate Relative Track ID to flight.
  RTP <- RTP %>%
    group_by(Flight_Plan_ID) %>%
    mutate(Track_ID = row_number()) %>%
    ungroup()
  
  # Get the Full Path Leg Transition Data
  PLT <- Generate_RTPD_Full_Path_Leg_Transition_Table(Path_Leg_Transitions, Volumes, Polygons)
  
  # Loop across Track Numbers
  Max_Points <- max(RTP$Track_ID)
  #Max_Points <- 5
  for (i in 1:Max_Points){
    
    #i <- 1
    message(paste0("Generating Path Leg ", i, " for all flights."))
    
    RTPi <- filter(RTP, Track_ID == i)
    if (i != 1){
      CPLs <- filter(RTP_New, Track_ID == (i-1)) %>% select(Flight_Plan_ID, Path_Leg)
    } else {
        CPLs <- select(RTPi, Flight_Plan_ID) %>% mutate(Path_Leg = "NULL")
      }
    
    FPi <- select(RTPi, Radar_Track_Point_ID, Flight_Plan_ID, Track_ID)
    RTPi <- RTPi %>% left_join(CPLs, by = c("Flight_Plan_ID")) %>% left_join(PLT, by = c("Path_Leg" = "Current_Path_Leg")) %>% select(-Radar_Track_Point_ID, -Track_ID)
    
    RTPi <- RTPi %>% mutate(
             Flag = ifelse(Corrected_Mode_C >= Min_Altitude & Corrected_Mode_C <= Max_Altitude, 1, 0),
             Flag = ifelse(Min_Sustained_RoCD >= Min_RoCD | is.na(Min_RoCD), Flag, 0)) %>%
      filter(Flag != 0) %>%
      mutate(Flag = ifelse((Runway_Name == Landing_Runway | Runway_Name == "NULL" & (Difference_Runway != Landing_Runway | Difference_Runway == "NULL")), Flag, 0)) %>%
      filter(Flag != 0) %>% Is_In_Heading_Range("Ground_Track_Heading", "Min_Heading", "Max_Heading") %>% mutate(Flag = ifelse(InHeadingRange == 1, Flag, 0)) %>%
      filter(Flag != 0) %>% Is_Inside_Polygon() %>% mutate(Flag = ifelse(Is_Inside == 1, Flag, 0)) %>% filter(Flag != 0)
  
    RTPi <- arrange(RTPi, Flight_Plan_ID, PLT_ID) %>% group_by(Flight_Plan_ID) %>% mutate(Temp = row_number()) %>% ungroup() %>% filter(Temp == 1) %>% select(-Temp)
    RTPi <- left_join(FPi, RTPi, by = c("Flight_Plan_ID")) %>% select(Radar_Track_Point_ID, Flight_Plan_ID, Track_ID, New_Path_Leg) %>%
      left_join(CPLs, by = c("Flight_Plan_ID")) %>%
      mutate(New_Path_Leg = ifelse(is.na(New_Path_Leg), Path_Leg, New_Path_Leg)) %>% select(-Path_Leg) %>%
      rename(Path_Leg = New_Path_Leg)
    
    if (i == 1){RTP_New <- RTPi} else {RTP_New <- rbind(RTP_New, RTPi)}
    
  }
  
  RTP_New <- arrange(RTP_New, Flight_Plan_ID, Track_ID) %>%
    select(Radar_Track_Point_ID, !!sym(Var) := Path_Leg)
  
  return(RTP_New)

}


Generate_FPD <- function(FP, Radar, Runways){
  
  # Local Adaptation
  Capture_Radius <- 350
  Max_Heading_Diff <- 5 * deg_to_rad
  Max_Mode_C <- 7500 * Ft_to_m
  CheckDMEs <- c(1, 4)
  Time_Vars <- c()
  Runway_Vars <- c()
  
  # Filter for Mode C 
  Radar <- filter(Radar, Mode_C <= Max_Mode_C)
  
  # Get Ground Track Heading (Simple)
  Radar <- Radar %>%
    mutate(Ground_Track_Heading = ifelse(is.na(Mode_S_Track_HDG), Track_HDG, Mode_S_Track_HDG),
           New_Runway = NA) %>%
    arrange(Flight_Plan_ID, Track_Time)
  
  for (i in 1:length(CheckDMEs)){
    DME <- CheckDMEs[i]
    message(paste0("Generating ", DME, "DME Times."))
    DME_m <- DME * NM_to_m
    Time_Var <- paste0("Time_At_", DME, "DME")
    Runway_Var <- paste0("Landing_Runway_", DME, "DME")
    Time_Vars <- append(Time_Vars, Time_Var)
    Runway_Vars <- append(Runway_Vars, Runway_Var)
    
    for (j in 1:nrow(Runways)){
      message (paste0("Looking at Runway ", Runways$Runway_Name[j]))
      Runway_X <- Runways$Threshold_X_Pos[j]
      Runway_Y <- Runways$Threshold_Y_Pos[j]
      Runway_Heading <- Runways$Heading[j]
      Radar <- Radar %>%
        mutate(Runway_Name = Runways$Runway_Name[j],
               Heading = Runways$Heading[j]) %>%
        mutate(DME_X_Pos = Calculate_Single_DME_Position_X(Runway_X, DME_m, Runway_Heading),
               DME_Y_Pos = Calculate_Single_DME_Position_Y(Runway_Y, DME_m, Runway_Heading)) %>%
        mutate(Flag = ifelse(Ground_Track_Heading >= Heading - Max_Heading_Diff &
                               Ground_Track_Heading <= Heading + Max_Heading_Diff, 1, 0),
               Flag = ifelse(X_Pos >= DME_X_Pos - Capture_Radius &
                               X_Pos <= DME_X_Pos + Capture_Radius, Flag, 0),
               Flag = ifelse(Y_Pos >= DME_Y_Pos - Capture_Radius &
                               Y_Pos <= DME_Y_Pos + Capture_Radius, Flag, 0)) %>%
        mutate(New_Runway = ifelse(Flag == 1, Runway_Name, New_Runway)) 
    }
    
    
    RadarT <- Radar %>% 
      filter(!is.na(New_Runway)) %>%
      group_by(Flight_Plan_ID) %>% 
      mutate(ID = row_number()) %>%
      mutate(Max_ID = max(ID, na.rm=T)) %>%
      ungroup() %>% filter(ID == Max_ID) %>%
      select(Flight_Plan_ID, 
             !!sym(Runway_Var) := New_Runway, !!sym(Time_Var) := Track_Time)
    
    Radar <- Radar %>% select(-Flag, -Runway_Name, -Heading, -DME_X_Pos, -DME_Y_Pos) %>% mutate(New_Runway = NA)
    
    FP <- left_join(FP, RadarT, by = c("Flight_Plan_ID"))
    
  }
  
  FP <- mutate(FP, New_Landing_Runway = NA)
  for (r in 1:length(Runway_Vars)){
    RVar <- Runway_Vars[r]
    FP <- mutate(FP, New_Landing_Runway = ifelse(!is.na(!!sym(RVar)), !!sym(RVar), New_Landing_Runway))
  }
  
  FP <- FP %>%
    select(Flight_Plan_ID, Landing_Runway = New_Landing_Runway, all_of(Time_Vars))
  
  return(FP)
  
}



# ------------------------------------------------------------------------------------ #

time_start <- Sys.time()
message("Loading Data...")
RTP <- sqlQuery(con, "SELECT * FROM tbl_Radar_Track_Point", stringsAsFactors = F)
FP <- sqlQuery(con, "SELECT * FROM tbl_Flight_Plan", stringsAsFactors = F)
Runways <- Load_Adaptation_Table(con, "tbl_Runway")
Path_Legs <- Load_Adaptation_Table(con, "tbl_Path_Leg")
Path_Leg_Transitions <- Load_Adaptation_Table(con, "tbl_Path_Leg_Transition")
Volumes <- Load_Adaptation_Table(con, "tbl_Volume")
Polygons <- Load_Adaptation_Table(con, "tbl_Polygon")
Adaptation <- Load_Adaptation_Table(con, "tbl_Adaptation_Data")
Localisers <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Localiser_Capture")
GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")
Baro <- sqlQuery(con, "SELECT * FROM tbl_Baro", stringsAsFactors = F)


# ------------------------------------------------------------------------------------ #

# Start Time
time_proc <- Sys.time()
message(paste0("Loaded data with: ", time_proc-time_start))

# Begin Flight Plan Derived
message("Generating Flight Plan Derived...")
FPD <- Generate_FPD(FP, RTP, Runways)
time_fp <- Sys.time()
message(paste0("Generated FPD with: ", time_fp-time_proc))

# Begin Radar Track Point Derived
message("Generating Radar Track Point Derived...")
Corrected_Mode_C <- Generate_RTPD_Corrected_Mode_C(RTP, Baro)
Wind_Parameters <- Generate_RTPD_Wind_Parameters(RTP, Adaptation)
RTP <- left_join(RTP, Corrected_Mode_C,by = c("Radar_Track_Point_ID"))
RoCD <- Generate_RTPD_Min_Sustained_RoCD(RTP, Adaptation)
LocaliserCaptures <- Generate_RTPD_Localiser_Capture(RTP, Runways, Localisers, Volumes, Polygons, Adaptation)
RTP <- left_join(RTP, LocaliserCaptures, by =c("Radar_Track_Point_ID"))
RTT <- Generate_RTPD_Range_To_Threshold(RTP, Runways, Method=1, ValorVer = "Val")
RTP <- left_join(RTP, RoCD, by = c("Radar_Track_Point_ID"))
PLs <- Generate_RTPD_Path_Leg(RTP, FP, Runway, Path_Legs, Path_Leg_Transitions, Volumes, Polygons, Adaptation, 1)
ILS <- Generate_RTPD_ILS_Relative_Fields(RTP, FP, Runways, GWCS_Adaptation)
RTPD <- select(RTP, Radar_Track_Point_ID) %>%
  left_join(Corrected_Mode_C, by = c("Radar_Track_Point_ID")) %>%
  left_join(Wind_Parameters, by = c("Radar_Track_Point_ID")) %>%
  left_join(RoCD, by = c("Radar_Track_Point_ID")) %>%
  left_join(LocaliserCaptures, by = c("Radar_Track_Point_ID")) %>%
  left_join(RTT, by = c("Radar_Track_Point_ID")) %>%
  left_join(PLs, by = c("Radar_Track_Point_ID")) %>%
  left_join(ILS, by = c("Radar_Track_Point_ID"))
RTPD <- mutate(RTPD, Path_Leg = ifelse(Path_Leg == "NULL", NA, Path_Leg))

time_rtt <- Sys.time()
message(paste0("Generated RTPD with: ", time_rtt-time_fp))
message(paste0("Completed Derived Fields with: ", time_rtt-time_start))

rm(PLs, ILS, RTT, LocaliserCaptures, RoCD, Wind_Parameters, Corrected_Mode_C)

################################################
RTPD_SQL <- sqlQuery(con, "SELECT * FROM tbl_Radar_Track_Point_Derived", stringsAsFactors = F)
FPD_SQL <- sqlQuery(con, "SELECT * FROM tbl_Flight_Plan_Derived", stringsAsFactors = F)
################################################


FP <- sqlQuery(con, "SELECT * FROM tbl_Flight_Plan", stringsAsFactors = F)
FPD_SQL <- sqlQuery(con, "SELECT * FROM tbl_Flight_Plan_Derived", stringsAsFactors = F)
LP_SQL <- sqlQuery(con, "SELECT * FROM tbl_Landing_Pair", stringsAsFactors = F)

LP <- Generate_Landing_Pair(FP, FPD_SQL)

LPTest <- full_join(LP, LP_SQL, by = c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID")) %>%
  filter(is.na(Landing_Pair_Type.y))

LPTest1 <- full_join(LP, LP_SQL, by = c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID")) %>%
  filter(Landing_Pair_ID.x %in% c(8393, 8394, 8395, 8396, 8397, 14531, 14532, 14533, 14534, 14535, 15666, 15667, 15668, 15669, 15770))



#return(select(RTP_Comp, Radar_Track_Point_ID, !!sym(Var)))

#}



