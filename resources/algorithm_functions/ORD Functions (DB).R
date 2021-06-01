library(dplyr)
library(data.table)

Get_Max_Valid_Value_2Var <- function(Data, New_Var, Var_1, Var_2){
  Data <- mutate(Data, 
                 !!sym(New_Var) := ifelse(!!sym(Var_1) > !!sym(Var_2), !!sym(Var_1), !!sym(Var_2)),
                 !!sym(New_Var) := ifelse(is.na(!!sym(New_Var)) & is.na(!!sym(Var_1)), !!sym(Var_2), !!sym(New_Var)),
                 !!sym(New_Var) := ifelse(is.na(!!sym(New_Var)) & is.na(!!sym(Var_2)), !!sym(Var_1), !!sym(New_Var)))
  return(Data)
}

Get_Min_Valid_Value_2Var <- function(Data, New_Var, Var_1, Var_2){
  Data <- mutate(Data, 
                 !!sym(New_Var) := ifelse(!!sym(Var_1) < !!sym(Var_2), !!sym(Var_1), !!sym(Var_2)),
                 !!sym(New_Var) := ifelse(is.na(!!sym(New_Var)) & is.na(!!sym(Var_1)), !!sym(Var_2), !!sym(New_Var)),
                 !!sym(New_Var) := ifelse(is.na(!!sym(New_Var)) & is.na(!!sym(Var_2)), !!sym(Var_1), !!sym(New_Var)))
  return(Data)
}

Get_Max_Valid_Value_Const <- function(Data, New_Var, Var_1, Constant){
  Data <- mutate(Data, !!sym(New_Var) := ifelse(is.na(!!sym(Var_1)) | !!sym(Var_1) < Constant, Constant, !!sym(Var_1)))
  return(Data)
}

Get_Value_If_Equal_2Var <- function(Data, New_Var, Dep_Var, Check_Var_1, Check_Var_2, Set_Var_1, Set_Var_2){
  
  Data <- mutate(Data, !!sym(New_Var) := ifelse(!!sym(Dep_Var) == !!sym(Check_Var_1), !!sym(Set_Var_1), NA))
  Data <- mutate(Data, !!sym(New_Var) := ifelse(is.na(!!sym(New_Var)) & !!sym(Dep_Var) == !!sym(Check_Var_2), 
                                                !!sym(Set_Var_2), !!sym(New_Var)))
                 
  return(Data)
  
}


Get_Not_In_Trail <- function(Landing_Pair){
  Landing_Pair <- filter(Landing_Pair, Landing_Pair_Type == "Not_In_Trail")
  return(Landing_Pair)
}

Get_In_Trail <- function(Landing_Pair){
  Landing_Pair <- filter(Landing_Pair, Landing_Pair_Type != "Not_In_Trail")
  return(Landing_Pair)
}

Order_Radar_Range <- function(Radar){
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Range_To_Threshold),]
  return(Radar)
}

Order_Radar_Range_Reverse <- function(Radar){
  Radar <- mutate(Inverse_RTT = 1e7 - Range_To_Threshold)
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Inverse_RTT),]
  Radar <- select(Radar, -c("Inverse_RTT"))
  return(Radar)
}

Order_Radar <- function(Radar){
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Track_Time),]
  return(Radar)
}

Order_Radar_Reverse <- function(Radar){
  Radar <- mutate(Radar, Inverse_Time = 86400 - Track_Time)
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Inverse_Time),]
  Radar <- select(Radar, -c("Inverse_Time"))
  return(Radar)
}

Order_Segments <- function(Segments){
  Segments <- Segments[order(Segments$ID, Segments$DME_Seg),]
  return(Segments)
}

Order_Landing_Pairs <- function(LPR, LPID_Var){
  LPR <- arrange(LPR, !!sym(LPID_Var))
  return(LPR)
}

# Function to join the correct ORD adaptation
Join_ORD_Adaptation <- function(LP, ORD_Profile_Selection, ORD_OP, ORD_AC, ORD_W, ORD_DBS){
  #LP <- left_join(LP, ORD_Runway_Reduced, by=c("Landing_Runway"="Runway_Name"))
  
  # Operator level added for PWS
  if (ORD_Profile_Selection == "Operator"){
    LP <- mutate(LP, AC_Operator_Pair = paste0(Aircraft_Type, "-", Operator))
    ORD_OP <- mutate(ORD_OP, AC_Operator_Pair = paste0(Aircraft_Type, "-", Operator)) %>% select-c("Aircraft_Type", "Operator")
    LP0 <- inner_join(LP, ORD_OP, by = c("AC_Operator_Pair"))
    LP1 <- filter(LP, AC_Operator_Pair %!in% ORD_AC$AC_Operator_Pair)
    LP1 <- inner_join(LP1, ORD_AC, by=c("Aircraft_Type"))
    LP2 <- filter(LP1, Aircraft_Type %!in% ORD_AC$Aircraft_Type)
    LP2 <- inner_join(LP2, ORD_W, by=c("Wake_Cat"))
    LP <- rbind(LP0, LP1, LP2) %>% select(-AC_Operator_Pair)
  }
  
  if (ORD_Profile_Selection == "Aircraft_Type"){
    LP1 <- inner_join(LP, ORD_AC, by=c("Aircraft_Type"))
    LP2 <- filter(LP, Aircraft_Type %!in% ORD_AC$Aircraft_Type)
    LP2 <- inner_join(LP2, ORD_W, by=c("Wake_Cat"))
    LP <- rbind(LP1, LP2)
  }
  
  if (ORD_Profile_Selection == "Wake"){
    LP <- inner_join(LP, ORD_W, by=c("Wake_Cat"))
  }
  
  if (ORD_Profile_Selection == "TBS_Table"){
    LP <- inner_join(LP, ORD_DBS, by=c("DBS_All_Sep_Distance"="DBS_Distance"))
  }
  
  return(LP)
}

# Function to Get Correct Compression Distances (LST/CCT) For ORD Observation. This
# means ORD Aircraft Profile can be run in correct order for Prediction.
Get_Compression_Distances <- function(LP, ORD_Operator, ORD_Aircraft, ORD_Wake, ORD_DBS, ORD_Profile_Selection, Distance){

  ORD_Aircraft_1 <- select(ORD_Aircraft, Aircraft_Type,
                           Local_Stabilisation_Distance = Local_Stabilisation_Distance_Lead,
                           Compression_Commencement_Threshold)
  ORD_Wake_1 <- select(ORD_Wake, Wake_Cat,
                       Local_Stabilisation_Distance = Local_Stabilisation_Distance_Lead,
                       Compression_Commencement_Threshold)
  ORD_DBS_1 <- select(ORD_DBS, DBS_Distance,
                      Local_Stabilisation_Distance = Local_Stabilisation_Distance_Lead,
                      Compression_Commencement_Threshold)
  LP <- select(LP, Landing_Pair_ID,
               Leader_Flight_Plan_ID,
               Aircraft_Type = Leader_Aircraft_Type,
               Wake_Cat = Leader_Recat_Wake_Cat,
               DBS_All_Sep_Distance)
  LP <- Join_ORD_Adaptation(LP, ORD_Profile_Selection, ORD_Operator, ORD_Aircraft_1, ORD_Wake_1, ORD_DBS_1)
  if (Distance == "LST"){LP <- select(LP, Landing_Pair_ID, Leader_Flight_Plan_ID, Local_Stabilisation_Distance)}
  if (Distance == "CCT"){LP <- select(LP, Landing_Pair_ID, Leader_Flight_Plan_ID, Compression_Commencement_Threshold)}
  
  return(LP)
}


Select_Nth_Observation <- function(Data, Group_Var, N){
  
  Data <- group_by(Data, !!sym(Group_Var)) %>% mutate(ID = row_number()) %>%
    ungroup() %>% filter(ID == N) %>% select(-ID)
  
  return(Data)
}

Extract_Nth_Radar_Time_RTT <- function(Data, Radar, N, Time_Var, RTT_Var, FPID){
  
  # Filter results above delivery point
  Radar <- filter(Radar, Flag == 1) %>% select(-c("Flag"))
  
  # Get the top result for each Flight Plan ID (a.k.a. closest to DME)
  Radar <- Select_Nth_Observation(Radar, "Flight_Plan_ID", 1) %>%
    select(Flight_Plan_ID, Track_Time, Range_To_Threshold) %>%
    rename(!!sym(Time_Var) := "Track_Time", !!sym(RTT_Var) := "Range_To_Threshold")
  
  # Perform Join to Landing Pair Reference
  Data <- left_join(Data, Radar, by = setNames("Flight_Plan_ID", FPID))
  
  return(Data)
}

Get_Follower_ILS_Join_Time <- function(Data, Radar, Path_Legs, PLTorGWCS){
  
  # Order Radar in Ascending Time Order
  Radar <- Order_Radar(Radar)
  
  # If Using GWCS ILS, Add Flag for Non-NA RTT Values
  if(PLTorGWCS == "GWCS"){Radar <- mutate(Radar, Flag = ifelse(!is.na(Range_To_Threshold), 1, 0))}
  if(PLTorGWCS == "PLT"){}
  
  # Get the Variables
  FPID <- "Follower_Flight_Plan_ID"
  Time_Var <- "Follower_ILS_Join_Time"
  RTT_Var <- "Follower_ILS_Join_RTT"
  
  # Extract The Follower Join Times/RTTs
  Data <- Extract_Nth_Radar_Time_RTT(Data, Radar, 1, Time_Var, RTT_Var, FPID)
  
  return(Data)
                                
}

# Calculate ORD Prediction Time: Do we want radar inside or outside?
Get_ORD_Prediction_Time <- function(Data, Radar, Path_Legs){
  
  # Order Radar Time Ascending
  Radar <- Order_Radar(INP_Radar)
  
  # Get Path Leg Type
  Path_Leg_Reduced <- select(Path_Legs, Path_Leg_Name, Path_Leg_Type)
  Radar <- left_join(Radar, Path_Leg_Reduced, by = c("Path_Leg" = "Path_Leg_Name"))
  
  # Add Flag for Path Leg
  Radar <- mutate(Radar, Flag = ifelse(!is.na(Path_Leg) & Path_Leg_Type != "Down_Wind", 1, 0))
  #Radar <- mutate(Radar, Flag = ifelse(!is.na(Path_Leg), 1, 0))
  
  # Get the Variables
  FPID <- "Follower_Flight_Plan_ID"
  Time_Var <- "Prediction_Time"
  RTT_Var <- "Prediction_RTT"
  
  # Extract The Follower Join Times/RTTs
  Data <- Extract_Nth_Radar_Time_RTT(Data, Radar, 1, Time_Var, RTT_Var, FPID) %>% select(-c("Prediction_RTT"))
  
  return(Data)
}



# Time At Fixed DME. 
Get_Time_At_Fixed_DME <- function(Data, Radar, DME, LorF, OrderBy){
  
  # Order Radar by Range To Threshold
  if(OrderBy == "Range"){Radar <- Order_Radar_Range(Radar)}
  if(OrderBy == "Time"){Radar <- Order_Radar(Radar)}
  
  # Get Variable names - If not Leader/Follower Default to FPD Names
  Time_Var <- paste0(LorF, "_", DME, "DME_Time")
  RTT_Var <- paste0(LorF, "_", DME, "DME_RTT")
  FPID <- paste0(LorF, "_Flight_Plan_ID")
  if (is.na(LorF)){
    FPID <- "Flight_Plan_ID"
    Time_Var <- paste0("Time_At_", DME, "DME")
    RTT_Var <- paste0("RTT_At_", DME, "DME")}
  
  # Create flag for RTT above delivery point
  Radar <- mutate(Radar, Flag = ifelse(Range_To_Threshold >= (DME * NM_to_m), 1, 0))
  
  # Extract the Data and Join on
  Data <- Extract_Nth_Radar_Time_RTT(Data, Radar, 1, Time_Var, RTT_Var, FPID)
  
  return(Data)
  
}




Get_Nth_Time_At_LP_Distance <- function(LPR, Radar, N, Prefix, Filter_Var, Crit_Var, LorF, OrderBy){
  
  # Order Radar by OrderBy
  if(OrderBy == "Range Ascending"){Radar <- Order_Radar_Range(Radar)}
  if(OrderBy == "Range Descending"){Radar <- Order_Radar_Range_Reverse(Radar)}
  if(OrderBy == "Time Ascending"){Radar <- Order_Radar(Radar)}
  if(OrderBy == "Time Descending"){Radar <- Order_Radar_Reverse(Radar)}
  
  # Get Variable names
  Time_Var <- paste0(LorF, "_", Prefix, "_Time")
  RTT_Var <- paste0(LorF, "_", Prefix, "_RTT")
  FPID <- paste0(LorF, "_Flight_Plan_ID")
  
  # Select Distance Variable and Join to Radar
  Criteria <- select(LPR, !!sym(FPID), !!sym(Crit_Var))
  Radar <- left_join(Radar, Criteria, by = setNames(FPID, "Flight_Plan_ID"))
  
  # Create flag for Either RTT or Track Time Over Criteria
  Radar <- mutate(Radar, Flag = ifelse(!!sym(Filter_Var) >= !!sym(Crit_Var), 1, 0))
  
  # Extract the Data and Join on
  LPR <- Extract_Nth_Radar_Time_RTT(LPR, Radar, N, Time_Var, RTT_Var, FPID)
  
  return(LPR)
  
}

# Calculate Interpolated Follower RTTs. Leader_Times is a DF with the
# Follower Flight Plan ID and the relevant Leader time at a certain distance.
Get_Follower_Interpolated_RTTs <- function(LPR, Radar, Leader_Time_Var){
  
  # Max Range to ILS Value to Consider
  Max_Range_To_ILS <- 3 * NM_to_m

  # Get the Leader Times
  Leader_Times <- select(LPR, "Follower_Flight_Plan_ID", !!sym(Leader_Time_Var))
  
  # Join Leader Times to Radar data
  Radar <- left_join(Radar, Leader_Times, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID")) %>%
    rename("Leader_Time" := !!sym(Leader_Time_Var))
  
  # Get Flag for Time above Leader Time
  Radar <- mutate(Radar, Flag = ifelse(Track_Time > Leader_Time, 1, 0))
  
  # Order Radar1 by Descending Time and Radar 2 by Ascending
  Radar1 <- Order_Radar_Reverse(Radar)
  Radar2 <- Order_Radar(Radar)
  
  # Get the First Track Point Above and Last Point Below
  Radar1 <- filter(Radar1, Flag == 0) %>% group_by(Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% filter(ID == 1) %>% 
    rename(First_Track_Distance = Range_To_Threshold, First_Track_Time = Track_Time, 
           First_Path_Leg = Path_Leg, First_ILS_Locus_RTT = ILS_Locus_RTT, First_Range_To_ILS = Range_To_ILS)
  Radar2 <- filter(Radar2, Flag == 1) %>% group_by(Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% filter(ID == 1) %>% 
    rename(Second_Track_Distance = Range_To_Threshold, Second_Track_Time = Track_Time, 
           Second_Path_Leg = Path_Leg, Second_ILS_Locus_RTT = ILS_Locus_RTT, Second_Range_To_ILS = Range_To_ILS) %>%
            select(-Leader_Time)
  
  #### MAYBE ADD: IF NA RANGE TO THRESHOLD AND ON INTERCEPT LEG - REPLACE WITH ILS LOCUS RTT + RANGE TO ILS
  #Radar1 <- mutate(Radar1, First_Track_Distance = 
  #                   ifelse(is.na(First_Track_Distance) & (str_detect(First_Path_Leg, "Int") | str_detect(First_Path_Leg, "ILS")) & First_Range_To_ILS < Max_Range_To_ILS, 
  #                          First_ILS_Locus_RTT,
  #                          First_Track_Distance))
  #Radar1 <- mutate(Radar1, Second_Track_Distance = 
  #                   ifelse(is.na(Second_Track_Distance) & (str_detect(Second_Path_Leg, "Int") | str_detect(Second_Path_Leg, "ILS")) & Second_Range_To_ILS < Max_Range_To_ILS, 
  #                          Second_ILS_Locus_RTT,
  #                          Second_Track_Distance))
  ##########################################################################################################
  
  # Join the Two Sets together
  Radar <- full_join(Radar1, Radar2, by = c("Flight_Plan_ID"))
  
  # Use ratios to find the Interpolated Distance
  Radar <- mutate(Radar, Distance_Delta = Second_Track_Distance - First_Track_Distance,
                   Time_Delta = Second_Track_Time - First_Track_Time,
                   Time_Delta_2 = Second_Track_Time - Leader_Time,
                   Time_Ratio = Time_Delta_2 / Time_Delta,
                   Interp_Distance = Second_Track_Distance - Time_Ratio*Distance_Delta) %>% select(Flight_Plan_ID, Interp_Distance)
  return(Radar)
}


Get_Interpolated_Separation_At_DME <- function(LPR, Radar, DME){
  
  # Get Required Variable Names
  Time_Var <- paste0("Leader_", DME, "DME_Time")
  RTT_Var <- paste0("Leader_", DME, "DME_RTT")
  Interp_Var <- paste0("Follower_Int_", DME, "DME_RTT")
  Sep_Var <- paste0("Observed_", DME, "DME_Separation_Distance")
  
  # Get the Leader Times
  Leader_Times <- select(LPR, c("Follower_Flight_Plan_ID", !!sym(Time_Var)))
  
  # Interpolate & Join
  RTT1 <- Get_Follower_Interpolated_RTTs(LPR, Radar, Time_Var) %>% rename(!!sym(Interp_Var) := "Interp_Distance")
  LPR <- left_join(LPR, RTT1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  # Add Delivered Separation
  LPR <- mutate(LPR, !!sym(Sep_Var) := !!sym(Interp_Var) - !!sym(RTT_Var))
  
  return(LPR)
  
}


Get_Observed_Separation_Time_At_DME <- function(LPR, DME){
  
  # Get Variables
  Leader_Var <- paste0("Leader_", DME, "DME_Time")
  Follower_Var <- paste0("Follower_", DME, "DME_Time")
  Sep_Var <- paste0("Observed_", DME, "DME_Separation_Time")
  
  # Calculate Time Separation
  LPR <- mutate(LPR, !!sym(Sep_Var) := !!sym(Follower_Var) - !!sym(Leader_Var))
  
  return(LPR)
  
}

Get_Interpolated_Compression_Distances <- function(LPR, Radar, ORDorWAD, Start_Time_Var, End_Time_Var){
  
  # Get the Start/End Labels based on ORD or WAD
  Start_Interp_Var <- paste0("Follower_", ORDorWAD, "_Start_RTT")
  End_Interp_Var <- paste0("Follower_", ORDorWAD, "_Stop_RTT")
  
  # Get the Follower Start RTT and Join
  RTT1 <- Get_Follower_Interpolated_RTTs(LPR, Radar, Start_Time_Var) %>% rename(!!sym(Start_Interp_Var) := "Interp_Distance")
  LPR <- left_join(LPR, RTT1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  # Get the Follower Stop RTT and Join
  RTT2 <- Get_Follower_Interpolated_RTTs(LPR, Radar, End_Time_Var) %>% rename(!!sym(End_Interp_Var) := "Interp_Distance")
  LPR <- left_join(LPR, RTT2, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  return(LPR)
}

Get_Observed_Compression_Values <- function(LPR, ORDorWAD){
  
  # Get Variables
  if (ORDorWAD == "ORD"){
    Leader_Start_Var <- "Leader_ORD_FAF_RTT"
    Leader_Stop_Var <- "Leader_Delivery_RTT"
  }
  if (ORDorWAD == "WAD"){
    Leader_Start_Var == "Leader_CC_RTT"
    Leader_Stop_Var == "Leader_WAD_FAF_RTT"
  }
  Foll_Start_Var <- paste0("Follower_", ORDorWAD, "_Start_RTT")
  Foll_Stop_Var <- paste0("Follower_", ORDorWAD, "_Stop_RTT")
  Comp_Var <- paste0("Observed_", ORDorWAD, "_Compression")
  
  # Calculate Observed Compression
  LPR <- mutate(LPR, !!sym(Comp_Var) := (!!sym(Foll_Start_Var) - !!sym(Foll_Stop_Var)) - (!!sym(Leader_Start_Var) - !!sym(Leader_Stop_Var)))
  
  return(LPR)
  
}



# Calculate Surface Wind Parameters. Join_Time, Join_Date, Join_Runway need to 
# be specified before function application, and variables output should be renamed.
Get_Surface_Wind <- function(Data, SW, Runways, Prefix, ID_Var, Date_Var, Time_Var, Runway_Var){
  
  # Get Variable Names
  Wind_SPD_Var <- paste0(Prefix, "_Surface_Wind_SPD")
  Wind_HDG_Var <- paste0(Prefix, "_Surface_Wind_HDG")
  Headwind_Var <- paste0(Prefix, "_Surface_Headwind")
  
  # Get the Required Join Parameters (LP)
  Data <- mutate(Data,
                Join_Time := !!sym(Time_Var),
                Join_Date := !!sym(Date_Var),
                Join_Runway := !!sym(Runway_Var))
  
  # Get the Required Join Parameters (SW)
  SW <- mutate(SW, 
               Join_Time = Anemo_Time,
               Join_Date = Anemo_Date,
               Join_Runway = Landing_Runway)
  
  # Convert to Data Tables and Set Keys for Rolling Join
  SW <- as.data.table(SW)
  Data <- as.data.table(Data)
  setkey(SW, Join_Runway, Join_Date, Join_Time)
  setkey(Data, Join_Runway, Join_Date, Join_Time)
  
  # Do Rolling Join and convert back to data frame
  Data <- SW[Data, roll = Inf]
  Data <- as.data.frame(Data)
  
  # Get Surface Headwind Parameter
  Runways2 <- select(Runways, Runway_Name, Heading)
  Data <- left_join(Data, Runways2, by = c("Join_Runway" = "Runway_Name"))
  Data <- mutate(Data, Anemo_HW = Get_2D_Scalar_Product(Anemo_SPD, Anemo_HDG, 1, Heading))
  
  # Select appropriate parameters and rename
  Wind_Parameters <- select(Data, !!sym(ID_Var), Anemo_SPD, Anemo_HDG, Anemo_HW) %>%
    rename(!!sym(Wind_SPD_Var) := "Anemo_SPD",
           !!sym(Wind_HDG_Var) := "Anemo_HDG",
           !!sym(Headwind_Var) := "Anemo_HW")
  
  # Join values back onto Data.
  Data <- left_join(Data, Wind_Parameters, by = setNames(ID_Var, ID_Var)) %>% 
    select(-c("Join_Time", "Join_Date", "Join_Runway", "Heading", "Anemo_HW",
              "Landing_Runway", "Anemo_SPD", "Anemo_HDG", "Anemo_Date", "Anemo_Time"))
  
  return(Data)
}



# Distances should include a Flight Plan ID, a Start Distance and End Distance
# This can be used for PM and ORD. Outputs Speed and WE Trapezium average
# across a distance window between start and end distance of Distances.
Get_Average_Observed_Mode_S_Parameters <- function(LPR, Radar, Prefix, LorF, TimeorRange, Start_Var, End_Var){
  
  # Get Variable Names
  FPID <- paste0(LorF, "_Flight_Plan_ID")
  
  # Get Relevant Pair Data
  Pair_Data <- select(LPR, !!sym(FPID), !!sym(Start_Var), !!sym(End_Var)) %>% rename("Flight_Plan_ID" := !!sym(FPID))
  
  # Join on the distances by Flight Plan ID
  Radar <- left_join(Radar, Pair_Data, by = c("Flight_Plan_ID"))
  
  # Filter for RTT within the Distance Bounds
  if (TimeorRange == "Range"){Radar <- rename(Radar, "End_Distance" := !!sym(End_Var), "Start_Distance" := !!sym(Start_Var))
    Radar <- filter(Radar, Range_To_Threshold >= End_Distance & Range_To_Threshold <= Start_Distance)}
  
  # Filter for Track_Time within the Time Bounds
  if (TimeorRange == "Time"){Radar <- rename(Radar, "End_Time" := !!sym(End_Var), "Start_Time" := !!sym(Start_Var))
    Radar <- filter(Radar, Track_Time <= End_Time & Track_Time >= Start_Time)}
  
  # Filter to remove NA Wind Effect/IAS Values
  Radar <- filter(Radar, !is.na(Mode_S_IAS) & !is.na(Wind_Effect_IAS))
  
  # Order by Flight Plan ID & Track Time
  Radar <- Order_Radar(Radar)
  
  # Get a Sequence Number
  Radar <- group_by(Radar, Flight_Plan_ID) %>% mutate(Sequence_Number = row_number()) %>% ungroup()
  
  # Take Required Fields from Radar
  Radar2 <- select(Radar, Flight_Plan_ID, Sequence_Number, Track_Time, Mode_S_IAS, Wind_Effect_IAS)
  
  # Change Sequence number to next number. Change names of parameters.
  Radar2 <- mutate(Radar2, Sequence_Number = Sequence_Number + 1) %>%
    rename(Previous_Track_Time = Track_Time, Previous_Mode_S_IAS = Mode_S_IAS, Previous_Wind_Effect_IAS = Wind_Effect_IAS)
  
  # Join on the Previous Parameters
  Radar <- left_join(Radar, Radar2, by = c("Flight_Plan_ID", "Sequence_Number"))
  
  # Remove Radar2
  rm(Radar2)
  
  # Get the Delta beween Track_Time and Previous_Track_Time
  Radar <- mutate(Radar, Track_Time_Delta = Track_Time - Previous_Track_Time)
  
  # Get each Observation's Contribution to the Trapezium sum: IAS
  Radar <- mutate(Radar, Observed_Mean_IAS = Track_Time_Delta * (Mode_S_IAS + Previous_Mode_S_IAS) / 2)
  
  # Get each Observation's Contribution to the Trapezium sum: Wind Effect
  Radar <- mutate(Radar, Observed_Mean_Wind_Effect = Track_Time_Delta * (Wind_Effect_IAS + Previous_Wind_Effect_IAS) / 2)
  
  # Sum Track Time Delta, Observed Mean IAS/Wind Effect by Flight Plan ID
  Radar <- group_by(Radar, Flight_Plan_ID) %>% summarise(Total_Track_Time_Delta = sum(Track_Time_Delta, na.rm=T),
                                                         Observed_Mean_IAS = sum(Observed_Mean_IAS, na.rm=T),
                                                         Observed_Mean_Wind_Effect = sum(Observed_Mean_Wind_Effect, na.rm=T)) %>% ungroup()
  
  # Divide the Observed sums by the Track Time delta to get the Trapezium rule average
  Radar <- mutate(Radar,
                  Observed_Mean_IAS = Observed_Mean_IAS / Total_Track_Time_Delta,
                  Observed_Mean_Wind_Effect = Observed_Mean_Wind_Effect / Total_Track_Time_Delta) %>%
    select(-Total_Track_Time_Delta)
  
  # Get Variable Names
  IAS_Var <- paste0("Observed_", LorF, "_", Prefix, "_IAS")
  WE_Var <- paste0("Observed_", LorF, "_", Prefix, "_Wind_Effect")
  
  # Rename Appropriately
  Radar <- rename(Radar, 
                  !!sym(IAS_Var) := "Observed_Mean_IAS",
                  !!sym(WE_Var) := "Observed_Mean_Wind_Effect")
  
  # Join on to Landing Pair Reference
  LPR <- left_join(LPR, Radar, by = setNames("Flight_Plan_ID", FPID))
  
  # Return the Observed parameters.
  return(LPR)
  
}

# Requires Validation Format. 
# Distances: A Data-frame with LAnding Pair ID, Start Distance and End Distance
# Speeds: A dataframe with Landing Pair ID and Reference Speeds
Get_Average_Forecast_Wind_Effect <- function(Data, Segment_Forecast, Prefix, ID_Var, Start_Dist_Var, End_Dist_Var, Speed_Var, Seg_Size){
  
  # Order Segments
  Segment_Forecast <- Order_Segments(Segment_Forecast)
  
  # Rename ID to Relevant ID Variable
  Segment_Forecast <- rename(Segment_Forecast, !!sym(ID_Var) := "ID")
  
  # Get the Distances and Speeds Data
  Distances <- select(Data, !!sym(ID_Var), !!sym(Start_Dist_Var), !!sym(End_Dist_Var))
  Speeds <- select(Data, !!sym(ID_Var), !!sym(Speed_Var))
  
  # Join on the Distances/Speeds to the Segs
  Segment_Forecast <- left_join(Segment_Forecast, Distances, by = setNames(ID_Var, ID_Var))
  Segment_Forecast <- left_join(Segment_Forecast, Speeds, by = setNames(ID_Var, ID_Var))
  
  # Filter for Segments within Distance bounds somehow (Needs additions - need to incorporate seg size) ## EDIT: CHANGED TO USE FLOOR OF NM DISTANCE
  Segment_Forecast <- filter(Segment_Forecast, 
                             DME_Seg >= floor(!!sym(End_Dist_Var) + NM_to_m - Seg_Size) & 
                               DME_Seg <= (!!sym(Start_Dist_Var) - NM_to_m + Seg_Size))
  
  # Get the Segment Size Delta
  Segment_Forecast <- mutate(Segment_Forecast, 
                             Distance_Delta = ifelse((DME_Seg + Seg_Size) > !!sym(Start_Dist_Var), !!sym(Start_Dist_Var) - DME_Seg, Seg_Size),
                             Time_Delta = Distance_Delta / (!!sym(Speed_Var) + Forecast_Wind_Effect_IAS))
  
  # Get the Total Distance/Time Flown for each Pair
  Segment_Stats <- group_by(Segment_Forecast, !!sym(ID_Var)) %>%
    summarise(Total_Distance = sum(Distance_Delta),
              Total_Time = sum(Time_Delta)) %>% ungroup()
  
  # Rejoin Speeds
  Segment_Stats <- left_join(Segment_Stats, Speeds, by = setNames(ID_Var, ID_Var))
  
  # Wind Effect Variable Name
  WE_Var <- paste0("Follower_Forecast_", Prefix, "_Wind_Effect")
  
  # Get the Forecast GSPD and Wind Effect
  Segment_Stats <- mutate(Segment_Stats,
                          Forecast_GSPD = Total_Distance / Total_Time,
                          Forecast_Wind_Effect_IAS = Forecast_GSPD - !!sym(Speed_Var)) %>%
    select(!!sym(ID_Var), Forecast_Wind_Effect_IAS) %>% rename(!!sym(WE_Var) := "Forecast_Wind_Effect_IAS")
  
  # Join back onto Data
  Data <- left_join(Data, Segment_Stats, by = setNames(ID_Var, ID_Var))
  
  return(Data)  
  
} 

Get_LF_Ref_Parameters <- function(Landing_Pair, Flight_Plan, Runways, ACTW, ACTWL, LorF){
  
  # Get the Variable Names
  FPID <- paste0(LorF, "_Flight_Plan_ID")
  Runway_Var <- paste0(LorF, "_Landing_Runway")
  FP_Time_Var <- paste0(LorF, "_FP_Time")
  AC_Type_Var <- paste0(LorF, "_Aircraft_Type")
  Csn_Var <- paste0(LorF, "_Callsign")
  New_Wake_Var <- paste0(LorF, "_Recat_Wake_Cat")
  Old_Wake_Var <- paste0(LorF, "_Legacy_Wake_Cat")
  Time_4DME_Var <- paste0(LorF, "_Time_At_4DME")
  
  # Join L/F ID to Flight Plan and Rename Variables
  Landing_Pair <- left_join(Landing_Pair, Flight_Plan, by = setNames("Flight_Plan_ID", FPID)) %>%
    rename(!!sym(Runway_Var) := "Landing_Runway",
           !!sym(FP_Time_Var) := "FP_Time",
           !!sym(AC_Type_Var) := "Aircraft_Type",
           !!sym(Csn_Var) := "Callsign",
           !!sym(Time_4DME_Var) := "Time_At_4DME")
  
  # Join L/F ID to AC to Wake and Rename
  Landing_Pair <- left_join(Landing_Pair, ACTW, by = setNames("Aircraft_Type", AC_Type_Var)) %>%
    rename(!!sym(New_Wake_Var) := "Wake")
  Landing_Pair <- left_join(Landing_Pair, ACTWL, by = setNames("Aircraft_Type", AC_Type_Var)) %>%
    rename(!!sym(Old_Wake_Var) := "Wake")
  
  # Join on Runway Group/MRS (NEED TO ADD MRS TO SQL TBL_RUNWAY - 3NM FOR NOW)
  Runway_Reduced <- select(Runways, Runway_Name, Runway_Group) %>%
    mutate(Min_Radar_Separation = 3 * NM_to_m)
  
  # Do for Followers (Doesn't matter which)
  if(LorF == "Follower"){Landing_Pair <- left_join(Landing_Pair, Runway_Reduced, by = c("Follower_Landing_Runway" = "Runway_Name"))}
  
  return(Landing_Pair)
  
}

Get_Pair_Ref_Parameter <- function(con, Landing_Pair, Ref_Source_Type, RecatorLegacy, Constraint, ACT_Enabled, Operator_Enabled){
  
  # Operation settings
  
  # Get the Wake Category Variable names (Aircraft Type/Operator variables are named consistently).
  Lead_Wake_Var <- paste0("Leader_", RecatorLegacy, "_Wake_Cat") 
  Foll_Wake_Var <- paste0("Follower_", RecatorLegacy, "_Wake_Cat")
  Lead_Wake_Var2 <- ifelse(RecatorLegacy == "Recat", "Leader_WTC", "Leader_WVI") # Should probably change the DB to make these the same
  Foll_Wake_Var2 <- ifelse(RecatorLegacy == "Recat", "Follower_WTC", "Follower_WVI")
  
  # Get the Final Output Variable name
  Final_Outp_Var <- paste0("Reference_", RecatorLegacy, "_", Constraint, "_", Ref_Source_Type)
  
  # Group Constraints by Pairing Level (Currently only supports Wake Separation/ROT Spacing)
  if (Constraint %in% c("Wake_Separation")){Level <- "Type"}
  if (Constraint %in% c("ROT_Spacing", "Non_Wake_Separation", "Spacing")){Level <- "Runway, Type"}
  if (Constraint %in% c("Runway_Dependent")){Level <- "Runway Pair, Type"}
  
  
  # Load the reference data to be joined depending on New/Old operations (RecatorLegacy), 
  # Separation/Spacing Constraint (Constraint) and time/ias/distance parameter (Ref_Source_Type)
  if (RecatorLegacy == "Legacy"){
    
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Distance"){Ref_Source <- Load_Adaptation_Table(con, "tbl_DBS_Wake_Turbulence")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Time"){Ref_Source <- Load_Adaptation_Table(con, "tbl_TBS_Wake_Turbulence")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "IAS"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Assumed_Legacy_Wake_Separation_IAS")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Distance"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Reference_Legacy_ROT_Spacing_Dist")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Time"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Reference_Legacy_ROT_Spacing_Time")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "IAS"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Assumed_Legacy_ROT_Spacing_IAS")}
    
  }
  
  # NOTE: Operator and ACT can be independent (may be the case for ROT times for example)
  if (RecatorLegacy == "Recat"){
    
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Distance"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Reference_Recat_Separation_Dist")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Time"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Reference_Recat_Separation_Time")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "IAS"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Assumed_Recat_Separation_IAS")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Distance"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Reference_ROT_Spacing_Dist")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Time"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Reference_ROT_Spacing_Time")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "IAS"){Ref_Source <- Load_Adaptation_Table(con, "tbl_Assumed_ROT_Spacing_IAS")}
    
  }
  
  
  # For New operations only: if ACT_Enabled then get Aircraft type pair parameters (e.g. PWS Wake)
  if (ACT_Enabled == T){
    
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Distance"){Ref_Source_1 <- Load_Adaptation_Table(con, "tbl_Reference_ACTP_Wake_Separation_Dist")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Time"){Ref_Source_1 <- Load_Adaptation_Table(con, "tbl_Reference_ACTP_Wake_Separation_Time")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "IAS"){Ref_Source_1 <- Load_Adaptation_Table(con, "tbl_Assumed_ACTP_Wake_Separation_IAS")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Distance"){Ref_Source_1 <- Load_Adaptation_Table(con, "tbl_Reference_ACTP_ROT_Spacing_Dist")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Time"){Ref_Source_1 <- Load_Adaptation_Table(con, "tbl_Reference_ACTP_ROT_Spacing_Time")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "IAS"){Ref_Source_1 <- Load_Adaptation_Table(con, "tbl_Assumed_ACTP_ROT_Spacing_IAS")}
    
  }
  
  # For New operations only, and only if we decide to calibrate operator/type pairs.
  if (Operator_Enabled == T){
    
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Distance"){Ref_Source_2 <- Load_Adaptation_Table(con, "tbl_Reference_AC_Operator_Wake_Separation_Dist")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "Time"){Ref_Source_2 <- Load_Adaptation_Table(con, "tbl_Reference_AC_Operator_Wake_Separation_Time")}
    if (Constraint == "Wake_Separation" & Ref_Source_Type == "IAS"){Ref_Source_2 <- Load_Adaptation_Table(con, "tbl_Assumed_AC_Operator_Wake_Separation_IAS")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Distance"){Ref_Source_2 <- Load_Adaptation_Table(con, "tbl_Reference_AC_Operator_ROT_Spacing_Dist")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "Time"){Ref_Source_2 <- Load_Adaptation_Table(con, "tbl_Reference_AC_Operator_ROT_Spacing_Time")}
    if (Constraint == "ROT_Spacing" & Ref_Source_Type == "IAS"){Ref_Source_2 <- Load_Adaptation_Table(con, "tbl_Assumed_AC_Operator_ROT_Spacing_IAS")}
    
  }
  
  
  # Create placeholder column for which the hierarchy is applied
  Landing_Pair <- mutate(Landing_Pair, Placeholder = NA)
  
  # Begin vlaue assignment based on hierarchy: Operator -> Aircraft -> Wake
  # Only uses data if it has been loaded
  # Structure for thre tiers very similar so only below tier described
  if (exists("Ref_Source_2")){
    
    # Rename the desired variable to the new variable name for this tier
    Outp_Var_2 <- paste0("Reference_", "AC_Operator", "_", Constraint, "_", Ref_Source_Type)
    Ref_Var_2 <- colnames(Ref_Source_2)[ncol(Ref_Source_2)]
    Ref_Source_2 <- rename(Ref_Source_2, !!sym(Outp_Var_2) := !!sym(Ref_Var))
    
    # If a Wake separation, join by aircraft types and operators
    if (Level == "Type"){
      Landing_Pair <- left_join(Landing_Pair, Ref_Source_2, 
                                by = c(setNames("Leader_Aircraft_Type", "Leader_Aircraft_Type"), 
                                       setNames("Follower_Aircraft_Type", "Follower_Aircraft"),
                                       setNames("Leader_Operator", "Leader_Operator"), 
                                       setNames("Follower_Operator", "Follower_Operator")))
    }
    
    # If a runway pair grouping join by Aircraft Types, Operators and any Landing Runway (Follower used, Not-in-trails are NA for now)
    # Note: May need to add Runway Pair ROT Distances - unsure of scope for now
    if (Level == "Runway, Type"){
      Landing_Pair <- left_join(Landing_Pair, Ref_Source_2, 
                                by = c(setNames("Leader_Aircraft_Type", "Leader_Aircraft_Type"), 
                                       setNames("Follower_Aircraft_Type", "Follower_Aircraft"),
                                       setNames("Leader_Operator", "Leader_Operator"), 
                                       setNames("Follower_Operator", "Follower_Operator"),
                                       setNames("Runway", "Follower_Landing_Runway"))) %>%
        mutate(!!sym(Outp_Var_2) := ifelse(Leader_Landing_Runway != Follower_Landing_Runway, NA, !!sym(Outp_Var_2)))
    }
    
    # Replace NA values in placeholder with Non-NA values from this tier, remove tier variable
    Landing_Pair <- mutate(Landing_Pair, Placeholder = ifelse(is.na(Placeholder), !!sym(Outp_Var_2, Placeholder))) %>%
      select(-!!sym(Outp_Var_2))
    
  }
  
  if (exists("Ref_Source_1")){
    
    Outp_Var_1 <- paste0("Reference_", "ACTP", "_", Constraint, "_", Ref_Source_Type)
    Ref_Var_1 <- colnames(Ref_Source_1)[ncol(Ref_Source_1)]
    Ref_Source_1 <- rename(Ref_Source_1, !!sym(Outp_Var_1) := !!sym(Ref_Var_1))
    
    
    if (Level == "Type"){
      Landing_Pair <- left_join(Landing_Pair, Ref_Source_1, 
                                by = c(setNames("Leader_Aircraft_Type", "Leader_Aircraft_Type"), 
                                       setNames("Follower_Aircraft_Type", "Follower_Aircraft")))
    }
    
    if (Level == "Runway, Type"){
      Landing_Pair <- left_join(Landing_Pair, Ref_Source_1, 
                                by = c(setNames("Leader_Aircraft_Type", "Leader_Aircraft_Type"), 
                                       setNames("Follower_Aircraft_Type", "Follower_Aircraft"),
                                       setNames("Runway", "Follower_Landing_Runway")))
    }
    
    Landing_Pair <- mutate(Landing_Pair, Placeholder = ifelse(is.na(Placeholder), !!sym(Outp_Var_1), Placeholder)) %>%
      select(-!!sym(Outp_Var_1))
    
  }
  
  if (exists("Ref_Source")){
    
    Outp_Var <- Final_Outp_Var
    Ref_Var <- colnames(Ref_Source)[ncol(Ref_Source)]
    Ref_Source <- rename(Ref_Source, !!sym(Outp_Var) := !!sym(Ref_Var))
    
    if (Level == "Type"){
      Landing_Pair <- left_join(Landing_Pair, Ref_Source, 
                                by = c(setNames(Lead_Wake_Var2, Lead_Wake_Var), 
                                       setNames(Foll_Wake_Var2, Foll_Wake_Var)))
    }
    
    if (Level == "Runway, Type"){
      Landing_Pair <- left_join(Landing_Pair, Ref_Source, 
                                by = c(setNames(Lead_Wake_Var2, Lead_Wake_Var), 
                                       setNames(Foll_Wake_Var2, Foll_Wake_Var),
                                       setNames("Runway", "Follower_Landing_Runway")))
    }
    
    Landing_Pair <- mutate(Landing_Pair, Placeholder = ifelse(is.na(Placeholder), !!sym(Outp_Var), Placeholder)) %>%
      select(-!!sym(Outp_Var))
    
    
  }
  
  # Rename the placeholder variable
  Landing_Pair <- rename(Landing_Pair, !!sym(Final_Outp_Var) := "Placeholder")
  
  return(Landing_Pair)
  
}

Get_Dependent_Runway_Offset_Changes <- function(Landing_Pair, Runway_Offsets){
  
  # Join on the Offsets
  Landing_Pair <- left_join(Landing_Pair, Runway_Offsets, by = c("Leader_Landing_Runway", "Follower_Landing_Runway")) %>% 
    mutate(Runway_Offset = ifelse(is.na(Runway_Offset), 0, Runway_Offset))
  
  # Make Relevant Changes to DBS Distances
  Landing_Pair <- mutate(Landing_Pair, Reference_Recat_Wake_Separation_Distance = Reference_Recat_Wake_Separation_Distance + Runway_Offset)
  Landing_Pair <- mutate(Landing_Pair, Reference_Recat_ROT_Spacing_Distance = Reference_ROT_Spacing_Distance + Runway_Offset)
  Landing_Pair <- mutate(Landing_Pair, Reference_Legacy_Wake_Separation_Distance = Reference_Legacy_Wake_Separation_Distance + Runway_Offset)
  Landing_Pair <- mutate(Landing_Pair, DBS_All_Sep_Distance = DBS_All_Sep_Distance + Runway_Offset)
  
  return(Landing_Pair)
}



Get_Forecast_Wind_Effect_At_DME <- function(Segments, Aircraft_Profile, ID_Var, DME){
  
  # Get Variable Name
  WE_Var <- paste0("Wind_Effect_", DME, "DME")
  
  # Get the Wind Effects from the Segment Data
  Wind_Effects <- filter(Segments, DME_Seg == DME * NM_to_m) %>%
    rename(!!sym(WE_Var) := "Forecast_Wind_Effect_IAS")
  
  # Join Wind Effect to Aircraft Profile
  Aircraft_Profile <- left_join(Aircraft_Profile, Wind_Effects, by = setNames("ID", ID_Var))
  
  return(Aircraft_Profile)
  
}

Select_ORD_LF_Adaptation <- function(Ref_Data, Level, LorF, ORDBuffers){
  
  # Change "Leader" to "Lead" For Variable Compatibility
  if(LorF == "Leader"){LorF <- "Lead"}
  
  # Define Variable Unique to Adaptation Level
  if(Level == "DBS"){Unique_Var <- "DBS_Distance"}
  if(Level == "Wake"){Unique_Var <- "Wake_Cat"}
  if(Level == "Aircraft"){Unique_Var <- "Aircraft_Type"}
  if(Level == "Operator"){Unique_Var <- "Aircraft_Type"  ## Added for PWS
                          Unique_Var2 <- "Operator"}
  
  # Get Other Variable Names
  LSS_Type_Var <- paste0("Landing_Stabilisation_Speed_Type_", LorF)
  VRef_Var <- paste0("Min_Safe_Landing_Speed_", LorF)
  Gusting_Var <- paste0("Apply_Gusting_", LorF)
  LSD_Var <- paste0("Local_Stabilisation_Distance_", LorF)
  End_Fin_Decel_Var <- paste0("End_Final_Deceleration_Distance_", LorF) ## Added for PWS
  SPS_Var <- paste0("Steady_Procedural_Speed_", LorF)
  Fin_Decel_Var <- paste0("Final_Deceleration_", LorF)
  End_Init_Decel_Var <- paste0("End_Initial_Deceleration_Distance_", LorF)
  IPS_Var <- paste0("Initial_Procedural_Speed_", LorF)
  Init_Decel_Var <- paste0("Initial_Deceleration_", LorF)
  
  ## Temporary Fix for Discrepancy in Init_Decel_Var Names across Files ---------------------------- #
  if (Level == "DBS" & LorF == "Lead"){Init_Decel_Var <- "Initial_Deceleration_Lead"}
  if (Level == "DBS" & LorF == "Follower"){Init_Decel_Var <- "Initial_Deceleration_Follower"}
  if (Level == "Wake" & LorF == "Lead"){Init_Decel_Var <- "Initial_deceleration_Lead"}
  if (Level == "Wake" & LorF == "Follower"){Init_Decel_Var <- "Initial_deceleration_Follower"}
  if (Level == "Aircraft" & LorF == "Lead"){Init_Decel_Var <- "Initial_deceleration_Lead"}
  if (Level == "Aircraft" & LorF == "Follower"){Init_Decel_Var <- "Initial_deceleration_follower"}
  # -------------------------------------------------------------------------------------------------- #
  
  # TEMP: Harcoded Operation Choice. Update ASAP
  Operation <- "IA"
  
  # Select Appropriate Variables
  if (Operation == "IA"){
    Ref_Data1 <- select(Ref_Data,
                       !!sym(Unique_Var),
                       "Compression_Commencement_Threshold",
                       "Landing_Stabilisation_Speed_Type" := !!sym(LSS_Type_Var),
                       "VRef" := !!sym(VRef_Var),
                       "Apply_Gusting" := !!sym(Gusting_Var),
                       "Local_Stabilisation_Distance" := !!sym(LSD_Var),
                       "Steady_Procedural_Speed" := !!sym(SPS_Var),
                       "Final_Deceleration" := !!sym(Fin_Decel_Var),
                       "End_Initial_Deceleration_Distance" := !!sym(End_Init_Decel_Var),
                       "Initial_Procedural_Speed" := !!sym(IPS_Var),
                       "Initial_Deceleration" := !!sym(Init_Decel_Var))
  }
  
  if (Operation == "IA PWS" & Level != "Operator"){
    Ref_Data1 <- select(Ref_Data,
                       !!sym(Unique_Var),
                       "Compression_Commencement_Threshold",
                       "Landing_Stabilisation_Speed_Type" := !!sym(LSS_Type_Var),
                       "VRef" := !!sym(VRef_Var),
                       "Apply_Gusting" := !!sym(Gusting_Var),
                       "Local_Stabilisation_Distance" := !!sym(LSD_Var),
                       "End_Final_Deceleration_Distance" := !!sym(End_Fin_Decel_Var),
                       "Steady_Procedural_Speed" := !!sym(SPS_Var),
                       "Final_Deceleration" := !!sym(Fin_Decel_Var),
                       "End_Initial_Deceleration_Distance" := !!sym(End_Init_Decel_Var),
                       "Initial_Procedural_Speed" := !!sym(IPS_Var),
                       "Initial_Deceleration" := !!sym(Init_Decel_Var))
    
  }
  
  if (Operation == "IA PWS" & Level == "Operator"){
    Ref_Data1 <- select(Ref_Data,
                       !!sym(Unique_Var),
                       !!sym(Unique_Var2),
                       "Compression_Commencement_Threshold",
                       "Landing_Stabilisation_Speed_Type" := !!sym(LSS_Type_Var),
                       "VRef" := !!sym(VRef_Var),
                       "Apply_Gusting" := !!sym(Gusting_Var),
                       "Local_Stabilisation_Distance" := !!sym(LSD_Var),
                       "End_Final_Deceleration_Distance" := !!sym(End_Fin_Decel_Var),
                       "Steady_Procedural_Speed" := !!sym(SPS_Var),
                       "Final_Deceleration" := !!sym(Fin_Decel_Var),
                       "End_Initial_Deceleration_Distance" := !!sym(End_Init_Decel_Var),
                       "Initial_Procedural_Speed" := !!sym(IPS_Var),
                       "Initial_Deceleration" := !!sym(Init_Decel_Var))
    
  }
  
  
  return(Ref_Data1)

}

Build_Aircraft_Profile <- function(Landing_Pair, LPID_Var, LorF, ORD_Profile_Selection, ORD_Operator, ORD_Aircraft, ORD_Wake, ORD_DBS, ORD_Runway){
  
  # Get Variable Names
  ID_Var <- LPID_Var
  FPID <- paste0(LorF, "_Flight_Plan_ID") # Not Currently Used - Need to Add to Verification
  AC_Type_Var <- paste0(LorF, "_Aircraft_Type")
  Operator_Var <- paste0(LorF, "_Operator") ## Added for PWS
  Wake_Var <- paste0(LorF, "_Recat_Wake_Cat")
  RW_Var <- paste0(LorF, "_Landing_Runway")
  TPR_Var <- substr(LorF, 1, 1)
  SW_Var <- "Forecast_AGI_Surface_Headwind"
  if(LPID_Var == "ORD_Tool_Calculation_ID"){SW_Var <- paste0(LorF, "_", SW_Var)}
  
  # TEMP: Hardcoded Operation
  Operation <- "IA"
  Operation <- ifelse(Operation == "IA PWS" & ORD_Profile_Selection == "Operator", "IA PWS 2", Operation)
  
  # Select Relevant Variables
  if (Operation %in% c("IA", "IA PWS")){
    Aircraft_Profile <- select(Landing_Pair,
                               !!sym(ID_Var),
                               "Aircraft_Type" := !!sym(AC_Type_Var),
                               "Wake_Cat" := !!sym(Wake_Var),
                               "DBS_All_Sep_Distance",
                               "Surface_Headwind" := !!sym(SW_Var),
                               "Landing_Runway" := !!sym(RW_Var))
  }
  
  if (Operation == "IA PWS 2"){
    Aircraft_Profile <- select(Landing_Pair,
                               !!sym(ID_Var),
                               "Aircraft_Type" := !!sym(AC_Type_Var),
                               "Operator" := !!sym(Operator_Var),
                               "Wake_Cat" := !!sym(Wake_Var),
                               "DBS_All_Sep_Distance",
                               "Surface_Headwind" := !!sym(SW_Var),
                               "Landing_Runway" := !!sym(RW_Var))
  }
  
  # Add "This_Pair_Role" Column
  Aircraft_Profile <- mutate(Aircraft_Profile, This_Pair_Role = TPR_Var)
  
  # Join on the ORD Adaptation using Join_ORD_Adaptation
  Aircraft_Profile <- Join_ORD_Adaptation(Aircraft_Profile, ORD_Profile_Selection, ORD_Operator, ORD_Aircraft, ORD_Wake, ORD_DBS)
  
  # Get a Reduced ORD Runway Table
  ORD_Runway_Reduced <- select(ORD_Runway, Runway_Name, Thousand_Ft_Gate, Gust_Adjustment)
  
  # Join on Reduced Runway Table
  Aircraft_Profile <- left_join(Aircraft_Profile, ORD_Runway_Reduced, by = c("Landing_Runway" = "Runway_Name"))
  
  # if doing IA (Not PWS), Set End_Final_Deceleration_Distance to Thousand_Ft_Gate
  if (Operation == "IA"){Aircraft_Profile <- mutate(Aircraft_Profile, End_Final_Deceleration_Distance = Thousand_Ft_Gate)}
  
  # Update Gust Adjustment Value based on Apply Gusting: Set former to 0 if latter is 0
  Aircraft_Profile <- mutate(Aircraft_Profile, Gust_Adjustment = ifelse(Apply_Gusting == 1, Gust_Adjustment, 0))
                             
  return(Aircraft_Profile)                          
  
}

Split_Leader_Follower <- function(Data, LorF){
  LorF <- substr(LorF, 1, 1)
  Data <- filter(Data, This_Pair_Role == LorF)
  return(Data)
}

Get_ORD_Runway_Parameters <- function(Aircraft_Profile, Landing_Pair, LPID_Var, LorFRunway){
  
  # ORD Runway Adaptation
  ORD_Runway <- Load_Adaptation_Table(con, "tbl_ORD_Runway_Adaptation")
  
  # Get Variables
  RW_Var <- paste0(LorFRunway, "_Landing_Runway")
  
  # Get the Aircraft Runways from Landing Pair
  Aircraft_Runways <- select(Landing_Pair, !!sym(LPID_Var), !!sym(RW_Var)) %>% 
    rename("Runway" = !!sym(RW_Var))
  
  # Join on ORD Runway PArameters. Thousand Ft Gate already present from ORD Aircraft Profile
  ORD_Runway_Reduced <- select(ORD_Runway, Runway_Name, Six_Hundred_Ft_AAL, Four_Hundred_Ft_AAL, Max_DTT)
  
  # Join on relevant Runways. Keep Leader runway on both for now. (But follower should have follower in future)
  Aircraft_Profile <- left_join(Aircraft_Profile, Aircraft_Runways, by = setNames(LPID_Var, LPID_Var))
  
  # Join on ORD Runway Data
  Aircraft_Profile <- left_join(Aircraft_Profile, ORD_Runway_Reduced, by = c("Runway" = "Runway_Name"))
  
  # Get the Adjusted Final Deceleration/IAS Values for Airbus Aircraft
  Aircraft_Profile <- mutate(Aircraft_Profile,
                             Adjusted_Final_Decel = (Landing_Stabilisation_Speed - Steady_Procedural_Speed) / (End_Final_Deceleration_Distance - Start_Final_Deceleration_Distance),
                             IAS_3DME = Adjusted_Final_Decel * ((3 * NM_to_m) - Start_Final_Deceleration_Distance) + Steady_Procedural_Speed,
                             IAS_4DME = Adjusted_Final_Decel * ((4 * NM_to_m) - Start_Final_Deceleration_Distance) + Steady_Procedural_Speed)
  
  return(Aircraft_Profile)
  
}

Flag_PLT_Go_Arounds <- function(Landing_Pair, Radar, Path_Legs){
  
  # Make reduced path leg table
  Path_Leg_Types <- select(Path_Legs, Path_Leg_Name, Path_Leg_Type)
  
  # Join on Path Leg Type from tbl_Path_Leg
  Radar1 <- left_join(Radar, Path_Leg_Types, by=c("Path_Leg" = "Path_Leg_Name"))
  
  # Make flag for Go-Around
  Radar1 <- mutate(Radar1, Is_Go_Around = ifelse(Path_Leg_Type == "Go_Around", 1, 0))
  
  # Find the number of Go-Around path legs
  Radar1 <- group_by(Radar1, Flight_Plan_ID) %>% summarise(Go_Around_Count = sum(Is_Go_Around, na.rm=T)) %>% ungroup()
  
  # Create flag for those with >=1 Go_Around path Leg
  Radar1 <- mutate(Radar1, Goes_Around = ifelse(Go_Around_Count >= 1, 1, 0))
  
  # Reduce Datasets: Get Go-Around Flag to Join
  Radar1 <- select(Radar1, Flight_Plan_ID, Goes_Around)
  
  # Join onto Landing Pair and Rename: Leader
  Landing_Pair <- left_join(Landing_Pair, Radar1, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
    rename(Goes_Around_Leader = Goes_Around)
  
  # Join onto Landing Pair and Rename: Follower
  Landing_Pair <- left_join(Landing_Pair, Radar1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
    rename(Goes_Around_Follower = Goes_Around)
  
  # Get Final Pair Go-Around Flag
  Landing_Pair <- mutate(Landing_Pair, Go_Around_Flag = ifelse(Goes_Around_Leader + Goes_Around_Follower >= 1, 1, 0))
  
  # Remove intermediary variables
  Landing_Pair <- select(Landing_Pair, -c("Goes_Around_Leader", "Goes_Around_Follower"))
  
  return(Landing_Pair)
} 

Create_Filter_Flag_Reference <- function(Landing_Pair, Radar, Path_Legs){
  
  # Get Go-Around Flag
  Landing_Pair <- Flag_PLT_Go_Arounds(Landing_Pair, Radar, Path_Legs)
  
  # Create New Reference Flag
  Landing_Pair <- mutate(Landing_Pair, Reference_Flag = Go_Around_Flag)
  
  # Add to Reference Flag if Missing Aircraft Types
  Landing_Pair <- mutate(Landing_Pair, Reference_Flag = ifelse(is.na(Leader_Aircraft_Type) | is.na(Follower_Aircraft_Type), 1, Reference_Flag))
  
  return(Landing_Pair)
  
}

Create_Filter_Flag_Observation <- function(Landing_Pair, ORD_Aircraft, ORD_Profile_Selection, Wake_Pairs_Only, In_Trail_Only){
    
  # Initialise Observation Flag equal to Reference Flag - All Criteria is included in Observation
  Landing_Pair <- mutate(Landing_Pair, Observation_Flag = Reference_Flag)
    
  # If Aircraft Type Only Validation: Only consider pairs where at least one Aircraft Type has specific parameters.
  if (ORD_Profile_Selection == "Aircraft_Type"){
    Landing_Pair <- mutate(Landing_Pair, Observation_Flag = ifelse(
      Leader_Aircraft_Type %!in% ORD_Aircraft$Aircraft_Type &
        Follower_Aircraft_Type %!in% ORD_Aircraft$Aircraft_Type,
      1, Observation_Flag))}
  
  # Add to Flag if only Wake Pairs being Considered - Note this actually includes ROT Pairs too
  if (Wake_Pairs_Only){Landing_Pair <- mutate(Landing_Pair, Observation_Flag = 
                                                ifelse(is.na(Reference_Recat_Wake_Separation_Distance) & 
                                                         is.na(Reference_Recat_ROT_Spacing_Distance), 1, Observation_Flag))}
  
  # Add to Flag if only In Trail Pairs being Considered
  if (In_Trail_Only){Landing_Pair <- mutate(Landing_Pair, Observation_Flag = ifelse(Landing_Pair_Type == "Not_In_Trail", 1, Observation_Flag))}
  
  return(Landing_Pair)
    
} 

Create_Filter_Flag_Prediction <- function(Landing_Pair, ORDorWAD){
  
  # Get Variable Names
  Flag_Var <- paste0(ORDorWAD, "_Prediction_Flag")
  Comp_Var <- paste0("Observed_", ORDorWAD, "_Compression")
  
  # Initialise Prediction Flag equal to Observation Flag as Prediction only Uses Observation Values
  Landing_Pair <- mutate(Landing_Pair, !!sym(Flag_Var) := Observation_Flag)
  
  # Add to Flag if No Observed Compression Values.
  Landing_Pair <- mutate(Landing_Pair, !!sym(Flag_Var) := ifelse(is.na(!!sym(Comp_Var)), 1, !!sym(Flag_Var)))
  
  return(Landing_Pair)
                         
}

Create_Filter_Flag_Performance <- function(Landing_Pair, Wake_Pairs_Only, In_Trail_Only){
  
  # Initialise Performance Flag equal to Reference Flag - All Criteria is included in Performance Model
  Landing_Pair <- mutate(Landing_Pair, Performance_Flag = Reference_Flag)
  
  # Add to Flag if only Wake Pairs being Considered - Note this actually includes ROT Pairs too
  if (Wake_Pairs_Only){Landing_Pair <- mutate(Landing_Pair, Performance_Flag = 
                                                ifelse(is.na(Reference_Recat_Wake_Separation_Distance) & 
                                                         is.na(Reference_Recat_ROT_Spacing_Distance), 1, Performance_Flag))}
  
  # Add to Flag if only In Trail Pairs being Considered
  if (In_Trail_Only){Landing_Pair <- mutate(Landing_Pair, Performance_Flag = ifelse(Landing_Pair_Type == "Not_In_Trail", 1, Performance_Flag))}
  
  return(Landing_Pair)
  
}

# ----------------------------------------------- #
# Landing Stabilisation Speed Calculations
# ----------------------------------------------- #
# Functions for calculating LSS for Different Types.
# There is a function for each different calculation,
# as well as a function for each type that uses one of 
# these calculations. There is a final function wrapping
# these all up, which is used in ORD Aircraft Profile.
# NOTE: Requires Forecast Surface Headwind.
# ----------------------------------------------- #

# Variant A of the Landing Stabilisation Speed Calculation. Currently used for types 0, 10, 11, 12
Calculate_LSS_A <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Landing_Stabilisation_Speed = ifelse((Surface_Headwind + Gust_Adjustment)/2 > (20*kts_To_mps),
                                                                      VRef + 20*kts_To_mps,
                                                                      ifelse((Surface_Headwind + Gust_Adjustment)/2 < (5*kts_To_mps),
                                                                             VRef + 5*kts_To_mps,
                                                                             VRef + (Surface_Headwind + Gust_Adjustment)/2)))
  return(ORD_Aircraft_Profile)
}

# Variant B of the Landing Stabilisation Speed Calculation. Currently used for types 1, 2, 3, 4
Calculate_LSS_B <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Landing_Stabilisation_Speed = ifelse((Surface_Headwind)/3 > (15*kts_To_mps),
                                                                      VRef + 15*kts_To_mps,
                                                                      ifelse((Surface_Headwind)/3 < (5*kts_To_mps),
                                                                             VRef + 5*kts_To_mps,
                                                                             VRef + (Surface_Headwind)/3)))
  return(ORD_Aircraft_Profile)
}

# Variant C of the Landing Stabilisation Speed Calculation. Currently used for type 5
Calculate_LSS_C <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Landing_Stabilisation_Speed = ifelse((Surface_Headwind + Gust_Adjustment)/3 > (15*kts_To_mps),
                                                                      VRef + 15*kts_To_mps,
                                                                      ifelse((Surface_Headwind + Gust_Adjustment)/3 < (5*kts_To_mps),
                                                                             VRef + 5*kts_To_mps,
                                                                             VRef + (Surface_Headwind + Gust_Adjustment)/3)))
  return(ORD_Aircraft_Profile)
}

# Variant D of the Landing Stabilisation Speed Calculation. Currently used for type 6
Calculate_LSS_D <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Landing_Stabilisation_Speed = ifelse((Surface_Headwind + Gust_Adjustment)/2 > (20*kts_To_mps),
                                                                      VRef + 20*kts_To_mps,
                                                                      ifelse((Surface_Headwind + Gust_Adjustment)/2 < 0,
                                                                             VRef,
                                                                             VRef + (Surface_Headwind + Gust_Adjustment)/2)))
  return(ORD_Aircraft_Profile)
}

# Variant E of the Landing Stabilisation Speed Calculation. Currently used for type 7
Calculate_LSS_E <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Landing_Stabilisation_Speed = ifelse((Gust_Adjustment)/2 > (20*kts_To_mps),
                                                                      VRef + 20*kts_To_mps,
                                                                      ifelse((Gust_Adjustment)/2 < (10*kts_To_mps),
                                                                             VRef + 10*kts_To_mps,
                                                                             VRef + (Gust_Adjustment)/2)))
  return(ORD_Aircraft_Profile)
}

# Variant F of the Landing Stabilisation Speed Calculation. Currently used for type 8
Calculate_LSS_F <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Landing_Stabilisation_Speed = ifelse((Gust_Adjustment)/2 > (20*kts_To_mps),
                                                                      VRef + 20*kts_To_mps,
                                                                      ifelse((Gust_Adjustment)/2 < 0,
                                                                             VRef,
                                                                             VRef + (Gust_Adjustment)/2)))
  return(ORD_Aircraft_Profile)
}

# Variant G of the Landing Stabilisation Speed Calculation. Currently used for type 9
Calculate_LSS_G <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Landing_Stabilisation_Speed = ifelse((Surface_Headwind + Gust_Adjustment) > (20*kts_To_mps),
                                                                      VRef + 15*kts_To_mps,
                                                                      ifelse((Surface_Headwind + Gust_Adjustment)/2 > (10*kts_To_mps),
                                                                             VRef + 10*kts_To_mps,
                                                                             VRef + 5*kts_To_mps)))
  return(ORD_Aircraft_Profile)
}

# ----------------------------------------------- #

# LSS For Type 0
Calculate_LSS_Type_0 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_A(ORD_Aircraft_Profile))
}

# LSS For Type 1
Calculate_LSS_Type_1 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_B(ORD_Aircraft_Profile))
}

# LSS For Type 2
Calculate_LSS_Type_2 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_B(ORD_Aircraft_Profile))
}

# LSS For Type 3
Calculate_LSS_Type_3 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_B(ORD_Aircraft_Profile))
}

# LSS For Type 4
Calculate_LSS_Type_4 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_B(ORD_Aircraft_Profile))
}

# LSS For Type 5
Calculate_LSS_Type_5 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_C(ORD_Aircraft_Profile))
}

# LSS For Type 6
Calculate_LSS_Type_6 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_D(ORD_Aircraft_Profile))
}

# LSS For Type 7
Calculate_LSS_Type_7 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_E(ORD_Aircraft_Profile))
}

# LSS For Type 8
Calculate_LSS_Type_8 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_F(ORD_Aircraft_Profile))
}

# LSS For Type 9
Calculate_LSS_Type_9 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_G(ORD_Aircraft_Profile))
}

# LSS For Type 10
Calculate_LSS_Type_10 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_A(ORD_Aircraft_Profile))
}

# LSS For Type 11
Calculate_LSS_Type_11 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_A(ORD_Aircraft_Profile))
}

# LSS For Type 12
Calculate_LSS_Type_12 <- function(ORD_Aircraft_Profile){
  return(Calculate_LSS_A(ORD_Aircraft_Profile))
}

# ----------------------------------------------- #

# General Landing Stabilisation Speed Function. 
Calculate_Landing_Stabilisation_Speed <- function(ORD_Aircraft_Profile, LPID_Var){
  for (i in 0:12){
    Type_Profiles <- filter(ORD_Aircraft_Profile, Landing_Stabilisation_Speed_Type == i)
    Type_Profiles <- eval(parse(text = paste0("Calculate_LSS_Type_", i, "(Type_Profiles)")))
    if (i == 0){Full_Profile <- Type_Profiles} else {Full_Profile <- rbind(Full_Profile, Type_Profiles)}
  }
  Full_Profile <- arrange(Full_Profile, !!sym(LPID_Var))
  return(Full_Profile)
}

# ----------------------------------------------- #
# Deceleration Distance Calculations
# ----------------------------------------------- #
# Functions for calculating The Final Deceleration
# Distance as well as the Start Initial Deceleration
# Distance. Requires Thousand Ft.Gate.
# ----------------------------------------------- #

# Calculate Start Initial Decel Distance.
Calculate_Start_Initial_Decel_Distance <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Start_Initial_Deceleration_Distance = End_Initial_Deceleration_Distance + (Initial_Procedural_Speed - Steady_Procedural_Speed) / Initial_Deceleration)
  return(ORD_Aircraft_Profile)
}

# Calculate Start Final Deceleration Distance.
Calculate_Final_Decel_Distance <- function(ORD_Aircraft_Profile){
  
    ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                   Start_Final_Deceleration_Distance = ifelse(Local_Stabilisation_Distance > (End_Final_Deceleration_Distance + (Steady_Procedural_Speed - Landing_Stabilisation_Speed) / Final_Deceleration),
                                                                              Local_Stabilisation_Distance,
                                                                              End_Final_Deceleration_Distance + (Steady_Procedural_Speed - Landing_Stabilisation_Speed) / Final_Deceleration))
  
  return(ORD_Aircraft_Profile)
}


# -- Generic

# Generic Parameter Selection Function.
Select_IAS_Profile_Fields <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- select(Aircraft_Profile, 
                             !!sym(LPID_Var),
                             This_Pair_Role,
                             Section_Number,
                             Profile_Section,
                             Profile_Type,
                             Start_IAS,
                             End_IAS,
                             Start_Dist,
                             End_Dist)
  return(Aircraft_Profile)
}

# Build Full IAS Profile
Build_Full_IAS_Profile <- function(Aircraft_Profile, LPID_Var){
  for (i in 0:12){
    Type_Profiles <- eval(parse(text = paste0("Build_IAS_Profile_Type_", i, "(Aircraft_Profile, LPID_Var)")))
    if (i == 0){Full_Profile <- Type_Profiles} else {Full_Profile <- rbind(Full_Profile, Type_Profiles)}
  }
  Full_Profile <- arrange(Full_Profile, !!sym(LPID_Var))
  return(Full_Profile)
}

# ----------------------------------------------- #
# Gust Adjustments
# ----------------------------------------------- #

# Gust Adjustment calculations for type 1 and type 2.
Generate_Gust_Adjustments_A <- function(Aircraft_Profile){
  Aircraft_Profile <- mutate(Aircraft_Profile,
                             Gust_Adjustment_1a = ifelse((-Wind_Effect_0DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_0DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_1b = ifelse((-Wind_Effect_1DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_1DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_1c = ifelse((-Wind_Effect_1DME - Surface_Headwind) > 0, (-Wind_Effect_1DME - Surface_Headwind), 0),
                             Gust_Adjustment_1d = ifelse((-Wind_Effect_2DME - Surface_Headwind) > 0, (-Wind_Effect_2DME - Surface_Headwind), 0),
                             Gust_Adjustment_2a = ifelse((-Wind_Effect_2DME - Surface_Headwind) > 0, (-Wind_Effect_2DME - Surface_Headwind), 0),
                             Gust_Adjustment_2b = ifelse((-Wind_Effect_3DME - Surface_Headwind) > 0, (-Wind_Effect_3DME - Surface_Headwind), 0),
                             Gust_Adjustment_2c = ifelse((-Wind_Effect_4DME - Surface_Headwind) > 0, (-Wind_Effect_4DME - Surface_Headwind), 0),
                             Gust_Adjustment_1a = ifelse(is.na(Gust_Adjustment_1a), 0, Gust_Adjustment_1a),
                             Gust_Adjustment_1b = ifelse(is.na(Gust_Adjustment_1b), 0, Gust_Adjustment_1b),
                             Gust_Adjustment_1c = ifelse(is.na(Gust_Adjustment_1c), 0, Gust_Adjustment_1c),
                             Gust_Adjustment_1d = ifelse(is.na(Gust_Adjustment_1d), 0, Gust_Adjustment_1d),
                             Gust_Adjustment_2a = ifelse(is.na(Gust_Adjustment_2a), 0, Gust_Adjustment_2a),
                             Gust_Adjustment_2b = ifelse(is.na(Gust_Adjustment_2b), 0, Gust_Adjustment_2b),
                             Gust_Adjustment_2c = ifelse(is.na(Gust_Adjustment_2c), 0, Gust_Adjustment_2c))
  
  
  return(Aircraft_Profile)
}

# Gust adjustment aclculations for type 3
Generate_Gust_Adjustments_B <- function(Aircraft_Profile){
  Aircraft_Profile <- mutate(Aircraft_Profile,
                             Gust_Adjustment_1a = ifelse((-Wind_Effect_0DME - Surface_Headwind) > 0, (-Wind_Effect_0DME - Surface_Headwind), 0),
                             Gust_Adjustment_1b = ifelse((-Wind_Effect_1DME - Surface_Headwind) > 0, (-Wind_Effect_1DME - Surface_Headwind), 0),
                             Gust_Adjustment_1c = ifelse((-Wind_Effect_2DME - Surface_Headwind) > 0, (-Wind_Effect_2DME - Surface_Headwind), 0),
                             Gust_Adjustment_2a = ifelse((-Wind_Effect_2DME - Surface_Headwind) > 0, (-Wind_Effect_2DME - Surface_Headwind), 0),
                             Gust_Adjustment_2b = ifelse((-Wind_Effect_3DME - Surface_Headwind) > 0, (-Wind_Effect_3DME - Surface_Headwind), 0),
                             Gust_Adjustment_2c = ifelse((-Wind_Effect_4DME - Surface_Headwind) > 0, (-Wind_Effect_4DME - Surface_Headwind), 0),
                             Gust_Adjustment_1a = ifelse(is.na(Gust_Adjustment_1a), 0, Gust_Adjustment_1a),
                             Gust_Adjustment_1b = ifelse(is.na(Gust_Adjustment_1b), 0, Gust_Adjustment_1b),
                             Gust_Adjustment_1c = ifelse(is.na(Gust_Adjustment_1c), 0, Gust_Adjustment_1c),
                             Gust_Adjustment_2a = ifelse(is.na(Gust_Adjustment_2a), 0, Gust_Adjustment_2a),
                             Gust_Adjustment_2b = ifelse(is.na(Gust_Adjustment_2b), 0, Gust_Adjustment_2b),
                             Gust_Adjustment_2c = ifelse(is.na(Gust_Adjustment_2c), 0, Gust_Adjustment_2c))
  
}

# Gust adjustment calculations for type 4
Generate_Gust_Adjustments_C <- function(Aircraft_Profile){
  Aircraft_Profile <- mutate(Aircraft_Profile,
                             Gust_Adjustment_1a = ifelse((-Wind_Effect_0DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_0DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_1b = ifelse((-Wind_Effect_1DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_1DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_1c = ifelse((-Wind_Effect_2DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_2DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_2a = ifelse((-Wind_Effect_2DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_2DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_2b = ifelse((-Wind_Effect_3DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_3DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_2c = ifelse((-Wind_Effect_4DME - Surface_Headwind) / 3 > 0, (-Wind_Effect_4DME - Surface_Headwind) / 3, 0),
                             Gust_Adjustment_1a = ifelse(is.na(Gust_Adjustment_1a), 0, Gust_Adjustment_1a),
                             Gust_Adjustment_1b = ifelse(is.na(Gust_Adjustment_1b), 0, Gust_Adjustment_1b),
                             Gust_Adjustment_1c = ifelse(is.na(Gust_Adjustment_1c), 0, Gust_Adjustment_1c),
                             Gust_Adjustment_2a = ifelse(is.na(Gust_Adjustment_2a), 0, Gust_Adjustment_2a),
                             Gust_Adjustment_2b = ifelse(is.na(Gust_Adjustment_2b), 0, Gust_Adjustment_2b),
                             Gust_Adjustment_2c = ifelse(is.na(Gust_Adjustment_2c), 0, Gust_Adjustment_2c))
  return(Aircraft_Profile)
}

# Segment Data Preparation. This converts ORD Validation, GWCS Validation and GWCS Verification data into
# common format for universal processing. ORD Verification does not require this algorithm.
Prepare_Segment_Data <- function(Type, Data, Seg_Size, Forecast_Seg_Max, Lookahead_Time){
  
  # ORD Validation. Uses Landing Pair Reference Table - IDs are Landing Pair IDs. Uses Leader Landing Runway.
  if (Type == "ORD"){
    Segment_Forecast <- select(Data, c("Landing_Pair_ID", "Landing_Pair_Date", "Leader_Landing_Runway", "Runway_Group", "Prediction_Time"))
    Segment_Forecast <- rename(Segment_Forecast, c("ID" = "Landing_Pair_ID", "Forecast_Time" = "Prediction_Time",
                                                   "Runway" = "Leader_Landing_Runway", "Date" = "Landing_Pair_Date"))
    
  }
  
  # GWCS Validation. Uses Flight Plan/Derived. IDs are Flight Plan IDs and Forecast Times are Time at 4DME - Lookahead Time.
  # Requires Runway Group to be joined on beforehand.
  if (Type == "GWCS Validation"){
    Segment_Forecast <- select(Data, c("Flight_Plan_ID", "FP_Date", "Landing_Runway", "Runway_Group", "Time_At_4DME"))
    Segment_Forecast <- rename(Segment_Forecast, c("ID" = "Flight_Plan_ID", "Forecast_Time" = "Time_At_4DME",
                                                   "Runway" = "Landing_Runway", "Date" = "FP_Date"))
  }
  
  # GWCS Verification. Leave alone for now.
  if (Type == "GWCS Verification"){}
  
  # Adjust the Forecast Times by subtracting the Lookahead Time. (For ORD this will be 0)
  Segment_Forecast <- mutate(Segment_Forecast, Forecast_Time = Forecast_Time - Lookahead_Time)
  
  # Make a Data Frame for all segments up to Forecast_Seg_Max. (Incorporate Seg Size here?)
  Total_Segments <- data.frame(DME_Seg = 0:(Forecast_Seg_Max / NM_to_m))
  
  # Merge the two Dataframes Together.
  Segment_Forecast <- merge(Segment_Forecast, Total_Segments)
  
  # Convert the DME Segments to SI units.
  Segment_Forecast <- mutate(Segment_Forecast, DME_Seg = DME_Seg * NM_to_m)
  
  return(Segment_Forecast)
  
}


Get_Non_Stale_Segments <- function(Segment_Forecast, Forecast_Seg, Stale_Time){
  
  # -- Add duplicates of the join parameters to not lose anything. 
  
  # For our Segment Forecast Table. Add a minor offset as to match segments correctly.
  Segment_Forecast <- mutate(Segment_Forecast,
                             Join_Date = Date,
                             Join_Runway = Runway_Group,
                             Join_Time = Forecast_Time + 0.00001)
  
  # For tbl_Mode_S_Wind_Seg_Forecast.
  Forecast_Seg <- mutate(Forecast_Seg,
                         Join_Date = Forecast_Date,
                         Join_Runway = Runway_Group,
                         Join_Time = Forecast_Time)
  
  # -- Perform a Rolling Join
  
  # Convert data to data.tables to allow rolling joins.
  Segment_Forecast <- as.data.table(Segment_Forecast)
  Forecast_Seg <- as.data.table(Forecast_Seg)
  
  # Set Keys for Rolling Join. Join_Time will be the roll.
  setkey(Segment_Forecast, Join_Date, Join_Runway, DME_Seg, Join_Time)
  setkey(Forecast_Seg, Join_Date, Join_Runway, DME_Seg, Join_Time)
  
  # Perform the Rolling Join, with the max roll being the Stale Time. 
  Segment_Forecast <- Forecast_Seg[Segment_Forecast, roll = Stale_Time]
  
  # Return to data.frame format and select relevant fields.
  Segment_Forecast <- as.data.frame(Segment_Forecast) %>% select(ID, Date, Join_Runway, DME_Seg, Join_Time, Segment_Time = Forecast_Time,
                                                                 Forecast_Wind_Effect_IAS, Forecast_Wind_SPD, Forecast_Wind_HDG,
                                                                 Forecast_Aircraft_Type) %>% rename(Runway_Group = Join_Runway,
                                                                                                    Forecast_Time = Join_Time)
  # Order data by ID and DME_Seg.
  Segment_Forecast <- Segment_Forecast[order(Segment_Forecast$ID, Segment_Forecast$DME_Seg),]
  
  return(Segment_Forecast)
}

# Function for Transferring Extrpolated Data to updated segment table. Extrapolation_Type = ("ORD", "TBS")
Transfer_Extrapolated_Segment_Data <- function(Segment_Forecast, Extrapolation_Type){
  
  if (Extrapolation_Type == "TBS"){
    Segment_Forecast <- mutate(Segment_Forecast, 
                               TBS_Interpolated_Seg.x = ifelse(!is.na(Forecast_Wind_Effect_IAS.y), DME_Seg.y, TBS_Interpolated_Seg.x),
                               Forecast_Time.x = Forecast_Time.y,
                               Forecast_Wind_Effect_IAS.x = Forecast_Wind_Effect_IAS.y,
                               Forecast_Wind_SPD.x = Forecast_Wind_SPD.y,
                               Forecast_Wind_HDG.x = Forecast_Wind_HDG.y,
                               Forecast_Aircraft_Type.x = Forecast_Aircraft_Type.y) %>%
      select(ID, Date = Date.x, Runway_Group = Runway_Group.x, DME_Seg = DME_Seg.x, Segment_Time = Segment_Time.x,
             Forecast_Time = Forecast_Time.x, Forecast_Wind_Effect_IAS = Forecast_Wind_Effect_IAS.x, Forecast_Wind_SPD = Forecast_Wind_SPD.x,
             Forecast_Wind_HDG = Forecast_Wind_HDG.x, Forecast_Aircraft_Type = Forecast_Aircraft_Type.x, TBS_Interpolated_Seg = TBS_Interpolated_Seg.x)
  }
  
  if (Extrapolation_Type == "ORD"){
    Segment_Forecast <- mutate(Segment_Forecast, 
                               ORD_Extrapolated_Seg.x = ifelse(!is.na(Forecast_Wind_Effect_IAS.y), DME_Seg.y, ORD_Extrapolated_Seg.x),
                               TBS_Interpolated_Seg.x = ifelse(!is.na(Forecast_Wind_Effect_IAS.y), TBS_Interpolated_Seg.y, TBS_Interpolated_Seg.x),
                               Forecast_Time.x = Forecast_Time.y,
                               Forecast_Wind_Effect_IAS.x = Forecast_Wind_Effect_IAS.y,
                               Forecast_Wind_SPD.x = Forecast_Wind_SPD.y,
                               Forecast_Wind_HDG.x = Forecast_Wind_HDG.y,
                               Forecast_Aircraft_Type.x = Forecast_Aircraft_Type.y) %>%
      select(ID, Date = Date.x, Runway_Group = Runway_Group.x, DME_Seg = DME_Seg.x, Segment_Time = Segment_Time.x,
             Forecast_Time = Forecast_Time.x, Forecast_Wind_Effect_IAS = Forecast_Wind_Effect_IAS.x, Forecast_Wind_SPD = Forecast_Wind_SPD.x,
             Forecast_Wind_HDG = Forecast_Wind_HDG.x, Forecast_Aircraft_Type = Forecast_Aircraft_Type.x, TBS_Interpolated_Seg = TBS_Interpolated_Seg.x,
             ORD_Extrapolated_Seg = ORD_Extrapolated_Seg.x)
  }
  
  return(Segment_Forecast)
  
}


TBS_Extrapolate_Segments <- function(Segment_Forecast, Max_Seg_Extrapolation, Extrapolation_Seg_Min, Separation_Forecast_Seg_Max){
  
  # Make a variable for the origin of TBS Interpolated Segment
  Segment_Forecast <- mutate(Segment_Forecast, TBS_Interpolated_Seg = NA)
  
  # Keep the Segs that do not allow extrapolation separately
  Unextrapolated_Forecast <- filter(Segment_Forecast, DME_Seg < Extrapolation_Seg_Min)
  
  # TBS Segs are between Forecast Seg Min and Separation Forecast Seg Max. We Want these + maximum Seg Extrapolation range
  TBS_Segment_Forecast <- filter(Segment_Forecast, DME_Seg >= Extrapolation_Seg_Min & DME_Seg <= (Separation_Forecast_Seg_Max + (Max_Seg_Extrapolation * NM_to_m)))
  
  # Loop through each segment number. Should be updated to reflect segment size. 
  for (i in (Extrapolation_Seg_Min / NM_to_m) : (Separation_Forecast_Seg_Max / NM_to_m)){
    
    # Filter for DME Seg in loop, separate raw segments from ones to be interpolated
    TBS_Segments_This_Seg <- filter(TBS_Segment_Forecast, DME_Seg == i * NM_to_m)
    TBS_Segments_This_Seg_Valid <- filter(TBS_Segments_This_Seg, !is.na(Forecast_Wind_Effect_IAS))
    TBS_Segments_This_Seg_Null <- filter(TBS_Segments_This_Seg, is.na(Forecast_Wind_Effect_IAS))
    
    # Bind the raw segments for this DME to the complete processed segments
    if (i == (Extrapolation_Seg_Min / NM_to_m)){TBS_Segs_Complete <- TBS_Segments_This_Seg_Valid} else {
      TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Valid)}
    
    # Create new loop for extrapolation search. Go outward from 1 to Max_Seg_Extrapolation in either direction
    for (j in 1 : Max_Seg_Extrapolation){
      
      # -- Prioritise lower RTT values, but only do if segment within allowed interpolating range.
      if ((i - j) * NM_to_m >= Extrapolation_Seg_Min){
        
        # Filter for the Segment (i - j). e.g. For Segment 5, will start at Segment 4. 
        TBS_Segments_Extrapolate <- filter(TBS_Segment_Forecast, DME_Seg == (i - j) * NM_to_m)
        
        # Join the Invalid segments to attempt to extrapolate with this segment
        TBS_Segments_Compare <- inner_join(TBS_Segments_This_Seg_Null, TBS_Segments_Extrapolate, by = c("ID")) # and all extra manipulation
        
        # Perform the TBS Extrapolation data transfer.
        TBS_Segments_Compare <- Transfer_Extrapolated_Segment_Data(TBS_Segments_Compare, "TBS")
        
        # Isolate remaining NULL segments.
        TBS_Segments_This_Seg_Null <- filter(TBS_Segments_Compare, is.na(Forecast_Wind_Effect_IAS))
        
        # Isolate successfully extrapolated segments
        TBS_Segments_This_Seg_Valid <- filter(TBS_Segments_Compare, !is.na(Forecast_Wind_Effect_IAS))
        
        # Add extrapolated segments to complete segment list.
        TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Valid)
      }
      
      # -- Then do higher values
      
      # Filter for segment (i + j). 
      TBS_Segments_Extrapolate <- filter(TBS_Segment_Forecast, DME_Seg == (i + j) * NM_to_m)
      
      # Join the Invalid segments to attempt to extrapolate with this segment
      TBS_Segments_Compare <- inner_join(TBS_Segments_This_Seg_Null, TBS_Segments_Extrapolate, by = c("ID"))
      
      # Perform the TBS Extrapolation data transfer.
      TBS_Segments_Compare <- Transfer_Extrapolated_Segment_Data(TBS_Segments_Compare, "TBS")
      
      # Isolate remaining NULL segments.
      TBS_Segments_This_Seg_Null <- filter(TBS_Segments_Compare, is.na(Forecast_Wind_Effect_IAS))
      
      # Isolate successfully extrapolated segments
      TBS_Segments_This_Seg_Valid <- filter(TBS_Segments_Compare, !is.na(Forecast_Wind_Effect_IAS))
      
      # Add extrapolated segments to complete segment list.
      TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Valid)
      
      # If final attempt at extrapolation for ths segment, bind on the remaining NULL forecasts.
      if (j == Max_Seg_Extrapolation){TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Null)}
      
    }
    
  }
  
  # Remove Old segments in the TBS Extrapolation range
  Segment_Forecast <- filter(Segment_Forecast, DME_Seg > Separation_Forecast_Seg_Max)
  
  # Add on thenew TBS Extrapolated Segments, as well as the Untouched segments
  Segment_Forecast <- rbind(Segment_Forecast, Unextrapolated_Forecast) %>% rbind(TBS_Segs_Complete)
  
  # Order Segments as before
  Segment_Forecast <- Segment_Forecast[order(Segment_Forecast$ID, Segment_Forecast$DME_Seg),]
  
  return(Segment_Forecast)
}



ORD_Extrapolate_Segments <- function(Segment_Forecast, Forecast_Seg_Max, Separation_Forecast_Seg_Max){
  
  # Add a new parameter to signify which segment was used for ORD Extrapolation.
  Segment_Forecast <- mutate(Segment_Forecast, ORD_Extrapolated_Seg = NA)
  
  # ORD Segs are between Separation Forecast Seg Max - Extrap range and Forecast_Valid_Seg_Max.
  ORD_Segs <- filter(Segment_Forecast, DME_Seg <= Forecast_Seg_Max & DME_Seg >= (Separation_Forecast_Seg_Max))
  
  # We want to loop through all segments, starting with Separation_Forecast_Seg_Max + 1 
  for (i in ((Separation_Forecast_Seg_Max / NM_to_m) + 1) : (Forecast_Seg_Max / NM_to_m)){
    
    # Select Data for this segment and the previous segment separately.
    ORD_Segs_This_Seg <- filter(ORD_Segs, DME_Seg == (i * NM_to_m))
    ORD_Segs_Prev_Seg <- filter(ORD_Segs, DME_Seg == ((i - 1) * NM_to_m))
    
    # Select Populated and Non-Populated data from This Segment separately.
    ORD_Segs_This_Seg_Valid <- filter(ORD_Segs_This_Seg, !is.na(Forecast_Wind_Effect_IAS))
    ORD_Segs_This_Seg_Null <- filter(ORD_Segs_This_Seg, is.na(Forecast_Wind_Effect_IAS))
    
    # Join on the Invalid This Segment Data to the Previous Segment Data.
    ORD_Segs_This_Seg_Compare <- inner_join(ORD_Segs_This_Seg_Null, ORD_Segs_Prev_Seg, by=c("ID"))
    
    # Perform the ORD Extrapolation data transfer.
    ORD_Segs_This_Seg_Compare <- Transfer_Extrapolated_Segment_Data(ORD_Segs_This_Seg_Compare, "ORD")
    
    # Bind the previously valid segs with those attempted to be extrapolated.
    ORD_Segs_This_Seg <- rbind(ORD_Segs_This_Seg_Valid, ORD_Segs_This_Seg_Compare)
    
    # Filter out this DME Segment from ORD_Segs.
    ORD_Segs <- filter(ORD_Segs, DME_Seg != (i * NM_to_m))
    
    # Add in the extrapolated data for this segment.
    ORD_Segs <- rbind(ORD_Segs, ORD_Segs_This_Seg)
  }
  
  # Remove the old ORD Segs
  Segment_Forecast <- filter(Segment_Forecast, DME_Seg < Separation_Forecast_Seg_Max)
  
  # Add new ORD Segs
  Segment_Forecast <- rbind(Segment_Forecast, ORD_Segs)
  
  # Order as before
  Segment_Forecast <- Segment_Forecast[order(Segment_Forecast$ID, Segment_Forecast$DME_Seg),]
  
  return(Segment_Forecast)
}


# Function that changes the Wind entries depending on GWCS Wind Selection. Requires Extrapolation Complete.
# !!! Do we need to change Wind Speed, HDG and Aircraft Type? See for GWCS.
Treat_Default_Wind_Segments <- function(Segment_Forecast, Default_Wind, GWCS_Wind_Selection){
  
  # Join on the Default Wind
  Segment_Forecast <- left_join(Segment_Forecast, Default_Wind, by = c("DME_Seg" = "Wind_Segment_Start"))
  
  # If GWCS_Wind_Selection is Always_Default, replace ALL Wind Effect values with Default
  if (GWCS_Wind_Selection == "Always_Default"){Segment_Forecast <- mutate(Segment_Forecast, Forecast_Wind_Effect_IAS = Wind_Effect)}
  
  # If GWCS_Wind_Selection is Always_Zero, replace ALL Wind Effect values with 0.
  if (GWCS_Wind_Selection == "Always_Zero"){Segment_Forecast <- mutate(Segment_Forecast, Forecast_Wind_Effect_IAS = 0)}
  
  # If GWCS_Wind_Selection is Auto_Default, replace NA Wind Effect values with Default. (After Extrapolation)
  if (GWCS_Wind_Selection == "Auto_Default"){
    Segment_Forecast <- mutate(Segment_Forecast, 
                               Forecast_Wind_Effect_IAS = ifelse(is.na(Forecast_Wind_Effect_IAS), Wind_Effect, Forecast_Wind_Effect_IAS))
  }
  
  # Remove Default Parameters.
  Segment_Forecast <- select(Segment_Forecast, -c("Wind_Effect", "Wind_Segment_End"))
  
  return(Segment_Forecast)
  
}  

# ----------------------------------------------- #
# Profile Addition
# ----------------------------------------------- #

# Generic Section 1 setting
Set_IAS_Profile_Section_1 <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed,
                             End_IAS = Landing_Stabilisation_Speed,
                             Start_Dist = End_Final_Deceleration_Distance,
                             End_Dist = 0)
  return(Aircraft_Profile)
}

# Generic Section 1a Setting
Set_IAS_Profile_Section_1a <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1a",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1a,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1a,
                             Start_Dist = 1 * NM_to_m,
                             End_Dist = 0)
  return(Aircraft_Profile)
}

# Section 1b Setting A - 1DME to 4HF AAL
Set_IAS_Profile_Section_1b_A <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1b",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             Start_Dist = Four_Hundred_Ft_AAL,
                             End_Dist = 1 * NM_to_m)
  return(Aircraft_Profile)
}

# section 1b setting B - 1DME to 6HF AAL
Set_IAS_Profile_Section_1b_B <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1b",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             Start_Dist = Six_Hundred_Ft_AAL,
                             End_Dist = 1 * NM_to_m)
  return(Aircraft_Profile)
}

# section 1b setting C - 1DME to 2DME
Set_IAS_Profile_Section_1b_C <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1b",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             Start_Dist = 2 * NM_to_m,
                             End_Dist = 1 * NM_to_m)
  return(Aircraft_Profile)
}

# Section 1c Setting A - 4HF AAL - 2DME
Set_IAS_Profile_Section_1c_A <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1c",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             Start_Dist = 2 * NM_to_m,
                             End_Dist = Four_Hundred_Ft_AAL)
}

# Section 1c Setting B - 6HF AAL - 2DME
Set_IAS_Profile_Section_1c_B <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1c",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             Start_Dist = 2 * NM_to_m,
                             End_Dist = Six_Hundred_Ft_AAL)
  return(Aircraft_Profile)
}

# Section 1c Setting C - 2DME - Thousand Ft Gate
Set_IAS_Profile_Section_1c_C <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1c",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             Start_Dist = End_Final_Deceleration_Distance,
                             End_Dist = 2 * NM_to_m)
  return(Aircraft_Profile)
}


# Section 1d (only present for types 1 and 2 but the same for both)
Set_IAS_Profile_Section_1d <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1d",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1d,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1d,
                             Start_Dist = End_Final_Deceleration_Distance,
                             End_Dist = 2 * NM_to_m)
  return(Aircraft_Profile)
}

# Generic Section 2 Setting
Set_IAS_Profile_Section_2 <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2",
                             Profile_Type = "A",
                             Start_IAS = Steady_Procedural_Speed,
                             End_IAS = Landing_Stabilisation_Speed,
                             Start_Dist = Start_Final_Deceleration_Distance,
                             End_Dist = End_Final_Deceleration_Distance)
  return(Aircraft_Profile)
}

# Section 2a
Set_IAS_Profile_Section_2a <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2a",
                             Profile_Type = "A",
                             Start_IAS = IAS_3DME + Gust_Adjustment_2a,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_2a,
                             Start_Dist = 3 * NM_to_m,
                             End_Dist = End_Final_Deceleration_Distance)
  return(Aircraft_Profile)
}

# Section 2b
Set_IAS_Profile_Section_2b <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2b",
                             Profile_Type = "A",
                             Start_IAS = IAS_4DME + Gust_Adjustment_2b,
                             End_IAS = IAS_3DME + Gust_Adjustment_2b,
                             Start_Dist = 4 * NM_to_m,
                             End_Dist = 3 * NM_to_m)
  return(Aircraft_Profile)
}

# Section 2c
Set_IAS_Profile_Section_2c <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2c",
                             Profile_Type = "A",
                             Start_IAS = Steady_Procedural_Speed + Gust_Adjustment_2c,
                             End_IAS = IAS_4DME + Gust_Adjustment_2c,
                             Start_Dist = Start_Final_Deceleration_Distance,
                             End_Dist = 4 * NM_to_m)
  return(Aircraft_Profile)
}

# Section 3
Set_IAS_Profile_Section_3 <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "3",
                             Profile_Type = "C",
                             Start_IAS = Steady_Procedural_Speed,
                             End_IAS = Steady_Procedural_Speed,
                             Start_Dist = End_Initial_Deceleration_Distance,
                             End_Dist = Start_Final_Deceleration_Distance)
  return(Aircraft_Profile)
}

# Section 4
Set_IAS_Profile_Section_4 <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "4",
                             Profile_Type = "A",
                             Start_IAS = Initial_Procedural_Speed,
                             End_IAS = Steady_Procedural_Speed,
                             Start_Dist = Start_Initial_Deceleration_Distance,
                             End_Dist = End_Initial_Deceleration_Distance)
  return(Aircraft_Profile)
}

# Section 5
Set_IAS_Profile_Section_5 <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "5",
                             Profile_Type = "C",
                             Start_IAS = Initial_Procedural_Speed,
                             End_IAS = Initial_Procedural_Speed,
                             Start_Dist = Max_DTT,
                             End_Dist = Start_Initial_Deceleration_Distance)
  return(Aircraft_Profile)
}

# ----------------------------------------------- #
# Profile Building Options
# ----------------------------------------------- #

# Generic 5 stage IAS profile Build. Applicable for all types except gusting enabled 1, 2, 3, 4
Build_IAS_Profile_Main <- function(Aircraft_Profile, LPID_Var){
  
  IAS_Profile_1 <- Set_IAS_Profile_Section_1(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2 <- Set_IAS_Profile_Section_2(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields(LPID_Var)
  
  IAS_Profile <- rbind(IAS_Profile_1, IAS_Profile_2) %>% rbind(IAS_Profile_3) %>% rbind(IAS_Profile_4) %>%
    rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

# Complex 
Build_IAS_Profile_A <- function(Aircraft_Profile, LPID_Var){
  
  IAS_Profile_1a <- Set_IAS_Profile_Section_1a(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1b <- Set_IAS_Profile_Section_1b_A(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1c <- Set_IAS_Profile_Section_1c_A(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1d <- Set_IAS_Profile_Section_1d(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2a <- Set_IAS_Profile_Section_2a(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2b <- Set_IAS_Profile_Section_2b(Aircraft_Profile, 6) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2c <- Set_IAS_Profile_Section_2c(Aircraft_Profile, 7) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 8) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 9) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 10) %>% Select_IAS_Profile_Fields(LPID_Var)
  
  IAS_Profile <- rbind(IAS_Profile_1a, IAS_Profile_1b) %>% rbind(IAS_Profile_1c) %>% rbind(IAS_Profile_1d) %>%
    rbind(IAS_Profile_2a) %>% rbind(IAS_Profile_2b) %>% rbind(IAS_Profile_2c) %>% rbind(IAS_Profile_3) %>%
    rbind(IAS_Profile_4) %>% rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

Build_IAS_Profile_B <- function(Aircraft_Profile, LPID_Var){
  
  IAS_Profile_1a <- Set_IAS_Profile_Section_1a(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1b <- Set_IAS_Profile_Section_1b_B(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1c <- Set_IAS_Profile_Section_1c_B(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1d <- Set_IAS_Profile_Section_1d(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2a <- Set_IAS_Profile_Section_2a(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2b <- Set_IAS_Profile_Section_2b(Aircraft_Profile, 6) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2c <- Set_IAS_Profile_Section_2c(Aircraft_Profile, 7) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 8) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 9) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 10) %>% Select_IAS_Profile_Fields(LPID_Var)
  
  IAS_Profile <- rbind(IAS_Profile_1a, IAS_Profile_1b) %>% rbind(IAS_Profile_1c) %>% rbind(IAS_Profile_1d) %>%
    rbind(IAS_Profile_2a) %>% rbind(IAS_Profile_2b) %>% rbind(IAS_Profile_2c) %>% rbind(IAS_Profile_3) %>%
    rbind(IAS_Profile_4) %>% rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

Build_IAS_Profile_C <- function(Aircraft_Profile, LPID_Var){
  
  IAS_Profile_1a <- Set_IAS_Profile_Section_1a(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1b <- Set_IAS_Profile_Section_1b_C(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_1c <- Set_IAS_Profile_Section_1c_C(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2a <- Set_IAS_Profile_Section_2a(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2b <- Set_IAS_Profile_Section_2b(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_2c <- Set_IAS_Profile_Section_2c(Aircraft_Profile, 6) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 7) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 8) %>% Select_IAS_Profile_Fields(LPID_Var)
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 9) %>% Select_IAS_Profile_Fields(LPID_Var)
  
  IAS_Profile <- rbind(IAS_Profile_1a, IAS_Profile_1b) %>% rbind(IAS_Profile_1c) %>% 
    rbind(IAS_Profile_2a) %>% rbind(IAS_Profile_2b) %>% rbind(IAS_Profile_2c) %>% rbind(IAS_Profile_3) %>%
    rbind(IAS_Profile_4) %>% rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

# ----------------------------------------------- #
# Profile Building by LSS Type
# ----------------------------------------------- #

Build_IAS_Profile_Type_0 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 0)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_1 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 1)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a, LPID_Var)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_A(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_A(Aircraft_Profile_b, LPID_Var)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_2 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 2)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a, LPID_Var)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_A(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_B(Aircraft_Profile_b, LPID_Var)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_3 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 3)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a, LPID_Var)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_B(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_C(Aircraft_Profile_b, LPID_Var)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_4 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 4)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a, LPID_Var)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_C(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_C(Aircraft_Profile_b, LPID_Var)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_5 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 5)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_6 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 6)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_7 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 7)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_8 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 8)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_9 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 9)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_10 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 10)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_11 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 11)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_12 <- function(Aircraft_Profile, LPID_Var){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 12)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile, LPID_Var)
  return(IAS_Profile)
}

# ----------------------------------------------- #
# GSPD Profile Building
# ----------------------------------------------- #
# Single function to build GS Profile for either L/F
# ----------------------------------------------- #


Build_GSPD_Profile <- function(IAS_Profile, ORD_Segments, LPID_Var){
  
  # Find all the unique profile sections 
  Sections <- unique(IAS_Profile$Profile_Section)
  
  # Remove all IAS Sections with Start_Dist < End_Dist (Bug fix 2)
  IAS_Profile <- filter(IAS_Profile, Start_Dist >= End_Dist)
  
  # Open loop for each profile section
  for (i in 1:length(Sections)){
    
    # Filter IAS Profile for this profile section only and select relevant fields
    IAS_Section <- filter(IAS_Profile, Profile_Section == Sections[i]) %>% select(!!sym(LPID_Var), This_Pair_Role, Profile_Section, Profile_Type, End_Dist, Start_Dist, End_IAS, Start_IAS)
    
    # Join on this Section Parameters onto Wind Segments by Landing Pair, Select relevant fields.
    ORD_Section_Segments <- inner_join(ORD_Segments, IAS_Section, by = setNames(LPID_Var, "ID"))
    ORD_Section_Segments <- select(ORD_Section_Segments, ID, This_Pair_Role, Profile_Section, Profile_Type, End_Dist_Wind, Start_Dist_Wind, End_Dist, Start_Dist, End_IAS, Start_IAS, Forecast_Wind_Effect_IAS)
    
    # For this Profile Section: Create flags for Full segments within profile (Within), partial lower segment (Bottom)
    # partial upper segment (Top)
    ORD_Section_Segments <- mutate(ORD_Section_Segments,
                                   Within_Flag = ifelse((End_Dist_Wind >= End_Dist) & (Start_Dist_Wind <= Start_Dist), 1, 0),
                                   Bottom_Flag = ifelse(End_Dist_Wind < Start_Dist & Start_Dist_Wind > Start_Dist, 1, 0),
                                   Top_Flag = ifelse(End_Dist_Wind < End_Dist & Start_Dist_Wind > End_Dist, 1, 0))
    
    # Filter for all segments fully/partially within profile section and bind to 1 table
    Within_Segs <- filter(ORD_Section_Segments, Within_Flag == 1)
    Bottom_Segs <- filter(ORD_Section_Segments, Bottom_Flag == 1)
    Top_Segs <- filter(ORD_Section_Segments, Top_Flag == 1)
    Change_Segs <- rbind(Top_Segs, Within_Segs, Bottom_Segs)
    
    # Adjust Partial Segments: End Distance -> Profile End Distance (Top), Start Distance -> Profile Start Distance (Bottom)
    Change_Segs <- mutate(Change_Segs, 
                          End_Dist_Wind = ifelse(Top_Flag == 1, End_Dist, End_Dist_Wind),
                          Start_Dist_Wind = ifelse(Bottom_Flag == 1, Start_Dist, Start_Dist_Wind))
    
    # Remove Flag variables
    Change_Segs <- select(Change_Segs, -c("Within_Flag", "Bottom_Flag", "Top_Flag"))
    
    # Bind to Complete GS Profile DF.
    if (i == 1){GS_Complete <- Change_Segs} else {GS_Complete <- rbind(GS_Complete, Change_Segs)}
  }
  
  # Order GS Profile by ID and End Distance 
  GS_Complete <- GS_Complete[order(GS_Complete$ID, GS_Complete$End_Dist),]
  
  # Remove Observations with No Start Distance (Bug Fix 1)
  GS_Complete <- filter(GS_Complete, !is.na(Start_Dist_Wind))
  
  # Use y=mx+c to get new speeds, remove intermediary variables, and rename Segment Distances to match GS Profile table
  GS_Complete <- mutate(GS_Complete,
                        IAS_Difference = Start_IAS - End_IAS,
                        Total_Dist_Diff = Start_Dist - End_Dist,
                        End_Dist_Diff = End_Dist_Wind - End_Dist,
                        End_Ratio = End_Dist_Diff / Total_Dist_Diff,
                        Start_Dist_Diff = Start_Dist_Wind - Start_Dist,
                        Start_Ratio = Start_Dist_Diff / Total_Dist_Diff,
                        End_IAS = End_IAS + (IAS_Difference * End_Ratio),
                        Start_IAS = Start_IAS +(IAS_Difference * Start_Ratio)
  ) %>% 
    select(-c("IAS_Difference", "Total_Dist_Diff", "End_Dist_Diff", "End_Ratio", "Start_Dist_Diff",
              "Start_Ratio", "End_Dist", "Start_Dist")) %>% 
    rename(End_Dist = End_Dist_Wind, Start_Dist = Start_Dist_Wind)
  
  
  
  # Add on WE to get Start/End GS. Get ordered section number by using row number per Landing Pair/Pair Role.
  # Select relevant columns (and rename ID to Landing Pair ID) to match GS Profile table format.
  GS_Complete <- mutate(GS_Complete,
                        Start_GS = Start_IAS + Forecast_Wind_Effect_IAS,
                        End_GS = End_IAS + Forecast_Wind_Effect_IAS) %>%
    select(-c("Forecast_Wind_Effect_IAS")) %>% group_by(ID, This_Pair_Role) %>%
    mutate(Section_Number = row_number()) %>% ungroup() %>% select(ID, This_Pair_Role, Section_Number,
                                                                   Profile_Section, Profile_Type, Start_IAS, End_IAS, Start_GS,
                                                                   End_GS, Start_Dist, End_Dist) %>% rename(!!sym(LPID_Var) := "ID")
  
  return(GS_Complete)
}

# Forecast Compression Function. Requires Leader (GS_Profile_Leader) and Follower (GS_Profile_Follower) Groundspeed Profiles 
# With Joined values for Compression Start and End.
Get_Forecast_Compression <- function(GS_Profile, Landing_Pair, LPID_Var, Prefix, Comp_End_Var, Comp_Start_Var, Sep_Dist_Var){
  
  # ------------------------ #
  ### --- Setup
  # ------------------------ #
  
  # Variable Names
  Comp_Var <- paste0("Forecast_", Prefix, "_Compression")
  Lead_Spd_Var <- paste0("Forecast_Leader_", Prefix, "_IAS")
  Lead_WE_Var <- paste0("Forecast_Leader_", Prefix, "_Wind_Effect")
  Foll_Spd_Var <- paste0("Forecast_Follower_", Prefix, "_IAS")
  Foll_WE_Var <- paste0("Forecast_Follower_", Prefix, "_Wind_Effect")
  
  # Split GS Profile into Leader and Follower
  GS_Profile_Leader <- Split_Leader_Follower(GS_Profile, "Leader")
  GS_Profile_Follower <- Split_Leader_Follower(GS_Profile, "Follower")
  
  # Get Start/End Leader Distance from Landing Pair
  Distances <- select(Landing_Pair, !!sym(LPID_Var),
                      "Compression_Start" := !!sym(Comp_Start_Var),
                      "Compression_End" := !!sym(Comp_End_Var),
                      "Follower_End_Distance" := !!sym(Sep_Dist_Var)) 
  
  # Add The End Distances (Separation Distance + Delivery)
  Distances <- mutate(Distances, Follower_End_Distance = Follower_End_Distance + Compression_End)
  
  # Split the Leader/Follower Distances
  Leader_Distances <- select(Distances, -c("Follower_End_Distance"))
  Follower_Distance <- select(Distances, -c("Compression_Start", "Compression_End"))
  
  # Join on the distances data ready for Calculations
  GS_Profile_Leader <- left_join(GS_Profile_Leader, Leader_Distances, by = setNames(LPID_Var, LPID_Var))
  GS_Profile_Follower <- left_join(GS_Profile_Follower, Follower_Distance, by = setNames(LPID_Var, LPID_Var))
  
  # ------------------------ #
  ### --- Leader
  # ------------------------ #
  
  # Get flags for all segments within compression range, and one for start and end
  GS_Profile_Leader <- mutate(GS_Profile_Leader, 
                 In_Range_Flag = ifelse(Start_Dist >= Compression_End & End_Dist < Compression_Start, 1, 0),
                 Compression_End_Flag = ifelse(In_Range_Flag == 1 & End_Dist <= Compression_End, 1, 0),
                 Compression_Start_Flag = ifelse(In_Range_Flag == 1 & Start_Dist >= Compression_Start, 1, 0))
  
  # Filter all segments not within the Compression range
  GS_Profile_Leader <- filter(GS_Profile_Leader, In_Range_Flag != 0)
  
  # Find the GSPD and IAS differences for adjustment
  GS_Profile_Leader <- mutate(GS_Profile_Leader, GSPD_Difference = Start_GS - End_GS)
  GS_Profile_Leader <- mutate(GS_Profile_Leader, IAS_Difference = Start_IAS - End_IAS)
  
  # Adjust Leader Start/End Section values: Compression End Section.
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Distance_Ratio = (Compression_End - End_Dist)/(Start_Dist - End_Dist))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_GS = ifelse(Compression_End_Flag == 1, End_GS + (GSPD_Difference * Distance_Ratio), End_GS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_IAS = ifelse(Compression_End_Flag == 1, End_IAS + (IAS_Difference * Distance_Ratio), End_IAS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_Dist = ifelse(Compression_End_Flag == 1, Compression_End, End_Dist))
  
  # Adjust Leader Start/End Section values: Compression Start Section.
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Distance_Ratio = (Start_Dist - Compression_Start)/(Start_Dist - End_Dist))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_GS = ifelse(Compression_Start_Flag == 1, Start_GS - (GSPD_Difference * Distance_Ratio), Start_GS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_IAS = ifelse(Compression_Start_Flag == 1, Start_IAS - (IAS_Difference * Distance_Ratio), Start_IAS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_Dist = ifelse(Compression_Start_Flag == 1, Compression_Start, Start_Dist))
  
  # Calculate the Leader Section Flying Times/Distance
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Section_Flying_Time = 2 * (Start_Dist - End_Dist)/(Start_GS + End_GS),
                 Section_Flying_Distance = (Start_GS + End_GS) * Section_Flying_Time / 2)
  
  # Calculate the Start and End Wind Effects
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_WE = Start_GS - Start_IAS)
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_WE = End_GS - End_IAS)
  
  # Calculate the aggregate Mean Leader IAS/Wind Effect Trapezium rule calculations
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Mean_Leader_IAS = (Start_IAS + End_IAS) * Section_Flying_Time / 2)
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Forecast_Mean_Leader_Wind_Effect = (Start_WE + End_WE) * Section_Flying_Time / 2) 
  
  # Get the Leader Stats: Flying Time, Flying Disance, Mean IAS and Mean WE (Trapezium)
  Leader_Stats <- GS_Profile_Leader %>% group_by(Landing_Pair_ID) %>% 
    summarise(Leader_Flying_Time = sum(Section_Flying_Time, na.rm = F),
              Leader_Flying_Distance = max(Compression_Start - Compression_End, na.rm = F),
              Mean_Leader_IAS = sum(Mean_Leader_IAS, na.rm = F),
              Forecast_Mean_Leader_Wind_Effect = sum(Forecast_Mean_Leader_Wind_Effect, na.rm = F)) %>% ungroup() %>%
    mutate(Mean_Leader_IAS = Mean_Leader_IAS / Leader_Flying_Time,
           Forecast_Mean_Leader_Wind_Effect = Forecast_Mean_Leader_Wind_Effect / Leader_Flying_Time)

  
  # Get the Leader Flying times specifically for the Follower
  Leader_Times <- select(Leader_Stats, !!sym(LPID_Var), Leader_Flying_Time)
  
  # ------------------------ #
  ### --- Follower
  # ------------------------ #
  
  # Get flags for Follower being within travel range and it's end section
  GS_Profile_Follower <- mutate(GS_Profile_Follower, 
                 Viable_Flag = ifelse(Start_Dist >= Follower_End_Distance, 1, 0),
                 Start_Flag = ifelse(Viable_Flag == 1 & End_Dist <= Follower_End_Distance, 1, 0))
  
  # Filter for only Viable follower segments
  GS_Profile_Follower <- filter(GS_Profile_Follower, Viable_Flag == 1)
  
  # Find the Follower IAS/GSPD Differences for Partial Section Calculations.
  GS_Profile_Follower <- mutate(GS_Profile_Follower, GSPD_Difference = Start_GS - End_GS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, IAS_Difference = Start_IAS - End_IAS)
  
  # Adjust Follower end section values. 
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Distance_Ratio = (Follower_End_Distance - End_Dist)/(Start_Dist - End_Dist))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_GS = ifelse(Start_Flag == 1, End_GS + (GSPD_Difference * Distance_Ratio), End_GS))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_IAS = ifelse(Start_Flag == 1, End_IAS + (IAS_Difference * Distance_Ratio), End_IAS))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_Dist = ifelse(Start_Flag == 1, Follower_End_Distance, End_Dist))
  
  # Find the first pass of the Section Flying Times. The End section time will be updated later.
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Section_Flying_Time = 2 * (Start_Dist - End_Dist)/(Start_GS + End_GS))
  
  # Join on the Leader Flying Times for each Landing Pair ID.
  GS_Profile_Follower <- left_join(GS_Profile_Follower, Leader_Times, by=setNames(LPID_Var, LPID_Var))
  
  # Get the Cumulative Flying Times for each Follower. For each segment and it's previous segment.
  GS_Profile_Follower <- group_by(GS_Profile_Follower, !!sym(LPID_Var)) %>% mutate(Cumulative_Time = cumsum(Section_Flying_Time)) %>% ungroup()
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Prev_Cumulative_Time = Cumulative_Time - Section_Flying_Time)
  
  # Create flags for all Valid sections and the Follower start section
  GS_Profile_Follower <- mutate(GS_Profile_Follower, 
                 Section_Time_Flag = ifelse(Prev_Cumulative_Time <= Leader_Flying_Time, 1, 0),
                 Last_Section_Flag = ifelse(Cumulative_Time >= Leader_Flying_Time & Prev_Cumulative_Time < Leader_Flying_Time, 1, 0))
  
  # Filter only for valid Follower Sections
  GS_Profile_Follower <- filter(GS_Profile_Follower, Section_Time_Flag == 1)
  
  # Adjust Follower Start Section values. Use Time Ratio.
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Time_Ratio = (Leader_Flying_Time - Prev_Cumulative_Time)/(Cumulative_Time - Prev_Cumulative_Time))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, GSPD_Difference = Start_GS - End_GS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, IAS_Difference = Start_IAS - End_IAS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_GS = ifelse(Last_Section_Flag == 1, End_GS + (GSPD_Difference * Time_Ratio), Start_GS))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_IAS = ifelse(Last_Section_Flag == 1, End_IAS + (IAS_Difference * Time_Ratio), Start_IAS))
  
  # Adjust Initial section Flying Time, start distance and Cumulative time 
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Section_Flying_Time = ifelse(Last_Section_Flag == 1, (Leader_Flying_Time - Prev_Cumulative_Time), Section_Flying_Time))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Cumulative_Time = ifelse(Last_Section_Flag == 1, Leader_Flying_Time, Cumulative_Time))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_Dist = ifelse(Last_Section_Flag == 1, End_Dist + (0.5 * Section_Flying_Time * (Start_GS + End_GS)),  Start_Dist),
                 Section_Flying_Distance = (Start_GS + End_GS) * Section_Flying_Time / 2)
  
  # Calculate the Start/End Follower Wind Effects
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_WE = Start_GS - Start_IAS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_WE = End_GS - End_IAS)
  
  # Calculate the aggregate Mean Follower IAS/Wind Effect Trapezium rule calculations
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Mean_Follower_IAS = (Start_IAS + End_IAS) * Section_Flying_Time / 2)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Forecast_Mean_Follower_Wind_Effect = (Start_WE + End_WE) * Section_Flying_Time / 2)  
  
  # Calculate the Cumulative Follower Flying Distance
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Cumulative_Follower_Distance = Start_Dist - Follower_End_Distance)
  
  # Get the Follower Statistics: Follower Flying Distance/Time, Mean IAS, Mean WE (Trapezium rule)
  Follower_Stats <- group_by(GS_Profile_Follower, Landing_Pair_ID) %>% 
    summarise(Follower_Flying_Distance = sum(Section_Flying_Distance, na.rm = F),
              Follower_Flying_Time = max(Cumulative_Time, na.rm = F),
              Mean_Follower_IAS = sum(Mean_Follower_IAS, na.rm = F),
              Forecast_Mean_Follower_Wind_Effect = sum(Forecast_Mean_Follower_Wind_Effect, na.rm = F)) %>% ungroup() %>%
    mutate(Mean_Follower_IAS = Mean_Follower_IAS / Follower_Flying_Time,
           Forecast_Mean_Follower_Wind_Effect = Forecast_Mean_Follower_Wind_Effect / Follower_Flying_Time)
  
  # ------------------------ #
  ### --- Results & Output
  # ------------------------ #
  
  # Combine the Leader and Follower Stats
  All_Stats <- full_join(Leader_Stats, Follower_Stats, by = setNames(LPID_Var, LPID_Var))
  
  # Calculate Compression
  All_Stats <- mutate(All_Stats, Forecast_Compression = Follower_Flying_Distance - Leader_Flying_Distance)
  
  # TEMP FIX: If Leader Time != Follower Time, Make Everything NULL
  All_Stats <- mutate(All_Stats, 
                      Forecast_Compression = ifelse(Leader_Flying_Time - Follower_Flying_Time > 0.0001, NA, Forecast_Compression),
                      Follower_Flying_Distance = ifelse(Leader_Flying_Time - Follower_Flying_Time > 0.0001, NA, Follower_Flying_Distance),
                      Mean_Follower_IAS = ifelse(Leader_Flying_Time - Follower_Flying_Time > 0.0001, NA, Mean_Follower_IAS),
                      Forecast_Mean_Follower_Wind_Effect = ifelse(Leader_Flying_Time - Follower_Flying_Time > 0.0001, NA, Forecast_Mean_Follower_Wind_Effect))
  
  
  ## Rename Variables
  All_Stats <- select(All_Stats,
                      !!sym(LPID_Var),
                      !!sym(Comp_Var) := "Forecast_Compression",
                      !!sym(Lead_Spd_Var) := "Mean_Leader_IAS",
                      !!sym(Lead_WE_Var) := "Forecast_Mean_Leader_Wind_Effect",
                      !!sym(Foll_Spd_Var) := "Mean_Follower_IAS",
                      !!sym(Foll_WE_Var) := "Forecast_Mean_Follower_Wind_Effect")
  
  ## Join with Landing Pair Data
  Landing_Pair <- left_join(Landing_Pair, All_Stats, by = setNames(LPID_Var, LPID_Var))
                      
  
  # Return results
  return(Landing_Pair)
  
}

Get_Prediction_Error_Variables <- function(Landing_Pair, Prefix){
  
  # Define Variables
  Pred_Comp_Var <- paste0("Forecast_", Prefix, "_Compression")
  Obs_Comp_Var <- paste0("Observed_", Prefix, "_Compression")
  Comp_Error_Var <- paste0(Prefix, "_Compression_Error")
  Pred_Lead_Spd_Var <- paste0("Forecast_Leader_", Prefix, "_IAS")
  Pred_Foll_Spd_Var <- paste0("Forecast_Follower_", Prefix, "_IAS")
  Pred_Lead_WE_Var <- paste0("Forecast_Leader_", Prefix, "_Wind_Effect")
  Pred_Foll_WE_Var <- paste0("Forecast_Follower_", Prefix, "_Wind_Effect")
  Obs_Lead_Spd_Var <- paste0("Observed_Leader_", Prefix, "_IAS")
  Obs_Foll_Spd_Var <- paste0("Observed_Follower_", Prefix, "_IAS")
  Obs_Lead_WE_Var <- paste0("Observed_Leader_", Prefix, "_Wind_Effect")
  Obs_Foll_WE_Var <- paste0("Observed_Follower_", Prefix, "_Wind_Effect")
  Lead_Spd_Error_Var <- paste0("Forecast_Leader_", Prefix, "_IAS_Error")
  Foll_Spd_Error_Var <- paste0("Forecast_Follower_", Prefix, "_IAS_Error")
  Lead_WE_Error_Var <- paste0("Forecast_Leader_", Prefix, "_Wind_Effect_Error")
  Foll_WE_Error_Var <- paste0("Forecast_Follower_", Prefix, "_Wind_Effect_Error")
  
  # Mutate Variables
  Landing_Pair <- mutate(Landing_Pair,
                         !!sym(Comp_Error_Var) := !!sym(Obs_Comp_Var) - !!sym(Pred_Comp_Var),
                         !!sym(Lead_Spd_Error_Var) := !!sym(Obs_Lead_Spd_Var) - !!sym(Pred_Lead_Spd_Var),
                         !!sym(Foll_Spd_Error_Var) := !!sym(Obs_Foll_Spd_Var) - !!sym(Pred_Foll_Spd_Var),
                         !!sym(Lead_WE_Error_Var) := !!sym(Obs_Lead_WE_Var) - !!sym(Pred_Lead_WE_Var),
                         !!sym(Foll_WE_Error_Var) := !!sym(Obs_Foll_WE_Var) - !!sym(Pred_Foll_WE_Var))
                        
  return(Landing_Pair)

}

Add_Test_Variable <- function(Data, Type, Parameter, Tolerance){
  
  # Get SQL Parameter Name
  SQL_Param <- paste0("SQL_", Parameter)
  FLAG_Param <- paste0("FLAG_", Parameter)
  DIFF_Param <- paste0("DIFF_", Parameter)
  
  # Add DIFF and FLAG Parameters
  if (Type == "Numeric"){
  Data <- mutate(Data, 
                 !!sym(DIFF_Param) := abs(!!sym(SQL_Param) - !!sym(Parameter)),
                 !!sym(FLAG_Param) := ifelse(!!sym(DIFF_Param) > Tolerance, 1, 0))
  }
  
  if (Type != "Numeric"){
    Data <- mutate(Data, !!sym(FLAG_Param) := ifelse(!!sym(SQL_Param) != !!sym(Parameter), 1, 0))
  }
  
  Data <- mutate(Data,
                 !!sym(FLAG_Param) := ifelse(is.na(!!sym(Parameter)) & !is.na(!!sym(SQL_Param)), 1, !!sym(FLAG_Param)),
                 !!sym(FLAG_Param) := ifelse(!is.na(!!sym(Parameter)) & is.na(!!sym(SQL_Param)), 1, !!sym(FLAG_Param)))
  
  return(Data)
}

Debug_Test_Variable <- function(Data, ID_Var, Type, Parameter){
  
  # Get Parameters
  SQL_Param <- paste0("SQL_", Parameter)
  DIFF_Param <- paste0("DIFF_", Parameter)
  FLAG_Param <- paste0("FLAG_", Parameter)
  
  # Filter for Flagged Observations
  Data <- filter(Data, !!sym(FLAG_Param) == 1)
  
  # Select Relevant Parameters (Numeric)
  if(Type == "Numeric"){Data <- select(Data,
                                       !!sym(ID_Var),
                                       !!sym(Parameter),
                                       !!sym(SQL_Param),
                                       !!sym(DIFF_Param),
                                       !!sym(FLAG_Param))}
  
  return(Data)
  
}

Load_Radar_Data_ORD_Validation <- function(con, PROC_Period, PROC_Criteria){
  
  # Get Start Time
  message(paste0("Loading Radar data for ORD Validation for the ", PROC_Period, " of ", PROC_Criteria, "..."))
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  
  # Original Radar Data Query
  Radar_Query <- "SELECT 
                    rtp.Flight_Plan_ID,
                    rtp.Track_Time,
                    rtpd.Range_To_Threshold,
                    rtpd.Mode_S_Wind_Localiser_Capture,
                    rtp.Mode_S_IAS,
                    rtpd.Wind_Effect_IAS,
                    rtpd.ILS_Locus_RTT,
                    rtpd.Range_To_ILS,
                    rtpd.Path_Leg
                  FROM tbl_Radar_Track_Point rtp
                  LEFT JOIN tbl_Radar_Track_Point_Derived rtpd
                  ON rtp.Radar_Track_Point_ID = rtpd.Radar_Track_Point_ID "
  
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

Load_Surface_Wind_Data <- function(con, PROC_Period, PROC_Criteria){
  
  # Get Start Time
  message(paste0("Loading Surface Wind data for the ", PROC_Period, " of ", PROC_Criteria, "..."))
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  
  # Original Surface Wind Query
  Surface_Wind_Query <- "SELECT
                         Landing_Runway,
                         Anemo_Date,
                         Anemo_Time,
                         Anemo_SPD,
                         Anemo_HDG
                       FROM tbl_Anemometer"  
  
  # Edit Based on Data Loading Criteria
  if (PROC_Period == "Day"){
    Surface_Wind_Query <- paste0(Surface_Wind_Query, " WHERE Anemo_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){
    Surface_Wind_Query <- paste0(Surface_Wind_Query, " WHERE Anemo_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Acquire the Data
  Surface_Wind <- sqlQuery(con, Surface_Wind_Query, stringsAsFactors = F)
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Completed Surface Wind Loading for the ", PROC_Period, " of ", PROC_Criteria, " in ",
                 seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Surface_Wind)
  
}

Load_Flight_Data_ORD_Validation <- function(con, PROC_Period, PROC_Criteria){
  
  # Get Start Time
  message(paste0("Loading ORD Validation Flight Plan data for the ", PROC_Period, " of ", PROC_Criteria, "..."))
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  
  # Original Flight Plan Query
  Flight_Plan_Query <- "SELECT
                         FP.Flight_Plan_ID,
                         FP_Time,
                         Aircraft_Type,
                         Callsign,
                         FP.Landing_Runway,
                         Time_At_4DME
                       FROM tbl_Flight_Plan FP
                       LEFT JOIN tbl_Flight_Plan_Derived FPD
                       ON FP.Flight_Plan_ID = FPD.Flight_Plan_ID"
  
  # Edit Based on Data Loading Criteria
  if (PROC_Period == "Day"){
    Flight_Plan_Query <- paste0(Flight_Plan_Query, " WHERE FP_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){
    Flight_Plan_Query <- paste0(Flight_Plan_Query, " WHERE FP_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Acquire the Data
  Flight_Plan <- sqlQuery(con, Flight_Plan_Query, stringsAsFactors = F)
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Completed ORD Validation Flight Plan data Loading for the ", PROC_Period, " of ", PROC_Criteria, " in ",
                 seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Flight_Plan)
  
}

Load_Landing_Pair_Data <- function(con, PROC_Period, PROC_Criteria){
  
  # Get Start Time
  message(paste0("Loading Landing Pair data for the ", PROC_Period, " of ", PROC_Criteria, "..."))
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  
  # Original Landing Pair Query
  Landing_Pair_Query <- "SELECT 
                         *
                       FROM tbl_Landing_Pair" 
  
  # Edit Based on Data Loading Criteria
  if (PROC_Period == "Day"){
    Landing_Pair_Query <- paste0(Landing_Pair_Query, " WHERE Landing_Pair_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){
    Landing_Pair_Query <- paste0(Landing_Pair_Query, " WHERE Landing_Pair_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Acquire the Data
  Landing_Pair <- sqlQuery(con, Landing_Pair_Query, stringsAsFactors = F)
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Completed Landing Pair data Loading for the ", PROC_Period, " of ", PROC_Criteria, " in ",
                 seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Landing_Pair)
  
}

Load_Stage_2_Segment_Data <- function(con, PROC_Period, PROC_Criteria){
  
  # Get Start Time
  message(paste0("Loading Stage 2 Segment data for the ", PROC_Period, " of ", PROC_Criteria, "..."))
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  
  # Original Stage 2 Segment Query
  Forecast_Segs_Query <- "SELECT 
                         *
                        FROM tbl_Mode_S_Wind_Seg_Forecast"
  
  # Edit Based on Data Loading Criteria
  if (PROC_Period == "Day"){
    Forecast_Segs_Query <- paste0(Forecast_Segs_Query, " WHERE Forecast_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){
    Forecast_Segs_Query <- paste0(Forecast_Segs_Query, " WHERE Forecast_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Acquire the Data
  Segments <- sqlQuery(con, Forecast_Segs_Query, stringsAsFactors = F)
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Completed Stage 2 Segment data Loading for the ", PROC_Period, " of ", PROC_Criteria, " in ",
                 seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Segments)
  
}

Get_LP_Primary_Key <- function(Database_Type){
  
  # Name of Landing Pair Primary Key for Validation/Verification
  if(Database_Type == "Validation"){return("Landing_Pair_ID")}
  if(Database_Type == "Verification"){return("ORD_Tool_Calculation_ID")}
}


# Forecast Compression Function. Requires Leader (GS_Profile_Leader) and Follower (GS_Profile_Follower) Groundspeed Profiles 
# With Joined values for Compression Start and End.
Get_Forecast_ORD_Parameters <- function(GS_Profile, Landing_Pair, LPID_Var, Prefix, Comp_End_Var, Comp_Start_Var, Sep_Dist_Var, Metric_Type){
  
  # ------------------------ #
  ### --- Setup
  # ------------------------ #
  
  # Hardcoded Compression Metric: 1 = Distance difference, 2 = Distance,Speed,WE
  #'Metric_Type <- 1
  
  # Variable Names
  Foll_Flying_Time_Var <- paste0("Forecast_Follower_", Prefix, "_Flying_Time")
  Lead_Flying_Time_Var <- paste0("Forecast_Leader_", Prefix, "_Flying_Time")
  Foll_Flying_Dist_Var <- paste0("Forecast_Follower_", Prefix, "_Flying_Distance")
  Lead_Flying_Dist_Var <- paste0("Forecast_Leader_", Prefix, "_Flying_Distance")
  Lead_Spd_Var <- paste0("Forecast_Leader_", Prefix, "_IAS")
  Lead_WE_Var <- paste0("Forecast_Leader_", Prefix, "_Wind_Effect")
  Foll_Spd_Var <- paste0("Forecast_Follower_", Prefix, "_IAS")
  Foll_WE_Var <- paste0("Forecast_Follower_", Prefix, "_Wind_Effect")
  
  # ------------------------ #
  ### --- Processing
  # ------------------------ #
  
  # Split GS Profile into Leader/Follower
  GS_Profile_Follower <- Split_Leader_Follower(GS_Profile, "Follower")
  GS_Profile_Leader <- Split_Leader_Follower(GS_Profile, "Leader")
  
  # Get the Leader Flying Stats
  Leader_Stats <- Calculate_Predicted_ORD_Flying_Parameters_Leader(GS_Profile_Leader, Landing_Pair, LPID_Var, 
                                                                   Comp_Start_Var,
                                                                   Delivery_Var = Comp_End_Var,
                                                                   Prefix)
  
  # Select relevant fields from Leader Stats
  Leader_Stats <- select(Leader_Stats, 
                         !!sym(LPID_Var),
                         !!sym(Lead_Flying_Dist_Var),
                         !!sym(Lead_Flying_Time_Var),
                         !!sym(Lead_Spd_Var),
                         !!sym(Lead_WE_Var))
  
  # Join the relevant Leader Stats to Landing Pair
  Landing_Pair <- left_join(Landing_Pair, Leader_Stats, by = setNames(LPID_Var, LPID_Var))
  
  # Now calculate the Follower flying stats based on the Leader flying times we just calculated
  Follower_Stats <- Calculate_Predicted_ORD_Flying_Parameters_Follower(GS_Profile_Follower, Landing_Pair, LPID_Var,
                                                                       Delivery_Var = Comp_End_Var,
                                                                       Sep_Dist_Var,
                                                                       Target_Time_Var = Lead_Flying_Time_Var,
                                                                       Prefix)
  
  # Select the relevant fields from Follower stats.
  Follower_Stats <- select(Follower_Stats, 
                           !!sym(LPID_Var),
                           !!sym(Foll_Flying_Dist_Var),
                           !!sym(Foll_Flying_Time_Var),
                           !!sym(Foll_Spd_Var),
                           !!sym(Foll_WE_Var))
  
  # ------------------------ #
  ### --- Results & Output
  # ------------------------ #
  
  ## Join with Landing Pair Data
  Landing_Pair <- left_join(Landing_Pair, Follower_Stats, by = setNames(LPID_Var, LPID_Var))
  
  #TEMP FIX: If Leader Time != Follower Time, Make Everything NULL
  Landing_Pair <- mutate(Landing_Pair,
                      Temp_Flag = ifelse(!!sym(Lead_Flying_Time_Var) - !!sym(Foll_Flying_Time_Var) > 0.0001, 1, 0),
                      !!sym(Foll_Flying_Dist_Var) := ifelse(Temp_Flag == 1, NA, !!sym(Foll_Flying_Dist_Var)),
                      !!sym(Foll_Spd_Var) := ifelse(Temp_Flag == 1, NA, !!sym(Foll_Spd_Var)),
                      !!sym(Foll_WE_Var) := ifelse(Temp_Flag == 1, NA, !!sym(Foll_WE_Var))) %>%
    select(-Temp_Flag)
  
  # Calculate Compression
  Landing_Pair <- Calculate_Forecast_Compression(Landing_Pair, Metric_Type, Prefix)
  
  # Return results
  return(Landing_Pair)
  
  
}


Calculate_Predicted_ORD_Flying_Parameters_Leader <- function(GS_Profile_Leader, Landing_Pair, LP_Primary_Key, Comp_Start_Var, Delivery_Var, Prefix){
  
  # ------------------------ #
  ### --- Setup
  # ------------------------ #
  
  # Use LPID_Var as Primary Key for now
  LPID_Var <- LP_Primary_Key
  
  # Variable Names
  Lead_Spd_Var <- paste0("Forecast_Leader_", Prefix, "_IAS")
  Lead_WE_Var <- paste0("Forecast_Leader_", Prefix, "_Wind_Effect")
  Lead_Flying_Time_Var <- paste0("Forecast_Leader_", Prefix, "_Flying_Time")
  Lead_Flying_Dist_Var <- paste0("Forecast_Leader_", Prefix, "_Flying_Distance")
  
  # Get Start/End Leader Distance from Landing Pair
  Distances <- select(Landing_Pair, !!sym(LPID_Var),
                      "Compression_Start" := !!sym(Comp_Start_Var),
                      "Compression_End" := !!sym(Delivery_Var)) 
  
  # Join on the distances data ready for Calculations
  GS_Profile_Leader <- inner_join(GS_Profile_Leader, Distances, by = setNames(LPID_Var, LPID_Var))
  
  # ------------------------ #
  ### --- Processing
  # ------------------------ #
  
  # Get flags for all segments within compression range, and one for start and end
  GS_Profile_Leader <- mutate(GS_Profile_Leader, 
                              In_Range_Flag = ifelse(Start_Dist >= Compression_End & End_Dist < Compression_Start, 1, 0),
                              Compression_End_Flag = ifelse(In_Range_Flag == 1 & End_Dist <= Compression_End, 1, 0),
                              Compression_Start_Flag = ifelse(In_Range_Flag == 1 & Start_Dist >= Compression_Start, 1, 0))
  
  # Filter all segments not within the Compression range
  GS_Profile_Leader <- filter(GS_Profile_Leader, In_Range_Flag != 0)
  
  # Find the GSPD and IAS differences for adjustment
  GS_Profile_Leader <- mutate(GS_Profile_Leader, GSPD_Difference = Start_GS - End_GS)
  GS_Profile_Leader <- mutate(GS_Profile_Leader, IAS_Difference = Start_IAS - End_IAS)
  
  # Adjust Leader Start/End Section values: Compression End Section.
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Distance_Ratio = (Compression_End - End_Dist)/(Start_Dist - End_Dist))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_GS = ifelse(Compression_End_Flag == 1, End_GS + (GSPD_Difference * Distance_Ratio), End_GS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_IAS = ifelse(Compression_End_Flag == 1, End_IAS + (IAS_Difference * Distance_Ratio), End_IAS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_Dist = ifelse(Compression_End_Flag == 1, Compression_End, End_Dist))
  
  # Adjust Leader Start/End Section values: Compression Start Section.
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Distance_Ratio = (Start_Dist - Compression_Start)/(Start_Dist - End_Dist))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_GS = ifelse(Compression_Start_Flag == 1, Start_GS - (GSPD_Difference * Distance_Ratio), Start_GS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_IAS = ifelse(Compression_Start_Flag == 1, Start_IAS - (IAS_Difference * Distance_Ratio), Start_IAS))
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_Dist = ifelse(Compression_Start_Flag == 1, Compression_Start, Start_Dist))
  
  # Calculate the Leader Section Flying Times/Distance
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Section_Flying_Time = 2 * (Start_Dist - End_Dist)/(Start_GS + End_GS),
                              Section_Flying_Distance = (Start_GS + End_GS) * Section_Flying_Time / 2)
  
  # Calculate the Start and End Wind Effects
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Start_WE = Start_GS - Start_IAS)
  GS_Profile_Leader <- mutate(GS_Profile_Leader, End_WE = End_GS - End_IAS)
  
  # Calculate the aggregate Mean Leader IAS/Wind Effect Trapezium rule calculations
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Mean_Leader_IAS = (Start_IAS + End_IAS) * Section_Flying_Time / 2)
  GS_Profile_Leader <- mutate(GS_Profile_Leader, Forecast_Mean_Leader_Wind_Effect = (Start_WE + End_WE) * Section_Flying_Time / 2) 
  
  # Get the Leader Stats: Flying Time, Flying Disance, Mean IAS and Mean WE (Trapezium)
  Leader_Stats <- GS_Profile_Leader %>%
    group_by(!!sym(LPID_Var)) %>% 
    summarise(!!sym(Lead_Flying_Time_Var) := sum(Section_Flying_Time, na.rm = F),
              !!sym(Lead_Flying_Dist_Var) := max((Compression_Start - Compression_End), na.rm = F),
              !!sym(Lead_Spd_Var) := sum(Mean_Leader_IAS, na.rm = F),
              !!sym(Lead_WE_Var) := sum(Forecast_Mean_Leader_Wind_Effect, na.rm = F)) %>% 
    ungroup() %>%
    mutate(!!sym(Lead_Spd_Var) := !!sym(Lead_Spd_Var) / !!sym(Lead_Flying_Time_Var),
           !!sym(Lead_WE_Var) := !!sym(Lead_WE_Var) / !!sym(Lead_Flying_Time_Var))
  
  return(Leader_Stats)
  
}


Calculate_Predicted_ORD_Flying_Parameters_Follower <- function(GS_Profile_Follower, Landing_Pair, LP_Primary_Key, Delivery_Var, Sep_Dist_Var, Target_Time_Var, Prefix){
  
  # ------------------------ #
  ### --- Setup
  # ------------------------ #
  
  # Use LPID_Var as Primary Key for now
  LPID_Var <- LP_Primary_Key
  
  # Variable Names
  Foll_Spd_Var <- paste0("Forecast_Follower_", Prefix, "_IAS")
  Foll_WE_Var <- paste0("Forecast_Follower_", Prefix, "_Wind_Effect")
  Foll_Flying_Time_Var <- paste0("Forecast_Follower_", Prefix, "_Flying_Time")
  Foll_Flying_Dist_Var <- paste0("Forecast_Follower_", Prefix, "_Flying_Distance")
  
  # Get Start/End Leader Distance from Landing Pair
  Parameters <- select(Landing_Pair, !!sym(LPID_Var),
                       "Delivery_Point" := !!sym(Delivery_Var),
                       "Follower_End_Distance" := !!sym(Sep_Dist_Var),
                       "Target_Flying_Time" := !!sym(Target_Time_Var)) 
  
  # Add The End Distances (Separation Distance + Delivery)
  Parameters <- mutate(Parameters, Follower_End_Distance = Follower_End_Distance + Delivery_Point)
  
  # Join on the distances data ready for Calculations
  GS_Profile_Follower <- left_join(GS_Profile_Follower, Parameters, by = setNames(LPID_Var, LPID_Var))
  
  # ------------------------ #
  ### --- Processing
  # ------------------------ #
  
  # Get flags for Follower being within travel range and it's end section
  GS_Profile_Follower <- mutate(GS_Profile_Follower, 
                                Viable_Flag = ifelse(Start_Dist >= Follower_End_Distance, 1, 0),
                                Start_Flag = ifelse(Viable_Flag == 1 & End_Dist <= Follower_End_Distance, 1, 0))
  
  # Filter for only Viable follower segments
  GS_Profile_Follower <- filter(GS_Profile_Follower, Viable_Flag == 1)
  
  # Find the Follower IAS/GSPD Differences for Partial Section Calculations.
  GS_Profile_Follower <- mutate(GS_Profile_Follower, GSPD_Difference = Start_GS - End_GS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, IAS_Difference = Start_IAS - End_IAS)
  
  # Adjust Follower end section values. 
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Distance_Ratio = (Follower_End_Distance - End_Dist)/(Start_Dist - End_Dist))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_GS = ifelse(Start_Flag == 1, End_GS + (GSPD_Difference * Distance_Ratio), End_GS))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_IAS = ifelse(Start_Flag == 1, End_IAS + (IAS_Difference * Distance_Ratio), End_IAS))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_Dist = ifelse(Start_Flag == 1, Follower_End_Distance, End_Dist))
  
  # Find the first pass of the Section Flying Times. The End section time will be updated later.
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Section_Flying_Time = 2 * (Start_Dist - End_Dist)/(Start_GS + End_GS))
  
  # Get the Cumulative Flying Times for each Follower. For each segment and it's previous segment.
  GS_Profile_Follower <- group_by(GS_Profile_Follower, !!sym(LPID_Var)) %>% mutate(Cumulative_Time = cumsum(Section_Flying_Time)) %>% ungroup()
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Prev_Cumulative_Time = Cumulative_Time - Section_Flying_Time)
  
  # Create flags for all Valid sections and the Follower start section
  GS_Profile_Follower <- mutate(GS_Profile_Follower, 
                                Section_Time_Flag = ifelse(Prev_Cumulative_Time <= Target_Flying_Time, 1, 0),
                                Last_Section_Flag = ifelse(Cumulative_Time >= Target_Flying_Time & Prev_Cumulative_Time < Target_Flying_Time, 1, 0))
  
  # Filter only for valid Follower Sections
  GS_Profile_Follower <- filter(GS_Profile_Follower, Section_Time_Flag == 1)
  
  # Adjust Follower Start Section values. Use Time Ratio.
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Time_Ratio = (Target_Flying_Time - Prev_Cumulative_Time)/(Cumulative_Time - Prev_Cumulative_Time))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, GSPD_Difference = Start_GS - End_GS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, IAS_Difference = Start_IAS - End_IAS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_GS = ifelse(Last_Section_Flag == 1, End_GS + (GSPD_Difference * Time_Ratio), Start_GS))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_IAS = ifelse(Last_Section_Flag == 1, End_IAS + (IAS_Difference * Time_Ratio), Start_IAS))
  
  # Adjust Initial section Flying Time, start distance and Cumulative time 
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Section_Flying_Time = ifelse(Last_Section_Flag == 1, (Target_Flying_Time - Prev_Cumulative_Time), Section_Flying_Time))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Cumulative_Time = ifelse(Last_Section_Flag == 1, Target_Flying_Time, Cumulative_Time))
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_Dist = ifelse(Last_Section_Flag == 1, End_Dist + (0.5 * Section_Flying_Time * (Start_GS + End_GS)),  Start_Dist),
                                Section_Flying_Distance = (Start_GS + End_GS) * Section_Flying_Time / 2)
  
  # Calculate the Start/End Follower Wind Effects
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Start_WE = Start_GS - Start_IAS)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, End_WE = End_GS - End_IAS)
  
  # Calculate the aggregate Mean Follower IAS/Wind Effect Trapezium rule calculations
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Mean_Follower_IAS = (Start_IAS + End_IAS) * Section_Flying_Time / 2)
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Forecast_Mean_Follower_Wind_Effect = (Start_WE + End_WE) * Section_Flying_Time / 2)  
  
  # Calculate the Cumulative Follower Flying Distance
  GS_Profile_Follower <- mutate(GS_Profile_Follower, Cumulative_Follower_Distance = Start_Dist - Follower_End_Distance)
  
  # Get the Follower Statistics: Follower Flying Distance/Time, Mean IAS, Mean WE (Trapezium rule)
  Follower_Stats <- GS_Profile_Follower %>%
    group_by(!!sym(LPID_Var)) %>% 
    summarise(!!sym(Foll_Flying_Time_Var) := sum(Section_Flying_Time, na.rm = F),
              !!sym(Foll_Flying_Dist_Var) := sum(Section_Flying_Distance, na.rm = F),
              !!sym(Foll_Spd_Var) := sum(Mean_Follower_IAS, na.rm = F),
              !!sym(Foll_WE_Var) := sum(Forecast_Mean_Follower_Wind_Effect, na.rm = F)) %>% 
    ungroup() %>%
    mutate(!!sym(Foll_Spd_Var) := !!sym(Foll_Spd_Var) / !!sym(Foll_Flying_Time_Var),
           !!sym(Foll_WE_Var) := !!sym(Foll_WE_Var) / !!sym(Foll_Flying_Time_Var))
  
  
  return(Follower_Stats)
  
  
}


Calculate_Forecast_Compression <- function(Landing_Pair, Metric_Type, Prefix){
  
  # Get Output Variable name
  Comp_Var <- paste0("Forecast_", Prefix, "_Compression")
  
  # Get input variable names
  Foll_Flying_Dist_Var <- paste0("Forecast_Follower_", Prefix, "_Flying_Distance")
  Lead_Flying_Dist_Var <- paste0("Forecast_Leader_", Prefix, "_Flying_Distance")
  Lead_Spd_Var <- paste0("Forecast_Leader_", Prefix, "_IAS")
  Lead_WE_Var <- paste0("Forecast_Leader_", Prefix, "_Wind_Effect")
  Foll_Spd_Var <- paste0("Forecast_Follower_", Prefix, "_IAS")
  Foll_WE_Var <- paste0("Forecast_Follower_", Prefix, "_Wind_Effect")
  
  # Type 1: Simple Predicted Distance Difference
  if (Metric_Type == 1){Landing_Pair <- mutate(Landing_Pair, !!sym(Comp_Var) := !!sym(Foll_Flying_Dist_Var) - !!sym(Lead_Flying_Dist_Var))}
  
  # Type 2: Predicted Leader Flying Distance & Forecast Leader/Follower Speeds/WEs
  if (Metric_Type == 2){Landing_Pair <- Landing_Pair %>%
    mutate(!!sym(Comp_Var) := (!!sym(Lead_Flying_Dist_Var) / (!!sym(Lead_Spd_Var) + !!sym(Lead_WE_Var))) * 
             ((!!sym(Foll_Spd_Var) + !!sym(Foll_WE_Var)) - (!!sym(Lead_Spd_Var) + !!sym(Lead_WE_Var))))}
  
  return(Landing_Pair)
  
}




#################################--#####################################################


# Generic Section 1 setting
Set_IAS_Profile_Section_1_PWS <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed,
                             End_IAS = Landing_Stabilisation_Speed,
                             Start_Dist = End_Final_Deceleration_Distance,
                             End_Dist = 0)
  return(Aircraft_Profile)
}


# Generic Section 2 Setting
Set_IAS_Profile_Section_2_PWS <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2",
                             Profile_Type = "A",
                             Start_IAS = Steady_Procedural_Speed,
                             End_IAS = Landing_Stabilisation_Speed,
                             Start_Dist = Start_Final_Deceleration_Distance,
                             End_Dist = End_Final_Deceleration_Distance)
  return(Aircraft_Profile)
}


Generate_Gust_Adjustment_Generic <- function(Aircraft_Profile){
  
  # Minimum Gust Adjustment Value.
  Min_Adjustment <- 0
  
  # 
  
  Aircraft_Profile <- Aircraft_Profile %>%
    mutate(Gust_Adjustment = -(Section_Wind_Effect + Surface_Headwind) / Gust_Coefficient) %>%
    mutate(Gust_Adjustment = ifelse(Gust_Adjustment < Min_Adjustment, Min_Adjustment, Gust_Adjustment)) %>%
    mutate(Gust_Adjustment = ifelse(is.na(Gust_Adjustment), 0, Gust_Adjustment))
  
  # Assumes Start Distance, End Distance, Wind_Effect, Gust Change Distance, Pre_Change_Coefficient, Post_Change_Coefficient
  
  
  
}


