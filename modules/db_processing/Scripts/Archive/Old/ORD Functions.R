# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ORD Functions
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: 
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


# ----------------------------------------------- #
# Adaptation Functions
# ----------------------------------------------- #
# Specific functions for building ORD Aircraft Profile.
# ----------------------------------------------- #

# Function to join the correct ORD adaptation
Join_ORD_Adaptation <- function(LP, ORD_Profile_Selection, ORD_AC, ORD_W, ORD_DBS){
  #LP <- left_join(LP, ORD_Runway_Reduced, by=c("Landing_Runway"="Runway_Name"))
  
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
Get_Compression_Distances <- function(LP, ORD_Profile_Selection, Distance){
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
  LP <- Join_ORD_Adaptation(LP, ORD_Profile_Selection, ORD_Aircraft_1, ORD_Wake_1, ORD_DBS_1)
  if (Distance == "LST"){LP <- select(LP, Landing_Pair_ID, Leader_Flight_Plan_ID, Local_Stabilisation_Distance)}
  if (Distance == "CCT"){LP <- select(LP, Landing_Pair_ID, Leader_Flight_Plan_ID, Compression_Commencement_Threshold)}
  
  return(LP)
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
Calculate_Landing_Stabilisation_Speed <- function(ORD_Aircraft_Profile){
  for (i in 0:12){
    Type_Profiles <- filter(ORD_Aircraft_Profile, Landing_Stabilisation_Speed_Type == i)
    Type_Profiles <- eval(parse(text = paste0("Calculate_LSS_Type_", i, "(Type_Profiles)")))
    if (i == 0){Full_Profile <- Type_Profiles} else {Full_Profile <- rbind(Full_Profile, Type_Profiles)}
  }
  Full_Profile <- Full_Profile[order(Full_Profile$Landing_Pair_ID),]
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

# Calculate Final Deceleration Distance.
Calculate_Final_Decel_Distance <- function(ORD_Aircraft_Profile){
  ORD_Aircraft_Profile <- mutate(ORD_Aircraft_Profile,
                                 Final_Deceleration_Distance = ifelse(Local_Stabilisation_Distance > (Thousand_Ft_Gate + (Steady_Procedural_Speed - Landing_Stabilisation_Speed) / Final_Deceleration),
                                                                      Local_Stabilisation_Distance,
                                                                      Thousand_Ft_Gate + (Steady_Procedural_Speed - Landing_Stabilisation_Speed) / Final_Deceleration))
  return(ORD_Aircraft_Profile)
}

# ----------------------------------------------- #
# Time/RTT Acquisition
# ----------------------------------------------- #
#
# ----------------------------------------------- #

# Calculate ORD Prediction Time: Do we want radar inside or outside?
Get_ORD_Prediction_Time <- function(Landing_Pair_Reference, Radar){
  Radar <- filter(Radar, !is.na(Path_Leg)) %>% select(-c("Path_Leg")) %>%
   group_by(Flight_Plan_ID) %>% summarise(Prediction_Time = min(Track_Time, na.rm=T)) %>% ungroup()
  Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Radar, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  return(Landing_Pair_Reference)
}

# Find the time the Follower joins the ILS (GWCS Currently, First NON NA Range To Threshold for airport)
Get_Follower_Join_Time <- function(Landing_Pair_Reference, Radar1){
  Radar1 <- group_by(Radar1, Flight_Plan_ID) %>% summarise(Follower_Join_Time = min(Track_Time, na.rm=T)) %>% ungroup()
  Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Radar1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  return(Landing_Pair_Reference)
}

# Calculate Surface Wind Parameters. Join_Time, Join_Date, Join_Runway need to 
# be specified before function application, and variables output should be renamed.
Get_Surface_Wind <- function(SW, LP){
  Runway_Reduced_2 <- select(Runway, Runway_Name, Heading)
  SW <- mutate(SW, Join_Time = Anemo_Time,
               Join_Date = Anemo_Date,
               Join_Runway = Landing_Runway)
  SW <- as.data.table(SW)
  LP <- as.data.table(LP)
  setkey(SW, Join_Runway, Join_Date, Join_Time)
  setkey(LP, Join_Runway, Join_Date, Join_Time)
  LP <- SW[LP, roll = Inf]
  LP <- as.data.frame(LP)
  LP <- left_join(LP, Runway_Reduced_2, by = c("Join_Runway" = "Runway_Name"))
  LP <- mutate(LP, Anemo_HW = Get_2D_Scalar_Product(Anemo_SPD, Anemo_HDG, 1, Heading))
  Wind_Parameters <- select(LP, Landing_Pair_ID, Anemo_SPD, Anemo_HDG, Anemo_HW)
  return(Wind_Parameters)
}

# Calculate Interpolated Follower RTTs. Leader_Times is a DF with the
# Follower Flight Plan ID and the relevant Leader time at a certain distance.
Get_Follower_Interpolated_RTTs <- function(Radar1, Leader_Times){
  
  # Order Radar by Descending Time
  Radar1 <- Order_Radar_Reverse(Radar1)
  
  Radar2 <- left_join(Radar1, Leader_Times, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID"))
  Radar2 <- mutate(Radar2, Flag = ifelse(Track_Time > Leader_Time, 1, 0)) %>% select(-c("Mode_S_IAS", "Wind_Effect_IAS"))
  Radar3 <- filter(Radar2, Flag == 0)
  Radar2 <- filter(Radar2, Flag == 1)
  Radar2 <- Radar2[order(Radar2$Flight_Plan_ID, Radar2$Track_Time),]
  Radar2 <- group_by(Radar2, Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% filter(ID == 1) %>% select(-c("Flag", "ID", "Leader_Time"))
  Radar2 <- rename(Radar2, Second_Track_Time = Track_Time, Second_Track_Distance = Range_To_Threshold)
  Radar3 <- group_by(Radar3, Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% filter(ID == 1) %>% select(-c("Flag", "ID"))
  Radar3 <- rename(Radar3, First_Track_Time = Track_Time, First_Track_Distance = Range_To_Threshold)
  Radar2 <- full_join(Radar3, Radar2, by = c("Flight_Plan_ID"))
  rm(Radar3)
  Radar2 <- mutate(Radar2, Distance_Delta = Second_Track_Distance - First_Track_Distance,
                   Time_Delta = Second_Track_Time - First_Track_Time,
                   Time_Delta_2 = Second_Track_Time - Leader_Time,
                   Time_Ratio = Time_Delta_2 / Time_Delta,
                   Interp_Distance = Second_Track_Distance - Time_Ratio*Distance_Delta) %>% select(Flight_Plan_ID, Interp_Distance)
  return(Radar2)
}

# Distances should include a Flight Plan ID, a Start Distance and End Distance
# This can be used for PM and ORD. Outputs Speed and WE Trapezium average
# across a distance window between start and end distance of Distances.
Get_Average_Observed_Mode_S_Parameters <- function(Radar, Times_Distances, Times_Or_Distances){
  
  # Join on the distances by Flight Plan ID
  Radar <- left_join(Radar, Times_Distances, by = c("Flight_Plan_ID"))
  
  # Filter for RTT within the Distance Bounds
  if (Times_Or_Distances == "Distances"){Radar <- filter(Radar, Range_To_Threshold >= End_Distance & Range_To_Threshold <= Start_Distance)}
  
  # Filter for Track_Time within the Time Bounds
  if (Times_Or_Distances == "Times"){Radar <- filter(Radar, Track_Time <= End_Time & Track_Time >= Start_Time)}
  
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
                                                         Count = n(),
                                                         Observed_Mean_IAS = sum(Observed_Mean_IAS, na.rm=T),
                                                         Observed_Mean_Wind_Effect = sum(Observed_Mean_Wind_Effect, na.rm=T)) %>% ungroup()
  
  # Divide the Observed sums by the Track Time delta to get the Trapezium rule average
  Radar <- mutate(Radar,
                  Observed_Mean_IAS = Observed_Mean_IAS / Total_Track_Time_Delta,
                  Observed_Mean_Wind_Effect = Observed_Mean_Wind_Effect / Total_Track_Time_Delta)
  
  # Return the Observed parameters.
  return(Radar)
  
}


# ----------------------------------------------- #
# IAS Profile Building
# ----------------------------------------------- #
# Gust Adjustment functions, parameter selection
# ----------------------------------------------- #

# -- Generic

# Generic Parameter Selection Function.
Select_IAS_Profile_Fields <- function(Aircraft_Profile){
  Aircraft_Profile <- select(Aircraft_Profile, 
                             Landing_Pair_ID,
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
Build_Full_IAS_Profile <- function(Aircraft_Profile){
  for (i in 0:12){
    Type_Profiles <- eval(parse(text = paste0("Build_IAS_Profile_Type_", i, "(Aircraft_Profile)")))
    if (i == 0){Full_Profile <- Type_Profiles} else {Full_Profile <- rbind(Full_Profile, Type_Profiles)}
  }
  Full_Profile <- Full_Profile[order(Full_Profile$Landing_Pair_ID),]
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
                             Start_Dist = Thousand_Ft_Gate,
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
                             Start_Dist = 1 * 1852,
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
                             End_Dist = 1 * 1852)
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
                             End_Dist = 1 * 1852)
  return(Aircraft_Profile)
}

# section 1b setting C - 1DME to 2DME
Set_IAS_Profile_Section_1b_C <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1b",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1b,
                             Start_Dist = 2 * 1852,
                             End_Dist = 1 * 1852)
  return(Aircraft_Profile)
}

# Section 1c Setting A - 4HF AAL - 2DME
Set_IAS_Profile_Section_1c_A <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1c",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             Start_Dist = 2 * 1852,
                             End_Dist = Four_Hundred_Ft_AAL)
}

# Section 1c Setting B - 6HF AAL - 2DME
Set_IAS_Profile_Section_1c_B <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1c",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1c,
                             Start_Dist = 2 * 1852,
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
                             Start_Dist = Thousand_Ft_Gate,
                             End_Dist = 2 * 1852)
  return(Aircraft_Profile)
}


# Section 1d (only present for types 1 and 2 but the same for both)
Set_IAS_Profile_Section_1d <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "1d",
                             Profile_Type = "C",
                             Start_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1d,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_1d,
                             Start_Dist = Thousand_Ft_Gate,
                             End_Dist = 2 * 1852)
  return(Aircraft_Profile)
}

# Generic Section 2 Setting
Set_IAS_Profile_Section_2 <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2",
                             Profile_Type = "A",
                             Start_IAS = Steady_Procedural_Speed,
                             End_IAS = Landing_Stabilisation_Speed,
                             Start_Dist = Final_Deceleration_Distance,
                             End_Dist = Thousand_Ft_Gate)
  return(Aircraft_Profile)
}

# Section 2a
Set_IAS_Profile_Section_2a <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2a",
                             Profile_Type = "A",
                             Start_IAS = IAS_3DME + Gust_Adjustment_2a,
                             End_IAS = Landing_Stabilisation_Speed + Gust_Adjustment_2a,
                             Start_Dist = 3 * 1852,
                             End_Dist = Thousand_Ft_Gate)
  return(Aircraft_Profile)
}

# Section 2b
Set_IAS_Profile_Section_2b <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2b",
                             Profile_Type = "A",
                             Start_IAS = IAS_4DME + Gust_Adjustment_2b,
                             End_IAS = IAS_3DME + Gust_Adjustment_2b,
                             Start_Dist = 4 * 1852,
                             End_Dist = 3 * 1852)
  return(Aircraft_Profile)
}

# Section 2c
Set_IAS_Profile_Section_2c <- function(Aircraft_Profile, Section_No){
  Aircraft_Profile <- mutate(Aircraft_Profile, Section_Number = Section_No,
                             Profile_Section = "2c",
                             Profile_Type = "A",
                             Start_IAS = Steady_Procedural_Speed + Gust_Adjustment_2c,
                             End_IAS = IAS_4DME + Gust_Adjustment_2c,
                             Start_Dist = Final_Deceleration_Distance,
                             End_Dist = 4 * 1852)
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
                             End_Dist = Final_Deceleration_Distance)
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
Build_IAS_Profile_Main <- function(Aircraft_Profile){
  
  IAS_Profile_1 <- Set_IAS_Profile_Section_1(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2 <- Set_IAS_Profile_Section_2(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields()
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields()
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields()
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields()
  
  IAS_Profile <- rbind(IAS_Profile_1, IAS_Profile_2) %>% rbind(IAS_Profile_3) %>% rbind(IAS_Profile_4) %>%
    rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

# Complex 
Build_IAS_Profile_A <- function(Aircraft_Profile){
  
  IAS_Profile_1a <- Set_IAS_Profile_Section_1a(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1b <- Set_IAS_Profile_Section_1b_A(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1c <- Set_IAS_Profile_Section_1c_A(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1d <- Set_IAS_Profile_Section_1d(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2a <- Set_IAS_Profile_Section_2a(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2b <- Set_IAS_Profile_Section_2b(Aircraft_Profile, 6) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2c <- Set_IAS_Profile_Section_2c(Aircraft_Profile, 7) %>% Select_IAS_Profile_Fields()
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 8) %>% Select_IAS_Profile_Fields()
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 9) %>% Select_IAS_Profile_Fields()
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 10) %>% Select_IAS_Profile_Fields()
  
  IAS_Profile <- rbind(IAS_Profile_1a, IAS_Profile_1b) %>% rbind(IAS_Profile_1c) %>% rbind(IAS_Profile_1d) %>%
    rbind(IAS_Profile_2a) %>% rbind(IAS_Profile_2b) %>% rbind(IAS_Profile_2c) %>% rbind(IAS_Profile_3) %>%
    rbind(IAS_Profile_4) %>% rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

Build_IAS_Profile_B <- function(Aircraft_Profile){
  
  IAS_Profile_1a <- Set_IAS_Profile_Section_1a(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1b <- Set_IAS_Profile_Section_1b_B(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1c <- Set_IAS_Profile_Section_1c_B(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1d <- Set_IAS_Profile_Section_1d(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2a <- Set_IAS_Profile_Section_2a(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2b <- Set_IAS_Profile_Section_2b(Aircraft_Profile, 6) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2c <- Set_IAS_Profile_Section_2c(Aircraft_Profile, 7) %>% Select_IAS_Profile_Fields()
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 8) %>% Select_IAS_Profile_Fields()
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 9) %>% Select_IAS_Profile_Fields()
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 10) %>% Select_IAS_Profile_Fields()
  
  IAS_Profile <- rbind(IAS_Profile_1a, IAS_Profile_1b) %>% rbind(IAS_Profile_1c) %>% rbind(IAS_Profile_1d) %>%
    rbind(IAS_Profile_2a) %>% rbind(IAS_Profile_2b) %>% rbind(IAS_Profile_2c) %>% rbind(IAS_Profile_3) %>%
    rbind(IAS_Profile_4) %>% rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

Build_IAS_Profile_C <- function(Aircraft_Profile){
  
  IAS_Profile_1a <- Set_IAS_Profile_Section_1a(Aircraft_Profile, 1) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1b <- Set_IAS_Profile_Section_1b_C(Aircraft_Profile, 2) %>% Select_IAS_Profile_Fields()
  IAS_Profile_1c <- Set_IAS_Profile_Section_1c_C(Aircraft_Profile, 3) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2a <- Set_IAS_Profile_Section_2a(Aircraft_Profile, 4) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2b <- Set_IAS_Profile_Section_2b(Aircraft_Profile, 5) %>% Select_IAS_Profile_Fields()
  IAS_Profile_2c <- Set_IAS_Profile_Section_2c(Aircraft_Profile, 6) %>% Select_IAS_Profile_Fields()
  IAS_Profile_3 <- Set_IAS_Profile_Section_3(Aircraft_Profile, 7) %>% Select_IAS_Profile_Fields()
  IAS_Profile_4 <- Set_IAS_Profile_Section_4(Aircraft_Profile, 8) %>% Select_IAS_Profile_Fields()
  IAS_Profile_5 <- Set_IAS_Profile_Section_5(Aircraft_Profile, 9) %>% Select_IAS_Profile_Fields()
  
  IAS_Profile <- rbind(IAS_Profile_1a, IAS_Profile_1b) %>% rbind(IAS_Profile_1c) %>% 
    rbind(IAS_Profile_2a) %>% rbind(IAS_Profile_2b) %>% rbind(IAS_Profile_2c) %>% rbind(IAS_Profile_3) %>%
    rbind(IAS_Profile_4) %>% rbind(IAS_Profile_5)
  
  return(IAS_Profile)
  
}

# ----------------------------------------------- #
# Profile Building by LSS Type
# ----------------------------------------------- #

Build_IAS_Profile_Type_0 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 0)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_1 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 1)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_A(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_A(Aircraft_Profile_b)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_2 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 2)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_A(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_B(Aircraft_Profile_b)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_3 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 3)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_B(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_C(Aircraft_Profile_b)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_4 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 4)
  Aircraft_Profile_a <- filter(Aircraft_Profile, Apply_Gusting == 0)
  IAS_Profile_a <- Build_IAS_Profile_Main(Aircraft_Profile_a)
  Aircraft_Profile_b <- filter(Aircraft_Profile, Apply_Gusting == 1)
  Aircraft_Profile_b <- Generate_Gust_Adjustments_C(Aircraft_Profile_b)
  IAS_Profile_b <- Build_IAS_Profile_C(Aircraft_Profile_b)
  IAS_Profile <- rbind(IAS_Profile_a, IAS_Profile_b) 
  return(IAS_Profile)
}

Build_IAS_Profile_Type_5 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 5)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_6 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 6)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_7 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 7)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_8 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 8)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_9 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 9)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_10 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 10)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_11 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 11)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

Build_IAS_Profile_Type_12 <- function(Aircraft_Profile){
  Aircraft_Profile <- filter(Aircraft_Profile, Landing_Stabilisation_Speed_Type == 12)
  IAS_Profile <- Build_IAS_Profile_Main(Aircraft_Profile)
  return(IAS_Profile)
}

# ----------------------------------------------- #
# GSPD Profile Building
# ----------------------------------------------- #
# Single function to build GS Profile for either L/F
# ----------------------------------------------- #


Build_GSPD_Profile <- function(IAS_Profile, ORD_Segments){
  
  # Find all the unique profile sections 
  Sections <- unique(IAS_Profile$Profile_Section)
  
  # Remove all IAS Sections with Start_Dist < End_Dist (Bug fix 2)
  IAS_Profile <- filter(IAS_Profile, Start_Dist >= End_Dist)
  
  # Open loop for each profile section
  for (i in 1:length(Sections)){
    
    # Filter IAS Profile for this profile section only and select relevant fields
    IAS_Section <- filter(IAS_Profile, Profile_Section == Sections[i]) %>% select(Landing_Pair_ID, This_Pair_Role, Profile_Section, Profile_Type, End_Dist, Start_Dist, End_IAS, Start_IAS)
    
    # Join on this Section Parameters onto Wind Segments by Landing Pair, Select relevant fields.
    ORD_Section_Segments <- inner_join(ORD_Segments, IAS_Section, by = c("ID" = "Landing_Pair_ID"))
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
                                                                   End_GS, Start_Dist, End_Dist) %>% rename(Landing_Pair_ID = ID)
  
  return(GS_Complete)
}



# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #


  
  
  
  
  
  