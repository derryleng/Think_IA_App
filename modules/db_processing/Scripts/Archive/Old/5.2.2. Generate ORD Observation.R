# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.2. ORD/WAD Observations
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Uses tbl_All_Pair_Reference_Data, tbl_ORD_Aircraft_Profile and a Radar Track ~Point/Derived/Flight Plan hybrid
#              to find the RTT's/Times at the first track point above Delivery Point and FAF (ORD) and the second track point
#              above FAF - to not double count any track points - and the first point above CCT - if not the furthest point 
#              where Follower has joined the ILS (WAD). Mean Leader/Follower IAS/WE across these sections are calculated too.
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

# Updates Required/Theorized
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# - !!!Are we using Runway FAF or ORD LST??
# - Save to SQL/Local
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Observed 0DME, 1DME, 4DME Separation Distance
Get_Time_At_Fixed_RTT <- function(Radar, RTT){
  
  # Order Radar by Range To Threshold
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Range_To_Threshold),]
  
  # Create flag for RTT above delivery point
  Radar2 <- mutate(Radar, Flag = ifelse(Range_To_Threshold >= RTT, 1, 0))
  
  # Filter results above delivery point
  Radar2 <- filter(Radar2, Flag == 1) %>% select(-c("Flag"))
  
  # Get the top result for each Flight Plan ID (a.k.a. closest to DME)
  Radar2 <- group_by(Radar2, Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% 
    filter(ID == 1) %>% select(Flight_Plan_ID, Track_Time, Range_To_Threshold)
  
  return(Radar2)
  
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

Get_FAF_Times_RTTs <- function(LPR, LSTs, Radar, WAD_Enabled){
  
  # Order Radar by Range To Threshold
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Range_To_Threshold),]
  
  # Filter for Same LPIDs.
  LSTs <- filter(LSTs, Landing_Pair_ID %in% LPR$Landing_Pair_ID) %>% select(-c("Landing_Pair_ID"))
  
  # Join on to Radar. Should be done for only NIT/IT pairs as such Leader FPIDs are unique.
  Radar <- left_join(Radar, LSTs, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID"))
  
  # Create >LST Flag, remove all below, and get row number.
  Radar <- mutate(Radar, LST_Flag = ifelse(Range_To_Threshold >= Local_Stabilisation_Distance, 1, 0)) %>%
    filter(LST_Flag == 1) %>% select(-c("LST_Flag")) %>% 
    group_by(Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup()
  
  # Filter for Row Number = 1 for the first point above LST (ORD), Row number = 2 for 2nd above LST (WAD) 
  Radar1 <- filter(Radar, ID == 1) %>% select(Flight_Plan_ID, Track_Time, Range_To_Threshold)
  Radar2 <- filter(Radar, ID == 2) %>% select(Flight_Plan_ID, Track_Time, Range_To_Threshold)
  
  # Rename Parameters. 
  Radar1 <- rename(Radar1, Leader_ORD_FAF_Time = Track_Time, Leader_ORD_FAF_RTT = Range_To_Threshold)
  Radar2 <- rename(Radar2, Leader_WAD_FAF_Time = Track_Time, Leader_WAD_FAF_RTT = Range_To_Threshold)
  
  # Join Together if WAD Enabled.
  if (WAD_Enabled) {Radar1 <- left_join(Radar1, Radar2, by = c("Flight_Plan_ID"))}
  
  # Join to Landing Pair Reference Data.
  LPR <- left_join(LPR, Radar1, by =c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  return(LPR)
}


# Leader CC WAD Distance/Time
# NOTE: NEED TO FIND FOLLOWER ILS JOIN TIME AND USE THIS INSTEAD!!!!

Get_CC_Times_RTTs <- function(LPR, CCTs, Radar){
  
  # --- Compression Commencement Threshold Calcs
  
  # Order Radar by Flight Plan ID in Reverse Time.
  Radar <- Order_Radar_Reverse(Radar)
  
  # Filter CCTs for Same LPIDs.
  CCTs <- filter(CCTs, Landing_Pair_ID %in% LPR$Landing_Pair_ID) %>% select(-c("Landing_Pair_ID"))
  
  # Join on to Radar. Should be done for only NIT/IT pairs as such Leader FPIDs are unique.
  Radar <- left_join(Radar, CCTs, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID"))
  
  # Get Leader CCT Time/RTT.
  CCT_Parameters <- mutate(Radar, CC_Flag = ifelse(Range_To_Threshold >= Compression_Commencement_Threshold, 1, 0)) %>%
    filter(CC_Flag == 1) %>% select(-c("CC_Flag")) %>% group_by(Flight_Plan_ID) %>% mutate(ID = row_number()) %>%
    ungroup() %>% filter(Radar2, ID == 1) %>% select(Flight_Plan_ID, Track_Time, Range_To_Threshold) %>%
    rename(Leader_CCT_Time = Track_Time, Leader_CCT_RTT = Range_To_Threshold)
  
  # Remove CCT and Inverse_Time from Radar.
  Radar <- select(Radar, -c("Compression_Commencement_Threshold", "Inverse_Time"))
  
  # Join on the CCT Parameters (Leader_CCT_Time, Leader_CCT_RTT)
  LPR <- left_join(LPR, CCT_Parameters, by =c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  # --- Follower ILS Join Time Calcs
  
  # Order Radar In Ascending Track Time - We want FIRST point after follower joins ILS.
  Radar <- Order_Radar(Radar)

  # Get the Follower ILS Join Parameters (Time and RTT)
  Follower_ILS_Join <- select(LPR, Leader_Flight_Plan_ID, Follower_ILS_Time)
  
  # Join on the Follower ILS Join Time to Radar.
  Radar <- left_join(Radar, Follower_ILS_Join, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID"))
  
  # Get Leader time/RTT when Follower Joins ILS.
  FJ_Parameters <- filter(Radar, Track_Time >= Follower_ILS_Time) %>%
    group_by(Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% filter(ID == 1) %>%
    select(Flight_Plan_ID, Track_Time, Range_To_Threshold) %>% rename(Leader_FJ_Time = Track_Time, Leader_FJ_RTT = Range_To_Threshold)
  
  # Join on FJ Parameters (Leader_FJ_Time, Leader_FJ_RTT)
  LPR <- left_join(LPR, FJ_Parameters,by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  # --- Choosing the Correct CC/Tidying
  
  # We want the Compression Commencement Threshold values if they're available. If not, take FJ.
  LPR <- mutate(LPR, 
                Leader_CC_Time = ifelse(is.na(Leader_CCT_RTT), Leader_FJ_Time, Leader_CCT_Time),
                Leader_CC_RTT = ifelse(is.na(Leader_CCT_RTT), Leader_FJ_RTT, Leader_CCT_RTT),
                Leader_CCT_Used = ifelse(is.na(Leader_CCT_RTT), 0, 1))
  
  # Remove unwanted variables from LPR
  LPR <- select(LPR, -c("Leader_CCT_RTT", "Leader_CCT_Time", "Leader_FJ_Time", "Leader_FJ_RTT"))
  
  return(LPR)
  
}

Get_Interpolated_ORD_RTTs <- function(LPR, Radar){
  
  Leader_Times_Delivery <- select(LPR, c("Follower_Flight_Plan_ID", "Leader_Delivery_Time")) %>% 
    rename(Leader_Time = Leader_Delivery_Time)
  Leader_Times_ORD_FAF <- select(LPR, c("Follower_Flight_Plan_ID", "Leader_ORD_FAF_Time"))%>% 
    rename(Leader_Time = Leader_ORD_FAF_Time)
  
  RTT1 <- Get_Follower_Interpolated_RTTs(Radar, Leader_Times_Delivery) %>% rename(Follower_ORD_Stop_RTT = Interp_Distance)
  LPR <- left_join(LPR, RTT1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  RTT2 <- Get_Follower_Interpolated_RTTs(Radar, Leader_Times_ORD_FAF) %>% rename(Follower_ORD_Start_RTT = Interp_Distance)
  LPR <- left_join(LPR, RTT2, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  return(LPR)
}

Get_Interpolated_WAD_RTTs <- function(LPR, Radar){
  
  Leader_Times_WAD_FAF <- select(LPR, c("Follower_Flight_Plan_ID", "Leader_WAD_FAF_Time")) %>% 
    rename(Leader_Time = Leader_WAD_FAF_Time)
  Leader_Times_CC <- select(LPR, c("Follower_Flight_Plan_ID", "Leader_CC_Time"))%>% 
    rename(Leader_Time = Leader_CC_Time)
  
  RTT1 <- Get_Follower_Interpolated_RTTs(Radar, Leader_Times_WAD_FAF) %>% rename(Follower_WAD_Stop_RTT = Interp_Distance)
  LPR <- left_join(LPR, RTT1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  RTT2 <- Get_Follower_Interpolated_RTTs(Radar, Leader_Times_CC) %>% rename(Follower_WAD_Start_RTT = Interp_Distance)
  LPR <- left_join(LPR, RTT2, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  return(LPR)
}

# ----------------------------------------------- #
# X.X.X Load in Data
# ----------------------------------------------- #
# First attempt at observed parameters.
# ----------------------------------------------- #

# Make a copy of Landing Pair Reference from 4.2.1
#LPR <- Landing_Pair_Reference

# Get the Compression Commencement Thresholds
CCTs <- Get_Compression_Distances(Landing_Pair_Reference, ORD_Profile_Selection, "CCT")

# Get the Local Stabilisation Thresholds
LSTs <- Get_Compression_Distances(Landing_Pair_Reference, ORD_Profile_Selection, "LST")
LSTs <- mutate(LSTs, Local_Stabilisation_Distance = 4 * NM_to_m)

#Landing_Pair_Reference <- left_join(Landing_Pair_Reference, CCTs, by = c("Landing_Pair_ID"))
#Landing_Pair_Reference <- left_join(Landing_Pair_Reference, LSTs, by = c("Landing_Pair_ID"))


# This should be done, but currently isn't.
# --------------------------------------------------------------------------------------------- #

# Load the Flight Plan Runway. Only data with the same Runway/Localiser pair considered.
#Flight_Plan_Reduced <- select(Flight_Plan, Flight_Plan_ID, Landing_Runway)

# Join on the Flight Plan Runway for filtering later as above.
#Radar <- left_join(Radar, Flight_Plan_Reduced, by = c("Flight_Plan_ID"))

# Remove Radar Points where Mode S Wind Localiser Capture is not Equal to Landing Runway
#Radar <- filter(Radar, Landing_Runway == Mode_S_Wind_Localiser_Capture)

# Remove Flight Plan Runway from Dataset
#Radar <- select(Radar, -c("Landing_Runway)) 

# Remove Flight Plan Reduced Data
#rm(Flight_Plan_Reduced)

# --------------------------------------------------------------------------------------------- #

# ----------------------------------------------- #
# X.X.X Filtering
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Reload the original Reference Pair Data
#Landing_Pair_Reference <- LPR

# Filter out Pairs based on Adaptation Configuration (In-Trail-Only? Wake Only?)
#if (In_Trail_Only){Landing_Pair_Reference <- filter(Landing_Pair_Reference, Landing_Pair_Type != "Not_In_Trail")}
#if (Wake_Pairs_Only){Landing_Pair_Reference <- filter(Landing_Pair_Reference, !is.na(Reference_Wake_Separation_Distance))}

# Filter Landing Pairs for no go-arounds
Landing_Pair_Reference <- filter(Landing_Pair_Reference, Go_Around_Flag == 0)

# Remove Aircraft Types not 
if (ORD_Profile_Selection == "Aircraft_Type"){
  Landing_Pair_Reference <- filter(Landing_Pair_Reference, Leader_Aircraft_Type %in% ORD_Aircraft$Aircraft_Type | Follower_Aircraft_Type %in% ORD_Aircraft$Aircraft_Type)
  #Landing_Pair_Reference <- filter(Landing_Pair_Reference, Follower_Aircraft_Type %in% ORD_Aircraft$Aircraft_Type)
}

# Filter out all points not on ILS (No Range to Threshold)
#Radar <- filter(Radar, !is.na(Range_To_Threshold))

# Due to using Data.Frames for space constraints, Ordering cannot be done descending - so make inverse time field for desc ordering
Radar <- Order_Radar_Reverse(Radar)

# ----------------------------------------------- #
# X.X.X Follower Join Time & Leader Delivery Time
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Get Follower Join Time - we need ascending time order to choose first point. Used for WAD but could be useful.
Radar1 <- Order_Radar(Radar)
Radar1 <- group_by(Radar1, Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% 
  filter(ID == 1) %>% select(Flight_Plan_ID, Follower_ILS_Time = Track_Time, Follower_ILS_RTT = Range_To_Threshold)

# Join on Follower Join Time
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Radar1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))

# Get the Times/RTTs closest to New Delivery Point (0DME currently)
Delivery_Time_RTT <- Get_Time_At_Fixed_RTT(Radar, New_Delivery) 

# Join on Leader Delivery Time/RTT
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Delivery_Time_RTT, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
  rename(Leader_Delivery_RTT = Range_To_Threshold,
         Leader_Delivery_Time = Track_Time)

# Remove Delivery_Time_RTT and Radar1
rm(Delivery_Time_RTT, Radar1)

# ----------------------------------------------- #
# X.X.X Leader FAF / CC Times 
# ----------------------------------------------- #
# ORD Start & WAD End
# ----------------------------------------------- #

Get_Rough_Follower_Stop_Time <- function(LPR, Radar){
  
  # Order Radar by Range To Threshold
  Radar <- Order_Radar(Radar)
  
  # Filter for Same LPIDs.
  Leader_0DME_Times <- select(LPR, Follower_Flight_Plan_ID, Leader_Delivery_Time)
  
  # Join on to Radar. Should be done for only NIT/IT pairs as such Leader FPIDs are unique.
  Radar <- left_join(Radar, Leader_0DME_Times, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID"))
  
  # Create >LST Flag, remove all below, and get row number.
  Radar <- mutate(Radar, Flag = ifelse(Track_Time >= Leader_Delivery_Time, 1, 0)) %>%
    filter(Flag == 1) %>% select(-c("Flag")) %>% 
    group_by(Flight_Plan_ID) %>% mutate(ID = row_number()) %>% ungroup()
  
  # Filter for Row Number = 1 for the first point above LST (ORD), Row number = 2 for 2nd above LST (WAD) 
  Radar1 <- filter(Radar, ID == 1) %>% select(Flight_Plan_ID, Range_To_Threshold)
  
  # Rename Parameters. 
  Radar1 <- rename(Radar1, Follower_Rough_Stop_RTT = Range_To_Threshold)
  
  # Join
  LPR <- left_join(LPR, Radar1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  return(LPR)
}

# -- Split up Landing Pairs into In-Trail and Not-In-Trail for the next bits
LPR_In_Trail <- filter(Landing_Pair_Reference, Landing_Pair_Type != "Not_In_Trail")
LPR_Not_In_Trail <- filter(Landing_Pair_Reference, Landing_Pair_Type == "Not_In_Trail")


# Get ORD FAF Time/RTT, WAD FAF Time/RTT if WAD Enabled, and Follower Interpolated Delivery/ORD FAF RTTs. (In-Trail)
LPR_In_Trail <- Get_FAF_Times_RTTs(LPR_In_Trail, LSTs, Radar, WAD_Enabled)
LPR_In_Trail <- Get_Rough_Follower_Stop_Time(LPR_In_Trail, Radar)
LPR_In_Trail <- Get_Interpolated_ORD_RTTs(LPR_In_Trail, Radar)

# Get ORD FAF Time/RTT, WAD FAF Time/RTT if WAD Enabled, and Follower Interpolated Delivery/ORD FAF RTTs. (Not-In-Trail)

  LPR_Not_In_Trail <- Get_FAF_Times_RTTs(LPR_Not_In_Trail, LSTs, Radar, WAD_Enabled)
  LPR_Not_In_Trail <- Get_Rough_Follower_Stop_Time(LPR_Not_In_Trail, Radar)
  LPR_Not_In_Trail <- Get_Interpolated_ORD_RTTs(LPR_Not_In_Trail, Radar)



if (WAD_Enabled){
  
  # Get CC Times for In-Trail Pairs
  LPR_In_Trail <- Get_CC_Times_RTTs(LPR_In_Trail, CCTs, Radar)
  LPR_In_Trail <- Get_Interpolated_WAD_RTTs(LPR_In_Trail, Radar)
  
  # Get CC Times and Interpolated Follower times for Not-In_Trail Pairs if there are any

    LPR_Not_In_Trail <- Get_CC_Times_RTTs(LPR_Not_In_Trail, CCTs, Radar)
    LPR_Not_In_Trail <- Get_Interpolated_WAD_RTTs(LPR_Not_In_Trail, Radar)
  
  
}

# Bind all Landing Pairs back together.
#if (!In_Trail_Only) {Landing_Pair_Reference <- rbind(LPR_In_Trail, LPR_Not_In_Trail)} else {Landing_Pair_Reference <- LPR_In_Trail}
Landing_Pair_Reference <- rbind(LPR_In_Trail, LPR_Not_In_Trail)

# TEMP: Substitute NA for FAF & CC Times/RTTs if using Non-ORD Supported Runway
Landing_Pair_Reference <- mutate(Landing_Pair_Reference,
                                 Leader_ORD_FAF_Time = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_ORD_FAF_Time, NA),
                                 Leader_ORD_FAF_RTT = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_ORD_FAF_RTT, NA))

if (WAD_Enabled){
  Landing_Pair_Reference <- mutate(Landing_Pair_Reference,
                                   Leader_WAD_FAF_Time = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_WAD_FAF_Time, NA),
                                   Leader_WAD_FAF_RTT = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_WAD_FAF_RTT, NA),
                                   Leader_CC_RTT = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_CC_RTT, NA),
                                   Leader_CC_Time = ifelse(Leader_Landing_Runway %in% ORD_Runway$Runway_Name, Leader_CC_Time, NA))
}

# Order Pairs by Landing Pair ID.
Landing_Pair_Reference <- Landing_Pair_Reference[order(Landing_Pair_Reference$Landing_Pair_ID),]

# Remove In-Trail and Not-In_Trail segments.
rm(LPR_Not_In_Trail, LPR_In_Trail)

# ----------------------------------------------- #
# X.X.X Calculate Observed Compression
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Get ORD Compression.
Landing_Pair_Reference <- mutate(Landing_Pair_Reference,
                                 Observed_ORD_Compression = (Follower_ORD_Start_RTT - Follower_ORD_Stop_RTT) - (Leader_ORD_FAF_RTT - Leader_Delivery_RTT),
                                 Observed_Delivery_Separation = Follower_ORD_Stop_RTT - Leader_Delivery_RTT,
                                 ORD_Separation_Distance = Follower_Rough_Stop_RTT)

# If WAD Enabled, Get WAD Compression.
if (WAD_Enabled){
Landing_Pair_Reference <- mutate(Landing_Pair_Reference,
                                 Observed_WAD_Compression = (Follower_WAD_Start_RTT - Follower_WAD_Stop_RTT) - (Leader_CC_RTT - Leader_WAD_FAF_RTT))
}

# ----------------------------------------------- #
# X.X.X Calculate Observed Speeds
# ----------------------------------------------- #
# # Leader/Follower Mean ORD/WAD IAS (4 total)
# ----------------------------------------------- #

# Leader ORD Observed IAS/WE Values. Uses Leader Flight Plan ID, Leader_ORD_FAF_RTT and Delivery Point.
Leader_ORD_Observed <- select(Landing_Pair_Reference, 
                        Flight_Plan_ID = Leader_Flight_Plan_ID, 
                        Start_Time = Leader_ORD_FAF_Time,
                        End_Time = Leader_Delivery_Time)

Leader_ORD_Observed <- Get_Average_Observed_Mode_S_Parameters(Radar, Leader_ORD_Observed, "Times") %>% 
  rename(Observed_Leader_ORD_Wind_Effect = Observed_Mean_Wind_Effect,
         Observed_Leader_ORD_IAS = Observed_Mean_IAS,
         Leader_Av_Count = Count)

# Follower ORD Observed IAS/WE Values. Uses Follower Flight Plan ID, Follower_ORD_Start_RTT and Follower_ORD_Stop_RTT.
Landing_Pair_Reference <- filter(Landing_Pair_Reference, Landing_Pair_Type != "Not_In_Trail")
Follower_ORD_Observed <- select(Landing_Pair_Reference, 
                                Flight_Plan_ID = Follower_Flight_Plan_ID, 
                                Start_Time = Leader_ORD_FAF_Time,
                                End_Time = Leader_Delivery_Time)

Follower_ORD_Observed <- Get_Average_Observed_Mode_S_Parameters(Radar, Follower_ORD_Observed, "Times") %>% 
  rename(Observed_Follower_ORD_Wind_Effect = Observed_Mean_Wind_Effect,
         Observed_Follower_ORD_IAS = Observed_Mean_IAS,
         Follower_Av_Count = Count)

if (WAD_Enabled){
# Leader WAD Observed IAS/WE Values. Uses Leader Flight Plan ID, Leader_CC_RTT and Leader_WAD_FAF_RTT.
Leader_WAD_Observed <- select(Landing_Pair_Reference, 
                              Flight_Plan_ID = Leader_Flight_Plan_ID, 
                              Start_Distance = Leader_CC_RTT,
                              End_Distance = Leader_WAD_FAF_RTT)

Leader_WAD_Observed <- Get_Average_Observed_Mode_S_Parameters(Radar, Leader_WAD_Observed, "Times") %>% 
  rename(Observed_Leader_WAD_Wind_Effect = Observed_Mean_Wind_Effect,
         Observed_Leader_WAD_IAS = Observed_Mean_IAS)

# Follower ORD Observed IAS/WE Values. Uses Follower Flight Plan ID, Follower_WAD_Start_RTT and Follower_WAD_Stop_RTT.
Follower_WAD_Observed <- select(Landing_Pair_Reference, 
                                Flight_Plan_ID = Follower_Flight_Plan_ID, 
                                Start_Distance = Follower_WAD_Start_RTT,
                                End_Distance = Follower_WAD_Stop_RTT)

Follower_WAD_Observed <- Get_Average_Observed_Mode_S_Parameters(Radar, Follower_WAD_Observed, "Times") %>% 
  rename(Observed_Follower_WAD_Wind_Effect = Observed_Mean_Wind_Effect,
         Observed_Follower_WAD_IAS = Observed_Mean_IAS)
}

# Join on Observed Values
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Leader_ORD_Observed, by=c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Follower_ORD_Observed, by=c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))

if (WAD_Enabled){
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Leader_WAD_Observed, by=c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Follower_WAD_Observed, by=c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
}

# Remove Observed ORD/WAD Speed Data
rm(Leader_ORD_Observed, Follower_ORD_Observed)
if (WAD_Enabled) {rm(Leader_WAD_Observed, Follower_WAD_Observed)}

# ----------------------------------------------- #
# X.X.X Get Observed Surface Wind
# ----------------------------------------------- #
# Function found in ORD Functions.
# ----------------------------------------------- #

# -- Get ORD Observed Surface Wind - Based on Leader FP Time.

# Add in The join columms used by the Get_Surface_Wind Function.
Landing_Pair_Reference <- mutate(Landing_Pair_Reference,
                                 Join_Time = Leader_Delivery_Time,
                                 Join_Date = Landing_Pair_Date,
                                 Join_Runway = Leader_Landing_Runway)

# Perform function and rename variables to observed SW,
Observed_Surface_Wind <- Get_Surface_Wind(Surface_Wind, Landing_Pair_Reference)
Observed_Surface_Wind <- rename(Observed_Surface_Wind,
                                Observed_AGI_Surface_Wind_SPD = Anemo_SPD,
                                Observed_AGI_Surface_Wind_HDG = Anemo_HDG,
                                Observed_AGI_Surface_Headwind = Anemo_HW)

# Join onto LPR and remove join columns.
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, Observed_Surface_Wind, by = c("Landing_Pair_ID"))
Landing_Pair_Reference <- select(Landing_Pair_Reference, -c("Join_Time", "Join_Date", "Join_Runway"))

# ----------------------------------------------- #
# X.X.X Tidying
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Remove LPR.
rm(LPR, LSTs, CCTs, Observed_Surface_Wind)

# ----------------------------------------------- #
# X.X.X Testing
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

if (Test_Mode){
  
  OO_Query <- "SELECT
  OO.Landing_Pair_ID,
  OO.Observed_Compression AS SQL_Observed_Compression,
  OO.Observed_Mean_Leader_IAS AS SQL_Observed_Mean_Leader_ORD_IAS,
  OO.Observed_Mean_Follower_IAS AS SQL_Observed_Mean_Follower_ORD_IAS,
  OO.Observed_Mean_Leader_Wind_Effect AS SQL_Observed_Mean_Leader_ORD_Wind_Effect,
  OO.Observed_Mean_Follower_Wind_Effect AS SQL_Observed_Mean_Follower_ORD_Wind_Effect,
  OO.Observed_AGI_Surface_Headwind AS SQL_Observed_Surface_Headwind,
  OO.Leader_FAF_Time AS SQL_Leader_ORD_FAF_Time,
  OO.Leader_0DME_Time AS SQL_Leader_Delivery_Time,
  OO.Leader_FAF_RTT AS SQL_Leader_ORD_FAF_RTT,
  OO.Leader_0DME_RTT AS SQL_Leader_Delivery_RTT,
  OO.Follower_Start_RTT AS SQL_Follower_ORD_Start_RTT,
  OO.Follower_Stop_RTT AS SQL_Follower_ORD_Stop_RTT
  FROM tbl_ORD_Observation OO"
  
  if (Processing_Period == "Day"){
    OO_Query <- paste0(OO_Query, " WHERE OO.Observation_Date = '", Processing_Date, "'")
  }
  
  if (Processing_Period == "Month"){
    OO_Query <- paste0(OO_Query, " WHERE OO.Observation_Date LIKE '%", Processing_Month, "%'")
  }
  
  ORD_Observation_SQL <- sqlQuery(con, OO_Query, stringsAsFactors = F) %>%
    mutate(SQL_Observed_Mean_Leader_ORD_IAS = ifelse(SQL_Observed_Mean_Leader_ORD_IAS == 0, NA, SQL_Observed_Mean_Leader_ORD_IAS),
           SQL_Observed_Mean_Follower_ORD_IAS = ifelse(SQL_Observed_Mean_Follower_ORD_IAS == 0, NA, SQL_Observed_Mean_Follower_ORD_IAS),
           SQL_Observed_Mean_Leader_ORD_Wind_Effect = ifelse(SQL_Observed_Mean_Leader_ORD_Wind_Effect == 0, NA, SQL_Observed_Mean_Leader_ORD_Wind_Effect),
           SQL_Observed_Mean_Follower_ORD_Wind_Effect = ifelse(SQL_Observed_Mean_Follower_ORD_Wind_Effect == 0, NA, SQL_Observed_Mean_Follower_ORD_Wind_Effect))
  
  ORD_Observation_R <- select(Landing_Pair_Reference,
                              Landing_Pair_ID,
                              Landing_Pair_Date,
                              Leader_Aircraft_Type,
                              Leader_Landing_Runway,
                              Follower_Aircraft_Type,
                              Leader_Flight_Plan_ID,
                              Follower_Flight_Plan_ID,
                              Observed_ORD_Compression,
                              Observed_Leader_ORD_IAS,
                              Observed_Follower_ORD_IAS,
                              Observed_Leader_ORD_Wind_Effect,
                              Observed_Follower_ORD_Wind_Effect,
                              Observed_AGI_Surface_Headwind,
                              Leader_ORD_FAF_Time,
                              Leader_Delivery_Time,
                              Leader_ORD_FAF_RTT,
                              Leader_Delivery_RTT,
                              Follower_ORD_Start_RTT,
                              Follower_ORD_Stop_RTT,
                              Leader_Av_Count,
                              Follower_Av_Count
                              )
  
  ZCOMP_ORD_Observation <- full_join(ORD_Observation_SQL, ORD_Observation_R, by = c("Landing_Pair_ID"))
  
  ZCOMP_ORD_Observation <- mutate(ZCOMP_ORD_Observation,
                                  DIFF_Observed_ORD_Compression = abs(Observed_ORD_Compression - SQL_Observed_Compression),
                                  DIFF_Observed_Leader_ORD_IAS = abs(Observed_Leader_ORD_IAS - SQL_Observed_Mean_Leader_ORD_IAS),
                                  DIFF_Observed_Follower_ORD_IAS = abs(Observed_Follower_ORD_IAS - SQL_Observed_Mean_Follower_ORD_IAS),
                                  DIFF_Observed_Leader_ORD_WE = abs(SQL_Observed_Mean_Leader_ORD_Wind_Effect - Observed_Leader_ORD_Wind_Effect),
                                  DIFF_Observed_Follower_ORD_WE = abs(SQL_Observed_Mean_Follower_ORD_Wind_Effect - Observed_Follower_ORD_Wind_Effect),
                                  DIFF_Observed_Surface_Headwind = abs(Observed_AGI_Surface_Headwind - SQL_Observed_Surface_Headwind),
                                  DIFF_Leader_ORD_FAF_Time = abs(Leader_ORD_FAF_Time - SQL_Leader_ORD_FAF_Time),
                                  DIFF_Leader_Delivery_Time = abs(Leader_Delivery_Time - SQL_Leader_Delivery_Time),
                                  DIFF_Leader_ORD_FAF_RTT = abs(Leader_ORD_FAF_RTT - SQL_Leader_ORD_FAF_RTT),
                                  DIFF_Leader_Delivery_RTT = abs(Leader_Delivery_RTT - SQL_Leader_Delivery_RTT),
                                  DIFF_Follower_ORD_Start_RTT = abs(Follower_ORD_Start_RTT - SQL_Follower_ORD_Start_RTT),
                                  DIFF_Follower_ORD_Stop_RTT = abs(Follower_ORD_Stop_RTT - SQL_Follower_ORD_Stop_RTT),
                                  FLAG_Observed_ORD_Compression = ifelse(DIFF_Observed_ORD_Compression > 0.01, 1, 0),
                                  FLAG_Observed_ORD_Compression = ifelse(is.na(Observed_ORD_Compression) & !is.na(SQL_Observed_Compression), 1, FLAG_Observed_ORD_Compression),
                                  FLAG_Observed_ORD_Compression = ifelse(!is.na(Observed_ORD_Compression) & is.na(SQL_Observed_Compression), 1, FLAG_Observed_ORD_Compression),
                                  FLAG_Observed_Leader_ORD_IAS = ifelse(DIFF_Observed_Leader_ORD_IAS > 0.0001, 1, 0),
                                  FLAG_Observed_Leader_ORD_IAS = ifelse(is.na(Observed_Leader_ORD_IAS) & !is.na(SQL_Observed_Mean_Leader_ORD_IAS), 1, FLAG_Observed_Leader_ORD_IAS),
                                  FLAG_Observed_Leader_ORD_IAS = ifelse(!is.na(Observed_Leader_ORD_IAS) & is.na(SQL_Observed_Mean_Leader_ORD_IAS), 1, FLAG_Observed_Leader_ORD_IAS),
                                  FLAG_Observed_Follower_ORD_IAS = ifelse(DIFF_Observed_Follower_ORD_IAS > 0.0001, 1, 0),
                                  FLAG_Observed_Follower_ORD_IAS = ifelse(is.na(Observed_Follower_ORD_IAS) & !is.na(SQL_Observed_Mean_Follower_ORD_IAS), 1, FLAG_Observed_Follower_ORD_IAS),
                                  FLAG_Observed_Follower_ORD_IAS = ifelse(!is.na(Observed_Follower_ORD_IAS) & is.na(SQL_Observed_Mean_Follower_ORD_IAS), 1, FLAG_Observed_Follower_ORD_IAS),
                                  FLAG_Observed_Leader_ORD_WE = ifelse(DIFF_Observed_Leader_ORD_WE > 0.0001, 1, 0),
                                  FLAG_Observed_Leader_ORD_WE = ifelse(is.na(Observed_Leader_ORD_Wind_Effect) & !is.na(SQL_Observed_Mean_Leader_ORD_Wind_Effect), 1, FLAG_Observed_Leader_ORD_WE),
                                  FLAG_Observed_Leader_ORD_WE = ifelse(!is.na(Observed_Leader_ORD_Wind_Effect) & is.na(SQL_Observed_Mean_Leader_ORD_Wind_Effect), 1, FLAG_Observed_Leader_ORD_WE),
                                  FLAG_Observed_Follower_ORD_WE = ifelse(DIFF_Observed_Follower_ORD_WE > 0.0001, 1, 0),
                                  FLAG_Observed_Follower_ORD_WE = ifelse(is.na(Observed_Follower_ORD_Wind_Effect) & !is.na(SQL_Observed_Mean_Follower_ORD_Wind_Effect), 1, FLAG_Observed_Follower_ORD_WE),
                                  FLAG_Observed_Follower_ORD_WE = ifelse(!is.na(Observed_Follower_ORD_Wind_Effect) & is.na(SQL_Observed_Mean_Follower_ORD_Wind_Effect), 1, FLAG_Observed_Follower_ORD_WE),
                                  FLAG_Observed_Surface_Headwind = ifelse(DIFF_Observed_Surface_Headwind > 0.0001, 1, 0),
                                  FLAG_Observed_Surface_Headwind = ifelse(is.na(Observed_AGI_Surface_Headwind) & !is.na(SQL_Observed_Surface_Headwind), 1, FLAG_Observed_Surface_Headwind),
                                  FLAG_Observed_Surface_Headwind = ifelse(!is.na(Observed_AGI_Surface_Headwind) & is.na(SQL_Observed_Surface_Headwind), 1, FLAG_Observed_Surface_Headwind),
                                  FLAG_Leader_ORD_FAF_Time = ifelse(DIFF_Leader_ORD_FAF_Time > 0.0001, 1, 0),
                                  FLAG_Leader_ORD_FAF_Time = ifelse(is.na(Leader_ORD_FAF_Time) & !is.na(SQL_Leader_ORD_FAF_Time), 1, FLAG_Leader_ORD_FAF_Time),
                                  FLAG_Leader_ORD_FAF_Time = ifelse(!is.na(Leader_ORD_FAF_Time) & is.na(SQL_Leader_ORD_FAF_Time), 1, FLAG_Leader_ORD_FAF_Time),
                                  FLAG_Leader_Delivery_Time = ifelse(DIFF_Leader_Delivery_Time > 0.0001, 1, 0),
                                  FLAG_Leader_Delivery_Time = ifelse(is.na(Leader_Delivery_Time) & !is.na(SQL_Leader_Delivery_Time), 1, FLAG_Leader_Delivery_Time),
                                  FLAG_Leader_Delivery_Time = ifelse(!is.na(Leader_Delivery_Time) & is.na(SQL_Leader_Delivery_Time), 1, FLAG_Leader_Delivery_Time),
                                  FLAG_Leader_ORD_FAF_RTT = ifelse(DIFF_Leader_ORD_FAF_RTT > 0.001, 1, 0),
                                  FLAG_Leader_ORD_FAF_RTT = ifelse(is.na(Leader_ORD_FAF_RTT) & !is.na(SQL_Leader_ORD_FAF_RTT), 1, FLAG_Leader_ORD_FAF_RTT),
                                  FLAG_Leader_ORD_FAF_RTT = ifelse(!is.na(Leader_ORD_FAF_RTT) & is.na(SQL_Leader_ORD_FAF_RTT), 1, FLAG_Leader_ORD_FAF_RTT),
                                  FLAG_Leader_Delivery_RTT = ifelse(DIFF_Leader_Delivery_RTT > 0.001, 1, 0),
                                  FLAG_Leader_Delivery_RTT = ifelse(is.na(Leader_Delivery_RTT) & !is.na(SQL_Leader_Delivery_RTT), 1, FLAG_Leader_Delivery_RTT),
                                  FLAG_Leader_Delivery_RTT = ifelse(!is.na(Leader_Delivery_RTT) & is.na(SQL_Leader_Delivery_RTT), 1, FLAG_Leader_Delivery_RTT),
                                  FLAG_Follower_ORD_Start_RTT = ifelse(DIFF_Follower_ORD_Start_RTT > 0.001, 1, 0),
                                  FLAG_Follower_ORD_Start_RTT = ifelse(is.na(Follower_ORD_Start_RTT) & !is.na(SQL_Follower_ORD_Start_RTT), 1, FLAG_Follower_ORD_Start_RTT),
                                  FLAG_Follower_ORD_Start_RTT = ifelse(!is.na(Follower_ORD_Start_RTT) & is.na(SQL_Follower_ORD_Start_RTT), 1, FLAG_Follower_ORD_Start_RTT),
                                  FLAG_Follower_ORD_Stop_RTT = ifelse(DIFF_Follower_ORD_Stop_RTT > 0.001, 1, 0),
                                  FLAG_Follower_ORD_Stop_RTT = ifelse(is.na(Follower_ORD_Stop_RTT) & !is.na(SQL_Follower_ORD_Stop_RTT), 1, FLAG_Follower_ORD_Stop_RTT),
                                  FLAG_Follower_ORD_Stop_RTT = ifelse(!is.na(Follower_ORD_Stop_RTT) & is.na(SQL_Follower_ORD_Stop_RTT), 1, FLAG_Follower_ORD_Stop_RTT))
                                  
                                
  
  ZSTAT_ORD_Observation <- group_by(ZCOMP_ORD_Observation, Landing_Pair_Date) %>% summarise(
    CNT_Observed_ORD_Compression = sum(FLAG_Observed_ORD_Compression, na.rm=T),
    CNT_Observed_Leader_ORD_IAS = sum(FLAG_Observed_Leader_ORD_IAS, na.rm=T),
    CNT_Observed_Follower_ORD_IAS = sum(FLAG_Observed_Follower_ORD_IAS, na.rm=T),
    CNT_Observed_Leader_ORD_WE = sum(FLAG_Observed_Leader_ORD_WE, na.rm=T),
    CNT_Observed_Follower_ORD_WE = sum(FLAG_Observed_Follower_ORD_WE, na.rm=T),
    CNT_Observed_Surface_Headwind = sum(FLAG_Observed_Surface_Headwind, na.rm=T),
    CNT_Leader_ORD_FAF_Time = sum(FLAG_Leader_ORD_FAF_Time, na.rm=T),
    CNT_Leader_Delivery_Time = sum(FLAG_Leader_Delivery_Time, na.rm=T),
    CNT_Leader_ORD_FAF_RTT = sum(FLAG_Leader_ORD_FAF_RTT, na.rm=T),
    CNT_Leader_Delivery_RTT = sum(FLAG_Leader_Delivery_RTT, na.rm=T),
    CNT_Follower_ORD_Start_RTT = sum(FLAG_Follower_ORD_Start_RTT, na.rm=T),
    CNT_Follower_ORD_Stop_RTT = sum(FLAG_Follower_ORD_Stop_RTT, na.rm=T)) %>%
    ungroup()
  
  test <- filter(ZCOMP_ORD_Observation, FLAG_Leader_ORD_FAF_Time == 1)

}



# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

  
  
  
  
  
  