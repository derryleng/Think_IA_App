# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.8. Setup Performance Model
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: 
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# v0: Constructed unsorted working tmemplate. Calculates the same ORD_Compression, ORD_Mean_IAS, Forecast_Mean_Wind_Effects as
#     the database. Requires update to account for configurable Leader delivery time, and partial final leader section, similarly
#     to the follower method.
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Updates Required/Theorized
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# - 
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ----------------------------------------------- #
# X.X.X Required Fields
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Landing_Pair_ID
# FP_Date
# Leader_Callsign
# Leader_Aircraft_Type
# Leader_UK_Wake_Cat
# Leader_Recat_Wake_Cat
# UK6Cat_Separation_Distance
# Ref_Recat_Wake_Separation_Distance
# Ref_ROT_Spacing_Distance
# UK6Cat_Separation_Time
# Ref_Recat_Wake_Separation_Time
# Ref_ROT_Spacing_Time
# Follower_Ass_Recat_Separation_IAS
# Follower_Ass_ROT_Spacing_IAS
# Follower_Ass_IAS
#### Follower_Forecast_TBS_Wind_Effect
#### Follower_Forecast_eTBS_Wind_Effect
# Observed_AGI_Surface_Headwind
# Observed_AGI_Surface_Wind_SPD
# Observed_AGI_Surface_Wind_HDG
#### UK6Cat_TBS_4DME_Wake_Separation_Distance
#### Recat_eTBS_0DME_Wake_Separation_Distance
#### Recat_eTBS_0DME_ROT_Spacing_Distance
#### Recat_eTBS_0DME_All_Separation_Distance
# Forecast_ORD_TBS_Compression
# Forecast_ORD_eTBS_Compression
#### Recat_eTBS_4DME_Wake_Separation_Distance
#### Recat_eTBS_4DME_ROT_Spacing_Distance
#### Recat_eTBS_4DME_All_Separation_Distance
# Leader_0DME_RTT
#### Observed_0DME_Separation_Distance
#### Observed_1DME_Separation_Distance
#### Observed_4DME_Separation_Distance
#### Observed_4DME_Separation_Accuracy
# Leader_4DME_Time
#### Observed_0DME_Separation_Time
#### Observed_1DME_Separation_Time
#### Observed_4DME_Separation_Time
#### Observed_Follower_eTBS_IAS
#### Observed_Follower_TBS_Wind_Effect
#### Observed_Follower_eTBS_Wind_Effect
# Observed_Compression

# Requires Validation Format. 
# Distances: A Data-frame with LAnding Pair ID, Start Distance and End Distance
# Speeds: A dataframe with Landing Pair ID and Reference Speeds
Get_Average_Forecast_Wind_Effect <- function(Segment_Forecast, Distances, Speeds, Seg_Size){
  
  ##TESTING
  #Segment_Forecast <- ORD_Segment_Forecast
  #Distances <- RECAT_Wake_Distances
  #Speeds <- RECAT_Wake_Speeds
  
  #d <- filter(Distances, Landing_Pair_ID %in% Segment_Forecast$ID)
  
  # Rename ID to Landing_Pair_ID
  Segment_Forecast <- rename(Segment_Forecast, Landing_Pair_ID = ID)
  
  # Ensure Segments are Ordered Correctly
  Segment_Forecast <- Segment_Forecast[order(Segment_Forecast$Landing_Pair_ID, Segment_Forecast$DME_Seg),]
  
  # Join on the Distances/Speeds to the Segs
  Segment_Forecast <- left_join(Segment_Forecast, Distances, by = c("Landing_Pair_ID"))
  Segment_Forecast <- left_join(Segment_Forecast, Speeds, by = c("Landing_Pair_ID"))
  
  # Filter for Segments within Distance bounds somehow (Needs additions - need to incorporate seg size)
  Segment_Forecast <- filter(Segment_Forecast, 
                             DME_Seg >= floor(End_Distance + NM_to_m - Seg_Size) & 
                               DME_Seg <= ceiling(Start_Distance - NM_to_m + Seg_Size))
  
  # Get the Segment Size Delta
  Segment_Forecast <- mutate(Segment_Forecast, 
                             Distance_Delta = ifelse((DME_Seg + Seg_Size) > Start_Distance, Start_Distance - DME_Seg, Seg_Size),
                             Time_Delta = Distance_Delta / (Assumed_IAS + Forecast_Wind_Effect_IAS))
  
  # Get the Total Distance/Time Flown for each Pair
  Segment_Stats <- group_by(Segment_Forecast, Landing_Pair_ID) %>%
    summarise(Total_Distance = sum(Distance_Delta),
              Total_Time = sum(Time_Delta)) %>% ungroup()
  
  # Rejoin Speeds
  Segment_Stats <- left_join(Segment_Stats, Speeds, by = c("Landing_Pair_ID"))
  
  # Get the Forecast GSPD and Wind Effect
  Segment_Stats <- mutate(Segment_Stats,
                          Forecast_GSPD = Total_Distance / Total_Time,
                          Forecast_Wind_Effect_IAS = Forecast_GSPD - Assumed_IAS) %>%
    select(Landing_Pair_ID, Forecast_Wind_Effect_IAS)

  return(Segment_Stats)  
  
} 

# ----------------------------------------------- #
# X.X.X Get Forecast Wind Effects
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# ----------------------------------------------- #
# RECAT: Wake
# ----------------------------------------------- #

# Get the Recat Wake TBS Distance bounds for wind effect calculation.
RECAT_Wake_Distances <- select(Landing_Pair_Reference_PM, Landing_Pair_ID, Reference_Wake_Separation_Time, Assumed_Wake_Separation_IAS) %>%
  mutate(Start_Distance = (Reference_Wake_Separation_Time * Assumed_Wake_Separation_IAS) + New_Delivery,
         End_Distance = New_Delivery) %>% select(Landing_Pair_ID, Start_Distance, End_Distance)

# Get the Recat Wake Assumed Speeds
RECAT_Wake_Speeds <- select(Landing_Pair_Reference_PM, Landing_Pair_ID, Assumed_IAS = Assumed_Wake_Separation_IAS)

# USe the function to get the RECAT wake wind effects.
RECAT_Wake_WEs <- Get_Average_Forecast_Wind_Effect(ORD_Segment_Forecast, RECAT_Wake_Distances, RECAT_Wake_Speeds, Seg_Size) %>%
  rename(Recat_eTBS_Wake_Wind_Effect = Forecast_Wind_Effect_IAS)

# Join on the Wake Wind Effects
Landing_Pair_Reference_PM <- left_join(Landing_Pair_Reference_PM, RECAT_Wake_WEs, by = c("Landing_Pair_ID"))

# ----------------------------------------------- #
# RECAT: ROT
# ----------------------------------------------- #

# Get the Recat ROT TBS Distance bounds for wind effect calculation.
RECAT_ROT_Distances <- mutate(Landing_Pair_Reference_PM,
                              Start_Distance = (Reference_ROT_Spacing_Time * Assumed_ROT_Spacing_IAS) + New_Delivery,
                              End_Distance = New_Delivery) %>% select(Landing_Pair_ID, Start_Distance, End_Distance)

# Get the Recat ROT Assumed Speeds
RECAT_ROT_Speeds <- select(Landing_Pair_Reference_PM, Landing_Pair_ID, Assumed_IAS = Assumed_ROT_Spacing_IAS)

# USe the function to get the RECAT ROT wind effects.
RECAT_ROT_WEs <- Get_Average_Forecast_Wind_Effect(ORD_Segment_Forecast, RECAT_ROT_Distances, RECAT_ROT_Speeds, Seg_Size) %>%
  rename(Recat_eTBS_ROT_Wind_Effect = Forecast_Wind_Effect_IAS)

# Join on the ROT Wind Effects
Landing_Pair_Reference_PM <- left_join(Landing_Pair_Reference_PM, RECAT_ROT_WEs, by = c("Landing_Pair_ID"))

# ----------------------------------------------- #
# Legacy: Wake
# ----------------------------------------------- #

#Legacy_Wake_Distances <- select(Landing_Pair_Reference_PM)
#Legacy_Wake_Speeds <- select(Landing_Pair_Reference_PM)
#Legacy_Wake_WEs <- Get_Average_Forecast_Wind_Effect(ORD_Segment_Forecast, RECAT_Wake_Distances, RECAT_Wake_Speeds, Seg_Size)

# ----------------------------------------------- #
# X.X.X Get TBS Distances (RECAT)
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Define Caps (After testing will be migrated)
UK6_Min <- 2.5 * NM_to_m
Recat_0DME_Min <- 2.5 * NM_to_m
Recat_4DME_Min <- 3.0 * NM_to_m

# Get 0DME RECAT Wake/ROT Distances
Landing_Pair_Reference_PM <- mutate(Landing_Pair_Reference_PM, 
                                 Recat_TBS_0DME_Wake_Separation_Distance = 
                                   Reference_Wake_Separation_Time * (Assumed_Wake_Separation_IAS + Recat_eTBS_Wake_Wind_Effect),
                                 Recat_TBS_0DME_ROT_Spacing_Distance = 
                                   Reference_ROT_Spacing_Time * (Assumed_ROT_Spacing_IAS + Recat_eTBS_ROT_Wind_Effect))
                                
# Get the Forecast ORD Compression: Currently used to convert 0DME to 4DME Distance (Take from LAnding_Pair_Reference used in PRed.)
Forecast_Compressions <- select(Landing_Pair_Reference, Landing_Pair_ID, ORD_Forecast_Compression)

# Get the "All Separation" 0DME Distances by taking the max Non-NA Value between RECAT Wake & ROT
Landing_Pair_Reference_PM <- mutate(Landing_Pair_Reference_PM,
                                    Recat_TBS_0DME_All_Separation_Distance = ifelse(Recat_TBS_0DME_Wake_Separation_Distance > Recat_TBS_0DME_ROT_Spacing_Distance,
                                                                                    Recat_TBS_0DME_Wake_Separation_Distance, Recat_TBS_0DME_ROT_Spacing_Distance),
                                    Recat_TBS_0DME_All_Separation_Distance = ifelse(is.na(Recat_TBS_0DME_All_Separation_Distance) & is.na(Recat_TBS_0DME_Wake_Separation_Distance),
                                                                                    Recat_TBS_0DME_ROT_Spacing_Distance, Recat_TBS_0DME_All_Separation_Distance),
                                    Recat_TBS_0DME_All_Separation_Distance = ifelse(is.na(Recat_TBS_0DME_All_Separation_Distance),
                                                                                    Recat_TBS_0DME_Wake_Separation_Distance, Recat_TBS_0DME_All_Separation_Distance))

# Apply caps at the 0DME level. 
Landing_Pair_Reference_PM <- mutate(Landing_Pair_Reference_PM,
                                    Recat_TBS_0DME_All_Separation_Distance = 
                                      ifelse(is.na(Recat_TBS_0DME_All_Separation_Distance) | Recat_TBS_0DME_All_Separation_Distance < Recat_0DME_Min,
                                             Recat_0DME_Min, Recat_TBS_0DME_All_Separation_Distance))

# Get 4DME Distances (RECAT)
Landing_Pair_Reference_PM <- left_join(Landing_Pair_Reference_PM, Forecast_Compressions, by = c("Landing_Pair_ID")) %>%
  rename(Forecast_ORD_TBS_Compression = ORD_Forecast_Compression) %>% 
  mutate(Recat_TBS_4DME_Wake_Separation_Distance = Recat_TBS_0DME_Wake_Separation_Distance + Forecast_ORD_TBS_Compression,
         Recat_TBS_4DME_ROT_Spacing_Distance = Recat_TBS_0DME_ROT_Spacing_Distance + Forecast_ORD_TBS_Compression,
         Recat_TBS_4DME_All_Separation_Distance = Recat_TBS_0DME_All_Separation_Distance + Forecast_ORD_TBS_Compression)

# Apply Caps at the 4DME Level.
Landing_Pair_Reference_PM <- mutate(Landing_Pair_Reference_PM,
                                    Recat_TBS_4DME_All_Separation_Distance = 
                                      ifelse(is.na(Recat_TBS_4DME_All_Separation_Distance) | Recat_TBS_4DME_All_Separation_Distance < Recat_4DME_Min,
                                             Recat_4DME_Min, Recat_TBS_4DME_All_Separation_Distance))

# 



# ----------------------------------------------- #
# X.X.X "Winning" Speeds and WEs
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Perform similar procedure to get the eTBS Wind Effect (Max(Wake, Rot))
Landing_Pair_Reference_PM <- mutate(Landing_Pair_Reference_PM,
                                    Follower_Forecast_eTBS_Wind_Effect = ifelse(Recat_eTBS_Wake_Wind_Effect > Recat_eTBS_ROT_Wind_Effect,
                                                                                Recat_eTBS_Wake_Wind_Effect, Recat_eTBS_ROT_Wind_Effect),
                                    Follower_Forecast_eTBS_Wind_Effect = ifelse(is.na(Follower_Forecast_eTBS_Wind_Effect) & is.na(Recat_eTBS_ROT_Wind_Effect),
                                                                                Recat_eTBS_Wake_Wind_Effect, Follower_Forecast_eTBS_Wind_Effect),
                                    Follower_Forecast_eTBS_Wind_Effect = ifelse(is.na(Follower_Forecast_eTBS_Wind_Effect) & is.na(Recat_eTBS_Wake_Wind_Effect),
                                                                                Recat_eTBS_ROT_Wind_Effect, Follower_Forecast_eTBS_Wind_Effect))

# Perform similar procedure to get the eTBS Wind Effect (Max(Wake, Rot))
Landing_Pair_Reference_PM <- mutate(Landing_Pair_Reference_PM,
                                    Follower_Ass_IAS = ifelse(Assumed_Wake_Separation_IAS > Assumed_ROT_Spacing_IAS,
                                                              Assumed_Wake_Separation_IAS, Assumed_ROT_Spacing_IAS),
                                    Follower_Ass_IAS = ifelse(is.na(Follower_Ass_IAS) & is.na(Assumed_ROT_Spacing_IAS),
                                                              Assumed_Wake_Separation_IAS, Follower_Ass_IAS),
                                    Follower_Ass_IAS = ifelse(is.na(Follower_Ass_IAS) & is.na(Assumed_Wake_Separation_IAS),
                                                              Assumed_ROT_Spacing_IAS, Follower_Ass_IAS))

# ----------------------------------------------- #
# X.X.X Observed Parameters
# ----------------------------------------------- #
# Currently only In-Trail 
# ----------------------------------------------- #

# Observed Wind Effect/Speeds
eTBS_Observed_Distances <- select(Landing_Pair_Reference_PM, Flight_Plan_ID = Follower_Flight_Plan_ID, Recat_TBS_0DME_All_Separation_Distance) %>%
  mutate(End_Distance = New_Delivery,
         Start_Distance = New_Delivery + Recat_TBS_0DME_All_Separation_Distance) %>% select(-c("Recat_TBS_0DME_All_Separation_Distance"))

# Get Observed Parameters
eTBS_Observed_Parameters <- Get_Average_Observed_Mode_S_Parameters(Radar, eTBS_Observed_Distances, "Distances") %>%
  rename(Observed_Mean_eTBS_IAS = Observed_Mean_IAS,
         Observed_Mean_eTBS_Wind_Effect = Observed_Mean_Wind_Effect)

# Join
Landing_Pair_Reference_PM <- left_join(Landing_Pair_Reference_PM, eTBS_Observed_Parameters, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))



# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ----------------------------------------------- #
# X.X.X Testing
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

if (Test_Mode){
  
  PM_Query <- 
  "SELECT
  
  "
  
  
  
  
}
  
  
  
  
  
  
  
  
  