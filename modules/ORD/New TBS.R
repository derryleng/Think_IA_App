
# 1. Generate Landing Pairs

if (!Testing){
  INP_Landing_Pair <- Generate_Landing_Pair(INP_Flight_Plan)
  Clear_Landing_Pair(con, PROC_Period, PROC_Criteria)
  PopulateSQLTable(con, "tbl_Landing_Pair", select(INP_Landing_Pair, -Landing_Pair_ID))
}

INP_Landing_Pair <- Load_Landing_Pair_Data(con, PROC_Period, PROC_Criteria)


# 2. Gather Aircraft Reference Data


Get_FAF_Distances <- function(FP, Runway){
  
  # Acquire the FAF Distance
  Runway <- select(Runway, Runway_Name, FAF_Distance)
  
  # Join on the FAF Distance
  FP <- left_join(Runway, by= c("Landing_Runway" = "Runway_Name"))
  
  return(FP)
  
  
}``

Get_RTTs_From_Distance <- function(FP, Radar, N, Distance, RTT_Var_In, Time_Var_In, Prefix_Out, MaxorMin){
  
  RTT_Var <- paste0(Prefix_Out, "_RTT")
  Time_Var <- paste0(Prefix_Out, "_Time")
  
  if (class(Distance) == "character"){
    
    FP1 <- select(FP, Flight_Plan_ID, NewDistance = !!sym(Distance))
    Radar <- left_join(Radar, FP1, by = c("Flight_Plan_ID"))
    
  }
  
  if (class(Distance) == "numeric"){
    
    Radar <- mutate(Radar, NewDistance = Distance)
    
  }
  
  Radar <- filter(Radar, !is.na(!!sym(RTT_Var_In)))
  
  if (MaxorMin == "Max"){
    Radar <- arrange(Radar, Flight_Plan_ID, desc(!!sym(RTT_Var_In)))
    if (!is.na(Distance)){Radar <- filter(Radar, !!sym(RTT_Var_In) <= NewDistance)}
  }
  
  if (MaxorMin == "Min"){
    Radar <- arrange(Radar, Flight_Plan_ID, !!sym(RTT_Var_In))
    if (!is.na(Distance)){Radar <- filter(Radar, !!sym(RTT_Var_In) >= NewDistance)}
  }
  
  Radar <- group_by(Radar, Flight_Plan_ID) %>% mutate(ID = row_number()) %>%
    ungroup() %>% filter(ID == N) %>%
    select(Flight_Plan_ID, !!sym(RTT_Var) := !!sym(RTT_Var_In), !!sym(Time_Var) := !!sym(Time_Var_In))
  
  FP <- left_join(FP, Radar, by = c("Flight_Plan_ID"))
  
  return(FP)
  
}

Get_FAF_RTTs <- function(FP, Radar, N){
  
  Prefix_Out <- paste0("Leader_FAF_", N)
  
  FP <- Get_RTTs_From_Distance(FP, Radar, N, "FAF_Distance", "Range_To_Threshold", "Track_Time", Prefix_Out, "Min")
  
  return(FP)
  
}

Get_CCT_RTTs <- function(FP, Radar, NeworOld){
  
  Prefix_Out <- paste0(NeworOld, "_Leader_CCT")
  Distance <- paste0(NeworOld, "_Compression_Commencement_Threshold")
  
  FP <- Get_RTTs_From_Distance(FP, Radar, 1, Distance, "Range_To_Threshold", "Track_Time", Prefix_Out, "Min")
  
  return(FP)
  
}

Get_Max_RTTs <- function(FP, Radar, RTT_Var_In){
  
  Prefix_Out <- "Max"
  
  FP <- Get_RTTs_From_Distance(FP, Radar, 1, NA, RTT_Var_In, "Track_Time", Prefix_Out, "Max")
  
  return(FP)
  
}

Get_Delivery_RTTs <- function(FP, Radar, Delivery_Distance, NeworOld){
  
  Prefix_Out <- paste0(NeworOld, "_Leader_Delivery")
  
  FP <- Get_RTTs_From_Distance(FP, Radar, 1, Delivery_Distance, "Range_To_Threshold", "Track_Time", Prefix_Out, "Max")
  
  return(FP)
  
} 

Get_CC_RTTs <- function(FP, NeworOld){
  
  RTT_Var <- paste0(NeworOld, "_Leader_CC_RTT")
  Time_Var <- paste0(NeworOld, "_Leader_CC_Time")
  CCT_RTT_Var <- paste0(NeworOld, "_Leader_CCT_RTT")
  CCT_Time_Var <- paste0(NeworOld, "_Leader_CCT_Time")
  
  FP <- FP %>% mutate(!!sym(RTT_Var) := pmin(!!sym(CCT_RTT_Var), Max_RTT, na.rm = T),
                      !!sym(Time_Var) := ifelse(!!sym(RTT_Var) == !!sym(CCT_RTT_Var), !!sym(CCT_Time_Var), Max_Time))
  
  return(FP)
  
}

Get_Wake_Scheme_Equivalence <- function(New_Wake_Scheme, Old_Wake_Scheme, New_WTC){
  
  New_Wake_Cats <- Get_Wake_Cats(New_Wake_Scheme)
  Old_Wake_Cats <- Get_Wake_Cats(Old_Wake_Scheme)
  
  if (New_Wake_Scheme == Old_Wake_Scheme){Table <- data.frame(New = New_Wake_Cats, Old = Old_Wake_Cats)}
  else if (New_Wake_Scheme == "RECAT-EU" & Old_Wake_Scheme == "UK6Cat"){Table <- data.frame(New = New_Wake_Cats, Old = Old_Wake_Cats)}
  else if (New_Wake_Scheme %in% c("RECAT-20", "RECAT-14") & Old_Wake_Scheme == "RECAT-EU"){Table <- data.frame(New = New_Wake_Cats, Old = substr(New_Wake_Cats, 1, 1))}
  else {return(NA)}
  
  Old_WTC <- filter(Table, New == New_WTC)$Old_WTC
  if (length(Old_WTC) == 1){return(as.character(Old_WTC))} else {return(NA)}
  
}

# Wake Pair Bolstering. Assumes Aircraft FP fields joined already.
# (Callsign, Operator, Recat_Wake, Legacy_Wake, )
Bolster_Landing_Pair_By_Leader_WTC <- function(LP, New_Wake_Scheme, Old_Wake_Scheme, Wake_Distances, Bolster_Pairs){
  
  # Get the Pairs (Do all for now)
  if (length(Bolster_Pairs) != 0){
    Wake_Distances <- mutate(Wake_Distances, LF_Pair = paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "_")) %>%
      filter(LF_Pair %in% Bolster_Pairs) %>% select(-LF_Pair)
  }
      
  # Code to retrieve the relevant pairs: Loop through each pair
  for (i in 1:nrow(Wake_Distances)){
    
    # Get the Leader/Follower Wake
    Wake_F <- Bolster_Pairs$Follower_WTC[i]
    Wake_L <- Bolster_Pairs$Leader_WTC[i]
    
    # Attempt to map the Leader WTC to its legacy. If one to one mapping, add to bolstered data. (Will return NA if not 1-1)
    Leg_WTC <- Get_Wake_Scheme_Equivalence(New_Wake_Scheme, Old_Wake_Scheme, Wake_L)
    
    # Get the Pairs in the Data to expand:
    # Gets all pairs with matching follower and different leader. Duplicates and changes leader.
    PM_Bolster_Main_Iter <- filter(PM_Bolster_Main_Original, Follower_Recat_Wake_Cat == Wake_F & Leader_Recat_Wake_Cat != Wake_L) %>% 
      mutate(Leader_Recat_Wake_Cat = Wake_L) %>%
      mutate(Leader_Aircraft_Type = "ZZZZ") %>%
      mutate(Leader_Callsign = "ZZZ999") %>%
      mutate(Leader_Legacy_Wake_Cat = Leg_WTC)
    
    # Bind Together
    PM_Bolster_Main_Iter <- PM_Bolster_Main_Iter %>%
      mutate(Bolster_Flag_Main = 1)
    
    # If total Pair Observations over certain amount, cap amount (remove for now)
    #if (nrow(pm_iter) > max_obs){pm_iter <- pm_iter[1:max_obs,]}
    
    # Bind onto main dataset.
    if (i == 1){PM_Bolster_Main <- PM_Bolster_Main_Iter} else {PM_Bolster_Main <- rbind(PM_Bolster_Main, PM_Bolster_Main_Iter)}
    
  }
  
  
  
  
  
  # Add on the Three Bolster Flags
  Performance_Model <- Performance_Model %>%
    mutate(Bolster_Flag_WTC = 0,
           Bolster_Flag_Sep = 0)
  
  # Bolster for Wake Pairs
  if (Bolster_Main){
    

    
    # Join on the GWCS Data
    PM_Bolster_Main <- PM_Bolster_Main %>%
      select(-c("Ref_Recat_Wake_Separation_Time", "Ref_Recat_Wake_Separation_Distance", "Follower_Ass_Recat_Separation_IAS")) %>% # Remove Reference Wake Parameters
      select(-Follower_Forecast_eTBS_Wind_Effect) %>% # Remove old Forecast Wind Effect
      left_join(Recat_Wake_Dist, by = c("Leader_Recat_Wake_Cat" = "Leader_WTC", "Follower_Recat_Wake_Cat" = "Follower_WTC")) %>% # Join on new Reference Wake Distance
      left_join(Recat_Wake_Time, by = c("Leader_Recat_Wake_Cat" = "Leader_WTC", "Follower_Recat_Wake_Cat" = "Follower_WTC")) %>% # Join on new Reference Wake Time
      left_join(Recat_Wake_Speed, by = c("Leader_Recat_Wake_Cat" = "Leader_WTC", "Follower_Recat_Wake_Cat" = "Follower_WTC")) %>% # Join on new Assumed Wake IAS
      rename(Ref_Recat_Wake_Separation_Distance = Reference_Wake_Separation_Distance,
             Ref_Recat_Wake_Separation_Time = Reference_Wake_Separation_Time,
             Follower_Ass_Recat_Separation_IAS = Assumed_Wake_Separation_IAS) %>% # Rename Variables to match PM Data
      left_join(GWCS_Data, by=c("FP_Date", "Follower_Callsign"="Callsign",
                                "Ref_Recat_Wake_Separation_Distance"="Sep_Dist",
                                "Follower_Time_At_4DME"="Time_At_4DME")) %>% 
      rename(Follower_Forecast_eTBS_Wind_Effect = Forecast_Wind_Effect_IAS) %>%
      mutate(Recat_eTBS_0DME_Wake_Separation_Distance = Ref_Recat_Wake_Separation_Time * (Follower_Ass_Recat_Separation_IAS + Follower_Forecast_eTBS_Wind_Effect) / 3600) %>% # Set new Wake TBS Distance
      mutate(Recat_eTBS_4DME_Wake_Separation_Distance = NA, Recat_eTBS_0DME_ROT_Spacing_Distance = NA, Recat_eTBS_4DME_ROT_Spacing_Distance = NA) %>% # Set other TBS Distances to NA
      mutate(Recat_eTBS_0DME_All_Separation_Distance = Recat_eTBS_0DME_Wake_Separation_Distance, Recat_eTBS_4DME_All_Separation_Distance = NA) 
    
    
    Performance_Model <- rbind(Performance_Model, PM_Bolster_Main)
    
  }
  
}

# ROT Pair Bolstering
Bolster_Landing_Pair_By_Separation <- function(){}

# Prediction Time Adjustment
Prediction_Time_Adjustment <- function(LP, Original_Distance_Var, TimeperNM = 25){}


# 3. Find Leader Max RTT on Glideslope (and other distances)


# Max RTT, CCT, CCT RTT, FAF, FAF RTT, DP, DP RTT
# Follower at time of all above


# 4. Produce Bolstered Samples (Wake Pair, Runway/ROT Pair)
# -- Remove Leader Aircraft Type
# -- For Wake Bolstering, Remove Legacy Wake Categories unless direct mapping from RECAT to Legacy (e.g. both RECAT-EU or RECAT-20 -> RECAT-EU)
# -- For ROT Bolstering, 

# 5. Find Prediction Times

# -- Find Prediction Times for ORiginal Pairs as Time Follower Joins the PL Sequence (not DW)
# -- Find Prediction Times for Bolstered Pairs with the following: ((Ref_Distance_New - Ref_Distance_Old) * n(Prediction_Time_Original)) + Prediction_Time_Original (Assume n = 25 for simplicity?)
# -- Find Predicted Surface Wind

# 6. Generate GWCS Forecast
# -- Use Existing Function for all pairs, using Observation_ID instead of Landing_Pair_ID

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # # OPEN ORD CAV

# 7. Gather RECAT/Legacy Reference Data
# -- 

# 8. Create ORD Profiles (RECAT/LEGACY)
# -- Aircraft Profile 
# -- IAS Profile
# -- GSPD Profile

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # # CLOSE ORD CAV
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # # OPEN TBSC CAV

# 9. Create TBS Profiles (RECAT/LEGACY)
# -- 

# 10. TBS Calculations All Distances (RECAT/LEGACY)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # # CLOSE TBSC CAV
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # # OPEN ORD CAV


# 11. Forecast ORD Speeds/WEs/Compression (RECAT/LEGACY)

# 12. Forecast WAD Speeds/WEs/Compression (RECAT/LEGACY)
# -- Leader Compression Commencement = min(Leader Max RTT, CCT)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # # CLOSE ORD CAV

# 13. Observed Range to Threshold & Wind Effect Recalculation 
# -- Define Allowed Path Legs/Angle Limit, Max Range to ILS, Time Limits. Should be stored as adaptation
# -- All Wind Effects recalculated into new field
# -- Fit Criteria wind effects recalculated into new field
# -- New Mode S IAS field onl populated with glideslope.
# -- Have the option to turn off and new wind effect will be NULL for values with no original RTT. New RTT will equal Original RTT.

# 14. Observed ORD Calculations
# -- Two Options: Use Observed Flying Distance or Forecast Flying Distance (Observed is old)
# -- Forecast Flying Distance better matches Forecast Metric and can directly compare Speeds/WEs.
# -- IAS cannot be proxied using radar off the glideslope.
# -- However, Set some rules for proxy values so that if x% portion is inside allowed region then average of this portion can be used for full approximation.

# 15. Observed WAD calculations 
# -- Use the CC calculated for Forecast WAD
# -- 

# 16. ORD/WAD Error Metrics
# -- Calculate O-F for ORD, WAD and Combined. (Distance Compression, Time Compression, Speed etc)

# 17. Assumed Separation Accuracy Calculations
# -- For Actual Time Separations, unknown for now. Usually use statistical transformation using sep accuracy of 
# -- Add to FAF/CC Distances to get "Actual" Distance.

# 18. Observed Average IAS / Wind Effect for Separation Distances
# -- For each Time Scenario 
# -- Perfect require Follower Average across Separation Distance (From Delivery Point)
# -- Actual require Leader ORD average and Follower average across Actual Separation Distance + CC

# 19. Time Separations
# -- Use Speeds/WEs above to calculate Average GSPD
# -- Time Flown = Distance Flown / Average GSPD
# -- Perfects are Follower Time Flown, Actuals Follower Total - Leader.

# 20. Split into tables and upload to database. 
# -- Decide whether we want proxies for ORD performance validation







