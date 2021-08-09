Generate_Observed_DME_Parameters <- function(LP, FP, Radar, DMEs){
  
  # Loop across all DMEs.
  for (DME in DMEs){
    #message("Generating Observed ", DME, "DME Parameters...")
    
    # Get the Values for DME Times/RTTs for each Aircraft in Flight Plan.
    FP <- FP %>% Get_Fixed_RTTs(Radar, DME)
    
    # Join to LP for Leader/Follower separately.
    for (LorF in c("Leader", "Follower")){
      
      # Select relevant DME RTT/Time Fields.
      FPAC <- select(FP, Flight_Plan_ID,
                     !!sym(paste0(LorF, "_", DME, "DME_RTT")) := !!sym(paste0(DME, "DME_RTT")),
                     !!sym(paste0(LorF, "_", DME, "DME_Time")) := !!sym(paste0(DME, "DME_Time")))
      
      # Join relevant fields.
      LP <- left_join(LP, FPAC, by = setNames("Flight_Plan_ID", paste0(LorF, "_Flight_Plan_ID")))
      
    }
    
    # Get the Follower Interpolated Distances based on the Leader DME Times. Currently uses trail types: will need to use runway pairs soon.
    for (Trail in c("Not_In_Trail", "In_Trail", "In_Trail_Non_Sequential")){
      
      # Filter Data for this trail, again will need to change to do Runway Groups.
      LPTrail <- filter(LP, Landing_Pair_Type == Trail)
      
      # Get the Interpolated Follower RTTs.
      FollDistances <- Get_Follower_Interpolated_RTTs(LPTrail, Radar, Leader_Time_Var = paste0("Leader_", DME, "DME_Time")) %>%
        rename(!!sym(paste0("Follower_Int_", DME, "DME_RTT")) := Interp_Distance)
      
      # Join onto LPTrail
      LPTrail <- left_join(LPTrail, FollDistances, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) # TEMP:
      
      # Bind Results Together
      if (exists("LPAll")){LPAll <- rbind(LPAll, LPTrail)} else {LPAll <- LPTrail}
      
    }
    
    # Get the Observed Separation Distance and Time Values
    LP <- LPAll %>% 
      mutate(!!sym(paste0("Observed_", DME, "DME_Separation_Distance")) := !!sym(paste0("Follower_Int_", DME, "DME_RTT")) - !!sym(paste0("Leader_", DME, "DME_RTT"))) %>%
      mutate(!!sym(paste0("Observed_", DME, "DME_Separation_Time")) := !!sym(paste0("Follower_", DME, "DME_Time")) - !!sym(paste0("Leader_", DME, "DME_Time")))
    
    rm(LPAll)
    
  }
  
  return(LP)
  
}


Generate_ORD_Input_Parameters_Operation_Independent <- function(LP, FP, Radar, ORDRunways){
  
  # Get the Distances present on an Aircraft Type Level. (FAF, Delivery, Max RTT) START FROM LEADER.
  FPL <- FP %>%
    Get_Max_RTTs(Radar, "Range_To_Threshold") %>%
    Get_FAF_Distances(ORDRunways) %>% # FAF Distances
    Get_FAF_RTTs(Radar, N = 1) %>% # FAF RTTs for ORD
    Get_FAF_RTTs(Radar, N = 2) %>% # FAF RTTs for WAD
    select(Flight_Plan_ID, Max_RTT, FAF_Distance, Leader_FAF_1_RTT, Leader_FAF_1_Time, Leader_FAF_2_RTT, Leader_FAF_2_Time) %>%
    rename(Leader_FAF_Distance = FAF_Distance) %>%
    rename(Leader_Max_RTT = Max_RTT)
  
  # Now rename variables to get the follower.
  FPF <- FPL %>%
    rename(Follower_FAF_1_RTT = Leader_FAF_1_RTT, Follower_FAF_1_Time = Leader_FAF_1_Time,
           Follower_FAF_2_RTT = Leader_FAF_2_RTT, Follower_FAF_2_Time = Leader_FAF_2_Time,
           Follower_FAF_Distance = Leader_FAF_Distance, Follower_Max_RTT = Leader_Max_RTT)
  
  # Join on the Aircraft Level Parameters.
  LP <- LP %>%
    left_join(FPL, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
    left_join(FPF, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  # For legacy IA, take Follower FAF RTT/Time as Follower_FAF_1.
  LP <- LP %>%
    mutate(Follower_FAF_RTT = Follower_FAF_1_RTT, Follower_FAF_Time = Follower_FAF_1_Time)
  
  return(LP)
  
}


Generate_ORD_Input_Parameters_Operation_Dependent <- function(LP, FP, Radar, ACProfile, DeliveryPoint, LegacyorRecat){
  
  # From LegacyorRecat, get New or Old name
  NeworOld <- ifelse(LegacyorRecat != "Legacy", "New", "Old")
  
  # Add on Delivery Point variable.
  LP <- mutate(LP, !!sym(paste0(NeworOld, "_Delivery")) := DeliveryPoint)
  
  # Get the Distances present on an Aircraft Type Level. (FAF, Delivery, Max RTT) START FROM LEADER.
  FPL <- FP %>%
    Get_Delivery_RTTs(Radar, DeliveryPoint, LegacyorRecat) %>% # Delivery RTTs
    select(Flight_Plan_ID,
           !!sym(paste0(LegacyorRecat, "_Leader_Delivery_RTT")), !!sym(paste0(LegacyorRecat, "_Leader_Delivery_Time")))
    
  # Now rename variables to get the follower.
  FPF <- FPL %>%
    rename(!!sym(paste0(LegacyorRecat, "_Follower_Delivery_RTT")) := !!sym(paste0(LegacyorRecat, "_Leader_Delivery_RTT")),
           !!sym(paste0(LegacyorRecat, "_Follower_Delivery_Time")) := !!sym(paste0(LegacyorRecat, "_Leader_Delivery_Time")))
  
  # Join on the Aircraft Level Parameters.
  LP <- LP %>%
    left_join(FPL, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
    left_join(FPF, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  # Now get the LP CCT/CC Distances etc.
  LP <- LP %>%
    Get_CCT_Distances(ACProfile, LP_Primary_Key, LegacyorRecat) %>%
    Get_CCT_RTTs(Radar, LegacyorRecat) %>%
    Get_CC_RTTs(LegacyorRecat) %>%
    Get_CC_Distances(LegacyorRecat)
  
  return(LP)
  
}


Get_ORD_Interval_Prefix <- function(Interval){
  
  if (Interval == "ORD"){return("JustORD")}
  if (Interval == "WAD"){return("WAD")}
  if (Interval == "Full"){return("ORD")}
  message("ORD Related Interval '", Interval, "' does not exist. Only 'ORD', 'WAD' and 'Full' are currently supported. Returning NULL.")
  
}



Generate_Observed_ORD_Results <- function(LP, Radar, ORD_AC_Profile, TimeorRange, Metric_Type, Use_Adjustment, LegacyorRecat, AdjustWEs, MaxRangeToILS, AllowedPathLegs, MaxUnderRep){
  
  # Get the Prefix names for the Three ORD Measurement Intervals. 
  ORDPrefix <- Get_ORD_Interval_Prefix("ORD")
  WADPrefix <- Get_ORD_Interval_Prefix("WAD")
  FullPrefix <- Get_ORD_Interval_Prefix("Full")
  
  # Adjust the Radar RTTs and WEs if AdjustWEs is on
  Radar <- Adjust_Radar_WEs(Radar, AdjustWEs, MaxRangeToILS, AllowedPathLegs)
  
  # Get the Parameter prefix for the bounds for Observed Distance, Time and Speed Calculations
  TimeorRTT <- ifelse(TimeorRange == "Time", "Time", "RTT")
  
  # Declare the parameter names for the Observed ORD/WAD/Full calculations
  Lead_ORD_Stop_Var <- paste0(LegacyorRecat, "_Leader_Delivery_", TimeorRTT)
  Lead_ORD_Start_Var <- paste0("Leader_FAF_1_", TimeorRTT)
  Lead_WAD_Stop_Var <- paste0("Leader_FAF_2_", TimeorRTT)
  Lead_WAD_Start_Var <- paste0(LegacyorRecat, "_Leader_CC_", TimeorRTT)
  Foll_ORD_Stop_Var <- ifelse(TimeorRTT == "Time", Lead_ORD_Stop_Var, paste0(LegacyorRecat, "_Follower_Int_Delivery_", TimeorRTT))
  Foll_ORD_Start_Var <- ifelse(TimeorRTT == "Time", Lead_ORD_Start_Var, paste0("Follower_Int_FAF_1_", TimeorRTT))
  Foll_WAD_Stop_Var <- ifelse(TimeorRTT == "Time", Lead_WAD_Stop_Var, paste0("Follower_Int_FAF_2_", TimeorRTT))
  Foll_WAD_Start_Var <- ifelse(TimeorRTT == "Time", Lead_WAD_Start_Var, paste0(LegacyorRecat, "_Follower_Int_CC_", TimeorRTT))
  
  # Set the Start and End Variables based on Time or RTT
  for (Trail in c("Not_In_Trail", "In_Trail", "In_Trail_Non_Sequential")){
    
    # Filter Data for Trail Type (This will need to be fixed for 3 parallel runways...)
    LP_Trail <- filter(LP, Landing_Pair_Type == Trail)

    # Get the Interpolated Follower RTTs for The Delivery Point, two FAF Points and the CC Point.
    D1 <- Get_Follower_Interpolated_RTTs(LP_Trail, Radar, Leader_Time_Var = paste0(LegacyorRecat, "_Leader_Delivery_Time")) %>%
      rename(!!sym(paste0(LegacyorRecat, "_Follower_Int_Delivery_RTT")) := Interp_Distance)
    D2 <- Get_Follower_Interpolated_RTTs(LP_Trail, Radar, Leader_Time_Var = "Leader_FAF_1_Time") %>%
      rename(!!sym(paste0(LegacyorRecat, "_Follower_Int_FAF_1_RTT")) := Interp_Distance)
    D3 <- Get_Follower_Interpolated_RTTs(LP_Trail, Radar, Leader_Time_Var = "Leader_FAF_2_Time") %>%
      rename(!!sym(paste0(LegacyorRecat, "_Follower_Int_FAF_2_RTT")) := Interp_Distance)
    D4 <- Get_Follower_Interpolated_RTTs(LP_Trail, Radar, Leader_Time_Var = paste0(LegacyorRecat, "_Leader_CC_Time")) %>%
      rename(!!sym(paste0(LegacyorRecat, "_Follower_Int_CC_RTT")) := Interp_Distance)
    
    # Join on these these interpolated Distances.
    LP_Trail <- LP_Trail %>%
      left_join(D1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
      left_join(D2, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
      left_join(D3, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
      left_join(D4, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
    
    # Remove the Individual Distance files.
    rm(D1, D2, D3, D4)
    
    # Leader ORD Observed Speeds/WEs
    LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                         Prefix = ORDPrefix,
                                                         LorF = "Leader",
                                                         TimeorRange,
                                                         Start_Var = Lead_ORD_Start_Var,
                                                         End_Var = Lead_ORD_Stop_Var,
                                                         LegacyorRecat,
                                                         MaxUnderRep,
                                                         LP_Primary_Key,
                                                         NA)
    
    # Follower ORD Observed Speeds/WEs
    LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                         Prefix = ORDPrefix,
                                                         LorF = "Follower",
                                                         TimeorRange,
                                                         Start_Var = Foll_ORD_Start_Var,
                                                         End_Var = Foll_ORD_Stop_Var,
                                                         LegacyorRecat,
                                                         MaxUnderRep,
                                                         LP_Primary_Key,
                                                         NA)
    
    # Leader WAD Observed Speeds/WEs
    LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                         Prefix = WADPrefix,
                                                         LorF = "Leader",
                                                         TimeorRange,
                                                         Start_Var = Lead_WAD_Start_Var,
                                                         End_Var = Lead_WAD_Stop_Var,
                                                         LegacyorRecat,
                                                         MaxUnderRep,
                                                         LP_Primary_Key,
                                                         NA)
    
    # Follower WAD Observed Speeds/WEs
    LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                         Prefix = WADPrefix,
                                                         LorF = "Follower",
                                                         TimeorRange,
                                                         Start_Var = Foll_WAD_Start_Var,
                                                         End_Var = Foll_WAD_Stop_Var,
                                                         LegacyorRecat,
                                                         MaxUnderRep,
                                                         LP_Primary_Key,
                                                         NA)
    
    # Leader Full Observed Speeds/WEs
    LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                         Prefix = FullPrefix,
                                                         LorF = "Leader",
                                                         TimeorRange,
                                                         Start_Var = Lead_WAD_Start_Var,
                                                         End_Var = Lead_ORD_Stop_Var,
                                                         LegacyorRecat,
                                                         MaxUnderRep,
                                                         LP_Primary_Key,
                                                         NA)
    
    # Follower Full Observed Speeds/WEs
    LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                         Prefix = FullPrefix,
                                                         LorF = "Follower",
                                                         TimeorRange,
                                                         Start_Var = Foll_WAD_Start_Var,
                                                         End_Var = Foll_ORD_Stop_Var,
                                                         LegacyorRecat,
                                                         MaxUnderRep,
                                                         LP_Primary_Key,
                                                         NA)
    
    if (exists("LP_New")){
      LP_New <- rbind(LP_New, LP_Trail)
    } else {LP_New <- LP_Trail}
    
  }
  
  # Replace LP
  LP <- LP_New
  rm(LP_New)
  
  # Get the Observed Leader ORD/WAD/Full Distances
  LP <- LP %>%
    mutate(!!sym(paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_Flying_Distance")) := Leader_FAF_1_RTT - !!sym(paste0(LegacyorRecat, "_Leader_Delivery_RTT")),
           !!sym(paste0(LegacyorRecat, "_Observed_Leader_", WADPrefix, "_Flying_Distance")) := !!sym(paste0(LegacyorRecat, "_Leader_CC_RTT")) - Leader_FAF_2_RTT,
           !!sym(paste0(LegacyorRecat, "_Observed_Leader_", FullPrefix, "_Flying_Distance")) := !!sym(paste0(LegacyorRecat, "_Leader_CC_RTT")) - !!sym(paste0(LegacyorRecat, "_Leader_Delivery_RTT")))
  
  # Get the Observed Follower ORD/WAD/Full Distances
  LP <- LP %>%
    mutate(!!sym(paste0(LegacyorRecat, "_Observed_Follower_", ORDPrefix, "_Flying_Distance")) := !!sym(paste0(LegacyorRecat, "_Follower_Int_FAF_1_RTT")) - !!sym(paste0(LegacyorRecat, "_Follower_Int_Delivery_RTT")),
           !!sym(paste0(LegacyorRecat, "_Observed_Follower_", WADPrefix, "_Flying_Distance")) := !!sym(paste0(LegacyorRecat, "_Follower_Int_CC_RTT")) - !!sym(paste0(LegacyorRecat, "_Follower_Int_FAF_2_RTT")),
           !!sym(paste0(LegacyorRecat, "_Observed_Follower_", FullPrefix, "_Flying_Distance")) := !!sym(paste0(LegacyorRecat, "_Follower_Int_CC_RTT")) - !!sym(paste0(LegacyorRecat, "_Follower_Int_Delivery_RTT")))
  
  # Get the Delivered FAF and CC Separations
  LP <- LP %>%
    mutate(!!sym(paste0(LegacyorRecat, "_Observed_FAF_Separation_Distance")) := !!sym(paste0(LegacyorRecat, "_Follower_Int_FAF_1_RTT")) - Leader_FAF_1_RTT) %>%
    mutate(!!sym(paste0(LegacyorRecat, "_Observed_CC_Separation_Distance")) := !!sym(paste0(LegacyorRecat, "_Follower_Int_CC_RTT")) - !!sym(paste0(LegacyorRecat, "_Leader_CC_RTT")))

  # Generate ORD Compression
  LP <- Generate_Observed_Compression(LP, ORD_AC_Profile, Observed_Compression_Type, Use_Adjustment, LegacyorRecat)
  
  return(LP)
  
}


Generate_Observed_Compression <- function(LP, ACProfile, Metric_Type, Use_Adjustment, LegacyorRecat){
  
  # Get the Procedural Speed Difference (Assume Difference in Follower/Leader Steady Proc Speed)
  Adjust_Var <- paste0(LegacyorRecat, "_Follower_Adjustment")
  if (Use_Adjustment){
    ACLeader <- filter(ACProfile, This_Pair_Role == "L") %>% select(Observation_ID, LS = Steady_Procedural_Speed)
    ACFoll <- filter(ACProfile, This_Pair_Role == "F") %>% select(Observation_ID, FS = Steady_Procedural_Speed)
    Adjustments <- left_join(ACLeader, ACFoll, by = c("Observation_ID")) %>%
      mutate(!!sym(Adjust_Var) := LS - FS) %>% select(Observation_ID, !!sym(Adjust_Var))
    LP <- left_join(LP, Adjustments, by = c("Observation_ID"))
  } else {
    LP <- mutate(LP, !!sym(Adjust_Var) := 0)
  }
  
  # Get the Prefix names for the Three ORD Measurement Intervals. 
  ORDPrefix <- Get_ORD_Interval_Prefix("ORD")
  WADPrefix <- Get_ORD_Interval_Prefix("WAD")
  FullPrefix <- Get_ORD_Interval_Prefix("Full")
  
  # Compression Variables
  ORD_Comp_Var <- paste0(LegacyorRecat, "_Observed_", ORDPrefix, "_Compression")
  WAD_Comp_Var <- paste0(LegacyorRecat, "_Observed_", WADPrefix, "_Compression")
  Full_Comp_Var <- paste0(LegacyorRecat, "_Observed_", FullPrefix, "_Compression")
  
  # Observed Distance Variables
  Lead_ORD_Dist_Var <- paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_Flying_Distance") 
  Lead_WAD_Dist_Var <- paste0(LegacyorRecat, "_Observed_Leader_", WADPrefix, "_Flying_Distance")
  Lead_Full_Dist_Var <- paste0(LegacyorRecat, "_Observed_Leader_", FullPrefix, "_Flying_Distance")
  Foll_ORD_Dist_Var <- paste0(LegacyorRecat, "_Observed_Follower_", ORDPrefix, "_Flying_Distance")
  Foll_WAD_Dist_Var <- paste0(LegacyorRecat, "_Observed_Follower_", WADPrefix, "_Flying_Distance") 
  Foll_Full_Dist_Var <- paste0(LegacyorRecat, "_Observed_Follower_", FullPrefix, "_Flying_Distance") 
  
  # Observed IAS Variables
  Obs_Lead_ORD_IAS_Var <- paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_IAS") 
  Obs_Lead_WAD_IAS_Var <- paste0(LegacyorRecat, "_Observed_Leader_", WADPrefix, "_IAS")
  Obs_Lead_Full_IAS_Var <- paste0(LegacyorRecat, "_Observed_Leader_", FullPrefix, "_IAS") 
  Obs_Foll_ORD_IAS_Var <- paste0(LegacyorRecat, "_Observed_Follower_", ORDPrefix, "_IAS") 
  Obs_Foll_WAD_IAS_Var <- paste0(LegacyorRecat, "_Observed_Follower_", WADPrefix, "_IAS") 
  Obs_Foll_Full_IAS_Var <- paste0(LegacyorRecat, "_Observed_Follower_", FullPrefix, "_IAS") 
  
  # Forecast IAS Variables
  Pre_Lead_ORD_IAS_Var <- paste0(LegacyorRecat, "_Forecast_Leader_", ORDPrefix, "_IAS") 
  Pre_Lead_WAD_IAS_Var <- paste0(LegacyorRecat, "_Forecast_Leader_", WADPrefix, "_IAS")
  Pre_Lead_Full_IAS_Var <- paste0(LegacyorRecat, "_Forecast_Leader_", FullPrefix, "_IAS") 
  Pre_Foll_ORD_IAS_Var <- paste0(LegacyorRecat, "_Forecast_Follower_", ORDPrefix, "_IAS") 
  Pre_Foll_WAD_IAS_Var <- paste0(LegacyorRecat, "_Forecast_Follower_", WADPrefix, "_IAS")
  Pre_Foll_Full_IAS_Var <- paste0(LegacyorRecat, "_Forecast_Follower_", FullPrefix, "_IAS") 
  
  # Observed WE Variables
  Obs_Lead_ORD_WE_Var <- paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_Wind_Effect") 
  Obs_Lead_WAD_WE_Var <- paste0(LegacyorRecat, "_Observed_Leader_", WADPrefix, "_Wind_Effect")
  Obs_Lead_Full_WE_Var <- paste0(LegacyorRecat, "_Observed_Leader_", FullPrefix, "_Wind_Effect") 
  Obs_Foll_ORD_WE_Var <- paste0(LegacyorRecat, "_Observed_Follower_", ORDPrefix, "_Wind_Effect") 
  Obs_Foll_WAD_WE_Var <- paste0(LegacyorRecat, "_Observed_Follower_", WADPrefix, "_Wind_Effect")
  Obs_Foll_Full_WE_Var <- paste0(LegacyorRecat, "_Observed_Follower_", FullPrefix, "_Wind_Effect") 
  
  # Metric 1: Based on Observed Flying Distances.
  if (Metric_Type == 1){
    LP <- LP %>%
      mutate(!!sym(ORD_Comp_Var) := !!sym(Foll_ORD_Dist_Var) - !!sym(Lead_ORD_Dist_Var)) %>%
      mutate(!!sym(WAD_Comp_Var) := !!sym(Foll_WAD_Dist_Var) - !!sym(Lead_WAD_Dist_Var)) %>%
      mutate(!!sym(Full_Comp_Var) := !!sym(Foll_Full_Dist_Var) - !!sym(Lead_Full_Dist_Var))
  }
  
  # Metric 2: Based on Speeds. ORD assumes Forecast Follower IAS, WAD assumes Forecast Leader AND Follower IAS.
  if (Metric_Type == 2){
    
    LP <- LP %>%
      mutate(!!sym(ORD_Comp_Var) := (!!sym(Lead_ORD_Dist_Var) / (!!sym(Obs_Lead_ORD_IAS_Var) + !!sym(Obs_Lead_ORD_WE_Var))) *
               ((!!sym(Pre_Foll_ORD_IAS_Var) + !!sym(Adjust_Var) + !!sym(Obs_Foll_ORD_WE_Var)) - (!!sym(Obs_Lead_ORD_IAS_Var) + !!sym(Obs_Lead_ORD_WE_Var))),
             !!sym(WAD_Comp_Var) := (!!sym(Lead_WAD_Dist_Var) / (!!sym(Pre_Lead_WAD_IAS_Var) + !!sym(Obs_Lead_WAD_WE_Var))) *
               ((!!sym(Pre_Foll_WAD_IAS_Var) + !!sym(Adjust_Var) + !!sym(Obs_Foll_WAD_WE_Var)) - (!!sym(Pre_Lead_WAD_IAS_Var) + !!sym(Obs_Lead_WAD_WE_Var))),
             !!sym(Full_Comp_Var) := ifelse(is.na(!!sym(WAD_Comp_Var)) | is.nan(!!sym(WAD_Comp_Var)), 
                                            !!sym(ORD_Comp_Var),
                                            !!sym(ORD_Comp_Var) + !!sym(WAD_Comp_Var)))
  }
  
  return(LP)
  
}


Get_ORD_Error_Variables <- function(LP, LegacyorRecat){
  
  # Get the Prefix names for the Three ORD Measurement Intervals. 
  ORDPrefix <- Get_ORD_Interval_Prefix("ORD")
  WADPrefix <- Get_ORD_Interval_Prefix("WAD")
  FullPrefix <- Get_ORD_Interval_Prefix("Full")
  
  for (Prefix in c(ORDPrefix, WADPrefix, FullPrefix)){
    
    # Define Variables
    Pred_Comp_Var <- paste0(LegacyorRecat, "_Forecast_", Prefix, "_Compression")
    Obs_Comp_Var <- paste0(LegacyorRecat, "_Observed_", Prefix, "_Compression")
    Comp_Error_Var <- paste0(LegacyorRecat, "_", Prefix, "_Compression_Error")
    Pred_Lead_Spd_Var <- paste0(LegacyorRecat, "_Forecast_Leader_", Prefix, "_IAS")
    Pred_Foll_Spd_Var <- paste0(LegacyorRecat, "_Forecast_Follower_", Prefix, "_IAS")
    Pred_Lead_WE_Var <- paste0(LegacyorRecat, "_Forecast_Leader_", Prefix, "_Wind_Effect")
    Pred_Foll_WE_Var <- paste0(LegacyorRecat, "_Forecast_Follower_", Prefix, "_Wind_Effect")
    Obs_Lead_Spd_Var <- paste0(LegacyorRecat, "_Observed_Leader_", Prefix, "_IAS")
    Obs_Foll_Spd_Var <- paste0(LegacyorRecat, "_Observed_Follower_", Prefix, "_IAS")
    Obs_Lead_WE_Var <- paste0(LegacyorRecat, "_Observed_Leader_", Prefix, "_Wind_Effect")
    Obs_Foll_WE_Var <- paste0(LegacyorRecat, "_Observed_Follower_", Prefix, "_Wind_Effect")
    Lead_Spd_Error_Var <- paste0(LegacyorRecat, "_Forecast_Leader_", Prefix, "_IAS_Error")
    Foll_Spd_Error_Var <- paste0(LegacyorRecat, "_Forecast_Follower_", Prefix, "_IAS_Error")
    Lead_WE_Error_Var <- paste0(LegacyorRecat, "_Forecast_Leader_", Prefix, "_Wind_Effect_Error")
    Foll_WE_Error_Var <- paste0(LegacyorRecat, "_Forecast_Follower_", Prefix, "_Wind_Effect_Error")
    
    # Mutate Variables
    LP <- mutate(LP,
                 !!sym(Comp_Error_Var) := !!sym(Obs_Comp_Var) - !!sym(Pred_Comp_Var),
                 !!sym(Lead_Spd_Error_Var) := !!sym(Obs_Lead_Spd_Var) - !!sym(Pred_Lead_Spd_Var),
                 !!sym(Foll_Spd_Error_Var) := !!sym(Obs_Foll_Spd_Var) - !!sym(Pred_Foll_Spd_Var),
                 !!sym(Lead_WE_Error_Var) := !!sym(Obs_Lead_WE_Var) - !!sym(Pred_Lead_WE_Var),
                 !!sym(Foll_WE_Error_Var) := !!sym(Obs_Foll_WE_Var) - !!sym(Pred_Foll_WE_Var))
    
  }
  
  return(LP)
  
}


Generate_Forecast_ORD_Results <- function(LP, ORD_GS_Profile, LP_Primary_Key, Forecast_Compression_Type, LegacyorRecat){
  
  # Get the Prefix names for the Three ORD Measurement Intervals. 
  ORDPrefix <- Get_ORD_Interval_Prefix("ORD")
  WADPrefix <- Get_ORD_Interval_Prefix("WAD")
  FullPrefix <- Get_ORD_Interval_Prefix("Full")
  
  # Get the New or Old based on Recat/Legacy 
  NeworOld <- ifelse(LegacyorRecat == "Recat", "New", "Old")
  
  ORD_Sep_Var_In <- paste0(LegacyorRecat, "_Threshold_All_Separation_Distance")
  ORD_Sep_Var_Out <- paste0(LegacyorRecat, "_", ORDPrefix, "_Separation_Distance")
  ORD_Comp_Var <- paste0(LegacyorRecat, "_Forecast_", ORDPrefix, "_Compression")
  WAD_Sep_Var <- paste0(LegacyorRecat, "_", WADPrefix, "_Separation_Distance")
  
  ORD_Comp_End_Var <- paste0(NeworOld, "_Delivery")
  ORD_Comp_Start_Var <- "FAF_Distance"
  WAD_Comp_End_Var <- "FAF_Distance"
  WAD_Comp_Start_Var <- paste0(LegacyorRecat, "_Leader_CC_Distance")
  
  # Get the FAF Distance and the ORD Separation Distance in correct format
  LP <- LP %>% mutate(!!sym(ORD_Sep_Var_Out) := !!sym(ORD_Sep_Var_In),
                      FAF_Distance = Leader_FAF_Distance)
  
  # Calculate Full ORD from Compression Commencement to Delivery Point
  LP <- Get_Forecast_ORD_Parameters(ORD_GS_Profile, LP, LP_Primary_Key,
                                    Prefix = FullPrefix,
                                    Comp_End_Var = ORD_Comp_End_Var,
                                    Comp_Start_Var = WAD_Comp_Start_Var,
                                    Sep_Dist_Var = ORD_Sep_Var_Out,
                                    Forecast_Compression_Type,
                                    LegacyorRecat)
  
  # Calculate the ORD Portion from the FAF to the Delivery Point
  LP <- Get_Forecast_ORD_Parameters(ORD_GS_Profile, LP, LP_Primary_Key,
                                    Prefix = ORDPrefix,
                                    Comp_End_Var = ORD_Comp_End_Var,
                                    Comp_Start_Var = ORD_Comp_Start_Var,
                                    Sep_Dist_Var = ORD_Sep_Var_Out,
                                    Forecast_Compression_Type,
                                    LegacyorRecat)
  
  # Get the WAD Separation Distance as the ORD Separation Distance + ORD Portion Compression
  LP <- LP %>% mutate(!!sym(WAD_Sep_Var) := !!sym(ORD_Sep_Var_Out) + !!sym(ORD_Comp_Var))
  
  # Calculate the WAD Portion from the Compression Commencement to the FAF
  LP <- Get_Forecast_ORD_Parameters(ORD_GS_Profile, LP, LP_Primary_Key,
                                    Prefix = WADPrefix,
                                    Comp_End_Var = WAD_Comp_End_Var,
                                    Comp_Start_Var = WAD_Comp_Start_Var,
                                    Sep_Dist_Var = WAD_Sep_Var,
                                    Forecast_Compression_Type,
                                    LegacyorRecat)
  
  
  return(LP)
  
}


Generate_TBS_Indicator_Distances_Threshold <- function(LP, Delivery_Points, Delivery_Column, TBSC_Profiles_ROT, TBSC_Profiles_Wake,
                                                       Seg_Size, LegacyorRecat){
  
  # Initialise List of Distance Parameter names
  Dist_Values <- c()
  
  # Change to Delivery Distance field to match this operation.
  LP <- mutate(LP, Delivery_Distance = !!sym(Delivery_Column))
  
  ## Generate the Distance for all Constraints at Threshold
  for (Constraint in Constraints){
    if (Constraint == "ROT"){TBSC_Profiles <- TBSC_Profiles_ROT} else {TBSC_Profiles <- TBSC_Profiles_Wake}
    Constraint_Delivery <- filter(Delivery_Points, SASAI_Constraint == Constraint) %>% select(!!sym(Delivery_Column)) %>% as.character()
    LP <- Generate_Constraint_Spacing_All(con, LPR = LP, TBSC_Profile = TBSC_Profiles,
                                          TTB_Type, Constraint_Type = Constraint,
                                          Constraint_Delivery, Evaluated_Delivery = "THRESHOLD",
                                          ID_Var = LP_Primary_Key,
                                          Time_Var = Get_Reference_Variable_Name(Constraint, "Time", LegacyorRecat),
                                          Speed_Var = Get_Reference_Variable_Name(Constraint, "IAS", LegacyorRecat),
                                          Seg_Size, LegacyorRecat)
    Dist_Name_Var <- paste0(Get_Constraint_Prefix(Constraint, "THRESHOLD", "TBS", LegacyorRecat), "_Distance")
    if (Dist_Name_Var %in% names(LP)){
      Dist_Values <- append(Dist_Values, Dist_Name_Var)
    }
    rm(Dist_Name_Var)
  }
  
  # Set names for the maximum separation distance and it's assosciated constraint
  All_Sep_Var <- paste0(LegacyorRecat, "_Threshold_All_Separation_Distance")
  Thresh_Max_Constraint_Var <- paste0(LegacyorRecat, "_Threshold_Max_Constraint")
  
  # Get the maximum separation distance
  LP <- LP %>%
    mutate(!!sym(All_Sep_Var) := pmax(!!!syms(Dist_Values), na.rm=T))
  
  # Get the Max Constraint (Threshold)
  LP <- mutate(LP, !!sym(Thresh_Max_Constraint_Var) := NA)
  for (c in 1:length(Constraints)){
    Dist_Name_Var <- paste0(Get_Constraint_Prefix(Constraints[c], "THRESHOLD", "TBS", LegacyorRecat), "_Distance")
    if (Dist_Name_Var %in% Dist_Values){
      LP <- LP %>% 
        mutate(!!sym(Thresh_Max_Constraint_Var) := ifelse(!is.na(!!sym(Dist_Name_Var)) & abs(!!sym(Dist_Name_Var) - !!sym(All_Sep_Var)) < 0.01, Constraints[c], !!sym(Thresh_Max_Constraint_Var)))
    }
  } 
  
  rm(Dist_Name_Var)
  
  # Get the "Winning" Assumed IAS and Forecast WE - TEMP: will need to Generalise in case of FAF Constraint delivery
  Wake_Prefix <- Get_Constraint_Prefix("Wake", "THRESHOLD", "TBS", LegacyorRecat)
  ROT_Prefix <- Get_Constraint_Prefix("ROT", "THRESHOLD", "TBS", LegacyorRecat)
  LP <- LP %>%
    mutate(!!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS")) := NA,
           !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect")) := NA,
           !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "Wake", !!sym(paste0(Wake_Prefix, "_IAS")), !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS"))),
           !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "ROT", !!sym(paste0(ROT_Prefix, "_IAS")), !!sym(paste0(LegacyorRecat, "_Follower_Assumed_IAS"))),
           !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "Wake", !!sym(paste0(Wake_Prefix, "_Wind_Effect")), !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect"))),
           !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect")) := ifelse(!!sym(Thresh_Max_Constraint_Var) == "ROT", !!sym(paste0(ROT_Prefix, "_Wind_Effect")), !!sym(paste0(LegacyorRecat, "_Follower_Forecast_Wind_Effect"))))
  
  return(LP)
  
}


Generate_TBS_Indicator_Distances_FAF <- function(LP, Delivery_Points, Delivery_Column, TBSC_Profiles_ROT, TBSC_Profiles_Wake,
                                                 Seg_Size, LegacyorRecat){
  
  # Initialise List of Distance Parameter names
  Dist_Values <- c()
  
  # Now get the Distances + Compression (If viable!)
  for (Constraint in Constraints){
    if (Constraint == "ROT"){TBSC_Profiles <- TBSC_Profiles_ROT} else {TBSC_Profiles <- TBSC_Profiles_Wake}
    Constraint_Delivery <- filter(Delivery_Points, SASAI_Constraint == Constraint) %>% select(!!sym(Delivery_Column)) %>% as.character()
    LP <- Generate_Constraint_Spacing_All(con, LPR = LP, TBSC_Profile = TBSC_Profiles,
                                          TTB_Type, Constraint_Type = Constraint,
                                          Constraint_Delivery, Evaluated_Delivery = "FAF",
                                          ID_Var = LP_Primary_Key,
                                          Time_Var = Get_Reference_Variable_Name(Constraint, "Time", LegacyorRecat),
                                          Speed_Var = Get_Reference_Variable_Name(Constraint, "IAS", LegacyorRecat),
                                          Seg_Size, LegacyorRecat)
    if (paste0(Get_Constraint_Prefix(Constraint, "FAF", "TBS", LegacyorRecat), "_Distance") %in% names(LP)){
      Dist_Values <- append(Dist_Values, paste0(Get_Constraint_Prefix(Constraint, "FAF", "TBS", LegacyorRecat), "_Distance"))
    }
  }
  
  ## Get the Maximum Distance FAF Constraint
  All_Sep_Var <- paste0(LegacyorRecat, "_FAF_All_Separation_Distance")
  FAF_Max_Constraint_Var <- paste0(LegacyorRecat, "_FAF_Max_Constraint")
  LP <- LP %>%
    mutate(!!sym(All_Sep_Var) := pmax(!!!syms(Dist_Values), na.rm=T))
  
  # Get the Max Constraint (FAF)
  LP <- mutate(LP, !!sym(FAF_Max_Constraint_Var) := NA)
  for (c in 1:length(Constraints)){
    Dist_Name_Var <- paste0(Get_Constraint_Prefix(Constraints[c], "FAF", "TBS", LegacyorRecat), "_Distance")
    if (Dist_Name_Var %in% Dist_Values){
      LP <- LP %>% 
        mutate(!!sym(FAF_Max_Constraint_Var) := ifelse(!is.na(!!sym(Dist_Name_Var)) & !!sym(Dist_Name_Var) == !!sym(All_Sep_Var), Constraints[c], !!sym(FAF_Max_Constraint_Var)))
    }
    
  }
  rm(Dist_Name_Var)
  
  return(LP)
  
}

# Needs Revision.
Generate_TBS_Assumed_Separation_Accuracy <- function(LP, LegacyorRecat){
  
  if (LegacyorRecat == "Legacy"){LP <- mutate(LP, Observed_4DME_Separation_Accuracy = Observed_4DME_Separation_Distance - Legacy_FAF_Wake_Separation_Distance)}
  RSA <- 0.2 * NM_to_m
  Prototyping <- T
  LP <- LP %>% mutate(!!sym(paste0(LegacyorRecat, "_Required_Separation_Accuracy")) := ifelse(!Prototyping & LegacyorRecat == "Legacy", Observed_4DME_Separation_Accuracy, RSA))
 
  return(LP)
  
}

# Needs Revision.
Generate_TBS_Actual_Separation_Distances <- function(LP, DistInput, DistOutput, LegacyorRecat){
  
  for (i in 1:length(DistInput)){
    LP <- LP %>% mutate(!!sym(DistOutput[i]) := !!sym(DistInput[i]) + !!sym(paste0(LegacyorRecat, "_Required_Separation_Accuracy")))
  }
  
  return(LP)
  
}


Generate_TBS_Separation_Scenario_Parameters <- function(LP, Delivery_Column, UnderSeps, ORDInterval, LegacyorRecat,
                                                        PerfDistInput, PerfTimeOutput, PerfSpeedOutput, ActDistInput, ActTimeOutput, ActSpeedOutput){
  
  # Determines whether Actual Distances are based on Leader @ FAF or CC
  ORDPrefix <- Get_ORD_Interval_Prefix(ORDInterval)

  # Get Observed Speed/IAS
  for (Trail in c("Not_In_Trail", "In_Trail", "In_Trail_Non_Sequential")){
    
    # Filter for Trail Type
    LP_Trail <- filter(LP, Landing_Pair_Type == Trail)
    
    # Loop across required Under Separation Values
    for (UnderSep in UnderSeps){
      
      # Construct the variable suffix based on Under Sep Value
      USString <- ""
      if (UnderSep != 0){USString <- paste0("_US", UnderSep / NM_to_m)}
      
      # Create copies of the Output names to reflext US requirements
      PerfTimeOutputUS <- gsub("_Time", paste0(USString, "_Time"), PerfTimeOutput)
      PerfSpeedOutputUS <- paste0(PerfSpeedOutput, USString)
      ActTimeOutputUS <- gsub("_Time", paste0(USString, "_Time"), ActTimeOutput)
      ActSpeedOutputUS <- paste0(ActSpeedOutput, USString)
      
      # Loop across the different scenarios (PERFECT).
      for (i in 1:length(PerfDistInput)){
        
        # Get required variable names
        Time_Var <- PerfTimeOutputUS[i]
        IAS_Var <- paste0(PerfSpeedOutputUS[i], "_IAS")
        WE_Var <- paste0(PerfSpeedOutputUS[i], "_Wind_Effect")
        Dist_Var <- PerfDistInput[i]
        
        # Initialise the Start/End Variables for the Observed parameter calculation (Perfect - Follower From Delivery to Delivery plus Sep Distance)
        LP_Trail <- mutate(LP_Trail, Start_Var := !!sym(Delivery_Column) + !!sym(Dist_Var) - UnderSep, End_Var := !!sym(Delivery_Column))
        
        # Generate the observed follower parameters
        LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                             Prefix = "NULL",
                                                             LorF = "Follower",
                                                             TimeorRange = "Range",
                                                             Start_Var = "Start_Var",
                                                             End_Var = "End_Var",
                                                             LegacyorRecat,
                                                             MaxUnderRep,
                                                             LP_Primary_Key,
                                                             PerfSpeedOutputUS[i])
        
        # Calculate the Perfect Time Separations.
        LP_Trail <- mutate(LP_Trail, !!sym(Time_Var) := (!!sym(Dist_Var) - UnderSep) / (!!sym(IAS_Var) + !!sym(WE_Var)))
        
      }
      
      # Loop across the different scenarios (ACTUAL)
      for (i in 1:length(ActDistInput)){
        
        # Get required variable names
        Time_Var <- ActTimeOutputUS[i]
        Foll_IAS_Var <- paste0(ActSpeedOutputUS[i], "_IAS")
        Foll_WE_Var <- paste0(ActSpeedOutputUS[i], "_Wind_Effect")
        Foll_Dist_Var <- ActDistInput[i]
        Lead_IAS_Var <- paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_IAS")
        Lead_WE_Var <- paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_Wind_Effect")
        Lead_Dist_Var <- paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_Flying_Distance")
        
        # Get the Leader start distance  variable based on the ORD Interval
        if (ORDInterval == "Full"){LeaderStartDistance <- paste0(LegacyorRecat, "_Leader_CC_Distance")} else {LeaderStartDistance <- "Leader_FAF_Distance"}
        
        # Initialise the Start/End Variables for the Observed parameter calculation (Actual - Follower From Delivery to Leader Start plus Sep Distance)
        LP_Trail <- mutate(LP_Trail, Start_Var := !!sym(LeaderStartDistance) + !!sym(Foll_Dist_Var) - UnderSep, End_Var := !!sym(Delivery_Column))
        
        # Generate the Observed Follower parameters
        LP_Trail <- Get_Average_Observed_Mode_S_Parameters_2(LP_Trail, Radar,
                                                             Prefix = "NULL",
                                                             LorF = "Follower",
                                                             TimeorRange = "Range",
                                                             Start_Var = "Start_Var",
                                                             End_Var = "End_Var",
                                                             LegacyorRecat,
                                                             MaxUnderRep,
                                                             LP_Primary_Key,
                                                             ActSpeedOutputUS[i])
        
        # Calculate the Actual time separations using Leader ORD Interval information and the calculated follower parameters
        LP_Trail <- LP_Trail %>%
          mutate(!!sym(Time_Var) := ((!!sym(LeaderStartDistance) + !!sym(Foll_Dist_Var) - UnderSep) /
                                       (!!sym(Foll_IAS_Var) + !!sym(Foll_WE_Var))) - (!!sym(Lead_Dist_Var) / (!!sym(Lead_IAS_Var) + !!sym(Lead_WE_Var))))
        
      }
      
    }
    
    if (!exists("LP_New")){LP_New <- LP_Trail} else {LP_New <- rbind(LP_New, LP_Trail)}
    
  }
  
  return(LP_New)
  
}


ConstructORDObservation <- function(con, LPORD, LegacyorRecat, Testing, OpTables, PopTables){
  
  # Original SQL Table Name
  SQLTable <- "tbl_ORD_Observation"
  
  # Get the ORD Prefix
  ORDPrefix <- Get_ORD_Interval_Prefix("ORD")
  
  # Construct Table
  Table <- LPORD %>%
    select(Landing_Pair_ID,
           Observed_Compression := !!sym(paste0(LegacyorRecat, "_Observed_", ORDPrefix, "_Compression")),
           Observed_Mean_Leader_IAS := !!sym(paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_IAS")),
           Observed_Mean_Follower_IAS := !!sym(paste0(LegacyorRecat, "_Observed_Follower_", ORDPrefix, "_IAS")),
           Observed_Mean_Leader_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Observed_Leader_", ORDPrefix, "_Wind_Effect")),
           Observed_Mean_Follower_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Observed_Follower_", ORDPrefix, "_Wind_Effect")),
           Observed_AGI_Surface_Headwind,
           Observation_Date = Landing_Pair_Date,
           Leader_FAF_Time = Leader_FAF_1_Time,
           Leader_0DME_Time := !!sym(paste0(LegacyorRecat, "_Leader_Delivery_Time")),
           Leader_FAF_RTT = Leader_FAF_1_RTT,
           Leader_0DME_RTT := !!sym(paste0(LegacyorRecat, "_Leader_Delivery_RTT")),
           Follower_Start_RTT := !!sym(paste0(LegacyorRecat, "_Follower_Int_FAF_1_RTT")),
           Follower_Stop_RTT := !!sym(paste0(LegacyorRecat, "_Follower_Int_Delivery_RTT")),
           Leader_Callsign,
           Follower_Callsign,
           Follower_0DME_Time := !!sym(paste0(LegacyorRecat, "_Follower_Delivery_RTT")),
           Follower_0DME_RTT := !!sym(paste0(LegacyorRecat, "_Follower_Delivery_Time")),
           Follower_Time_At_4DME,
           Landing_Runway = Leader_Landing_Runway,
           Delivered_FAF_Separation := !!sym(paste0(LegacyorRecat, "_Observed_FAF_Separation_Distance")))
  
  # Are we populating this Table?
  if (PopTables[which(OpTables == SQLTable)[[1]]]){
    PopulateSQLTableOperation(con, SQLTable, Table, LegacyorRecat, c("Landing_Pair_ID"), Testing)
  }
  
  return(Table)
  
}


ConstructORDPrediction <- function(con, LPORD, LegacyorRecat, Testing , OpTables, PopTables){
  
  # Original SQL Table Name
  SQLTable <- "tbl_ORD_Prediction"
  
  # Get the ORD Prefix
  ORDPrefix <- Get_ORD_Interval_Prefix("ORD")
  
  # Construct Table
  Table <- LPORD %>%
    select(Landing_Pair_ID,
           ORD_Compression := !!sym(paste0(LegacyorRecat, "_Forecast_", ORDPrefix, "_Compression")),
           ORD_Compression_Error := !!sym(paste0(LegacyorRecat, "_", ORDPrefix, "_Compression_Error")),
           ORD_Mean_Leader_IAS := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", ORDPrefix, "_IAS")),
           ORD_Leader_IAS_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", ORDPrefix, "_IAS_Error")),
           ORD_Mean_Follower_IAS := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", ORDPrefix, "_IAS")),
           ORD_Follower_IAS_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", ORDPrefix, "_IAS_Error")),
           Forecast_Mean_Leader_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", ORDPrefix, "_Wind_Effect")),
           Forecast_Mean_Leader_Wind_Effect_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", ORDPrefix, "_Wind_Effect_Error")),
           Forecast_Mean_Follower_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", ORDPrefix, "_Wind_Effect")),
           Forecast_Mean_Follower_Wind_Effect_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", ORDPrefix, "_Wind_Effect_Error")),
           Forecast_AGI_Surface_Headwind,
           Forecast_AGI_Surface_Headwind_Error,
           Prediction_Time,
           Leader_Distance_To_Threshold = Leader_FAF_Distance,
           ORD_Separation_Distance := !!sym(paste0(LegacyorRecat, "_", ORDPrefix, "_Separation_Distance")),
           DBS_All_Sep_Distance := !!sym(paste0(LegacyorRecat, "_DBS_All_Sep_Distance")))
  
  # Are we populating this Table?
  if (PopTables[which(OpTables == SQLTable)[[1]]]){
    PopulateSQLTableOperation(con, SQLTable, Table, LegacyorRecat, c("Landing_Pair_ID"), Testing)
  }
  
 return(Table) 
  
}


ConstructWADObservation <- function(con, LPORD, LegacyorRecat, Testing, OpTables, PopTables){
  
  # Original SQL Table Name
  SQLTable <- "tbl_WAD_Observation"
  
  # Get the WAD Prefix
  WADPrefix <- Get_ORD_Interval_Prefix("WAD")
  
  # Construct Table
  Table <- LPORD %>%
    select(Landing_Pair_ID,
           Observed_Compression := !!sym(paste0(LegacyorRecat, "_Observed_", WADPrefix, "_Compression")),
           Observed_Mean_Leader_IAS := !!sym(paste0(LegacyorRecat, "_Observed_Leader_", WADPrefix, "_IAS")),
           Observed_Mean_Follower_IAS := !!sym(paste0(LegacyorRecat, "_Observed_Follower_", WADPrefix, "_IAS")),
           Observed_Mean_Leader_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Observed_Leader_", WADPrefix, "_Wind_Effect")),
           Observed_Mean_Follower_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Observed_Follower_", WADPrefix, "_Wind_Effect")),
           Observed_AGI_Surface_Headwind,
           Observation_Date = Landing_Pair_Date,
           Leader_CCT_Time := !!sym(paste0(LegacyorRecat, "_Leader_CC_Time")),
           Leader_FAF_Time = Leader_FAF_2_Time,
           Leader_CCT_RTT := !!sym(paste0(LegacyorRecat, "_Leader_CC_RTT")),
           Leader_FAF_RTT = Leader_FAF_2_RTT,
           Follower_Start_RTT := !!sym(paste0(LegacyorRecat, "_Follower_Int_CC_RTT")),
           Follower_Stop_RTT := !!sym(paste0(LegacyorRecat, "_Follower_Int_FAF_2_RTT")),
           Leader_Callsign,
           Follower_Callsign,
           Follower_FAF_Time,
           Follower_FAF_RTT,
           Follower_Time_At_4DME,
           Landing_Runway = Leader_Landing_Runway,
           Delivered_CCT_Separation := !!sym(paste0(LegacyorRecat, "_Observed_CC_Separation_Distance")))
  
  # Are we populating this Table?
  if (PopTables[which(OpTables == SQLTable)[[1]]]){
    PopulateSQLTableOperation(con, SQLTable, Table, LegacyorRecat, c("Landing_Pair_ID"), Testing)
  }
  
  return(Table) 
  
}


ConstructWADPrediction <- function(con, LPORD, LegacyorRecat, Testing, OpTables, PopTables){
  
  # Original SQL Table Name
  SQLTable <- "tbl_WAD_Prediction"
  
  # Get the WAD Prefix
  WADPrefix <- Get_ORD_Interval_Prefix("WAD")
  
  # Construct Table
  Table <- LPORD %>%
    select(Landing_Pair_ID,
           WAD_Compression := !!sym(paste0(LegacyorRecat, "_Forecast_", WADPrefix, "_Compression")),
           WAD_Compression_Error := !!sym(paste0(LegacyorRecat, "_", WADPrefix, "_Compression_Error")),
           WAD_Mean_Leader_IAS := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", WADPrefix, "_IAS")),
           WAD_Leader_IAS_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", WADPrefix, "_IAS_Error")),
           WAD_Mean_Follower_IAS := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", WADPrefix, "_IAS")),
           WAD_Follower_IAS_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", WADPrefix, "_IAS_Error")),
           Forecast_Mean_Leader_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", WADPrefix, "_Wind_Effect")),
           Forecast_Mean_Leader_Wind_Effect_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Leader_", WADPrefix, "_Wind_Effect_Error")),
           Forecast_Mean_Follower_Wind_Effect := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", WADPrefix, "_Wind_Effect")),
           Forecast_Mean_Follower_Wind_Effect_Error := !!sym(paste0(LegacyorRecat, "_Forecast_Follower_", WADPrefix, "_Wind_Effect_Error")),
           Forecast_AGI_Surface_Headwind,
           Forecast_AGI_Surface_Headwind_Error,
           Prediction_Time,
           Leader_Distance_To_Threshold := !!sym(paste0(LegacyorRecat, "_Leader_CC_RTT")),
           WAD_Separation_Distance := !!sym(paste0(LegacyorRecat, "_WAD_Separation_Distance")),
           DBS_All_Sep_Distance := !!sym(paste0(LegacyorRecat, "_DBS_All_Sep_Distance")))
  
  # Are we populating this Table?
  if (PopTables[which(OpTables == SQLTable)[[1]]]){
    PopulateSQLTableOperation(con, SQLTable, Table, LegacyorRecat, c("Landing_Pair_ID"), Testing)
  }
  
  return(Table) 
  
  
}

# only works for validation atm
ConstructORDACProfile <- function(con, ORD_AC_Profile, LPORD, LP_IDs, LegacyorRecat, Testing, OpTables, PopTables){
  
  # Original SQL Table Name
  SQLTable <- "tbl_ORD_Aircraft_Profile"
  
  # Construct table.
  Table <- left_join(ORD_AC_Profile, LP_IDs, by = c("Observation_ID")) %>%
    filter(Observation_ID %in% LPORD$Observation_ID) %>%
    select(Landing_Pair_ID,
           This_Pair_Role,
           Aircraft_Type,
           Operator,
           Wake_Cat,
           Runway = Landing_Runway,
           VRef,
           Apply_Gusting,
           Landing_Stabilisation_Speed_Type,
           Local_Stabilisation_Distance,
           Compression_Commencement_Threshold,
           Landing_Stabilisation_Speed,
           Final_Deceleration,
           End_Final_Deceleration_Distance,
           Start_Final_Deceleration_Distance,
           Steady_Procedural_Speed,
           Initial_Deceleration,
           End_Initial_Deceleration_Distance,
           Start_Initial_Deceleration_Distance,
           Initial_Procedural_Speed)
  
  # Are we populating this Table?
  if (PopTables[which(OpTables == SQLTable)[[1]]]){
    PopulateSQLTableOperation(con, SQLTable, Table, LegacyorRecat, c("Landing_Pair_ID"), Testing)
  }
  
  return(Table) 
  
}

ConstructORDIASProfile <- function(con, ORD_IAS_Profile, LPORD, LP_IDs, LegacyorRecat, Testing, OpTables, PopTables){
  
  # Original SQL Table Name
  SQLTable <- "tbl_ORD_IAS_Profile"
  
  # Construct table.
  Table <- left_join(ORD_IAS_Profile, LP_IDs, by = c("Observation_ID")) %>%
    filter(Observation_ID %in% LPORD$Observation_ID) %>%
    select(Landing_Pair_ID,
           This_Pair_Role,
           Section_Number,
           Profile_Section,
           Profile_Type,
           Start_IAS,
           End_IAS,
           Start_Dist,
           End_Dist)
  
  # Are we populating this Table?
  if (PopTables[which(OpTables == SQLTable)[[1]]]){
    PopulateSQLTableOperation(con, SQLTable, Table, LegacyorRecat, c("Landing_Pair_ID"), Testing)
  }
  
  return(Table) 
  
}

ConstructORDGSProfile <- function(con, ORD_GS_Profile, LPORD, LP_IDs, LegacyorRecat, Testing, OpTables, PopTables){
  
  # Original SQL Table Name
  SQLTable <- "tbl_ORD_GS_Profile"
  
  # construct table.
  Table <- left_join(ORD_GS_Profile, LP_IDs, by = c("Observation_ID")) %>%
    filter(Observation_ID %in% LPORD$Observation_ID) %>%
    select(Landing_Pair_ID,
           This_Pair_Role,
           Section_Number,
           Profile_Section,
           Profile_Type,
           Start_IAS,
           End_IAS,
           Start_GS,
           End_GS,
           Start_Dist,
           End_Dist)
  
  # Are we populating this Table?
  if (PopTables[which(OpTables == SQLTable)[[1]]]){
    PopulateSQLTableOperation(con, SQLTable, Table, LegacyorRecat, c("Landing_Pair_ID"), Testing)
  }
  
  return(Table) 
  
}

