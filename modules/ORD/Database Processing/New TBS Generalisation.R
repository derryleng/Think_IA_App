Generate_Observed_DME_Parameters <- function(LP, FP, Radar, DMEs){
  
  # Loop across all DMEs.
  for (DME in DMEs){
    message("Generating Observed ", DME, "DME Parameters...")
    
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
      
      # Bind Results Together
      if (exists("LPAll")){LPAll <- rbind(LPAll, LPTrail)} else {LPAll <- LPTrail}
      
    }
    
    # Get the Observed Separation Distance and Time Values
    LP <- LPAll %>% 
      mutate(!!sym(paste0("Observed_", DME, "DME_Separation_Distance")) := !!sym(paste0("Follower_Int_", DME, "DME_RTT")) - !!sym(paste0("Leader_", DME, "DME_RTT"))) %>%
      mutate(!!sym(paste0("Observed_", DME, "DME_Separation_Time")) := !!sym(paste0("Follower_", DME, "DME_Time")) - !!sym(paste0("Leader_", DME, "DME_Time")))
    
  }
  
  return(LP)
  
}

Generate_ORD_Input_Parameters <- function(LP, FP, Radar, ACPRofile, ORDRunways, DeliveryPoint, LegacyorRecat){
  
  # From LegacyorRecat, get New or Old name
  NeworOld <- ifelse(LegacyorRecat == "Recat", "New", "Old")
  
  # Add on Delivery Point variable.
  LP <- mutate(LP, !!sym(paste0(NeworOld, "_Delivery")) := DeliveryPoint)
  
  # Get Max RTTs.
  MaxRTTs <- Get_Max_RTTs(Radar, "Range_To_Threshold")
  
  # Get the Distances present on an Aircraft Type Level. (FAF, Delivery, Max RTT) START FROM LEADER.
  FPL <- FP %>%
    left_join(MaxRTTs, by = c("Flight_Plan_ID")) %>%
    Get_FAF_Distances(ORDRunways) %>%
    Get_FAF_RTTs(Radar, N = 1) %>%
    Get_FAF_RTTs(Radar, N = 2) %>%
    Get_Delivery_RTTs(Radar, DeliveryPoint, LegacyorRecat) %>%
    select(Flight_Plan_ID, Max_RTT, FAF_Distance, Leader_FAF_1_RTT, Leader_FAF_1_Time, Leader_FAF_2_RTT, Leader_FAF_2_Time,
           !!sym(paste0("Leader_", LegacyorRecat, "_Delivery_RTT")), !!sym(paste0("Leader_", LegacyorRecat, "_Delivery_Time"))) %>%
    rename(Leader_FAF_Distance = FAF_Distance) %>%
    rename(Leader_Max_RTT = Max_RTT)
    
  # Now rename variables to get the follower.
  FPF <- FPL %>%
    rename(Follower_FAF_1_RTT = Leader_FAF_1_RTT, Follower_FAF_1_Time = Leader_FAF_1_Time,
           Follower_FAF_2_RTT = Leader_FAF_2_RTT, Follower_FAF_2_Time = Leader_FAF_2_Time,
           !!sym(paste0("Follower_", LegacyorRecat, "_Delivery_RTT")) := !!sym(paste0("Leader_", LegacyorRecat, "_Delivery_RTT")),
           !!sym(paste0("Follower_", LegacyorRecat, "_Delivery_Time")) := !!sym(paste0("Leader_", LegacyorRecat, "_Delivery_Time")),
           Follower_FAF_Distance = Leader_FAF_Distance, Follower_Max_RTT = Leader_Max_RTT)
  
  # Join on the Aircraft Level Parameters.
  LP <- LP %>%
    left_join(FPL, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>%
    left_join(FPF, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
  
  # For legacy IA, take Follower FAF RTT/Time as Follower_FAF_1.
  LP <- LP %>%
    mutate(Follower_FAF_RTT = Follower_FAF_1_RTT, Follower_FAF_Time = Follower_FAF_1_Time)
  
  # Now get the LP CCT/CC Distances etc.
  LP <- LP %>%
    Get_CCT_Distances(ACProfile, LP_Primary_Key, LegacyorRecat) %>%
    Get_CCT_RTTs(Radar, LegacyorRecat) %>%
    Get_CC_RTTs(LegacyorRecat) %>%
    Get_CC_Distances(LegacyorRecat)
  
  # Get the Interpolated Follower ORD/WAD Distances
  
  
}