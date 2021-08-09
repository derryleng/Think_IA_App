
Get_TBS_Service_Level <- function(con, LPR, ID_Var){
  
  if (ID_Var != Get_LP_Primary_Key("Verification")){
    if (Load_Adaptation_Table(con, "tbl_Adaptation_Data")$ORD_Profile_Selection == "TBS_Table"){
      LPR <- mutate(LPR, TBS_Service_Level = "DBS")
    } else {LPR <- mutate(LPR, TBS_Service_Level = "TBS")}
  }
  
  return(LPR)
  
}

Get_In_Trail_Spacing <- function(){}

Get_RNAV_Flag <- function(LPR){
  
  LPR <- mutate(LPR, RNAV_Flag = 0)
  return(LPR)
}

Get_Non_Wake_Spacing <- function(LPR){
  
  LPR <- mutate(LPR, 
                Non_Wake_RNAV_Separation_Distance = (2.5*1852),
                Non_Wake_Separation_Distance = (2.5*1852))
  return(LPR)
  
}

Get_Runway_Dependent_Separation <- function(LPR){
  
  LPR <- mutate(LPR, Runway_Dependent_Separation_Distance = 1 * NM_to_m)
  return(LPR)
  
}

# Function to extract the Time buffers from adaptation. 
# Returns A time buffer for each landing pair (i.e. each follower)
# Returns based on Aircraft Type, Wake, DBS Precedence (Essentially mimicing the ORD_Profile_Type = "Aircraft_Type")
# Currently requires "Active" argument which turns all buffers on or off
# If not "Active" returns a buffer of 0 for each Landing Pair/Follower
#### REDUNDANT
Generate_TBSC_Time_Buffers <- function(con, LPR, ID_Var, Active){
  
  if (Active){
    
    
    
  } else {
    
    Buffers <- select(LPR, !!sym(ID_Var)) %>% unique() %>% mutate(Time_Buffer = 0)
    message("Setting Time buffers to 0 by default.")
    
  }
  
  return(Buffers)
  
}


# Function that generates the required extra data for calculating TBS Distances
# This is dependent on the method of TBS Distance calculations
# "Original" requires the full wind forecast as is used currently
# "ORD" and "T2F" TTB variants require calculation of a GSPD profile 
# Not yet completed but: These two should use different methods to provide similar outputs
# This way the Distances can be calculated using the ORD Follower calculations as mentioned in Sprints.
Generate_TBSC_Profiles <- function(con, LPR, Full_Wind_Forecast, ID_Var, TTB_Type, Use_EFDD, Use_Gust_Data, Precedences, Levels, LegacyorRecat, SymmetryWake){
  
  if (TTB_Type == "Original"){TBSC_Profile <- Full_Wind_Forecast}
  if (TTB_Type == "ORD"){TBSC_Profile <- Generate_TTB_ORD_GSPD_Profile(con, LPR, Full_Wind_Forecast, ID_Var, Use_EFDD, Use_Gust_Data, Precedences, Levels, LegacyorRecat, SymmetryWake)}
  if (TTB_Type == "T2F"){TBSC_Profile <- Generate_TTB_T2F_GSPD_Profile(con, LPR, Full_Wind_Forecast, ID_Var, Seg_Size = 1852, Precedences, Levels, LegacyorRecat, SymmetryWake)}
  
  if (exists("TBSC_Profile")){return(TBSC_Profile)} else {
    message(paste0("TBS Calculation Type (", TTB_Type, ") Not Supported! No profile returned."))
  }
  
}

# Generic function that calculates a TBS Distance (and accompanying speed/wind effect) 
# This has TTB_Type as an argument to decide which method of calculation to use
# Time Buffers are added at this stage 
# Currently only supports TBS Wake/ROT
Calculate_Perfect_TBS_Distance <- function(con, LPR, TBSC_Profile, TTB_Type, Out_Prefix, Constraint_Type, ID_Var, Time_Var, Speed_Var, Seg_Size){
  
  Dist_Var <- paste0(Out_Prefix, "_Distance")
  WE_Var <- paste0(Out_Prefix, "_Wind_Effect")
  if (TTB_Type != "Original"){Speed_Var <- paste0(Out_Prefix, "_IAS")}
  
  ## Join on the Time Buffers and Generate new Time aim
  LPR <- LPR %>%
    mutate(TTB_Reference_Time = !!sym(Time_Var)) %>%
    mutate(Sep_Dist_Buffer = 0)
  
  if (TTB_Type == "Original"){TTB_Results <- Calculate_Perfect_TBS_Distance_Original(LPR, TBSC_Profile, Out_Prefix, ID_Var, Time_Var = "TTB_Reference_Time", Speed_Var, Seg_Size)}
  if (TTB_Type == "ORD"){TTB_Results <- Calculate_Perfect_TBS_Distance_TTB_ORD(LPR, TBSC_Profile, ID_Var, Dist_Var, Speed_Var, WE_Var)}
  if (TTB_Type == "T2F"){TTB_Results <- Calculate_Perfect_TBS_Distance_TTB_T2F(LPR, TBSC_Profile, ID_Var, Dist_Var, Speed_Var, WE_Var)}
  
  LPR <- LPR %>%
    select(-TTB_Reference_Time, -Sep_Dist_Buffer) 
  
  if (exists("TTB_Results")){LPR <- LPR %>% left_join(TTB_Results, by = setNames(ID_Var, ID_Var))}
  
  return(LPR)
  
}


Calculate_Perfect_TBS_Distance_Original <- function(LPR, Segment_Forecast, Out_Prefix, ID_Var, Time_Var, Speed_Var, Seg_Size){
  
  Dist_Var <- paste0(Out_Prefix, "_Distance")
  WE_Var <- paste0(Out_Prefix, "_Wind_Effect")
  Alt_Speed_Var <- paste0(Out_Prefix, "_IAS")
  
  LPR <- LPR %>%
    mutate(TBS_Dist_No_WE := (!!sym(Time_Var) * !!sym(Speed_Var)) + Delivery_Point)
  
  LPR <- Get_Average_Forecast_Wind_Effect(LPR, Segment_Forecast,
                                                   Prefix = "TBSC",
                                                   ID_Var = ID_Var,
                                                   Start_Dist_Var = "TBS_Dist_No_WE",
                                                   End_Dist_Var = "Delivery_Point",
                                                   Speed_Var,
                                                   Seg_Size)
  
  LPR <- LPR %>%
    mutate(!!sym(Dist_Var) := !!sym(Time_Var) * (Follower_Forecast_TBSC_Wind_Effect + !!sym(Speed_Var)),
           !!sym(Alt_Speed_Var) := !!sym(Speed_Var)) %>%
    rename(!!sym(WE_Var) := Follower_Forecast_TBSC_Wind_Effect) %>%
    select(!!sym(ID_Var), !!sym(Dist_Var), !!sym(Alt_Speed_Var), !!sym(WE_Var))
  
  return(LPR)
    
}


Calculate_Perfect_TBS_Distance_TTB_ORD <- function(LPR, TBSC_Profile, ID_Var, Dist_Var, Speed_Var, WE_Var){
  
  TTB_Results <- Calculate_Predicted_ORD_Flying_Parameters_Follower(TBSC_Profile, LPR, ID_Var, "Delivery_Point", "Sep_Dist_Buffer", "TTB_Reference_Time", Prefix = "TBSC") %>%
    select(!!sym(ID_Var),
           !!sym(Dist_Var) := "Forecast_Follower_TBSC_Flying_Distance",
           !!sym(Speed_Var) := "Forecast_Follower_TBSC_IAS",
           !!sym(WE_Var) := "Forecast_Follower_TBSC_Wind_Effect")
  
  
  return(TTB_Results)
  
} 

Calculate_Perfect_TBS_Distance_TTB_T2F <- function(LPR, TBSC_Profile, ID_Var, Dist_Var, Speed_Var, WE_Var){
  
  TTB_Results <- Calculate_Predicted_ORD_Flying_Parameters_Follower(TBSC_Profile, LPR, ID_Var, "Delivery_Point", "Sep_Dist_Buffer", "TTB_Reference_Time", Prefix = "TBSC") %>%
    select(!!sym(ID_Var),
           !!sym(Dist_Var) := "Forecast_Follower_TBSC_Flying_Distance",
           !!sym(Speed_Var) := "Forecast_Follower_TBSC_IAS",
           !!sym(WE_Var) := "Forecast_Follower_TBSC_Wind_Effect")
  
  
  return(TTB_Results)
  
}

Generate_TTB_ORD_GSPD_Profile <- function(con, LPR, Full_Wind_Forecast, ID_Var, Use_EFDD, Use_Gust_Data, Precedences, ORD_Levels, LegacyorRecat, Symmetry){
  
  # Build an Aircraft Profile with ORD Buffers
  Aircraft_Profile <- Generate_ORD_Aircraft_Profile_Symmetric(con, ID_Var, LPR, ORDBuffers = T, Use_EFDD, Use_Gust_Data, Precedences, ORD_Levels, LegacyorRecat, Symmetry)
  
  # Build a Normal IAS Profile
  IAS_Profile <- Generate_ORD_IAS_Profile(con, ID_Var, Aircraft_Profile, LPR, Full_Wind_Forecast)
  
  ## Build Follower GSPD Profile
  GS_Profile <- Generate_ORD_GSPD_Profile(con, ID_Var, IAS_Profile, Full_Wind_Forecast, Seg_Size)
  
  GS_Profile <- filter(GS_Profile, This_Pair_Role == "F")
  
  return(GS_Profile)
  
}

Generate_ORD_Aircraft_Profile_Symmetric <- function(con, ID_Var, LPR, ORDBuffers, Use_EFDD, Use_Gust_Data, Precedences, ORD_Levels, LegacyorRecat, Symmetry){

  # ID_Var <- LP_Primary_Key
  # LPR <- LP
  # ORDBuffers <- T
  # Precedences <- Full_Level_Precedence
  # ORD_Levels <- Wake_Levels_Used
  # Symmetry <- T
  
  # Create standard aircraft profile if symmetry not required.
  if (!Symmetry){
    #message("Symmetry not required!")
    return(Generate_ORD_Aircraft_Profile(con, ID_Var, LPR, ORDBuffers, Use_EFDD, Use_Gust_Data, Precedences, ORD_Levels, LegacyorRecat))
  }
  
  if (exists("AircraftProfile")){rm(AircraftProfile)}
  
  # Loop across all levels
  for (LevelIndex in 1:length(ORD_Levels)){
    
    #LevelIndex <- 3
    
    # If level is being used
    if (ORD_Levels[LevelIndex]){

      # Make local copy of ORD_Levels to be edited.
      ORD_Levels_Local <- ORD_Levels
      
      # Turn off all other Levels.
      for (LevelIndexLocal in 1:length(ORD_Levels_Local)){
        
        if (LevelIndex != LevelIndexLocal){ORD_Levels_Local[LevelIndexLocal] <- F}
        
      }
      
      # Get Aircraft Profile for This Level only.
      AircraftProfileLocal <- Generate_ORD_Aircraft_Profile(con, ID_Var, LPR, ORDBuffers, Use_EFDD, Use_Gust_Data, Precedences, ORD_Levels_Local, LegacyorRecat)
      
      # Separate into Leader/Follower.
      AircraftProfileLocalLeader <- filter(AircraftProfileLocal, This_Pair_Role == "L") %>% select(!!sym(ID_Var), VRef_Lead = VRef)
      AircraftProfileLocalFollower <- filter(AircraftProfileLocal, This_Pair_Role == "F") %>% select(!!sym(ID_Var), VRef_Foll = VRef)
      AircraftProfileLocalCheck <- full_join(AircraftProfileLocalLeader, AircraftProfileLocalFollower, by = setNames(ID_Var, ID_Var)) %>%
        rename(ID = !!sym(ID_Var))
      
      # Check to see if both Leader/Follower values are populated. If not, remove from Landing Pair from AircraftProfileLocal.
      AircraftProfileLocalCheck <- filter(AircraftProfileLocalCheck, !is.na(VRef_Lead) & !is.na(VRef_Foll))
      AircraftProfileLocal <- filter(AircraftProfileLocal, !!sym(ID_Var) %in% AircraftProfileLocalCheck$ID)
      LPR <- filter(LPR, !!sym(ID_Var) %!in% AircraftProfileLocalCheck$ID)
      
      # Add to Final Aircraft Profile Table.
      if (exists("AircraftProfile")){AircraftProfile <- rbind(AircraftProfile, AircraftProfileLocal)} else {AircraftProfile <- AircraftProfileLocal}
      
    }
    
  }
  
  # Order by ID again.
  AircraftProfile <- arrange(AircraftProfile, !!sym(ID_Var))
  
  return(AircraftProfile)
  
}

Generate_T2F_Aircraft_Profile_Symmetric <- function(con, ID_Var, LPR, Precedences, T2F_Levels, LegacyorRecat, Symmetry){
  
  # If symmetry not required, determine T2F Profile as normal
  if (!Symmetry){
    LPLead <- Get_T2F_Adaptation_In_Precedence(con, ID_Var, LPR, Precedences, T2F_Levels, LegacyorRecat, LorF = "Leader")
    LPFoll <- Get_T2F_Adaptation_In_Precedence(con, ID_Var, LPR, Precedences, T2F_Levels, LegacyorRecat, LorF = "Follower")
    Profile <- rbind(LPLead, LPFoll)
    return(Profile)
  }
  
  # Loop across all levels
  for (LevelIndex in 1:length(T2F_Levels)){
    
    # Make local copy of ORD_Levels to be edited.
    T2F_Levels_Local <- T2F_Levels
    
    # If level is being used
    if (T2F_Levels[LevelIndex]){
      
      # Turn off all other Levels.
      for (LevelIndexLocal in 1:length(T2F_Levels_Local)){
        
        if (LevelIndex != LevelIndexLocal){T2F_Levels_Local[LevelIndexLocal] <- F}
        
      }
      
      # Get T2F Model Adaptation
      LPLead <- Get_T2F_Adaptation_In_Precedence(con, ID_Var, LPR, Precedences, T2F_Levels_Local, LegacyorRecat, LorF = "Leader") 
      LPLeadCheck <- LPLead %>% group_by(!!sym(ID_Var)) %>% summarise(Lead_Count = n()) %>% ungroup()
      LPFoll <- Get_T2F_Adaptation_In_Precedence(con, ID_Var, LPR, Precedences, T2F_Levels_Local, LegacyorRecat, LorF = "Follower")
      LPFollCheck <- LPFoll %>% group_by(!!sym(ID_Var)) %>% summarise(Foll_Count = n()) %>% ungroup()
      LPCheck <- full_join(LPLeadCheck, LPFollCheck, by = setNames(ID_Var, ID_Var)) %>% rename(ID := !!sym(ID_Var))
      
      # Check to see if both Leader/Follower values are populated. If not, remove from Landing Pair from AircraftProfileLocal.
      LPCheck <- filter(LPCheck, !is.na(Foll_Count) & !is.na(Lead_Count))
      LPLead <- filter(LPLead, !!sym(ID_Var) %in% LPCheck$ID)
      LPFoll <- filter(LPLead, !!sym(ID_Var) %in% LPCheck$ID)
      ProfileLocal <- rbind(LPLead, LPFoll)
      LPR <- filter(LPR, !!sym(ID_Var) %!in% LPCheck$ID)
      
      # Add to Final Aircraft Profile Table.
      if (exists("Profile")){Profile <- rbind(Profile, ProfileLocal)} else {Profile <- ProfileLocal}
      
    }
    
  }
  
  return(Profile)
  
}

Generate_TTB_T2F_GSPD_Profile <- function(con, LPR, Full_Wind_Forecast, ID_Var, Seg_Size, Precedences, T2F_Levels, LegacyorRecat, Symmetry){
  
  # Get T2F Model Adaptation. Now compatible with Symmetry.
  Profile <- Generate_T2F_Aircraft_Profile_Symmetric(con, ID_Var, LPR, Precedences, T2F_Levels, LegacyorRecat, Symmetry)
  
  # Build full Profile
  Profile <- Profile %>% arrange(!!sym(ID_Var), This_Pair_Role, Range_To_Threshold) %>%
    rename(End_Dist = Range_To_Threshold, End_IAS = Average_IAS)
  
  # Lag to get the End IAS of each section
  Profile <- Profile %>% group_by(!!sym(ID_Var), This_Pair_Role) %>% mutate(Start_Dist = lead(End_Dist), Start_IAS = lead(End_IAS)) %>% ungroup() %>%
    filter(!is.na(Start_Dist))
  
  # Get Wind Effects
  Wind_Effects <- select(Full_Wind_Forecast, !!sym(ID_Var) := ID, DME_Seg, Forecast_Wind_Effect_IAS)
  
  # Get the Joiun Columns
  ProfileColumns <- c(ID_Var, "End_Dist")
  WindColumns <- c(ID_Var, "DME_Seg")
  
  # Join on Wind Effects
  Profile <- left_join(Profile, Wind_Effects, by = setNames(WindColumns, ProfileColumns))
  
  ### Use shift etc to get Start/End Distances/IAS/GSPD
  Profile <- Profile %>% mutate(Start_GS = Start_IAS + Forecast_Wind_Effect_IAS, End_GS = End_IAS + Forecast_Wind_Effect_IAS)
  
  # TEMP FIX: Add on 1/2 mile to all distances and set base distance as 0.
  Profile <- Profile %>%
    mutate(End_Dist = End_Dist + 0.5*Seg_Size, Start_Dist = Start_Dist + 0.5*Seg_Size, End_Dist = ifelse(End_Dist == 0.5*Seg_Size, 0, End_Dist))
  
  # Filter for Follower Only.
  Profile <- filter(Profile, This_Pair_Role == "F")
  
  return(Profile)
  
}

# Function to get the Service Level of the constraint. If IA is in DBS mode all constraints become DBS.
Get_Constraint_Service_Level <- function(Constraint_Type, IA_Service_Level){
  
  if (IA_Service_Level == "DBS"){return("DBS")}
  if (Constraint_Type %in% c("Wake", "ROT")){return("TBS")}
  if (Constraint_Type %in% c("Runway_Dependent", "Non_Wake", "Spacing")){return("DBS")}
  
}

Get_Minimum_Constraint_Distances <- function(LPR, Constraint_Type, Delivery, Runway_Rule, Runway_Pair_Rule, Wake_Sep_Min){
  
  # TEMP: Set Minimum to 2.5
  LPR <- LPR %>% mutate(Minimum_Constraint_Distance = (0)*1852)
  
  return(LPR)
}

Set_TBSC_Delivery_Point <- function(LPR, Delivery){
  
  # Set the "Delivery Point" (The point at which any TBS Distance Calculations "Start" the evaluation) 
  # Set to the FAF Distance if calculating a FAF constraint
  # Set the Delivery_Distance if constraint is THRESHOLD (technically this could be 1DME etc)
  if (Delivery == "THRESHOLD"){LPR <- mutate(LPR, Delivery_Point = Delivery_Distance)}
  else if (Delivery == "FAF"){LPR <- mutate(LPR, Delivery_Point = FAF_Distance)}
  else {LPR <- mutate(LPR, Delivery_Point = NA)}
  
  return(LPR)
}

Generate_Constraint_DBS_Distance <- function(con, LPR, Out_Prefix, Constraint_Type){

  Dist_Var <- paste0(Out_Prefix, "_Distance")
  
  ## Firstly, because we are using a DBS Distance, we require NA Wind Effect/Speeds. Only if Constraint has possibility of TBS though.
  if (Get_Constraint_Service_Level(Constraint_Type, "TBS") == "TBS"){
    LPR <- LPR %>%
      mutate(!!sym(paste0(Out_Prefix, "_Wind_Effect")) := NA,
             !!sym(paste0(Out_Prefix, "_IAS")) := NA)
  }
  
  ## Now we need to acquire the correct DBS distances.
  if (Constraint_Type == "Spacing"){LPR <- mutate(LPR, !!sym(Dist_Var) := In_Trail_Spacing)}
  if (Constraint_Type == "Wake"){LPR <- mutate(LPR, !!sym(Dist_Var) := Reference_Recat_Wake_Separation_Distance)}
  if (Constraint_Type == "ROT"){LPR <- mutate(LPR, !!sym(Dist_Var) := Reference_Recat_ROT_Spacing_Distance)}
  if (Constraint_Type == "Non_Wake"){
    LPR <- mutate(LPR, !!sym(Dist_Var) := ifelse(RNAV_Flag == 1, Non_Wake_RNAV_Separation_Distance, Non_Wake_Separation_Distance))}
  if (Constraint_Type == "Runway_Dependent"){LPR <- mutate(LPR, !!sym(Dist_Var) := Runway_Dependent_Separation_Distance)}
  return(LPR)
}

Get_Legal_Spacing_Name <- function(Constraint_Type){
  
  if (Constraint_Type %in% c("Wake", "Non_Wake", "Runway_Dependent")){return("Separation")}
  else if (Constraint_Type %in% c("ROT")){return("Spacing")}
  else {return("")}
  
}

Get_Constraint_Prefix <- function(Constraint_Type, Constraint_Delivery, Service_Level, LegacyorRecat){
  
  Legal <- Get_Legal_Spacing_Name(Constraint_Type)
  Constraint_Delivery <- ifelse(Constraint_Delivery == "THRESHOLD", "Threshold", "FAF")
  return(paste0(LegacyorRecat, "_", Constraint_Delivery, "_", Constraint_Type, "_", Legal))
  
}

# Generic function to Generate Constraint Spacing for ALL constraints at either FAF or Threshold.
Generate_Constraint_Spacing <- function(con, LPR, TBSC_Profile, TTB_Type, Constraint_Type, Constraint_Delivery, Evaluated_Delivery, IA_Service_Level, ID_Var, Time_Var, Speed_Var, Seg_Size, LegacyorRecat){
  
  # Get the Prefix for the Constraint being EVALUATED.
  Out_Prefix <- Get_Constraint_Prefix(Constraint_Type, Evaluated_Delivery, Service_Level, LegacyorRecat)
  
  # Is this constraint DBS or TBS based?
  Service_Level <- Get_Constraint_Service_Level(Constraint_Type, IA_Service_Level)
  
  # If Constraint at FAF and evaluated at Threshold, Threshold constraint is NULL by default.
  if (Constraint_Delivery == "FAF" & Evaluated_Delivery == "THRESHOLD"){
    LPR <- mutate(LPR, !!sym(paste0(Out_Prefix, "_Distance")) := NA)
    return(LPR)
  } else {
  
    # Get the Minimums for This Constraint/Delivery Point
    LPR <- Get_Minimum_Constraint_Distances(LPR, Constraint_Type, Evaluated_Delivery)
    
    # Set the Delivery Point for TBS Calculations if Required
    LPR <- Set_TBSC_Delivery_Point(LPR, Evaluated_Delivery)
    
    # If evaluated at constraint delivery point, perform standard distance matching/calculation.
    # Apply Minimums here.
    if (Constraint_Delivery == Evaluated_Delivery){
      if (Service_Level == "TBS"){
        LPR <- Calculate_Perfect_TBS_Distance(con, LPR, TBSC_Profile, TTB_Type, Out_Prefix, Constraint_Type, ID_Var, Time_Var, Speed_Var, Seg_Size) 
    } else {
      LPR <- Generate_Constraint_DBS_Distance(con, LPR, Out_Prefix, Constraint_Type)
    }
    }
    
    # If constraint at Threshold and Evaluated at FAF, assume Threshold constraint calculated
    # and Add ORD Compression.
    Compression_Var <- paste0(LegacyorRecat, "_Forecast_ORD_Compression")
    if (Constraint_Delivery == "THRESHOLD" & Evaluated_Delivery == "FAF"){
      Thresh_Var <- paste0(Get_Constraint_Prefix(Constraint_Type, Constraint_Delivery, Service_Level, LegacyorRecat), "_Distance")
      LPR <- LPR %>%
        mutate(!!sym(paste0(Out_Prefix, "_Distance")) := !!sym(Thresh_Var) + !!sym(Compression_Var))
    }
    
    # Remove Wake/ROT Constraints for Not in Trail Pairs (TEMP!!!!)
    if (Constraint_Type %in% c("Wake", "ROT")){
      LPR <- LPR %>% mutate(!!sym(paste0(Out_Prefix, "_Distance")) := ifelse(Landing_Pair_Type == "Not_In_Trail", NA, !!sym(paste0(Out_Prefix, "_Distance"))))
      if (paste0(Out_Prefix, "_IAS") %in% names(LPR)){LPR <- LPR %>% mutate(!!sym(paste0(Out_Prefix, "_IAS")) := ifelse(Landing_Pair_Type == "Not_In_Trail", NA, !!sym(paste0(Out_Prefix, "_IAS"))))}
      if (paste0(Out_Prefix, "_Wind_Effect") %in% names(LPR)){LPR <- LPR %>% mutate(!!sym(paste0(Out_Prefix, "_Wind_Effect")) := ifelse(Landing_Pair_Type == "Not_In_Trail", NA, !!sym(paste0(Out_Prefix, "_Wind_Effect"))))}
    }
    
    # Remove Runway Dep Constraints for In Trail Pairs
    if (Constraint_Type %in% c("Runway_Dependent")){
      LPR <- LPR %>% mutate(!!sym(paste0(Out_Prefix, "_Distance")) := ifelse(Landing_Pair_Type != "Not_In_Trail", NA, !!sym(paste0(Out_Prefix, "_Distance"))))
    }
    
    if (Constraint_Type == "Non_Wake"){Remove_NA = T} else{Remove_NA = F}
    Max_Vars <- c(paste0(Out_Prefix, "_Distance"), "Minimum_Constraint_Distance")
    LPR <- LPR %>%
      mutate(!!sym(paste0(Out_Prefix, "_Distance")) := pmax(!!!syms(Max_Vars), na.rm = Remove_NA))

    LPR <- LPR %>% select(-Minimum_Constraint_Distance, -Delivery_Point)
    
  }
  
  return(LPR)
  
}

Generate_Constraint_Spacing_All <- function(con, LPR, TBSC_Profile,
                                            TTB_Type, Constraint_Type,
                                            Constraint_Delivery, Evaluated_Delivery,
                                            ID_Var, 
                                            Time_Var, Speed_Var,
                                            Seg_Size, LegacyorRecat){
  
  # LPR_DBS <- filter(LPR, TBS_Service_Level == "DBS")
  # LPR_DBS <- Generate_Constraint_Spacing(con, LPR_DBS, TBSC_Profile, TTB_Type, Constraint_Type, Constraint_Delivery, Evaluated_Delivery, IA_Service_Level = "DBS", ID_Var, Time_Var, Speed_Var, Seg_Size, LegacyorRecat)
  LPR_TBS <- filter(LPR, TBS_Service_Level == "TBS")
  LPR_TBS <- Generate_Constraint_Spacing(con, LPR_TBS, TBSC_Profile, TTB_Type, Constraint_Type, Constraint_Delivery, Evaluated_Delivery, IA_Service_Level = "TBS", ID_Var, Time_Var, Speed_Var, Seg_Size, LegacyorRecat)
  
  LPR <- LPR_TBS %>%
    arrange(!!sym(ID_Var))
  
  return(LPR)
  
}
## Value can be "IAS", "Distance", "Time"
Get_Reference_Variable_Name <- function(Constraint_Type, Value, LegacyorRecat){
  
  if (Get_Constraint_Service_Level(Constraint_Type, "TBS") == "TBS"){
    return(paste0("Reference_", LegacyorRecat, "_", Constraint_Type, "_", Get_Legal_Spacing_Name(Constraint_Type), "_", Value))
  } else {return(NA)}
  
}









############################################################################################
# 
# # Define TBS Calculation Type. ("Original"|"ORD"|"T2F")
# 
# TTB_Type <- "ORD"
# Time_Buffers <- Generate_TBSC_Time_Buffers(con, test, LP_Primary_Key, Active = F)  # UNTESTED WITH BUFFER ADAPTATION
# TBSC_Profiles <- Generate_TBSC_Profiles(con, LPR, Full_Wind_Forecast, ID_Var, TTB_Type)  # UNTESTED WITH T2F
# 
# 
# 
# 
# ## loop test
# test <- Get_TBS_Service_Level(con, INT_Landing_Pairs, LP_Primary_Key) %>% mutate(FAF_Distance = (4.5*1852)) # WORKS
# test <- test %>% Get_RNAV_Flag() %>% Get_Non_Wake_Spacing() %>% mutate(Forecast_ORD_Compression = 1852)
# Constraints <- c("Wake", "Non_Wake", "ROT")
# Dist_Values <- c()
# 
# ## Generate the Distance for all Constraints at Threshold
# for (Constraint in Constraints){
#     test <- Generate_Constraint_Spacing_All(con, LPR = test, TBSC_Profile = TBSC_Profiles,
#                                             TTB_Type, Constraint_Type = Constraint,
#                                             Constraint_Delivery = "THRESHOLD", Evaluated_Delivery = "THRESHOLD",
#                                             ID_Var = LP_Primary_Key, 
#                                             Time_Var = Get_Reference_Variable_Name(Constraint, "Time"), Speed_Var = Get_Reference_Variable_Name(Constraint, "IAS"),
#                                             Seg_Size, Time_Buffers)
#     if (paste0(Get_Constraint_Prefix(Constraint, "THRESHOLD", "TBS"), "_Distance") %in% names(test)){
#       Dist_Values <- append(Dist_Values, paste0(Get_Constraint_Prefix(Constraint, "THRESHOLD", "TBS"), "_Distance"))
#     }
# }
# 
# ## Get the Maximum Distance Threshold Constraint
# test <- test %>%
#   mutate(Threshold_All_Separation_Distance = pmax(!!!syms(Dist_Values), na.rm=T))
# 
# ## 


