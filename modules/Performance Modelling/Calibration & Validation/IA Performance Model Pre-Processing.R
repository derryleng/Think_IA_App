# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# IA Performance Model Pre-Processing
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v1.0
#
# Authors: George Clark
# 
# Description: Script to Model Overall IA Performance.
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


# --------------------------------------------------------------------------------- #
# Filtering
# --------------------------------------------------------------------------------- #

# Initialise Filter Table Columns
Filter <- c("Original")
OldRows <- c(NA)
NewRows <- c(nrow(Performance_Model))
Removed <- c(NA)

# Filter for RECAT Wake Pairs Only
if (Use_Filter_Recat_Wake){
  Pre_Row <- nrow(Performance_Model)
  Performance_Model <- filter(Performance_Model, !is.na(Ref_Recat_Wake_Separation_Distance))
  Post_Row <- nrow(Performance_Model)
  Rem_Row <- Pre_Row - Post_Row
  Filter <- append(Filter, "Wake Pairs Only")
  OldRows <- append(OldRows, Pre_Row)
  NewRows <- append(NewRows, Post_Row)
  Removed <- append(Removed, Rem_Row)
}

# Filter to remove Negative Separation Times (Go-Arounds)
if (Use_Filter_Go_Arounds){
  Pre_Row <- nrow(Performance_Model)
  Performance_Model <- filter(Performance_Model, Observed_1DME_Separation_Time > 0)
  Post_Row <- nrow(Performance_Model)
  Rem_Row <- Pre_Row - Post_Row
  Filter <- append(Filter, "Go-Arounds (Neg 1DME Sep Time)")
  OldRows <- append(OldRows, Pre_Row)
  NewRows <- append(NewRows, Post_Row)
  Removed <- append(Removed, Rem_Row)
}

# Filter for Average IAS from (Max_IAS_Filter_DME)DME Upwards
if (Use_Filter_Average_IAS){
  Pre_Row <- nrow(Performance_Model)
  FAF_Segs <- filter(Segments, DME_Seg >= (Filter_Average_IAS_Max_DME - 1)) %>%
    mutate(Weighting = ifelse(Filter_Average_IAS_Max_DME - DME_Seg > 1, 1, Filter_Average_IAS_Max_DME - DME_Seg)) %>%
    group_by(Landing_Pair_ID) %>%
    summarise(Leader_Ave_Flight_IAS = mean(Leader_Average_IAS, na.rm=T), Follower_Ave_Flight_IAS = mean(Follower_Average_IAS, na.rm=T)) %>%
    ungroup()

  Performance_Model <- Performance_Model %>%
    left_join(FAF_Segs, by  =c("Landing_Pair_ID")) %>%
    filter(Follower_Ave_Flight_IAS >= Filter_Average_IAS_Min_SPD & Follower_Ave_Flight_IAS <= Filter_Average_IAS_Max_SPD)
  
  Post_Row <- nrow(Performance_Model)
  Rem_Row <- Pre_Row - Post_Row
  Filter <- append(Filter, "Average Speed beyond FAF too extreme")
  OldRows <- append(OldRows, Pre_Row)
  NewRows <- append(NewRows, Post_Row)
  Removed <- append(Removed, Rem_Row)

}

# Add to Filter Table.
Filter_Table <- data.frame(Filter_Name = Filter, Prev_Count = OldRows, New_Count = NewRows, Removed = Removed)

# --------------------------------------------------------------------------------- #
# Legacy Wake Fields
# --------------------------------------------------------------------------------- #



# --------------------------------------------------------------------------------- #
# Tidying Fields from SQL
# --------------------------------------------------------------------------------- #

# Group Fields Together
Grouping_Vars <- c("Landing_Pair_ID", "FP_Date", "Leader_Callsign", "Follower_Callsign")
Aircraft_Vars <- c("Leader_Aircraft_Type", "Follower_Aircraft_Type", "LF_Pair_Aircraft") 
Recat_Wake_Vars <- c("Leader_Recat_Wake_Cat", "Follower_Recat_Wake_Cat", "LF_Pair_Recat")
Legacy_Wake_Vars <- c("Leader_Legacy_Wake_Cat", "Follower_Legacy_Wake_Cat", "LF_Pair_Legacy")

Legacy_Dist_Vars <- c("Ref_Legacy_Wake_Separation_Distance", "Ref_Legacy_All_Separation_Distance")
Recat_Dist_Vars <-  c("Ref_Recat_Wake_Separation_Distance", "Ref_ROT_Spacing_Distance", "Ref_Recat_All_Separation_Distance")
Ref_Time_Vars <- c("Ref_Recat_Wake_Separation_Time", "Ref_ROT_Spacing_Time")
Ref_Speed_Vars <- c("Follower_Ass_Recat_Separation_IAS", "Follower_Ass_ROT_Spacing_IAS")

Observed_Vars <- c("Observed_AGI_Surface_Headwind_Group")

## All Other Vars

# --------------------------------------------------------------------------------- #
# Sample Size Bolstering
# --------------------------------------------------------------------------------- #

# Get the Wake Cats
Wake_Cats <- Get_Wake_Cats(Wake_Scheme)

# Add on the Three Bolster Flags
Performance_Model <- Performance_Model %>%
  mutate(Bolster_Flag_Main = 0,
         Bolster_Flag_ROT = 0,
         Bolster_Flag_ROT_Other = 0)

# Bolster for Wake Pairs
if (Bolster_Main){
  
  # Select these Variables
  PM_Bolster_Main_Original <- Performance_Model
  
  # Get the Pairs (Do all for now)
  Bolster_Pairs <- Recat_Wake_Dist
  
  # Code to retrieve the relevant pairs: Loop through each pair
  for (i in 1:nrow(Bolster_Pairs)){
    
    # Get the Leader/Follower Wake
    Wake_F <- Bolster_Pairs$Follower_WTC[i]
    Wake_L <- Bolster_Pairs$Leader_WTC[i]
    
    # Get the Pairs in the Data to expand:
    # Gets all pairs with matching follower and different leader. Duplicates and changes leader.
    PM_Bolster_Main_Iter <- filter(PM_Bolster_Main_Original, Follower_Recat_Wake_Cat == Wake_F & Leader_Recat_Wake_Cat != Wake_L) %>% 
      mutate(Leader_Recat_Wake_Cat = Wake_L) %>%
      mutate(Leader_Aircraft_Type = NA)
    
    # Bind Together
    PM_Bolster_Main_Iter <- PM_Bolster_Main_Iter %>%
      mutate(Bolster_Flag_Main = 1)
    
    # If total Pair Observations over certain amount, cap amount (remove for now)
    #if (nrow(pm_iter) > max_obs){pm_iter <- pm_iter[1:max_obs,]}
    
    # Bind onto main dataset.
    if (i == 1){PM_Bolster_Main <- PM_Bolster_Main_Iter} else {PM_Bolster_Main <- rbind(PM_Bolster_Main, PM_Bolster_Main_Iter)}
    
  }
  
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

# Bolster for ROT Pairs
if (Bolster_ROT){}

# Get the Legacy Wake Categories
Performance_Model <- Performance_Model %>%
  left_join(Legacy_AC_To_Wake, by= c("Leader_Aircraft_Type" = "Aircraft_Type")) %>% rename(Leader_Legacy_Wake_Cat = Wake) %>%
  left_join(Legacy_AC_To_Wake, by= c("Follower_Aircraft_Type" = "Aircraft_Type")) %>% rename(Follower_Legacy_Wake_Cat = Wake)

# Get the Legacy Wake Distance
Performance_Model <- Performance_Model %>%
  left_join(Legacy_Wake_Dist, by = c("Leader_Legacy_Wake_Cat" = "Leader_WVI", "Follower_Legacy_Wake_Cat" = "Follower_WVI")) %>%
  rename(Ref_Legacy_Wake_Separation_Distance = WT_Separation_Distance) %>%
  mutate(Ref_Legacy_All_Separation_Distance = ifelse(!is.na(Ref_Legacy_Wake_Separation_Distance), Ref_Legacy_Wake_Separation_Distance, Min_Radar_Sep))

# Pair Variables
Performance_Model <- Performance_Model %>%
  mutate(LF_Pair_Recat = paste0(Leader_Recat_Wake_Cat, "-", Follower_Recat_Wake_Cat)) %>%
  mutate(LF_Pair_Legacy = paste0(Leader_Legacy_Wake_Cat, "-", Follower_Legacy_Wake_Cat)) %>%
  mutate(LF_Pair_Aircraft = paste0(Leader_Aircraft_Type, "-", Follower_Aircraft_Type))

# --------------------------------------------------------------------------------- #
# Capping Values
# --------------------------------------------------------------------------------- #

Performance_Model <- Performance_Model %>% mutate(Observation_ID = row_number()) %>%
  arrange(Observation_ID)
