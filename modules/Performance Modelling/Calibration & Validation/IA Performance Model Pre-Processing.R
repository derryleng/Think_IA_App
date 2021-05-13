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

# Filter for RECAT Wake Pairs Only
if (Use_Filter_Recat_Wake){}

# Filter to remove Negative Separation Times (Go-Arounds)
if (Use_Filter_Go_Arounds){}

# Filter for Average IAS from Threshold to (Max_IAS_Filter_DME)DME
if (Use_Filter_Average_IAS){
  
}

# --------------------------------------------------------------------------------- #
# Legacy Wake Fields
# --------------------------------------------------------------------------------- #

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
         Bolster_Flag_Main_Leader = 0,
         Bolster_Flag_Main_Follower = 0,
         Bolster_Flag_ROT = 0,
         Bolster_Flag_ROT_Other = 0)

# Bolster for Wake Pairs
if (Bolster_Main){
  
  # Get the Variables that will remain the same/Need adjusting (The rest will be replaced)
  Bolster_Main_Vars <- Grouping_Vars %>%
    append(Category_Vars) %>%
    apppend(Recat_Wake_Vars)
  
  # Select these Variables
  PM_Bolster_Main <- select(Performance_Model, all_of(Bolster_Main_Vars))
  
  # Get a table of the wake pairs we want to bolster
  a <- merge(leader_list, wake_cats) 
  names(a) <- c("Leader_WTC", "Follower_WTC")
  b <- merge(follower_list, wake_cats) %>% select(2, everything())
  names(b) <- c("Leader_WTC", "Follower_WTC")
  Bolster_Pairs <- rbind(a, b) %>% unique()
  rm(a, b)
  
  # Code to retrieve the relevant pairs: Loop through each pair
  for (i in 1:nrow(Bolster_Pairs)){
    
    # Get the Leader/Follower Wake
    Wake_F <- Bolster_Pairs$Follower_WTC[i]
    Wake_L <- Bolster_Pairs$Leader_WTC[i]
    
    # Get the Pairs in the Data to expand:
    # Gets all pairs with matching follower and different leader. Duplicates and changes leader.
    PM_Bolster_Main_Iter_L <- filter(PM_Bolster_Main, Follower_Recat_Wake_Cat == Wake_F & Leader_Recat_Wake_Cat != Wake_L) %>% 
      mutate(Leader_Recat_Wake_Cat = Wake_L) %>%
      mutate(Leader_Aircraft_Type = NA) %>%
      mutate(Bolster_Flag_Main_Leader = 1)
    
    # Does the same with reverse l/f.
    PM_Bolster_Main_Iter_F <- filter(PM_Bolster_Main, Follower_Recat_Wake_Cat != Wake_F & Leader_Recat_Wake_Cat == Wake_L) %>% 
      mutate(Follower_Recat_Wake_Cat = Wake_F) %>%
      mutate(Follower_Aircraft_Type = NA) %>%
      mutate(Bolster_Flag_Main_Follower = 1)
    
    # Bind Together
    PM_Bolster_Main_Iter <- rbind(PM_Bolster_Main_Iter_L, PM_Bolster_Main_Iter_F) %>%
      mutate(Bolster_Flag_Main = 1)
    
    # TEMP UNDER INVESTIGATION: Remove Follower Bolsters
    PM_Bolster_Main_Iter <- filter(PM_Bolster_Main_Iter, Bolster_Flag_Main_Follower == 0)
    
    if (nrow(pm_iter) > max_obs){pm_iter <- pm_iter[1:max_obs,]}
    if (i == 1){pm_bolst <- pm_iter} else {pm_bolst <- rbind(pm_bolst, pm_iter)}
  }
  
  # Remove the columns from pm_bolst that will be recalculated/joined
  pm_bolst_test <- select(pm_bolst, -c("Recat_TBS_Wake_Separation_Time", "Recat_TBS_Wake_Separation_IAS", "Recat_DBS_Wake_Separation_Distance",
                                       "Follower_Forecast_IA_Wind_Effect"))
  
  # Get the reference distances
  pm_bolst_test <- left_join(pm_bolst_test, ref_dist, by=c("Leader_Recat_Wake_Cat"="Leader_WTC", "Follower_Recat_Wake_Cat"="Follower_WTC")) %>% 
    rename(Recat_DBS_Wake_Separation_Distance = Reference_Wake_Separation_Distance)
  
  # Get the reference times
  pm_bolst_test <- left_join(pm_bolst_test, ref_times, by=c("Leader_Recat_Wake_Cat"="Leader_WTC", "Follower_Recat_Wake_Cat"="Follower_WTC")) %>% 
    rename(Recat_TBS_Wake_Separation_Time = Reference_Wake_Separation_Time)
  
  # Get the reference Speeds
  pm_bolst_test <- left_join(pm_bolst_test, ref_speeds, by=c("Leader_Recat_Wake_Cat"="Leader_WTC", "Follower_Recat_Wake_Cat"="Follower_WTC")) %>% 
    rename(Recat_TBS_Wake_Separation_IAS = Assumed_Wake_Separation_IAS)
  
  # Get the wind effects 
  pm_bolst_test <- left_join(pm_bolst_test, gwcs_merge, by=c("FP_Date", "Follower_Callsign"="Callsign",
                                                             "Recat_DBS_Wake_Separation_Distance"="Sep_Dist",
                                                             "Follower_Time_At_4DME"="Time_At_4DME")) %>% 
    rename(Follower_Forecast_IA_Wind_Effect = Forecast_Wind_Effect_IAS)
  
  # Get the Assumed follower GSPD
  pm_bolst_test <- mutate(pm_bolst_test, Follower_Reference_GSPD = Follower_Forecast_IA_Wind_Effect + Recat_TBS_Wake_Separation_IAS)
  
  # Get the Time based IA 0DME distance separation and capped version at 3NM
  pm_bolst_test <- mutate(pm_bolst_test, Recat_TBS_0DME_Wake_Separation_Distance_Uncapped = Follower_Reference_GSPD * Recat_TBS_Wake_Separation_Time / 3600)
  
  # Remove any other fields so that columns match between two pm sets
  pm_bolst_test <- select(pm_bolst_test, -c("Follower_Reference_GSPD"))
  
  # Merge the bolstered/original datasets
  pm <- rbind(pm, pm_bolst_test)
  
  
}

# Bolster for ROT Pairs
if (Bolster_ROT){}



# Select Required Fields



# --------------------------------------------------------------------------------- #
# Capping Values
# --------------------------------------------------------------------------------- #

