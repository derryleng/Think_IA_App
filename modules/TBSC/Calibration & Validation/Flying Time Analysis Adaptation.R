# Output Table Names
TBS_Adaptation_Speeds_Name <- "tbl_Assumed_TBS_Table_IAS"
TBS_Adaptation_Times_Name <- "tbl_Reference_TBS_Table_Time"
Wake_Adaptation_Speeds_Wake_Name <- "tbl_Assumed_Recat_Separation_IAS"
Wake_Adaptation_Times_Wake_Name <- "tbl_Reference_Recat_Separation_Time"
ROT_Adaptation_Speeds_Wake_Name <- "tbl_Assumed_ROT_Spacing_IAS"
ROT_Adaptation_Times_Wake_Name <- "tbl_Reference_ROT_Spacing_Time"
Wake_Adaptation_Speeds_Aircraft_Name <- "tbl_Assumed_ACTP_Wake_Separation_IAS"
Wake_Adaptation_Times_Aircraft_Name <- "tbl_Reference_ACTP_Wake_Separation_Time"
ROT_Adaptation_Speeds_Aircraft_Name <- "tbl_Assumed_ACTP_ROT_Spacing_IAS"
ROT_Adaptation_Times_Aircraft_Name <- "tbl_Reference_ACTP_ROT_Spacing_Time"
Wake_Adaptation_Speeds_Operator_Name <- "tbl_Assumed_AC_Operator_Wake_Separation_IAS"
Wake_Adaptation_Times_Operator_Name <- "tbl_Reference_AC_Operator_Wake_Separation_Time"
ROT_Adaptation_Speeds_Operator_Name <- "tbl_Assumed_AC_Operator_ROT_Spacing_IAS"
ROT_Adaptation_Times_Operator_Name <- "tbl_Reference_AC_Operator_ROT_Spacing_Time"
# ------------------------------------------------------------------------------------------------- #
# Add to Local Adaptation Extension
# ------------------------------------------------------------------------------------------------- #
# Get the Weighted Type String
Weighted_Type_List <- ""
for (i in 1:length(Weighted_Types)){
  Weighted_Type_List <- paste0(Weighted_Type_List, Weighted_Types[i])
}

# Define Parameters/Values
Local_Adaptation_Ext_Parameters <- c(
  "Reference Winds Used",
  "Delivery Point Chosen",
  "Use AC Type Recat Weighting",
  "Types used for AC Weighting",
  "AC Type Recat Weights Version",
  "Minimum Obs for Local Weight",
  "Weighting Method",
  "TBS Table Weights Version"
)

Local_Adaptation_Ext_Values <- c(
  ifelse(Safety_Case == 1, "Surface Wind Speed", "Surface Headwind"),
  ifelse(Use_Secondary_Adaptation, "Secondary", "Primary"),
  as.character(Use_Weighted_Wake),
  ifelse(Use_Weighted_Wake, as.character(Weighted_Type_List), "None"),
  ifelse(Weighting_Local, as.character(Weighting_Version_Alt), as.character(Local_Iteration_Version)),
  as.character(Min_Weighting_Obs),
  as.character(Weighting_Method),
  ifelse(Counts_Local, as.character(Counts_Version_Alt), as.character(Local_Iteration_Version))
)

# Build data.frame
Local_Adaptation_Ext <- data.frame(
  Parameter = Local_Adaptation_Ext_Parameters,
  Value = Local_Adaptation_Ext_Values
)

# save adaptation as file
fwrite(Local_Adaptation_Ext, file.path(Local_Iteration_Dir, paste0("Local Adaptation Extension v", Local_Iteration_Version, ".csv")))

# Local Adaptation Comparison
if (Do_Version_Comparison){
  Local_Adaptation_New <- Load_Local_Adaptation_Data(Compare_Airfield_Dir, Local_Iteration_Version)
  Local_Adaptation_Old <- Load_Local_Adaptation_Data(Compare_Airfield_Dir, Compare_Version)
  #if (!is.na(Local_Adaptation_Old) & !is.na(Local_Adaptation_New)){
    Local_Adaptation_Compare <- full_join(Local_Adaptation_New, Local_Adaptation_Old, by = c("Parameter")) %>% rename(Value_Old = Value.y, Value_New = Value.x)
    Local_Adaptation_Differences <- filter(Local_Adaptation_Compare, Value_Old != Value_New)
    fwrite(Local_Adaptation_Compare, file.path(Adaptation_Compare_Dir, paste0("Local Adaptation Comparison (v", Local_Iteration_Version, " & v", Compare_Version, ").csv")))
    fwrite(Local_Adaptation_Differences, file.path(Adaptation_Compare_Dir, paste0("Local Adaptation Differences (v", Local_Iteration_Version, " & v", Compare_Version, ").csv")))
  #} else {
  #  message(paste0("Cannot output Local Adaptation Comparison file."))
  #}
}

# Filter Counts comparison
if (Do_Version_Comparison){
  Filter_Counts_New <- Load_Output_Data("Filter Stats", Airfield_Dir, NA, Local_Iteration_Version)
  Filter_Counts_Old <- Load_Output_Data("Filter Stats", Airfield_Dir, NA, Compare_Version)
  Seg_Filter_Compare <- full_join(
    select(Filter_Counts_New, Filter, Total_Seg_Issues_New = Seg_Total_Occurences),
    select(Filter_Counts_Old, Filter, Total_Seg_Issues_Old = Seg_Total_Occurences),
    by = c("Filter")) %>%
    filter(Filter != "Original") %>%
    mutate(Difference = Total_Seg_Issues_New - Total_Seg_Issues_Old)
  fwrite(Seg_Filter_Compare, file.path(Adaptation_Compare_Dir, paste0("Filter Result Differences (v", Local_Iteration_Version, " & v", Compare_Version, ").csv")))
}

# ------------------------------------------------------------------------------------------------- #
# Get the Separations used for Adaptation
# ------------------------------------------------------------------------------------------------- #

if (Safety_Case == 1 & Use_Secondary_Adaptation == F){
  Delivery_Chosen <- Delivery_Primary
  Case_Var <- "Surface_Wind_Group"
  Case_Var_Max <- Ref_Wind_SWS_Max
  Separations_Wake <- Separations_Primary_SWS_Wake
  Summary_Wake <- Summary_Primary_SWS_Wake
  IAS_Values <- IAS_Values_Primary
  if (Operation == "IA PWS"){Separations_Aircraft <- Separations_Primary_SWS_Aircraft}
  if (Operation == "IA PWS" & Operator_Enabled){Separations_Operator <- Separations_Primary_SWS_Operator}
}
if (Safety_Case == 1 & Use_Secondary_Adaptation == T){
  Delivery_Chosen <- Delivery_Secondary
  Case_Var <- "Surface_Wind_Group"
  Case_Var_Max <- Ref_Wind_SWS_Max
  Separations_Wake <- Separations_Secondary_SWS_Wake
  Summary_Wake <- Summary_Secondary_SWS_Wake
  IAS_Values <- IAS_Values_Secondary
  if (Operation == "IA PWS"){Separations_Aircraft <- Separations_Secondary_SWS_Aircraft}
  if (Operation == "IA PWS" & Operator_Enabled){Separations_Operator <- Separations_Secondary_SWS_Operator}
}
if (Safety_Case == 2 & Use_Secondary_Adaptation == F){
  Delivery_Chosen <- Delivery_Primary
  Case_Var <- "Surface_Headwind_Group"
  Case_Var_Max <- Ref_Wind_SHW_Max
  Separations_Wake <- Separations_Primary_SHW_Wake
  Summary_Wake <- Summary_Primary_SHW_Wake
  IAS_Values <- IAS_Values_Primary
  if (Operation == "IA PWS"){Separations_Aircraft <- Separations_Primary_SHW_Aircraft}
  if (Operation == "IA PWS" & Operator_Enabled){Separations_Operator <- Separations_Primary_SHW_Operator}
}
if (Safety_Case == 2 & Use_Secondary_Adaptation == T){
  Delivery_Chosen <- Delivery_Secondary
  Case_Var <- "Surface_Headwind_Group"
  Case_Var_Max <- Ref_Wind_SHW_Max
  Separations_Wake <- Separations_Secondary_SHW_Wake
  Summary_Wake <- Summary_Secondary_SHW_Wake
  IAS_Values <- IAS_Values_Secondary
  if (Operation == "IA PWS"){Separations_Aircraft <- Separations_Secondary_SHW_Aircraft}
  if (Operation == "IA PWS" & Operator_Enabled){Separations_Operator <- Separations_Secondary_SHW_Operator}
}

# --------------------------------------------------------------------------------------------------------- #
# Get Weighting Data for Weighted Wake & TBS Table
# --------------------------------------------------------------------------------------------------------- #

# Get the Wake counts for the current Separations & save to file.
TBS_Table_Counts <- Separations_Wake %>% select(Follower_WTC, Separation_Distance, Count) %>% unique()
fwrite(TBS_Table_Counts, file.path(Local_Iteration_Dir, paste0("TBS Table Recat Counts v", Local_Iteration_Version, ".csv")))

# Load local TBS Table counts if setting active
if (Counts_Local){
  TBS_Table_Counts <- Load_Output_Data("TBS Table Recat Counts", Airfield_Dir, NA, Counts_Version_Alt)
}

# Create summary of wake cat and Aircraft to get counts
Summary_Weighting <- Create_Summary_Main(IAS_Values, "Wake_Cat", "Aircraft_Type", NA, Case_Var, Delivery_Chosen) %>% 
  Create_seps(Recat_Wake_Dist, Recat_ROT_Dist, Runway, level = "Wake", Case_Var, Case_Var_Max)

# Get the weighting values to be used
Summary_Weighting <- filter(Summary_Weighting, Count > Min_Weighting_Obs) ## New Addition
Summary_Weighting <- Summary_Weighting %>% group_by(Runway, Leader_WTC, Follower_WTC, Separation_Distance) %>% mutate(Total_Count = sum(Count, na.rm=T)) %>% ungroup() %>%
  mutate(Weighting_Value = Count / Total_Count)

Aircraft_Counts <- Summary_Weighting %>% select(Follower_WTC, Separation_Distance, Aircraft_Type, Count) %>% unique()
fwrite(Aircraft_Counts, file.path(Local_Iteration_Dir, paste0("Summary AC Recat Counts v", Local_Iteration_Version, ".csv")))

# Get the weighting values for the current Separations and remove 
Weighting_Values <- Summary_Weighting %>% select(Follower_WTC, Separation_Distance, Aircraft_Type, Weighting_Value) %>% unique() %>%
  mutate(Weighting_Value = ifelse(is.na(Weighting_Value) | Weighting_Value == 0, 1, Weighting_Value))
Summary_Weighting <- select(Summary_Weighting, -c("Weighting_Value", "Total_Count"))
fwrite(Weighting_Values, file.path(Local_Iteration_Dir, paste0("Summary AC Recat Weightings v", Local_Iteration_Version, ".csv")))

# If local weightings to be used, load them and overwrite
if (Weighting_Local){
  
  # Get a comparison of the Counts/Weightings by Aircraft Type
  Weighting_Values_Old <- Load_Output_Data("Summary AC Recat Weightings", Airfield_Dir, NA, Weighting_Version_Alt)
  Aircraft_Counts_Old <- Load_Output_Data("Summary AC Recat Counts", Airfield_Dir, NA, Weighting_Version_Alt)
  Weighting_Compare_Old <- full_join(Weighting_Values_Old, Aircraft_Counts_Old, by = c("Follower_WTC", "Separation_Distance", "Aircraft_Type")) %>%
    rename(Count_Old = Count, Weighting_Old = Weighting_Value)
  Weighting_Compare <- full_join(Weighting_Values, Aircraft_Counts, by = c("Follower_WTC", "Separation_Distance", "Aircraft_Type")) %>%
    rename(Count_New = Count, Weighting_New = Weighting_Value)
  Weighting_Compare <- full_join(Weighting_Compare, Weighting_Compare_Old, by = c("Follower_WTC", "Separation_Distance", "Aircraft_Type")) %>%
    mutate(Weighting_Old = ifelse(is.na(Weighting_Old), 0, round(Weighting_Old*100, 2)),
           Weighting_New = ifelse(is.na(Weighting_New), 0, round(Weighting_New*100, 2)),
           Count_Old = ifelse(is.na(Count_Old), 0, Count_Old),
           Count_New = ifelse(is.na(Count_New), 0, Count_New),
           Weighting_Diff = Weighting_Old - Weighting_New,
           Count_Diff = Count_Old - Count_New) %>% arrange(Follower_WTC, Separation_Distance, desc(Weighting_Diff))
  rm(Aircraft_Counts_Old, Weighting_Compare_Old)
  fwrite(Weighting_Compare, file.path(Adaptation_Compare_Dir, paste0("Weightings v", Local_Iteration_Version, " & v", Weighting_Version_Alt, ".csv")))
  
  if (Weighting_Method == 1){Weighting_Values <- Weighting_Values_Old}
  if (Weighting_Method == 2){
    Weighting_Values <- select(Weighting_Compare, Follower_WTC, Separation_Distance, Aircraft_Type, Weighting_Old, Weighting_New) %>% 
      mutate(Weighting_Value = ifelse(Weighting_New != 0 & Weighting_Old == 0, Weighting_New, NA)) %>%
      group_by(Follower_WTC, Separation_Distance) %>% mutate(Reserved_New_Weight = sum(Weighting_Value, na.rm = T)) %>%
      ungroup() %>% mutate(Weighting_Value = ifelse(is.na(Weighting_Value), (100 - Reserved_New_Weight)/100 * Weighting_Old, Weighting_Value)) %>%
      select(Follower_WTC, Separation_Distance, Aircraft_Type, Weighting_Value) %>% 
      group_by(Follower_WTC, Separation_Distance) %>% mutate(Total_Weight = sum(Weighting_Value, na.rm = T)) %>%
      mutate(Weighting_Value = Weighting_Value / Total_Weight) %>% select(-Total_Weight)
  }
}

# Write the "Final" Weighting Values
fwrite(Weighting_Values, file.path(Local_Iteration_Dir, paste0("Final AC Weightings v", Local_Iteration_Version, ".csv")))

# Join on the weighting parameters
Summary_Weighting <- left_join(Summary_Weighting, Weighting_Values, by = c("Follower_WTC", "Separation_Distance", "Aircraft_Type"))

# Select the relevant fields from the weighted data
Summary_Weighting <- select(Summary_Weighting,
                            Runway, Leader_WTC, Follower_WTC, Constraint, Separation_Distance, Aircraft_Type, 
                            Count_AC = Count, Median_Speed_AC = Median_Speed, 
                            Weighting_Value)

# Calculate the weighted median speed and calculate other parameters as per normal summary
Summary_Weighting <- filter(Summary_Weighting, !is.na(Median_Speed_AC))
Summary_Weighting <- Summary_Weighting %>%
  group_by(Runway, Leader_WTC, Follower_WTC, Constraint, Separation_Distance) %>%
  summarise(Count = sum(Count_AC, na.rm = T),
            Median_Speed = weighted.mean(Median_Speed_AC, Weighting_Value, na.rm = T),
            Pc05_Speed = quantile(Median_Speed_AC, 0.05, na.rm = T),
            Pc95_Speed = quantile(Median_Speed_AC, 0.95, na.rm = T)) %>%
  ungroup() %>%
  mutate(DME_Start = Delivery_Chosen,
         DME_End = Delivery_Chosen + Separation_Distance,
         Flying_Time = 3600 * Separation_Distance/Median_Speed)

# Replace certain categories with data weighted by ac type
if (Use_Weighted_Wake){
  Separations_Wake <- filter(Separations_Wake, Follower_WTC %!in% Weighted_Types)
  Summary_Weighting <- filter(Summary_Weighting, Follower_WTC %in% Weighted_Types)
  Separations_Wake <- rbind(Separations_Wake, Summary_Weighting) %>%
    arrange(Runway, Leader_WTC, Follower_WTC, Separation_Distance)
}

# Join on the counts again for TBS table
Separations_Wake <- Separations_Wake %>%
  select(-Count) %>%
  left_join(TBS_Table_Counts, by = c("Follower_WTC", "Separation_Distance"))

# Write the data
fwrite(Separations_Wake, file.path(Local_Iteration_Dir, paste0("Adjusted Separations (Recat) v", Local_Iteration_Version, ".csv")))
if (Operation == "IA PWS"){fwrite(Separations_Aircraft, file.path(Local_Iteration_Dir, paste0("Separations (Aircraft) v", Local_Iteration_Version, ".csv")))}
if (Operation == "IA PWS" & Operator_Enabled){fwrite(Separations_Operator, file.path(Local_Iteration_Dir, paste0("Separations (Operator) v", Local_Iteration_Version, ".csv")))}

# --------------------------------------------------------------------------------------------------------- #


# Get the Full Adaptation Sets
if (Operation == "IA"){
  TBS_Values <- Get_TBS_Table_Distances(Airfield)
  TBS_Adaptation_Full <- Create_TBS_Adaptation(Separations_Wake)
  fwrite(TBS_Adaptation_Full, file.path(Adaptation_Full_Dir, paste0("TBS Table Adaptation v", Local_Iteration_Version, ".csv")))
  if (Do_Version_Comparison){
    TBS_Adaptation_Full_Prev <- Load_Output_Data("TBS Table Adaptation", Airfield_Dir, Adaptation_Full_Dir_Name, Compare_Version)
    TBS_Adaptation_Full_Compare <- Compare_SASAI_Adaptation(TBS_Adaptation_Full, TBS_Adaptation_Full_Prev, "TBS_Table", "Wake")
    fwrite(TBS_Adaptation_Full_Compare, file.path(Adaptation_Compare_Dir, paste0("TBS Table (v", Local_Iteration_Version, " ", Airfield, " & v", Compare_Version, " ", Compare_Airfield, ").csv")))
  }
  if (Output_Adaptation){
    TBS_Adaptation_Speeds <- select(TBS_Adaptation_Full, Reference_Wake_Separation_Distance, Assumed_Wake_Separation_IAS)
    TBS_Adaptation_Times <- select(TBS_Adaptation_Full, Reference_Wake_Separation_Distance, Reference_Wake_Separation_Time)
    Save_Adaptation_Table(TBS_Adaptation_Speeds, TBS_Adaptation_Speeds_Name, Airfield, Adaptation_Dir)
    Save_Adaptation_Table(TBS_Adaptation_Times, TBS_Adaptation_Times_Name, Airfield, Adaptation_Dir)
  }
}

Wake_Adaptation_Full_Wake <- Create_Wake_Adaptation(Separations_Wake, level = "Wake")
fwrite(Wake_Adaptation_Full_Wake, file.path(Adaptation_Full_Dir, paste0("Recat Wake Adaptation v", Local_Iteration_Version, ".csv")))
if (Do_Version_Comparison){
  Wake_Adaptation_Full_Wake_Prev <- Load_Output_Data("Recat Wake Adaptation", Airfield_Dir, Adaptation_Full_Dir_Name, Compare_Version)
  Wake_Adaptation_Full_Wake_Compare <- Compare_SASAI_Adaptation(Wake_Adaptation_Full_Wake, Wake_Adaptation_Full_Wake_Prev, "Wake_Separation", "Wake")
  fwrite(Wake_Adaptation_Full_Wake_Compare, file.path(Adaptation_Compare_Dir, paste0("Recat Wake (v", Local_Iteration_Version, " ", Airfield, " & v", Compare_Version, " ", Compare_Airfield, ").csv")))
}
if (Output_Adaptation){
  if (!ORD_Profile_TBS_Calcs){
    Wake_Adaptation_Speeds_Wake <- select(Wake_Adaptation_Full_Wake, Leader_WTC, Follower_WTC, Assumed_Wake_Separation_IAS)
    Save_Adaptation_Table(Wake_Adaptation_Speeds_Wake, Wake_Adaptation_Speeds_Wake_Name, Airfield, Adaptation_Dir)
  }
  Wake_Adaptation_Times_Wake <- select(Wake_Adaptation_Full_Wake, Leader_WTC, Follower_WTC, Reference_Wake_Separation_Time)
  Save_Adaptation_Table(Wake_Adaptation_Times_Wake, Wake_Adaptation_Times_Wake_Name, Airfield, Adaptation_Dir)
}


ROT_Adaptation_Full_Wake <- Create_ROT_Adaptation(Separations_Wake, level = "Wake")
fwrite(ROT_Adaptation_Full_Wake, file.path(Adaptation_Full_Dir, paste0("Recat ROT Adaptation v", Local_Iteration_Version, ".csv")))
if (Do_Version_Comparison){
  ROT_Adaptation_Full_Wake_Prev <- Load_Output_Data("Recat ROT Adaptation", Airfield_Dir, Adaptation_Full_Dir_Name, Compare_Version)
  ROT_Adaptation_Full_Wake_Compare <- Compare_SASAI_Adaptation(ROT_Adaptation_Full_Wake, ROT_Adaptation_Full_Wake_Prev, "ROT_Spacing", "Wake")
  fwrite(ROT_Adaptation_Full_Wake_Compare, file.path(Adaptation_Compare_Dir, paste0("Recat ROT (v", Local_Iteration_Version, " ", Airfield, " & v", Compare_Version, " ", Compare_Airfield, ").csv")))
}
if (Output_Adaptation){
  if (!ORD_Profile_TBS_Calcs){
    ROT_Adaptation_Speeds_Wake <- select(ROT_Adaptation_Full_Wake, Runway, Leader_WTC, Follower_WTC, Assumed_ROT_Spacing_IAS)
    Save_Adaptation_Table(ROT_Adaptation_Speeds_Wake, ROT_Adaptation_Speeds_Wake_Name, Airfield, Adaptation_Dir)
  }
  ROT_Adaptation_Times_Wake <- select(ROT_Adaptation_Full_Wake, Runway, Leader_WTC, Follower_WTC, Reference_ROT_Spacing_Time)
  Save_Adaptation_Table(ROT_Adaptation_Times_Wake, ROT_Adaptation_Times_Wake_Name, Airfield, Adaptation_Dir)
}

if (Operation == "IA PWS"){
  Wake_Adaptation_Full_Aircraft <- Create_Wake_Adaptation(Separations, Level = "Aircraft")
  fwrite(Wake_Adaptation_Full_Aircraft, file.path(Adap_Output_Dir, paste0("ACTP Wake Adaptation v", Full_Version, ".csv")))
  if (Do_Version_Comparison){
    Wake_Adaptation_Full_Aircraft_Prev <- Load_Output_Data(Name, Base_Dir, "Adaptation", Airfield, Compare_Version)
    Wake_Adaptation_Full_Aircraft_Compare <- Compare_SASAI_Adaptation(Wake_Adaptation_Full_Wake, Wake_Adaptation_Full_Wake_Prev, "Wake_Separation", "Aircraft")
    fwrite(Wake_Adaptation_Full_Aircraft_Compare, file.path(Compare_Dir, paste0("ACTP Wake Adaptation Comparison (v", Full_Version, " & v", Compare_Version, ").csv")))
  }
  if (Output_Adaptation){
    if (!ORD_Profile_TBS_Calcs){
      Wake_Adaptation_Speeds_Aircraft <- select(Wake_Adaptation_Full_Aircraft, Leader_Aircraft_Type, Follower_Aircraft_Type, Assumed_Wake_Separation_IAS)
      Save_Adaptation_Data(Wake_Adaptation_Speeds_Aircraft, Wake_Adaptation_Speeds_Aircraft_Name, Airfield, Adap_Output_Dir)
    }
    Wake_Adaptation_Times_Aircraft <- select(Wake_Adaptation_Full_Aircraft, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_Wake_Separation_Time)
    Save_Adaptation_Data(Wake_Adaptation_Times_Aircraft, Wake_Adaptation_Times_Aircraft_Name, Airfield, Adap_Output_Dir)
  }
  
  
  if (Output_ROT_Aircraft){
    ROT_Adaptation_Full_Aircraft <- Create_ROT_Adaptation(Separations, Level = "Aircraft")
    fwrite(ROT_Adaptation_Full_Aircraft, file.path(Adap_Output_Dir, paste0("ACTP ROT Adaptation v", Full_Version, ".csv")))
    if (Do_Version_Comparison){
      ROT_Adaptation_Full_Aircraft_Prev <- Load_Output_Data(Name, Base_Dir, "Adaptation", Airfield, Compare_Version)
      ROT_Adaptation_Full_Aircraft_Compare <- Compare_SASAI_Adaptation(ROT_Adaptation_Full_Aircraft, ROT_Adaptation_Full_Aircraft_Prev, "ROT_Spacing", "Aircraft")
      fwrite(ROT_Adaptation_Full_Aircraft_Compare, file.path(Compare_Dir, paste0("ACTP ROT Adaptation Comparison (v", Full_Version, " & v", Compare_Version, ").csv")))
    }
    if (Output_Adaptation){
      if (!ORD_Profile_TBS_Calcs){
        ROT_Adaptation_Speeds_Aircraft <- select(ROT_Adaptation_Full_Aircraft, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Assumed_ROT_Spacing_IAS)
        Save_Adaptation_Data(ROT_Adaptation_Speeds_Aircraft, ROT_Adaptation_Speeds_Name_Aircraft, Airfield, Adap_Output_Dir)
      }
      ROT_Adaptation_Times_Aircraft <- select(ROT_Adaptation_Full_Aircraft, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_ROT_Spacing_Time)
      Save_Adaptation_Data(ROT_Adaptation_Times_Aircraft, ROT_Adaptation_Times_Wake_Aircraft, Airfield, Adap_Output_Dir)
    }
  }
}

if (Operation == "IA PWS" & Operator_Enabled){
  Wake_Adaptation_Full_Operator <- Create_Wake_Adaptation(Separations, Level = "Operator")
  fwrite(Wake_Adaptation_Full_Operator, file.path(Adap_Output_Dir, paste0("AC Operator Wake Adaptation v", Full_Version, ".csv")))
  if (Do_Version_Comparison){
    Wake_Adaptation_Full_Operator_Prev <- Load_Output_Data(Name, Base_Dir, "Adaptation", Airfield, Compare_Version)
    Wake_Adaptation_Full_Operator_Compare <- Compare_SASAI_Adaptation(Wake_Adaptation_Full_Operator, Wake_Adaptation_Full_Operator_Prev, "Wake_Separation", "Operator")
    fwrite(Wake_Adaptation_Full_Aircraft_Compare, file.path(Compare_Dir, paste0("AC Operator Wake Adaptation Comparison (v", Full_Version, " & v", Compare_Version, ").csv")))
  }
  if (Output_Adaptation){
    if (!ORD_Profile_TBS_Calcs){
      Wake_Adaptation_Speeds_Operator <- select(Wake_Adaptation_Full_Operator, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Assumed_Wake_Separation_IAS)
      Save_Adaptation_Data(Wake_Adaptation_Speeds_Operator, Wake_Adaptation_Speeds_Operator_Name, Airfield, Adap_Output_Dir)
    }
    Wake_Adaptation_Times_Operator <- select(Wake_Adaptation_Full_Operator, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_Wake_Separation_Time)
    Save_Adaptation_Data(Wake_Adaptation_Times_Operator, Wake_Adaptation_Times_Operator_Name, Airfield, Adap_Output_Dir)
  }
  
  
  if (Output_ROT_Operator){
    ROT_Adaptation_Full_Operator <- Create_ROT_Adaptation(Separations, Level = "Operator")
    fwrite(ROT_Adaptation_Full_Operator, file.path(Adap_Output_Dir, paste0("AC Operator ROT Adaptation v", Full_Version, ".csv")))
    if (Do_Version_Comparison){
      ROT_Adaptation_Full_Operator_Prev <- Load_Output_Data(Name, Base_Dir, "Adaptation", Airfield, Compare_Version)
      ROT_Adaptation_Full_Operator_Compare <- Compare_SASAI_Adaptation(ROT_Adaptation_Full_Operator, ROT_Adaptation_Full_Operator_Prev, "ROT_Spacing", "Operator")
      fwrite(ROT_Adaptation_Full_Operator_Compare, file.path(Compare_Dir, paste0("AC Operator ROT Adaptation Comparison (v", Full_Version, " & v", Compare_Version, ").csv")))
    }
    if (Output_Adaptation){
      if (!ORD_Profile_TBS_Calcs){
        ROT_Adaptation_Speeds_Operator <- select(ROT_Adaptation_Full_Operator, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Assumed_ROT_Spacing_IAS)
        Save_Adaptation_Data(ROT_Adaptation_Speeds_Operator, ROT_Adaptation_Speeds_Name_Operator, Airfield, Adap_Output_Dir)
      }
      ROT_Adaptation_Times_Operator <- select(ROT_Adaptation_Full_Operator, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator, Reference_ROT_Spacing_Time)
      Save_Adaptation_Data(ROT_Adaptation_Times_Operator, ROT_Adaptation_Times_Wake_Operator, Airfield, Adap_Output_Dir)
    }
  }
}

# ------------------------------------------------------------------------------------------------- #
# TEMP: Combine Speeds from ROT/Wake.
# ------------------------------------------------------------------------------------------------- #

### TEMP:
# ROT_Temp <- fread(file.path(Adaptation_Full_Dir, paste0("Recat ROT Adaptation v", Local_Iteration_Version, ".csv")))
# Wake_Temp <- fread(file.path(Adaptation_Full_Dir, paste0("Recat Wake Adaptation v", Local_Iteration_Version, ".csv")))
# 
# ROT_Temp <- select(ROT_Temp, -Runway, -Reference_ROT_Spacing_Time) %>% unique() %>% 
#   rename(Separation_Distance = Reference_ROT_Spacing_Distance,
#          Reference_IAS = Assumed_ROT_Spacing_IAS)
# 
# Wake_Temp <- select(Wake_Temp, -Reference_Wake_Separation_Time) %>% unique() %>%
#   rename(Separation_Distance = Reference_Wake_Separation_Distance,
#          Reference_IAS = Assumed_Wake_Separation_IAS)
# 
# Temp <- rbind(Wake_Temp, ROT_Temp) %>% arrange(Leader_WTC, Follower_WTC) %>% unique()
# fwrite(Temp, file.path(Adaptation_Full_Dir, "All_Ref_Speeds.csv"))
