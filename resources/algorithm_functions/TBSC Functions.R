
#-----------------------------------------------------------------------------------------------------------#
# Main Processing Functions
#-----------------------------------------------------------------------------------------------------------#

Apply_Achieved_Separation_Filter <- function(df, Legacy_Delivery, SA_Max, SA_Max_Alt){
  
  # Hardcoded for Now
  Alt_AC_Types <- c("A388")
  Accuracy_Var <- paste0("Separation_Accuracy_", Legacy_Delivery, "DME")
  
  df1 <- filter(df, Leader_Aircraft_Type %in% Alt_AC_Types & !!sym(Accuracy_Var) <= SA_Max_Alt)
  df2 <- filter(df, Leader_Aircraft_Type %!in% Alt_AC_Types & !!sym(Accuracy_Var) <= SA_Max)
  
  df <- rbind(df1, df2) %>% arrange(Mode_S_Wind_Seg_ID)
  
  return(df)
}

# This function takes the data after spreading IAS by DME_Seg, cuts DataFrame for IAS
# between start and dist, and averages these values

avg_spd2 <-  function(df, start, dist) {
  df <- df[,(start+2):(dist+start+1)]
  result <- apply(df, 1, mean, na.rm=FALSE)
  return(result)
}

# This function operates AFTER the avg_spd function and calculates averages between
# two successive avg_spd output values (xDME - x+1DME) for half mile average IAS

half_mile2 <- function(df, start, dist, min, max){
  half <- max+start+3+dist-min
  two <- df[half:(half+1)]
  result <- apply(two, 1, mean, na.rm=FALSE)
  return(result)
}

# This function loops between a min and max DME segment to provide average IAS values for
# (start - kDME) where k is in the range min:max. This also provides the averages between
# each successive IAS - giving half mile separation values for certain VFR/ROT/Wake scenarios 
# Currently gives warning message for melt function. Change to pivot_longer in next version  

Get_Speeds <- function(df, start, min, max, halves){
  
  message("Getting required flight information...")
  dfn <- select(df, Flight_Plan_ID, Wake_Cat, Aircraft_Type, 
                Runway, Surface_Headwind_Group, Surface_Wind_Group) %>% unique()
  message("Isolated filtering details for each flight.")
  
  dft <- select(df, Flight_Plan_ID, DME_Seg, Ave_SPD) %>% filter(DME_Seg<=(max + start))
  dft_piv <- pivot_wider(dft, names_from = DME_Seg, values_from = Ave_SPD, names_prefix = "Speed_DME_") 
  
  for (dist in min:max){
    speeds <- as.data.frame(avg_spd2(dft_piv, start, dist))
    names(speeds) <- paste0(dist)
    dft_piv <- as.data.frame(cbind(dft_piv, speeds))
    message("Bound Average Speeds for Separation of ", dist, ", From ", start, "DME to ", (dist+start), "DME")
  }
  
  for (dist in halves){
    speeds <- as.data.frame(half_mile2(dft_piv, start, dist, min, max))
    names(speeds) <- paste0((dist+0.5))
    dft_piv <- as.data.frame(cbind(dft_piv, speeds))
    message("Bound Average Speeds for Separation of ", dist+0.5, ", From ", start, "DME to ", (dist+start+0.5), "DME")
  }
  
  id <- dft_piv[1:1]
  dft_piv2 <- dft_piv[(max+start+3):ncol(dft_piv)]
  dft_piv2 <- cbind(id, dft_piv2)
  
  speed_output <- pivot_longer(dft_piv2, cols=2:ncol(dft_piv2), names_to="Separation_Distance", values_to="Ave_SPD")
  speed_output$Separation_Distance <- as.numeric(speed_output$Separation_Distance)
  
  speed_output <- right_join(speed_output, dfn, by=c("Flight_Plan_ID"))
  speed_values <- filter(speed_output, !is.na(Ave_SPD))
  
  return(speed_values)
  
}
#-----------------------------------------------------------------------------------------------------------#
# Summary & Output Functions
#-----------------------------------------------------------------------------------------------------------#

# This function creates the "Flying_Time_Analysis.csv" output.
# Includes all necessary separation distances for each follower WTC 
# and each surface wind group.

Create_Summary_Main <- function(df, Group_Var_1, Group_Var_2, Group_Var_3, Case_Var, start) {
  if (!is.na(Group_Var_2) & !is.na(Group_Var_3)){df <- df %>% group_by(!!sym(Group_Var_1), !!sym(Group_Var_2), !!sym(Group_Var_3), Separation_Distance, !!sym(Case_Var))}
  if (!is.na(Group_Var_2) & is.na(Group_Var_3)){df <- df %>% group_by(!!sym(Group_Var_1), !!sym(Group_Var_2), Separation_Distance, !!sym(Case_Var))}
  if (is.na(Group_Var_2) & is.na(Group_Var_3)){df <- df %>% group_by(!!sym(Group_Var_1), Separation_Distance, !!sym(Case_Var))}
  summary <- summarise(df,
                       Count = sum(!is.na(Ave_SPD)),
                       Median_Speed = median(Ave_SPD, na.rm=TRUE),
                       Pc05_Speed = quantile(Ave_SPD, 0.05, na.rm = TRUE),
                       Pc95_Speed = quantile(Ave_SPD, 0.95, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(DME_Start = as.numeric(start),
           DME_End = Separation_Distance+start) %>% filter(Count>0)
  return(summary)
}



Create_seps <- function(summary, wake_sep, ROT_sep, Runway, level, Group_Var, Group_Var_Max){
  
  if (Group_Var == "Surface_Headwind_Group"){
    Filter <- paste0("(", -Group_Var_Max, ",", Group_Var_Max, "]")
  }
  if (Group_Var == "Surface_Wind_Group"){
    Filter <- paste0("(", 0, ",", Group_Var_Max, "]")
  }
  
  summary <- summary %>%
    filter(!!sym(Group_Var) == Filter) %>%
    select(-!!sym(Group_Var))
  
  wake_sep1 <- rename(wake_sep, Ref_Distance = Reference_Wake_Separation_Distance) %>%
    mutate(Ref_Distance = Ref_Distance,
           Constraint = "Wake")
  ROT_sep1 <- rename(ROT_sep, Ref_Distance = Reference_ROT_Spacing_Distance) %>%
    mutate(Ref_Distance = Ref_Distance,
           Constraint = "ROT")
  for (i in 1:length(Runway$Runway)){
    rwwake <- mutate(wake_sep1, Runway = Runway$Runway[i])
    if (i == 1){
      wake_full <- rwwake
    }
    else{wake_full <- rbind(wake_full, rwwake)}
  }
  wake_full <- select(wake_full, Runway, everything())
  
  all_seps <- rbind(wake_full, ROT_sep1)
  if (level == "Wake"){seps <- right_join(summary, all_seps, by=c("Wake_Cat" = "Follower_WTC", "Separation_Distance" = "Ref_Distance"))}
  if (level == "Aircraft"){seps <- right_join(summary, all_seps, by=c("Aircraft_Type" = "Follower_Aircraft_Type", "Separation_Distance" = "Ref_Distance"))}
  if (level == "Operator"){seps <- right_join(summary, all_seps, by=c("Operator" = "Follower_Operator", "Aircraft_Type" = "Follower_Aircraft_Type", "Separation_Distance" = "Ref_Distance"))}
  seps <- seps %>%
    select(Runway, Leader_WTC, everything()) %>% rename(Follower_WTC = Wake_Cat) %>%
    mutate(Flying_Time = 3600 * Separation_Distance/Median_Speed)
  seps <- seps[order(seps$Runway, seps$Separation_Distance),]
  seps <- arrange(seps, Runway, Separation_Distance)
  return(seps)
  
}

Create_Wake_Adaptation <- function(seps, level){
  wake <- filter(seps, Constraint == "Wake") %>% select(-c("Runway", "Pc05_Speed", "Pc95_Speed",
                                                           "Count", "DME_Start", "DME_End", "Constraint")) %>% unique() %>%
    rename(Reference_Wake_Separation_Distance = Separation_Distance,
           Assumed_Wake_Separation_IAS = Median_Speed,
           Reference_Wake_Separation_Time = Flying_Time) %>%
    mutate(Assumed_Wake_Separation_IAS = round(Assumed_Wake_Separation_IAS, 0),
           Reference_Wake_Separation_Time= round(Reference_Wake_Separation_Time, 0))
  if (level == "Wake"){wake <- arrange(wake, Leader_WTC, Follower_WTC)}
  if (level == "Aircraft"){wake <- arrange(wake, Leader_Aircraft_Type, Follower_Aircraft_Type)}
  if (level == "Operator"){wake <- arrange(wake, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator)}
  return(wake)
}

Create_ROT_Adaptation <- function(seps, level){
  rot <- filter(seps, Constraint == "ROT") %>% select(-c("Pc05_Speed", "Pc95_Speed",
                                                         "Count", "DME_Start", "DME_End", "Constraint")) %>%
    rename(Reference_ROT_Spacing_Distance = Separation_Distance,
           Assumed_ROT_Spacing_IAS = Median_Speed,
           Reference_ROT_Spacing_Time = Flying_Time) %>%
    mutate(Assumed_ROT_Spacing_IAS = round(Assumed_ROT_Spacing_IAS, 0),
           Reference_ROT_Spacing_Time= round(Reference_ROT_Spacing_Time, 0))
  if (level == "Wake"){rot <- arrange(rot, Runway, Leader_WTC, Follower_WTC)}
  if (level == "Aircraft"){rot <- arrange(rot, Runway, Leader_Aircraft_Type, Follower_Aircraft_Type)}
  if (level == "Operator"){rot <- arrange(rot, Runway, Leader_Aircraft_Type, Leader_Operator, Follower_Aircraft_Type, Follower_Operator)}
  return(rot)
}

Create_TBS_Adaptation <- function(seps){
  seps <- filter(seps, Separation_Distance %in% TBS_Values)
  tbs <- seps %>% group_by(Separation_Distance) %>% summarise(
    Reference_Wake_Separation_Time = weighted.mean(Flying_Time, Count, na.rm=T),
    Assumed_Wake_Separation_IAS = weighted.mean(Median_Speed, Count, na.rm=T)) %>%
    rename(Reference_Wake_Separation_Distance = Separation_Distance) %>%
    mutate(Reference_Wake_Separation_Time = round(Reference_Wake_Separation_Time, 0),
           Assumed_Wake_Separation_IAS = round(Assumed_Wake_Separation_IAS, 0))
  return(tbs)
}

Create_Legacy_Adaptation <- function(seps){
  seps <- select(seps, -c("Pc05_Speed", "Pc95_Speed",
                          "Count", "DME_Start", "DME_End")) %>% 
    rename(Assumed_WT_IAS = Median_Speed,
           Leader_WVI = Leader_ICAO4_WTC,
           Follower_WVI = Follower_ICAO4_WTC,
           WT_Separation_Distance = Separation_Distance,
           WT_Separation_Time = Flying_Time) %>%
    mutate(Assumed_WT_IAS = round(Assumed_WT_IAS, 0),
           WT_Separation_Time = round(WT_Separation_Time, 0))
}

Use_Speed_Parameter <- function(data, speed_type){
  if (speed_type == "IAS"){input <- data %>% mutate(Ave_SPD = Ave_Mode_S_IAS)}
  if (speed_type =="GSPD"){input <- data %>% mutate(Ave_SPD = Ave_Mode_S_GSPD)}
  if (speed_type =="TAS"){input <- data %>% mutate(Ave_SPD = Ave_Mode_S_TAS)}
  if (speed_type =="WE"){input <- data %>% mutate(Ave_SPD = Ave_Wind_Effect_IAS)}
  if (speed_type =="TRACK"){input <- data %>% mutate(Ave_SPD = Ave_Track_SPD)}
  if (speed_type == "DERIVED TRACK"){input <- mutate(data, Ave_SPD = ifelse(abs(Max_Track_Time - Min_Track_Time) > 0,
                                                                             3600 * (Max_RTT - Min_RTT) / (Max_Track_Time - Min_Track_Time), NA))}
  
  return(input)
}

#-----------------------------------------------------------------------------------------------------------#
# Plot Functions
#-----------------------------------------------------------------------------------------------------------#

# Case = 0: All Wind
# Case = 1: (-5, 5)kts Surface Headwind
# Case = 2: (0, 3)kts Surface Wind
FTA_Boxplot_General <- function(Plot_Dir, Version, Speeds, Distances, Local_Adaptation, Plot_Var, Start, Case, Facet_Var, Facet_Rows){
  
  # Values from Adaptation
  Min_SHW <- filter(Local_Adaptation, Parameter = "Minimum Absolute Surface Headwind") %>% select(Value) %>% as.numeric()
  Min_SWS <- filter(Local_Adaptation, Parameter = "Minimum Surface Wind Speed") %>% select(Value) %>% as.numeric()
  
  # Initial Manipulation
  Speeds <- Speeds %>% filter(Separation_Distance %in% Distances)
  Speeds$Sep_Dist_Factor = factor(Speeds$Separation_Distance)
  Speeds <- mutate(Speeds, Flying_Time = 3600 * (Separation_Distance / Ave_SPD))
  
  # Labels by Facet
  if (Facet_Var == "Wake_Cat"){facet_lab <- " by RECAT WTC"}
  if (Facet_Var == "Runway"){facet_lab <- " by Runway"}
  if (Facet_Var == "Surface_Wind_Group"){facet_lab <- " by Surface Wind Group"}
  if (Facet_Var == "Surface_Headwind_Group"){facet_lab <- " by Surface Headwind Group"}
  if (is.na(Facet_Var)){Facet_Lab <- " (Overall)"}
  
  # Filtering/Labels by Safety Case 
  if (Case == 0){
    Case_Lab <- "All Wind"
  }
  if (Case == 2){
    Filter_Value <- paste0("(", -Min_SHW, ",", Min_SHW, "]")
    Case_Lab <- paste0("(", Filter_Value, " kts Surface Headwind)")
    Speeds <- filter(Speeds, Surface_Headwind_Gp == Filter_Value)
  }
  if (Case == 1){
    Filter_Value <- paste0("(", 0, ",", Min_SWS, "]")
    Case_Lab <- paste0(Filter_Value, " kts Surface Wind Speed")
    Speeds <- filter(Speeds, Surface_Wind_Gp == Filter_Value)
  }
  
  if (Plot_Var == "Flying_Time"){
    Plot_Title_Var <- "Flying Time"
    Plot_Lab_Var <- paste0(Plot_Title_Var, " (s)")
  }
  if (Plot_Var == "Ave_SPD"){
    Plot_Title_Var <- "Indicated Airspeed"
    Plot_Lab_Var <- paste0(Plot_Title_Var, " (kts)")
  }
  
  Min_Value <- min(as.numeric(select(Speeds, !!sym(Plot_Var))))
  Max_Value <- max(as.numeric(select(Speeds, !!sym(Plot_Var))))
  Tolerance <- 0.2
  Low_Bound <- max(floor(Min_Value - 0.2*Max_Value), 0)
  High_Bound <- ceiling(Max_Value + 0.2*Max_Value)
  
  Plot_Title <- paste0(Plot_Title_Var, " Boxplot over Separation Distance", Facet_Lab)
  Plot_Subtitle <- paste0(Case_Lab, ", Measured from ", Start, "DME")
  
  Plot <- ggplot(data = Speeds,
                 mapping = aes(x = Sep_Dist_Factor, y = !!sym(Plot_Var))) +
    geom_boxplot(aes(group=Sep_Dist_Factor, fill = Sep_Dist_Factor)) +
    labs(x = "Separation Distance (NM)",
         y = Plot_Lab_Var,
         title = Plot_Title,
         subtitle = Plot_Subtitle) +
    theme(text = element_text(size=16), legend.position = "none") +
    ylim(Low_Bound, High_Bound)
  
  if (!is.na(Facet_Var)){Plot <- Plot + facet_wrap(~!!sym(Facet_Var), nrow = Facet_Rows)}
  
  # Save Plot
  png(file.path(Plot_Dir, paste0(Plot_Filename, " v", Version, ".png")))
  print(Plot)
  dev.off()
  
  return(plot)
}


FT_Boxplot_General <- function(speeds, start, case, facet, low_bound, high_bound){
  
  plot_data <- speeds
  plot_data <- plot_data %>% filter(Separation_Distance %in% c(3, 4, 5, 6, 7, 8))
  plot_data$Sep_Dist_Factor = factor(plot_data$Separation_Distance)
  plot_data <- mutate(plot_data, Flying_Time = 3600 * (Separation_Distance / Ave_SPD))
  
  if (facet == "Wake"){facet_lab <- "RECAT-EU Follower WTC"}
  if (facet == "Runway"){facet_lab <- "Runway"}
  
  if (case == 0){case_lab <- "(All Wind)"}
  if (case == 1){case_lab <- "((-5, 5) kts Surface Headwind)"
  filt <- "(-5,5]"
  plot_data <- filter(plot_data, Surface_Headwind_Group == filt)}
  if (case == 2){case_lab <- "((0, 3) kts Surface Wind)"
  filt <- "(0,3]"
  plot_data <- filter(plot_data, Surface_Wind_Group == filt)}
  
  plot_title <- paste0("Flying Time Boxplot over Separation Distance by ", facet_lab, " ", case_lab, " (", start, "DME)")
  
  plot <- ggplot(data = plot_data,
                 mapping = aes(x = Sep_Dist_Factor, y = Flying_Time)) +
    geom_boxplot(aes(group=Sep_Dist_Factor, fill = Sep_Dist_Factor)) +
    labs(x = "Separation Distance (NM)", y = "Flying Time (s)",
         title = plot_title) +
    theme(text = element_text(size=16), legend.position = "none") +
    ylim(low_bound, high_bound)
  
  if (facet == "Wake"){plot <- plot + facet_wrap(~Wake_Cat, nrow=2)} 
  if (facet == "Runway"){plot <- plot + facet_wrap(~Runway, nrow=3)} 
  
  return(plot)
}


# boxplot function for single RECAT EU types
bplot <- function(data, wake, low){
  Plot_Values <- c(3, 4, 5, 6, 7, 8)
  data <- filter(data, Wake_Cat == wake)
  data <- filter(data, Separation_Distance %in% Plot_Values)
  if (low == 1){data <- filter(data, Surface_Headwind_Group == "(-5,5]")}
  plot <- ggplot(data, mapping = aes(x = factor(Separation_Distance), y = Ave_SPD)) + 
    geom_boxplot(mapping = aes(group = factor(Separation_Distance), fill = factor(Separation_Distance))) 
  if (low == 0){lab <- "(All Wind)"} else {lab <- "(-5 to 5 kts)"}
  plot <- plot + labs(x = "Separation Distance (NM)", y = "IAS (kts)", title = paste0("IAS Values for Category ", wake, " ", lab)) +
    theme(text = element_text(size=12), legend.position = "none") + ylim(100, 200)
  return(plot)
}

# Plots by Aircraft Type

IAS_Aircraft_Boxplot <- function(Speeds, Wake, Airfield, Local_Iteration_Version){
  
  #Speeds <- IAS_Values
  #Wake <- "E"
  
  # Filter data for this Wake Category
  Speeds <- filter(Speeds, Wake_Cat == Wake, Separation_Distance %in% Plot_Values) %>% 
    select(Aircraft_Type, Separation_Distance, Ave_SPD)
  
  if (nrow(Speeds) == 0){return(ggplot())}
  
  Counts <- group_by(Speeds, Aircraft_Type, Separation_Distance) %>% summarise(Count = n(), Ave_SPD = median(Ave_SPD, na.rm = F)) %>% ungroup()
  
  # Base plot
  Plot <- ggplot(Speeds, mapping = aes(x = reorder(as.factor(Aircraft_Type), Ave_SPD, median), y = Ave_SPD)) + geom_boxplot() + facet_wrap(~Separation_Distance)
  
  # Get labels
  Title <- "IAS by Aircraft Type and Separation Distance"
  
  Subtitle <- paste0("Type ", Wake, ", ", Airfield, ", v", Local_Iteration_Version)
  
  # Add labels
  Plot <- Plot + labs(title = Title, subtitle = Subtitle, x = "Aircraft Type", y = "IAS (kts)") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  # Add Counts
  Plot <- Plot + geom_text(data = Counts, mapping=aes(label=Count), position=position_dodge(width=1.0), size=3)
  print(Plot)
  
  return(Plot)
}


Compare_SASAI_Adaptation <- function(Data1, Data2, Constraint, Level){
  
  # Get Grouping
  if (Constraint %in% c("Wake_Separation")){Grouping <- "Pair"}
  if (Constraint %in% c("ROT_Spacing", "Non_Wake_Separation", "Spacing")){Grouping <- "Runway, Pair"}
  
  # Get Variable Names (Assumes existing format)
  Dist_Var <- paste0("Reference_", Constraint, "_Distance")
  Time_Var <- paste0("Reference_", Constraint, "_Time")
  Speed_Var <- paste0("Assumed_", Constraint, "_IAS")
  if(Constraint == "TBS_Table"){
    Dist_Var <- "Reference_Wake_Separation_Distance"
    Time_Var <- "Reference_Wake_Separation_Time"
    Speed_Var <- "Assumed_Wake_Separation_IAS"
    Grouping <- Constraint
  }
  
  # Rename Variables
  Data1 <- Data1 %>%
    rename("IAS_New" := !!sym(Speed_Var),
           "Time_New" := !!sym(Time_Var))
  
  Data2 <- Data2 %>%
    rename("IAS_Old" := !!sym(Speed_Var),
           "Time_Old" := !!sym(Time_Var))
  
  # Get the Join Columns
  if (Level == "Wake"){Join_Columns <- c("Leader_WTC", "Follower_WTC")}
  if (Level == "Aircraft"){Join_Columns <- c("Leader_Aircraft_Type", "Follower_Aircraft_Type")}
  if (Level == "Operator"){Join_Columns <- c("Leader_Aircraft_Type", "Follower_Aircraft_Type", "Leader_Operator", "Follower_Operator")}
  if (Grouping == "Runway, Pair"){Join_Columns <- append(Join_Columns, "Runway")}
  if (Constraint == "TBS Table"){Join_Columns <- c("Reference_Wake_Separation_Distance")}
  
  # Attempt using EvalParse
  #Data_Compare <- full_join(Data1, Data2, by = eval(parse(text = Join_Columns)))
  if (Grouping == "Pair"){Data_Compare <- full_join(Data1, Data2, by = c("Leader_WTC", "Follower_WTC", "Reference_Wake_Separation_Distance"))}
  if (Grouping == "Runway, Pair"){Data_Compare <- full_join(Data1, Data2, by = c("Runway", "Leader_WTC", "Follower_WTC", "Reference_ROT_Spacing_Distance"))}
  if (Grouping == "TBS_Table"){Data_Compare <- full_join(Data1, Data2, by = c("Reference_Wake_Separation_Distance"))}
  
  # Now Calculate the Difference Columns
  Data_Compare <- mutate(Data_Compare,
                         `Time Delta (N-O)` = Time_New - Time_Old,
                         `IAS Delta (N-O)` = IAS_New - IAS_Old)
  
  return(Data_Compare)

}


#### Calculate Reference Values
Get_Unique_Distances <- function(Recat_Wake_Dist, Recat_ROT_Dist, Aircraft_Wake_Dist, Aircraft_ROT_Dist, Operator_Wake_Dist, Operator_ROT_Dist){
  Dist1 <- ifelse(!is.na(Recat_Wake_Dist), unique(Recat_Wake_Dist$Reference_Wake_Separation_Distance), c(0))
  Dist2 <- ifelse(!is.na(Recat_ROT_Dist), unique(Recat_ROT_Dist$Reference_ROT_Spacing_Distance), c(0))
  Dist3 <- ifelse(!is.na(Aircraft_Wake_Dist), unique(Aircraft_Wake_Dist$Reference_Wake_Separation_Distance), c(0))
  Dist4 <- ifelse(!is.na(Aircraft_ROT_Dist), unique(Aircraft_ROT_Dist$Reference_ROT_Spacing_Distance), c(0))
  Dist5 <- ifelse(!is.na(Operator_Wake_Dist), unique(Operator_Wake_Dist$Reference_Wake_Separation_Distance), c(0))
  Dist6 <- ifelse(!is.na(Operator_ROT_Dist), unique(Operator_ROT_Dist$Reference_ROT_Spacing_Distance), c(0))
  Dist <- append(Dist1, Dist2) 
  Dist <- append(Dist, Dist3)
  Dist <- append(Dist, Dist4)
  Dist <- append(Dist, Dist5)
  Dist <- append(Dist, Dist6)
  Dist <- unique(Dist)
  Dist <- data.frame(D = Dist) %>% arrange(D) %>% filter(D > 0)
  Dist <- as.numeric(Dist$D)
  return(Dist)
}

Get_Whole_Distances <- function(Dist){
  Dist <- data.frame(D = Dist) %>% filter(D == floor(D))
  Dist <- as.numeric(Dist$D)
  return(Dist)
}

Get_Half_Distances <- function(Dist){
  Dist <- data.frame(D = Dist) %>% filter(D == floor(D) + 0.5)
  Dist <- as.numeric(floor(Dist$D))
  return(Dist)
}


Get_Single_Flight_Count <- function(Data){
  Data <- group_by(Data, Flight_Plan_ID) %>% mutate(ID = row_number()) %>% filter(ID == 1) %>% select(-ID) 
  return(nrow(Data))
}

Add_To_Filter_Table <- function(Filter_Table, Data_Original, Data_Original_Rem, Data_Previous, Data_Current, Filter_Label){
  Original_Size_Segs <- nrow(Data_Original)
  Original_Size_AC <- Get_Single_Flight_Count(Data_Original)
  Original_Rem_Size_Segs <- nrow(Data_Original_Rem)
  Original_Rem_Size_AC <- Get_Single_Flight_Count(Data_Original_Rem)
  Previous_Size_Segs <- nrow(Data_Previous)
  Previous_Size_AC <- Get_Single_Flight_Count(Data_Previous)
  Current_Size_Segs <- nrow(Data_Current)
  Current_Size_AC <- Get_Single_Flight_Count(Data_Current)
  Filter_Table_Add <- data.frame(
    Filter = Filter_Label,
    New_Seg_Sample_Size = Current_Size_Segs,
    Seg_Total_Occurences = (Original_Size_Segs - Original_Rem_Size_Segs),
    Seg_Removed_Occurences = (Previous_Size_Segs - Current_Size_Segs),
    New_AC_Sample_Size = Current_Size_AC,
    AC_Total_Occurences = (Original_Size_AC - Original_Rem_Size_AC),
    AC_Removed_Occurences = (Previous_Size_AC - Current_Size_AC)
  )
  Filter_Table <- rbind(Filter_Table, Filter_Table_Add)
  return(Filter_Table)
}


Get_TBS_Table_Distances <- function(Airfield){
  if (Airfield == "EHAM"){
    return(c(3, 4, 5, 6, 7, 8))
  }
  else if (Airfield == "CYYZ"){
    return(c(3, 3.5, 4, 5, 6, 7, 8))
  }
  else if (Airfield == "EGLL"){
    return(c(3, 4, 5, 6, 7, 8))
  }
  else {
    return(c(3, 4, 5, 6, 7, 8))
  }
}

# This function converts degrees into radians.

radians <- function(degrees){
  radians <- degrees * pi / 180
  return(radians)
}

# This function converts radians into degrees.

degrees <- function(radians){
  degrees <- radians * 180 / pi
  return(degrees)
}

# This function calculates the difference between two angles in degrees.

angle_diff <- function(angle_a, angle_b) {
  angle_diff <- atan2(sin(radians(angle_a) - radians(angle_b)), cos(radians(angle_a) - radians(angle_b)))
  return(degrees(angle_diff))
}

# This function returns the headwind component along the runway heading
# Runway_Heading is the runway heading in degrees, relating to the heading flown by aircraft as they land
# Wind_Heading is the wind heading from the database in degrees and is in direction "from".
# Wind_Speed is in kts

calculate_headwind_component <- function(runway_hdg, wind_hdg, wind_spd){
  
  runway_hdg <- as.numeric(runway_hdg)
  wind_hdg <- as.numeric(wind_hdg)
  wind_spd <- as.numeric(wind_spd)
  head_diff <- angle_diff(runway_hdg, wind_hdg)
  
  headwind_component <- wind_spd * cos(radians(head_diff)) 
  return(headwind_component)
}

PlotAgainstReferenceFTA <- function(Data, Reference, RefDists, PlotVar, SepDist, FollowerWTC, Colour, Unit, RefWinds){
  
  if (RefWinds == "SHW"){
    Data <- filter(Data, Surface_Headwind_Group == "(-5,5]")
    Addon <- " (Ref Winds Only)"}
  
  # String for Title
  if (Unit == "IAS"){
    Unit <- "kts"
    PlotTitle <- paste0("Follower Mean IAS (", FollowerWTC, " - ", SepDist, "NM)")
    names(Reference)[ncol(Reference)] <- "IAS"
    Var <- "IAS"
    SubTitle <- paste0("Average Mode S IAS across ", SepDist, "NM Against Reference IAS (")
    Bound_Low <- 80
    Bound_High <- 200
  }
  
  if (Unit == "Time"){
    Unit <- "s"
    PlotTitle <- paste0("Flying Time (", FollowerWTC, " - ", SepDist, "NM)")
    names(Reference)[ncol(Reference)] <- "Time"
    Var <- "Time"
    SubTitle <- paste0("Average Flying Time across ", SepDist, "NM Against Reference Time (")
    Bound_Low <- 40
    Bound_High <- 200
  }
  
  if (exists("Addon")){PlotTitle <- paste0(PlotTitle, Addon)}
  names(RefDists)[ncol(RefDists)] <- "Distance"
  
  # Get variable names.
  Dist_Var <- "Separation_Distance"
  Foll_Var <- "Wake_Cat"
  
  # Filter Data for WTCs of interest
  Data <- filter(Data, !!sym(Dist_Var) == SepDist & !!sym(Foll_Var) == FollowerWTC)
  
  if (ncol(Reference) == 4){
    Reference <- select(Reference, -Runway) %>% unique()
    RefDists <- select(Reference, -Runway) %>% unique()
  }
  Reference <- left_join(Reference, RefDists, by = c("Leader_WTC", "Follower_WTC"))
  RefVar <- as.numeric(filter(Reference, Distance == SepDist & Follower_WTC == FollowerWTC) %>% select(!!sym(Var)))[1]
  SubTitle <- paste0(SubTitle, RefVar, Unit, ")") 
  String <- paste0(PlotTitle)
  
  # Initialise Histogram plot
  Plot <- ggplot(Data) + geom_histogram(mapping = aes(x = !!sym(PlotVar), y = ..density..), binwidth = 1, fill = Colour) + geom_vline(xintercept = RefVar) + 
    labs(x = paste0(Var, "(", Unit, ")"), y = "Density", title = PlotTitle, subtitle = SubTitle) + xlim(Bound_Low, Bound_High)
  
  return(Plot)
  
}


