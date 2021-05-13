
# Initial Config #########################################################################################

# --------------------------------------------------------------------------------- #
# Imports
# --------------------------------------------------------------------------------- #

library(dplyr)
library(RODBC)
library(ggplot2)
library(gridExtra)

# --------------------------------------------------------------------------------- #
# Functions
# --------------------------------------------------------------------------------- #

Get_Wake_Cats <- function(Wake_Scheme){
  Wakes <- c("")
  if (Wake_Scheme == "UK6Cat"){Wakes <- c("SUPER", "HEAVY", "UPPER", "MEDIUM", "LIGHT", "SMALL")}
  if (Wake_Scheme == "ICAO4"){Wakes <- c("J", "H", "M", "L")}
  if (Wake_Scheme == "ICAO4 Long"){Wakes <- c("SUPER", "HEAVY", "MEDIUM", "LIGHT")}
  if (Wake_Scheme == "RECAT-EU"){Wakes <- LETTERS[1:6]}
  if (Wake_Scheme == "ICAO7"){Wakes <- LETTERS[1:7]}
  return(Wakes)
}

Get_PM_Groupings <- function(Grouping_Type){
  if (Grouping_Type == "Landing_Pair_ID"){return(c("Landing_Pair_ID"))}
  if (Grouping_Type == "Flight_Plan_ID"){return(c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID"))}
  if (Grouping_Type == "Callsign_Date_Time"){return("FP_Date", "Leader_Callsign", "Follower_Callsign", "Leader_Time_At_4DME", "Follower_Time_At_4DME")}
  return(c("Landing_Pair_ID")) # Default is Landing Pair ID
}

Calculate_PM_Time_Separation <- function(PM, Segs, Grouping_Type, Seg_Size, Time_Var, Sep_Dist_Var, Leader_Start_Var, Spacing_Type, Delivery_Distance, Under_Sep, All_Segs_Required){
  
  if (Spacing_Type == "Perfect"){
    return(Calculate_PM_Time_Separation_Perfect(PM, Segs, Grouping_Type, Seg_Size, Time_Var, Sep_Dist_Var, Delivery_Distance, Under_Sep, All_Segs_Required))}
  if (Spacing_Type == "Actual"){
    return(Calculate_PM_Time_Separation_Actual(PM, Segs, Grouping_Type, Seg_Size, Time_Var, Sep_Dist_Var, Leader_Start_Var, Delivery_Distance, Under_Sep, All_Segs_Required))}
  
}


Calculate_PM_Time_To_Fly <- function(PM, Segs, LorF, Groupings, Seg_Size, All_Segs_Required){
  
  # Paste LorF to Variable Names
  Speed_Var <- paste0(LorF, "_Average_GSPD")
  Time_Var <- paste0(LorF, "_Flying_Time")
  
  # Get the Distance Variables
  Distances <- select(PM, all_of(Groupings), Start_Distance, End_Distance)
  
  # Get the Distances Flown in the Segment Data
  Segs <- Segs %>% mutate(DME_Seg_End = DME_Seg + Seg_Size)
  
  # Get Variable Groupings for Arrange & Group By Statements
  Groupings1 <- append(Groupings, "ID")
  Groupings2 <- append(Groupings, "DME_Seg")
  
  # ASSUMES ARRANGED IN ORDER OF ID VARIABLES AND DME SEG ASCENDING!!
  Segs <- Segs %>% group_by_at(vars(one_of(Groupings))) %>% mutate(ID = row_number()) %>% ungroup()
  Segs1 <- Segs %>% mutate(ID = ID - 1) %>% select(all_of(Groupings1), DME_Seg_Next = DME_Seg)
  Segs <- left_join(Segs, Segs1, by = setNames(Groupings1, Groupings1)) %>% mutate(DME_Diff = ifelse(!is.na(DME_Seg_Next), DME_Seg_Next - DME_Seg, Seg_Size)) %>%
    mutate(DME_Seg_Next = ifelse(is.na(DME_Seg_Next), DME_Seg + Seg_Size, DME_Seg_Next))
  
  # Get the Time Taken to fly between successive segments
  Segs <- mutate(Segs, Flying_Time = (DME_Diff / !!sym(Speed_Var)) * 3600)
  
  # Join on Distances to Segments
  Segs <- left_join(Segs, Distances, by = setNames(Groupings, Groupings))
  
  # Filter for Segments between Start and End Distances
  Segs <- filter(Segs, DME_Seg < Start_Distance & DME_Seg >= floor(End_Distance))
  
  # Generate Flags for the Required Start and End Segments (These are required for calculations)
  if (!All_Segs_Required){
    Segs <- Segs %>% 
      mutate(End_Seg_Flag = ifelse(DME_Seg_Next > End_Distance & DME_Seg <= End_Distance, 1, 0),
             Start_Seg_Flag = ifelse(DME_Seg_Next >= Start_Distance & DME_Seg < Start_Distance, 1, 0))
  } else {
    Segs <- Segs %>% 
      mutate(End_Seg_Flag = ifelse(DME_Seg_End > End_Distance & DME_Seg <= End_Distance, 1, 0),
             Start_Seg_Flag = ifelse(DME_Seg_End >= Start_Distance & DME_Seg < Start_Distance, 1, 0))
  }
  
  # Generate Flag for DME Difference not equal to the Segment Size (Shows missing segment(s))
  Segs <- Segs %>%
    mutate(Seg_Size_Flag = ifelse(DME_Diff != Seg_Size & Start_Seg_Flag == 0, 1, 0))
  
  # Sum up the numbers of above flags
  Summary <- Segs %>% group_by_at(vars(one_of(Groupings))) %>% 
    summarise(End_Seg_Flags = sum(End_Seg_Flag, na.rm = T),
              Start_Seg_Flags = sum(Start_Seg_Flag, na.rm = T),
              Seg_Size_Flags = sum(Seg_Size_Flag, na.rm = T))
  
  # Join on the flag counts to the segment data
  Segs <- left_join(Segs, Summary, by = setNames(Groupings, Groupings))
  
  # REQUIRED: Remove the Pair if either the First or Final Seg is not available
  Segs <- filter(Segs, End_Seg_Flags == 1 & Start_Seg_Flags == 1)
  
  # If All_Segs_Required, then remove pair if any intermediate segs are missing
  if (All_Segs_Required){Segs <- Segs %>% filter(Seg_Size_Flags == 0)}
  
  # Recalculate Time for the First/Final Segments As Required
  Segs <- Segs %>% mutate(Flying_Time = ifelse(Start_Seg_Flag == 1, ((Start_Distance - DME_Seg) / DME_Diff) * Flying_Time, Flying_Time))
  Segs <- Segs %>% mutate(Flying_Time = ifelse(End_Seg_Flag == 1, (DME_Diff - (DME_Seg - End_Distance)) / DME_Diff * Flying_Time, Flying_Time))
  
  # Sum the Flying Times 
  Times <- Segs %>% group_by_at(vars(one_of(Groupings))) %>% summarise(!!sym(Time_Var) := sum(Flying_Time, na.rm=F)) %>% ungroup()
  
  # Join onto PM Data
  PM <- left_join(PM, Times, by = setNames(Groupings, Groupings))
  
  return(PM)
  
}

Calculate_PM_Time_Separation_Perfect <- function(PM, Segs, Grouping_Type, Seg_Size, Time_Var, Sep_Dist_Var, Delivery_Distance, Under_Sep, All_Segs_Required){
  
  # Get Grouping Variables based on Grouping Type
  Groupings <- Get_PM_Groupings(Grouping_Type)
  
  # Filter for Segments at or below the Integer of Separation Distance
  PM <- PM %>% mutate(Start_Distance = !!sym(Sep_Dist_Var) + Delivery_Distance - Under_Sep, 
                      End_Distance = Delivery_Distance)
  
  # Get the Follower Flying Time
  PM <- PM %>%
    Calculate_PM_Time_To_Fly(Segs, LorF = "Follower", Groupings, Seg_Size, All_Segs_Required) %>%
    rename(!!sym(Time_Var) := Follower_Flying_Time) %>%
    select(-c("Start_Distance", "End_Distance"))
  
  return(PM)
  
}

Calculate_PM_Time_Separation_Actual <- function(PM, Segs, Grouping_Type, Seg_Size, Time_Var, Sep_Dist_Var, Leader_Start_Var, Delivery_Distance, Under_Sep, All_Segs_Required){
  
  # Get Grouping Variables based on Grouping Type
  Groupings <- Get_PM_Groupings(Grouping_Type)
  
  # Filter for Segments at or below the Integer of Separation Distance (Follower)
  PM <- PM %>% mutate(Start_Distance = !!sym(Sep_Dist_Var) + !!sym(Leader_Start_Var) - Under_Sep, 
                      End_Distance = Delivery_Distance)
  
  # Get the Follower Flying Time
  PM <- PM %>%
    Calculate_PM_Time_To_Fly(Segs, LorF = "Follower", Groupings, Seg_Size, All_Segs_Required) %>%
    select(-c("Start_Distance", "End_Distance"))
  
  # Filter for Segments at or below the Integer of Separation Distance (Leader)
  PM <- PM %>% mutate(Start_Distance = !!sym(Leader_Start_Var), 
                      End_Distance = Delivery_Distance)
  
  # Get the Follower Flying Time
  PM <- PM %>%
    Calculate_PM_Time_To_Fly(Segs, LorF = "Leader", Groupings, Seg_Size, All_Segs_Required) %>%
    select(-c("Start_Distance", "End_Distance"))
  
  # Calculate the Actual Separation Time
  PM <- PM %>%
    mutate(!!sym(Time_Var) := Follower_Flying_Time - Leader_Flying_Time) %>%
    select(-c("Leader_Flying_Time", "Follower_Flying_Time"))
  
  return(PM)
  
}






