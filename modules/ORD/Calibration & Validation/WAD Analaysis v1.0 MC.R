# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  WAD Validation Analysis                               #
#                |                                                        #
# Version No.    |  0                                                     #
#                |                                                        #
# Date Modified  |  20/04/2021                                            #
#                |                                                        #
# Author(s)      |  George Clark                                          #
#                |                                                        #
# Project        |  IA Related Projects                                   #
#                |                                                        #
# Purpose        |  Assess performance requirements of adaptation data    #
#                |                                                        #
# ----------------------------------------------------------------------- #

library(RODBC)
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

# ----------------------------------------------------------------------- #
# Functions
# ----------------------------------------------------------------------- #

Order_Radar <- function(Radar){
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Track_Time),]
  return(Radar)
}

EvalParse <- function(Text){
  return(eval(str2expression(Text)))
}

Build_Summary_Table_Code <- function(WAD_Data, Metrics, Include_Mean){
  Base <- paste0("summarise(Envelope = n(), ")
  if (Include_Mean){Base <- paste0(Base, "`Mean WAD` = round(mean(WAD_Compression, na.rm = T), 2), ")}
  Base <- paste0(Base, "`Mean WAD Error` = round(mean(WAD_Compression_Error, na.rm = T), 2), ")
  for (i in 1: length(Metrics)){
    Met <- Metrics[i]
    Count_Text <- paste0("`Count >", Met, "NM` = sum(WAD_Compression_Error_", gsub(".", "", as.character(Met), fixed=T), ", na.rm = T)")
    if (i == length(Metrics)){Base <- paste0(Base, Count_Text, ")")}
    if (i != length(Metrics)){Base <- paste0(Base, Count_Text, ", ")}
  }
  Base <- paste0(Base, " %>% ungroup() %>% mutate(")
  for (i in 1: length(Metrics)){
    Met <- Metrics[i]
    Rate_Text <- paste0("`Rate >", Met, "NM` = toupper(formatC(`Count >", Met, "NM` / Envelope, format = \"e\", digits = 2))")
    if (i == length(Metrics)){Base <- paste0(Base, Rate_Text, ")")}
    if (i != length(Metrics)){Base <- paste0(Base, Rate_Text, ", ")}
  }
  #WAD_Data <- EvalParse(Base)
  return(Base)
}

Output_Generic_WAD_Performance <- function(WAD_Data_Original, WAD_Data, Groupings, Metrics){
  
  Group_Text <- ""
  EvalString <- "WAD_Data %>% "
  if (!is.na(Groupings)){
    for (i in 1:length(Groupings)){
      if (i == length(Groupings)){Group_Text <- paste0(Group_Text, Groupings[i])} else {
        Group_Text <- paste0(Group_Text, Groupings[i], ", ")
      }
    }
    WAD_Data <- WAD_Data %>% group_by_at(vars(one_of(Groupings)))
    WAD_Data_Original <- WAD_Data_Original %>% group_by_at(vars(one_of(Groupings)))
    EvalString <- paste0(EvalString, "group_by(", Group_Text, ") %>% ")
  }
  
  # Get the Total Counts
  WAD_Data_Total_Counts <- WAD_Data_Original %>%
    summarise(N = n()) %>% ungroup()
  
  # Get the rest of the stats 
  WAD_Summary <- EvalParse(paste0(EvalString, Build_Summary_Table_Code(WAD_Data, Metrics, Include_Mean = T)))
  
  if (!is.na(Groupings)){WAD_Summary <- left_join(WAD_Data_Total_Counts, WAD_Summary, by = setNames(Groupings, Groupings))}
  if (is.na(Groupings)){WAD_Summary <- cbind(WAD_Data_Total_Counts, WAD_Summary)}
  
  # Arrange Descending by Total Count
  WAD_Summary <- arrange(WAD_Summary, desc(N))
  
  return(WAD_Summary)
  
}

Output_Filtered_WAD_Performance <- function(WAD_Data_Original, WAD_Data, Groupings, Values, Minimum, Metrics){
  
  # Get Full Summary
  Summary <- Output_Generic_WAD_Performance(WAD_Data_Original, WAD_Data, Groupings, Metrics)
  
  # Filter Based on Grouping Values
  if (is.na(Groupings)){return(Summary)}
  
  # Get the number of groupings
  Group_Length <- length(Groupings)
  
  if (Group_Length == length(Values)){
    for (i in 1:Group_Length){
      if (!is.na(Values[i])){Summary <- filter(Summary, !!sym(Groupings[i]) == Values[i]) %>% select(-!!sym(Groupings[i]))}
    }
    if (!is.na(Minimum)){Summary <- filter(Summary, !is.na(Envelope) & Envelope >= Minimum)}
    
    return(Summary)
  } else {message("ERROR: NUMBER OF GROUPING VARIABLES DOES NOT MATCH NUMBER OF SEARCH VALUES.")}
}

# Distances should include a Flight Plan ID, a Start Distance and End Distance
# This can be used for PM and ORD. Outputs Speed and WE Trapezium average
# across a distance window between start and end distance of Distances.
Get_Average_Observed_Mode_S_Parameters <- function(LPR, Radar, Prefix, LorF, TimeorRange, Start_Var, End_Var){
  
  # Set the Maximum Delta between the Start Distance and the max(RTT) (NM)
  Max_Delta <- 2
  
  # Get Variable Names
  FPID <- paste0(LorF, "_Flight_Plan_ID")
  
  # Get Relevant Pair Data
  Pair_Data <- select(LPR, !!sym(FPID), !!sym(Start_Var), !!sym(End_Var)) %>% rename("Flight_Plan_ID" := !!sym(FPID))
  
  # Join on the distances by Flight Plan ID
  Radar <- left_join(Radar, Pair_Data, by = c("Flight_Plan_ID"))
  
  # Filter for RTT within the Distance Bounds
  if (TimeorRange == "Range"){Radar <- rename(Radar, "End_Distance" := !!sym(End_Var), "Start_Distance" := !!sym(Start_Var))
  Radar <- filter(Radar, Range_To_Threshold >= End_Distance & Range_To_Threshold <= Start_Distance)}
  
  # Filter for Track_Time within the Time Bounds
  if (TimeorRange == "Time"){Radar <- rename(Radar, "End_Time" := !!sym(End_Var), "Start_Time" := !!sym(Start_Var))
  Radar <- filter(Radar, Track_Time <= End_Time & Track_Time >= Start_Time)}
  
  # Filter to remove NA Wind Effect/IAS Values
  Radar <- filter(Radar, !is.na(Mode_S_IAS) & !is.na(Wind_Effect_IAS))
  
  # Order by Flight Plan ID & Track Time
  Radar <- Order_Radar(Radar)
  
  # Get a Sequence Number
  Radar <- group_by(Radar, Flight_Plan_ID) %>% mutate(Sequence_Number = row_number()) %>% ungroup()
  
  # Take Required Fields from Radar
  Radar2 <- select(Radar, Flight_Plan_ID, Sequence_Number, Track_Time, Mode_S_IAS, Wind_Effect_IAS)
  
  # Change Sequence number to next number. Change names of parameters.
  Radar2 <- mutate(Radar2, Sequence_Number = Sequence_Number + 1) %>%
    rename(Previous_Track_Time = Track_Time, Previous_Mode_S_IAS = Mode_S_IAS, Previous_Wind_Effect_IAS = Wind_Effect_IAS)
  
  # Join on the Previous Parameters
  Radar <- left_join(Radar, Radar2, by = c("Flight_Plan_ID", "Sequence_Number"))
  
  # Remove Radar2
  rm(Radar2)
  
  # Get the Delta beween Track_Time and Previous_Track_Time
  Radar <- mutate(Radar, Track_Time_Delta = Track_Time - Previous_Track_Time)
  
  # Get each Observation's Contribution to the Trapezium sum: IAS
  Radar <- mutate(Radar, Observed_Mean_IAS = Track_Time_Delta * (Mode_S_IAS + Previous_Mode_S_IAS) / 2)
  
  # Get each Observation's Contribution to the Trapezium sum: Wind Effect
  Radar <- mutate(Radar, Observed_Mean_Wind_Effect = Track_Time_Delta * (Wind_Effect_IAS + Previous_Wind_Effect_IAS) / 2)
  
  # Sum Track Time Delta, Observed Mean IAS/Wind Effect by Flight Plan ID
  Radar <- group_by(Radar, Flight_Plan_ID) %>% summarise(Total_Track_Time_Delta = sum(Track_Time_Delta, na.rm=T),
                                                         Observed_Mean_IAS = sum(Observed_Mean_IAS, na.rm=T),
                                                         Observed_Mean_Wind_Effect = sum(Observed_Mean_Wind_Effect, na.rm=T)) %>% ungroup()
  
  # Divide the Observed sums by the Track Time delta to get the Trapezium rule average
  Radar <- mutate(Radar,
                  Observed_Mean_IAS = Observed_Mean_IAS / Total_Track_Time_Delta,
                  Observed_Mean_Wind_Effect = Observed_Mean_Wind_Effect / Total_Track_Time_Delta) %>%
    select(-Total_Track_Time_Delta)
  
  # Get Variable Names
  IAS_Var <- paste0("Observed_", LorF, "_", Prefix, "_IAS")
  WE_Var <- paste0("Observed_", LorF, "_", Prefix, "_Wind_Effect")
  
  # Rename Appropriately
  Radar <- rename(Radar, 
                  !!sym(IAS_Var) := "Observed_Mean_IAS",
                  !!sym(WE_Var) := "Observed_Mean_Wind_Effect")
  
  # Join on to Landing Pair Reference
  LPR <- left_join(LPR, Radar, by = setNames("Flight_Plan_ID", FPID))
  
  # Return the Observed parameters.
  return(LPR)
  
}

# ----------------------------------------------------------------------- #
# Configuration
# ----------------------------------------------------------------------- #
# TODO: Set Out_Dir
# ----------------------------------------------------------------------- #

# Database Name (Assuming Maverick)
database_name <- "NavCan_TBS_V3"

# SQL Server database connection
con <- RODBC::odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.23", database_name, "ruser", "Th!nkruser"
))

### 28/04: MC ADD - Output Directory Config
ver <- "1.0"

user <- "Michael Cowham"

base_dir <- file.path("C:", "Users", user, "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "23 Data Analysis", "Outputs", "WAD")
out_dir <- file.path(base_dir, ver)
plot_dir <- file.path(out_dir, "plots")

if (!dir.exists(out_dir)) dir.create(out_dir)
if (!dir.exists(plot_dir)) dir.create(plot_dir)

plot_max <- 50

### END 

# ----------------------------------------------------------------------- #
# Local Adaptation
# ----------------------------------------------------------------------- #

# Metrics to Display in Table Outputs 
Metrics <- c(0.1, 0.5, 1)

# Do we want to adjust Observed Follower WEs?
Adjust_Follower_WEs <- T

# Minimum Legacy Category Separation Distance (Assumed MRS)
Min_Legacy_Separation <- 3

# Do Filters
Apply_Observed_IAS_Filter <- F
Apply_Predicted_IAS_Filter <- T
Apply_Separation_Accuracy_Filter <- F
Apply_Leader_Establish_Filter <- T

# Speed Filters
Min_Observed_Leader_IAS <- 80
Max_Observed_Leader_IAS <- 300
Min_Observed_Follower_IAS <- 80
Max_Observed_Follower_IAS <- 300
Min_WAD_Leader_IAS <- 80
Max_WAD_Leader_IAS <- 250
Min_WAD_Follower_IAS <- 80
Max_WAD_Follower_IAS <- 250

# Other Values
Min_Leader_CC_RTT <- 8

# WAD IAS Adjustment.  Added to the observed follower groundspeed.

wad_follower_ias_adjustment <- -2

# ----------------------------------------------------------------------- #
# Data Loading
# ----------------------------------------------------------------------- #
# TODO: Enable Local File Loading 
# ----------------------------------------------------------------------- #

# Load the WAD Validation View
WAD_VV <- sqlQuery(con, sprintf("SELECT * FROM vw_WAD_Validation_View"), stringsAsFactors = F)

# Load the Recat/Legacy Wake Categories
Recat_Wake_Dist <- sqlQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Dist", stringsAsFactors = F) %>%
  mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852)
Legacy_Wake_Dist <- sqlQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence", stringsAsFactors = F) %>%
  mutate(WT_Separation_Distance = WT_Separation_Distance / 1852)

# Load the AC to Wake Mappings
Legacy_ACtoWake <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy", stringsAsFactors = F) %>% unique()

# Load the Radar Data for Follower WE adjustment if necessary 
if (Adjust_Follower_WEs){
  Radar_Query <- "SELECT 
  RTPD.Flight_Plan_ID,
  Track_Time,
  Range_To_Threshold,
  ILS_Locus_RTT,
  Range_To_ILS,
  Path_Leg_Type,
  Wind_Effect_IAS,
  Mode_S_IAS
  FROM vw_Radar_Track_Point_Derived RTPD
  LEFT JOIN tbl_Flight_Plan FP
  ON RTPD.Flight_Plan_ID = FP.Flight_Plan_ID
  LEFT JOIN tbl_Flight_Plan_Derived FPD
  ON RTPD.Flight_Plan_ID = FPD.Flight_Plan_ID
  LEFT JOIN tbl_Runway R1
  ON R1.Runway_Name = RTPD.Mode_S_Wind_Localiser_Capture
  LEFT JOIN tbl_Runway R2
  ON R2.Runway_Name = FP.Landing_Runway
  WHERE FP.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
  OR FPD.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
  OR R1.Runway_Group = R2.Runway_Group
  OR RTPD.Mode_S_Wind_Localiser_Capture IS NULL
  ORDER BY Flight_Plan_ID, Track_Time"
  
  # Get Radar Data
  Radar <- sqlQuery(con, Radar_Query, stringsAsFactors = F)
}

# ----------------------------------------------------------------------- #
# Extra Fields
# ----------------------------------------------------------------------- #
# TODO: Generalise Surface Wind/HW Bands
# ----------------------------------------------------------------------- #

# Initialise copy of WAD_VV
WAD_Data <- WAD_VV

# Join on Legacy Aircraft Type to Wake to get Legacy Wake Categories
WAD_Data <- left_join(WAD_Data, Legacy_ACtoWake, by = c("Leader_Aircraft_Type" = "Aircraft_Type")) %>% rename(Leader_Legacy_Wake_Cat = Wake)
WAD_Data <- left_join(WAD_Data, Legacy_ACtoWake, by = c("Follower_Aircraft_Type" = "Aircraft_Type")) %>% rename(Follower_Legacy_Wake_Cat = Wake)

# Adjustment for SWF Aircraft Type (Remove L/M Type)
WAD_Data <- mutate(WAD_Data,
                   Leader_Legacy_Wake_Cat = ifelse(Leader_Aircraft_Type == "SW4", "M", Leader_Legacy_Wake_Cat))

# Join on Legacy & RECAT Wake Separation Distances
WAD_Data <- WAD_Data %>%
  left_join(Recat_Wake_Dist, by = c("Leader_RECAT" = "Leader_WTC", "Follower_RECAT" = "Follower_WTC")) %>%
    rename(Reference_Recat_Wake_Separation_Distance = Reference_Wake_Separation_Distance) %>%
  left_join(Legacy_Wake_Dist, by = c("Leader_Legacy_Wake_Cat" = "Leader_WVI", "Follower_Legacy_Wake_Cat" = "Follower_WVI")) %>%
    rename(Reference_Legacy_Wake_Separation_Distance = WT_Separation_Distance)

# Add on more Pair fields
WAD_Data <- mutate(WAD_Data,
                   LF_Pair_Recat = paste0(Leader_RECAT, "-", Follower_RECAT),
                   LF_Pair_Legacy = paste0(Leader_Legacy_Wake_Cat, "-", Follower_Legacy_Wake_Cat),
                   Is_Legacy_Wake = ifelse(is.na(Reference_Legacy_Wake_Separation_Distance), 0, 1),
                   Is_RECAT_Wake = ifelse(is.na(Reference_Recat_Wake_Separation_Distance), 0, 1),
                   Is_In_Trail = ifelse(Landing_Pair_Type != "Not_In_Trail", 1, 0))

# Cap the Legacy Wake Distance by Minimum Radar (Assume 3NM)
WAD_Data <- mutate(WAD_Data, Reference_Legacy_Separation_Distance = 
                     ifelse(is.na(Reference_Legacy_Wake_Separation_Distance), Min_Legacy_Separation, Reference_Legacy_Wake_Separation_Distance))


# ----------------------------------------------------------------------- #
# Field Recalculation
# ----------------------------------------------------------------------- #

# Firstly, recalculate the WAD Forecast Compression (Keep Intermediary variables for now)
WAD_Data <- WAD_Data %>%
  mutate(WAD_Mean_Leader_GSPD = WAD_Mean_Leader_IAS + Forecast_Mean_Leader_Wind_Effect,
         WAD_Mean_Follower_GSPD = WAD_Mean_Follower_IAS + Forecast_Mean_Follower_Wind_Effect,
         Leader_Distance_Flown = Leader_CC_RTT - Leader_FAF_RTT,
         WAD_Compression = (Leader_Distance_Flown / WAD_Mean_Leader_GSPD) * (WAD_Mean_Follower_GSPD - WAD_Mean_Leader_GSPD))

# Adjust WAD Compression to be no less than 0
WAD_Data <- mutate(WAD_Data, WAD_Compression = ifelse(WAD_Compression > 0, WAD_Compression, 0))

if (Adjust_Follower_WEs){
  
  # Set 
  data1o <- WAD_Data
  Radaro <- Radar
    
    # Adaptation Data
    Allowed_Path_Legs <- c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept")
    Max_Range_To_ILS <- 4
    FAF_Distance_Val <- 4.5 #(Should be matched..)
    Sep_Buffer <- 1
    Max_Allowable_Inside_Sep <- 2
    
    # data1 reversal (testing)
    data1 <- data1o
    Radar <- Radaro
    
    # Data Field removal
    data1 <- select(data1, -c("Observed_Mean_Follower_IAS", "Observed_Mean_Follower_Wind_Effect"))
    
    # Change Range to Threshold value based on intercept ILS criteria
    Radar <- mutate(Radar,
                    ILS_Intercept_Flag = ifelse(is.na(Range_To_Threshold) & Path_Leg_Type %in% Allowed_Path_Legs & Range_To_ILS <= Max_Range_To_ILS, 1, 0),
                    ILS_Intercept_Flag = ifelse(is.na(ILS_Intercept_Flag), 0, ILS_Intercept_Flag),
                    Range_To_Threshold = ifelse(ILS_Intercept_Flag == 1, ILS_Locus_RTT, Range_To_Threshold))
    
    # Get the Forecast Compression Start/End Distances for the follower aircraft. (Assume 4.5NM LST)
    data1 <- mutate(data1, 
                    WAD_Separation_Distance = Currently_Used_WAD_Separation_Distance, ## ADDED TO FIX BUG
                    Follower_Forecast_Start_Distance = WAD_Compression + WAD_Separation_Distance + Sep_Buffer + (Leader_CC_RTT - Leader_FAF_RTT),
                    Follower_Forecast_End_Distance = WAD_Separation_Distance + Sep_Buffer) ## CHANGED FROM WRD_ to WAD_
    
    # Get the max RTTs
    data1_allowed <- Radar %>% group_by(Flight_Plan_ID) %>%
      filter(!is.na(Range_To_Threshold)) %>%
      mutate(ID = row_number()) %>% ungroup() %>%
      arrange(Flight_Plan_ID, desc(Range_To_Threshold)) %>%
      filter(ID == 1) %>%
      select(Flight_Plan_ID, Max_RTT_Follower = Range_To_Threshold, Follower_Max_ILS_Intercept_Flag = ILS_Intercept_Flag)
    
    data_it <- filter(data1, Landing_Pair_Type != "Not_In_Trail")
    data_nit <- filter(data1, Landing_Pair_Type == "Not_In_Trail")
    
    foll_start_dist_it <- select(data_it, Follower_Flight_Plan_ID, Follower_Forecast_Start_Distance)
    foll_start_dist_nit <- select(data_nit, Follower_Flight_Plan_ID, Follower_Forecast_Start_Distance)
    
    data1_sep1 <- Radar %>%
      left_join(foll_start_dist_it, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID")) %>%
      filter(!is.na(Range_To_Threshold)) %>%
      filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
      arrange(Flight_Plan_ID, Track_Time) %>%
      group_by(Flight_Plan_ID) %>%
      mutate(ID = row_number()) %>%
      ungroup() %>%
      filter(ID == 1) %>% 
      select(Flight_Plan_ID, Est_Start_RTT_Follower = Range_To_Threshold, Follower_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
    
    data_it <- left_join(data_it, data1_sep1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
    
    data1_sep2 <- Radar %>%
      left_join(foll_start_dist_nit, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID")) %>%
      filter(!is.na(Range_To_Threshold)) %>%
      filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
      arrange(Flight_Plan_ID, Track_Time) %>%
      group_by(Flight_Plan_ID) %>%
      mutate(ID = row_number()) %>%
      ungroup() %>%
      filter(ID == 1) %>% 
      select(Flight_Plan_ID, Est_Start_RTT_Follower = Range_To_Threshold, Follower_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
    
    data_nit <- left_join(data_nit, data1_sep2, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
    
    data1 <- rbind(data_it, data_nit)
    
    # Join on the Max RTTs
    data1 <- left_join(data1, data1_allowed, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
    
    data_it <- filter(data1, Landing_Pair_Type != "Not_In_Trail")
    data_nit <- filter(data1, Landing_Pair_Type == "Not_In_Trail")
    
    lead_start_dist_nit <- select(data_nit, Leader_Flight_Plan_ID, Follower_Forecast_Start_Distance)
    lead_start_dist_it <- select(data_it, Leader_Flight_Plan_ID, Follower_Forecast_Start_Distance)
    
    data1_sep1 <- Radar %>%
      left_join(lead_start_dist_it, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID")) %>%
      filter(!is.na(Range_To_Threshold)) %>%
      filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
      arrange(Flight_Plan_ID, Track_Time) %>%
      group_by(Flight_Plan_ID) %>%
      mutate(ID = row_number()) %>%
      ungroup() %>%
      filter(ID == 1) %>% 
      select(Flight_Plan_ID, Est_Start_RTT_Leader = Range_To_Threshold, Leader_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
    
    data_it <- left_join(data_it, data1_sep1, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
    
    data1_sep2 <- Radar %>%
      left_join(lead_start_dist_nit, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID")) %>%
      filter(!is.na(Range_To_Threshold)) %>%
      filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
      arrange(Flight_Plan_ID, Track_Time) %>%
      group_by(Flight_Plan_ID) %>%
      mutate(ID = row_number()) %>%
      ungroup() %>%
      filter(ID == 1) %>% 
      select(Flight_Plan_ID, Est_Start_RTT_Leader = Range_To_Threshold, Leader_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
    
    data_nit <- left_join(data_nit, data1_sep2, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
    
    data1 <- rbind(data_it, data_nit)
    
    # Change data1_allowed for use of Leader parameters
    data1_allowed <- rename(data1_allowed, 
                            Max_RTT_Leader = Max_RTT_Follower,
                            Leader_Max_ILS_Intercept_Flag = Follower_Max_ILS_Intercept_Flag)
    
    
    # Join on the Max RTTs (Leader!)
    data1 <- left_join(data1, data1_allowed, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
    
    # Follower Filter flag
    data1 <- mutate(data1, Invalid_Follower_Flag = ifelse(Max_RTT_Follower < (Follower_Forecast_Start_Distance - Max_Allowable_Inside_Sep), 1, 0)) %>%
      mutate(Invalid_Follower_Flag = ifelse(is.na(Invalid_Follower_Flag), 1, Invalid_Follower_Flag))
    
    # Filter for the Allowed/Not Allowed
    data1_fol <- filter(data1, Invalid_Follower_Flag == 0)
    data1_nofol <- filter(data1, Invalid_Follower_Flag == 1) %>% mutate(Observed_Mean_Follower_IAS = NA,
                                                                        Failed_Valid_Follower_Flag = 0)
    
    # Perform calculations for Allowed Pairs
    data1_fol <- Get_Average_Observed_Mode_S_Parameters(data1_fol, Radar, 
                                                        Prefix = "Rename_Me",
                                                        "Follower",
                                                        "Range", 
                                                        Start_Var = "Follower_Forecast_Start_Distance", 
                                                        End_Var = "Follower_Forecast_End_Distance") %>%
      rename(Observed_Mean_Follower_Wind_Effect = Observed_Follower_Rename_Me_Wind_Effect,
             Observed_Mean_Follower_IAS = Observed_Follower_Rename_Me_IAS)
    
    # Get the Failed Follower Attempts
    data1_folfailed <- filter(data1_fol, is.na(Observed_Mean_Follower_Wind_Effect)) %>% select(-Observed_Mean_Follower_Wind_Effect) %>%
      mutate(Invalid_Follower_Flag = 1,
             Failed_Valid_Follower_Flag = 1)
    
    # Remove these from the follower dataset - this is now complete
    data1_fol <- filter(data1_fol, !is.na(Observed_Mean_Follower_Wind_Effect)) %>% mutate(Failed_Valid_Follower_Flag = 0)
    
    # Bind on to the Nofol data
    data1_nofol <- rbind(data1_nofol, data1_folfailed)
    
    # Leader Filter flag
    data1_nofol <- mutate(data1_nofol, Invalid_Leader_Flag = ifelse(Max_RTT_Leader < (Follower_Forecast_Start_Distance - Max_Allowable_Inside_Sep), 1, 0)) %>%
      mutate(Invalid_Leader_Flag = ifelse(is.na(Invalid_Leader_Flag), 1, Invalid_Leader_Flag))
    
    # Filter for the Allowed/Not Allowed
    data1_nofol1 <- filter(data1_nofol, Invalid_Leader_Flag == 0) %>% select(-Invalid_Leader_Flag)
    data1_nofol2 <- filter(data1_nofol, Invalid_Leader_Flag == 1) %>% select(-Invalid_Leader_Flag) %>% mutate(Observed_Mean_Follower_Wind_Effect = NA)
    
    # Perform calculations for Allowed Pairs
    data1_nofol1 <- Get_Average_Observed_Mode_S_Parameters(data1_nofol1, Radar, 
                                                           Prefix = "Rename_Me",
                                                           "Leader",
                                                           "Range", 
                                                           Start_Var = "Follower_Forecast_Start_Distance", 
                                                           End_Var = "Follower_Forecast_End_Distance") %>%
      rename(Observed_Mean_Follower_Wind_Effect = Observed_Leader_Rename_Me_Wind_Effect) %>%
      select(-Observed_Leader_Rename_Me_IAS)
    
    # Bind together Leader data
    data1_nofol <- rbind(data1_nofol1, data1_nofol2)
    
    # Bind datasets together again
    data1 <- rbind(data1_fol, data1_nofol)
    
    data1 <- mutate(data1, Forecast_Mean_Follower_Wind_Effect_Error = Observed_Mean_Follower_Wind_Effect - Forecast_Mean_Follower_Wind_Effect)
    data1 <- arrange(data1, desc(Forecast_Mean_Follower_Wind_Effect_Error))
    
    data1 <- mutate(data1,
                    Not_Calculated_Flag = ifelse(is.na(Observed_Mean_Follower_Wind_Effect), 1, 0),
                    Follower_Standard_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 0 & Follower_Start_ILS_Intercept_Flag == 0, 1, 0),
                    Follower_Extended_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 0 & Follower_Start_ILS_Intercept_Flag == 1, 1, 0),
                    Leader_Standard_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 1 & Leader_Start_ILS_Intercept_Flag == 0, 1, 0),
                    Leader_Extended_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 1 & Leader_Start_ILS_Intercept_Flag == 1, 1, 0)
    )
    
    # How many will be removed? 
    print(paste0("Will Remove ", nrow(filter(data1, is.na(Forecast_Mean_Follower_Wind_Effect))), " Observations out of ", nrow(data1), "."))
    
    # Replace WAD_Data 
    WAD_Data <- data1
    
}

# Temporary IAS adjustment to set forecast IAS to actual procedural
WAD_Follower_IAS_Adjustment <- -2

# Recalculate the Observed WAD Compression
WAD_Data <- WAD_Data %>%
  mutate(Observed_Mean_Leader_GSPD = WAD_Mean_Leader_IAS + Observed_Mean_Leader_Wind_Effect,
         Observed_Mean_Follower_GSPD = WAD_Mean_Follower_IAS + Observed_Mean_Follower_Wind_Effect + WAD_Follower_IAS_Adjustment,
         Observed_Compression = (Leader_Distance_Flown / Observed_Mean_Leader_GSPD) * (Observed_Mean_Follower_GSPD - Observed_Mean_Leader_GSPD))

# Recalculate All Error Variables
WAD_Data <- WAD_Data %>%
  mutate(WAD_Compression_Error = Observed_Compression - WAD_Compression,
         WAD_Leader_IAS_Error = Observed_Mean_Leader_IAS - WAD_Mean_Leader_IAS,
         WAD_Follower_IAS_Error = Observed_Mean_Follower_IAS - WAD_Mean_Follower_IAS,
         Forecast_Mean_Leader_Wind_Effect_Error = Observed_Mean_Leader_Wind_Effect - Forecast_Mean_Leader_Wind_Effect,
         Forecast_Mean_Follower_Wind_Effect_Error = Observed_Mean_Follower_Wind_Effect - Forecast_Mean_Follower_Wind_Effect,
         Combined_Wind_Effect_Error = Forecast_Mean_Leader_Wind_Effect_Error - Forecast_Mean_Follower_Wind_Effect_Error)

# ----------------------------------------------------------------------- #
# Add Analysis Fields
# ----------------------------------------------------------------------- #

#  Get Observed/Forecast Leader/Follower Wind Effect/Error Groups (Currently only Observed Mean Leader WE)
WAD_Data <- WAD_Data %>%
  mutate(Observed_Mean_Leader_Wind_Effect_Group = cut(Observed_Mean_Leader_Wind_Effect, breaks = c(-Inf, -25, -20, -15, -10, -5, 0, 5, Inf)))

# Get rounded versions of WAD Compression and Compression error to match operational system
WAD_Data <- WAD_Data %>%
  mutate(WAD_Compression_Rounded = round(WAD_Compression, 1),
         WAD_Compression_Error_Rounded = round(WAD_Compression_Error, 1))

# ----------------------------------------------------------------------- #
# Apply Filters
# ----------------------------------------------------------------------- #

# Create copy of WAD Data 
WAD_Data_Original <- WAD_Data

# Initialise Elements of Filter Table
Filters <- c("Original")
New_Values <- c(nrow(WAD_Data))
Removed_Values <- c(NA)
Min_Values <- c(NA)
Max_Values <- c(NA)


# Remove excess observed IAS values if enabled
if (Apply_Observed_IAS_Filter){
  
  # Observed Leader IAS
  Original_Count <- nrow(WAD_Data)
  WAD_Data <- WAD_Data %>%
    filter(Observed_Mean_Leader_IAS > Min_Observed_Leader_IAS & Observed_Mean_Leader_IAS < Max_Observed_Leader_IAS) 
  New_Count <- nrow(WAD_Data)
  Removed_Count <- Original_Count - New_Count
  Filters <- append(Filters, "Excessive Observed Leader IAS")
  New_Values <- append(New_Values, New_Count)
  Removed_Values <- append(Removed_Values, Removed_Count)
  message(paste0("Removed ", Removed_Count, " Excessive Observed Leader IAS Observations out of ", Original_Count,  ", with ", New_Count, " remaining." ))
  Max_Values = append(Max_Values, Max_Observed_Leader_IAS)
  Min_Values = append(Min_Values, Min_Observed_Leader_IAS)
  
  # Observed Follower IAS
  Original_Count <- New_Count
  WAD_Data <- WAD_Data %>%
    filter(Observed_Mean_Follower_IAS > Min_Observed_Follower_IAS & Observed_Mean_Follower_IAS < Max_Observed_Follower_IAS)
  New_Count <- nrow(WAD_Data)
  Removed_Count <- Original_Count - New_Count
  Filters <- append(Filters, "Excessive Observed Follower IAS")
  New_Values <- append(New_Values, New_Count)
  Removed_Values <- append(Removed_Values, Removed_Count)
  message(paste0("Removed ", Removed_Count, " Excessive Observed Follower IAS Observations out of ", Original_Count,  ", with ", New_Count, " remaining." ))
  Max_Values = append(Max_Values, Max_Observed_Follower_IAS)
  Min_Values = append(Min_Values, Min_Observed_Follower_IAS)
}

# Remove excess Predicted IAS if enabled
if (Apply_Predicted_IAS_Filter){
  
  # Forecast Leader IAS
  Original_Count <- nrow(WAD_Data)
  WAD_Data <- WAD_Data %>%
    filter(WAD_Mean_Leader_IAS > Min_WAD_Leader_IAS & WAD_Mean_Leader_IAS < Max_WAD_Leader_IAS)
  New_Count <- nrow(WAD_Data)
  Removed_Count <- Original_Count - New_Count
  Filters <- append(Filters, "Excessive Predicted Leader IAS")
  New_Values <- append(New_Values, New_Count)
  Removed_Values <- append(Removed_Values, Removed_Count)
  message(paste0("Removed ", Removed_Count, " Excessive Predicted Leader IAS Observations out of ", Original_Count,  ", with ", New_Count, " remaining." ))
  Max_Values = append(Max_Values, Max_WAD_Leader_IAS)
  Min_Values = append(Min_Values, Min_WAD_Leader_IAS)
  
  # Forecast Follower IAS
  Original_Count <- nrow(WAD_Data)
  WAD_Data <- WAD_Data %>%
    filter(WAD_Mean_Follower_IAS > Min_WAD_Follower_IAS & WAD_Mean_Follower_IAS < Max_WAD_Follower_IAS)
  New_Count <- nrow(WAD_Data)
  Removed_Count <- Original_Count - New_Count
  Filters <- append(Filters, "Excessive Predicted Follower IAS")
  New_Values <- append(New_Values, New_Count)
  Removed_Values <- append(Removed_Values, Removed_Count)
  message(paste0("Removed ", Removed_Count, " Excessive Predicted Follower IAS Observations out of ", Original_Count,  ", with ", New_Count, " remaining." ))
  Max_Values = append(Max_Values, Max_WAD_Follower_IAS)
  Min_Values = append(Min_Values, Min_WAD_Follower_IAS)
}

# Remove far spaced pairs if enabled
if (Apply_Separation_Accuracy_Filter){
  Original_Count <- nrow(WAD_Data)
  WAD_Data <- WAD_Data %>%
    filter(Delivered_FAF_Separation_Accuracy <= Max_FAF_Separation_Accuracy)
  New_Count <- nrow(WAD_Data)
  Removed_Count <- Original_Count - New_Count
  Filters <- append(Filters, "Far Spaced Pairs")
  New_Values <- append(New_Values, New_Count)
  Removed_Values <- append(Removed_Values, Removed_Count)
  message(paste0("Removed ", Removed_Count, " Far Spaced Pair Observations out of ", Original_Count,  ", with ", New_Count, " remaining." ))
  Max_Values = append(Max_Values, Max_FAF_Separation_Accuracy)
  Min_Values = append(Min_Values, NA)
}

# Remove Pairs where the leader doesn't establish until after xNM
if (Apply_Leader_Establish_Filter){
  Original_Count <- nrow(WAD_Data)
  WAD_Data <- WAD_Data %>%
    filter(Leader_CC_RTT <= Min_Leader_CC_RTT)
  New_Count <- nrow(WAD_Data)
  Removed_Count <- Original_Count - New_Count
  Filters <- append(Filters, "Late Established Leaders")
  New_Values <- append(New_Values, New_Count)
  Removed_Values <- append(Removed_Values, Removed_Count)
  message(paste0("Removed ", Removed_Count, " Late Established Leaders Observations out of ", Original_Count,  ", with ", New_Count, " remaining." ))
  Max_Values = append(Max_Values, NA)
  Min_Values = append(Min_Values, Min_Leader_CC_RTT)
}

# Create Filter Table
Filter_Table <- data.frame(
  Filter = Filters,
  `Min Value`= Min_Values,
  `Max Value` = Max_Values,
  `New Sample Size` = New_Values,
  `Sample Removed` = Removed_Values
)

# Save Filter Table
# fwrite(Filter_Table, file.path(out_dir_wad, "WAD_Filter_Stats.csv"))

# ----------------------------------------------------------------------- #
# Output Performance
# ----------------------------------------------------------------------- #
### IMPORTANT: READ THE BELOW!!!!
## use Output_Filtered_WAD_Performance() for any tables with Counts/Rates
## Takes the following arguments:
# WAD_Data_Original - Leave as WAD_Data_Original (This is to get the total dataset size before filtering)
# WAD_Data - Leave as WAD_Data. This is the dataset being summarised
# Groupings - 
# - Leave as NA if you want General Performance across the entire dataset
# - Input a character vector e.g. c("Is_In_Trail", "Leader_Aircraft_Type") to group by these elements (if 1 element, keep in vector)
# - If you want to only include e.g. In Trail Pairs: Put the variable that defines this in grouping
# Values - 
# - Can be NA or character vector as before
# - MUST BE THE SAME LENGTH AS GROUPINGS!!!
# - Setting Values[i] = x will filter results for groupings[i] = x
# - With in-trail only example: Set Groupings = c("Is_In_Trail), Values = c(1) - This will filter for in-trail pairs and remove the grouping field
# - If we do not want to filter for a specific instance of a grouping variable groupings[j], set Values[j] = NA
# - For example, If we want to Look at In-Trail Pairs by Leader Wake Category - Groupings = c("Is_In_Trail", "Leader_RECAT"), Values = c(1, NA)
# - Currently does not support selecting a SET of discrete values. Easily can normal filter for this - intention is to choose one of two binary variables (in-trail/not-in-trail, RECAT Wake/non-wake etc)
# Minimum - 
# This is a single numeric filter that will remove observations below this number. Useful for Aircraft Type breakdowns for example.
# Metrics - 
# Currently MUST keep as Metrics. However...
# Is a numeric vector input
# This determines the error count/rate metrics to display.
# For example, NAV currently requires Rate of 0.1NM errors no more than 1/100
# We need only Metrics = c(0.1), but can easily extend to look at other milestones e.g. c(0, 0.1, 0.5, 1)
# Metrics is set at the top and can be updated accordingly.
# ----------------------------------------------------------------------- #

# Create variables to count metrics
for (i in 1:length(Metrics)){
  Met <- Metrics[i]
  WAD_Data <- WAD_Data %>%
    mutate(!!sym(paste0("WAD_Compression_Error_", gsub(".", "", as.character(Met), fixed=T))) := ifelse(WAD_Compression_Error_Rounded >= Met, 1, 0))
}

# ----------------------------------------------------------------------- #
### Preliminary Performance Tables

# General Overall Performance
General_Performance <- WAD_Data_Original %>%
  Output_Filtered_WAD_Performance(WAD_Data, 
                                  Groupings = NA, 
                                  Values = NA, 
                                  Minimum = NA, Metrics)

# In Trail Only General Performance
In_Trail_Performance <- WAD_Data_Original %>%
  Output_Filtered_WAD_Performance(WAD_Data, 
                                  Groupings = c("Is_In_Trail"), 
                                  Values = c(1),
                                  Minimum = 0, Metrics)

# Not In Trail Only General Performance
Not_In_Trail_Performance <- WAD_Data_Original %>%
  Output_Filtered_WAD_Performance(WAD_Data, 
                                  Groupings = c("Is_In_Trail"), 
                                  Values = c(0),
                                  Minimum = 0, Metrics)

# In Trail Aircraft Pair Performance
In_Trail_ACT_Pair_Performance <- WAD_Data_Original %>%
  Output_Filtered_WAD_Performance(WAD_Data, 
                                  Groupings = c("Is_In_Trail", "Leader_Aircraft_Type", "Follower_Aircraft_Type"),
                                  Values = c(1, NA, NA),
                                  Minimum = 0, Metrics)

# In Trail Runway Performance
In_Trail_Runway_Performance <- WAD_Data_Original %>%
  Output_Filtered_WAD_Performance(WAD_Data, 
                                  Groupings = c("Is_In_Trail", "Landing_Runway"),
                                  Values = c(1, NA),
                                  Minimum = 0, Metrics)


# ----------------------------------------------------------------------- #

WAD_Error_Histogram <- function(WAD_Data, Metrics){
  linetypes <- c("longdash", "solid")
  Plot <- ggplot() + 
    geom_histogram(data = WAD_Data,
                   mapping = aes(x = WAD_Compression_Error, y = ..density..),
                   binwidth = 0.05)
  for (i in 1:length(Metrics)){
    Line <- linetypes[i%%(length(linetypes)+1)]
    Met <- Metrics[i] - 0.05
    Plot <- Plot + geom_vline(xintercept = Met, linetype = Line) +
      geom_text(mapping = aes(label = paste0(Metrics[i], "NM"), x = Met, y = 1/i))
  }

  return(Plot)
}

print(WAD_Error_Histogram(WAD_Data, Metrics) + xlab("WAD Compression Error (NM)"))

ggplot()+
geom_histogram(data = WAD_Data,
               mapping = aes(x = WAD_Compression_Error, y = ..density..),
               binwidth = 0.05)

WAD_Data <- mutate(WAD_Data, Flags = paste0(Follower_Standard_Flag, "-", Follower_Extended_Flag))



ggplot()+
  geom_histogram(data = WAD_Data,
                 mapping = aes(x = Forecast_Mean_Follower_Wind_Effect_Error, y = ..density..),
                 binwidth = 1)

ggplot()+
  geom_histogram(data = WAD_Data,
                 mapping = aes(x = Forecast_Mean_Follower_Wind_Effect_Error, y = ..density..),
                 binwidth = 1)+
  facet_wrap(~Flags)

WAD_Data %>% group_by(Flags) %>% summarise(N = n(), Mean_Error = mean(Forecast_Mean_Follower_Wind_Effect_Error, na.rm = T), Median_Error = median(Forecast_Mean_Follower_Wind_Effect_Error, na.rm = T))

fwrite(WAD_Data, file.path(out_dir, "WAD_Data.csv"))

ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)



plot_ord_profile_we <- function(lp_id, ord_line, type, include_xy){
  
  # lp_id <- 161163
  # include_gspd <- T
  # ord_line <- WAD_Data[2,]
  # Get the ORD data line
  
  #ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)
  
  title_text <- paste0("Leader WE Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  
  if (type == "ord"){
    subtitle_text <- paste0("ORD Error = ", ord_line$ORD_Compression_Error_Rounded, "NM ", "Leader WE Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error), "kt")
  } else {
    subtitle_text <- paste0("ORD Error = ", ord_line$WAD_Compression_Error_Rounded, "NM ", "Leader WE Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error), "kt")
    
  }
  
  p1 <- tryCatch(plot_single_wind_effect(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, "L", ord_line$Leader_FAF_RTT, ord_line$Leader_CC_RTT, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })
  
  title_text <- paste0("Follower WE Profile for ", ord_line$Follower_Callsign, " ",ord_line$Follower_Aircraft_Type)
  subtitle_text <- paste0("Follower Wind Effect Error = ", sprintf("%.0fkt ", ord_line$Forecast_Mean_Follower_Wind_Effect_Error), "Flag=", ord_line$Follower_Standard_Flag, ord_line$Follower_Extended_Flag, ord_line$Leader_Standard_Flag, ord_line$Leader_Extended_Flag)
  
  p2 <- tryCatch(plot_single_wind_effect(ord_line$Follower_Flight_Plan_ID, ord_line$Landing_Pair_ID, "F", ord_line$Follower_Forecast_End_Distance, ord_line$Follower_Forecast_Start_Distance, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })
  
  if (include_xy == F){
    grid.arrange(p1, p2, nrow = 1)
  } else {
    
    title_text <- paste0("XY plot for ", ord_line$Leader_Callsign)
    p3 <- tryCatch(plot_single_xy(ord_line$Leader_Flight_Plan_ID, title_text),
                   error = function(e) {
                     plot.new()
                     text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                   })
    
    title_text <- paste0("XY plot for ", ord_line$Follower_Callsign)    
    p4 <- tryCatch(plot_single_xy(ord_line$Follower_Flight_Plan_ID, title_text),
                   error = function(e) {
                     plot.new()
                     text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                   })
    
    grid.arrange(p1, p2, p3, p4, nrow = 2)
    
  }
    
}

plot_single_wind_effect <- function(fp_id, lp_id, role, min, max, title_text, subtitle_text){
  
  #fp_id <- 28298
  #lp_id <- 161163
  #role <- "F"
  #title_text = "test"
  #subtitle_text = "test"
  
  #min <- 13.11
  #max <- 9.67
  
  leader_track <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))
  
  leader_track <- filter(leader_track, Path_Leg_Type %in% Allowed_Path_Legs)
  
  min_y = 10 * floor(min(c(leader_track$Wind_Effect_IAS,ord_profile$End_WE),  na.rm = T) / 10)
  max_y = 10 * floor(max(c(leader_track$Wind_Effect_IAS,ord_profile$End_WE),  na.rm = T) / 10) + 10
  
  min_x <- 0
  max_x <- ceiling(max(c(leader_track$ILS_Locus_RTT, max), na.rm = T))
  
  # Get the ORD Profile for leader and follower
  
  ord_profile <- sqlQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = '", role, "'"))
  
  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852,
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852,
                        Start_WE = Start_GS - Start_IAS,
                        End_WE = End_GS - End_IAS)
  
  p1 <- ggplot()+
    geom_point(data = leader_track, mapping = aes(x = ILS_Locus_RTT, y = Wind_Effect_IAS, color = Path_Leg_Type))+
    #geom_point(data = leader_track, mapping = aes(x = ILS_Locus_RTT, y = Wind_Effect_IAS, color = "Wind Effect"))+
    
    geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_WE, color = "ORD WE"))+
    xlim(c(min_x, max_x))+
    ylim(c(min_y, max_y))+
    labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Wind Effect (kt)")+
    scale_color_manual(name = "Legend",
                       breaks = c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept", "ORD WE"),
                       values = c("ILS_Leg" = "firebrick2", "Landing_Leg" = "firebrick2", "Intercept_Leg" = "deepskyblue4", "Extended_Intercept" = "deepskyblue4", "ORD WE" = "grey"))+
    geom_vline(xintercept = min, color = "grey")+
    geom_vline(xintercept = max, color = "grey")+
    theme_bw()+
    theme(legend.position = "bottom")
  

  
  return(p1)
  
}


plot_single_xy <- function(fp_id, title_text){

  #fp_id <- 20690
  #title_text = "test"
  #subtitle_text = "test"
  
  leader_track <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))
  
  leader_track <- filter(leader_track, Path_Leg_Type %in% Allowed_Path_Legs)
  
  p1 <- ggplot()+
    geom_point(data = leader_track, mapping = aes(x = X_Pos, y = Y_Pos, color = Path_Leg_Type))+
    labs(title = title_text, x = "X (NM)", y = "Y (NM)")+
    scale_color_manual(name = "Legend",
                       breaks = c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept"),
                       values = c("ILS_Leg" = "firebrick2", "Landing_Leg" = "firebrick2", "Intercept_Leg" = "deepskyblue4", "Extended_Intercept" = "deepskyblue4"))+
    theme_bw()+
    theme(legend.position = "bottom")
  
  return(p1)
  
}


WAD_Data <- arrange(WAD_Data, desc(WAD_Compression_Error))

for (i in 1:min(c(plot_max, nrow(WAD_Data)))){
  ord_line <- WAD_Data[i, ]
  plot_name <- paste0(i, "-",ord_line$Landing_Pair_ID, ".png")
  png(filename = file.path(plot_dir, plot_name), width = 900, height = 600)
  plot_ord_profile_we(ord_line$Landing_Pair_ID, ord_line, "wad")
  dev.off()
  
  
}

plot_ord_profile_we(161163, WAD_Data[2, ], "wad", T)
plot_ord_profile_we(149438, WAD_Data, "wad")

