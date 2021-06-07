# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  WAD Validation Analysis                               #
#                |                                                        #
# Version No.    |  1.0                                                   #
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

# library(RODBC)
library(tidyverse)
library(data.table)
library(ggplot2)
library(getPass)


# --------------------------------------------------------------------------- #
ModuleFolder <- "ORD"
ModuleSubfolder <- "Calibration & Validation"
# Script_out <- "Performance Analysis"
# OutputFolder <- paste(ModuleFolder, Script_out, version, sep = "/")
# --------------------------------------------------------------------------- #

#Set to 1 when in git structure

FileFlag <- c("global.R", "GlobalPlaceholder.txt")[1]
ResourcesFolder <- c("resources", "GlobalFunctionsPlaceholder")[1]
AlgoResourcesFolder <- c("algorithm_functions", "AlgoFunctionsPlaceholder")[1]
ModulesFolder <- c("modules", "ModulesPlaceholder")[1]

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  Base_Dir <- getwd()
  Global_Dir <- Base_Dir
  Script_Dir <- file.path(Base_Dir)
  while (!file.exists(file.path(Global_Dir, FileFlag))){
    Global_Dir <- file.path(Global_Dir, "..")
  }
} else {
  Global_Dir <- getwd()
  Script_Dir <- file.path(Global_Dir, ModulesFolder, ModuleFolder, ModuleSubfolder)
}

Global_Dir <- file.path(Global_Dir, ResourcesFolder)
Algo_Func_Dir <- file.path(Global_Dir, AlgoResourcesFolder)

# Global Functions, imports & parameters
source(file.path(Global_Dir, "imports.R"), local = T)
source(file.path(Global_Dir, "unit conversions.R"), local = T)
source(file.path(Global_Dir, "functions.R"), local = T)
source(file.path(Algo_Func_Dir, "ORD Functions.R"), local = T)

project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3", noblank = FALSE, forcemask = FALSE))

#IorO for "Ouputs" or "Inputs" directory, must be a string as used in directory
Base_Dir <- GetSaveDirectory(Project = project, Algorithm = ModuleFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

# ----------------------------------------------------------------------- #
# Functions
# ----------------------------------------------------------------------- #

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

rolling_join <- function(df1, df2, by_join1, by_join2, by_roll, roll_val){

  #roll_val <- NA
  #if (is.na(roll_val)){roll_val <- F}
  #by_join1 <- c("Leader_Aircraft_Type", "Follower_Aircraft_Type")
  #by_join2 <- c("Leader_Aircraft_Type", "Follower_Aircraft_Type")
  #by_roll <- c("Prediction_Time", "Prediction_Time")

  #df2 <- select(WAD_Data_Original, Leader_Aircraft_Type, Follower_Aircraft_Type, Prediction_Time)
  #df1 <- select(WAD_Data, Leader_Aircraft_Type, Follower_Aircraft_Type, Prediction_Time, WAD_Compression) %>% mutate(Prediction_Time = Prediction_Time)

  df1 <- as.data.table(df1)
  df2 <- as.data.table(df2)

  df1_vars <- append(by_join1, by_roll[1])
  df2_vars <- append(by_join2, by_roll[2])

  EvalBase <- paste0("setkey(")
  EvalSet1 <- paste0(EvalBase, "df1, ")
  EvalSet2 <- paste0(EvalBase, "df2, ")
  for (i in 1: length(df1_vars)){
    EvalSet1 <- paste0(EvalSet1, df1_vars[i])
    EvalSet2 <- paste0(EvalSet2, df2_vars[i])
    if (i == length(df1_vars)){
      EvalSet1 <- paste0(EvalSet1, ")")
      EvalSet2 <- paste0(EvalSet2, ")")
    } else {
      EvalSet1 <- paste0(EvalSet1, ", ")
      EvalSet2 <- paste0(EvalSet2, ", ")
    }
  }

  EvalParse(EvalSet1)
  EvalParse(EvalSet2)

  df1 <- df2[df1, roll = roll_val]

  df1 <- as.data.frame(df1)

  return(df1)

}

# ----------------------------------------------------------------------- #
# Configuration
# ----------------------------------------------------------------------- #
# TODO: Set Out_Dir
# ----------------------------------------------------------------------- #

# Database Name (Assuming Maverick)
database_name <- "NavCan_TBS_V3"

# Get Database Connection (RODBC)
con <- Get_DBI_Connection(IP = "192.168.1.23", Database = database_name)


# ----------------------------------------------------------------------- #
# Local Adaptation
# ----------------------------------------------------------------------- #

# Metrics to Display in Table Outputs
Metrics <- c(0.1, 0.5, 1)

## WAD PROXIES
Use_Proxy_Wind <- T
Use_Proxy_Wind_Leader <- T
Use_Proxy_Wind_More <- T
Use_Proxy_Wind_More_Follower <- T
Cap_ORD_Compression <- F
Cap_WAD_Compression <- T
FilterLeadRadar <- F
Remove_Old_Observations <- T
RTTPathLegs <- c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept")
WEPathLegs <- c("Intercept_Leg", "Extended_Intercept")
MaxILSRange <- 4
FAF_Distance_Val <- 4.5
MaxInsideBuffer <- 2
Delivery_Point <- 0

# Minimum Legacy Category Separation Distance (Assumed MRS)
Min_Legacy_Separation <- 3

# Do Filters
Apply_Observed_IAS_Filter <- T
Apply_Predicted_IAS_Filter <- T
Apply_Separation_Accuracy_Filter <- F
Apply_Leader_Establish_Filter <- T

# Speed Filters
Min_Observed_Leader_IAS <- 80
Max_Observed_Leader_IAS <- 300
Min_Observed_Follower_IAS <- 80
Max_Observed_Follower_IAS <- 300
Min_WAD_Leader_IAS <- 80
Max_WAD_Leader_IAS <- 300
Min_WAD_Follower_IAS <- 80
Max_WAD_Follower_IAS <- 300

# ----------------------------------------------------------------------- #
# Data Loading
# ----------------------------------------------------------------------- #
# TODO: Enable Local File Loading
# ----------------------------------------------------------------------- #

# Load the WAD Validation View
WAD_VV <- dbGetQuery(con, sprintf("SELECT * FROM vw_WAD_Validation_View"), stringsAsFactors = F)

# Load the Recat/Legacy Wake Categories
Recat_Wake_Dist <- dbGetQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Dist", stringsAsFactors = F) %>%
  mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852)
Legacy_Wake_Dist <- dbGetQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence", stringsAsFactors = F) %>%
  mutate(WT_Separation_Distance = WT_Separation_Distance / 1852)

# Load the AC to Wake Mappings
Legacy_ACtoWake <- dbGetQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy", stringsAsFactors = F) %>% unique()


# ----------------------------------------------------------------------- #
# Extra Fields
# ----------------------------------------------------------------------- #
# TODO: Generalise Surface Wind/HW Bands
# ----------------------------------------------------------------------- #

# Initialise copy of WAD_VV
WAD_Data <- WAD_VV

# First: Get WAD_Separation_Distance - requires Adding ORD Compression (Using Old, as distance bounds do not change for ORD so value should be very similar)
if (Cap_ORD_Compression){
  WAD_Data <- WAD_Data %>%
    mutate(Old_ORD_Compression = ifelse(Old_ORD_Compression < 0, 0, Old_ORD_Compression))
}

WAD_Data <- WAD_Data %>%
  mutate(WAD_Separation_Distance = WAD_Separation_Distance + Old_ORD_Compression)

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
if (Cap_WAD_Compression){
  WAD_Data <- mutate(WAD_Data, WAD_Compression = ifelse(WAD_Compression > 0, WAD_Compression, 0))
}

## Radar <- RadarOrig
if (Use_Proxy_Wind){

  if (Use_Proxy_Wind_More){
    if (FilterLeadRadar){RadarLead <- filter(Radar, RTTPRoxyFlag == 0)} else {RadarLead <- Radar}
    # Recalculate the Observed Leader Wind Effect and Speed - Based on Predicted Compression Commencement Distance (CCT, 10NM).
    WAD_Data <- GenerateProxyWindEffect(WAD_Data, RadarLead, Algo = "WAD", LorFIn = "Leader", LorFOut = "Leader", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
    if (Use_Proxy_Wind_More & Use_Proxy_Wind_More_Follower){
      WAD_Data <- GenerateProxyWindEffect(WAD_Data, RadarLead, Algo = "WAD", LorFIn = "Follower", LorFOut = "Leader", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
    }
    WAD_Data <- ORDRealignFlags(WAD_Data, "Leader")
    WADSummary <- QuickProxyTablePlot(WAD_Data, "WAD", "Summary", "Leader")
  }

  # Recalculate the Observed Follower Wind Effect and Speed - Based on Predicted Follower Distances and the CCT.
  WAD_Data <- GenerateProxyWindEffect(WAD_Data, Radar, Algo = "WAD", LorFIn = "Follower", LorFOut = "Follower", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
  if (Use_Proxy_Wind & Use_Proxy_Wind_Leader){
    WAD_Data <- GenerateProxyWindEffect(WAD_Data, Radar, Algo = "WAD", LorFIn = "Leader", LorFOut = "Follower", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
  }
  WAD_Data <- ORDRealignFlags(WAD_Data, "Follower")
  WADSummary <- QuickProxyTablePlot(WAD_Data, "WAD", "Summary", "Follower")

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

#print(WAD_Error_Histogram(WAD_Data, Metrics) + xlab("WAD Compression Error (NM)"))

p1 <- ggplot() + geom_histogram(data=WAD_Data, mapping=aes(x = Forecast_Mean_Leader_Wind_Effect_Error, y = ..density..)) + labs(title = "Leader WE Error")
p2 <- ggplot() + geom_histogram(data=WAD_Data, mapping=aes(x = Forecast_Mean_Follower_Wind_Effect_Error, y = ..density..)) + labs(title = "Follower WE Error")
p3 <- ggplot() + geom_histogram(data=WAD_Data, mapping=aes(x = WAD_Compression_Error, y = ..density..)) + labs(title = "Compression Error")
grid.arrange(p1, p2, p3)



################################
# MC plots

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

plot_ord_profile_we(161163)

ord_data <- WAD_Data

plot_ord_profile_ll <- function(lp_id){

  #lp_id <- 118746
  #include_gspd <- T

  # Get the ORD data line

  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)

  title_text <- paste0("Leader IAS Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("ORD Error = ", ord_line$ORD_Compression_Error_Rounded, "NM ", "Leader IAS Error = ", sprintf("%.0f", ord_line$ORD_Leader_IAS_Error), "kt")

  p1 <- tryCatch(plot_single_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, F , title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  title_text <- paste0("Leader GSPD Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("Lead Wind Effect Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error), "kt Follower Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error), "kt")

  p2 <- tryCatch(plot_single_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, T , title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  grid.arrange(p1, p2, nrow = 1)

}

plot_ord_profile_we <- function(lp_id){

  lp_id <- 161163
  #include_gspd <- T

  # Get the ORD data line

  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)

  title_text <- paste0("Leader WE Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("ORD Error = ", ord_line$ORD_Compression_Error_Rounded, "NM ", "Leader WE Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error), "kt")

  p1 <- tryCatch(plot_single_wind_effect(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, "L", ord_line$Leader_FAF_RTT, ord_line$Leader_CC_RTT, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  title_text <- paste0("Follower WE Profile for ", ord_line$Follower_Callsign, " ",ord_line$Follower_Aircraft_Type)
  subtitle_text <- paste0("Follower Wind Effect Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error))

  p2 <- tryCatch(plot_single_wind_effect(ord_line$Follower_Flight_Plan_ID, ord_line$Landing_Pair_ID, "F", ord_line$Follower_Forecast_End_Distance, ord_line$Follower_Forecast_Start_Distance, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  grid.arrange(p1, p2, nrow = 1)

}


plot_ord_profile_lf <- function(lp_id, include_gspd){

  #lp_id <- 855810
  #include_gspd <- T

  # Get the ORD data line

  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)

  title_text <- paste0("Leader IAS Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("IAS Error = ", sprintf("%.0f", ord_line$ORD_Leader_IAS_Error), " Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error))

  p1 <- tryCatch(plot_ord_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, "L", include_gspd, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  title_text <- paste0("Follower IAS Speed Profile for ", ord_line$Follower_Callsign, " ",ord_line$Follower_Aircraft_Type)
  subtitle_text <- paste0("IAS Error = ", sprintf("%.0f", ord_line$ORD_Follower_IAS_Error), " Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error))

  p2 <- tryCatch(plot_ord_aircraft(ord_line$Follower_Flight_Plan_ID, ord_line$Landing_Pair_ID, "F", include_gspd, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })

  grid.arrange(p1, p2, nrow = 1)

}

plot_ord_aircraft <- function(fp_id, lp_id, l_or_f, include_gspd, title_text, subtitle_text){

  #fp_id <- 247329
  #lp_id <- 855810
  #include_gspd <- F
  #l_or_f <- "L"

  leader_track <- dbGetQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))

  # Get the ORD Profile for leader and follower

  ord_profile <- dbGetQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = '", l_or_f, "'"))

  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852,
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852)


  if (include_gspd == T){
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD, color = "GSPD"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_GS, color = "ORD GSPD"))+

      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "GSPD", "ORD IAS", "ORD GSPD"),
                         values = c("IAS" = "black", "GSPD" = "blue", "ORD IAS" = "grey", "ORD GSPD" = "light blue"))+
      theme_bw()+
      theme(legend.position = "bottom")

  } else {
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "ORD IAS"),
                         values = c("IAS" = "black", "ORD IAS" = "grey"))+
      theme_bw()+
      theme(legend.position = "bottom")
  }

  return(p1)

}


plot_single_aircraft <- function(fp_id, lp_id, gspd, title_text, subtitle_text){

  #fp_id <- 31169
  #lp_id <- 118746
  #lp_id <- 855810
  #gspd <- F
  #l_or_f <- "L"

  leader_track <- dbGetQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))

  # Get the ORD Profile for leader and follower

  ord_profile <- dbGetQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = 'L'"))

  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852,
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852)


  if (gspd == T){
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD, color = "GSPD"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_GS, color = "ORD GSPD"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("GSPD", "ORD GSPD"),
                         values = c("GSPD" = "blue", "ORD GSPD" = "light blue"))+
      theme_bw()+
      theme(legend.position = "bottom")

  } else {
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "ORD IAS"),
                         values = c("IAS" = "black", "ORD IAS" = "grey"))+
      theme_bw()+
      theme(legend.position = "bottom")
  }

  return(p1)

}


plot_single_wind_effect <- function(fp_id, lp_id, role, min, max, title_text, subtitle_text){

  #fp_id <- 28298
  #lp_id <- 161163
  #role <- "F"
  #title_text = "test"
  #subtitle_text = "test"

  #min <- 13.11
  #max <- 9.67

  leader_track <- dbGetQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))

  leader_track <- filter(leader_track, Path_Leg_Type %in% Allowed_Path_Legs)

  min_y = 10 * floor(min(c(leader_track$Wind_Effect_IAS,ord_profile$End_WE),  na.rm = T) / 10)
  max_y = 10 * floor(max(c(leader_track$Wind_Effect_IAS,ord_profile$End_WE),  na.rm = T) / 10) + 10

  min_x <- 0
  max_x <- ceiling(max(c(leader_track$ILS_Locus_RTT, max), na.rm = T))

  # Get the ORD Profile for leader and follower

  ord_profile <- dbGetQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = '", role, "'"))

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
