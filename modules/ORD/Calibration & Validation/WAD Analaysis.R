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

library(RODBC)
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
Get_RODBC_Database_Connection <- function(IP, Database){
  User <- getPass(msg = "Username: ", noblank = FALSE, forcemask = FALSE)
  Pass <- getPass(msg = "Password: ", noblank = FALSE, forcemask = TRUE)
  con <- RODBC::odbcDriverConnect(connection=paste0("Driver={SQL Server};
                                  Server={",IP,"};Database={", Database, "};
                                  Uid={",User,"};Pwd={",Pass,"};"))
  return(con)
}

con <- Get_RODBC_Database_Connection(IP = "192.168.1.23", Database = database_name)


# ----------------------------------------------------------------------- #
# Local Adaptation
# ----------------------------------------------------------------------- #

# Metrics to Display in Table Outputs
Metrics <- c(0.1, 0.5, 1)

## WAD PROXIES
Use_Proxy_Wind <- T
Use_Proxy_Wind_Leader <- T
Remove_Old_Observations <- T
RTTPathLegs <- c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept")
WEPathLegs <- c("Intercept_Leg", "Extended_Intercept")
MaxILSRange <- 4
FAF_Distance_Val <- 4.5
MaxInsideBuffer <- 2

# Minimum Legacy Category Separation Distance (Assumed MRS)
Min_Legacy_Separation <- 3

# Do Filters
Apply_Observed_IAS_Filter <- T
Apply_Predicted_IAS_Filter <- T
Apply_Separation_Accuracy_Filter <- F

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
WAD_VV <- sqlQuery(con, sprintf("SELECT * FROM vw_WAD_Validation_View"), stringsAsFactors = F)

# Load the Recat/Legacy Wake Categories
Recat_Wake_Dist <- sqlQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Dist", stringsAsFactors = F) %>%
  mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852)
Legacy_Wake_Dist <- sqlQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence", stringsAsFactors = F) %>%
  mutate(WT_Separation_Distance = WT_Separation_Distance / 1852)

# Load the AC to Wake Mappings
Legacy_ACtoWake <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy", stringsAsFactors = F) %>% unique()

# Load the Radar Data for Follower WE adjustment if necessary
# if (Adjust_Follower_WEs){
#   Radar_Query <- "SELECT
#   RTPD.Flight_Plan_ID,
#   Track_Time,
#   Range_To_Threshold,
#   ILS_Locus_RTT,
#   Range_To_ILS,
#   Path_Leg_Type,
#   Wind_Effect_IAS,
#   Mode_S_IAS
#   FROM vw_Radar_Track_Point_Derived RTPD
#   LEFT JOIN tbl_Flight_Plan FP
#   ON RTPD.Flight_Plan_ID = FP.Flight_Plan_ID
#   LEFT JOIN tbl_Flight_Plan_Derived FPD
#   ON RTPD.Flight_Plan_ID = FPD.Flight_Plan_ID
#   LEFT JOIN tbl_Runway R1
#   ON R1.Runway_Name = RTPD.Mode_S_Wind_Localiser_Capture
#   LEFT JOIN tbl_Runway R2
#   ON R2.Runway_Name = FP.Landing_Runway
#   WHERE FP.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
#   OR FPD.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
#   OR R1.Runway_Group = R2.Runway_Group
#   OR RTPD.Mode_S_Wind_Localiser_Capture IS NULL
#   ORDER BY Flight_Plan_ID, Track_Time"
# 
#   # Get Radar Data
#   Radar <- sqlQuery(con, Radar_Query, stringsAsFactors = F)
# }

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


if (Use_Proxy_Wind){
  Radar <- sqlQuery(con, GetORDAnalysisRadarQuery(), stringsAsFactors = F)
  Radar <- RecalculateRadarValuesORD(Radar, RTTPathLegs, WEPathLegs, MaxILSRange)
  WAD_Data <- GenerateProxyWindEffect(WAD_Data, Radar, Algo = "WAD", LorFIn = "Follower", LorFOut = "Follower", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
  if (Use_Proxy_Wind & Use_Proxy_Wind_Leader){
    WAD_Data <- GenerateProxyWindEffect(WAD_Data, Radar, Algo = "WAD", LorFIn = "Leader", LorFOut = "Follower", MaxInsideBuffer, FAF_Distance_Val, Remove_Old_Observations)
  }
  WAD_Data <- ORDRealignFlags(WAD_Data)
  WADSummary <- QuickProxyTablePlot(WAD_Data, "WAD", "Summary")
}

# if (Adjust_Follower_WEs){
# 
#   # Set
#   data1o <- WAD_Data
#   Radaro <- Radar
# 
#     # Adaptation Data
#     Allowed_Path_Legs <- c("ILS_Leg", "Landing_Leg", "Intercept_Leg", "Extended_Intercept")
#     Max_Range_To_ILS <- 4
#     FAF_Distance_Val <- 4.5 #(Should be matched..)
#     Sep_Buffer <- 1
#     Max_Allowable_Inside_Sep <- 2
# 
#     # data1 reversal (testing)
#     data1 <- data1o
#     Radar <- Radaro
# 
#     # Data Field removal
#     data1 <- select(data1, -c("Observed_Mean_Follower_IAS", "Observed_Mean_Follower_Wind_Effect"))
# 
#     # Change Range to Threshold value based on intercept ILS criteria
#     Radar <- mutate(Radar,
#                     ILS_Intercept_Flag = ifelse(is.na(Range_To_Threshold) & Path_Leg_Type %in% Allowed_Path_Legs & Range_To_ILS <= Max_Range_To_ILS, 1, 0),
#                     ILS_Intercept_Flag = ifelse(is.na(ILS_Intercept_Flag), 0, ILS_Intercept_Flag),
#                     Range_To_Threshold = ifelse(ILS_Intercept_Flag == 1, ILS_Locus_RTT, Range_To_Threshold))
# 
#     # Get the Forecast Compression Start/End Distances for the follower aircraft. (Assume 4.5NM LST)
#     data1 <- mutate(data1,
#                     Follower_Forecast_Start_Distance = WAD_Compression + WAD_Separation_Distance + Sep_Buffer + (Leader_CC_RTT - Leader_FAF_RTT),
#                     Follower_Forecast_End_Distance = WRD_Separation_Distance + Sep_Buffer)
# 
#     # Get the max RTTs
#     data1_allowed <- Radar %>% group_by(Flight_Plan_ID) %>%
#       filter(!is.na(Range_To_Threshold)) %>%
#       mutate(ID = row_number()) %>% ungroup() %>%
#       arrange(Flight_Plan_ID, desc(Range_To_Threshold)) %>%
#       filter(ID == 1) %>%
#       select(Flight_Plan_ID, Max_RTT_Follower = Range_To_Threshold, Follower_Max_ILS_Intercept_Flag = ILS_Intercept_Flag)
# 
#     data_it <- filter(data1, Landing_Pair_Type != "Not_In_Trail")
#     data_nit <- filter(data1, Landing_Pair_Type == "Not_In_Trail")
# 
#     foll_start_dist_it <- select(data_it, Follower_Flight_Plan_ID, Follower_Forecast_Start_Distance)
#     foll_start_dist_nit <- select(data_nit, Follower_Flight_Plan_ID, Follower_Forecast_Start_Distance)
# 
#     data1_sep1 <- Radar %>%
#       left_join(foll_start_dist_it, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID")) %>%
#       filter(!is.na(Range_To_Threshold)) %>%
#       filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
#       arrange(Flight_Plan_ID, Track_Time) %>%
#       group_by(Flight_Plan_ID) %>%
#       mutate(ID = row_number()) %>%
#       ungroup() %>%
#       filter(ID == 1) %>%
#       select(Flight_Plan_ID, Est_Start_RTT_Follower = Range_To_Threshold, Follower_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
# 
#     data_it <- left_join(data_it, data1_sep1, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
# 
#     data1_sep2 <- Radar %>%
#       left_join(foll_start_dist_nit, by = c("Flight_Plan_ID" = "Follower_Flight_Plan_ID")) %>%
#       filter(!is.na(Range_To_Threshold)) %>%
#       filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
#       arrange(Flight_Plan_ID, Track_Time) %>%
#       group_by(Flight_Plan_ID) %>%
#       mutate(ID = row_number()) %>%
#       ungroup() %>%
#       filter(ID == 1) %>%
#       select(Flight_Plan_ID, Est_Start_RTT_Follower = Range_To_Threshold, Follower_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
# 
#     data_nit <- left_join(data_nit, data1_sep2, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
# 
#     data1 <- rbind(data_it, data_nit)
# 
#     # Join on the Max RTTs
#     data1 <- left_join(data1, data1_allowed, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID"))
# 
#     data_it <- filter(data1, Landing_Pair_Type != "Not_In_Trail")
#     data_nit <- filter(data1, Landing_Pair_Type == "Not_In_Trail")
# 
#     lead_start_dist_nit <- select(data_nit, Leader_Flight_Plan_ID, Follower_Forecast_Start_Distance)
#     lead_start_dist_it <- select(data_it, Leader_Flight_Plan_ID, Follower_Forecast_Start_Distance)
# 
#     data1_sep1 <- Radar %>%
#       left_join(lead_start_dist_it, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID")) %>%
#       filter(!is.na(Range_To_Threshold)) %>%
#       filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
#       arrange(Flight_Plan_ID, Track_Time) %>%
#       group_by(Flight_Plan_ID) %>%
#       mutate(ID = row_number()) %>%
#       ungroup() %>%
#       filter(ID == 1) %>%
#       select(Flight_Plan_ID, Est_Start_RTT_Leader = Range_To_Threshold, Leader_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
# 
#     data_it <- left_join(data_it, data1_sep1, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
# 
#     data1_sep2 <- Radar %>%
#       left_join(lead_start_dist_nit, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID")) %>%
#       filter(!is.na(Range_To_Threshold)) %>%
#       filter(Range_To_Threshold <= Follower_Forecast_Start_Distance) %>%
#       arrange(Flight_Plan_ID, Track_Time) %>%
#       group_by(Flight_Plan_ID) %>%
#       mutate(ID = row_number()) %>%
#       ungroup() %>%
#       filter(ID == 1) %>%
#       select(Flight_Plan_ID, Est_Start_RTT_Leader = Range_To_Threshold, Leader_Start_ILS_Intercept_Flag = ILS_Intercept_Flag)
# 
#     data_nit <- left_join(data_nit, data1_sep2, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
# 
#     data1 <- rbind(data_it, data_nit)
# 
#     # Change data1_allowed for use of Leader parameters
#     data1_allowed <- rename(data1_allowed,
#                             Max_RTT_Leader = Max_RTT_Follower,
#                             Leader_Max_ILS_Intercept_Flag = Follower_Max_ILS_Intercept_Flag)
# 
# 
#     # Join on the Max RTTs (Leader!)
#     data1 <- left_join(data1, data1_allowed, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))
# 
#     # Follower Filter flag
#     data1 <- mutate(data1, Invalid_Follower_Flag = ifelse(Max_RTT_Follower < (Follower_Forecast_Start_Distance - Max_Allowable_Inside_Sep), 1, 0)) %>%
#       mutate(Invalid_Follower_Flag = ifelse(is.na(Invalid_Follower_Flag), 1, Invalid_Follower_Flag))
# 
#     # Filter for the Allowed/Not Allowed
#     data1_fol <- filter(data1, Invalid_Follower_Flag == 0)
#     data1_nofol <- filter(data1, Invalid_Follower_Flag == 1) %>% mutate(Observed_Mean_Follower_IAS = NA,
#                                                                         Failed_Valid_Follower_Flag = 0)
# 
#     # Perform calculations for Allowed Pairs
#     data1_fol <- Get_Average_Observed_Mode_S_Parameters(data1_fol, Radar,
#                                                         Prefix = "Rename_Me",
#                                                         "Follower",
#                                                         "Range",
#                                                         Start_Var = "Follower_Forecast_Start_Distance",
#                                                         End_Var = "Follower_Forecast_End_Distance") %>%
#       rename(Observed_Mean_Follower_Wind_Effect = Observed_Follower_Rename_Me_Wind_Effect,
#              Observed_Mean_Follower_IAS = Observed_Follower_Rename_Me_IAS)
# 
#     # Get the Failed Follower Attempts
#     data1_folfailed <- filter(data1_fol, is.na(Observed_Mean_Follower_Wind_Effect)) %>% select(-Observed_Mean_Follower_Wind_Effect) %>%
#       mutate(Invalid_Follower_Flag = 1,
#              Failed_Valid_Follower_Flag = 1)
# 
#     # Remove these from the follower dataset - this is now complete
#     data1_fol <- filter(data1_fol, !is.na(Observed_Mean_Follower_Wind_Effect)) %>% mutate(Failed_Valid_Follower_Flag = 0)
# 
#     # Bind on to the Nofol data
#     data1_nofol <- rbind(data1_nofol, data1_folfailed)
# 
#     # Leader Filter flag
#     data1_nofol <- mutate(data1_nofol, Invalid_Leader_Flag = ifelse(Max_RTT_Leader < (Follower_Forecast_Start_Distance - Max_Allowable_Inside_Sep), 1, 0)) %>%
#       mutate(Invalid_Leader_Flag = ifelse(is.na(Invalid_Leader_Flag), 1, Invalid_Leader_Flag))
# 
#     # Filter for the Allowed/Not Allowed
#     data1_nofol1 <- filter(data1_nofol, Invalid_Leader_Flag == 0) %>% select(-Invalid_Leader_Flag)
#     data1_nofol2 <- filter(data1_nofol, Invalid_Leader_Flag == 1) %>% select(-Invalid_Leader_Flag) %>% mutate(Observed_Mean_Follower_Wind_Effect = NA)
# 
#     # Perform calculations for Allowed Pairs
#     data1_nofol1 <- Get_Average_Observed_Mode_S_Parameters(data1_nofol1, Radar,
#                                                            Prefix = "Rename_Me",
#                                                            "Leader",
#                                                            "Range",
#                                                            Start_Var = "Follower_Forecast_Start_Distance",
#                                                            End_Var = "Follower_Forecast_End_Distance") %>%
#       rename(Observed_Mean_Follower_Wind_Effect = Observed_Leader_Rename_Me_Wind_Effect) %>%
#       select(-Observed_Leader_Rename_Me_IAS)
# 
#     # Bind together Leader data
#     data1_nofol <- rbind(data1_nofol1, data1_nofol2)
# 
#     # Bind datasets together again
#     data1 <- rbind(data1_fol, data1_nofol)
# 
#     data1 <- mutate(data1, Forecast_Mean_Follower_Wind_Effect_Error = Observed_Mean_Follower_Wind_Effect - Forecast_Mean_Follower_Wind_Effect)
#     data1 <- arrange(data1, desc(Forecast_Mean_Follower_Wind_Effect_Error))
# 
#     data1 <- mutate(data1,
#                     Not_Calculated_Flag = ifelse(is.na(Observed_Mean_Follower_Wind_Effect), 1, 0),
#                     Follower_Standard_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 0 & Follower_Start_ILS_Intercept_Flag == 0, 1, 0),
#                     Follower_Extended_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 0 & Follower_Start_ILS_Intercept_Flag == 1, 1, 0),
#                     Leader_Standard_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 1 & Leader_Start_ILS_Intercept_Flag == 0, 1, 0),
#                     Leader_Extended_Flag = ifelse(Not_Calculated_Flag == 0 & Invalid_Follower_Flag == 1 & Leader_Start_ILS_Intercept_Flag == 1, 1, 0)
#     )
# 
#     # How many will be removed?
#     print(paste0("Will Remove ", nrow(filter(data1, is.na(Forecast_Mean_Follower_Wind_Effect))), " Observations out of ", nrow(data1), "."))
# 
#     # Replace WAD_Data
#     WAD_Data <- data1
# 
# }

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

# Set up Flags for Compression Error being above 0, 0.1, 0.5 and 1NM
# WAD_Data <- WAD_Data %>%
#   mutate(WAD_Compression_Error_1 = ifelse(WAD_Compression_Error_Rounded >= 1, 1, 0),
#          WAD_Compression_Error_05 = ifelse(WAD_Compression_Error_Rounded >= 0.5, 1, 0),
#          WAD_Compression_Error_01 = ifelse(WAD_Compression_Error_Rounded >= 0.1, 1, 0),
#          WAD_Compression_Error_0 = ifelse(WAD_Compression_Error_Rounded >= 0, 1, 0))

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

ggplot() + geom_histogram(data=WAD_Data, mapping=aes(x = WAD_Compression_Error, y = ..density..))
