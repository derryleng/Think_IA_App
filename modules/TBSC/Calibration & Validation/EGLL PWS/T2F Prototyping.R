
# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# T2F Speed Calibration
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v1.0
#
# Authors: George Clark
#
# Description: All TBS Processing elements for the UTMA Validation Database.
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

ModuleFolder <- "TBSC"
ModuleSubfolder <- "Calibration & Validation"
ModuleProjectFolder <- "EGLL PWS"
# --------------------------------------------------------------------------- #
#
FileFlag <- "global.R"
ResourcesFolder <- "resources"
AlgoResourcesFolder <- "algorithm_functions"
ModulesFolder <- "modules"

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
  if (exists("ModuleProjectFolder")){ModuleSubfolder <- file.path(ModuleSubfolder, ModuleProjectFolder)}
  Script_Dir <- file.path(Global_Dir, ModulesFolder, ModuleFolder, ModuleSubfolder)
}

Global_Dir <- file.path(Global_Dir, ResourcesFolder)
Algo_Func_Dir <- file.path(Global_Dir, AlgoResourcesFolder)
DB_Module_Dir <- file.path(Script_Dir, "Database Modules")

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Source Global Configuration Files (Requires Shiny Integration)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path(Global_Dir, "imports.R"), local = T)
source(file.path(Global_Dir, "unit conversions.R"), local = T)
source(file.path(Global_Dir, "functions.R"), local = T)

# Start Time
Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Source Function Files
# ----------------------------------------------------------------------------------------------------------------------------------------- #
source(file.path(Algo_Func_Dir, "ORD Functions (DB).R"), local = T) # ORD Functions
source(file.path(Algo_Func_Dir, "TBSC Functions (PWS).R"), local = T)
#source(file.path(Algo_Func_Dir, "GWCS Functions (DB).R"), local = T) # GWCS Functions for Wind Forecasting
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------------- #
# Functions ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

library(RODBC)
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
library(factoextra)

# Summary tables by Surface Headwind/Wind Groups
TableFunction <- function(Data, Groups, Suffix){
  
  Data <- Data %>%
    group_by(!!!syms(Groups)) %>%
    summarise(!!sym(paste0("Count", Suffix)) := n()) %>%
    ungroup()
  
  return(Data)
  
}

# Function to Generate "Baseline"
GenerateBaseline <- function(Table, Grouping, Segs, Min_Seg, Max_Seg){
  
  Baseline <- filter(Table, Surface_Headwind_Group == "(-5,5]") %>% select(-Surface_Headwind_Group) %>% filter(DME_Seg >= Min_Seg & DME_Seg <= Max_Seg) %>%
    full_join(Segs, by = c("DME_Seg")) %>% mutate(Count = ifelse(is.na(Count), 0, Count))
  
  Scores <- Baseline %>% group_by(!!sym(Grouping)) %>%
    summarise(Min_Count = min(Count, na.rm = T),
              Avg_Count = mean(Count, na.rm = T)) %>%
    ungroup()
  
  Baseline <- left_join(Baseline, Scores, by = setNames(Grouping, Grouping))
  
  return(Baseline)
  
}

T2F_IAS_Summary_Table <- function(Data, Groupings){
  
  Summary <- Data %>%
    group_by(!!!syms(Groupings)) %>%
    summarise(Count = n(),
              Median_IAS = median(Ave_Mode_S_IAS, na.rm = T),
              Mean_IAS = mean(Ave_Mode_S_IAS, na.rm = T),
              Std_IAS = sd(Ave_Mode_S_IAS, na.rm = T)) %>%
    ungroup()
  
  return(Summary)
  
}

# ---------------------------------------------------------------------------------- #
# Configuration ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# Database Setup.
Database <- "EGLL_PWS"
IP <- "192.168.1.23"

# Database Connection
con <- Get_DBI_Connection(IP, Database)

# Give the users initials.
User_Initials <- "GC"

# Set PRoject Code (3 for EGLL PWS)
Project_Code <- 3

# Set Base Output Directory
Base_Dir <- GetSaveDirectory(Project = Project_Code, "Flying Time Analysis", "Outputs")

# Output Version
Version <- "v0.0.1"
Version_Folder <- paste0(Sys.Date(), " ", Version, " (", User_Initials, ")")

# Get Base Input Directory
Base_Dir_Input <- GetSaveDirectory(Project = Project_Code, "Flying_Time_Analysis_Input", "Inputs")

# Input Version (and are we using?)
Version_Folder_Input <- "Prototyping v0.0"

# Create Directories 
Out_Dir <- file.path(Base_Dir, Version_Folder)
Create_Directory(Out_Dir)
Adaptation_Dir <- file.path(Out_Dir, "Adaptation")
Create_Directory(Adaptation_Dir)
Plot_Dir <- file.path(Out_Dir, "Plots")
Create_Directory(Plot_Dir)
Inp_Dir <- file.path(Base_Dir_Input, Version_Folder_Input)
Create_Directory(Inp_Dir)
Compare_Dir <- file.path(Out_Dir, "Comparisons")
Create_Directory(Compare_Dir)

# Get the Intention Code
Code_or_Airfield <- "Code" # "Code" | "Airfield" -- Decides whether to use the 2 or 4 character suffix for adaptation files.
if (Code_or_Airfield == "Code"){AdapSuffix <- as.character(Load_Adaptation_Table(con, "tbl_Airfield")$Destination_Code)}
if (Code_or_Airfield == "Airfield"){AdapSuffix <- as.character(Load_Adaptation_Table(con, "tbl_Airfield")$Airfield_Name)}

# If Prototyping set to T, save the Legacy adaptation as the same as the Recat.
Prototyping <- T

# ---------------------------------------------------------------------------------- #
# Adaptation ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# What DME Seg for the quick plot below?
MikesOutputs <- F

# Adaptable Maximum Separation Accuracy Parameter
Use_Sep_Acc <- T
Remove_Neg_Sep_Acc <- T
Sep_Accuracy_Max <- 3

# Speed up to FAF filter
Use_FAF_Speed <- F
Max_Speed_To_FAF <- 180
Min_Speed_To_FAF <- 155
Min_FAF_RTT <- 3
Max_FAF_RTT <- 12
FAF_Speed_Used <- "Mean_Speed_To_FAF"

# Segments to have in adaptation (Min/Max RTT)
Min_Seg <- 0
Max_Seg <- 9

# Minimum counts of aircraft (Minimum segment count, average segment count)
Min_Count_Min <- 30
Avg_Count_Min <- 50

# Speed variable used ("Median_IAS" | "Mean_IAS")
Speed_Used <- "Mean_IAS"

# Use Procedural Speed past Applied_From Range To Threshold?
Procedural_Applied <- F # Turn on/Off
Procedural_Cap <- 160 # Minimum PRocedural Speed
Applied_From <- 6 # Range To Threshold at which above will be max(Procedural_Cap, Average_Speed @ Applied_From DME)

# Do we want to Calibrate AC Types by Cluster?
Use_Clusters <- T
Num_Clusters <- 6
Cluster_Max_DME <- 6
Use_Ref_Wind_Clusters <- T
Min_Count_Min_Cluster <- 5

# Create Adaptation Table
Adaptation <- data.frame(
  Filter = c("Separation Accuracy Filter Active", "Filter Negative Separation Accuracy", "Maximum Separation Accuracy (NM)", 
             "Speed up to FAF Filter Active", "Min Speed to FAF (kts)", "Max Speed to FAF (kts)", "Min RTT for FAF Filter (NM)", "Max RTT for FAF Filter (NM)", "Speed Used for FAF Filter",
             "Min Seg in T2F Adaptation", "Max Seg in T2F Adaptation", "Overall Minimum Seg Count for AC Type", "Average Minimum Seg Count for AC Type", "Speed Used for Adaptation",
             "Profile cut at Procedural Speed?", "Procedural Speed Capped Value", "Segment at which PS Cap Applies from", 
             "Clustering for Extra AC Types Active", "Number of Clusters used", "Maximum DME Distance for Clustering", "Clustering in Reference Winds Only?", "Minimum Count for Clustering"),
  Value = c(Use_Sep_Acc, ifelse(Use_Sep_Acc, Remove_Neg_Sep_Acc, NA), ifelse(Use_Sep_Acc, Sep_Accuracy_Max, NA), Use_FAF_Speed, ifelse(Use_FAF_Speed, Min_Speed_To_FAF, NA), 
            ifelse(Use_FAF_Speed, Max_Speed_To_FAF, NA), ifelse(Use_FAF_Speed, Min_FAF_RTT, NA), ifelse(Use_FAF_Speed, Max_FAF_RTT, NA), ifelse(Use_FAF_Speed, FAF_Speed_Used, NA),
            Min_Seg, Max_Seg, Min_Count_Min, Avg_Count_Min, Speed_Used, Procedural_Applied, ifelse(Procedural_Applied, Procedural_Cap, NA), ifelse(Procedural_Applied, Applied_From, NA),
            Use_Clusters, ifelse(Use_Clusters, Num_Clusters, NA), ifelse(Use_Clusters, Cluster_Max_DME, NA), ifelse(Use_Clusters, Use_Ref_Wind_Clusters, NA), ifelse(Use_Clusters, Min_Count_Min_Cluster, NA))
)

# Filter out NA values for clarity.
Adaptation <- filter(Adaptation, !is.na(Value))

# ---------------------------------------------------------------------------------- #
# Data Loading ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# Load the T2F Calibration View.
View_Path <- file.path(Inp_Dir, "vw_T2F_Calibration_View.csv")

if (file.exists(View_Path)){
  T2FView <- fread(View_Path)
  message("Loaded in T2F Calibration View, Version ", Version_Folder_Input, ".")
} else {
    message("Tried to use local input but none found. Will take view from database and save into ", Version_Folder_Input, " directory.")
    T2FView <- dbGetQuery(con, "SELECT * FROM vw_T2F_Calibration_View")
    fwrite(T2FView, View_Path)
}

# ---------------------------------------------------------------------------------- #
# Initial Maniuplation ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# Filter out for flags with the Segment Duration Flag.
T2FData <- filter(T2FView, Seg_Duration_Flag == 0)

# Create the Surface Wind/Headwind Groups
T2FData <- T2FData %>%
  mutate(Surface_Wind_SPD_Group = cut(Surface_Wind_SPD, breaks = c(0, 3, 6, 10, 15, 20, 30, Inf)),
         Surface_Headwind_Group = cut(Surface_Headwind, breaks = c(-Inf, -30, -15, -5, 5, 15, 30, Inf)))

# Remove NA Surface Headwind Group
T2FData <- filter(T2FData, !is.na(Surface_Headwind_Group))

# Make df of Segs from min to max
Segs <- data.frame(DME_Seg = seq(Min_Seg, Max_Seg, 1))

# ---------------------------------------------------------------------------------- #
# Filtering ----
# ---------------------------------------------------------------------------------- #
# Seems like the Separation Accuracy is rperesented incorrectly.
# Uses Delivered_FAF_Separation - Legacy_FAF_All_Separation_Distance
# Definitely an issue with incorrect MRS but extends further than this
# ---------------------------------------------------------------------------------- #

# Find the Average Speed up to the FAF
FAFSpeeds <- T2FData %>%
  filter(DME_Seg > Min_FAF_RTT & DME_Seg < Max_FAF_RTT) %>%
  group_by(Landing_Pair_ID) %>%
  summarise(Mean_Speed_To_FAF = mean(Ave_Mode_S_IAS, na.rm = T),
            Med_Speed_To_FAF = median(Ave_Mode_S_IAS, na.rm = T),
            SD_Speed_To_FAF = sd(Ave_Mode_S_IAS, na.rm = T)) %>%
  ungroup()

# Join on the Average Speed(s) up to the FAF
T2FData <- left_join(T2FData, FAFSpeeds, by = c("Landing_Pair_ID"))

# Remove FAFSpeeds
rm(FAFSpeeds)

# Create Copy of Data for Filtering
T2FDataFilt <- T2FData

# Apply the FAF Speed filter
if (Use_FAF_Speed){
  T2FDataFilt <- filter(T2FDataFilt, !!sym(FAF_Speed_Used) >= Min_Speed_To_FAF & !!sym(FAF_Speed_Used) <= Max_Speed_To_FAF)
}

# Apply the Sep Accuracy Filter (at FAF)
if (Use_Sep_Acc){
  T2FDataFilt <- filter(T2FDataFilt, Delivered_Separation_Accuracy <= Sep_Accuracy_Max)
  if (Remove_Neg_Sep_Acc){
    T2FDataFilt <- filter(T2FDataFilt, Delivered_Separation_Accuracy >= 0)
  }
}

# ---------------------------------------------------------------- #
# Plots/Tables for Checks 
# ---------------------------------------------------------------- #

# Quick distribution of FAF Speeds to look at where we should apply the filters
ggplot() + geom_histogram(data = T2FDataFilt, mapping = aes(x = Mean_Speed_To_FAF, y = ..density..)) + facet_wrap(~Follower_Recat_Wake_Cat)

# Quick check of Counts with and without applied filters
SampleGrouping <- c("Landing_Runway", "Leader_Recat_Wake_Cat", "Follower_Recat_Wake_Cat")
SampleCompare <- left_join(TableFunction(T2FData, SampleGrouping, ""), TableFunction(T2FDataFilt, SampleGrouping, "_Filt"), by = setNames(SampleGrouping, SampleGrouping)) %>% 
  mutate(Diff = Count - Count_Filt, Perc = round(100 * (Diff/Count), 2)) %>% arrange(Perc)

# ---------------------------------------------------------------------------------- #
# Outputs for Mike/New Clustering. ----
# ---------------------------------------------------------------------------------- #
# Use for clustering.
# ---------------------------------------------------------------------------------- #

# Initial outputs for Mike's first cluster analysis investigation.
if (MikesOutputs){
  
  # Tables of Mean/Median Speed grouped by Different Categories
  Table1 <- T2F_IAS_Summary_Table(T2FData, Groupings = c("Follower_Aircraft_Type", "DME_Seg"))
  Table2 <- T2F_IAS_Summary_Table(T2FData, Groupings = c("Follower_Recat_Wake_Cat", "DME_Seg"))
  Table3 <- T2F_IAS_Summary_Table(T2FData, Groupings = c("Surface_Headwind_Group", "DME_Seg"))
  Table4 <- T2F_IAS_Summary_Table(T2FData, Groupings = c("Surface_Wind_SPD_Group", "DME_Seg"))
  Table5 <- T2F_IAS_Summary_Table(T2FData, Groupings = c("Follower_Aircraft_Type", "Surface_Headwind_Group", "DME_Seg"))
  Table6 <- T2F_IAS_Summary_Table(T2FData, Groupings = c("Follower_Recat_Wake_Cat", "Surface_Headwind_Group", "DME_Seg"))
  
  # Save these tables
  fwrite(Table1, "Speeds_Aircraft_Type.csv")
  fwrite(Table2, "Speeds_Wake_Cat.csv")
  fwrite(Table3, "Speeds_SHW_Group.csv")
  fwrite(Table4, "Speeds_SWS_Group.csv")
  fwrite(Table5, "Speeds_Aircraft_Type_SHW_Group.csv")
  fwrite(Table6, "Speeds_Wake_Cat_SHW_Group.csv")
  
}

if (Use_Clusters){
  
  # Set a Random Seed to ensure same results every run with same adaptation.
  set.seed(123)
  
  # Get the Average Speeds for new Cluster Analysis.
  if (Use_Ref_Wind_Clusters){
    DataClusterStart <- T2F_IAS_Summary_Table(T2FDataFilt, Groupings = c("Follower_Aircraft_Type", "Surface_Headwind_Group", "DME_Seg")) %>%
      filter(Surface_Headwind_Group == "(-5,5]") %>% select(-Surface_Headwind_Group)
  } else {
    DataClusterStart <- T2F_IAS_Summary_Table(T2FDataFilt, Groupings = c("Follower_Aircraft_Type", "DME_Seg"))
  }
  
  # Filter for Segments we want
  DataClusterStart <- filter(DataClusterStart, DME_Seg <= Cluster_Max_DME)
  
  # Filter only for Observations above the Minimum for clusters.
  DataClusterStart <- group_by(DataClusterStart, Follower_Aircraft_Type) %>% 
    mutate(Min_Obs = min(Count)) %>% 
    filter(Min_Obs > Min_Count_Min_Cluster) %>% ungroup()
  
  # Put into Wide Format.
  DataClusterWide <- pivot_wider(DataClusterStart, id_cols = c("Follower_Aircraft_Type"), names_prefix = "DME", names_from = DME_Seg, values_from = !!sym(Speed_Used)) %>% 
    filter(!is.na(DME0))
  
  # Have a look at the optimal cluster numbers.
  print(fviz_nbclust(select(DataClusterWide, -Follower_Aircraft_Type), kmeans, method = "silhouette"))
  
  # Clustering
  DataCluster <- kmeans(select(DataClusterWide, -Follower_Aircraft_Type), Num_Clusters, nstart = 25)
  
  # Join on the Clusters.
  DataClusterWide <- mutate(DataClusterWide, Cluster = DataCluster$cluster)
  
  # Get a Lookup Table with Aircraft Type to Cluster Number.
  DataClusterAC <- select(DataClusterWide, Follower_Aircraft_Type, Cluster)
  
  # Join the Cluster Numbers onto the main dataset.
  T2FDataFilt <- left_join(T2FDataFilt, DataClusterAC, by = c("Follower_Aircraft_Type"))
  
  ## PLOTS
  
  # Put into Long Format for plotting. Note that currently only works for DME Distances < 10.
  DataClusterLong <- pivot_longer(DataClusterWide, DME0:!!sym(paste0("DME", Cluster_Max_DME)), names_to = "DME_Seg", values_to = "Average_IAS") %>% 
    mutate(Cluster = as.factor(Cluster), DME_Dist = as.numeric(substr(DME_Seg,4,4)))
  
  # Generate Cluster Visualisations 
  ClusterPlot <- ggplot(DataClusterLong)+
     geom_line(aes(x = DME_Dist, y = Average_IAS, group = Follower_Aircraft_Type, color = Cluster))+
     geom_text(data = filter(DataClusterLong, DME_Dist ==0), mapping = aes(x = DME_Dist, y = Average_IAS, label = Follower_Aircraft_Type), size = 3)+
     labs(title = "Clustering of Median IAS Profiles", x = "Range to Threshold (NM)", y = "Median IAS (kt)")+
     theme_classic()
  
  # Save plot to File.
  png(file.path(Plot_Dir, paste0("Aircraft Cluster Profiles ", Version_Folder, ".png")), width = 1200, height = 960)
  print(ClusterPlot)
  dev.off()
  
}

# ---------------------------------------------------------------------------------- #
# Speed Calibration ----
# ---------------------------------------------------------------------------------- #
# Calibration for Wake, Aircraft Type, and Cluster if Available
# ---------------------------------------------------------------------------------- #

# Create summary table with both Wake Cat and Aircraft Type.
TableWake <- T2F_IAS_Summary_Table(T2FDataFilt, Groupings = c("Follower_Recat_Wake_Cat", "Surface_Headwind_Group", "DME_Seg"))
TableAC <- T2F_IAS_Summary_Table(T2FDataFilt, Groupings = c("Follower_Aircraft_Type", "Surface_Headwind_Group", "DME_Seg"))
TableFull <- T2F_IAS_Summary_Table(T2FDataFilt, Groupings = c("Follower_Aircraft_Type", "Follower_Recat_Wake_Cat", "Surface_Headwind_Group", "DME_Seg"))

# Generate Baselines
Wake <- GenerateBaseline(TableWake, "Follower_Recat_Wake_Cat", Segs, Min_Seg, Max_Seg) %>% mutate(Follower_Aircraft_Type = "All")
AC <- GenerateBaseline(TableFull, "Follower_Aircraft_Type", Segs, Min_Seg, Max_Seg)

# Get the Wake level output. This doesn't require a minimum observations (Currently because of very low number of F types in ref winds)
WakeOutput <- Wake %>% select(Wake_Cat = Follower_Recat_Wake_Cat, Range_To_Threshold = DME_Seg, Average_IAS = !!sym(Speed_Used)) %>% mutate(Average_IAS = round(Average_IAS, 0))

# Get the VALID AC Level output. This requires a minimum min seg observation and average seg observation count.
AC <- AC %>% mutate(Flag = ifelse(Min_Count >= Min_Count_Min & Avg_Count >= Avg_Count_Min, 1, 0))
ACOutput <- filter(AC, Flag == 1) %>%
  select(Aircraft_Type = Follower_Aircraft_Type, Range_To_Threshold = DME_Seg, Average_IAS = !!sym(Speed_Used)) %>% mutate(Average_IAS = round(Average_IAS, 0)) %>%
  mutate(Used_Clustering = 0)

# If using Clustering, 
if (Use_Clusters){
  ACClusterAircraft <- unique(filter(AC, Flag == 0)$Follower_Aircraft_Type)
  TableCluster <- T2F_IAS_Summary_Table(T2FDataFilt, Groupings = c("Cluster", "Surface_Headwind_Group", "DME_Seg"))
  ACOutput2 <- GenerateBaseline(TableCluster, "Cluster", Segs, Min_Seg, Max_Seg)
  ACOutput2 <- left_join(DataClusterAC, ACOutput2, by = c("Cluster")) %>%
    filter(Follower_Aircraft_Type %in% ACClusterAircraft) %>%
    select(Aircraft_Type = Follower_Aircraft_Type, Range_To_Threshold = DME_Seg, Average_IAS = !!sym(Speed_Used)) %>% mutate(Average_IAS = round(Average_IAS, 0)) %>%
    mutate(Used_Clustering = 1)
  ACOutput <- rbind(ACOutput, ACOutput2)
  rm(ACOutput2)
}

# Quick table of what AC types have been calibrated with clustering
ACClustered <- select(ACOutput, Aircraft_Type, Range_To_Threshold, Used_Clustering)
ACOutput <- select(ACOutput, -Used_Clustering)

# If applicable, set speeds beyound Applied_From DME to procedural speed.
if (Procedural_Applied){
  WakeProc <- filter(WakeOutput, Range_To_Threshold == Applied_From) %>% select(-Range_To_Threshold) %>% rename(Proc_IAS = Average_IAS)
  ACProc <- filter(ACOutput, Range_To_Threshold == Applied_From) %>% select(-Range_To_Threshold) %>% rename(Proc_IAS = Average_IAS)
  WakeOutput <- left_join(WakeOutput, WakeProc, by = c("Wake_Cat")) %>% mutate(Average_IAS = ifelse(Range_To_Threshold > Applied_From, ifelse(Proc_IAS < Procedural_Cap, Procedural_Cap, Proc_IAS), Average_IAS)) %>%
    select(-Proc_IAS)
  ACOutput <- left_join(ACOutput, ACProc, by = c("Aircraft_Type")) %>% mutate(Average_IAS = ifelse(Range_To_Threshold > Applied_From, ifelse(Proc_IAS < Procedural_Cap, Procedural_Cap, Proc_IAS), Average_IAS)) %>%
    select(-Proc_IAS)
  rm(WakeProc, ACProc)
}

# Remove Unrequired Data.
rm(TableFull)

# ---------------------------------------------------------------------------------- #
# Time Calibration ----
# ---------------------------------------------------------------------------------- #
# Calibration for Wake and Aircraft Type
# ---------------------------------------------------------------------------------- #

# ---------------------------------------------------------------- #
# For Wake Level
# ---------------------------------------------------------------- #

# Load in the Wake Distances (Currently only from database
Distances_Wake_WTC <- Load_Adaptation_Table(con, "tbl_Reference_Recat_Separation_Dist")

# Create a shorter DF for Follower WTC/Distance Pairs
Distances_Wake_WTC_Unique <- select(Distances_Wake_WTC, -Leader_WTC) %>% unique()

# Make a copy of the Wake Speeds Output and put the end of the DME Seg on (TEMP: Assumes Seg Size = 1NM)
WakeOutputTime <- WakeOutput %>% mutate(Range_To_Threshold = Range_To_Threshold * 1852, Range_To_Threshold_End = Range_To_Threshold + 1852, Average_IAS = Average_IAS * (1852/3600))

# Join on the Distances so we can do all the processing at once.
WakeOutputTime <- left_join(WakeOutputTime, Distances_Wake_WTC_Unique, by = c("Wake_Cat" = "Follower_WTC"))

# Add a Flag to determine the segments the distances cover and filter
WakeOutputTime <- WakeOutputTime %>%
  mutate(Segment_Flag = ifelse(Reference_Wake_Separation_Distance > Range_To_Threshold, 1, 0)) %>%
  filter(Segment_Flag == 1) %>% select(-Segment_Flag)

# Get the weighted speed
WakeOutputTime <- WakeOutputTime %>%
  mutate(Weight = ifelse(Reference_Wake_Separation_Distance <= Range_To_Threshold_End, (Range_To_Threshold_End - Range_To_Threshold) - (Range_To_Threshold_End - Reference_Wake_Separation_Distance), (Range_To_Threshold_End - Range_To_Threshold)),
         Weight = Weight / Reference_Wake_Separation_Distance,
         SpeedProduct = Average_IAS * Weight)

# Sum the SpeedProduct field to get the Average Speed across the Separation Distance and then find the time.
WakeOutputTime <- WakeOutputTime %>%
  group_by(Wake_Cat, Reference_Wake_Separation_Distance) %>%
  summarise(Assumed_Wake_Separation_IAS = sum(SpeedProduct, na.rm=T)) %>%
  ungroup() %>%
  mutate(Reference_Wake_Separation_Time = Reference_Wake_Separation_Distance / Assumed_Wake_Separation_IAS)

# Round to 0DP and convert back to aviation units.
WakeOutputTime <- WakeOutputTime %>%
  mutate(Assumed_Wake_Separation_IAS = round(Assumed_Wake_Separation_IAS * (3600/1852), 0),
         Reference_Wake_Separation_Time = round(Reference_Wake_Separation_Time, 0))

# Join back onto original distances just in case there were any duplicates.
Wake_Adaptation_Output <- left_join(Distances_Wake_WTC, WakeOutputTime, by = c("Follower_WTC" = "Wake_Cat", "Reference_Wake_Separation_Distance")) %>%
  arrange(Leader_WTC, Follower_WTC)

# Separate the Times and Speeds
WakeOutputTimes <- select(Wake_Adaptation_Output, Leader_WTC, Follower_WTC, Reference_Wake_Separation_Time)
WakeOutputSpeeds <- select(Wake_Adaptation_Output, Leader_WTC, Follower_WTC, Assumed_Wake_Separation_IAS)
  

# ---------------------------------------------------------------- #
# For Aircraft Level
# ---------------------------------------------------------------- #

# Load in the Wake Distances (Currently only from database
Distances_Wake_AC <- Load_Adaptation_Table(con, "tbl_Reference_ACTP_Wake_Separation_Dist")

# Create a shorter DF for Follower WTC/Distance Pairs
Distances_Wake_AC_Unique <- select(Distances_Wake_AC, -Leader_Aircraft_Type) %>% unique()

# Make a copy of the Wake Speeds Output and put the end of the DME Seg on (TEMP: Assumes Seg Size = 1NM)
ACOutputTime <- ACOutput %>% mutate(Range_To_Threshold = Range_To_Threshold * 1852, Range_To_Threshold_End = Range_To_Threshold + 1852, Average_IAS = Average_IAS * (1852/3600))

# Join on the Distances so we can do all the processing at once.
ACOutputTime <- left_join(ACOutputTime, Distances_Wake_AC_Unique, by = c("Aircraft_Type" = "Follower_Aircraft_Type"))

# Add a Flag to determine the segments the distances cover and filter
ACOutputTime <- ACOutputTime %>%
  mutate(Segment_Flag = ifelse(Reference_Wake_Separation_Distance > Range_To_Threshold, 1, 0)) %>%
  filter(Segment_Flag == 1) %>% select(-Segment_Flag)

# Get the weighted speed
ACOutputTime <- ACOutputTime %>%
  mutate(Weight = ifelse(Reference_Wake_Separation_Distance <= Range_To_Threshold_End, (Range_To_Threshold_End - Range_To_Threshold) - (Range_To_Threshold_End - Reference_Wake_Separation_Distance), (Range_To_Threshold_End - Range_To_Threshold)),
         Weight = Weight / Reference_Wake_Separation_Distance,
         SpeedProduct = Average_IAS * Weight)

# Sum the SpeedProduct field to get the Average Speed across the Separation Distance and then find the time.
ACOutputTime <- ACOutputTime %>%
  group_by(Aircraft_Type, Reference_Wake_Separation_Distance) %>%
  summarise(Assumed_Wake_Separation_IAS = sum(SpeedProduct, na.rm=T)) %>%
  ungroup() %>%
  mutate(Reference_Wake_Separation_Time = Reference_Wake_Separation_Distance / Assumed_Wake_Separation_IAS)

# Round to 0DP and convert back to aviation units.
ACOutputTime <- ACOutputTime %>%
  mutate(Assumed_Wake_Separation_IAS = round(Assumed_Wake_Separation_IAS * (3600/1852), 0),
         Reference_Wake_Separation_Time = round(Reference_Wake_Separation_Time, 0))

# Join back onto original distances just in case there were any duplicates.
AC_Adaptation_Output <- left_join(Distances_Wake_AC, ACOutputTime, by = c("Follower_Aircraft_Type" = "Aircraft_Type", "Reference_Wake_Separation_Distance")) %>%
  arrange(Leader_Aircraft_Type, Follower_Aircraft_Type) %>% filter(!is.na(Reference_Wake_Separation_Time))

# Separate the Times and Speeds
ACOutputTimes <- select(AC_Adaptation_Output, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_Wake_Separation_Time)
ACOutputSpeeds <- select(AC_Adaptation_Output, Leader_Aircraft_Type, Follower_Aircraft_Type, Assumed_Wake_Separation_IAS)

# Get the Updated Distance matrix. Only consider those with valid Times/Speeds
ACOutputDists <- select(AC_Adaptation_Output, Leader_Aircraft_Type, Follower_Aircraft_Type, Reference_Wake_Separation_Distance) %>%
  mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance)

# Remove unrequired tables
rm(ACOutputTime, WakeOutputTime)

# ---------------------------------------------------------------- #
# Save Outputs
# ---------------------------------------------------------------- #

# Save Local Adaptation to base version directory
fwrite(Adaptation, file.path(Out_Dir, paste0("Local Adaptation ", Version_Folder, ".csv")))

# Save Summary Tables if required for further analysis.
fwrite(TableWake, file.path(Summary_Dir, paste0("Wake Level Summary ", Version_Folder, ".csv")))
fwrite(TableAC, file.path(Summary_Dir, paste0("Aircraft Level Summary ", Version_Folder, ".csv")))
fwrite(TableCluster, file.path(Summary_Dir, paste0("Cluster Level Summary ", Version_Folder, ".csv")))

# New T2F Profile Adaptation
fwrite(WakeOutput, file.path(Adaptation_Dir, paste0("Populate_tbl_T2F_Wake_Adaptation_", AdapSuffix, ".csv")))
fwrite(ACOutput, file.path(Adaptation_Dir, paste0("Populate_tbl_T2F_Aircraft_Adaptation_", AdapSuffix, ".csv")))

# Reference Times (and Speeds if required)
fwrite(WakeOutputTimes, file.path(Adaptation_Dir, paste0("Populate_tbl_Reference_Recat_Separation_Time_", AdapSuffix, ".csv")))
fwrite(WakeOutputSpeeds, file.path(Adaptation_Dir, paste0("Populate_tbl_Assumed_Recat_Separation_IAS_", AdapSuffix, ".csv")))
fwrite(ACOutputTimes, file.path(Adaptation_Dir, paste0("Populate_tbl_Reference_ACTP_Wake_Separation_Time_", AdapSuffix, ".csv")))
fwrite(ACOutputSpeeds, file.path(Adaptation_Dir, paste0("Populate_tbl_Assumed_ACTP_Wake_Separation_IAS_", AdapSuffix, ".csv")))
fwrite(ACOutputDists, file.path(Adaptation_Dir, paste0("Populate_tbl_Reference_ACTP_Wake_Separation_Dist_", AdapSuffix, ".csv")))

# Legacy Reference Times and Speeds if Prototyping.
if (Prototyping){
  fwrite(WakeOutput, file.path(Adaptation_Dir, paste0("Populate_tbl_T2F_Wake_Adaptation_Legacy_", AdapSuffix, ".csv")))
  fwrite(ACOutput, file.path(Adaptation_Dir, paste0("Populate_tbl_T2F_Aircraft_Adaptation_Legacy_", AdapSuffix, ".csv")))
  fwrite(WakeOutputTimes, file.path(Adaptation_Dir, paste0("Populate_tbl_Reference_Wake_Separation_Time_Legacy_", AdapSuffix, ".csv")))
  fwrite(WakeOutputSpeeds, file.path(Adaptation_Dir, paste0("Populate_tbl_Assumed_Wake_Separation_IAS_Legacy_", AdapSuffix, ".csv")))
  fwrite(ACOutputTimes, file.path(Adaptation_Dir, paste0("Populate_tbl_Reference_ACTP_Wake_Separation_Time_Legacy_", AdapSuffix, ".csv")))
  fwrite(ACOutputSpeeds, file.path(Adaptation_Dir, paste0("Populate_tbl_Assumed_ACTP_Wake_Separation_IAS_Legacy_", AdapSuffix, ".csv")))
  fwrite(ACOutputDists, file.path(Adaptation_Dir, paste0("Populate_tbl_Reference_ACTP_Wake_Separation_Dist_Legacy_", AdapSuffix, ".csv")))
}

# ---------------------------------------------------------------------------------- #
# Analysis ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# Create Copy of Data exclusively for plotting
T2FDataPlot <- filter(T2FDataFilt, DME_Seg <= 8)

# Boxplot of All Aircraft IAS across Separation Distance and Surface Headwind Group
SHWPlotAll <- ggplot(data = T2FDataPlot) + geom_boxplot(mapping = aes(x = as.factor(DME_Seg), y = Ave_Mode_S_IAS, group = as.factor(DME_Seg))) + facet_wrap(~Surface_Headwind_Group) +
  labs(x = "Range To Threshold (NM)", y = "Mode S IAS (kts)", title = "Mode S IAS by Separation Distance and Surface Headwind Group") + theme_light()
png(file.path(Plot_Dir, paste0("Overall Mode S IAS by Sep Dist & SHW Group ", Version_Folder, ".png")), width = 800, height = 480)
print(SHWPlotAll)
dev.off()

# ---------------------------------------------------------------------------------- #
# Comparisons ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# Choose the version for Comparison.
Version_Folder_Compare <- "2021-07-22 v0.0 (GC)"

# Get the Comparison Output Directories.
Out_Dir_Compare <- file.path(Base_Dir, Version_Folder_Compare)
Adaptation_Dir_Compare <- file.path(Out_Dir_Compare, "Adaptation")

# Local Adaptation Comparison
Adaptation_Compare <- fread(file.path(Out_Dir_Compare, paste0("Local Adaptation ", Version_Folder_Compare, ".csv"))) %>% rename(Old_Value = Value)
Adaptation_Compare <- full_join(Adaptation, Adaptation_Compare, by = c("Filter"))
Adaptation_Compare_Diff <- filter(Adaptation_Compare, Old_Value != Value)
fwrite(Adaptation_Compare, file.path(Compare_Dir, paste0("Local Adaptation Comparison (vs. ", Version_Folder_Compare, ").csv")))
fwrite(Adaptation_Compare_Diff, file.path(Compare_Dir, paste0("Local Adaptation Differences (vs. ", Version_Folder_Compare, ").csv")))

# Sample Size Comparison


# T2F Adaptation Comparison






