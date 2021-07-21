
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

ModuleFolder <- "ORD"
ModuleSubfolder <- "Database Processing"
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
source(file.path(DB_Module_Dir, "All Pair Reference Data.R"), local = T) # All Pair Reference Data Functions
source(file.path(DB_Module_Dir, "ORD-WAD Observation.R"), local = T) # ORD Observation Functions
source(file.path(DB_Module_Dir, "ORD Aircraft Profile.R"), local= T) # ORD Aircraft Profile Functions
source(file.path(DB_Module_Dir, "ORD IAS-GSPD Profile.R"), local= T) # ORD Segment/IAS/GS Profile Functions
source(file.path(DB_Module_Dir, "ORD-WAD Prediction.R"), local= T) # ORD Prediction Functions
source(file.path(DB_Module_Dir, "Setup IA Performance Model.R"), local= T) # Setup Performance Model Functions
# ----------------------------------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Configuration (## TODO: SETUP FOR SHINY)
# ----------------------------------------------------------------------------------------------------------------------------------------- #
Database <- "EGLL_PWS"
IP <- "192.168.1.23"
# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Database Connection
con <- Get_DBI_Connection(IP, Database)
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
# Adaptation ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# What DME Seg for the quick plot below?
MikesOutputs <- F

# Adaptable Maximum Separation Accuracy Parameter
Use_Sep_Acc <- F
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
Speed_Used <- "Median_IAS"

# Use Procedural Speed past Applied_From Range To Threshold?
Procedural_Applied <- T # Turn on/Off
Procedural_Cap <- 160 # Minimum PRocedural Speed
Applied_From <- 6 # Range To Threshold at which above will be max(Procedural_Cap, Average_Speed @ Applied_From DME)

# Do we want to Calibrate AC Types by Cluster?
Use_Clusters <- T
Num_Clusters <- 8
Cluster_Max_DME <- 6
Use_Ref_Wind_Clusters <- T
Min_Count_Min_Cluster <- 5

# ---------------------------------------------------------------------------------- #
# Data Loading ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

# Load the T2F Calibration View.
T2FView <- dbGetQuery(con, "SELECT * FROM vw_T2F_Calibration_View")

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
  
  # ## PLOTS
  # seg2_long <- pivot_longer(seg2_wide, DME0:DME6, names_to = "DME_Seg", values_to = "Median_IAS") %>% mutate(cluster = as.factor(cluster), DME_Dist = as.numeric(substr(DME_Seg,4,4)))
  # 
  # ggplot(seg2_long)+
  #   geom_line(aes(x = DME_Dist, y = Median_IAS, group = Follower_Aircraft_Type, color = cluster))+
  #   geom_text(data = filter(seg2_long, DME_Dist ==0), mapping = aes(x = DME_Dist, y = Median_IAS, label = Follower_Aircraft_Type), size = 3)+
  #   labs(title = "Clustering of Median IAS Profiles", x = "Range to Threshold (NM)", y = "Median IAS (kt)")+
  #   theme_classic()
  
}

# ---------------------------------------------------------------------------------- #
# Calibration ----
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
  TableCluster <- T2F_IAS_Summary_Table(T2FDataFilt, Groupings = c("Cluster", "Surface_Headwind_Group", "DME_Seg")) %>%
    GenerateBaseline("Cluster", Segs, Min_Seg, Max_Seg)
  ACOutput2 <- left_join(DataClusterAC, TableCluster, by = c("Cluster")) %>%
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
rm(TableWake, TableAC, TableFull)

# ---------------------------------------------------------------------------------- #
# Analysis ----
# ---------------------------------------------------------------------------------- #
# 
# ---------------------------------------------------------------------------------- #

test <- filter(Full, Follower_Recat_Wake_Cat == "B") %>% mutate(Flag = ifelse(Min_Count >= Min_Count_Min & Avg_Count >= Avg_Count_Min, 1, 0)) %>% filter(Flag == 1)
ggplot(data = test, mapping = aes(x = DME_Seg, y = Median_IAS)) + geom_point() + geom_line(mapping = aes(color = as.factor(Flag))) + facet_wrap(~Follower_Aircraft_Type)

Full <- rbind(AC, Wake) %>% arrange(Follower_Recat_Wake_Cat)

for (Wake in c("A", "B", "C", "D", "E", "F")){
  #print(ggplot(data = filter(T2FData, Follower_Recat_Wake_Cat == Wake & DME_Seg < 11), mapping = aes(x = as.factor(DME_Seg), y = Ave_Mode_S_IAS)) + geom_boxplot() + facet_wrap(~Follower_Aircraft_Type))
  print(ggplot(data = filter(T2FData, Follower_Recat_Wake_Cat == Wake & DME_Seg < 10), mapping = aes(x = as.factor(DME_Seg), y = Ave_Mode_S_IAS)) + geom_boxplot() + facet_wrap(~Surface_Headwind_Group))
}

# 
# s1 <- dbGetQuery(con, "SELECT * FROM tbl_ORD_Prediction")
# s2 <- dbGetQuery(con, "SELECT * FROM tbl_ORD_Prediction_Legacy")
# 
# p1 <- ggplot() + geom_histogram(data = s1, mapping = aes(x = ORD_Leader_IAS_Error, y = ..density..))
# p2 <- ggplot() + geom_histogram(data = s2, mapping = aes(x = ORD_Leader_IAS_Error, y = ..density..))
# grid.arrange(p1, p2)
# 
# 
# 
# s3 <- left_join(select(s1, Landing_Pair_ID, ORD_Leader_IAS_New = ORD_Mean_Leader_IAS), select(s2, Landing_Pair_ID, ORD_Leader_IAS_Old = ORD_Mean_Leader_IAS), by = c("Landing_Pair_ID"))
# 
# 





