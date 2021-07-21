# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ORD Final Decel Distance Analysis
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v1.0
#
# Authors: George Clark
#
# Description: Quick analysis into the effect of a calibrated variable end final deceleration distance
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
# Distribution by Aircraft Type ----
# ---------------------------------------------------------------------------------- #
# Investigate the distributions of Leader IAS Error by Aircraft Type.
# ---------------------------------------------------------------------------------- #

# Load the Error Data
ORD_Errors <- dbGetQuery(con, "SELECT * FROM vw_ORD_Prediction_Compare") %>% 
  mutate(Leader_IAS_Error_Diff = ORD_Leader_IAS_Error_New - ORD_Leader_IAS_Error_Old,
         Leader_IAS_Diff = ORD_Leader_IAS_New - ORD_Leader_IAS_Old,
         Leader_IAS_Error_Mag_Diff = abs(ORD_Leader_IAS_Error_New) - abs(ORD_Leader_IAS_Error_Old))

# Filtering

# Quick Boxplots by Aircraft Type
for (Wake in c("A", "B", "C", "D", "E", "F")){
  
  # Filter by Wake
  ORD_Errors_Wake <- filter(ORD_Errors, Leader_Recat_Wake_Cat == Wake) 
  
  # Boxplot by Aircraft Type
  P1 <- ggplot() + geom_boxplot(data = ORD_Errors_Wake, mapping = aes(x = Leader_Aircraft_Type, y = ORD_Leader_IAS_Error_New)) +
    labs(title = paste0("New ORD Leader IAS Errors for Category ", Wake, " by Aircraft Type (With EFDD - With TFG)"),
         x = "Aircraft Type", y = "ORD Leader IAS Error") + ylim(-5, 5)
  P2 <- ggplot() + geom_boxplot(data = ORD_Errors_Wake, mapping = aes(x = Leader_Aircraft_Type, y = ORD_Leader_IAS_Error_Old)) +
    labs(title = paste0("Old ORD Leader IAS Errors for Category ", Wake, " by Aircraft Type (With EFDD - With TFG)"),
         x = "Aircraft Type", y = "ORD Leader IAS Error") + ylim(-5, 5)
  P3 <- ggplot() + geom_boxplot(data = ORD_Errors_Wake, mapping = aes(x = Leader_Aircraft_Type, y = Leader_IAS_Error_Mag_Diff)) +
    labs(title = paste0("ORD Leader IAS Error Differences for Category ", Wake, " by Aircraft Type (With EFDD - With TFG)"),
         x = "Aircraft Type", y = "ORD Leader Mag IAS Error Difference") + ylim(-5, 5)
  grid.arrange(P1, P2, P3)
  # P4 <- ggplot() + geom_boxplot(data = ORD_Errors_Wake, mapping = aes(x = Leader_Aircraft_Type, y = Leader_IAS_Diff)) +
  #   labs(title = paste0("ORD Leader IAS Differences for Category ", Wake, " by Aircraft Type (With EFDD - With TFG)"),
  #        x = "Aircraft Type", y = "ORD Leader IAS Difference") + ylim(-10, 10)
  # 
  CompAgg <- ORD_Errors_Wake %>% group_by(Leader_Aircraft_Type) %>%
    summarise(Mean_Compression_Error_New = mean(ORD_Compression_Error_New, na.rm=T),
              Mean_Compression_Error_Old = mean(ORD_Compression_Error_Old, na.rm=T))

  PC1 <- ggplot(data = CompAgg) +
    geom_col(mapping = aes(x = Leader_Aircraft_Type, y = Mean_Compression_Error_New))
  PC2 <- ggplot(data = CompAgg) +
    geom_col(mapping = aes(x = Leader_Aircraft_Type, y = Mean_Compression_Error_Old))

  grid.arrange(PC1, PC2)
  
}




