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

# Choose mode of analysis
Mode <- c("EFDD", "TTB", "Profile Analysis")[2]

# Function for EFDD Plot.
Generate_EFDD_Plot <- function(Data, Group){
  
  if (Group == "Wake"){
    Title <- "Leader Wake Category"
    OldGroup <- "Leader_Legacy_Wake_Cat"
    NewGroup <- "Leader_Recat_Wake_Cat"
  }
  
  if (Group == "Aircraft"){
    Title <- "Leader Aircraft Type"
    OldGroup <- "Leader_Aircraft_Type"
    NewGroup <- "Leader_Aircraft_Type"
  }
  
  P1 <- ggplot() + geom_boxplot(data = Data, mapping = aes(x = !!sym(NewGroup), y = ORD_Leader_IAS_Error_New)) +
    labs(title = paste0("(With EFDD) ORD Leader IAS Errors by ", Title),
         x = Title, y = "ORD Leader IAS Error (kts)") + ylim(-5, 5) + geom_hline(yintercept = 0)
  P2 <- ggplot() + geom_boxplot(data = Data, mapping = aes(x = !!sym(NewGroup), y = ORD_Leader_IAS_Error_Old)) +
    labs(title = paste0("(With TFG) ORD Leader IAS Errors by ", Title),
         x = Title, y = "ORD Leader IAS Error (kts)") + ylim(-5, 5) + geom_hline(yintercept = 0)
  P3 <- ggplot() + geom_boxplot(data = Data, mapping = aes(x = !!sym(NewGroup), y = Leader_IAS_Error_Diff)) +
    labs(title = paste0("ORD Leader IAS Error Diffs by ", Title, " (EFDD-TFG)"),
         x = Title, y = "ORD Leader IAS Error Diff (kts)") + ylim(-5, 5) + geom_hline(yintercept = 0)
  P4 <- ggplot() + geom_boxplot(data = Data, mapping = aes(x = !!sym(NewGroup), y = EFDD_Diff)) +
    labs(title = paste0("ORD Leader EFDD Distance Diffs by ", Title, " (EFDD-TFG)"),
         x = Title, y = "Leader EFDD Difference (NM)") + geom_hline(yintercept = 0)
  
  return(grid.arrange(P1, P2, P3, P4))
  
}

# ---------------------------------------------------------------------------------- #
# Distribution by Aircraft Type ----
# ---------------------------------------------------------------------------------- #
# Investigate the distributions of Leader IAS Error by Aircraft Type.
# ---------------------------------------------------------------------------------- #

if (Mode == "EFDD"){
  
  # Load the Error Data
  ORD_Errors <- dbGetQuery(con, "SELECT * FROM vw_ORD_Prediction_Compare") %>% 
    mutate(Leader_IAS_Error_Diff = ORD_Leader_IAS_Error_New - ORD_Leader_IAS_Error_Old,
           Leader_IAS_Diff = ORD_Leader_IAS_New - ORD_Leader_IAS_Old,
           Leader_IAS_Error_Mag_Diff = abs(ORD_Leader_IAS_Error_New) - abs(ORD_Leader_IAS_Error_Old),
           EFDD_Diff = EFDD_New - EFDD_Old)
  
  ACCounts <- ORD_Errors %>% group_by(Leader_Aircraft_Type) %>% summarise(Count = n(), Abs_Average_EFDD_Diff = abs(mean(EFDD_Diff, na.rm=T)), Average_EFDD_Diff = mean(EFDD_Diff, na.rm=T)) %>% 
    ungroup() 
  
  # Top 10 Aircraft.
  N <- 10
  TopNCounts <- ACCounts %>% arrange(desc(Count)) %>% mutate(Row = row_number()) %>% filter(Row <= N)
  TopNErrors <- filter(ORD_Errors, Leader_Aircraft_Type %in% TopNCounts$Leader_Aircraft_Type)
  
  # Top 5 Aircraft (by ifference of EFDD, High and Low)
  N2 <- 5
  MinCount <- 100
  
  TopHighCounts <- ACCounts %>% filter(Count > MinCount) %>% arrange(desc(Average_EFDD_Diff)) %>% mutate(Row = row_number()) %>% filter(Row <= N2)
  TopHighErrors <- filter(ORD_Errors, Leader_Aircraft_Type %in% TopHighCounts$Leader_Aircraft_Type)
  
  TopLowCounts <- ACCounts %>% filter(Count > MinCount) %>% arrange(Average_EFDD_Diff) %>% mutate(Row = row_number()) %>% filter(Row <= N2)
  TopLowErrors <- filter(ORD_Errors, Leader_Aircraft_Type %in% TopLowCounts$Leader_Aircraft_Type)
  
  png("Wake EFDD.png", width = 960, height = 600)
  Generate_EFDD_Plot(ORD_Errors, "Wake")
  dev.off()
  
  png("Top 10 Common Aircraft EFDD.png", width = 960, height = 600)
  Generate_EFDD_Plot(TopNErrors, "Aircraft")
  dev.off()
  
  png("Top 5 Larger EFDD.png", width = 960, height = 600)
  Generate_EFDD_Plot(TopHighErrors, "Aircraft")
  dev.off()
  
  png("Top 5 Smaller EFDD.png", width = 960, height = 600)
  Generate_EFDD_Plot(TopLowErrors, "Aircraft")
  dev.off()
  
}


# ---------------------------------------------------------------------------------- #
# Performance Model Comparison ----
# ---------------------------------------------------------------------------------- #
# Investigate the Distributions of Perfect/Actual Time Separations.
# ---------------------------------------------------------------------------------- #

if (Mode == "TTB"){
  
  # Load datasets
  PM_Original <- fread("Original TBSC PM.csv")
  PM_TTB <- dbGetQuery(con, "SELECT * FROM tbl_IA_Performance_Model")
  LSS <- dbGetQuery(con, "SELECT Landing_Pair_ID, Landing_Stabilisation_Speed_Type AS Foll_Landing_Stabilisation_Speed_Type FROM tbl_ORD_Aircraft_Profile WHERE This_Pair_Role = 'F'")
  
  # Join on original Data to TTB Data to compare all 3.
  PM_Original <- PM_Original %>% mutate(Original_Diff = Perfect_Recat_Threshold_Wake_Separation_Time - Reference_Recat_Wake_Separation_Time) %>% select(Original_Diff)
  PM_TTB <- cbind(PM_TTB, PM_Original)
  
  PM_TTB <- left_join(PM_TTB, LSS, by = c("Landing_Pair_ID"))
  
  # Get the differences between perfect times and reference times.
  PM_TTB <- PM_TTB %>%
    mutate(T2F_Diff = Perfect_Recat_Threshold_Wake_Separation_Time - Reference_Recat_Wake_Separation_Time,
           ORD_Diff = Perfect_Legacy_Threshold_Wake_Separation_Time - Reference_Recat_Wake_Separation_Time,
           Compare_Abs = abs(ORD_Diff - T2F_Diff),
           Observed_AGI_Surface_Headwind = Observed_AGI_Surface_Headwind / kts_To_mps,
           Surface_Headwind_Group = cut(Observed_AGI_Surface_Headwind, breaks = c(-Inf, -30, -15, -5, 5, 15, 30, Inf)))
  
  # Optional: filter out the 2NM minimum distances. 
  PM_TTBFilt <- PM_TTB
  PM_TTBFilt <- filter(PM_TTBFilt,  
                         !is.na(Surface_Headwind_Group) & Surface_Headwind_Group != "(30, Inf]")
  
  # Summary Tables
  d <- PM_TTBFilt %>% group_by(Foll_Landing_Stabilisation_Speed_Type) %>% summarise(Count = n()) %>% ungroup()
  d1 <- PM_TTBFilt %>% group_by(Follower_Aircraft_Type) %>% summarise(Count = n()) %>% ungroup() %>% arrange(desc(Count)) %>% mutate(row = row_number())
  d2 <- PM_TTBFilt %>% group_by(Surface_Headwind_Group) %>% summarise(Count = n()) %>% ungroup()
  d3 <- PM_TTBFilt %>% group_by(Surface_Headwind_Group, Foll_Landing_Stabilisation_Speed_Type) %>% summarise(Count = n()) %>% ungroup() %>% arrange(Foll_Landing_Stabilisation_Speed_Type)
  d4 <- PM_TTBFilt %>% group_by(Follower_Aircraft_Type) %>% 
    summarise(Count = n(), Average_Error_Diff = median(Compare_Abs, na.rm=T), Average_T2F = mean(T2F_Diff, na.rm=T), Average_ORD = mean(ORD_Diff, na.rm=T)) %>% 
    ungroup() %>% mutate(Error_Mod = abs(Average_T2F - Average_ORD)) %>% arrange(desc(Error_Mod)) %>%
    filter(Count > 200) %>% mutate(row = row_number()) %>% filter(row <= 5)
  
  PlotOrig <- ggplot(PM_TTBFilt) + geom_histogram(mapping = aes(x = Original_Diff, y = ..density..)) + xlim(-20, 20) + geom_vline(xintercept = 0) +
    labs(x = "Time Delta (s) [Perfect - Reference]", y = "Observation Density", title = "TBS Performance: Original Method") + theme_bw()
  PlotORD <- ggplot(PM_TTBFilt) + geom_histogram(mapping = aes(x = ORD_Diff, y = ..density..)) + xlim(-20, 20) + geom_vline(xintercept = 0) +
    labs(x = "Time Delta (s) [Perfect - Reference]", y = "Observation Density", title = "TBS Performance: ORD TTB") + theme_bw()
  PlotT2F <- ggplot(PM_TTBFilt) + geom_histogram(mapping = aes(x = T2F_Diff, y = ..density..)) + xlim(-20, 20) + geom_vline(xintercept = 0) +
    labs(x = "Time Delta (s) [Perfect - Reference]", y = "Observation Density", title = "TBS Performance: AC Type TTB") + theme_bw()
  grid.arrange(PlotORD, PlotT2F)
  
  # Quick histograms by Surface Wind
  grid.arrange(
    PlotORD + facet_wrap(~Surface_Headwind_Group),
    PlotT2F + facet_wrap(~Surface_Headwind_Group)
  )
  # Quick histograms by Follower Wake Cat
  PlotORD + facet_wrap(~Follower_Recat_Wake_Cat)
  PlotT2F + facet_wrap(~Follower_Recat_Wake_Cat)
  
  # Boxplots
  BoxplotORD <- ggplot(PM_TTBFilt) + geom_boxplot(mapping = aes(x = Surface_Headwind_Group, y = ORD_Diff)) + ylim(-5, 5) +
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Surface Headwind Group", title = "TBS Performance: ORD TTB") + theme_bw()
  
  BoxplotT2F <- ggplot(PM_TTBFilt) + geom_boxplot(mapping = aes(x = Surface_Headwind_Group, y = T2F_Diff)) + ylim(-5, 5) +
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Surface Headwind Group", title = "TBS Performance: AC Type TTB") + theme_bw()
  
  grid.arrange(BoxplotORD, BoxplotT2F)
  
  BoxplotORD <- ggplot(filter(PM_TTBFilt, Follower_Recat_Wake_Cat != "F")) + geom_boxplot(mapping = aes(x = Follower_Recat_Wake_Cat, y = ORD_Diff)) + ylim(-5, 5) +
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Wake Category", title = "TBS Performance: ORD TTB") + theme_bw()
  
  BoxplotT2F <- ggplot(filter(PM_TTBFilt, Follower_Recat_Wake_Cat != "F")) + geom_boxplot(mapping = aes(x = Follower_Recat_Wake_Cat, y = T2F_Diff)) + ylim(-5, 5) +
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Wake Category", title = "TBS Performance: AC Type TTB") + theme_bw()
  
  grid.arrange(BoxplotORD, BoxplotT2F)
  
  grid.arrange(
    BoxplotORD + facet_wrap(~Follower_Recat_Wake_Cat), 
    BoxplotT2F + facet_wrap(~Follower_Recat_Wake_Cat)
  )
  
  BoxplotORD + facet_wrap(~Foll_Landing_Stabilisation_Speed_Type)
  BoxplotT2F + facet_wrap(~Foll_Landing_Stabilisation_Speed_Type)
  
  # Filter for top 10 AC Types
  #d1filt <- filter(d1, row <= 10)
  PM_TTBFilt <- mutate(PM_TTBFilt, Reference_Recat_Wake_Separation_Distance = Reference_Recat_Wake_Separation_Distance / 1852)
  
  PM_TTBFilt2 <- filter(PM_TTBFilt, Follower_Aircraft_Type %in% d4$Follower_Aircraft_Type)
  
  p1 <- ggplot(PM_TTBFilt2) + geom_boxplot(mapping = aes(x = Follower_Aircraft_Type, y = ORD_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Follower Aircraft Type", title = "TBS Performance: ORD TTB") + theme_bw()
  p2 <- ggplot(PM_TTBFilt2) + geom_boxplot(mapping = aes(x = Follower_Aircraft_Type, y = T2F_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Follower Aircraft Type", title = "TBS Performance: AC Type TTB") + theme_bw()
  
  grid.arrange(p1, p2)
  
  # PM_TTBFilt2 <- filter(PM_TTBFilt, Follower_Aircraft_Type %in% d1filt$Follower_Aircraft_Type) %>% left_join(d1filt, by = c("Follower_Aircraft_Type"))
  # 
  # p1 <- ggplot(PM_TTBFilt2) + geom_boxplot(mapping = aes(x = Follower_Aircraft_Type, y = ORD_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+
  #   labs(y = "Time Delta (s) [Perfect - Reference]", x = "Follower Aircraft Type", title = "TBS Performance: ORD TTB") + theme_bw()
  # p2 <- ggplot(PM_TTBFilt2) + geom_boxplot(mapping = aes(x = Follower_Aircraft_Type, y = T2F_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+
  #   labs(y = "Time Delta (s) [Perfect - Reference]", x = "Follower Aircraft Type", title = "TBS Performance: AC Type TTB") + theme_bw()
  # 
  # grid.arrange(p1, p2)
  
  ggplot(PM_TTBFilt2) + geom_boxplot(mapping = aes(x = as.factor(Reference_Recat_Wake_Separation_Distance), y = ORD_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+ facet_wrap(~Follower_Aircraft_Type)+
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Separation Distance (NM)", title = "TBS Performance: ORD TTB") + theme_bw()
  ggplot(PM_TTBFilt2) + geom_boxplot(mapping = aes(x = as.factor(Reference_Recat_Wake_Separation_Distance), y = T2F_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+ facet_wrap(~Follower_Aircraft_Type)+
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Separation Distance (NM)", title = "TBS Performance: AC Type TTB") + theme_bw()
  
  grid.arrange(p1, p2)
  
  p1 <- ggplot(PM_TTBFilt) + geom_boxplot(mapping = aes(x = as.factor(Reference_Recat_Wake_Separation_Distance), y = ORD_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Separation Distance (NM)", title = "TBS Performance: ORD TTB") + theme_bw()
  p2 <- ggplot(PM_TTBFilt) + geom_boxplot(mapping = aes(x = as.factor(Reference_Recat_Wake_Separation_Distance), y = T2F_Diff)) + ylim(-5, 5) +geom_hline(yintercept=0)+
    labs(y = "Time Delta (s) [Perfect - Reference]", x = "Separation Distance (NM)", title = "TBS Performance: AC Type TTB") + theme_bw()
  
  grid.arrange(p1, p2)
  
  # ### Surface wind, time delta scatter
  # Pa <- ggplot(PM_TTBFilt, mapping = aes(x = Observed_AGI_Surface_Headwind, y = T2F_Diff)) + geom_point(alpha = 0.05) + ylim(-20, 20) + geom_smooth(method=lm, se=FALSE) +
  #   labs(y = "Time Delta (s) [Perfect - Reference]", x = "Surface Headwind (kts)", title = "Surface Headwind against Time Diff: T2F TTB") + theme_bw()
  # Pb <- ggplot(PM_TTBFilt, mapping = aes(x = Observed_AGI_Surface_Headwind, y = ORD_Diff)) + geom_point(alpha = 0.05) + ylim(-20, 20) + geom_smooth(method=lm, se=FALSE) +
  #   labs(y = "Time Delta (s) [Perfect - Reference]", x = "Surface Headwind (kts)", title = "Surface Headwind against Time Diff: ORD TTB") + theme_bw()
  # 
  # grid.arrange(Pa, Pb)
  # 
  # Choose xamples to test Separation Distance differences
  dist <- 5
  HWs <- c(15)
  MinComp <- 1
  Plots <- 6
  
  # for (minhw in HWs){
  #   
  #   Test1 <- mutate(PM_TTBFilt, Compare_Abs = abs(ORD_Diff - T2F_Diff))
  #   
  #   Test1 <- filter(PM_TTBFilt, abs(ORD_Diff) < 0.5 & abs(T2F_Diff) > 2 & Reference_Recat_Wake_Separation_Distance == dist) %>% filter(Observed_AGI_Surface_Headwind >= minhw) %>%
  #     arrange(Observed_AGI_Surface_Headwind)
  #   
  #   #Test1 <- filter(Test1, Compare_Abs > MinComp, Reference_Recat_Wake_Separation_Distance == dist & Observed_AGI_Surface_Headwind <= minhw) %>% arrange(Compare_Abs)
  #   
  #   for (i in 1:min(nrow(Test1), Plots)){
  #     message("Plotting Graph ", i, " of ", min(nrow(Test1), Plots))
  #     Test <- Test1[i,]
  #     TestAC <- Test$Follower_Aircraft_Type
  #     TestLPID <- Test$Landing_Pair_ID
  #     TestSW <- round(Test$Observed_AGI_Surface_Headwind, 0)
  #     TestFPID <- as.character(dbGetQuery(con, paste0("SELECT Follower_Flight_Plan_ID FROM tbl_Landing_Pair WHERE Landing_Pair_ID = ", TestLPID)))
  #     TestT2F <- dbGetQuery(con, paste0("SELECT * FROM tbl_T2F_Aircraft_Adaptation WHERE Aircraft_Type = '", TestAC, "'")) %>% mutate(Range_To_Threshold = Range_To_Threshold / 1852, Average_IAS = Average_IAS * 3600/1852)
  #     TestGS <- dbGetQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile_Legacy WHERE This_Pair_Role = 'F' AND Landing_Pair_ID = ", TestLPID)) %>% filter(Section_Number <= 9) %>% mutate(End_GS = End_GS * 3600/1852, End_IAS = End_IAS * 3600/1852, End_Dist = End_Dist / 1852)
  #     TestRadar <- dbGetQuery(con, paste0("SELECT Mode_S_GSPD, Range_To_Threshold FROM tbl_Radar_Track_Point RTP INNER JOIN tbl_Radar_Track_Point_Derived RTPD ON RTP.Radar_Track_Point_ID = RTPD.Radar_Track_Point_ID WHERE Flight_Plan_ID = ", TestFPID)) %>% 
  #       filter(Range_To_Threshold <= 7*1852) %>% filter(Mode_S_GSPD > 0) %>% mutate(Range_To_Threshold = Range_To_Threshold / 1852, Mode_S_GSPD = Mode_S_GSPD*3600/1852)
  #     
  #     ErrorT2F <- round(as.numeric(Test$T2F_Diff), 1)
  #     ErrorORD <- round(as.numeric(Test$ORD_Diff), 1)
  #     Wind_Effects <- TestGS %>% mutate(Forecast_Wind_Effect_IAS = End_GS - End_IAS) %>% select(Landing_Pair_ID, End_Dist, Forecast_Wind_Effect_IAS) 
  #     
  #     TestT2FGSPD <- data.frame(Landing_Pair_ID = TestLPID, ACT = TestAC) %>% left_join(TestT2F, by = c("ACT" = "Aircraft_Type")) %>% left_join(Wind_Effects, by = c("Landing_Pair_ID", "Range_To_Threshold"="End_Dist")) %>%
  #       mutate(GSPD = (Average_IAS + Forecast_Wind_Effect_IAS)) %>% mutate(Range_To_Threshold = Range_To_Threshold + 0.5) %>% mutate(Range_To_Threshold = ifelse(Range_To_Threshold == 0.5, 0, Range_To_Threshold))
  #     
  #     a1 <- ggplot() + geom_line(TestGS, mapping = aes(x = End_Dist, y = End_GS)) + geom_point(TestRadar, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD)) + 
  #       xlim(0, dist + 0.5) + geom_vline(xintercept = dist) + labs(x = "Range To Threshold (NM)", y = "Mode S GSPD (kts)", title = paste0("Landing Pair ", TestLPID, ", Follower Aircraft Type ", TestAC, ": ORD TTB"), subtitle = paste0("Surface Headwind: ", TestSW, "kts, Time Delta = ", ErrorORD, "s"))
  #     a2 <- ggplot() + geom_line(TestT2FGSPD, mapping = aes(x = Range_To_Threshold, y = GSPD)) + geom_point(TestRadar, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD)) +
  #       xlim(0, dist + 0.5) + geom_vline(xintercept = dist) + labs(x = "Range To Threshold (NM)", y = "Mode S GSPD (kts)", title = paste0("Landing Pair ", TestLPID, ", Follower Aircraft Type ", TestAC, ": AC Type TTB"), subtitle = paste0("Surface Headwind: ", TestSW, "kts, Time Delta = ", ErrorT2F, "s"))
  #     
  #     png(file.path("Example Compare Radar", paste0(TestLPID, " ", minhw, ".png")))
  #     grid.arrange(a1, a2)
  #     dev.off()
  #     
  #   }
  #   
  # }
  # 
  
PM_TTBSum <- PM_TTBFilt %>% mutate(Large_New = ifelse(abs(T2F_Diff) > 10, 1, 0), Large_Old = ifelse(abs(ORD_Diff) > 10, 1, 0))
t1 <- PM_TTBSum %>% group_by(Surface_Headwind_Group) %>% summarise(Count = n(), Large_Errors_New = sum(Large_New, na.rm=T), Large_Errors_Old = sum(Large_Old, na.rm=T))
t0 <- PM_TTBSum %>% summarise(Count = n(), Large_Errors_New = sum(Large_New, na.rm=T), Large_Errors_Old = sum(Large_Old, na.rm=T))
}

if (Mode == "Profile Analysis"){
  
  OutputFolder <- file.path(GetSaveDirectory(3, "ORD", "Outputs"), "Speed Profiles", "2021-07-16 V1.0 (AH)")
  
  Times <- dbGetQuery(con, "SELECT LP.Landing_Pair_ID, Follower_Flight_Plan_ID, Prediction_Time FROM tbl_Landing_Pair LP INNER JOIN tbl_ORD_Prediction OP ON LP.Landing_Pair_ID = OP.LAnding_Pair_ID")
  Gust <- dbGetQuery(con, "SELECT Gust_Date, Gust_Time, Runway, Gust FROM tbl_Gusting")
  QNH <- dbGetQuery(con, "SELECT Baro_Date, Baro_Time, Baro_Pressure FROM tbl_Baro") %>% mutate(Baro_Pressure_Diff = (Baro_Pressure-101325) / 100)
  RefDists <- dbGetQuery(con, "SELECT Landing_Pair_ID, Ref_Recat_Wake_Separation_Distance, Observed_Follower_eTBS_Wind_Effect FROM tbl_eTBS_Performance_Model")
  RWs <- dbGetQuery(con, "SELECT Runway_Name FROM tbl_Runway")$Runway_Name
  
  Gust <- mutate(Gust, Gust_Date2 = paste0(substr(Gust_Date, 1, 6), substr(Gust_Date, 9, 10))) %>% mutate(Gust_Date = Gust_Date2) %>% select(-Gust_Date2)
  
  data <- fread(file.path(OutputFolder, "Approach_Speed_Profiles.csv")) %>% mutate(SHW_Group = cut(Surface_Headwind, breaks = c(-Inf, -15, -10, -5, 5, 10, 15, Inf)))
  
  data <- data %>%
    left_join(Times, by = "Follower_Flight_Plan_ID") %>%
    left_join(RefDists, by = c("Landing_Pair_ID")) %>%
    rolling_join(Gust, c("FP_Date", "Landing_Runway", "Prediction_Time"), c("Gust_Date", "Runway", "Gust_Time"), Roll = "nearest") %>%
    rolling_join(QNH, c("FP_Date", "Prediction_Time"), c("Baro_Date", "Baro_Time"),  Roll = "nearest")
  
  #data1 <- filter(data, Follower_Aircraft_Type %in% c("A388", "B744", "A320", "A319"))# %>% filter(abs(n1) < 7 & abs(n2) < 14)
  #ggplot(data1) + geom_boxplot(mapping = aes(x = as.factor(Follower_Aircraft_Type), y = n1)) + ylim(0, 5)
  #data1 <- filter(data, abs(Surface_Headwind) <= 15)
  data1 <- data %>% mutate(newd = (b-a1/n2-n1))
  
  stats <- data1 %>% group_by(Follower_Aircraft_Type) %>% summarise(count = n(), mean_n1 = median(n1, na.rm=T), mean_n2 = median(n2, na.rm=T), mean_d = median(n2-n1, na.rm=T)) %>%
    ungroup() %>% filter(count > 1000)
  
  # ex <- filter(data1, Follower_Aircraft_Type %in% c("A388", "B744", "A320", "A319")) #%>% filter(SHW_Group != "(-15,-10]")
  # ggplot(ex,mapping = aes(x = Surface_Headwind, y = n1)) + geom_point(alpha = 0.05) + ylim(2, 4) + facet_wrap(~Follower_Aircraft_Type) + geom_smooth(method = loess)
  # ggplot(data1,mapping = aes(x = SHW_Group, y = -newd)) + geom_boxplot() + ylim(-140, -110) + geom_smooth(method = lm) + facet_wrap(~lss_type)
  # ggplot(data1,mapping = aes(x = SHW_Group, y = n1)) + geom_boxplot() + ylim(2, 5) + geom_smooth(method = lm) + facet_wrap(~lss_type)
  # 
  data1 <- data1 %>% filter(a1 < 200 & a1 > 80 & n1 < 5 & n1 > 1) %>% mutate(Operator = substr(Follower_Callsign, 1, 3)) %>% mutate(Dummy = 1)
  
  list <- c()
  ACTs <- unique(stats$Follower_Aircraft_Type) 
  
  for (j in 1:length(ACTs)){
    ACT <- ACTs[j]
    Use_Gust <- T
    Use_SHW <- T
    Use_QNH <- T
    Use_OP <- T
    
    ex <- filter(data1, Follower_Aircraft_Type == ACT)
    
    str <- "n1 ~ Dummy"
    if (Use_Gust){str <- paste0(str, " + Gust")}
    if (Use_SHW){str <- paste0(str, " + Surface_Headwind")}
    if (Use_QNH){str <- paste0(str, " + Baro_Pressure_Diff")}
    
    if (Use_OP){
      l <- c()
      for (op in unique(ex$Operator)){
        l <- append(l, op)
        ex <- mutate(ex, !!sym(op) := ifelse(Operator == op, 1, 0))
      }
      
      for (i in l){
        if (i != 1){str <- paste0(str, " + `", i, "`")}
      }
    }
    
    model <- lm(data = ex, formula = as.formula(str))
    f <- summary(model)
    c <- data.frame(f$coefficients)
    print(paste0(ACT, " R2: ", f$adj.r.squared))
    list <- append(list, round(f$adj.r.squared, 3))
  }
  
  df <- data.frame(AC = ACTs, R2 = list)
  
#}

#df_new <- df

df <- select(df, -AC)

df_new <- cbind(df_new, df)





