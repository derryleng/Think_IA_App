# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# IA Performance Model Processing
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v1.0
#
# Authors: George Clark
# 
# Description: Script to Model Overall IA Performance. Contains Processing elements to process sample bolstered/prepared data.
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


# --------------------------------------------------------------------------------- #
# Separation Accuracy Adjustments for "Actual" Separations
# --------------------------------------------------------------------------------- #



# --------------------------------------------------------------------------------- #
# Time Separation Processing
# --------------------------------------------------------------------------------- #

# TEMP: Manually add 4DME Leader Local Stabilisation Threshold (FAF) for Actual Time Separations
Performance_Model <- mutate(Performance_Model, Leader_Local_Stabilisation_Threshold = 4)

# Re-define the Max DME Gap based on settings (Not used)
if (Use_Max_DME_Gap & All_Segs_Required){
  Max_DME_Gap <- Max_DME_Gap
} else {Max_DME_Gap <- 1}

# Load in / Construct Scenarios
Scenarios_Path <- file.path(Inp_Dir, "Time_Spacing_Scenarios.csv")
if (file.exists(Scenarios_Path)){
  Scenarios <- fread(Scenarios_Path)
} else {
  Scenarios <- data.frame(
    Time_Spacing_Variable = Separation_Time,
    Distance_Spacing_Variable = Separation_Distance,
    Spacing_Type = Separation_Type,
    Delivery_Point = Separation_Delivery,
    Under_Separation = Separation_Under
  )
}

# Loop through all Scenarios
for (i in 1:nrow(Scenarios)){
  
  # Define Scenario Variables
  Time <- as.character(Scenarios$Time_Spacing_Variable[i])
  Distance <- as.character(Scenarios$Distance_Spacing_Variable[i])
  Spacing <- as.character(Scenarios$Spacing_Type[i])
  Delivery <- as.numeric(Scenarios$Delivery_Point[i])
  Under_Separation <- as.numeric(Scenarios$Under_Separation[i])
  
  # Print Scenario Start Message
  Time_Before <- Sys.time()
  Message_Text <- paste0("Calculating Time Spacing Variable ", Time, " as a(n) ", Spacing, " Time Separation, Delivered to ", Delivery, "DME, With Separation Distance ", Distance, ".")
  if (Under_Separation != 0){Message_Text <- paste0(Message_Text, " (", Under_Separation, "NM Under-Separated)")}
  message(Message_Text)
  
  # Complete Time Separation Calculations.
  Performance_Model <- Calculate_PM_Time_Separation(Performance_Model, Segments, Grouping_Type, Seg_Size, 
                                                    Time, Distance, Leader_Start_Var, Spacing, Delivery, Under_Separation,
                                                    All_Segs_Required)
  # Print Scenario Finish Message
  Time_After <- Sys.time()
  message(paste0("Completed in ", round(Time_After - Time_Before, 2), " seconds."))
  
}

# Analysis & Output ######################################################################################

# --------------------------------------------------------------------------------- #
# Extra Analysis Fields
# --------------------------------------------------------------------------------- #



# --------------------------------------------------------------------------------- #
# Summary Tables
# --------------------------------------------------------------------------------- #


# --------------------------------------------------------------------------------- #
# Plot Outputs
# --------------------------------------------------------------------------------- #

Plot_Dir <- file.path(Out_Dir, "Plots")
Create_Directory(Plot_Dir)

PlotAgainstReference <- function(Data, Reference, RefDists, PlotVar, RecatorLegacy, LeaderWTC, FollowerWTC, Unit, Algo, Colour){
  
  # String for Title
  if (Unit == "IAS"){
    Unit <- "IAS (kts)"
    PlotTitle <- "Follower IAS"
    }
  
  if (Unit == "Time"){
    Unit <- "Time (s)"
    if (Algo == "PM"){
      PlotTitle <- "Time Separations"
    }
    if (Algo == "TBSC"){
      PlotTitle <- "Flying Times"
    }
  }
  
  # Get variable names.
  Leader_Var <- paste0("Leader_", RecatorLegacy, "_Wake_Cat")
  Foll_Var <- paste0("Follower_", RecatorLegacy, "_Wake_Cat")
  
  # Filter Data for WTCs of interest
  Data <- filter(Data, !!sym(Leader_Var) == LeaderWTC & !!sym(Foll_Var) == FollowerWTC)
  RefVar <- as.numeric(filter(Reference, Leader_WTC == LeaderWTC, Follower_WTC == FollowerWTC))
  RefDist <- as.numeric(filter(RefDists, Leader_WTC == LeaderWTC, Follower_WTC == FollowerWTC)$Reference_Wake_Separation_Distance)
  String <- paste0(PlotTitle, " for pair ", LeaderWTC, "-", FollowerWTC, " (", RefDist, "NM)")
  
  # Initialise Histogram plot
  Plot <- ggplot(Data) + geom_histogram(mapping = aes(x = !!sym(PlotVar), y = ..density..), binwidth = 2, fill = Colour) + geom_vline(xintercept = RefVar) + 
    labs(x = Unit, y = "Density", title = String, subtitle = PlotVar)
  
  return(Plot)
  
}

PlotTimeSeparationAgainstReference <- function(PM, RefTimes, RefDists, TimeVar, RecatorLegacy, LeaderWTC, FollowerWTC){
  
  Plot <- PlotAgainstReference(PM, RefTimes, RefDists, PlotVar = TimeVar, RecatorLegacy, LeaderWTC, FollowerWTC, Unit = "Time", Algo = "PM", Colour = "magenta")
  Plot <- Plot + xlim(40, 200)
  
  return(Plot)
  
}

PlotFollowerIASAgainstReference <- function(Data, RefSpeeds, RefDists, SpeedVar, RecatorLegacy, LeaderWTC, FollowerWTC){
  
  Plot <- PlotAgainstReference(PM, RefSpeeds, RefDists, PlotVar = SpeedVar, RecatorLegacy, LeaderWTC, FollowerWTC, Unit = "IAS", Algo = "PM", Colour = "green")
  Plot <- Plot + xlim(80, 240)
  
  return(Plot)
  
}

## Loop across all Wake Distances
for (i in 1:nrow(Recat_Wake_Time)){

   LeaderWTC <- Recat_Wake_Time[i,]$Leader_WTC
   FollowerWTC <- Recat_Wake_Time[i,]$Follower_WTC
   Path <- file.path(Plot_Dir, paste0(LeaderWTC, "-", FollowerWTC))
   Create_Directory(Path)

   for (j in 1:length(Separation_Time)){
     Plot <- PlotTimeSeparationAgainstReference(Performance_Model, Recat_Wake_Time, Recat_Wake_Dist, Separation_Time[j], "Recat", LeaderWTC, FollowerWTC)
     png(file.path(Path, paste0(LeaderWTC, "-", FollowerWTC, " ", Separation_Time[j], ".png")))
     print(Plot)
     dev.off()
   }
   
   ## TEMP Comparisons 
   
   # 1. 1DME TBS v DBS
   Plot1 <- PlotTimeSeparationAgainstReference(Performance_Model, Recat_Wake_Time, Recat_Wake_Dist, "Perfect_1DME_Wake_Separation_Time_TBS", "Recat", LeaderWTC, FollowerWTC)
   Plot2 <- PlotTimeSeparationAgainstReference(Performance_Model, Recat_Wake_Time, Recat_Wake_Dist, "Perfect_1DME_Wake_Separation_Time_DBS", "Recat", LeaderWTC, FollowerWTC)
   Plot3 <- PlotTimeSeparationAgainstReference(Performance_Model, Recat_Wake_Time, Recat_Wake_Dist, "Perfect_1DME_Wake_Separation_Time_TBS_US05", "Recat", LeaderWTC, FollowerWTC)
   Plot4 <- PlotTimeSeparationAgainstReference(Performance_Model, Recat_Wake_Time, Recat_Wake_Dist, "Perfect_0DME_Wake_Separation_Time_TBS", "Recat", LeaderWTC, FollowerWTC)
   
   png(file.path(Path, paste0(LeaderWTC, "-", FollowerWTC, " TBS v DBS 1DME.png")))
   grid.arrange(Plot1, Plot2)
   dev.off()
   
   png(file.path(Path, paste0(LeaderWTC, "-", FollowerWTC, " TBS v US TBS 1DME.png")))
   grid.arrange(Plot1, Plot3)
   dev.off()
   
   png(file.path(Path, paste0(LeaderWTC, "-", FollowerWTC, "  TBS 1DME v 0DME.png")))
   grid.arrange(Plot1, Plot4)
   dev.off()
   
}

# --------------------------------------------------------------------------------- #
# WaPT Output
# --------------------------------------------------------------------------------- #

Include_Observed_Values_WaPT <- F

WaPT_Columns <- c("FP_Date",
         "Leader_Callsign",
         "Leader_Aircraft_Type",
         "Leader_Legacy_Wake_Cat",
         "Leader_Recat_Wake_Cat",
         "Follower_Callsign",
         "Follower_Aircraft_Type",
         "Follower_Legacy_Wake_Cat",
         "Follower_Recat_Wake_Cat",
         "Ref_Legacy_Wake_Separation_Distance",
         "Ref_Recat_Wake_Separation_Distance",
         "Ref_Recat_Wake_Separation_Time",
         "Follower_Ass_Recat_Separation_IAS")

if (Include_Observed_Values_WaPT){
  WaPT_Columns <- append(WaPT_Columns,
                         c("Observed_AGI_Surface_Headwind",
                           "Observed_AGI_Surface_Wind_SPD",
                           "Observed_AGI_Surface_Wind_HDG",
                           "Observed_0DME_Separation_Distance",
                           "Observed_1DME_Separation_Distance",
                           "Observed_0DME_Separation_Time",
                           "Observed_1DME_Separation_Time",
                           "Observed_Follower_eTBS_Wind_Effect"))
}

WaPT_Columns <- append(WaPT_Columns, unique(Separation_Distance)) %>%
  append(unique(Separation_Time))

Performance_Model_WaPT <- select(Performance_Model, all_of(WaPT_Columns))
fwrite(Performance_Model_WaPT, file.path(Out_Dir, "Performance_Model_WaPT.csv"))

### BELOW IS ORIGINAL WAPT OUTPUT FOR REFERENCE
#   pm3[,c(
#     "Date",
#     "Leader_Callsign",
#     "Leader_Aircraft_Type",
#     "Leader_UK_Wake_Cat",
#     "Leader_Recat_Wake_Cat",
#     "Follower_Callsign",
#     "Follower_Aircraft_Type",
#     "Follower_UK_Wake_Cat",
#     "Follower_Recat_Wake_Cat",
#     #Landing runway
#     "UK6Cat_DBS_4DME_Wake_Separation_Distance",
#     "Recat_DBS_1DME_Wake_Separation_Distance",
#     "Ref_UK6Cat_Separation_Time",
#     "Ref_Recat_Wake_Separation_Time",
#     "Follower_Forecast_TBS_Wind_Effect",
#     "Follower_Forecast_eTBS_Wind_Effect",
#     #RWY_HDG
#     #Observed_AGI_Surface_Crosswind
#     "Observed_AGI_Surface_Headwind",
#     "Observed_AGI_Surface_Wind_SPD",
#     "Observed_AGI_Surface_Wind_HDG",
#     "Observed_1DME_Separation_Distance",
#     "Observed_1DME_Separation_Distance",
#     "Observed_1DME_Separation_Time",
#     "Observed_4DME_Separation_Time",
#     "Observed_Follower_TBS_Wind_Effect",
#     "Observed_Follower_eTBS_Wind_Effect",
#     "UK6Cat_TBS_4DME_Wake_Separation_Distance",
#     "Observed_4DME_Separation_Accuracy",
#     "Recat_eTBS_0DME_Wake_Separation_Distance",
#     "Forecast_ORD_TBS_Compression",
#     "Recat_eTBS_4DME_Wake_Separation_Distance",
#     "Ave_Mode_S_GSPD",
#     "Ave_Mode_S_IAS",
#     "eTBS_actual_4DME_Distance_Separation",
#     "eTBS_actual_1DME_Distance_Separation",
#     "eTBS_perfect_1DME_Time_Separation",
#     "eTBS_actual_4DME_Time_Separation",
#     "eTBS_actual_1DME_Time_Separation",
#     "UK6Cat_TBS_1DME_Time_Separation",
#     "RECAT_DBS_1DME_Time_Separation",
#     "UK6Cat_DBS_1DME_Time_Separation",
#     "Under_Separated_0.5_eTBS_perfect_1DME_Time_Separation",
#     "Under_Separated_0.5_UK6Cat_TBS_1DME_Time_Separation",
#     "Under_Separated_0.5_RECAT_DBS_1DME_Time_Separation",
#     "Under_Separated_0.5_UK6Cat_DBS_1DME_Time_Separation",
#     "Under_Separated_1.0_eTBS_perfect_1DME_Time_Separation",
#     "Under_Separated_1.0_UK6Cat_TBS_1DME_Time_Separation",
#     "Under_Separated_1.0_RECAT_DBS_1DME_Time_Separation",
#     "Under_Separated_1.0_UK6Cat_DBS_1DME_Time_Separation"
#   )]
# )

# --------------------------------------------------------------------------------- #
# Temporary Analysis
# --------------------------------------------------------------------------------- #


