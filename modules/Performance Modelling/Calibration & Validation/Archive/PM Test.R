# ----------------------------------------------------------------------- #
#                 |                                                       #
#  Title          |  eTBS Performance Analysis                            #
#                 |                                                       #
#  Author         |  Derry Leng, Katherine Cliff, Michael Cowham          #
#                 |                                                       #
#  Date Modified  |  22/07/2020                                           #
#                 |                                                       #
#  Description    |  To model and describe eTBS performance               #
#                 |                                                       #
# ----------------------------------------------------------------------- #
#
# Version History ------------------------------------------------------- #
#
#
# 5.2 (IA-155 Updates)
# Created Settings section
# New setting for radar minimum
# Added option to not run code related to UK 6 CAT
#
# 5.1
# Setting up script to work with LVNL data
# Added database Connection and new input files
# Will now remove flights with missing segment information
# Adjusted code to account for LVNL Wake category values
# Created new separation scheme
# Added readme.txt file creation
#
# 5.0
# Merged eTBS Performance Analysis v4.R with eTBS Calculations MCv1_2.R
#   and adapted to support new airports
# Changed reference data Ave_GSPD to use Ave_Mode_S_GSPD
# Fixed bad rounding in Observed_4DME_Separation_Accuracy_New calculation
# Removed runway heading and crosswind calculations
# Removed references to column names by index
# Updated section 3.3 to use officer for Word document generation
# General optimisations
#
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# i. Imports ------------------------------------------------------------
# ----------------------------------------------------------------------- #

library(data.table)
library(dplyr)
library(officer)
library(ggplot2)
library(gridExtra)
library(RODBC)

# ----------------------------------------------------------------------- #
# ii. Settings ----------------------------------------------------------
# ----------------------------------------------------------------------- #

readme_txt <- "IA Performance Analysis Script Run
Script version: v5.2
Sprint task: IA-155
User: George
Database Used: LVNL_UTMA_Validation
Notes: Full dataset run for use addressing issues in IA-155
"

# Use database or local copy of vw_eTBS_Performance_Model and Segment Data
use_database <- F

# Local dataset - WHEN use_database IS FALSE
# NOTE: local_data_folder is name of folder containing pm.csv & rd.csv
#       e.g. ..\7. TBS Performance Model\Reference\"local_data_folder"\..

local_data_folder <- "LVNL Exports 2020-09-24"

# Database dataset - WHEN use_database IS TRUE
db_ip <- "192.168.1.32"
db_name <- "LVNL_UTMA_Validation"

# NOTE: For date_filter, specify date(s) to query - NA for all dates
date_filter <- NA
# date_filter <- "01/10/2019"

# Only set True if running files for the first time since database run. Saves segments to local file.
save_segs <- F
save_pm <- F

### CHECK WHERE ROUNDING IS DONE IN HEATHROW ###

# ----------------------------------------------------------------------- #
# iii. Settings ---------------------------------------------------------
# ----------------------------------------------------------------------- #

# Minimum TBS Wake separation cap at 0DME and 4DME (NM)
wake_cap_0 <- 3
wake_cap_4 <- 3

# Minimum TBS ROT Spacing cap at 0DME and 4DME (NM)
rot_cap_0 <- 3
rot_cap_4 <- 3

# Radar minimum separation distance (NM)
radar_min_sep <- 3

# Maximum DME_Seg limit for segment flying time calculation
DME_Seg_Max <- 20

# Only use Wake Pairs
only_wake_pairs <- T

# Delivery Options: Delivery distances for Actual Separation

All_Actual <- 4
A388_Actual <- 5
A388_Adj <- F

# ------------- Separation Accuracy Adjustment -------------------------- #

# Heathrow Mean (1) and Standard deviation (2) 4DME Separation Accuracy
mu_h <- 0.55
sigma_h <- 0.46

# Maximum allowed ICAO 0DME Separation accuracy for filtered data (only used for adjustment)
icao_max_sep_acc <- 3

# Percent of observations under 0 Separation accuracy for Heathrow
pc_under <- 0.04

# Maximum permissable under-separation
lambda <- -0.54

# Method for calculating adjusted separation accuracy
# 1 = Use Mean, 2 = Use Quantiles
sep_adj_method <- 2

# ----------------------------------------------------------------------- #
# 0. Settings -------------------------------------------------------------
# ----------------------------------------------------------------------- #

# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Generate sub-directory in Output folder for this run
outdir <- file.path("Output", paste0("Run_", gsub(" ", "_", gsub("-|:", "", as.character(Sys.time())))))
dir.create(outdir)

# Write a readme file for this run
sink(file.path(outdir, "readme.txt"))
cat(readme_txt)
sink()

if (!use_database) {
  
  # Local performance model table directory:
  pm_dir <- file.path("Reference", local_data_folder, "pm.csv")
  
  # Local reference data table directory:
  seg_dir <- file.path("Reference", local_data_folder, "pm_segment_data.csv")
  
}

# Get Connection
con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, db_name, "ruser", "Th!nkruser"
))

# ----------------------------------------------------------------------- #
# Functions
# ----------------------------------------------------------------------- #

# Get the flying time between 2 segments
segmentFlyingTime <- function(df, start, end, units = "h") {
  # df: must be a data.frame/data.table containing two columns,
  #     with first column containing DME segment number,
  #     and second column containing average speed in segments.
  #
  # start: segment to start calculation of flying time.
  #
  # end: segment to end calculation of flying time.
  #
  # units: (use either h, m or s) the units in which the
  #        calculated flying time is returned as.
  #
  # NOTES
  # - Segment number should refer to the lower integer bound of
  #   the segment, e.g. segment number 3 refers to the segment
  #   between 3DME and 4DME.
  # - Start and end segments are right inclusive, i.e. start = 1
  #   and end = 6 will calculate flying time between 1 and 7DME,
  #   and start = 3, end = 3 will calculate between 3 and 4DME.
  if (!is.na(start)){
  if (start < end) {
    message("\nWarning: The start segment number must be greater than end segment, returning 0.\n")
    return(0)
  }
  names(df)[c(1,2)] <- c("V1", "V2")
  segs <- seq(end, start, 1)
  missing_segs <- setdiff(segs, df$V1)
  if (length(missing_segs) > 0) {
    message("\nWarning: Missing segment information for segments ", paste0(missing_segs, collapse = ", "), "\n")
    return(NA)
  }
  return(sum(1/df[df$V1 %in% segs,]$V2, na.rm = T))
  }
  else{return(NA)}
}

# Mike's generic plotting function
plot_time_histogram <- function(data, plot_title, line_value, fill_colour){
  
  if (data$Leader_Recat_Wake_Cat[1] == "A"){ x_lim = c(40, 220)} else {x_lim = c(40, 180)}
  
  plot_object <-ggplot(data, aes(plot_data, y=..density..) ) +
    geom_histogram(breaks = seq(0,200,5), na.rm = TRUE, fill = fill_colour, col = "black")+
    xlab("1DME Time SepN(s)")+
    scale_colour_brewer(palette = "Set1")+
    #   xlim(c(40,180))+
    scale_x_continuous(breaks = seq(40, 220, 10), limits = x_lim)+
    theme_bw()+
    geom_vline(xintercept = line_value)+
    theme(legend.position = "bottom", legend.title=element_blank())+
    ggtitle(plot_title)
  
  return(plot_object)
}

plot_time_histogram_2 <- function(data, plot_title, fill_colour){
  if (data$Leader_Recat_Wake_Cat[1] == "A"){ x_lim = c(40, 220)} else {x_lim = c(40, 180)}
  plot_object <-ggplot(data, aes(plot_data, y=..density..) ) +
    geom_histogram(breaks = seq(0,200,5), na.rm = TRUE, fill = fill_colour, col = "black")+
    xlab("1DME Time SepN(s)")+
    scale_colour_brewer(palette = "Set1")+
    #   xlim(c(40,180))+
    scale_x_continuous(breaks = seq(40, 220, 10), limits = x_lim)+
    theme_bw()+
    theme(legend.position = "bottom", legend.title=element_blank())+
    ggtitle(plot_title)
  
  return(plot_object)
}

# Get a Perfect Time Spacing
Get_Perfect_Time_Spacing <- function(Follower_Segs, Delivery, Separation_Distance, Under_Sep){
  Separation_Distance <- Separation_Distance + Delivery - 1
  TF <- segmentFlyingTime(Follower_Segs, floor(Separation_Distance - Under_Sep), Delivery) # Get the Full Portion of Follower Flying Time
  if (is.na(TF)){return(NA)}
  if ((TF > 0 & ceiling(Separation_Distance - Under_Sep) %in% Follower_Segs$DME_Seg) == T){
    if (Separation_Distance %% 1 == 0){return(TF*3600)}
  Extra_Dist <- (Separation_Distance - Under_Sep) - floor(Separation_Distance - Under_Sep) # Get the Extra Follower Distance
  GSPD_Index <- ceiling(Separation_Distance - Under_Sep)
  Extra_GSPD <- Follower_Segs[Follower_Segs$DME_Seg == GSPD_Index]$Follower_Average_GSPD # Get the GSPD From this segment
  Extra_Time <- Extra_Dist/Extra_GSPD # Get the Partial Extra Follower Flying Time
  return((TF + Extra_Time)*3600)
  } else {return(NA)}
}

# Get an actual time spacing
Get_Actual_Time_Spacing <- function(Follower_Segs, Leader_Segs, Leader_Aircraft, Delivery, Separation_Distance, Under_Sep){
  if (Leader_Aircraft == "A388" & A388_Adj == T){Leader_Start <- (A388_Actual - 1 - Under_Sep)} 
  else {Leader_Start <- (All_Actual - 1 - Under_Sep)}
  Follower_Start <- Leader_Start + Separation_Distance
  TL <- segmentFlyingTime(Leader_Segs, Leader_Start, Delivery)
  TF <- segmentFlyingTime(Follower_Segs, floor(Follower_Start), Delivery) # Get the Full Portion of Follower Flying Time
  if (is.na(TF)){return(NA)}
  if (is.na(TL)){return(NA)}
  if (Separation_Distance %% 1 == 0){return((TF - TL)*3600)}
  if ((TF > 0 & TL > 0 & ceiling(Follower_Start) %in% Follower_Segs$DME_Seg) == T){
  Extra_Dist <- (Separation_Distance - Under_Sep) - floor(Separation_Distance - Under_Sep) # Get the Extra Follower Distance
  GSPD_Index <- ceiling(Follower_Start)
  Extra_GSPD <- Follower_Segs[Follower_Segs$DME_Seg == GSPD_Index,]$Follower_Average_GSPD # Get the GSPD From this segment
  Extra_Time <- Extra_Dist/Extra_GSPD # Get the Partial Extra Follower Flying Time
  return((TF + Extra_Time - TL)*3600)
  } else {return(NA)}
}


TF <- segmentFlyingTime(follower_segs, pm_i$Actual_4DME_All_Separation_Distance, 1) 
TL <- segmentFlyingTime(leader_segs, 3,1)
Follower_Start <- D_E4
Follower_Segs <- follower_segs

# ----------------------------------------------------------------------- #
# 1. Import Data
# ----------------------------------------------------------------------- #

# ------------------------------- Reference ----------------------------- #

# 1.1. Get the ICAO4 Wake category pair distances
dbs_wake_turbulence <- sqlQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence")

# 1.2 Get the ICAO4 Aircraft type to wake mapping
aircraft_type_to_wake_legacy <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy") %>% unique()

# 1.3 Get the Schiphol Separation Adjustment parameters
sep_adjust <- fread(file.path("Input", "separation_adjustment_parameters_v2.csv"))

# ----------------------------------------------------------------------- #

# -------------------------- Performance Model -------------------------- #

# 1.4 Get the Performance Model from SQL
# Note: We want to add Leader_Flight_Plan_ID, Follower_Flight_Plan_ID, Leader Runway, Follower Runway to PM View

pm_query_str <- "SELECT * FROM vw_eTBS_Performance_Model"

tryCatch(
if (use_database) {
  if (!is.na(date_filter)) {
    pm <- sqlQuery(con, sprintf("%s WHERE FP_Date IN ('%s')", pm_query_str, paste(date_filter, collapse = "','")))
  } else {
    pm <- sqlQuery(con, pm_query_str)
  }
} else {
  pm <- fread(pm_dir)
},
error = function(e) stop("Performance Model not found!"))

# Set up Original copy of performance model and save if necessary
pm_orig <- pm

if (save_pm) {
  fwrite(pm_orig, file.path(pm_dir))
}


# ----------------------------------------------------------------------- #

# -------------------------- Segment Data ------------------------------- #

# Get the Segment Data from SQL - Query

Segment_Data_SQL <- paste0("SELECT

Landing_Pair_ID,
Landing_Pair_Type,
Landing_Pair_Date,
DME_Seg = LWS.DME_Seg / dbo.fnc_GI_Nm_To_M() ,
Leader_Flight_Plan_ID,
Leader_Wind_Segment_ID = LWS.Mode_S_Wind_Seg_ID,
Leader_Average_IAS = LWS.Ave_Mode_S_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec() ,
Leader_Average_GSPD = LWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec(),
--Leader_Flying_Time = 3600/(LWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec()),
Leader_Average_Wind_Effect = LWS.Ave_Wind_Effect_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec(),
Leader_Global_Flag = LWSF.Global_Flag,
Follower_Flight_Plan_ID,
Follower_Average_IAS = FWS.Ave_Mode_S_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec(),
Follower_Average_GSPD = FWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec(),
--Follower_Flying_Time = 3600/(FWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec()),
Follower_Average_Wind_Effect = FWS.Ave_Wind_Effect_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec(),
Follower_Global_Flag = FWSF.Global_Flag

FROM tbl_Landing_Pair LP

INNER JOIN tbl_Mode_S_Wind_Seg LWS
ON LP.Leader_Flight_Plan_ID = LWS.Flight_Plan_ID

INNER JOIN tbl_Mode_S_Wind_Seg_Flags LWSF
ON LWS.Mode_S_Wind_Seg_ID = LWSF.Mode_S_Wind_Seg_ID

INNER JOIN tbl_Mode_S_Wind_Seg FWS
ON LP.Follower_Flight_Plan_ID = FWS.Flight_Plan_ID AND LWS.DME_Seg = FWS.DME_Seg

INNER JOIN tbl_Mode_S_Wind_Seg_Flags FWSF
ON FWS.Mode_S_Wind_Seg_ID = FWSF.Mode_S_Wind_Seg_ID

ORDER BY Landing_Pair_ID, DME_Seg")

# Load the Data

tryCatch(
  if (use_database == T) {
    if (exists("con")) {
      test_segs <- sqlQuery(con, Segment_Data_SQL)
    }
  } else {
    test_segs <- fread(file.path(seg_dir))
  },
  error = function(e) stop("Segments not found!")
)

# Write the segment data if initially ran from SQL
if (save_segs == T){fwrite(test_segs, file.path(seg_dir))}

# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# Initial Data Cleaning
# ----------------------------------------------------------------------- #

# Filter out segments which do not pass Stage 2 criteria

# !!! MAKE A NOTE OF SPECIFIC FLAGS WE WANT TO INCLUDE IN SQL QUERY!!!!
# NONE maybe add Track Time Filter

# Reload the original Performance Model before being pre-processed
pm <- pm_orig

# Filter out data that have no wind segments
pm <- filter(pm, Landing_Pair_ID %in% unique(test_segs$Landing_Pair_ID))

# Remove all SQL columns that have no relevance
pm <- select(pm, -c("Leader_UK_Wake_Cat", "Follower_UK_Wake_Cat", "UK6Cat_Separation_Distance", "UK6Cat_Separation_Time",
                    "Follower_Forecast_TBS_Wind_Effect", "UK6Cat_TBS_4DME_Wake_Separation_Distance", "Forecast_ORD_eTBS_Compression", 
                    "Observed_4DME_Separation_Accuracy"))

# Remove NA values where there is no forecast compression
pre_pm_nrow <- nrow(pm)
pm <- pm[!is.na(pm$Forecast_ORD_TBS_Compression),]
post_pm_nrow <- nrow(pm)
message("Removed ", pre_pm_nrow - post_pm_nrow, " out of ", pre_pm_nrow, " performance model rows with NA Forecast_ORD_TBS_Compression.")

# Remove all negative Observed 1DME Separation Time - go arounds
pre_pm_nrow <- nrow(pm)
pm <- pm[pm$Observed_1DME_Separation_Time > 0,]
post_pm_nrow <- nrow(pm)
message("Removed ", pre_pm_nrow - post_pm_nrow, " out of ", pre_pm_nrow, " performance model rows with Observed_1DME_Separation_Time <= 0.")

# Remove all non-wake pairs if only_wake_pairs is true
if (only_wake_pairs == T){
pre_pm_nrow <- nrow(pm)
pm <- pm[!is.na(pm$Ref_Recat_Wake_Separation_Distance),]
post_pm_nrow <- nrow(pm)
message("Removed ", pre_pm_nrow - post_pm_nrow, " out of ", pre_pm_nrow, " performance model rows with NA Ref_Recat_Wake_Separation_Distance")
}



# SOME MORE FILTERS HERE

# ----------------------------------------------------------------------- #
# Get the rest of the inputs (This should all be done in SQL!)
# ----------------------------------------------------------------------- #

# Add ICAO4 wake categories
pm <- left_join(pm, aircraft_type_to_wake_legacy, by=c("Leader_Aircraft_Type"="Aircraft_Type")) %>% rename(Leader_ICAO_Wake_Cat = Wake)
pm <- left_join(pm, aircraft_type_to_wake_legacy, by=c("Follower_Aircraft_Type"="Aircraft_Type")) %>% rename(Follower_ICAO_Wake_Cat = Wake)
pm$LF_Pair_ICAO4 <- paste0(pm$Leader_ICAO_Wake_Cat, "-", pm$Follower_ICAO_Wake_Cat)

# Update ICAO4 wake separation distances (with 3NM MRS)
dbs_join <- mutate(dbs_wake_turbulence, LF_Pair_ICAO4 = paste0(Leader_WVI, "-", Follower_WVI)) %>% select(-c("Leader_WVI", "Follower_WVI"))
pm <- left_join(pm, dbs_join, by=c("LF_Pair_ICAO4")) %>% rename(ICAO_DBS_Wake_Separation_Distance = WT_Separation_Distance)
pm <- mutate(pm, 
             ICAO_DBS_Wake_Separation_Distance = ICAO_DBS_Wake_Separation_Distance/1852,
             ICAO_DBS_All_Separation_Distance = ifelse(is.na(ICAO_DBS_Wake_Separation_Distance), radar_min_sep, ICAO_DBS_Wake_Separation_Distance))
rm(dbs_join)

# Get ICAO4 Separation Accuracies for 0DME, 1DME, 4DME
pm <- mutate(pm,
             ICAO_DBS_0DME_Separation_Accuracy = Observed_0DME_Separation_Distance - ICAO_DBS_All_Separation_Distance,
             ICAO_DBS_1DME_Separation_Accuracy = Observed_1DME_Separation_Distance - ICAO_DBS_All_Separation_Distance,
             ICAO_DBS_4DME_Separation_Accuracy = Observed_4DME_Separation_Distance - ICAO_DBS_All_Separation_Distance)

# Rename Columns for consistency. 0DME TBS distance columns are "uncapped" and will be capped next.
pm <- rename(pm, 
             Recat_DBS_Wake_Separation_Distance = Ref_Recat_Wake_Separation_Distance,
             Recat_DBS_ROT_Spacing_Distance = Ref_ROT_Spacing_Distance,
             Recat_TBS_0DME_Wake_Separation_Distance_Uncapped = Recat_eTBS_0DME_Wake_Separation_Distance,
             Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped = Recat_eTBS_0DME_ROT_Spacing_Distance,
             Recat_TBS_0DME_All_Separation_Distance_Uncapped = Recat_eTBS_0DME_All_Separation_Distance,
             Recat_TBS_4DME_Wake_Separation_Distance_Uncapped = Recat_eTBS_4DME_Wake_Separation_Distance,
             Recat_TBS_4DME_ROT_Spacing_Distance_Uncapped = Recat_eTBS_4DME_ROT_Spacing_Distance,
             Recat_TBS_4DME_All_Separation_Distance_Uncapped = Recat_eTBS_4DME_All_Separation_Distance,
             Forecast_ORD_Compression_Uncapped = Forecast_ORD_TBS_Compression,
             Recat_TBS_Wake_Separation_Time = Ref_Recat_Wake_Separation_Time,
             Recat_TBS_ROT_Spacing_Time = Ref_ROT_Spacing_Time,
             Recat_TBS_Wake_Separation_IAS = Follower_Ass_Recat_Separation_IAS,
             Recat_TBS_ROT_Spacing_IAS = Follower_Ass_ROT_Spacing_IAS
             )

# Cap value of Compression to 0
# Cap values of 0DME Wake, ROT Separation
# Cap values of 0DME All to MRS (Legal)
# Get 4DME values from 0DME + Compression
pm <- mutate(pm, 
             Forecast_ORD_Compression = ifelse(Forecast_ORD_Compression_Uncapped < 0, 0, Forecast_ORD_Compression_Uncapped),
             Recat_TBS_0DME_Wake_Separation_Distance = ifelse(Recat_TBS_0DME_Wake_Separation_Distance_Uncapped < wake_cap_0, wake_cap_0, Recat_TBS_0DME_Wake_Separation_Distance_Uncapped),
             Recat_TBS_0DME_ROT_Spacing_Distance = ifelse(Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped < rot_cap_0, rot_cap_0, Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped),
             Recat_TBS_0DME_All_Separation_Distance = ifelse(Recat_TBS_0DME_All_Separation_Distance_Uncapped < radar_min_sep, radar_min_sep, Recat_TBS_0DME_All_Separation_Distance_Uncapped),
             Recat_TBS_4DME_Wake_Separation_Distance = Recat_TBS_0DME_Wake_Separation_Distance + Forecast_ORD_Compression,
             Recat_TBS_4DME_ROT_Spacing_Distance = Recat_TBS_0DME_ROT_Spacing_Distance + Forecast_ORD_Compression,
             Recat_TBS_4DME_All_Separation_Distance =  Recat_TBS_0DME_All_Separation_Distance + Forecast_ORD_Compression
             )

# Need to adjust 4DME stuff for A388 aircraft as they begin compression at 5DME

# ----------------------------------------------------------------------- #
# Get the assumed delivery separation accuracy at 4DME
# ----------------------------------------------------------------------- 

# Get Schiphol 0DME Separation Accuracy Standard Deviation
pm <- inner_join(pm, sep_adjust, by=c("LF_Pair_ICAO4"))

# Filter PM data to only include pairs with sep accuracy below icao_max_sep_acc
#pm_filt <- filter(pm, ICAO_DBS_0DME_Separation_Accuracy <= icao_max_sep_acc)
pm_filt <- filter(pm, ICAO_DBS_4DME_Separation_Accuracy <= icao_max_sep_acc)

# Get Assumed Delivery Separation Accuracy at 4DME using joined Schiphol Accuracy Mean and Standard Deviation data

#pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy_STD = sigma_h*((ICAO_DBS_4DME_Separation_Accuracy - Mean_Accuracy)/SD_Accuracy)) #USE 0DME (as per Plan)
pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy_STD = sigma_h*((ICAO_DBS_4DME_Separation_Accuracy - Mean_Accuracy)/SD_Accuracy)) #USE 4DME

# Use method from Plan (1) or Mike's method with Quantiles (2)
if (sep_adj_method == 1){
  pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy = mu_h + ICAO_Assumed_4DME_Separation_Accuracy_STD)
} else {
  # Get percentage of Separation accuracy under 0 and find averages per Wake Cat
  pc_under_0 <- pm_filt %>% group_by(Legacy_Wake, LF_Pair_ICAO4)
  #pc_under_0 <- summarise(pc_under_0, Quantile_0 = quantile(ICAO_DBS_4DME_Separation_Accuracy, pc_under)) %>% ungroup() #USE 0DME
  pc_under_0 <- summarise(pc_under_0, Quantile_0 = quantile(ICAO_DBS_4DME_Separation_Accuracy, pc_under)) %>% ungroup() #USE 4DME
  
  # Join to PM data and use as Mean
  pm <- inner_join(pm, pc_under_0, by=c("LF_Pair_ICAO4", "Legacy_Wake"))
  pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy = ICAO_Assumed_4DME_Separation_Accuracy_STD - Quantile_0 - 0.05)
}

# filter out results with a sep accuracy < lambda
pm <- filter(pm, ICAO_Assumed_4DME_Separation_Accuracy > lambda)
#'pm$ICAO_Assumed_4DME_Separation_Accuracy <- ifelse(pm$ICAO_Assumed_4DME_Separation_Accuracy > lambda, pm$ICAO_Assumed_4DME_Separation_Accuracy, lambda)


# ----------------------------------------------------------------------- #
# Get D_E4 and Adjust for A380s
# ----------------------------------------------------------------------- #

# Get D_E4 = Actual_IA_4DME_Wake_Separation
# Get D_E4M = Actual IA 4DME All Separation

pm <- mutate(pm, 
             Actual_4DME_Wake_Separation_Distance = Recat_TBS_4DME_Wake_Separation_Distance + ICAO_Assumed_4DME_Separation_Accuracy,
             Actual_4DME_All_Separation_Distance = Recat_TBS_4DME_All_Separation_Distance + ICAO_Assumed_4DME_Separation_Accuracy)

# Remove some more columns 

pm <- select(pm, -c(
  "Recat_TBS_4DME_All_Separation_Distance_Uncapped"))

# ----------------------- A388 Adjustment ------------------------------- #

png(file.path(outdir, "Obs 4DME ICAO Sep Acc A388 Lead.png"), height = 900, width = 1200)
hist(
  pm[pm$Leader_Aircraft_Type == "A388",]$ICAO_DBS_4DME_Separation_Accuracy,
  breaks = seq(-10, 10, 0.2),
  xlim = c(-10,10),
  prob = T,
  col = "light green",
  main = "Obs. 4DME Sep. Acc. New A388 Lead",
  xlab = "Separation Accuracy (NM)"
)
dev.off()

png(file.path(outdir, "Obs 0DME ICAO Sep Acc A388 Lead.png"), height = 900, width = 1200)
hist(
  pm[pm$Leader_Aircraft_Type == "A388",]$ICAO_DBS_0DME_Separation_Accuracy,
  breaks = seq(-10, 10, 0.2),
  xlim = c(-10,10),
  prob = T,
  col = "light green",
  main = "Obs. 4DME Sep. Acc. New A388 Lead",
  xlab = "Separation Accuracy (NM)"
)
dev.off()

# ----------------------------------------------------------------------- #                                                                

# ----------------------------------------------------------------------- #
# Get Remaining Outputs/Under Separated Outputs
# ----------------------------------------------------------------------- #

## Calculate time separations by match id to raw data and subset, then find time by adding the inverse ground speed of each segment ##
#no_of_iterations <- nrow(pm)
no_of_iterations <- 1000

# shorten parameter names for easier reading
pm$x <- pm$Recat_TBS_0DME_Wake_Separation_Distance 
pm$y1 <- pm$Actual_4DME_Wake_Separation_Distance 
pm$y2 <- pm$Actual_4DME_All_Separation_Distance 
pm$z <- pm$ICAO_DBS_All_Separation_Distance 

# Find these times
pm_list <- lapply(1:no_of_iterations, function(i) {
  
  # ----------------------------------------------------------------------- #
  # Get Relevant Data
  # ----------------------------------------------------------------------- #
  
  # Get i-th row of performance model
  pm_i <- pm[i,]
  
  # Get i-th Landing_Pair_ID
  lpid <- as.numeric(pm_i$Landing_Pair_ID)
  
  # Filter for Leader/Follower segments in the i-th Landing Pair
  segs1 <- test_segs[test_segs$Landing_Pair_ID == lpid,]
  leader_segs <- segs1[,c("DME_Seg", "Leader_Average_GSPD")]
  follower_segs <- segs1[,c("DME_Seg", "Follower_Average_GSPD")]
  
  # ----------------------------------------------------------------------- #
  # Get Relevant Outputs (Not Under-Separated)
  # ----------------------------------------------------------------------- #
  
  usa <- 0 #under separation adjustment 
  
  ## T_P1 - Perfect time separation at 1DME ##
  TF <- segmentFlyingTime(follower_segs, floor(pm_i$x - usa), 1)
  pm_i$Recat_Perfect_1DME_Time_Separation <- ifelse(
    TF > 0 & ceiling(pm_i$x-usa) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$x-usa) - floor(pm_i$x-usa)) / follower_segs[follower_segs$DME_Seg == ceiling(pm_i$x-usa),]$Follower_Average_GSPD)) * 3600,
    NA
  )
  
  ## T_E1 - Actual time separation at 1DME ##
  TL <- segmentFlyingTime(leader_segs, 3, 1)
  TF <- segmentFlyingTime(follower_segs, floor(3 + pm_i$y1 - usa), 1)
  pm_i$Actual_1DME_Wake_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & (3 + ceiling(pm_i$y1-usa)) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$y1-usa) - floor(pm_i$y1-usa)) / follower_segs[follower_segs$DME_Seg == (3 + ceiling(pm_i$y1-usa)),]$Follower_Average_GSPD) - TL) * 3600,
    NA
  )
  
  ## T_E0 - Actual time separation at 0DME for Wake/ROT/MRS ##
  TL <- segmentFlyingTime(leader_segs, 3, 0)
  TF <- segmentFlyingTime(follower_segs, floor(3 + pm_i$y2 - usa), 0)
  pm_i$Actual_0DME_All_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & (3 + ceiling(pm_i$y2-usa)) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$y2-usa) - floor(pm_i$y2-usa)) / follower_segs[follower_segs$DME_Seg == (3 + ceiling(pm_i$y2-usa)),]$Follower_Average_GSPD) - TL) * 3600,
    NA
  )
  
  ## T_PI1 - Actual time separation at 0DME for Wake/ROT/MRS ##
  TF <- segmentFlyingTime(follower_segs, floor(pm_i$z-usa), 0)
  pm_i$ICAO_Perfect_0DME_Time_Separation <- ifelse(TF > 0 & floor(pm_i$z-usa) %in% follower_segs$DME_Seg, TF*3600, NA)

  
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-0.5)
  # ----------------------------------------------------------------------- #
  
  usa <- 0.5 #under separation adjustment 
  
  ## T_P1 - Perfect time separation at 1DME ##
  TF <- segmentFlyingTime(follower_segs, floor(pm_i$x - usa), 1)
  pm_i$Under_0.5_Recat_Perfect_1DME_Time_Separation <- ifelse(
    TF > 0 & ceiling(pm_i$x-usa) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$x-usa) - floor(pm_i$x-usa)) / follower_segs[follower_segs$DME_Seg == ceiling(pm_i$x-usa),]$Follower_Average_GSPD)) * 3600,
    NA
  )
  
  ## T_E1 - Actual time separation at 1DME ##
  TL <- segmentFlyingTime(leader_segs, 3, 1)
  TF <- segmentFlyingTime(follower_segs, floor(3 + pm_i$y1 - usa), 1)
  pm_i$Under_0.5_Actual_1DME_Wake_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & (3 + ceiling(pm_i$y1-usa)) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$y1-usa) - floor(pm_i$y1-usa)) / follower_segs[follower_segs$DME_Seg == (3 + ceiling(pm_i$y1-usa)),]$Follower_Average_GSPD)) * 3600,
    NA
  )
  
  ## T_E0 - Actual time separation at 0DME for Wake/ROT/MRS ##
  TL <- segmentFlyingTime(leader_segs, 3, 0)
  TF <- segmentFlyingTime(follower_segs, floor(3 + pm_i$y2 - usa), 0)
  pm_i$Under_0.5_Actual_0DME_All_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & (3 + ceiling(pm_i$y2-usa)) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$y2-usa) - floor(pm_i$y2-usa)) / follower_segs[follower_segs$DME_Seg == (3 + ceiling(pm_i$y2-usa)),]$Follower_Average_GSPD)) * 3600,
    NA
  )
  
  ## T_PI1 - Actual time separation at 0DME for Wake/ROT/MRS ##
  TF <- segmentFlyingTime(follower_segs, floor(pm_i$z-usa), 0)
  pm_i$Under_0.5_ICAO_Perfect_0DME_Time_Separation <- ifelse(TF > 0 & floor(pm_i$z-usa) %in% follower_segs$DME_Seg, TF*3600, NA)
  
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-1.0)
  # ----------------------------------------------------------------------- #
  
  usa <- 1 #under separation adjustment 
  
  ## T_P1 - Perfect time separation at 1DME ##
  TF <- segmentFlyingTime(follower_segs, floor(pm_i$x - usa), 1)
  pm_i$Under_1.0_Recat_Perfect_1DME_Time_Separation <- ifelse(
    TF > 0 & ceiling(pm_i$x-usa) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$x-usa) - floor(pm_i$x-usa)) / follower_segs[follower_segs$DME_Seg == ceiling(pm_i$x-usa),]$Follower_Average_GSPD)) * 3600,
    NA
  )
  
  ## T_E1 - Actual time separation at 1DME ##
  TL <- segmentFlyingTime(leader_segs, 3, 1)
  TF <- segmentFlyingTime(follower_segs, floor(3 + pm_i$y1 - usa), 1)
  pm_i$Under_1.0_Actual_1DME_Wake_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & (3 + ceiling(pm_i$y1-usa)) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$y1-usa) - floor(pm_i$y1-usa)) / follower_segs[follower_segs$DME_Seg == (3 + ceiling(pm_i$y1-usa)),]$Follower_Average_GSPD)) * 3600,
    NA
  )
  
  ## T_E0 - Actual time separation at 0DME for Wake/ROT/MRS ##
  TL <- segmentFlyingTime(leader_segs, 3, 0)
  TF <- segmentFlyingTime(follower_segs, floor(3 + pm_i$y2 - usa), 0)
  pm_i$Under_1.0_Actual_0DME_All_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & (3 + ceiling(pm_i$y2-usa)) %in% follower_segs$DME_Seg,
    (TF + (((pm_i$y2-usa) - floor(pm_i$y2-usa)) / follower_segs[follower_segs$DME_Seg == (3 + ceiling(pm_i$y2-usa)),]$Follower_Average_GSPD)) * 3600,
    NA
  )
  
  ## T_PI1 - Actual time separation at 0DME for Wake/ROT/MRS ##
  TF <- segmentFlyingTime(follower_segs, floor(pm_i$z-usa), 0)
  pm_i$Under_1.0_ICAO_Perfect_0DME_Time_Separation <- ifelse(TF > 0 & floor(pm_i$z-usa) %in% follower_segs$DME_Seg, TF*3600, NA)
  
  
  message("Completed calculations for ", paste0(pm_i[,c("FP_Date", "Leader_Callsign", "Follower_Callsign")], collapse = "_"), " (", i, "/", nrow(pm), ")")
  
  return(pm_i)
  
})

pm_list <- lapply(1:no_of_iterations, function(i) {
  
  # ----------------------------------------------------------------------- #
  # Get Relevant Data
  # ----------------------------------------------------------------------- #
  # Get i-th row of performance model
  #i <- 23
  pm_i <- pm[i,]
  
  # Get i-th Landing_Pair_ID & Leader Aircraft Type
  lpid <- as.numeric(pm_i$Landing_Pair_ID)
  Leader_Aircraft <- pm_i$Leader_Aircraft_Type
  
  # Filter for Leader/Follower segments in the i-th Landing Pair
  segs1 <- test_segs[test_segs$Landing_Pair_ID == lpid,]
  leader_segs <- segs1[,c("DME_Seg", "Leader_Average_GSPD")]
  follower_segs <- segs1[,c("DME_Seg", "Follower_Average_GSPD")]
  
  # Get relevant distances
  D_RT <- pm_i$Recat_TBS_0DME_Wake_Separation_Distance
  D_RRT <- pm_i$Recat_TBS_0DME_All_Separation_Distance
  D_E4 <- pm_i$Actual_4DME_Wake_Separation_Distance
  D_E4M <- pm_i$Actual_4DME_All_Separation_Distance
  D_ICAOD <- pm_i$ICAO_DBS_All_Separation_Distance 
  
  # ----------------------------------------------------------------------- #
  # Get Relevant Outputs (Not Under-Separated)
  # ----------------------------------------------------------------------- #
  
  pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0)
  pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RRT, Under_Sep=0)
  pm_i$ICAO_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_ICAOD, Under_Sep=0)
  pm_i$Actual_4DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 0)
  pm_i$Actual_4DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0)
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-0.5)
  # ----------------------------------------------------------------------- #
  
  pm_i$Under_0.5_Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0.5)
  pm_i$Under_0.5_Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RRT, Under_Sep=0.5)
  pm_i$Under_0.5_ICAO_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_ICAOD, Under_Sep=0.5)
  pm_i$Under_0.5_Actual_4DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 0.5)
  pm_i$Under_0.5_Actual_4DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0.5)
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-1.0)
  # ----------------------------------------------------------------------- #
  
  pm_i$Under_1.0_Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=1)
  pm_i$Under_1.0_Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RRT, Under_Sep=1)
  pm_i$Under_1.0_ICAO_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_ICAOD, Under_Sep=1)
  pm_i$Under_1.0_Actual_4DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 1)
  pm_i$Under_1.0_Actual_4DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 1)
  
  message("Completed calculations for ", paste0("Landing Pair ID ", lpid, " (", i, "/", no_of_iterations, ")"))
  
  return(pm_i)

})

pm2 <- rbindlist(pm_list)
pm2 <- select(pm2, -c("x", "y1", "y2", "z"))

pm_view <- select(pm2,
                  Landing_Pair_ID,
                  Leader_Aircraft_Type,
                  Follower_Aircraft_Type,
                  Leader_ICAO_Wake_Cat,
                  Follower_ICAO_Wake_Cat,
                  Leader_Recat_Wake_Cat,
                  Follower_Recat_Wake_Cat,
                  ICAO_DBS_All_Separation_Distance,
                  Recat_DBS_Wake_Separation_Distance,
                  Recat_TBS_0DME_Wake_Separation_Distance,
                  Recat_TBS_0DME_ROT_Spacing_Distance,
                  Recat_TBS_0DME_All_Separation_Distance,
                  Forecast_ORD_Compression,
                  Recat_TBS_4DME_Wake_Separation_Distance,
                  Recat_TBS_4DME_ROT_Spacing_Distance,
                  Recat_TBS_4DME_All_Separation_Distance,
                  ICAO_Assumed_4DME_Separation_Accuracy,
                  Actual_4DME_Wake_Separation_Distance,
                  Actual_4DME_All_Separation_Distance,
                  Recat_Perfect_1DME_Time_Separation,
                  Actual_1DME_Wake_Time_Separation,
                  Actual_0DME_All_Time_Separation,
                  ICAO_Perfect_0DME_Time_Separation
                  )

## Export Total Calculations for Sense Checking ##
fwrite(pm2, file.path(outdir, "IA_Calculations.csv"))
fwrite(pm_view, file.path(outdir, "IA_Calculations_Narrow.csv"))

# ----------------------------------------------------------------------- #
# Temp Seg Check to see distributions of Leader-Follower Speeds
# ----------------------------------------------------------------------- #
# Currently an average 2-5s positive diff between l/f 0-1DME times
# still currently unsolved
# ----------------------------------------------------------------------- #
test_segs_4 <- filter(test_segs, DME_Seg < 12)

ggplot(test_segs_4, mapping = aes(x = as.factor(DME_Seg), y = Leader_Average_GSPD - Follower_Average_GSPD)) + 
  geom_boxplot() + ylim(-30, 30)

test_segs_4 <- mutate(test_segs_4, GSPD_Diff = (1/Leader_Average_GSPD - 1/Follower_Average_GSPD)*3600)
gspd_diff_summary <- test_segs_4 %>% group_by(DME_Seg) %>% summarise(Mean_Diff = mean(GSPD_Diff, na.rm=T),
                                                                     Median_Diff = median(GSPD_Diff, na.rm=T),
                                                                     std_diff = sd(GSPD_Diff, na.rm=T))

pm_view_inv <- filter(pm_view, Actual_4DME_Wake_Separation_Distance == Actual_4DME_All_Separation_Distance)
pm_view_inv <- mutate(pm_view_inv, Thr_1DME_Time_Diff = Actual_0DME_All_Time_Separation - Actual_1DME_Wake_Time_Separation)

ggplot(pm_view_inv, mapping=aes(y = Thr_1DME_Time_Diff, x = 1)) + geom_boxplot()


# ----------------------------------------------------------------------- #
# Observed 4DME separation accuracy histograms for A338s 
# ----------------------------------------------------------------------- #
# need to change variable names
# ----------------------------------------------------------------------- #



# ----------------------------------------------------------------------- #
# Final filtering & Data format Preparation
# ----------------------------------------------------------------------- #
# need to change variable names & WaPT format
# ----------------------------------------------------------------------- #

filter_sep_acc <- T

if (filter_sep_acc == T){

## Calculate D_A4 Then Compare with Perfect. Remove Anything With A Variability More Than 2NM or 3NM if A388##
pm2_non <- pm2[pm2$ICAO_DBS_4DME_Separation_Accuracy < 2 & pm2$Leader_Aircraft_Type != "A388"]
pm2_A388 <- pm2[pm2$ICAO_DBS_4DME_Separation_Accuracy < 3 & pm2$Leader_Aircraft_Type == "A388"]
pm3 <- rbind(pm2_non, pm2_A388)
  
## Export Filtered Calculations for Sense Checking ##
fwrite(pm3, file.path(outdir, "IA_Calculations_Filtered.csv"))
  
} else {
  
  pm3 <- pm2
  
}

# ----------------------------------------------------------------------- #
# 2. eTBS Performance Analysis --------------------------------------------
# NEED TO ADD OTHER 18 WAKE SCHEMES TIMES/SPEEDS FOR PLOT LINES
# ----------------------------------------------------------------------- #

# Import from eTBS calculations
data1 <- pm2

# Drop other variables - do we need this?
#rm(list = ls()[!(ls() %in% c("data1", "outdir", "radar_min_sep"))])

# 2.1 Definitions ---------------------------------------------------------

# wakes <- c("SUPER", "HEAVY", "UPPER", "MEDIUM", "SMALL", "LIGHT")
wakes <- LETTERS[1:6]

wake_levels <- paste0(rep(wakes, each = length(wakes)), "-", rep(wakes, times = length(wakes)))

sep_scheme <- fread("Input/Separation Schemes.csv") # this needs updating
sep_scheme$LF_Pair <- factor(sep_scheme$LF_Pair, levels = wake_levels)

# 2.2 Calculate new columns -----------------------------------------------

# Get Accuracies & Improvement values
data1 <- mutate(
  data1,
  RECAT_Wake_Type_Pair = paste(Leader_Recat_Wake_Cat, Follower_Aircraft_Type, sep = "-"),
  RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels),
  Actual_DBS_1DME_Time_Accuracy = Actual_1DME_Wake_Time_Separation - Recat_TBS_Wake_Separation_Time,
  Actual_TBS_1DME_Time_Accuracy = Actual_1DME_Wake_Time_Separation - Recat_Perfect_1DME_Time_Separation,
  #Actual_TBS_0DME_Time_Accuracy = Actual_0DME_All_Time_Separation - Recat_Perfect_0DME_Time_Separation,
  Actual_4DME_Wake_Distance_Accuracy = Observed_4DME_Separation_Distance - Actual_4DME_Wake_Separation_Distance,
  Actual_4DME_All_Distance_Accuracy = Observed_4DME_Separation_Distance - Actual_4DME_All_Separation_Distance,
  ICAO_Perfect_Time_Accuracy = Actual_0DME_All_Time_Separation - ICAO_Perfect_0DME_Time_Separation 
  
)

# Segment Accuracy

data1 <- mutate(
  data1,
  Actual_DBS_1DME_Time_Accuracy_Group = cut(Actual_DBS_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)),
  Actual_TBS_1DME_Time_Accuracy_Group = cut(Actual_TBS_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)),
  Actual_4DME_Wake_Distance_Accuracy_Group = cut(Actual_4DME_Wake_Distance_Accuracy, breaks = c(-Inf, -0.55, -0.05, Inf)),
  Actual_4DME_All_Distance_Accuracy_Group = cut(Actual_4DME_All_Distance_Accuracy, breaks = c(-Inf, -0.55, -0.05, Inf)),
  Observed_AGI_Surface_Wind_SPD_Group = cut(Observed_AGI_Surface_Wind_SPD, breaks = c(0,4,7,10,13,16,Inf), right=FALSE),
  ICAO_Perfect_Time_Accuracy_Group = cut(ICAO_Perfect_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf))
)


# 2.3 Find out when capping at 3NM will occur -----------------------------

data1$Capped_Compression <- ifelse(data1$Forecast_ORD_Compression_Uncapped < 0, 1, 0)
data1$Capped_Wake_Separation <- ifelse(data1$Recat_TBS_0DME_Wake_Separation_Distance_Uncapped < wake_cap_0, 1, ifelse(is.na(data1$Recat_TBS_0DME_Wake_Separation_Distance_Uncapped), NA, 0))
data1$Capped_ROT_Spacing <- ifelse(data1$Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped < rot_cap_0, 1, ifelse(is.na(data1$Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped), NA, 0))

date_breakdown <- table(data1$FP_Date, data1$Capped_Compression, data1$Capped_Wake_Separation, data1$Capped_ROT_Spacing)

fwrite(date_breakdown, file.path(outdir, "date_breakdown.csv"))

type_data <- filter(data1, Capped_at_3NM == 1) %>%
  group_by(FP_Date, RECAT_Wake_Pair) %>%
  summarise(Total = n(), MeanWE = mean(Follower_Forecast_eTBS_Wind_Effect, na.rm = TRUE)) %>%
  as.data.table()

ac_type_data <- filter(data1, Capped_at_3NM == 1) %>%
  group_by(FP_Date, Leader_Aircraft_Type) %>%
  summarise(Total = n(), MeanWE = mean(Follower_Forecast_eTBS_Wind_Effect, na.rm = TRUE)) %>%
  as.data.table()

fwrite(type_data, file.path(outdir, "type_breakdown.csv"))
fwrite(ac_type_data, file.path(outdir, "ac_type_breakdown.csv"))


# 2.4 Generate Word Document ----------------------------------------------

# Create a word document
doc <- read_docx()
doc <- body_add_par(doc, "Distributions of Actual Time Separations for IA", style = "heading 1")

# Output Graphs for each Wake Pair: Actual 1DME Time Separation
# UPDATED

for (i in 1:nrow(sep_scheme)){
  
  # Select the wake pair and grab the recat and uk data matching this pair.
  
  wake_pair <- sep_scheme$LF_Pair[i]
  recat_time <- sep_scheme$RECAT_Time_Reference[i]
  
  wake_pair_data_recat_tbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$Recat_Perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$Actual_1DME_Wake_Time_Separation
  
  plot_title_recat_tbs <- paste("1DME Perfect Time Separation for IA: ", wake_pair, sep = "")
  plot_title_recat_dbs <- paste("1DME Actual Time Separation for IA: ", wake_pair, sep = "")
  
  if (nrow(wake_pair_data_recat_tbs) == 0 & nrow(wake_pair_data_recat_dbs) == 0){
    next
  }
  
  if (is.na(recat_time)){
    next
  }
  
  if(nrow(wake_pair_data_recat_tbs) > 0){
    
    rcp <- plot_time_histogram(wake_pair_data_recat_tbs, plot_title_recat_tbs, recat_time, "light blue")
    
  } else { rcp <- ggplot()}
  
  if(nrow(wake_pair_data_recat_dbs) > 0){
    
    ukp <- plot_time_histogram(wake_pair_data_recat_dbs, plot_title_recat_dbs, recat_time, "light green")
    ggtitle(plot_title_recat_dbs)
    
  } else { ukp <- ggplot()}
  
  message("Printing for ", wake_pair)
  
  doc <- body_add_par(doc, paste("Histogram of Time Separation at 1DME for", wake_pair), style = "heading 2")
  
  src <- tempfile(fileext = ".png")
  png(filename = src, width = 5, height = 6, units = 'in', res = 300)
  grid.arrange(rcp, ukp, ncol = 1)
  dev.off()
  
  doc <- body_add_img(doc, src = src, width = 5, height = 6, style = "centered")
  
}

# Low Surface Wind

for (i in 1:nrow(sep_scheme)){
  
  # Select the wake pair and grab the recat and uk data matching this pair.
  
  wake_pair <- sep_scheme$LF_Pair[i]
  recat_time <- sep_scheme$RECAT_Time_Reference[i]
  
  wake_pair_data_recat_tbs <- filter(data1, RECAT_Wake_Pair == wake_pair & Observed_AGI_Surface_Wind_SPD >= -2 & Observed_AGI_Surface_Wind_SPD <= 2)
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$Recat_Perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair & Observed_AGI_Surface_Wind_SPD >= -2 & Observed_AGI_Surface_Wind_SPD <= 2)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$Actual_1DME_Wake_Time_Separation
  
  nrow1 = nrow(wake_pair_data_recat_tbs)
  nrow2 = nrow(wake_pair_data_recat_dbs)
  
  plot_title_recat_tbs <- paste("Low Wind Perfect Time SepN: ", wake_pair, " (n=", nrow1, ")", sep = "")
  plot_title_recat_dbs <- paste("Low Wind Actual Time SepN: ", wake_pair, " (n=", nrow2, ")", sep = "")
  
  if (nrow(wake_pair_data_recat_tbs) == 0 & nrow(wake_pair_data_recat_dbs) == 0){
    next
  }
  
  if (is.na(recat_time)){
    next
  }
  
  if(nrow(wake_pair_data_recat_tbs) > 0){
    
    rcp <- plot_time_histogram(wake_pair_data_recat_tbs, plot_title_recat_tbs, recat_time, "light blue")
    
  } else { rcp <- ggplot()}
  
  if(nrow(wake_pair_data_recat_dbs) > 0){
    
    ukp <- plot_time_histogram(wake_pair_data_recat_dbs, plot_title_recat_dbs, recat_time, "light green")
    ggtitle(plot_title_recat_dbs)
    
  } else { ukp <- ggplot()}
  
  message("Printing for ", wake_pair)
  
  doc <- body_add_par(doc, paste("Histogram of Time Separation at 1DME for", wake_pair), style = "heading 2")
  
  src <- tempfile(fileext = ".png")
  png(filename = src, width = 5, height = 6, units = 'in', res = 300)
  grid.arrange(rcp, ukp, ncol = 1)
  dev.off()
  
  doc <- body_add_img(doc, src = src, width = 5, height = 6, style = "centered")
  
}

# All Surface Wind

for (i in 1:nrow(sep_scheme)){
  
  # Select the wake pair and grab the recat and uk data matching this pair.
  
  wake_pair <- sep_scheme$LF_Pair[i]
  recat_time <- sep_scheme$RECAT_Time_Reference[i]
  
  wake_pair_data_recat_tbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$Recat_Perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$Actual_1DME_Wake_Time_Separation
  
  nrow1 = nrow(wake_pair_data_recat_tbs)
  nrow2 = nrow(wake_pair_data_recat_dbs)
  
  plot_title_recat_tbs <- paste("All Wind Perfect Time SepN: ", wake_pair, " (n=", nrow1, ")", sep = "")
  plot_title_recat_dbs <- paste("All Wind Actual Time SepN: ", wake_pair, " (n=", nrow2, ")", sep = "")
  
  if (nrow(wake_pair_data_recat_tbs) == 0 & nrow(wake_pair_data_recat_dbs) == 0){
    next
  }
  
  if (is.na(recat_time)){
    next
  }
  
  if(nrow(wake_pair_data_recat_tbs) > 0){
    
    rcp <- plot_time_histogram(wake_pair_data_recat_tbs, plot_title_recat_tbs, recat_time, "light blue")
    
  } else { rcp <- ggplot()}
  
  if(nrow(wake_pair_data_recat_dbs) > 0){
    
    ukp <- plot_time_histogram(wake_pair_data_recat_dbs, plot_title_recat_dbs, recat_time, "light green")
    ggtitle(plot_title_recat_dbs)
    
  } else { ukp <- ggplot()}
  
  message("Printing for ", wake_pair)
  
  doc <- body_add_par(doc, paste("Histogram of Time Separation at 1DME for", wake_pair), style = "heading 2")
  
  src <- tempfile(fileext = ".png")
  png(filename = src, width = 5, height = 6, units = 'in', res = 300)
  grid.arrange(rcp, ukp, ncol = 1)
  dev.off()
  
  doc <- body_add_img(doc, src = src, width = 5, height = 6, style = "centered")
  
}

# Output Graphs for each Wake Pair Separately: Perfect 1DME Time Separation

doc <- body_add_par(doc, "Distributions of Perfect Time Separations for eTBS/RECAT", style = "heading 1")

for (i in 1:nrow(sep_scheme)){
  
  # Select the wake pair and grab the recat and uk data matching this pair.
  
  wake_pair <- sep_scheme$LF_Pair[i]
  recat_time <- sep_scheme$RECAT_Time_Reference[i]
  
  wake_pair_data_recat_tbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$Recat_Perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$ICAO_Perfect_0DME_Time_Separation
  
  plot_title_recat_tbs <- paste("1DME Perfect Separation for IA: ", wake_pair, sep = "")
  plot_title_recat_dbs <- paste("1DME Perfect Separation for ICAO DBS: ", wake_pair, sep = "")
  
  if (nrow(wake_pair_data_recat_tbs) == 0 & nrow(wake_pair_data_recat_dbs) == 0){
    next
  }
  
  if (is.na(recat_time)){
    next
  }
  
  if(nrow(wake_pair_data_recat_tbs) > 0){
    
    rcp <- plot_time_histogram(wake_pair_data_recat_tbs, plot_title_recat_tbs, recat_time, "light blue")
    
  } else { rcp <- ggplot()}
  
  if(nrow(wake_pair_data_recat_dbs) > 0){
    
    ukp <- plot_time_histogram(wake_pair_data_recat_dbs, plot_title_recat_dbs, recat_time, "light green")
    ggtitle(plot_title_recat_dbs)
    
  } else { ukp <- ggplot()}
  
  message("Printing for ", wake_pair)
  
  doc <- body_add_par(doc, paste("Histogram of Time Separation at 1DME for", wake_pair), style = "heading 2")
  
  src <- tempfile(fileext = ".png")
  png(filename = src, width = 5, height = 6, units = 'in', res = 300)
  grid.arrange(rcp, ukp, ncol = 1)
  dev.off()
  
  doc <- body_add_img(doc, src = src, width = 5, height = 6, style = "centered")
  
}


# Export document
print(doc, target = file.path(outdir, "IA_Wake_Pair_Distributions.docx"))


# 2.5 Additional tables and plots -----------------------------------------

# Tables for the Wake - Type Pairs and Time Under-separation Rate

# t1 # Recat Wake-Type Pair Accuracy against Recat DBS Distance Reference Time Spacing (Actual 1DME Time)
t1 <- table(data1$RECAT_Wake_Type_Pair, data1$Actual_DBS_1DME_Time_Accuracy_Group)
fwrite(as.data.table(t1), file.path(outdir, "1DME_DBS_Time_Accuracy_Wake_Type.csv"))

# t2 # Recat Wake-Type Pair Accuracy against Recat TBS Distance Perfect Time Spacing (Actual 1DME Time)
t2 <- table(data1$RECAT_Wake_Type_Pair, data1$Actual_TBS_1DME_Time_Accuracy_Group)
fwrite(as.data.table(t2), file.path(outdir, "1DME_TBS_Time_Accuracy_Wake_Type.csv"))

# t3 # Recat Wake-Wake Pair Accuracy against Recat DBS Distance Reference Time Spacing (Actual 1DME Time)
t3 <- table(data1$RECAT_Wake_Pair, data1$Actual_DBS_1DME_Time_Accuracy_Group)
fwrite(as.data.table(t3), file.path(outdir, "1DME_DBS_Time_Accuracy_Wake_Wake.csv"))

# t4 # Recat Wake-Wake Pair Accuracy against Recat TBS Distance Perfect Time Spacing (Actual 1DME Time)
t4 <- table(data1$RECAT_Wake_Pair, data1$Actual_TBS_1DME_Time_Accuracy_Group)
fwrite(as.data.table(t4), file.path(outdir, "1DME_TBS_Time_Accuracy_Wake_Wake.csv"))

# t5 # Recat Wake-Type Pair Accuracy STATISTICS against Recat DBS Distance Reference Time Spacing (Actual 1DME Time)
t5 <- group_by(data1, RECAT_Wake_Type_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_DBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t5), file.path(outdir, "1DME_DBS_Time_Accuracy_Stats_Wake_Type.csv"))

# t6 # Recat Wake-Type Pair Accuracy STATISTICS against Recat TBS Distance Perfect Time Spacing (Actual 1DME Time)
t6 <- group_by(data1, RECAT_Wake_Type_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_TBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t6), file.path(outdir, "1DME_TBS_Time_Accuracy_Stats_Wake_Type.csv"))

# t7 # Recat Wake Pair Accuracy STATISTICS against Recat DBS Distance Reference Time Spacing (Actual 1DME Time)
t7 <- group_by(data1, RECAT_Wake_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_DBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t7), file.path(outdir, "1DME_DBS_Time_Accuracy_Stats_Wake_Wake.csv"))

# t8 # Recat Wake Pair Accuracy STATISTICS against Recat TBS Distance Perfect Time Spacing (Actual 1DME Time)
t8 <- group_by(data1, RECAT_Wake_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_TBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t8), file.path(outdir, "1DME_TBS_Time_Accuracy_Stats_Wake_Wake.csv"))



t9 <- table(data1$RECAT_Wake_Pair, data1$Observed_AGI_Surface_Wind_SPD_Group)

# Tables for the Wake - Type Pairs

group_by(data1, UK_Wake_Pair) %>%
  summarise(Sep = mean(UK6Cat_DBS_4DME_Wake_Separation_Distance, na.rm = TRUE)) %>%
  as.data.table()

group_by(data1, UK6Cat_DBS_4DME_Wake_Separation_Distance) %>%
  summarise(Sep = mean(Ref_UK6Cat_Separation_Time, na.rm = TRUE)) %>%
  as.data.table()

data1$check_1DME_actual <- data1$eTBS_actual_4DME_Distance_Separation - data1$Observed_Compression

## Histograms

# 1 DME Time Accuracy

hist_data <- select(data1, RECAT_Wake_Type_Pair ,RECAT_Wake_Pair, UK_Wake_Type_Pair, UK_Wake_Pair,eTBS_actual_1DME_Time_Accuracy, UK_actual_1DME_Time_Accuracy)
hist_data = melt(hist_data, id=c("RECAT_Wake_Type_Pair", "RECAT_Wake_Pair","UK_Wake_Type_Pair", "UK_Wake_Pair"))
hist_data$Wake_Pair <- factor(ifelse(hist_data$variable == "eTBS_actual_1DME_Time_Accuracy", as.character(hist_data$RECAT_Wake_Pair), as.character(hist_data$UK_Wake_Pair)), levels = levels(hist_data$RECAT_Wake_Pair))

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 5, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-50,100))+
  xlab("1DME Time SepN Accuracy(s)")+
  scale_fill_discrete(name = "Title",
                      labels = c("A", "B"))+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Time Separation Accuray")

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 5, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-50,100))+
  ylim(c(0,0.1))+
  xlab("1DME Time SepN Accuracy(s)")+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Time Separation Accuray")+
  facet_wrap(~ Wake_Pair, nrow = 3)

# 1 DME Distance  Accuracy

hist_data <- select(data1, RECAT_Wake_Type_Pair ,RECAT_Wake_Pair, UK_Wake_Type_Pair, UK_Wake_Pair,eTBS_actual_1DME_Distance_Accuracy, UK_actual_1DME_Distance_Accuracy)

hist_data = melt(hist_data, id=c("RECAT_Wake_Type_Pair", "RECAT_Wake_Pair","UK_Wake_Type_Pair", "UK_Wake_Pair"))
hist_data$Wake_Pair <- ifelse(hist_data$variable == "eTBS_actual_1DME_Distance_Accuracy", hist_data$RECAT_Wake_Pair, hist_data$UK_Wake_Pair)
hist_data$Wake_Pair <- factor(ifelse(hist_data$variable == "eTBS_actual_1DME_Time_Accuracy", as.character(hist_data$RECAT_Wake_Pair), as.character(hist_data$UK_Wake_Pair)), levels = levels(hist_data$RECAT_Wake_Pair))

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 0.1, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-1,2))+
  xlab("1DME Distance SepN Accuracy(s)")+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Distance Separation Accuray")

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 0.1, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-1,2))+
  ylim(c(0,1.5))+
  xlab("1DME Distance SepN Accuracy(s)")+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Distance Separation Accuray")+
  facet_wrap(~ Wake_Pair, nrow = 3)


