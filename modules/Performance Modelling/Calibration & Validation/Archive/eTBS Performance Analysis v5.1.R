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

library(data.table)
library(dplyr)
library(officer)
library(ggplot2)
library(gridExtra)
library(RODBC)

# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Generate sub-directory in Output folder for this run
outdir <- file.path("Output", paste0("Run_", gsub(" ", "_", gsub("-|:", "", as.character(Sys.time())))))
dir.create(outdir)

# Subset performance model to speed up testing
# NOTE: set to NA for full dataset or set to specific dates of correct format
# test_date <- "01/10/2019"
test_date <- NA

# Database Name
database <- "LVNL_UTMA_Validation"

# Connect to database
con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.11", database, "ruser", "Th!nkruser"
))

# Write a readme file for this run
sink(file.path(outdir, "readme.txt"))
cat("Derry's run through of the v5.1 script to check all is working correctly with LVNL data (IA-108)")
sink()

# ----------------------------------------------------------------------- #
# 1. eTBS Calculations ----------------------------------------------------
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
  return(sum(1/df[V1 %in% segs]$V2, na.rm = T))
}

# 1.1 Import data ---------------------------------------------------------

if (!is.na(test_date)) {
  pm <- as.data.table(sqlQuery(con, sprintf("SELECT * FROM vw_eTBS_Performance_Model WHERE FP_Date IN ('%s')", paste(test_date, collapse = "','"))))
} else {
  pm <- as.data.table(sqlQuery(con, "SELECT * FROM vw_eTBS_Performance_Model"))
}

if (!is.na(test_date)) {
  rd <- as.data.table(sqlQuery(con, sprintf("SELECT
       FTA.[FP_Date]
      ,[FP_Time]
      ,FTA.[Landing_Runway]
      ,[DME_Seg]
      ,[Ave_Mode_S_IAS]
      ,[Ave_Mode_S_GSPD]
      ,FTA.[Landing_Pair_ID]
      ,[Leader_Flight_Plan_ID]
      ,FTA.[Leader_Callsign]
      ,FTA.[Leader_Aircraft_Type]
      ,[Leader_Wake_Cat]
      ,[Leader_Time_At_4DME]
      ,FTA.[Delivered_4DME_Separation]
      ,[Delivered_1DME_Separation]
      ,[Delivered_0DME_Separation]
      ,[Surface_Wind_SPD]
      ,[Surface_Wind_HDG]
	    ,PDR.[Follower_Callsign]
	    ,PDR.[Follower_Aircraft_Type]
	    ,PDR.[Follower_0DME_Time]
	    ,PDR.[Observed_AGI_Surface_Headwind]
      ,PDR.[Observed_AGI_Surface_Wind_SPD]
      ,PDR.[Observed_AGI_Surface_Wind_HDG]
  FROM [LVNL_UTMA_Validation].[dbo].[vw_Flying_Time_Analysis] AS FTA
  JOIN vw_All_Pair_Reference_Data AS PDR
  ON FTA.[Landing_Pair_ID] = PDR.[Landing_Pair_ID]
  WHERE FTA.FP_Date IN ('%s')", paste(test_date, collapse = "','"))))
} else {
  rd <- as.data.table(sqlQuery(con, "SELECT
       FTA.[FP_Date]
      ,[FP_Time]
      ,FTA.[Landing_Runway]
      ,[DME_Seg]
      ,[Ave_Mode_S_IAS]
      ,[Ave_Mode_S_GSPD]
      ,FTA.[Landing_Pair_ID]
      ,[Leader_Flight_Plan_ID]
      ,FTA.[Leader_Callsign]
      ,FTA.[Leader_Aircraft_Type]
      ,[Leader_Wake_Cat]
      ,[Leader_Time_At_4DME]
      ,FTA.[Delivered_4DME_Separation]
      ,[Delivered_1DME_Separation]
      ,[Delivered_0DME_Separation]
      ,[Surface_Wind_SPD]
      ,[Surface_Wind_HDG]
	    ,PDR.[Follower_Callsign]
	    ,PDR.[Follower_Aircraft_Type]
	    ,PDR.[Follower_0DME_Time]
	    ,PDR.[Observed_AGI_Surface_Headwind]
      ,PDR.[Observed_AGI_Surface_Wind_SPD]
      ,PDR.[Observed_AGI_Surface_Wind_HDG]
  FROM [LVNL_UTMA_Validation].[dbo].[vw_Flying_Time_Analysis] AS FTA
  JOIN vw_All_Pair_Reference_Data AS PDR
  ON FTA.[Landing_Pair_ID] = PDR.[Landing_Pair_ID]"))
}

# 1.2 Clean data ----------------------------------------------------------

# Subset performance model for testing purposes
if (!is.na(test_date)) {
  pm <- pm[FP_Date %in% test_date]
}

# Remove NA values where Wake Separation not applied
pm <- pm[!is.na(Recat_eTBS_0DME_Wake_Separation_Distance)]

# Remove NA values where there is no forecast compression
pm <- pm[!is.na(Forecast_ORD_TBS_Compression)]

# Remove all negative Observed 1DME Separation Time - go arounds
pm <- pm[Observed_1DME_Separation_Time > 0]

# 1.3 Merge data ----------------------------------------------------------

# Get Average Mode S GSPD and IAS from Reference Data
rd_ave_spd <- data.table(
  unique(pm[,c("FP_Date", "Leader_Callsign", "Follower_Callsign")]),
  Ave_Mode_S_IAS = NA,
  Ave_Mode_S_GSPD = NA
)
for (i in 1:nrow(rd_ave_spd)) {
  
  rd_ave_spd$Ave_Mode_S_IAS[i] <- mean(
    rd[FP_Date == rd_ave_spd$FP_Date[i] &
         Leader_Callsign == rd_ave_spd$Leader_Callsign[i] &
         Follower_Callsign == rd_ave_spd$Follower_Callsign[i]]$Ave_Mode_S_IAS,
    na.rm = T
  )
  
  rd_ave_spd$Ave_Mode_S_GSPD[i] <- mean(
    rd[FP_Date == rd_ave_spd$FP_Date[i] &
         Leader_Callsign == rd_ave_spd$Leader_Callsign[i] &
         Follower_Callsign == rd_ave_spd$Follower_Callsign[i]]$Ave_Mode_S_GSPD,
    na.rm = T
  )
  
  if (i %in% round(quantile(1:nrow(rd_ave_spd), seq(0, 1, 0.05)))) {
    message("Merging average speeds - ", round(i/nrow(rd_ave_spd)*100, 0), "% complete.")
  }
  
}
pm <- merge(
  x = pm,
  y = rd_ave_spd,
  by = c("FP_Date", "Leader_Callsign", "Follower_Callsign"),
  all.x = T
)

# 1.4 Calculate crosswind -------------------------------------------------

# (WIP)

# degreesToRadian <- function(deg) {
#   return(deg / 180 * pi)
# }
# 
# calculateCrosswind <- function(RWY_HDG, WIND_HDG, WIND_SPD) {
#   
# }
# 
# ## Insert Runway Heading (LVNL)
# rd$Runway_True_Heading <- ifelse(
#   rd$Landing_Runway == "R18R", 183, ifelse(
#     rd$Landing_Runway == "R36L", 3, ifelse(
#       rd$Landing_Runway == "R06", 58, ifelse(
#         rd$Landing_Runway == "R24", 238, ifelse(
#           rd$Landing_Runway == "R09", 87, ifelse(
#             rd$Landing_Runway == "R27", 267, ifelse(
#               rd$Landing_Runway == "R18L", 183, ifelse(
#                 rd$Landing_Runway == "R36R", 3, ifelse(
#                   rd$Landing_Runway == "R18C", 183, ifelse(
#                     rd$Landing_Runway == "R36", 3, ifelse(
#                       rd$Landing_Runway == "R04", 41, ifelse(
#                         rd$Landing_Runway == "R22", 221, NA
#                       )
#                     )
#                   )
#                 )
#               )
#             )
#           )
#         )
#       )
#     )
#   )
# )

# 1.5 Apply spacing constraints -------------------------------------------

# UK6 Separation set 3NM lower bound
pm$UK6Cat_TBS_4DME_Wake_Separation_Distance_New <- ifelse(pm$UK6Cat_TBS_4DME_Wake_Separation_Distance < 3, 3, pm$UK6Cat_TBS_4DME_Wake_Separation_Distance)
pm$UK6Cat_TBS_4DME_Wake_Separation_Distance_New_Rounded <- round(pm$UK6Cat_TBS_4DME_Wake_Separation_Distance_New, 1)

# UK6Cat_Separation_Distance convert NA to 3NM
pm$UK6Cat_Separation_Distance <- ifelse(is.na(pm$UK6Cat_Separation_Distance), 3, pm$UK6Cat_Separation_Distance)

# Calculate current applied accuracy
pm$Observed_4DME_Separation_Accuracy_New <- pm$Observed_4DME_Separation_Distance - pm$UK6Cat_TBS_4DME_Wake_Separation_Distance_New

# RECAT Separation at 1DME set 2.5NM lower bound
pm$Recat_eTBS_0DME_Wake_Separation_Distance_New <- ifelse(pm$Recat_eTBS_0DME_Wake_Separation_Distance < 2.5, 2.5, pm$Recat_eTBS_0DME_Wake_Separation_Distance)
pm$Recat_eTBS_0DME_Wake_Separation_Distance_New_Rounded <- round(pm$Recat_eTBS_0DME_Wake_Separation_Distance_New, 1)

# Forecast Compression set 0NM lower bound
pm$Forecast_ORD_TBS_Compression_New <- ifelse(pm$Forecast_ORD_TBS_Compression < 0, 0, pm$Forecast_ORD_TBS_Compression)
pm$Forecast_ORD_TBS_Compression_New_Rounded <- round(pm$Forecast_ORD_TBS_Compression_New, 1)

# Calculate RECAT separation at 4DME and set 3NM lower bound
pm$Recat_eTBS_4DME_Wake_Separation_Distance_New <- pm$Recat_eTBS_0DME_Wake_Separation_Distance_New + pm$Forecast_ORD_TBS_Compression_New
pm$Recat_eTBS_4DME_Wake_Separation_Distance_New <- ifelse(pm$Recat_eTBS_4DME_Wake_Separation_Distance_New < 3, 3, pm$Recat_eTBS_4DME_Wake_Separation_Distance_New)

# 1.6 Time & distance separation calculation ------------------------------

## D_E4 ## - Actual Separation observed - Distance @ 4DME ##
pm$eTBS_actual_4DME_Distance_Separation <- pm$Observed_4DME_Separation_Accuracy_New + pm$Recat_eTBS_4DME_Wake_Separation_Distance_New

# Adjust J-X (RECAT 3NM) using H-X (UK6 3NM) New Calculations
j_j <- pm[Leader_Aircraft_Type == "A388" &
            Follower_Aircraft_Type == "A388" &
            Observed_4DME_Separation_Accuracy_New > -0.25 &
            Observed_4DME_Separation_Accuracy_New < 5]$Observed_4DME_Separation_Accuracy_New

mean_j_j <- mean(j_j)
sd_j_j <-  sd(j_j)

j_not_j <- pm[Leader_Aircraft_Type == "A388" &
                Follower_Aircraft_Type != "A388" &
                Observed_4DME_Separation_Accuracy_New > -0.25 &
                Observed_4DME_Separation_Accuracy_New < 3]$Observed_4DME_Separation_Accuracy_New

mean_j_not_j <- mean(j_not_j)
sd_j_not_j <-  if (length(j_not_j)==1){
  j_not_j
} else {
  sd(j_not_j)
}

h_x <- pm[Leader_UK_Wake_Cat == "H" &
            Observed_4DME_Separation_Accuracy_New > -0.5 &
            Observed_4DME_Separation_Accuracy_New < 2]$Observed_4DME_Separation_Accuracy_New

mean_h_x <- mean(h_x)
sd_h_x <- sd(h_x)

pm$Observed_4DME_Separation_Accuracy_New_Adj <- ifelse(
  pm$Leader_Aircraft_Type == "A388" & pm$Follower_Aircraft_Type == "A388",
  ((pm$Observed_4DME_Separation_Accuracy_New - mean_j_j) / sd_j_j) * sd_h_x + mean_h_x,
  ifelse(
    pm$Leader_Aircraft_Type == "A388" & pm$Follower_Aircraft_Type != "A388",
    ((pm$Observed_4DME_Separation_Accuracy_New - mean_j_not_j) / sd_j_not_j) * sd_h_x + mean_h_x,
    pm$Observed_4DME_Separation_Accuracy_New
  )
)

## D_E4 ## - Update Actual eTBS Separation observed - Distance @ 4DME ##
pm[Leader_Aircraft_Type == "A388"]$eTBS_actual_4DME_Distance_Separation <-
  pm[Leader_Aircraft_Type == "A388"]$Recat_eTBS_4DME_Wake_Separation_Distance_New + pm[Leader_Aircraft_Type == "A388"]$Observed_4DME_Separation_Accuracy_New_Adj

## D_E1 ## - Actual Separation Observed - Distance @ 1DME ##
pm$eTBS_actual_1DME_Distance_Separation <- pm$eTBS_actual_4DME_Distance_Separation - (pm$Observed_4DME_Separation_Distance - pm$Observed_1DME_Separation_Distance)

## Position Indicators ##
# Floored position of follower from threshold when leader at 1DME with perfect separation
pm$x <- floor(pm$Recat_eTBS_0DME_Wake_Separation_Distance_New)
# Floored position of follower from threshold when leader at 4DME with actual separation
pm$y <- floor(3 + pm$eTBS_actual_4DME_Distance_Separation)

#pm_test <- pm[Leader_Aircraft_Type == "A388"]

## Handle More Data Errors - Remove all Data where position of follower is more than 14NM as there is no segment information for this ##      
pm <- pm[y + 1 < 14]

## Calculate time separations by match id to raw data and subset, then find time by adding the inverse ground speed of each segment ##
pm_list <- lapply(1:nrow(pm), function(i) {
  
  # Get i-th row of performance model
  pm_i <- pm[i]
  
  
  # Get follower segment information (leader and follower pair)
  rd_i_foll <- rd[FP_Date == pm_i$FP_Date & Leader_Callsign == pm_i$Leader_Callsign & Follower_Callsign == pm_i$Follower_Callsign]
  
  # Get leader segment information (leader and its preceding flight)
  rd_i_lead <- rd[FP_Date == pm_i$FP_Date & Follower_Callsign == pm_i$Leader_Callsign]
  
  # Remove any flights with the same callsign on the same day not associated with the leader's preceding pair
  rd_i_lead <- rd_i_lead[Follower_0DME_Time[1] - Follower_0DME_Time < 3600]
  
  
  ## T_P1 - perfect time separation at 1DME ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], pm_i$x, 1)
  pm_i$eTBS_perfect_1DME_Time_Separation <- ifelse(
    TF > 0 & pm_i$x %in% rd_i_foll$DME_Seg, 
    (TF + (pm_i$Recat_eTBS_0DME_Wake_Separation_Distance_New - pm_i$x) / rd_i_foll[DME_Seg == (pm_i$x + 1)]$Ave_Mode_S_GSPD) * 3600, 
    NA
  )
  
  ## T_E4 - actual time separation at 4DME ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], pm_i$y, 5)
  pm_i$eTBS_actual_4DME_Time_Separation <- ifelse(
    TF > 0 & (pm_i$y + 1) %in% rd_i_foll$DME_Seg, 
    (TF + (pm_i$eTBS_actual_4DME_Distance_Separation %% 1) / rd_i_foll[DME_Seg == (pm_i$y + 1)]$Ave_Mode_S_GSPD) * 3600, 
    NA
  )
  
  # Time for leader to get from 4NM to 1NM
  TL <- segmentFlyingTime(rd_i_lead[,c("DME_Seg", "Ave_Mode_S_GSPD")], 3, 1)
  
  ## T_E1 - actual time separation at 1DME ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], pm_i$y, 1)
  pm_i$eTBS_actual_1DME_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & (pm_i$y + 1) %in% rd_i_foll$DME_Seg, 
    (TF + (pm_i$eTBS_actual_4DME_Distance_Separation %% 1 / rd_i_foll[DME_Seg == (pm$y[i] + 1)]$Ave_Mode_S_GSPD) - TL) * 3600, 
    NA
  )
  
  ## T_UK1_TBS - perfect UK6CAT time spacing at 1DME ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 3), 1)
  pm_i$UK6Cat_TBS_1DME_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 4) %in% rd_i_foll$DME_Seg, 
    (TF + (pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New %% 1 / rd_i_foll[DME_Seg == floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 4)]$Ave_Mode_S_GSPD) - TL) * 3600,
    NA
  )
  
  ## T_R1 - perfect RECAT DBS time spacing at 1DME ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$Ref_Recat_Wake_Separation_Distance), 1)
  pm_i$RECAT_DBS_1DME_Time_Separation <- ifelse(TF > 0 , TF * 3600, NA) 
  
  ## T_UK1_DBS - perfect uk DBS time spacing at 1DME ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$UK6Cat_Separation_Distance + 3), 1)
  pm_i$UK6Cat_DBS_1DME_Time_Separation <- ifelse(TF > 0 & TL > 0, (TF - TL) * 3600, NA)
  
  
  ## Under Separated Scenarios  -0.5##
  
  
  ## Perfect eTBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$x - 0.5), 1)
  pm_i$Under_Separated_0.5_eTBS_perfect_1DME_Time_Separation <- ifelse(
    TF > 0 & floor(pm_i$Recat_eTBS_0DME_Wake_Separation_Distance_New + 0.5) %in% rd_i_foll$DME_Seg, 
    (TF + (((pm_i$Recat_eTBS_0DME_Wake_Separation_Distance_New - 0.5) - floor(pm_i$x - 0.5)) / rd_i_foll[DME_Seg == floor(pm_i$x + 0.5)]$Ave_Mode_S_GSPD)) * 3600, 
    NA
  )
  
  ## Perfect UK6Cat TBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 2.5), 1)
  pm_i$Under_Separated_0.5_UK6Cat_TBS_1DME_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 3.5) %in% rd_i_foll$DME_Seg, 
    (TF + (((pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New - 0.5) %% 1) / rd_i_foll[DME_Seg == floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 3.5)]$Ave_Mode_S_GSPD) - TL) * 3600, 
    NA
  )
  
  ## RECAT DBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$Ref_Recat_Wake_Separation_Distance - 0.5), 1)
  pm_i$Under_Separated_0.5_RECAT_DBS_1DME_Time_Separation <- ifelse(
    TF > 0 & floor(pm_i$Ref_Recat_Wake_Separation_Distance + 0.5) %in% rd_i_foll$DME_Seg, 
    (TF + (((pm_i$Ref_Recat_Wake_Separation_Distance - 0.5) %% 1) / rd_i_foll[DME_Seg == floor(pm_i$Ref_Recat_Wake_Separation_Distance + 0.5)]$Ave_Mode_S_GSPD)) * 3600, 
    NA
  )
  
  ## UK6Cat DBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$UK6Cat_Separation_Distance + 2.5), 1)
  pm_i$Under_Separated_0.5_UK6Cat_DBS_1DME_Time_Separation <- ifelse(
    TF > 0 & TL > 0, 
    (TF + (((pm_i$UK6Cat_Separation_Distance - 0.5) %% 1) / rd_i_foll[DME_Seg == floor(pm_i$UK6Cat_Separation_Distance + 3.5)]$Ave_Mode_S_GSPD) - TL) * 3600,
    NA
  )
  
  
  ## Under Separated Scenarios  -1.0##
  
  ## Perfect eTBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$x - 1), 1)
  pm_i$Under_Separated_1.0_eTBS_perfect_1DME_Time_Separation <- ifelse(
    TF > 0 & floor(pm_i$Recat_eTBS_0DME_Wake_Separation_Distance_New) %in% rd_i_foll$DME_Seg, 
    (TF + (((pm_i$Recat_eTBS_0DME_Wake_Separation_Distance_New - 1) - floor(pm_i$x - 1)) / rd_i_foll[DME_Seg == floor(pm_i$x)]$Ave_Mode_S_GSPD)) * 3600, 
    NA
  )
  
  ## Perfect UK6Cat TBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 2), 1)
  pm_i$Under_Separated_1.0_UK6Cat_TBS_1DME_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 3) %in% rd_i_foll$DME_Seg, 
    (TF + (((pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New - 1) %% 1) / rd_i_foll[DME_Seg == floor(pm_i$UK6Cat_TBS_4DME_Wake_Separation_Distance_New + 3)]$Ave_Mode_S_GSPD) - TL) * 3600, 
    NA
  )
  
  ## RECAT DBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$Ref_Recat_Wake_Separation_Distance - 1), 1)
  pm_i$Under_Separated_1.0_RECAT_DBS_1DME_Time_Separation <- ifelse(
    TF > 0 & floor(pm_i$Ref_Recat_Wake_Separation_Distance) %in% rd_i_foll$DME_Seg, 
    (TF + (((pm_i$Ref_Recat_Wake_Separation_Distance - 1) %% 1) / rd_i_foll[DME_Seg == floor(pm_i$Ref_Recat_Wake_Separation_Distance)]$Ave_Mode_S_GSPD)) * 3600, 
    NA
  )
  
  ## UK6Cat DBS ##
  TF <- segmentFlyingTime(rd_i_foll[,c("DME_Seg", "Ave_Mode_S_GSPD")], floor(pm_i$UK6Cat_Separation_Distance + 2), 1)
  pm_i$Under_Separated_1.0_UK6Cat_DBS_1DME_Time_Separation <- ifelse(
    TF > 0 & TL > 0 & floor(pm_i$UK6Cat_Separation_Distance + 3) %in% rd_i_foll$DME_Seg, 
    (TF + (((pm_i$UK6Cat_Separation_Distance - 1) %% 1) / rd_i_foll[DME_Seg == floor(pm_i$UK6Cat_Separation_Distance + 3)]$Ave_Mode_S_GSPD) - TL) * 3600,
    NA
  )
  
  message("Completed calculations for ", paste0(pm_i[,c("FP_Date", "Leader_Callsign", "Follower_Callsign")], collapse = "_"), " (", i, "/", nrow(pm), ")")
  
  return(pm_i)
  
})

# Combine all calculated rows into data.table
pm2 <- rbindlist(pm_list)

# 1.7 Observed 4DME separation accuracy histograms for A338s --------------

png(file.path(outdir, "Obs 4DME Sep Acc New A388 Lead.png"), height = 900, width = 1200)
hist(
  pm[Leader_Aircraft_Type == "A388"]$Observed_4DME_Separation_Accuracy_New,
  breaks = seq(2, 9, 0.2),
  xlim = c(2,9),
  prob = T,
  col = "light green",
  main = "Obs. 4DME Sep. Acc. New A388 Lead",
  xlab = "Separation Accuracy (NM)"
)
dev.off()

png(file.path(outdir, "Obs 4DME Sep Acc New Adj A388 Lead.png"), height = 900, width = 1200)
hist(
  pm[Leader_Aircraft_Type == "A388"]$Observed_4DME_Separation_Accuracy_New_Adj,
  breaks = seq(-2, 6, 0.2),
  xlim = c(-1, 4),
  prob = T,
  col = "light blue",
  main = "Obs. 4DME Sep. Acc. New Adj A388 Lead",
  xlab = "Separation Accuracy (NM)"
)
dev.off()

png(file.path(outdir, "Obs 4DME Sep Acc New Adj A388 Pair.png"), height = 900, width = 1200)
hist(
  pm[Leader_Aircraft_Type == "A388" & Follower_Aircraft_Type == "A388"]$Observed_4DME_Separation_Accuracy_New_Adj,
  breaks = seq(-2, 6, 0.2),
  xlim = c(-1, 4),
  prob = T,
  col = "light blue",
  main = "Obs. 4DME Sep. Acc. New Adj A388 Pair",
  xlab = "Separation Accuracy (NM)"
)
dev.off()

png(file.path(outdir, "Obs 4DME Sep Acc New Adj A388 Lead Non-A388 Foll.png"), height = 900, width = 1200)
hist(
  pm[Leader_Aircraft_Type == "A388" & Follower_Aircraft_Type != "A388"]$Observed_4DME_Separation_Accuracy_New_Adj,
  breaks = seq(-2, 6, 0.2),
  xlim = c(-1, 4),
  prob = T,
  col = "light blue",
  main = "Obs. 4DME Sep. Acc. New Adj A388 Lead Non-A388 Foll",
  xlab = "Separation Accuracy (NM)"
)
dev.off()

# 1.8 Export eTBS calculations --------------------------------------------

## Change column names (previously done by index - not good!) ##

names(pm2)[names(pm2) == "FP_Date"] <- "Date"

# These are most likely matches but the original two columns do not refer to DBS
# or DME!!! Are these correct?? (Remove this comment if checked to be correct)
names(pm2)[names(pm2) == "UK6Cat_Separation_Distance"] <- "UK6Cat_DBS_4DME_Wake_Separation_Distance"
names(pm2)[names(pm2) == "Ref_Recat_Wake_Separation_Distance"] <- "Recat_DBS_1DME_Wake_Separation_Distance"

# Changed to match naming scheme of Ref_Recat_Wake_Separation_Time, is this
# correct? (Remove this comment if checked to be correct)
names(pm2)[names(pm2) == "UK6Cat_Separation_Time"] <- "Ref_UK6Cat_Separation_Time"

# Below column name changes should be self-explanatory

names(pm2)[names(pm2) == "UK6Cat_TBS_4DME_Wake_Separation_Distance"] <- "UK6Cat_TBS_4DME_Wake_Separation_Distance_Old"
names(pm2)[names(pm2) == "UK6Cat_TBS_4DME_Wake_Separation_Distance_New"] <- "UK6Cat_TBS_4DME_Wake_Separation_Distance"
names(pm2)[names(pm2) == "UK6Cat_TBS_4DME_Wake_Separation_Distance_New_Rounded"] <- "UK6Cat_TBS_4DME_Wake_Separation_Distance_Rounded"

names(pm2)[names(pm2) == "Observed_4DME_Separation_Accuracy"] <- "Observed_4DME_Separation_Accuracy_old"
names(pm2)[names(pm2) == "Observed_4DME_Separation_Accuracy_New"] <- "Observed_4DME_Separation_Accuracy"

names(pm2)[names(pm2) == "Recat_eTBS_0DME_Wake_Separation_Distance"] <- "Recat_eTBS_0DME_Wake_Separation_Distance_old"
names(pm2)[names(pm2) == "Recat_eTBS_0DME_Wake_Separation_Distance_New"] <- "Recat_eTBS_0DME_Wake_Separation_Distance"
names(pm2)[names(pm2) == "Recat_eTBS_0DME_Wake_Separation_Distance_New_Rounded"] <- "Recat_eTBS_0DME_Wake_Separation_Distance_Rounded"

names(pm2)[names(pm2) == "Forecast_ORD_TBS_Compression"] <- "Forecast_ORD_TBS_Compression_old"
names(pm2)[names(pm2) == "Forecast_ORD_TBS_Compression_New"] <- "Forecast_ORD_TBS_Compression"
names(pm2)[names(pm2) == "Forecast_ORD_TBS_Compression_New_Rounded"] <- "Forecast_ORD_TBS_Compression_Rounded"

names(pm2)[names(pm2) == "Recat_eTBS_4DME_Wake_Separation_Distance"] <- "Recat_eTBS_4DME_Wake_Separation_Distance_old"
names(pm2)[names(pm2) == "Recat_eTBS_4DME_Wake_Separation_Distance_New"] <- "Recat_eTBS_4DME_Wake_Separation_Distance"

## Export Total Calculations for Sense Checking ##
fwrite(pm2, file.path(outdir, "eTBS_Calculations.csv"))

## Calculate D_A4 Then Compare with Perfect. Remove Anything With A Variability More Than 2NM or 3NM if A388##
pm2_non <- pm2[(Observed_4DME_Separation_Distance - UK6Cat_TBS_4DME_Wake_Separation_Distance) < 2 & Leader_Aircraft_Type != "A388"]
pm2_A388 <- pm2[(Observed_4DME_Separation_Distance - UK6Cat_TBS_4DME_Wake_Separation_Distance) < 3 & Leader_Aircraft_Type == "A388"]  
pm3 <- rbind(pm2_non, pm2_A388)

## Export Filtered Calculations for Sense Checking ##
fwrite(pm3, file.path(outdir, "eTBS_Calculations_Filtered.csv"))

# Last update to re-arrange the columns to match previous WaPT outputs
pm4 <- data.table(
  MatchID = paste(pm3$Date, pm3$Leader_Callsign, pm3$Follower_Callsign),
  X = 1:nrow(pm3),
  pm3[,c(
    "Date",
    "Leader_Callsign",
    "Leader_Aircraft_Type",
    "Leader_UK_Wake_Cat",
    "Leader_Recat_Wake_Cat",
    "Follower_Callsign",
    "Follower_Aircraft_Type",
    "Follower_UK_Wake_Cat",
    "Follower_Recat_Wake_Cat",
    #Landing runway
    "UK6Cat_DBS_4DME_Wake_Separation_Distance",
    "Recat_DBS_1DME_Wake_Separation_Distance",
    "Ref_UK6Cat_Separation_Time",
    "Ref_Recat_Wake_Separation_Time",
    "Follower_Forecast_TBS_Wind_Effect",
    "Follower_Forecast_eTBS_Wind_Effect",
    #RWY_HDG
    #Observed_AGI_Surface_Crosswind
    "Observed_AGI_Surface_Headwind",
    "Observed_AGI_Surface_Wind_SPD",
    "Observed_AGI_Surface_Wind_HDG",
    "Observed_1DME_Separation_Distance",
    "Observed_1DME_Separation_Distance",
    "Observed_1DME_Separation_Time",
    "Observed_4DME_Separation_Time",
    "Observed_Follower_TBS_Wind_Effect",
    "Observed_Follower_eTBS_Wind_Effect",
    "UK6Cat_TBS_4DME_Wake_Separation_Distance",
    "Observed_4DME_Separation_Accuracy",
    "Recat_eTBS_0DME_Wake_Separation_Distance",
    "Forecast_ORD_TBS_Compression",
    "Recat_eTBS_4DME_Wake_Separation_Distance",
    "Ave_Mode_S_GSPD",
    "Ave_Mode_S_IAS",
    "eTBS_actual_4DME_Distance_Separation",
    "eTBS_actual_1DME_Distance_Separation",
    "eTBS_perfect_1DME_Time_Separation",
    "eTBS_actual_4DME_Time_Separation",
    "eTBS_actual_1DME_Time_Separation",
    "UK6Cat_TBS_1DME_Time_Separation",
    "RECAT_DBS_1DME_Time_Separation",
    "UK6Cat_DBS_1DME_Time_Separation",
    "Under_Separated_0.5_eTBS_perfect_1DME_Time_Separation",
    "Under_Separated_0.5_UK6Cat_TBS_1DME_Time_Separation",
    "Under_Separated_0.5_RECAT_DBS_1DME_Time_Separation",
    "Under_Separated_0.5_UK6Cat_DBS_1DME_Time_Separation",
    "Under_Separated_1.0_eTBS_perfect_1DME_Time_Separation",
    "Under_Separated_1.0_UK6Cat_TBS_1DME_Time_Separation",
    "Under_Separated_1.0_RECAT_DBS_1DME_Time_Separation",
    "Under_Separated_1.0_UK6Cat_DBS_1DME_Time_Separation"
  )]
)

## Output for WaPT
fwrite(pm4, file.path(outdir, "eTBS_Calculations_WaPT.csv"))

# ----------------------------------------------------------------------- #
# 2. eTBS Performance Analysis --------------------------------------------
# ----------------------------------------------------------------------- #

# Import from eTBS calculations
data1 <- pm2
# data1 <- fread(file.path(outdir, "eTBS_Calculations.csv"))

# Drop other variables
rm(list = ls()[!(ls() %in% c("data1", "outdir"))])

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


# 2.1 Definitions ---------------------------------------------------------

# wakes <- c("SUPER", "HEAVY", "UPPER", "MEDIUM", "SMALL", "LIGHT")
wakes <- LETTERS[1:6]

wake_levels <- paste0(rep(wakes, each = length(wakes)), "-", rep(wakes, times = length(wakes)))

sep_scheme <- fread("Input/Separation Schemes.csv")
sep_scheme$LF_Pair <- factor(sep_scheme$LF_Pair, levels = wake_levels)

# 2.2 Calculate new columns -----------------------------------------------

data1 <- mutate(
  data1,
  RECAT_Wake_Type_Pair = paste(Leader_Recat_Wake_Cat, Follower_Aircraft_Type, sep = "-"),
  RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels),
  UK_Wake_Type_Pair = paste(Leader_UK_Wake_Cat, Follower_Aircraft_Type, sep = "-"),
  UK_Wake_Pair = factor(paste(Leader_UK_Wake_Cat, Follower_UK_Wake_Cat, sep = "-"), levels = wake_levels),
  eTBS_actual_1DME_Time_Accuracy = eTBS_actual_1DME_Time_Separation - Ref_Recat_Wake_Separation_Time,
  eTBS_actual_1DME_Distance_Accuracy = eTBS_actual_1DME_Distance_Separation - Recat_eTBS_0DME_Wake_Separation_Distance,
  UK_actual_1DME_Time_Accuracy = Observed_1DME_Separation_Time - Ref_UK6Cat_Separation_Time,
  UK_actual_1DME_Distance_Accuracy = Observed_1DME_Separation_Distance - UK6Cat_TBS_4DME_Wake_Separation_Distance,
  eTBS_actual_4DME_Distance_Accuracy = eTBS_actual_4DME_Distance_Separation - Recat_eTBS_4DME_Wake_Separation_Distance,
  UK_actual_4DME_Distance_Accuracy = Observed_4DME_Separation_Distance - UK6Cat_TBS_4DME_Wake_Separation_Distance
)

# Block the 1DME Time Separation Accuracy

data1 <- mutate(
  data1,
  eTBS_actual_1DME_Time_Accuracy_Group = cut(eTBS_actual_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)), 
  eTBS_actual_1DME_Distance_Accuracy_Group = cut(eTBS_actual_1DME_Distance_Accuracy, breaks = c(-Inf, -0.55, -0.05, Inf)),
  UK_actual_1DME_Time_Accuracy_Group = cut(UK_actual_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)), 
  Observed_AGI_Surface_Wind_SPD_Group = cut(Observed_AGI_Surface_Wind_SPD, breaks = c(0,4,7,10,13,16,Inf), right=FALSE)
)

data1$Recat_eTBS_0DME_All_Separation_Distance_Cap <- ifelse(data1$Recat_eTBS_0DME_All_Separation_Distance <2.5, 2.5, data1$Recat_eTBS_0DME_All_Separation_Distance)
data1$Forecast_ORD_TBS_Compression_Cap <- ifelse(data1$Forecast_ORD_TBS_Compression < 0, 0, data1$Forecast_ORD_TBS_Compression)
data1 <- mutate(data1, eTBS_Uncapped_4DME_Distance_Separation = round(Recat_eTBS_0DME_All_Separation_Distance_Cap, 1) + Forecast_ORD_TBS_Compression)
data1$Capped_at_3NM <- ifelse(data1$eTBS_Uncapped_4DME_Distance_Separation < 3, 1, 0)

data1 <- mutate(data1, Aircraft_Type_Pair = paste(Leader_Aircraft_Type, Follower_Aircraft_Type, sep = "-"))

plot(data1$Recat_eTBS_4DME_All_Separation_Distance, data1$eTBS_Uncapped_4DME_Distance_Separation)

# 3.2 Find out when capping at 3NM will occur -----------------------------

data1$Capped_at_3NM <- ifelse(data1$eTBS_Uncapped_4DME_Distance_Separation < 3, 1, 0)

date_breakdown <- table(data1$Date, data1$Capped_at_3NM)

fwrite(date_breakdown, file.path(outdir, "date_breakdown.csv"))

type_data <- filter(data1, Capped_at_3NM == 1) %>%
  group_by(Date, RECAT_Wake_Pair) %>%
  summarise(Total = n(), MeanWE = mean(Follower_Forecast_eTBS_Wind_Effect, na.rm = TRUE)) %>%
  as.data.table()

ac_type_data <- filter(data1, Capped_at_3NM == 1) %>%
  group_by(Date, Leader_Aircraft_Type) %>%
  summarise(Total = n(), MeanWE = mean(Follower_Forecast_eTBS_Wind_Effect, na.rm = TRUE)) %>%
  as.data.table()

fwrite(type_data, file.path(outdir, "type_breakdown.csv"))
fwrite(ac_type_data, file.path(outdir, "ac_type_breakdown.csv"))


# 3.3 Generate Word Document ----------------------------------------------

# Create a word document
doc <- read_docx()
doc <- body_add_par(doc, "Distributions of Actual Time Separations for eTBS", style = "heading 1")

# Output Graphs for each Wake Pair: Actual 1DME Time Separation

for (i in 1:nrow(sep_scheme)){
  
  # Select the wake pair and grab the recat and uk data matching this pair.
  
  wake_pair <- sep_scheme$LF_Pair[i]
  recat_time <- sep_scheme$RECAT_Time_Reference[i]
  
  wake_pair_data_recat_tbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$eTBS_perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$eTBS_actual_1DME_Time_Separation
  
  plot_title_recat_tbs <- paste("1DME Perfect Time Separation for eTBS: ", wake_pair, sep = "")
  plot_title_recat_dbs <- paste("1DME Actual Time Separation for eTBS: ", wake_pair, sep = "")
  
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
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$eTBS_perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair & Observed_AGI_Surface_Wind_SPD >= -2 & Observed_AGI_Surface_Wind_SPD <= 2)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$eTBS_actual_1DME_Time_Separation
  
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
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$eTBS_perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$eTBS_actual_1DME_Time_Separation
  
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
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$eTBS_perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$RECAT_DBS_1DME_Time_Separation
  
  plot_title_recat_tbs <- paste("1DME Perfect Separation for eTBS: ", wake_pair, sep = "")
  plot_title_recat_dbs <- paste("1DME Perfect Separation for RECAT DBS: ", wake_pair, sep = "")
  
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

# Output Graphs for each Wake Pair Separately: Perfect 1DME Time Separation - 0.5Nm

doc <- body_add_par(doc, "Distributions of Perfect Time Separations -0.5Nm for eTBS/RECAT", style = "heading 1")

for (i in 1:nrow(sep_scheme)){
  
  # Select the wake pair and grab the recat and uk data matching this pair.
  
  wake_pair <- sep_scheme$LF_Pair[i]
  recat_time <- sep_scheme$RECAT_Time_Reference[i]
  
  wake_pair_data_recat_tbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$Under_Separated_0.5_eTBS_perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$Under_Separated_0.5_RECAT_DBS_1DME_Time_Separation
  
  plot_title_recat_tbs <- paste("1DME Perfect Separation -0.5NM for eTBS: ", wake_pair, sep = "")
  plot_title_recat_dbs <- paste("1DME Perfect Separation -0.5NM for RECAT DBS: ", wake_pair, sep = "")
  
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

# Output Graphs for each Wake Pair Separately: Perfect 1DME Time Separation - 1.0Nm

doc <- body_add_par(doc, "Distributions of Perfect Time Separations -1.0Nm for eTBS/RECAT", style = "heading 1")

for (i in 1:nrow(sep_scheme)){
  
  # Select the wake pair and grab the recat and uk data matching this pair.
  
  wake_pair <- sep_scheme$LF_Pair[i]
  recat_time <- sep_scheme$RECAT_Time_Reference[i]
  
  wake_pair_data_recat_tbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_tbs$plot_data <- wake_pair_data_recat_tbs$Under_Separated_1.0_eTBS_perfect_1DME_Time_Separation
  wake_pair_data_recat_dbs <- filter(data1, RECAT_Wake_Pair == wake_pair)
  wake_pair_data_recat_dbs$plot_data <- wake_pair_data_recat_dbs$Under_Separated_1.0_RECAT_DBS_1DME_Time_Separation
  
  plot_title_recat_tbs <- paste("1DME Perfect Separation -1.0NM for eTBS: ", wake_pair, sep = "")
  plot_title_recat_dbs <- paste("1DME Perfect Separation -1.0NM for RECAT DBS: ", wake_pair, sep = "")
  
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
print(doc, target = file.path(outdir, "eTBS_Wake_Pair_Distributions.docx"))


# 3.4 Additional tables and plots -----------------------------------------

# Tables for the Wake - Type Pairs and Time Under-separation Rate

t1 <- table(data1$RECAT_Wake_Type_Pair, data1$eTBS_actual_1DME_Time_Accuracy_Group)

t2 <- table(data1$UK_Wake_Type_Pair, data1$UK_actual_1DME_Time_Accuracy_Group )

fwrite(as.data.table(t1), file.path(outdir, "eTBS_1DME_Time_Accuracy_Wake_Type.csv"))

fwrite(as.data.table(t2), file.path(outdir, "UK6_1DME_Time_Accuracy_Wake_Type.csv"))

t3 <- table(data1$RECAT_Wake_Pair, data1$eTBS_actual_1DME_Time_Accuracy_Group)

t4 <- table(data1$UK_Wake_Pair, data1$UK_actual_1DME_Time_Accuracy_Group )

fwrite(as.data.table(t3), file.path(outdir, "eTBS_1DME_Time_Accuracy_Wake_Wake.csv"))

fwrite(as.data.table(t4), file.path(outdir, "UK6_1DME_Time_Accuracy_Wake_Wake.csv"))

t5 <- group_by(data1, RECAT_Wake_Type_Pair) %>%
  summarise(        CounTS = sum(!is.na(eTBS_actual_1DME_Time_Accuracy)),
                    MeanTS = mean(eTBS_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    MedianTC = median(eTBS_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    StdTS = sd(eTBS_actual_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()

t6 <- group_by(data1, UK_Wake_Type_Pair) %>%
  summarise(        CounTS = sum(!is.na(UK_actual_1DME_Time_Accuracy)),
                    MeanTS = mean(UK_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    MedianTC = median(UK_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    StdTS = sd(UK_actual_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()

fwrite(as.data.table(t5), file.path(outdir, "eTBS_1DME_Time_Accuracy_Stats_Wake_Type.csv"))

fwrite(as.data.table(t6), file.path(outdir, "UK6_1DME_Time_Accuracy_Stats_Wake_Type.csv"))

t7 <- group_by(data1, RECAT_Wake_Pair) %>%
  summarise(        CounTS = sum(!is.na(eTBS_actual_1DME_Time_Accuracy)),
                    MeanTS = mean(eTBS_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    MedianTC = median(eTBS_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    StdTS = sd(eTBS_actual_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()

t8 <- group_by(data1, UK_Wake_Pair) %>%
  summarise(        CounTS = sum(!is.na(UK_actual_1DME_Time_Accuracy)),
                    MeanTS = mean(UK_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    MedianTC = median(UK_actual_1DME_Time_Accuracy, na.rm = TRUE),
                    StdTS = sd(UK_actual_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()

fwrite(as.data.table(t7), file.path(outdir, "eTBS_1DME_Time_Accuracy_Stats_Wake_Wake.csv"))

fwrite(as.data.table(t8), file.path(outdir, "UK6_1DME_Time_Accuracy_Stats_Wake_Wake.csv"))

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

