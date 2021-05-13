# ----------------------------------------------------------------------- #
#                 |                                                       #
#  Title          |  IA Performance Model Pre-Process                     #
#                 |                                                       #
#  Author         |  Michael Cowham                                       #
#                 |                                                       #
#  Date Modified  |  16/10/2020                                           #
#                 |                                                       #
#  Description    |  To grow the available IA Performance Model Data      #
#                 |                                                       #
# ----------------------------------------------------------------------- #
#
# Version History ------------------------------------------------------- #
#
# 1 Initial version
#
# ----------------------------------------------------------------------- #

rm(list = ls())

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
Script version: v1.0
Sprint task: IA-209
User: Michael
Database Used: LVNL_UTMA_Validation
Notes: Not sure if this is going to work....
"
# Version 

version <- "V1.0"

# User (not ideal, but just to get the GWCS Data)

user <- "Michael Cowham"

# Use database or local copy of vw_eTBS_Performance_Model and Segment Data
use_database <- F

local_data_folder <- "LVNL Exports 2020-10-05"

# Database dataset - WHEN use_database IS TRUE
db_ip <- "192.168.1.32"
db_name <- "LVNL_UTMA_Validation"

# NOTE: For date_filter, specify date(s) to query - NA for all dates
date_filter <- NA
# date_filter <- "01/10/2019"

# Only set True if running files for the first time since database run. Saves to local file.
save_pm <- F

# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Generate sub-directory in Output folder for this run
outdir <- file.path("Pre-Process Output", version)
dir.create(outdir)

# Write a readme file for this run
sink(file.path(outdir, "readme.txt"))
cat(readme_txt)
sink()

# Performance Model Location

pm_dir <- file.path("Reference", local_data_folder, "pm.csv")


# Get Connection
con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, db_name, "ruser", "Th!nkruser"
))


# ----------------------------------------------------------------------- #
# ii. Parameters ----------------------------------------------------------
# ----------------------------------------------------------------------- #

# Minimum Radar Separation

radar_min <- 3

# Wake Categories

wake_cats <- as.data.table(c("A", "B", "C", "D", "E", "F"))
names(wake_cats) <- c("Wake")

# List of leaders to expand

leader_list <- c("A")

# List of followers to expand

follower_list <- c("F")

# ----------------------------------------------------------------------- #
# Functions
# ----------------------------------------------------------------------- #

### TEMP
pm <- mutate(pm, Bolster_Flag = 0)

# Get a table of the wake pairs we want to bolster
a <- merge(leader_list, wake_cats) 
names(a) <- c("Leader_WTC", "Follower_WTC")
b <- merge(follower_list, wake_cats) %>% select(2, everything())
names(b) <- c("Leader_WTC", "Follower_WTC")
bolster_pairs <- rbind(a, b) %>% unique()
rm(a, b)

# Code to retrieve the relevant pairs
max_obs <- 1000
for (i in 1:nrow(bolster_pairs)){
  fol_wake <- bolster_pairs$Follower_WTC[i]
  lead_wake <- bolster_pairs$Leader_WTC[i]
  pm_iter <- filter(pm, Follower_Recat_Wake_Cat == fol_wake) %>% mutate(Leader_Recat_Wake_Cat = lead_wake)
  # select relevant columns
  pm_iter <- pm_iter[!is.na(pm_iter$Forecast_ORD_Compression_Uncapped),]
  pm_iter <- mutate(pm_iter, Bolster_Flag = 1)
  if (nrow(pm_iter) > max_obs){pm_iter <- pm_iter[1:max_obs,]}
  if (i == 1){pm_bolst <- pm_iter} else {pm_bolst <- rbind(pm_bolst, pm_iter)}
}




# ----------------------------------------------------------------------- #
# 1. Import Data
# ----------------------------------------------------------------------- #

# ------------------------------- Reference ----------------------------- #

# 1.1. Get the ICAO4 Wake category pair distances
dbs_wake_turbulence <- sqlQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence")

# 1.2 Get the ICAO4 Aircraft type to wake mapping
aircraft_type_to_wake_legacy <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy") %>% unique()

# 1.5 Get the RECAT Aircraft Type to Wake for matching with Heathrow
aircraft_type_to_wake <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake") %>% unique() %>% select(-c("Aircraft_Class"))

recat_wake_turbulence <- sqlQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Dist") %>% mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852)

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

# Load in the 6 GWCS data sets
for (sepdist in 3:8){
  gwcs_file <-  paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\3. GWCS\\LVNL Data\\vw_Mode_S_Wind_Forecast_", sepdist, "nm.csv", sep = "")
  gwcs_dist <- fread(file = gwcs_file, na.strings = c("NA", "NULL") )
  
  if (sepdist == 3){
    gwcs_data <- gwcs_dist
  } else {
    gwcs_data <- rbind(gwcs_data, gwcs_dist)
  }
  rm(gwcs_dist)
}


gwcs_data <- mutate(gwcs_data,  Sep_Dist = Forecast_Seg_Max - Forecast_Seg_Min + 1, 
                                qc_flag = ifelse(Observed_Max_RTT >= (Forecast_Seg_Min + Sep_Dist - 2) & Observed_Flying_Time >= (Sep_Dist * 20 - 40) & abs(Observed_Ave_Mode_S_GSPD - Observed_Track_GSPD) <= 20, 1, 0))

# Extract the relevant fields from the GWCS data.  Date, Callsign, Sep Distance, GWCS Forecast Value, Anemo Speed and Anemo Heading 

gwcs_merge <- select(gwcs_data, FP_Date, Time_At_4DME, Callsign, Forecast_Wind_Effect_IAS, Sep_Dist)

# ----------------------------------------------------------------------- #

# -------------------------- Segment Data ------------------------------- #


pm_filter <- filter(pm, Leader_Recat_Wake_Cat %in% leader_list)

# Join each leader with every wake category as follower

pm_all_wake <- merge(pm_filter, wake_cats)

# Find the relevant separation distance

pm_all_wake <- left_join(pm_all_wake, recat_wake_turbulence, by = c("Leader_Recat_Wake_Cat" = "Leader_WTC", "Wake" = "Follower_WTC"))
pm_all_wake <- mutate(pm_wake_all, Reference_Separation_Distance = ifelse(is.na(Reference_Wake_Separation_Distance), radar_min, Reference_Wake_Separation_Distance))

# Join to the GWCS Data
# NOTE THIS SHOULD BE FOLLOWER HERE?  BUT THERE IS NO FOLLOWER?  COULD JUST USE THE SINGLE FOLLOWER?
pm_all_wake <- inner_join(pm_all_wake, gwcs_merge, by = c("FP_Date" = "FP_Date", "Leader_Callsign" = "Callsign", "Reference_Separation_Distance" = "Sep_Dist")) %>% filter(abs(Leader_4DME_Time - Time_At_4DME) < 3600)



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

# Add RECAT L-F Pair
pm <- mutate(pm, LF_Pair_RECAT = paste0(Leader_Recat_Wake_Cat, "-", Follower_Recat_Wake_Cat))

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




###### BOLSTER STUFF



# ----------------------------------------------------------------------- #
# Get the assumed delivery separation accuracy at 4DME
# ----------------------------------------------------------------------- 

# Get Schiphol 0DME Separation Accuracy Standard Deviation
pm <- inner_join(pm, sep_adjust, by=c("LF_Pair_ICAO4"))

# Get Assumed Delivery Separation Accuracy at 4DME using joined Schiphol Accuracy Mean and Standard Deviation data
pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy_STD = sigma_h*((ICAO_DBS_4DME_Separation_Accuracy - Mean_Accuracy)/SD_Accuracy)) #USE 4DME

# Filter PM data to only include pairs with sep accuracy below icao_max_sep_acc
pm_filt <- filter(pm, ICAO_DBS_4DME_Separation_Accuracy <= icao_max_sep_acc)

# Use method from Plan (1) or Mike's method with Quantiles (2)
if (sep_adj_method == 1){
  pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy = mu_h + ICAO_Assumed_4DME_Separation_Accuracy_STD)
} else {
  
  # Get percentage of Separation accuracy under 0 and find averages per Wake Cat
  #pc_under_0 <- pm_filt %>% group_by(Legacy_Wake, LF_Pair_ICAO4)
  #pc_under_0 <- summarise(pc_under_0, Quantile_0 = quantile(ICAO_Assumed_4DME_Separation_Accuracy_STD, pc_under)) %>% ungroup() #USE 4DME
  
  # Join to PM data and use as Mean
  pm <- inner_join(pm, sep_adjust_quantiles, by=c("LF_Pair_ICAO4", "Legacy_Wake"))
  pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy = ICAO_Assumed_4DME_Separation_Accuracy_STD - Qle_0 - 0.05)
}

# filter out results with a sep accuracy < lambda
pm <- filter(pm, ICAO_Assumed_4DME_Separation_Accuracy > lambda)

# Initial plots of Adjusted Separation Accuracy
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_DBS_4DME_Separation_Accuracy, y = ..density..)) + xlim(-2, 3)
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_DBS_4DME_Separation_Accuracy, y = ..density..)) + xlim(-2, 3) + facet_wrap(~LF_Pair_ICAO4)
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_Assumed_4DME_Separation_Accuracy, y = ..density..)) + xlim(-3, 3)
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_Assumed_4DME_Separation_Accuracy, y = ..density..)) + xlim(-3, 3) + facet_wrap(~LF_Pair_ICAO4)

# TEMP

# Add 0DME/1DME Separation Difference
pm <- mutate(pm, Thr_1DME_Observed_Time_Sep_Diff = Observed_1DME_Separation_Time - Observed_0DME_Separation_Time)

ggplot(data = pm) + geom_histogram(mapping=aes(x = Thr_1DME_Observed_Time_Sep_Diff, y = ..density..)) + xlim(-20, 20)
ggplot(data = pm) + geom_histogram(mapping=aes(x = Thr_1DME_Observed_Time_Sep_Diff, y = ..density..)) + xlim(-20, 20) + facet_wrap(~LF_Pair_ICAO4)
ggplot(data = pm) + geom_histogram(mapping=aes(x = Thr_1DME_Observed_Time_Sep_Diff, y = ..density..)) + xlim(-20, 20) + facet_wrap(~LF_Pair_RECAT)

sep_time_diff_icao <- pm %>% group_by(LF_Pair_ICAO4) %>% summarise(Count = n(),
                                                                   Mean = mean(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T),
                                                                   Median = median(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T),
                                                                   STD = sd(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T)) %>% ungroup()

sep_time_diff_recat <- pm %>% group_by(LF_Pair_RECAT) %>% summarise(Count = n(),
                                                                   Mean = mean(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T),
                                                                   Median = median(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T),
                                                                   STD = sd(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T)) %>% ungroup()

sep_time_diff_tot <- pm  %>% summarise(Count = n(),
                                                                    Mean = mean(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T),
                                                                    Median = median(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T),
                                                                    STD = sd(Thr_1DME_Observed_Time_Sep_Diff, na.rm=T)) %>% ungroup()


sep_time_diff_recat_below <- filter(sep_time_diff_recat, Mean < 0)
#fwrite(sep_time_diff_tot, "sep_time_1-0_all.csv")
#fwrite(sep_time_diff_icao, "sep_time_1-0_icao_all.csv")
#fwrite(sep_time_diff_recat, "sep_time_1-0_recat_all.csv")

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
no_of_iterations <- nrow(pm)
#no_of_iterations <- 100

pm_list <- lapply(1:no_of_iterations, function(i) {
  
  # ----------------------------------------------------------------------- #
  # Get Relevant Data
  # ----------------------------------------------------------------------- #
  # Get i-th row of performance model

  
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
  D_ICAOD <- pm_i$ICAO_DBS_Wake_Separation_Distance 
  
  # Get the 0-1DME GSPD Difference for sense checking Actual 1DME and 0DME values
  pm_i$Thr_1DME_GSPD_Diff <- Get_GSPD_Diff(leader_segs, follower_segs, 0)

  # ----------------------------------------------------------------------- #
  # Get Relevant Outputs (Not Under-Separated)
  # ----------------------------------------------------------------------- #
  
  pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0)
  pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RRT, Under_Sep=0)
  pm_i$ICAO_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_ICAOD, Under_Sep=0)
  pm_i$Actual_1DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 0)
  pm_i$Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0)
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-0.5)
  # ----------------------------------------------------------------------- #
  
  pm_i$Under_0.5_Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0.5)
  pm_i$Under_0.5_Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RRT, Under_Sep=0.5)
  pm_i$Under_0.5_ICAO_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_ICAOD, Under_Sep=0.5)
  pm_i$Under_0.5_Actual_1DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 0.5)
  pm_i$Under_0.5_Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0.5)
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-1.0)
  # ----------------------------------------------------------------------- #
  
  pm_i$Under_1.0_Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=1)
  pm_i$Under_1.0_Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RRT, Under_Sep=1)
  pm_i$Under_1.0_ICAO_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_ICAOD, Under_Sep=1)
  pm_i$Under_1.0_Actual_1DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 1)
  pm_i$Under_1.0_Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 1)
  
  message("Completed calculations for ", paste0("Landing Pair ID ", lpid, " (", i, "/", no_of_iterations, ")"))
  
  return(pm_i)

})

# Bind these results to new PM table
pm2 <- rbindlist(pm_list)

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
                  Actual_0DME_All_Time_Separation #,
                 # ICAO_Perfect_0DME_Time_Separation
                  )

## Export Total Calculations for Sense Checking ##
fwrite(pm2, file.path(outdir, "IA_Calculations.csv"))
fwrite(pm_view, file.path(outdir, "IA_Calculations_Narrow.csv"))

# ----------------------------------------------------------------------- #
# Final filtering & Data format Preparation
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

# If IAS Filter active, remove pairs where leader/follower flies on average outside of min/max IAS bounds to 4DME
if (use_ias_filter){
  av_segs <- filter(test_segs, DME_Seg > (max_ias_filter_dme - 1)) %>% group_by(Landing_Pair_ID) %>% 
    summarise(Leader_Ave_Flight_IAS = mean(Leader_Average_IAS, na.rm=T),
              Follower_Ave_Flight_IAS = mean(Follower_Average_IAS, na.rm=T)) %>% ungroup()
  
  pm2 <- left_join(pm2, av_segs, by=c("Landing_Pair_ID")) %>% filter(Leader_Ave_Flight_IAS >= min_ias & Leader_Ave_Flight_IAS <= max_ias &
                                                                           Follower_Ave_Flight_IAS >= min_ias & Follower_Ave_Flight_IAS <= max_ias)
}

fwrite(pm2, file.path(outdir, "IA_Calculations_Filtered.csv"))

fwrite(pm2, "IA_Calculations_Filtered.csv")


########################################################################################################################
# END DO NOT USE
########################################################################################################################

#pm2 <- fread("IA_Calculations_Filtered.csv")

# ----------------------------------------------------------------------- #
# 2. eTBS Performance Analysis --------------------------------------------
# ----------------------------------------------------------------------- #

# Import from eTBS calculations
data1 <- pm2

# 2.1 Definitions ---------------------------------------------------------

wakes <- LETTERS[1:6]
icao_wakes <- c("J", "H", "M", "L")

wake_levels <- paste0(rep(wakes, each = length(wakes)), "-", rep(wakes, times = length(wakes)))
icao_levels <- paste0(rep(icao_wakes, each = length(icao_wakes)), "-", rep(icao_wakes, times = length(icao_wakes)))

#sep_scheme_recat_wake <- fread("Input/Separation Schemes.csv") # this needs updating
#sep_scheme_recat_wake$LF_Pair <- factor(sep_scheme_recat_wake$LF_Pair, levels = wake_levels)

# ------------------------------------------------------------------------ #
# EHAM
con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, db_name, "ruser", "Th!nkruser"
))

# EGLL
con2 <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, "Heathrow_TWA_UTMA_Validation", "ruser", "Th!nkruser"
))

wake_time_str <- "SELECT * FROM tbl_Reference_Recat_Separation_Time"
icao_dist_str <- "SELECT * FROM tbl_DBS_Wake_Turbulence"

uk_recat_map <- data.frame(UK=c("SUPER", "HEAVY", "UPPER", "MEDIUM", "SMALL", "LIGHT"), RECAT=LETTERS[1:6])

sep_scheme_heathrow_wake <- sqlQuery(con2, wake_time_str, stringsAsFactors=F)
sep_scheme_heathrow_wake <- inner_join(sep_scheme_heathrow_wake, uk_recat_map, by=c("Leader_WTC"="UK")) %>% select(-c("Leader_WTC")) %>% rename(Leader_WTC=RECAT)
sep_scheme_heathrow_wake <- inner_join(sep_scheme_heathrow_wake, uk_recat_map, by=c("Follower_WTC"="UK")) %>% select(-c("Follower_WTC")) %>% rename(Follower_WTC=RECAT)
sep_scheme_heathrow_wake <- mutate(sep_scheme_heathrow_wake, LF_Pair = paste0(Leader_WTC, "-", Follower_WTC)) %>% select(-c("Leader_WTC", "Follower_WTC")) %>% 
  rename(RECAT_Time_Reference = Reference_Wake_Separation_Time) %>% mutate(LF_Pair = factor(LF_Pair, levels = wake_levels))
sep_scheme_heathrow_wake <- sep_scheme_heathrow_wake[order(sep_scheme_heathrow_wake$LF_Pair),] %>% select(LF_Pair, everything())

sep_scheme_recat_wake <- sqlQuery(con, wake_time_str, stringsAsFactors=F) #use Database (0DME)
#sep_scheme_recat_wake <- fread(file.path("Input", "sep_scheme_recat_wake.csv")) #use local (1DME)
  
sep_scheme_recat_wake <- mutate(sep_scheme_recat_wake, LF_Pair = paste0(Leader_WTC, "-", Follower_WTC)) %>% select(-c("Leader_WTC", "Follower_WTC")) %>% 
  rename(RECAT_Time_Reference = Reference_Wake_Separation_Time) %>% mutate(LF_Pair = factor(LF_Pair, levels = wake_levels))
sep_scheme_recat_wake <- sep_scheme_recat_wake[order(sep_scheme_recat_wake$LF_Pair),] %>% select(LF_Pair, everything())

sep_scheme_recat_all <- as.data.frame(wake_levels) %>% rename(LF_Pair = wake_levels)

sep_scheme_icao_wake <- sqlQuery(con, icao_dist_str, stringsAsFactors=F) %>% mutate(LF_Pair = paste0(Leader_WVI, "-", Follower_WVI)) %>% select(-c("Leader_WVI", "Follower_WVI"))
sep_scheme_icao_wake <- mutate(sep_scheme_icao_wake, LF_Pair = factor(LF_Pair, levels = icao_levels))
sep_scheme_icao_wake <- sep_scheme_icao_wake[order(sep_scheme_icao_wake$LF_Pair),] %>% select(LF_Pair, everything())


# 2.2 Calculate new columns -----------------------------------------------

data1 <- mutate(
  data1,
  RECAT_Wake_Type_Pair = paste(Leader_Recat_Wake_Cat, Follower_Aircraft_Type, sep = "-"),
  RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels),
  ICAO_Wake_Pair = factor(paste(Leader_ICAO_Wake_Cat, Follower_ICAO_Wake_Cat, sep = "-"), levels = icao_levels))

# Do some stuff to the Heathrow data to allow updates
heathrow_data <- heathrow_orig
heathrow_data <- select(heathrow_data, -c("Leader_Recat_Wake_Cat", "Follower_Recat_Wake_Cat"))
heathrow_data <- inner_join(heathrow_data, aircraft_type_to_wake, by=c("Leader_Aircraft_Type"="Aircraft_Type")) %>% rename(Leader_Recat_Wake_Cat=Wake)
heathrow_data <- inner_join(heathrow_data, aircraft_type_to_wake, by=c("Follower_Aircraft_Type"="Aircraft_Type")) %>% rename(Follower_Recat_Wake_Cat=Wake)
heathrow_data <- mutate(heathrow_data, RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels))

# Get Accuracies & Improvement values
#data1 <- mutate(
 # data1,
#  RECAT_Wake_Type_Pair = paste(Leader_Recat_Wake_Cat, Follower_Aircraft_Type, sep = "-"),
 # RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels),
#  Actual_DBS_1DME_Time_Accuracy = Actual_1DME_Wake_Time_Separation - Recat_TBS_Wake_Separation_Time,
#  Actual_TBS_1DME_Time_Accuracy = Actual_1DME_Wake_Time_Separation - Recat_Perfect_1DME_Time_Separation,
#  Actual_TBS_0DME_Time_Accuracy = Actual_0DME_All_Time_Separation - Recat_Perfect_0DME_Time_Separation,
#  Actual_4DME_Wake_Distance_Accuracy = Observed_4DME_Separation_Distance - Actual_4DME_Wake_Separation_Distance,
#  Actual_4DME_All_Distance_Accuracy = Observed_4DME_Separation_Distance - Actual_4DME_All_Separation_Distance,
#  ICAO_Perfect_Time_Accuracy = Actual_0DME_All_Time_Separation - ICAO_Perfect_0DME_Time_Separation 
#)

# Segment Accuracy

#data1 <- mutate(
#  data1,
#  Actual_DBS_1DME_Time_Accuracy_Group = cut(Actual_DBS_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)),
#  Actual_TBS_1DME_Time_Accuracy_Group = cut(Actual_TBS_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)),
#  Actual_4DME_Wake_Distance_Accuracy_Group = cut(Actual_4DME_Wake_Distance_Accuracy, breaks = c(-Inf, -0.55, -0.05, Inf)),
#  Actual_4DME_All_Distance_Accuracy_Group = cut(Actual_4DME_All_Distance_Accuracy, breaks = c(-Inf, -0.55, -0.05, Inf)),
#  Observed_AGI_Surface_Wind_SPD_Group = cut(Observed_AGI_Surface_Wind_SPD, breaks = c(0,4,7,10,13,16,Inf), right=FALSE),
#  ICAO_Perfect_Time_Accuracy_Group = cut(ICAO_Perfect_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf))
#)


# ------------------------------------------------------------------------- #
# 2.4 Generate Word Document ----------------------------------------------
# ------------------------------------------------------------------------- #

#########################
# BARE IN MIND FOR THESE PLOTS TO GENERATE THE SEP SCHEMES' LF PAIRS BEING USED MUST MATCH ORDER EXACTLY!!!!
#########################

# Create data subset with low surface wind
#data1_low <- filter(data1, Observed_AGI_Surface_Wind_SPD >= -low_sw_lim & Observed_AGI_Surface_Wind_SPD <= low_sw_lim)
data1_low <- filter(data1, Observed_AGI_Surface_Headwind >= -low_shw_lim & Observed_AGI_Surface_Headwind <= low_shw_lim)

# Create data subset specifically for RECAT ROT pairs only, and a low wind form
data1_rot <- filter(data1, !is.na(Recat_TBS_0DME_ROT_Spacing_Distance))
data1_rot_low <- filter(data1_low, !is.na(Recat_TBS_0DME_ROT_Spacing_Distance))

# create data subset for Heathrow low winds
heathrow_data_low <- filter(heathrow_data, Observed_AGI_Surface_Headwind >= -low_shw_lim & Observed_AGI_Surface_Headwind <= low_shw_lim)


##################################################################################
# Create a word document for RECAT
##################################################################################
doc <- read_docx()
doc <- body_add_par(doc, "Distributions of RECAT Perfect/Actual Time Separations for IA", style = "heading 1")

# ------------------------------------------------------------------------- #
# 2.4.1 RECAT Wake 1DME Perfect/Actual Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- data
scheme <- "RECAT"
time_required <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Actual_1DME_Wake_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual Wake Time Separation at 1DME (All Wind) for "
header <- "RECAT-EU Perfect/Actual Wake Time Separation at 1DME (All Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.2 RECAT Wake 1DME Perfect/Actual Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- data
scheme <- "RECAT"
time_required <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Actual_1DME_Wake_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual Wake Time Separation at 1DME (Low Wind) for "
header <- "RECAT-EU Perfect/Actual Wake Time Separation at 1DME (Low Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.3 RECAT All 0DME Perfect/Actual Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- data
scheme <- "RECAT"
time_required <- F
sep_scheme <- sep_scheme_recat_all
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_0DME_Time_Separation
data2$plot_data <- data2$Actual_0DME_All_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "0DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual All Time Separation at 0DME (All Wind) for "
header <- "RECAT-EU Perfect/Actual All Time Separation at 0DME (All Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.4 RECAT All 0DME Perfect/Actual Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- data
scheme <- "RECAT"
time_required <- F
sep_scheme <- sep_scheme_recat_all
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_0DME_Time_Separation
data2$plot_data <- data2$Actual_0DME_All_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "0DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual All Time Separation at 0DME (Low Wind) for "
header <- "RECAT-EU Perfect/Actual All Time Separation at 0DME (Low Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# Export document
print(doc, target = file.path(outdir, "IA_Wake_Pair_Distributions (RECAT).docx"))

##################################################################################
# Create a word document for ICAO
##################################################################################
doc2 <- read_docx()
doc2 <- body_add_par(doc2, "Distributions of ICAO Perfect/Observed Time Separations for IA", style = "heading 1")

# ------------------------------------------------------------------------- #
# 2.4.5 ICAO 1DME Perfect/Observed Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- data
scheme <- "ICAO"
time_required <- F
sep_scheme <- sep_scheme_icao_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$ICAO_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Observed_1DME_Separation_Time
plot_title_1 <- "ICAO Perfect Time Wake Spacing: "
plot_title_2 <- "ICAO Observed Time Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of ICAO4 Perfect/Observed Wake Time Separation at 1DME (All Wind) for "
header <- "ICAO4 Perfect/Observed Wake Time Separation at 1DME (All Wind)"

doc2 <- Output_To_Word(doc2, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.6 ICAO 1DME Perfect/Observed Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- data
scheme <- "ICAO"
time_required <- F
sep_scheme <- sep_scheme_icao_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$ICAO_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Observed_1DME_Separation_Time
plot_title_1 <- "ICAO Perfect Time Wake Spacing: "
plot_title_2 <- "ICAO Observed Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of ICAO4 Perfect/Observed Wake Time Separation at 1DME (Low Wind) for "
header <- "ICAO4 Perfect/Observed Wake Time Separation at 1DME (Low Wind)"

doc2 <- Output_To_Word(doc2, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# Export document
print(doc2, target = file.path(outdir, "IA_Wake_Pair_Distributions (ICAO).docx"))

##################################################################################
# Create a word document for Schiphol/Heathrow comparisons
##################################################################################
doc3 <- read_docx()
doc3 <- body_add_par(doc3, "Distributions of Perfect & Actual Schiphol/Heathrow 1DME Time Separations for IA", style = "heading 1")

# ------------------------------------------------------------------------- #
# 2.4.7 Schiphol/Heathrow Perfect Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- heathrow_data
scheme <- "RECAT"
time_required <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$eTBS_perfect_1DME_Time_Separation
plot_title_1 <- "Schiphol Perfect Time Wake Spacing: "
plot_title_2 <- "Heathrow Perfect Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Perfect RECAT Wake Time Separation at 1DME (All Wind) for "
header <- "RECAT (Schiphol/Heathrow) Perfect Wake Time Separation at 1DME (All Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.8 Schiphol/Heathrow Perfect Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- heathrow_data_low
scheme <- "RECAT"
time_required <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$eTBS_perfect_1DME_Time_Separation
plot_title_1 <- "Schiphol Perfect Time Wake Spacing: "
plot_title_2 <- "Heathrow Perfect Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Perfect RECAT Wake Time Separation at 1DME (Low Wind) for "
header <- "RECAT (Schiphol/Heathrow) Perfect Wake Time Separation at 1DME (Low Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.9 Schiphol/Heathrow Actual Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- heathrow_data
scheme <- "RECAT"
time_required <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Actual_1DME_Wake_Time_Separation
data2$plot_data <- data2$eTBS_actual_1DME_Time_Separation
plot_title_1 <- "Schiphol Actual Time Wake Spacing: "
plot_title_2 <- "Heathrow Actual Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Actual RECAT Wake Time Separation at 1DME (All Wind) for "
header <- "RECAT (Schiphol/Heathrow) Actual Wake Time Separation at 1DME (All Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.10 Schiphol/Heathrow Actual Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- heathrow_data_low
scheme <- "RECAT"
time_required <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Actual_1DME_Wake_Time_Separation
data2$plot_data <- data2$eTBS_actual_1DME_Time_Separation
plot_title_1 <- "Schiphol Actual Time Wake Spacing: "
plot_title_2 <- "Heathrow Actual Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Actual RECAT Wake Time Separation at 1DME (Low Wind) for "
header <- "RECAT (Schiphol/Heathrow) Actual Wake Time Separation at 1DME (Low Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# Export document
print(doc3, target = file.path(outdir, "IA_Wake_Pair_Distributions (w-Heathrow).docx"))

# ------------------------------------------------------------------------- #
# 2.5 Generate Additional Tables / Plots ----------------------------------
# ------------------------------------------------------------------------- #

# Sample size tables for Simon

#Schiphol RECAT Perfect/Actual 1DME Time Counts

sample_1 <- data1 %>% group_by(RECAT_Wake_Pair) %>% summarise(EHAM_Obs_Count = n(),
                                                              EHAM_Perfect_1DME_Count = sum(!is.na(Recat_Perfect_1DME_Time_Separation)),
                                                              EHAM_Actual_1DME_Count = sum(!is.na(Actual_1DME_Wake_Time_Separation))) %>% ungroup() %>% filter(RECAT_Wake_Pair %in% sep_scheme_recat_wake$LF_Pair)


sample_2 <- data1 %>% group_by(RECAT_Wake_Pair) %>% summarise(EHAM_Obs_Count = n(),
                                                              EHAM_Perfect_0DME_Count = sum(!is.na(Recat_Perfect_0DME_Time_Separation)),
                                                              EHAM_Actual_0DME_Count = sum(!is.na(Actual_0DME_All_Time_Separation))) %>% ungroup()

sample_3 <- heathrow_data %>% group_by(RECAT_Wake_Pair) %>% summarise(EGLL_Obs_Count = n(),
                                                              EGLL_Perfect_1DME_Count = sum(!is.na(eTBS_perfect_1DME_Time_Separation)),
                                                              EGLL_Actual_1DME_Count = sum(!is.na(eTBS_actual_1DME_Time_Separation))) %>% ungroup()


fwrite(sample_1, file.path(outdir, "EHAM Perfect-Actual Wake Counts.csv"))
fwrite(sample_2, file.path(outdir, "EHAM Perfect-Actual 0DME All Counts.csv"))
fwrite(sample_3, file.path(outdir, "EGLL Perfect-Actual Wake Counts.csv"))


# Tables for the Wake - Type Pairs and Time Under-separation Rate

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


