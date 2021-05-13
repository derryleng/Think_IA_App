# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  eTBS GWCS Validation Analysis                         #
#                |                                                        #
# Version No.    |  1.8                                                   #
#                |                                                        #
# Date Modified  |  09/12/2020                                            #
#                |                                                        #
# Author(s)      |  Michael Cowham, Catherine Mason                       #
#                |                                                        #
# Project        |  eTBS Related Projects (NavCan)                        #
#                |                                                        #
# Purpose        |  GWCS Analysis                                         #
#                |                                                        #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
#
# 1 Developed from main eTBS GWCS analysis specifically for MDS
#
# 2 Edited to allow saving of data by different users
#
# 3 Updated to re run old 1 year dataset
#
# updated for re-run results 19/02/18
#
# v1.4 Updated for use on LVNL data 02/07/2020
# v1.5 Updated to remove anomaly due to go-around capturing two sets of segment data
#       (Updated anem surface wind speed to use average between 4DME and threshold) -- *Check this*, currently it is commented out and old in use
# v1.6 Updating to use TBS Reference times from database
# v1.7 Updating for NavCan
# v1.8 Removed Runway Direction and replaced with Landing Runway
#      Added Runway Group as an aggregating factor
#      Removed all the old plyr summarises and used the new processing in its place for GWCS_xxxx table
# v1.9 Some more changes.  Graphs updated to run on the new performance tables.  New Performance
#      tables now output (max forcast offset, plus now consistent across all)
#      Updated to use the NAV drop with a widened Mode S to Radar GSPD filter (20kt) - 03.02.21_2_month_dataset
# v2.0 Updated to version 2.0 to use a different performance calculation that include CIs.  Note that
#      the weighted averages now done slightly different so may give small diffs on previous results
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# Load Packages ----------------------------------------------------------
# ----------------------------------------------------------------------- #

rm(list = ls())

library(RODBC) 
library(ggplot2)
library(lattice)
#library(plyr)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(data.table)
library(DescTools)

# ----------------------------------------------------------------------- #
# Set Control Parameters --------------------------------------------------
# ----------------------------------------------------------------------- #

version <- "v2.0 03.02.21 2_month_set"
#user <- "Catherine"
#user <- "George Clark"
user <- "Michael Cowham"

# Output directory
out_data <-paste0("C:\\Users\\",user,"\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Outputs\\GWCS\\Performance Results\\",version)
        if (!dir.exists(out_data)) dir.create(out_data)  

# Input - Update
input <- paste0("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Inputs\\GWCS_Input\\03.02.21_2_month_dataset")

# RefTimes <- c(3, 4, 5, 6, 7, 8)
# # LVNL Ref Time
# RefTimes <- cbind(RefTimes, c(76, 101, 124, 147, 177, 194))

 # Database name
#database <- "NavCan_UTMA_Validation_DB2"
#database <- "NavCan_TBS_Analysis_UTMA_Validation"
database <- "NavCan_TBS"
# ----------------------------------------------------------------------- #
# Load Data --------------------------------------------------------------
# ----------------------------------------------------------------------- #
# Wake pair proportions - update folder name
prop_str <-  paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Outputs\\Sample_Weighting_Output\\v2.2 25.01.21\\Wake Pair Proportions.csv", sep = "")
prop <- fread(prop_str)

#Arrival totals - update folder name
arr_str <- paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Outputs\\Sample_Weighting_Output\\v2.2 25.01.21\\Arrival Totals.csv", sep = "")
arr <- fread(arr_str)

# Load the Segment Data
#rawsegs <- read.csv(file = "C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\P33.C.11.2016 (eTBS)\\5. Validation\\Validation_DB_Output\\Wind_Seg_And_Anemometer_Data\\Raw_GWCS_Wind_Segs_May15_Apr16.csv", head = TRUE, sep = ",", na.strings = c('NULL'))

# Load in the 6 GWCS data sets
for (sepdist in 3:8){
        gwcs_file <-  paste(input,"\\vw_Mode_S_Wind_Forecast_", sepdist, "nm.csv", sep = "")
        gwcs_dist <- fread(file = gwcs_file, na.strings = c("NA", "NULL") )
        
        if (sepdist == 3){
                gwcs_data <- gwcs_dist
        } else {
                gwcs_data <- rbind(gwcs_data, gwcs_dist)
        }
        rm(gwcs_dist)
}

#Connect to database (Update IP 192.168.1.39 or 192.168.1.23)
con <- odbcDriverConnect(connection=sprintf(
        "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
        "SQL Server", "192.168.1.23", database, "ruser", "Th!nkruser"
))
# Read in the anemometer data 
anem <- as.data.table(sqlQuery(con, "SELECT 
        FP.Flight_Plan_ID,
	FP.FP_Date,
	FP.FP_Time,
	FP.Landing_Runway,
	FPD.Time_At_4DME,
	FPD.Time_At_1DME,
	Surface_Wind_SPD = (SELECT TOP 1 Anemo_SPD FROM tbl_Anemometer WHERE Landing_Runway = FP.Landing_Runway AND Anemo_Date = FP.FP_Date AND Anemo_Time > FPD.Time_At_4DME - 300 AND Anemo_Time <= FPD.Time_At_4DME + 300  ORDER BY ABS(Anemo_Time-FPD.Time_At_4DME)) / dbo.fnc_GI_Kts_To_M_Per_Sec(),
	--Surface_Wind_SPD = (SELECT AVG(Anemo_SPD) FROM tbl_Anemometer WHERE Landing_Runway = FP.Landing_Runway AND Anemo_Date = FP.FP_Date AND Anemo_Time > FPD.Time_At_4DME AND Anemo_Time <= FPD.Time_At_1DME + 22.5) / dbo.fnc_GI_Kts_To_M_Per_Sec(),
	Surface_Wind_HDG = (SELECT TOP 1 Anemo_HDG FROM tbl_Anemometer WHERE Landing_Runway = FP.Landing_Runway AND Anemo_Date = FP.FP_Date AND Anemo_Time > FPD.Time_At_4DME - 300 AND Anemo_Time <= FPD.Time_At_4DME + 300  ORDER BY ABS(Anemo_Time-FPD.Time_At_4DME)) / dbo.fnc_GI_Degs_To_Rads()
FROM tbl_Flight_Plan AS FP
JOIN tbl_Flight_Plan_Derived AS FPD 
ON FP.Flight_Plan_ID = FPD.Flight_Plan_ID"))
# anem <- sqlQuery(dbhandle, "SELECT * FROM inptAnemometer")
# anem <- read.csv(file = paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\eTBS Complex Algorithm\\GWCS\\Anemometer_Data_May15_Apr16.csv", sep = "")
#                  , head = TRUE, sep = ",", na.strings = c("", "NULL") )

#read in METAR data
#met <- read.csv(file = paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\3. GWCS\\Test Data\\EHAM_Nov18_Nov19_processed.csv", sep = "")
#                , head = TRUE, sep = ",", na.strings = c("", "NULL") )

# Load TBS Reference times
RefTimes <- as.data.table(sqlQuery(con, "SELECT Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852
                                                ,Reference_Wake_Separation_Time
                                         FROM tbl_Reference_TBS_Table_Time"))

# Get landing runway info
runway_list <- sqlQuery(con, sprintf("SELECT Runway_Name FROM tbl_Runway"))

# Data of flights to remove due to anomaly - segment data picked up twice for both runways (due to go-around)
flights_greater_200 <- fread(paste0("C:\\Users\\",user,"\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Outputs\\GWCS\\Outlier Investigation\\flights_greater_200.csv"))

# Read in the runway groups

RunGroups <- as.data.table(sqlQuery(con, "SELECT Runway_Name, Runway_Group FROM tbl_Runway"))

# ----------------------------------------------------------------------- #
# Data Processing---------------------------------------------------------
# ----------------------------------------------------------------------- #
## Temp - for comparison to test only 1 month data (July 2019)
# anem <- anem[grep("^[0-9]{2}/07/2019$",FP_Date)]
# gwcs_data <- gwcs_data[grep("^[0-9]{2}/07/2019$",FP_Date)]


names(gwcs_data) <- c('FP_Date','Time_At_4DME','Callsign','Aircraft_Type','Landing_Runway','Observed_Min_RTT','Observed_Max_RTT','Observed_Wind_Effect_IAS','Observed_Flying_Time','Observed_Ave_Mode_S_IAS','Observed_Ave_Mode_S_GSPD','Observed_Track_GSPD','Forecast_Type','Assumed_IAS','Forecast_Wind_Effect_IAS','Forecast_Wind_Effect_IAS_Error','Forecast_Flying_Time_Observed_IAS','Forecast_Flying_Time_Observed_IAS_Error','Forecast_Flying_Time_Assumed_IAS','Forecast_Flying_Time_Assumed_IAS_Error','Forecast_GSPD_Observed_IAS','Forecast_GSPD_Observed_IAS_Error','Forecast_GSPD_Assumed_IAS','Forecast_GSPD_Assumed_IAS_Error','Min_Forecast_Offset_Time','Max_Forecast_Offset_Time','Diff_Mode_S_To_Radar_GSPD_Flag','Mode_S_GSPD_Min','Mode_S_GSPD_Max','Mode_S_IAS_Min','Mode_S_IAS_Max','Mode_S_TAS_Min','Mode_S_TAS_Max','Mode_S_Roll_Angle_Max','Time_Of_Day_Min','Time_Of_Day_Max','DME_Seg_Min','DME_Seg_Max','DME_Seg_Size','Altitude_Tolerance','Seg_Duration_Min','Seg_Duration_Max','Diff_Track_To_Runway_HDG_Max','Diff_HDG_To_Runway_HDG_Max','Diff_Mode_S_To_Radar_Track_Max','Diff_Mode_S_To_Radar_GSPD_Max','Max_Wind_Effect','Max_Wind_SPD','Forecast_Seg_Min','Forecast_Seg_Max','Forecast_Lookahead_Time','Forecast_Stale_Time','Alpha','Forecast_Wind_Effect_IAS_a','Forecast_Wind_Effect_IAS_b','Forecast_GSPD_Observed_IAS_Error_Dist')
# Remove go arounds (temporary fix) (two sets of seg data pulling though in outlier script)
anem_proc <- anem[!(anem$Flight_Plan_ID %in% flights_greater_200$Flight_Plan_ID)]
gwcs_data_proc <- gwcs_data[!(paste(gwcs_data$FP_Date, gwcs_data$Callsign) %in% paste(flights_greater_200$FP_Date, flights_greater_200$Callsign)),]

#Format Time column
gwcs_data_proc$FP_Date <- as.Date(gwcs_data_proc$FP_Date, format ="%d/%m/%Y")
anem_proc$FP_Date <- as.Date(anem_proc$FP_Date, format ="%d/%m/%Y")

# MC Add 25/01
gwcs_data_proc <- inner_join(gwcs_data_proc, RunGroups, by = c("Landing_Runway" = "Runway_Name"))

# Temporary - but just keep the dates where the ranges overalap
#gwcs_data <- filter(gwcs_data, FP_Date %in% c('09/11/2017','10/11/2017','11/11/2017','12/11/2017','13/11/2017','14/11/2017','15/11/2017','16/11/2017','17/11/2017','18/11/2017','19/11/2017','20/11/2017','21/11/2017','22/11/2017','23/11/2017','24/11/2017','25/11/2017','26/11/2017','27/11/2017','28/11/2017','29/11/2017','30/11/2017','01/12/2017','02/12/2017','03/12/2017','04/12/2017','05/12/2017','06/12/2017','07/12/2017','08/12/2017'))

#remove the half days where no MDS
#gwcs_data <- subset(gwcs_data, !(FP_Date == "09/11/2017" & Time_At_4DME < 48600)) # remove half day before 13:30
#gwcs_data <- subset(gwcs_data, !(FP_Date == "08/12/2017" & Time_At_4DME > 50400)) # remove half day after 14:00
#Remove dates where 100% NULL forecasts
#gwcs_data <- filter(gwcs_data, !(FP_Date %in% c('07/06/2015','09/06/2015','07/09/2015','26/09/2015','28/09/2015','30/09/2015','09/12/2015','16/01/2016','24/01/2016','12/03/2016','14/03/2016','16/03/2016','18/03/2016','19/04/2016')))


# Exclude the values failing the QC criteria
gwcs_data_proc$Sep_Dist <- gwcs_data_proc$Forecast_Seg_Max - gwcs_data_proc$Forecast_Seg_Min + 1

gwcs_data_proc$qc_flag <- ifelse(gwcs_data_proc$Landing_Runway %in% runway_list$Runway_Name & 
                                gwcs_data_proc$Observed_Max_RTT >= (gwcs_data_proc$Forecast_Seg_Min + gwcs_data_proc$Sep_Dist - 2) & 
                                    gwcs_data_proc$Observed_Flying_Time >= (gwcs_data_proc$Sep_Dist * 20 - 40) & 
                                    abs(gwcs_data_proc$Observed_Ave_Mode_S_GSPD - gwcs_data_proc$Observed_Track_GSPD) <= 20, 1, 0)

#gwcs_data$early_intercept_flag <- ifelse(gwcs_data$Observed_Max_RTT >= gwcs_data$Forecast_Seg_Max, 1, 0)
#gwcs_data$flying_time_flag     <- ifelse(gwcs_data$Observed_Flying_Time >= (gwcs_data$Sep_Dist * 20 - 40), 1, 0) 
#gwcs_data$runway_flag          <- ifelse(gwcs_data$Landing_Runway %in% c('R18R','R06','R27','R18L','R36R','R18C','R36C','R22'), 1, 0)
#gwcs_data$gspd_flag            <- ifelse(abs(gwcs_data$Observed_Ave_Mode_S_GSPD - gwcs_data$Observed_Track_GSPD) <= 20, 1, 0)
#gwcs_data$qc_flag              <- ifelse((gwcs_data$early_intercept_flag + gwcs_data$flying_time_flag + gwcs_data$runway_flag + 
#                                                  gwcs_data$gspd_flag) == 4, 1, 0) 

# Replace any NAs with 0
gwcs_data_proc$qc_flag <- ifelse(is.na(gwcs_data_proc$qc_flag), 0, gwcs_data_proc$qc_flag)

gwcs_data_proc$stale_flag <- ifelse(is.na(gwcs_data_proc$Forecast_Wind_Effect_IAS), 0, 1)

exclusion_table <- table(gwcs_data_proc$Sep_Dist, gwcs_data_proc$qc_flag)
f <- data.frame(exclusion_table)
date_table <- table(gwcs_data_proc[gwcs_data_proc$stale_flag == 0, ]$FP_Date)
stale_table <- table(gwcs_data_proc$FP_Date, gwcs_data_proc$stale_flag)

# Save the exclusion table
#write.csv(exclusion_table, file = paste(out_data, "\\GWCS_Performance_Exclusions.csv", sep = ""))

#write.csv(date_table, file = paste(out_data, "\\GWCS_Performance_Date_Totals.csv", sep = ""))

# Save the results prior to filtering

fwrite(arrange(gwcs_data_proc, Sep_Dist, Runway_Group, FP_Date, Time_At_4DME), file = file.path(out_data, "GWCS_Pre_Filter_Consolidated_Results.csv"))

# Remove the observations failing the qc criteria
gwcs_data_proc <- filter(gwcs_data_proc, qc_flag == 1)

# Lookup the reference times
#gwcs_data_proc$Ref_Time <- RefTimes[match(gwcs_data_proc$Sep_Dist, RefTimes[,1]), 2] #Old - used in manual TBS table
gwcs_data_proc$Ref_Time <- RefTimes[match(gwcs_data_proc$Sep_Dist, RefTimes$Reference_Wake_Separation_Distance), Reference_Wake_Separation_Time]

# Calculate the error distance
gwcs_data_proc$Forecast_GWCS_Error_Distance <- gwcs_data_proc$Forecast_Wind_Effect_IAS_Error * gwcs_data_proc$Ref_Time / 3600

# Calculate flags for the errors
gwcs_data_proc$gt_10kt <- ifelse(gwcs_data_proc$Forecast_Wind_Effect_IAS_Error > 10, 1,0)
gwcs_data_proc$gt_20kt <- ifelse(gwcs_data_proc$Forecast_Wind_Effect_IAS_Error > 20, 1,0)
gwcs_data_proc$gt_25nm <- ifelse(gwcs_data_proc$Forecast_GWCS_Error_Distance > 0.25, 1,0)
gwcs_data_proc$gt_50nm <- ifelse(gwcs_data_proc$Forecast_GWCS_Error_Distance > 0.50, 1,0)
gwcs_data_proc$gt_100nm <- ifelse(gwcs_data_proc$Forecast_GWCS_Error_Distance > 1.00, 1,0)

# Split out the GWCS Wind COnditions
gwcs_data_proc$gwcs_band <- cut(gwcs_data_proc$Observed_Wind_Effect_IAS, c(-Inf, -30, -20, -10, 0, 10, Inf), right = FALSE)

# MC New 25/01
# Band the data by the max forecast offset time
gwcs_data_proc$max_forecast_offset <- cut(gwcs_data_proc$Max_Forecast_Offset_Time, c(seq(0, 8 * 120, 120)))
# Calculate the inter-arrival (4DME) time
gwcs_data_proc <- arrange(gwcs_data_proc, FP_Date, Runway_Group) %>% group_by(FP_Date, Runway_Group) %>% mutate(Inter_Arrival_Time = Time_At_4DME - lag(Time_At_4DME, n = 1), IAT_Group = cut(Inter_Arrival_Time, c(seq(0, 8 * 120, 120), Inf))) %>% ungroup()


# TEMP (VERY IMPORTANT TO REMOVE)
prop <- filter(prop, RECAT_Wake_Separation_Distance != 3.5) 

############################ For Anemometer Data - New###########################
#Merge Anemometer data onto gwcsdata
gwcs_data_anem <- merge(gwcs_data_proc,anem_proc, by=c("FP_Date","Time_At_4DME","Landing_Runway"))


# ## TEST to check the 9min forecast look ahead time
# ORD_prediction <- as.data.table(sqlQuery(con, "SELECT ORDP.[Landing_Pair_ID]
#       ,[Prediction_Time]
#       ,[Leader_Distance_To_Threshold]
#       ,[ORD_Separation_Distance]
#       ,[DBS_All_Sep_Distance]
#       ,[Landing_Pair_Date]
#       ,Flight_Plan_ID = [Follower_Flight_Plan_ID]
#   FROM tbl_ORD_Prediction as ORDP
#   LEFT JOIN (SELECT [Landing_Pair_ID]
#       ,[Landing_Pair_Date]
#       ,[Leader_Flight_Plan_ID]
#       ,[Follower_Flight_Plan_ID]
#       ,[Landing_Pair_Type]
#   FROM tbl_Landing_Pair) AS LP
#   ON ORDP.Landing_Pair_ID = LP.Landing_Pair_ID
#   WHERE Landing_Pair_Type != 'Not_In_Trail'"))
# 
# ord_gwcs_anem <- merge(unique(gwcs_data_anem[c("Flight_Plan_ID","Landing_Runway", "Time_At_1DME")]), ORD_prediction, by = "Flight_Plan_ID" )
# # 22.5s added to account for prediction time at 1DME rather than threshold
# ord_gwcs_anem$Time_At_1DME <- ord_gwcs_anem$Time_At_1DME + 22.5
# ord_gwcs_anem$tdiff <- ord_gwcs_anem$Time_At_1DME - ord_gwcs_anem$Prediction_Time
# look_ahead_time <- ord_gwcs_anem %>% group_by(Landing_Runway) %>% summarise(mean = (mean(tdiff, na.rm = TRUE))/60)
# fwrite(look_ahead_time, "look_ahead_time.csv")


#group by surface wind band
gwcs_data_anem$surface_wind_band <- cut(gwcs_data_anem$Surface_Wind_SPD, c(0, 4, 9, 14, Inf), right = FALSE)


#--------- George's Bit -------------------------#
sw_cuts <- c(0, 4, 9, 14, Inf)

qwph <- as.numeric(arr[4,3])

# Function to make unweighted performance tables

Performance_Table <- function(data){
        data <- summarise(data,
                          `Total Count`=n(),
                          `Non-Stale Count`=sum(stale_flag, na.rm=T),
                          `Mean (F)`=mean(Forecast_Wind_Effect_IAS, na.rm = TRUE),
                          `Mean (O-F)`=mean(Forecast_Wind_Effect_IAS_Error, na.rm = TRUE),
                          `Standard Deviation`=sd(Forecast_Wind_Effect_IAS_Error, na.rm = TRUE),
                          `O-F >10kts`=sum(gt_10kt, na.rm = TRUE),
                          `O-F >20kts`=sum(gt_20kt, na.rm = TRUE),
                          `O-F >0.25NM`=sum(gt_25nm, na.rm = TRUE),
                          `O-F >0.5NM`=sum(gt_50nm, na.rm = TRUE),
                          `O-F >1.0NM`=sum(gt_100nm, na.rm = TRUE),
                          ) 
        data <- mutate(data, 
                       `O-F >10kts Rate` = `O-F >10kts` / `Non-Stale Count`,
                       `O-F >20kts Rate` = `O-F >20kts` / `Non-Stale Count`,
                       `O-F >0.25NM Rate` = `O-F >0.25NM` / `Non-Stale Count`,
                       `O-F >0.5NM Rate` = `O-F >0.5NM` / `Non-Stale Count`,
                       `O-F >1.0NM Rate` = `O-F >1.0NM` / `Non-Stale Count`)
        
        return(data)
}

# Function to join on relevant weightings for Indicator metric

Indicator_Join <- function(data){
        data <- left_join(data, select(prop,-c("Percent_2dp")), by=c("Sep_Dist"="RECAT_Wake_Separation_Distance"))
        return(data)
}

# Function to create Indicacator metric tables
Performance_Table_Indicator <- function(data){
        data <- summarise(data,
                          `Weighted Count`=weighted.mean(as.numeric(`Total Count`), `Count`, na.rm = TRUE),
                          `Weighted Non-Stale Count`=weighted.mean(as.numeric(`Non-Stale Count`), `Count`, na.rm = TRUE),
                          `Mean (O-F)`=weighted.mean(`Mean (O-F)`, `Count`, na.rm = TRUE),
                          `O-F >10kts`=weighted.mean(`O-F >10kts Rate`, `Count`, na.rm = TRUE),
                          `O-F >20kts`=weighted.mean(`O-F >20kts Rate`, `Count`, na.rm = TRUE),
                          `O-F >0.25NM`=weighted.mean(`O-F >0.25NM Rate`, `Count`, na.rm = TRUE),
                          `O-F >0.5NM`=weighted.mean(`O-F >0.5NM Rate`, `Count`, na.rm = TRUE),
                          `O-F >1.0NM`=weighted.mean(`O-F >1.0NM Rate`, `Count`, na.rm = TRUE))
        return(data)
}

Performance_Table_Indicator_CI <- function(data){
        
        data <- summarise(data,
                          `Weighted Count`=weighted.mean(as.numeric(`Total Count`), `Count`, na.rm = TRUE),
                          `Weighted Non-Stale Count`=weighted.mean(as.numeric(`Non-Stale Count`), `Count`, na.rm = TRUE),
                          `Mean (O-F)`=weighted.mean(`Mean (O-F)`, `Count`, na.rm = TRUE),
                          `O-F >10kts`=weighted.mean(`O-F >10kts`, `Count`, na.rm = TRUE),
                          `O-F >20kts`=weighted.mean(`O-F >20kts`, `Count`, na.rm = TRUE),
                          `O-F >0.25NM`=weighted.mean(`O-F >0.25NM`, `Count`, na.rm = TRUE),
                          `O-F >0.5NM`=weighted.mean(`O-F >0.5NM`, `Count`, na.rm = TRUE),
                          `O-F >1.0NM`=weighted.mean(`O-F >1.0NM`, `Count`, na.rm = TRUE))
                      
        names <- c("O-F >10kts Rate", "O-F >10kts LRW", "O-F >10kts UPR", "O-F >20kts Rate", "O-F >20kts LWR", "O-F >20kts UPR", "O-F >0.25NM Rate","O-F >0.25NM LWR", "O-F >0.25NM UPR", "O-F >0.5NM Rate","O-F >0.5NM LRW", "O-F >0.5NM UPR", "O-F >1.0NM Rate", "O-F >1.0NM LWR", "O-F >1.0NM UPR")
        idx <- ncol(data) - 8
        ci <- as.data.frame(do.call(cbind, lapply((4+idx):(8+idx), function(x) BinomCI(data[[x]],data[[3]], conf.level = 0.95, method = "wilson"))))
        names(ci) <- names
        rownames(ci) <- c()
        data <- cbind(data, ci)
       
        return(data)
}

Performance_Table_Operational_CI <- function(data){
        data <- mutate(data,
                       `O-F >10kts Rate`=`O-F >10kts Rate` *qwph,
                       `O-F >20kts Rate`=`O-F >20kts Rate` *qwph,
                       `O-F >0.25NM Rate`=`O-F >0.25NM Rate` *qwph,
                       `O-F >0.5NM Rate`=`O-F >0.5NM Rate` *qwph,
                       `O-F >1.0NM Rate`=`O-F >1.0NM Rate` *qwph,
                       `O-F >10kts LWR`=`O-F >10kts LWR` *qwph,
                       `O-F >20kts LWR`=`O-F >20kts LWR` *qwph,
                       `O-F >0.25NM LWR`=`O-F >0.25NM LWR` *qwph,
                       `O-F >0.5NM LWR`=`O-F >0.5NM LWR` *qwph,
                       `O-F >1.0NM LWR`=`O-F >1.0NM LWR` *qwph,
                       `O-F >10kts UPR`=`O-F >10kts UPR` *qwph,
                       `O-F >20kts UPR`=`O-F >20kts UPR` *qwph,
                       `O-F >0.25NM UPR`=`O-F >0.25NM UPR` *qwph,
                       `O-F >0.5NM UPR`=`O-F >0.5NM UPR` *qwph,
                       `O-F >1.0NM UPR`=`O-F >1.0NM UPR` *qwph)
        return(data)
}


Performance_Table_Operational <- function(data){
        data <- mutate(data,
                          `O-F >10kts`=`O-F >10kts` *qwph,
                          `O-F >20kts`=`O-F >20kts` *qwph,
                          `O-F >0.25NM`=`O-F >0.25NM` *qwph,
                          `O-F >0.5NM`=`O-F >0.5NM` *qwph,
                          `O-F >1.0NM`=`O-F >1.0NM` *qwph)
        return(data)
}


# Convert to DF for ease of manipulation
perf_df <- data.frame(gwcs_data_anem) %>% rename(`Surface Wind`=surface_wind_band, `Runway Direction`=Landing_Runway)

# Define the allowed surface wind and runway groups for the tables
SW_groups <- as.character(unique(filter(perf_df, !is.na(`Surface Wind`))$`Surface Wind`))
#rw_groups <- substr(runway_list$Runway_Name, 0, 3)
rw_groups <- runway_list$Runway_Name

# TABLES

# Raw Tables 1: By Separation Distance
PR_Sep <- perf_df %>% group_by(Sep_Dist) %>% Performance_Table()
PR_Sep_SW <- perf_df %>% group_by(Sep_Dist, `Surface Wind`) %>% Performance_Table() %>% filter(`Surface Wind` %in% SW_groups)
PR_Sep_LR <- perf_df %>% group_by(Sep_Dist, `Runway Direction`) %>% Performance_Table() 
PR_Sep_SW_LR <- perf_df %>% group_by(Sep_Dist, `Surface Wind`, `Runway Direction`) %>% Performance_Table() %>% filter(`Surface Wind` %in% SW_groups & `Runway Direction` %in% rw_groups)
PR_Sep_MFO <- perf_df %>% group_by(Sep_Dist, `max_forecast_offset`) %>% Performance_Table()
PR_Sep_RG <- perf_df %>% group_by(Sep_Dist, `Runway_Group`) %>% Performance_Table() 
       
# Raw Tables 2: Per IA Indicator
PR_Ind <- Indicator_Join(PR_Sep) %>% Performance_Table_Indicator_CI()
PR_Ind_SW <- Indicator_Join(PR_Sep_SW) %>% group_by(`Surface Wind`) %>% Performance_Table_Indicator_CI()
PR_Ind_LR <- Indicator_Join(PR_Sep_LR) %>% group_by(`Runway Direction`) %>% Performance_Table_Indicator_CI()
PR_Ind_SW_LR <- Indicator_Join(PR_Sep_SW_LR) %>% group_by(`Surface Wind`, `Runway Direction`) %>% Performance_Table_Indicator_CI() %>% arrange(`Runway Direction`)
PR_Ind_RG <- Indicator_Join(PR_Sep_RG) %>% group_by(Runway_Group) %>% Performance_Table_Indicator_CI() %>% arrange(Runway_Group)
PR_Ind_MFO <- Indicator_Join(PR_Sep_MFO) %>% group_by(`max_forecast_offset`) %>% Performance_Table_Indicator_CI() %>% arrange(`max_forecast_offset`)

# Raw Tables 3: Queued Arrivals per Hour
PR_Arr <- Performance_Table_Operational_CI(PR_Ind)
PR_Arr_SW <- Performance_Table_Operational_CI(PR_Ind_SW)
PR_Arr_LR <- Performance_Table_Operational_CI(PR_Ind_LR)
PR_Arr_RG <- Performance_Table_Operational_CI(PR_Ind_RG)
PR_Arr_MFO <- Performance_Table_Operational_CI(PR_Ind_MFO)
PR_Arr_SW_LR <- Performance_Table_Operational_CI(PR_Ind_SW_LR)

#PR_Arr_SW_LR <- PR_Ind_SW_LR
#for (i in names(PR_Arr_SW_LR)) {
#        if (grepl("^O-F .*$", i)) {
#                PR_Arr_SW_LR[[i]] <- PR_Arr_SW_LR[[i]] * qwph
#        }
#}


# ----------------------------------------------------------------------- #
# Saving Tables ---------------------------------------------------------
# ----------------------------------------------------------------------- #

# Main Data
# fwrite(arrange(perf_df, Sep_Dist, Runway_Group, FP_Date, Time_At_4DME), file = file.path(out_data, "GWCS_Consolidated_Results.csv"))

# Overall Results by Separation Distance

fwrite(PR_Sep, file = file.path(out_data, "GWCS_Performance_Stats_All.csv"))
fwrite(PR_Sep_SW, file = file.path(out_data, "GWCS_Performance_Stats_Surface_Wind.csv"))
fwrite(PR_Sep_LR, file = file.path(out_data, "GWCS_Performance_Stats_Runway.csv"))
fwrite(PR_Sep_SW_LR, file = file.path(out_data, "GWCS_Performance_Stats_Surface_Wind_Runway.csv"))
fwrite(PR_Sep_MFO, file = file.path(out_data, "GWCS_Performance_Stats_Max_Forecast_Offset.csv"))
fwrite(PR_Sep_RG, file = file.path(out_data, "GWCS_Performance_Stats_Runway_Group.csv"))

# Aggregated Results Per Indicator

fwrite(PR_Ind, file = paste(out_data, "\\Performance Results, Per Indicator.csv", sep = ""))
fwrite(PR_Ind_SW, file = paste(out_data, "\\Performance Results, Per Indicator, Surface Wind.csv", sep = ""))
fwrite(PR_Ind_LR, file = paste(out_data, "\\Performance Results, Per Indicator, Runway Direction.csv", sep = ""))
fwrite(PR_Ind_SW_LR, file = paste(out_data, "\\Performance Results, Per Indicator, Surface Wind, Runway Direction.csv", sep = ""))
fwrite(PR_Ind_RG, file = paste(out_data, "\\Performance Results, Per Indicator, Runway Group.csv", sep = ""))
fwrite(PR_Ind_MFO, file = paste(out_data, "\\Performance Results, Per Indicator, Max Forecast Offset.csv", sep = ""))

# Aggregated Results Per Operational Hour

fwrite(PR_Arr, file = paste(out_data, "\\Performance Results, Hourly Arrivals.csv", sep = ""))
fwrite(PR_Arr_SW, file = paste(out_data, "\\Performance Results, Hourly Arrivals, Surface Wind.csv", sep = ""))
fwrite(PR_Arr_LR, file = paste(out_data, "\\Performance Results, Hourly Arrivals, Runway Direction.csv", sep = ""))
fwrite(PR_Arr_SW_LR, file = paste(out_data, "\\Performance Results, Hourly Arrivals, Surface Wind, Runway Direction.csv", sep = ""))
fwrite(PR_Arr_RG, file = paste(out_data, "\\Performance Results, Hourly Arrivals, Runway Group.csv", sep = ""))
fwrite(PR_Arr_MFO, file = paste(out_data, "\\Performance Results, Hourly Arrivals, Max Forecast Offset.csv", sep = ""))

# Capture the Weighting Data For referencce

fwrite(prop, file = file.path(out_data, "Wake_Pair_Weights.csv"))
fwrite(arr, file = file.path(out_data, "Operational_Hour_Multipler.csv"))
       

# ----------------------------------------------------------------------- #
# Plot Output ------------------------------------------------------------
# ----------------------------------------------------------------------- #


# Histogram of Wind Conditions based on the 4NM Separations
gwcs_data_4 <- gwcs_data_anem[gwcs_data_anem$Sep_Dist == 4, ]

# By GWCS

hist(gwcs_data_4$Observed_Wind_Effect_IAS, main  = "NODE Only Wind Effect Distribution", xlab = "Wind Effect (kt)", prob=TRUE, col = "Light Blue", xlim = c(-40,10))

# By Surface Wind Speed

#hist(gwcs_data_4$Anemo_SPD, main  = "NODE Only Surface Wind Distribution", xlab = "Surface Wind Speed (kt)", prob=TRUE, col = "Light Green", breaks = seq(0,40,2), xlim = c(0,30))
hist(gwcs_data_4$Surface_Wind_SPD, main  = "NODE Only Surface Wind Distribution", xlab = "Surface Wind Speed (kt)", prob=TRUE, col = "Light Green", breaks = seq(0,40,2), xlim = c(0,30))


# Overall Performance
plt_mean <- ggplot() +
        geom_point(data=PR_Sep, aes(x = Sep_Dist, y = `Mean (O-F)`), size =2)+
        geom_line(data=PR_Sep, aes(x = Sep_Dist, y = `Mean (O-F)`), size = 1)+
        ggtitle("Mean (O-F) Overall")+
        labs(x = "RECAT EU Distance", y = "Mean Error Rate")+
        #coord_cartesian(ylim=c(0,0.25))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(), axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


plt_std <- ggplot() +
        geom_point(data=PR_Sep, aes(x = Sep_Dist, y = `Standard Deviation`), size =2)+
        geom_line(data=PR_Sep, aes(x = Sep_Dist, y = `Standard Deviation`), size = 1)+
        ggtitle("Stdev (O-F) Overall")+
        labs(x = "RECAT EU Distance", y = "Stdev Error Rate")+
        #coord_cartesian(ylim=c(0,2.5))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


plt_10kt <- ggplot() +
        geom_point(data=PR_Sep, aes(x = Sep_Dist, y = `O-F >10kts`), size =2)+
        geom_line(data=PR_Sep, aes(x = Sep_Dist, y = `O-F >10kts`), size = 1)+
        ggtitle("10kt Error Rate Overall")+
        labs(x = "RECAT EU Distance", y = ">10kt Error Rate")+
        #coord_cartesian(ylim=c(0,0.0015))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


plt_50nm <- ggplot() +
        geom_point(data=PR_Sep, aes(x = Sep_Dist, y = `O-F >0.5NM`), size =2)+
        geom_line(data=PR_Sep, aes(x = Sep_Dist, y = `O-F >0.5NM`), size = 1)+
        ggtitle("0.5Nm Error Rate Overall")+
        labs(x = "RECAT EU Distance", y = ">0.5nm Error Rate")+
        #coord_cartesian(ylim=c(0,0.0015))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


#grid.arrange(plt_mean, plt_std, plt_10kt, plt_50nm ,ncol=2,nrow=2)

#Save results plots to chart
png(width=1000, height = 600, file= paste(out_data, "\\Plots Overall.png", sep = ""))
grid.arrange(plt_mean, plt_std, plt_10kt, plt_50nm ,ncol=2,nrow=2)
dev.off()


# Performance by Runway Group
plt_mean <- ggplot() +
        geom_point(data=PR_Sep_RG, aes(x = Sep_Dist, y = `Mean (O-F)`,colour=Runway_Group, group = Runway_Group), size =2)+
        geom_line(data=PR_Sep_RG, aes(x = Sep_Dist, y = `Mean (O-F)`,colour=Runway_Group, group = Runway_Group), size = 1)+
        ggtitle("Mean (O-F) by Landing Runway Direction")+
        labs(x = "RECAT EU Distance", y = "Mean Error Rate")+
        #coord_cartesian(ylim=c(0,0.25))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(), axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))

plt_std <- ggplot() +
        geom_point(data=PR_Sep_RG, aes(x = Sep_Dist, y = `Standard Deviation`,colour=Runway_Group, group = Runway_Group), size =2)+
        geom_line(data=PR_Sep_RG, aes(x = Sep_Dist, y = `Standard Deviation`,colour=Runway_Group, group = Runway_Group), size = 1)+
        ggtitle("Stdev (O-F) by Landing Runway Direction")+
        labs(x = "RECAT EU Distance", y = "Stdev Error Rate")+
        #coord_cartesian(ylim=c(0,2.5))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))

plt_10kt <- ggplot() +
        geom_point(data=PR_Sep_RG, aes(x = Sep_Dist, y = `O-F >10kts`,colour=Runway_Group, group = Runway_Group), size =2)+
        geom_line(data=PR_Sep_RG, aes(x = Sep_Dist, y = `O-F >10kts`,colour=Runway_Group, group = Runway_Group), size = 1)+
        ggtitle("10kt Error Rate by Landing Runway Direction")+
        labs(x = "RECAT EU Distance", y = ">10kt Error Rate")+
        #coord_cartesian(ylim=c(0,0.0015))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))

plt_50nm <- ggplot() +
        geom_point(data=PR_Sep_RG, aes(x = Sep_Dist, y = `O-F >0.5NM`,colour=Runway_Group, group = Runway_Group), size =2)+
        geom_line(data=PR_Sep_RG, aes(x = Sep_Dist, y = `O-F >0.5NM`,colour=Runway_Group, group = Runway_Group), size = 1)+
        ggtitle("0.5Nm Error Rate by Landing Runway Direction")+
        labs(x = "RECAT EU Distance", y = ">0.5nm Error Rate")+
        #coord_cartesian(ylim=c(0,0.0015))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


#grid.arrange(plt_mean, plt_std, plt_10kt, plt_50nm ,ncol=2,nrow=2)

#Save results plots to chart
png(width=1000, height = 600, file= paste(out_data, "\\Plots by Landing Runway.png", sep = ""))
grid.arrange(plt_mean, plt_std, plt_10kt, plt_50nm ,ncol=2,nrow=2)
dev.off()


# Performance by Surface Wind
plt_mean <- ggplot() +
        geom_point(data=PR_Sep_SW, aes(x = Sep_Dist, y = `Mean (O-F)`,colour=`Surface Wind`, group = `Surface Wind`), size =2)+
        geom_line(data=PR_Sep_SW, aes(x = Sep_Dist, y = `Mean (O-F)`,colour=`Surface Wind`, group = `Surface Wind`), size = 1)+
        ggtitle("Mean (O-F) by Surface Wind Speed")+
        labs(x = "RECAT EU Distance", y = "Mean Error Rate")+
        #coord_cartesian(ylim=c(-0.3,0.3))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))

plt_std <- ggplot() +
        geom_point(data=PR_Sep_SW, aes(x = Sep_Dist, y = `Standard Deviation`,colour=`Surface Wind`, group = `Surface Wind`), size =2)+
        geom_line(data=PR_Sep_SW, aes(x = Sep_Dist, y = `Standard Deviation`,colour=`Surface Wind`, group = `Surface Wind`), size = 1)+
        ggtitle("Stdev (O-F) by Surface Wind Speed")+
        labs(x = "RECAT EU Distance", y = "Stdev Error Rate")+
       # coord_cartesian(ylim=c(0,3))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))

plt_10kt <- ggplot() +
        geom_point(data=PR_Sep_SW, aes(x = Sep_Dist, y = `O-F >10kts`,colour=`Surface Wind`, group = `Surface Wind`), size =2)+
        geom_line(data=PR_Sep_SW, aes(x = Sep_Dist, y = `O-F >10kts`,colour=`Surface Wind`, group = `Surface Wind`), size = 1)+
        ggtitle("10kt Error Rate by Surface Wind Speed")+
        labs(x = "RECAT EU Distance", y = ">10kt Error Rate")+
        #coord_cartesian(ylim=c(0,0.0015))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))

plt_50nm <- ggplot() +
        geom_point(data=PR_Sep_SW, aes(x = Sep_Dist, y = `O-F >0.5NM`,colour=`Surface Wind`, group = `Surface Wind`), size =2)+
        geom_line(data=PR_Sep_SW, aes(x = Sep_Dist, y = `O-F >0.5NM`,colour=`Surface Wind`, group = `Surface Wind`), size = 1)+
        ggtitle("0.5Nm Error Rate by Surface Wind Speed")+
        labs(x = "RECAT EU Distance", y = ">0.5nm Error Rate")+
        #coord_cartesian(ylim=c(0,0.0015))+
        scale_y_continuous(labels = scales::scientific)+
        scale_colour_brewer(type = "seq", palette = "Paired", direction = -1)+
        theme(axis.line = element_line(colour = "black",size = 0.5, linetype = "solid"), plot.title =  element_text(size=16), legend.title=element_blank(),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


#Save results plots to chart
png(width=1000, height = 600, file= paste(out_data, "\\Plots by Surfance Wind Band.png", sep = ""))
grid.arrange(plt_mean, plt_std, plt_10kt, plt_50nm ,ncol=2,nrow=2)
dev.off()


