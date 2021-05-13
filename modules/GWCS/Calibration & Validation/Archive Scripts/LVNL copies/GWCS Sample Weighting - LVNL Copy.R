rm(list = ls()) #this removes a list, what is it?

#--------------------------------------------------------------------#
# Imports
#--------------------------------------------------------------------#

library(tidyverse)
library(data.table)
library(RODBC)
library(dplyr)
library(ggplot2)
library(stringr)
library(pastecs)

#--------------------------------------------------------------------#
# Config
#--------------------------------------------------------------------#

out_dirname <- "v2.1"

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
base_dir <- getwd()

out_data <- file.path(base_dir, "Output", out_dirname)
if (!dir.exists(out_data)) dir.create(out_data)

ref_data <- file.path(base_dir, "Reference Data")

out_data <- file.path(base_dir, "Output", out_dirname, "Analysis Results")
if (!dir.exists(out_data)) dir.create(out_data)
setwd(out_data)

db_data <- dirname(out_data)

con <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};Server={192.168.1.39};Database={LVNL_UTMA_Validation};Uid={ruser};Pwd={Th!nkruser};")

#--------------------------------------------------------------------#
# Functions
#--------------------------------------------------------------------#

"%!in%" <- function(x,y) !("%in%"(x,y)) #what does this do?

#--------------------------------------------------------------------#
# Loading Data
#--------------------------------------------------------------------#

# CSV Loading -------------------------------------------------------#

aircraft_wake_icao4 <- fread(file.path(ref_data, "reference_wake_category_icao_new.csv"))

ref_wake_icao4 <- fread(file.path(ref_data, "reference_wake_separation_dist_icao.csv")) %>% rename(ICAO_Wake_Separation_Distance = 
                                                                                                     Reference_Wake_Separation_Distance)
ref_wake_icao4_orig <- ref_wake_icao4

#ref_wake_recat 

aircraft_wake_recat <- fread(file.path(ref_data, "reference_wake_category_recat_new.csv"))

#fp_data <- fread(file.path(ref_data, "flight_plan_data.csv"))

# SQL Query Loading -------------------------------------------------#

ref_wake_recat <- sqlQuery(con, sprintf("SELECT * FROM tbl_Reference_Recat_Separation_Dist"))  %>% as.data.table() %>%
  mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance/1852) %>% rename(RECAT_Wake_Separation_Distance = Reference_Wake_Separation_Distance)

lp_data <- sqlQuery(con, sprintf("SELECT Landing_Pair_ID,
                    Landing_Pair_Type,
                    Leader_Flight_Plan_ID,
                    Follower_Flight_Plan_ID
                    FROM tbl_Landing_Pair"))  %>% as.data.table()

#pdata1 <- sqlQuery(con, sprintf("SELECT * FROM vw_eTBS_Performance_Model")) %>% as.data.table()

sep_data <- sqlQuery(con, sprintf("SELECT 
                                     Landing_Pair_ID,
                                     Observed_0DME_Separation_Distance,
                                     Observed_1DME_Separation_Distance,
                                     Observed_4DME_Separation_Distance
                                   FROM vw_eTBS_Performance_Model"))

#pdata1_original <- pdata1

fp_data <- sqlQuery(con, sprintf("SELECT 
	                                  fpd.Flight_Plan_ID,
	                                  fp.FP_Date,
	                                  fp.FP_Time,
	                                  fp.Callsign,
	                                  fp.Aircraft_Type,
	                                  fp.Landing_Runway,
	                                  fpd.Landing_Runway as Original_Runway
                                  FROM tbl_Flight_Plan fp
                                  INNER JOIN tbl_Flight_Plan_Derived fpd 
                                  ON fp.Flight_Plan_ID = fpd.Flight_Plan_ID")) %>% as.data.table()


#--------------------------------------------------------------------#
# Parameters & Pre-processing
#--------------------------------------------------------------------#

# Actual behaviour of the leader and follower
obs_lead_ias_min <- 80
obs_lead_ias_max <- 180
obs_follow_ias_min <- 100
obs_follow_ias_max <- 200
obs_follow_ias_max_tight <- 200

# Separation Accuracy: Entire Dataset
sep_accuracy_max <- 3

# Separation Accuracy: Tight Dataset
sep_accuracy_max_tight <- 1.5

rw <- c('R04', 'R06', 'R09', 'R18L', 'R18C', 'R18R', 'R22', 'R24', 'R27', 'R36L', 'R36C', 'R36R')
rw_gp <- c('04', '06', '09', '18', '18', '18', '22', '24', '27', '36', '36', '36')
rw_hdg <- as.numeric(c(41.25, 57.92, 86.78, 183.25, 183.22, 183.20, 221.27, 237.95, 266.82, 3.2, 3.22, 3.25))
rdr_min <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

rw <- cbind(rw, rw_gp)
rw <- as.data.frame(cbind(rw, rw_hdg))
rw <- as.data.frame(cbind(rw, rdr_min))
names(rw) = c('Landing_Runway', 'Group', 'Heading', 'Min_Rdr')
rw$Heading <- as.numeric(rw$Heading)
rw$Min_Rdr <- as.numeric(rw$Min_Rdr)

#--------------------------------------------------------------------#
# Pre-Processing
#--------------------------------------------------------------------#

# Stage 1: Use Flight plan information only from those who have landed.
# - Filters for those that have an "original runway" (a track point close to 4DME/1DME of any runway)
# - Uses "Landing_Runway" as sometimes these runways differ due to late circling approaches

valid_fp_data <- fp_data %>% filter(!is.na(Original_Runway)) %>% select(-Original_Runway)

# Stage 2: Make versions of ICAO4 and RECAT-EU Wake distance tables to join

ref_wake_recat_join <- ref_wake_recat %>% mutate(RECAT_Leader_Follower_Pair = paste0(Leader_WTC, Follower_WTC)) %>% select(-c(Leader_WTC, Follower_WTC))
ref_wake_icao4_join <- ref_wake_icao4 %>% mutate(ICAO4_Leader_Follower_Pair = paste0(Leader_ICAO_WTC, Follower_ICAO_WTC)) %>% select(-c(Leader_ICAO_WTC, Follower_ICAO_WTC))
aircraft_wake_icao4_join <- aircraft_wake_icao4
aircraft_wake_recat_join <- aircraft_wake_recat %>% select(-c("Aircraft_Class"))

# Stage 3a: Join Landing Pair to Leader Flight Plan

full_data <- left_join(lp_data, valid_fp_data, by=c("Leader_Flight_Plan_ID"="Flight_Plan_ID")) %>% rename(
  Leader_FP_Date = FP_Date,
  Leader_FP_Time = FP_Time,
  Leader_Callsign = Callsign,
  Leader_Aircraft_Type = Aircraft_Type,
  Leader_Landing_Runway = Landing_Runway
)

full_data <- left_join(full_data, aircraft_wake_icao4_join, by=c("Leader_Aircraft_Type" = "Aircraft_Type")) %>% rename(
  Leader_ICAO_WTC = ICAO_WTC,
  Leader_Aircraft_Class = Aircraft_Class
)

full_data <- left_join(full_data, aircraft_wake_recat_join, by=c("Leader_Aircraft_Type" = "Aircraft_Type")) %>% rename(
  Leader_RECAT_WTC = Recat_WTC,
)

full_data <- left_join(full_data, rw, by=c("Leader_Landing_Runway" = "Landing_Runway")) %>% rename(
  Leader_Runway_Group = Group,
  Leader_Runway_Heading = Heading,
  Leader_Min_Rdr = Min_Rdr
)

# Stage 3b: Join Landing Pair to Follower Flight Plan

full_data <- left_join(full_data, valid_fp_data, by=c("Follower_Flight_Plan_ID"="Flight_Plan_ID")) %>% rename(
  Follower_FP_Date = FP_Date,
  Follower_FP_Time = FP_Time,
  Follower_Callsign = Callsign,
  Follower_Aircraft_Type = Aircraft_Type,
  Follower_Landing_Runway = Landing_Runway
)

full_data <- left_join(full_data, aircraft_wake_icao4_join, by=c("Follower_Aircraft_Type" = "Aircraft_Type")) %>% rename(
  Follower_ICAO_WTC = ICAO_WTC,
  Follower_Aircraft_Class = Aircraft_Class
)

full_data <- left_join(full_data, aircraft_wake_recat_join, by=c("Follower_Aircraft_Type" = "Aircraft_Type")) %>% rename(
  Follower_RECAT_WTC = Recat_WTC,
)

full_data <- left_join(full_data, rw, by=c("Follower_Landing_Runway" = "Landing_Runway")) %>% rename(
  Follower_Runway_Group = Group,
  Follower_Runway_Heading = Heading,
  Follower_Min_Rdr = Min_Rdr
)

# Stage 4: RECAT & ICAO4 Separation Distances

full_data <- mutate(full_data, ICAO4_Leader_Follower_Pair = paste0(Leader_ICAO_WTC, Follower_ICAO_WTC))
full_data <- mutate(full_data, RECAT_Leader_Follower_Pair = paste0(Leader_RECAT_WTC, Follower_RECAT_WTC))

full_data <- left_join(full_data, ref_wake_icao4_join, by=c("ICAO4_Leader_Follower_Pair")) %>% select(-c("ICAO4_Leader_Follower_Pair"))
full_data <- left_join(full_data, ref_wake_recat_join, by=c("RECAT_Leader_Follower_Pair")) %>% select(-c("RECAT_Leader_Follower_Pair"))

# Stage 5: Add on Observed Separation Distances

full_data <- left_join(full_data, sep_data, by=c("Landing_Pair_ID"))

# Stage 6: Find ICAO4 and RECAT Max Constraint Distances (Use Follower_Min_Rdr/Wake)

full_data <- rename(full_data, Min_Rdr = Follower_Min_Rdr) %>% select(-c("Leader_Min_Rdr"))
full_data <- mutate(full_data, ICAO_Separation_Distance = ifelse(Landing_Pair_Type != "Not_In_Trail", ifelse(is.na(ICAO_Wake_Separation_Distance), Min_Rdr, ICAO_Wake_Separation_Distance), 1.5),
                    ICAO_Separation_Type = ifelse(Landing_Pair_Type != "Not_In_Trail", ifelse(is.na(ICAO_Wake_Separation_Distance), "Min_Rdr", "Wake"), "Not_In_Trail"))
full_data <- mutate(full_data, RECAT_Separation_Distance = ifelse(Landing_Pair_Type != "Not_In_Trail", ifelse(is.na(RECAT_Wake_Separation_Distance), Min_Rdr, RECAT_Wake_Separation_Distance), 1.5),
                    RECAT_Separation_Type = ifelse(Landing_Pair_Type != "Not_In_Trail", ifelse(is.na(RECAT_Wake_Separation_Distance), "Min_Rdr", "Wake"), "Not_In_Trail"))


# Stage 7: Find ICAO4 and RECAT Separation Accuracies using Stage 6 and Observed Data

full_data <- mutate(full_data, ICAO_Sep_Accuracy_4DME = Observed_4DME_Separation_Distance - ICAO_Separation_Distance,
                    ICAO_Sep_Accuracy_1DME = Observed_1DME_Separation_Distance - ICAO_Separation_Distance,
                    ICAO_Sep_Accuracy_0DME = Observed_0DME_Separation_Distance - ICAO_Separation_Distance,
                    RECAT_Sep_Accuracy_4DME = Observed_4DME_Separation_Distance - RECAT_Separation_Distance,
                    RECAT_Sep_Accuracy_1DME = Observed_1DME_Separation_Distance - RECAT_Separation_Distance,
                    RECAT_Sep_Accuracy_0DME = Observed_0DME_Separation_Distance - RECAT_Separation_Distance)

full_data <- mutate(full_data, ICAO_Sep_Accuracy_4DME_Rounded = round(ICAO_Sep_Accuracy_4DME, 1),
                    ICAO_Sep_Accuracy_1DME_Rounded = round(ICAO_Sep_Accuracy_1DME, 1),
                    ICAO_Sep_Accuracy_0DME_Rounded = round(ICAO_Sep_Accuracy_0DME, 1),
                    RECAT_Sep_Accuracy_4DME_Rounded = round(RECAT_Sep_Accuracy_4DME, 1),
                    RECAT_Sep_Accuracy_1DME_Rounded = round(RECAT_Sep_Accuracy_1DME, 1),
                    RECAT_Sep_Accuracy_0DME_Rounded = round(RECAT_Sep_Accuracy_0DME, 1))

full_data <- mutate(full_data, ICAO_Under_Sep_4DME = ifelse(ICAO_Sep_Accuracy_4DME_Rounded < 0, 1, 0),
                    ICAO_Under_Sep_1DME = ifelse(ICAO_Sep_Accuracy_1DME_Rounded < 0, 1, 0),
                    ICAO_Under_Sep_0DME = ifelse(ICAO_Sep_Accuracy_0DME_Rounded < 0, 1, 0),
                    RECAT_Under_Sep_4DME = ifelse(RECAT_Sep_Accuracy_4DME_Rounded < 0, 1, 0),
                    RECAT_Under_Sep_1DME = ifelse(RECAT_Sep_Accuracy_1DME_Rounded < 0, 1, 0),
                    RECAT_Under_Sep_0DME = ifelse(RECAT_Sep_Accuracy_0DME_Rounded < 0, 1, 0))

# Stage 8: Filter for queued data with respect to both ICAO and RECAT wake schemes
# Current Queued definition: <= 3NM Sep Accuracy for A380 Leads, <= 2NM for any other aircraft

queued_data_nota380 <- filter(full_data, Leader_Aircraft_Type != "A388") %>% filter(ICAO_Sep_Accuracy_0DME_Rounded <= 2)
queued_data_a380 <- filter(full_data, Leader_Aircraft_Type == "A388") %>% filter(ICAO_Sep_Accuracy_0DME_Rounded <= 3)
queued_data_ICAO <- rbind(queued_data_nota380, queued_data_a380)

queued_data_nota380 <- filter(full_data, Leader_Aircraft_Type != "A388") %>% filter(RECAT_Sep_Accuracy_0DME <= 2)
queued_data_a380 <- filter(full_data, Leader_Aircraft_Type == "A388") %>% filter(RECAT_Sep_Accuracy_0DME <= 3)
queued_data_RECAT <- rbind(queued_data_nota380, queued_data_a380)

rm(queued_data_nota380, queued_data_a380)

#--------------------------------------------------------------------#
# GWCS/ORD Sample Weighting
#--------------------------------------------------------------------#

# Make addition to find Hour (Currently using Follower FP_Time)
weighting_data <- full_data %>% mutate(Hour = floor(Follower_FP_Time/(86400/24)), Month = substr(Follower_FP_Date, 4, 5))
weighting_data_queued_ICAO <- queued_data_ICAO %>% mutate(Hour = floor(Follower_FP_Time/(86400/24)), Month = substr(Follower_FP_Date, 4, 5))
weighting_data_queued_RECAT <- queued_data_RECAT %>% mutate(Hour = floor(Follower_FP_Time/(86400/24)), Month = substr(Follower_FP_Date, 4, 5))

# Use Follower FP_Date
weighting_data <- rename(weighting_data, FP_Date = Follower_FP_Date) %>% select(-c("Leader_FP_Date"))
weighting_data_queued_ICAO <- rename(weighting_data_queued_ICAO, FP_Date = Follower_FP_Date) %>% select(-c("Leader_FP_Date"))
weighting_data_queued_RECAT <- rename(weighting_data_queued_RECAT, FP_Date = Follower_FP_Date) %>% select(-c("Leader_FP_Date"))

# Filter for Wake Pairs
wake_data <- filter(weighting_data, RECAT_Separation_Type == "Wake") #All Data
wake_data_queued_ICAO <- filter(weighting_data_queued_ICAO, ICAO_Separation_Type == "Wake") #Queued Data (ICAO)
wake_data_queued_RECAT <- filter(weighting_data_queued_RECAT, RECAT_Separation_Type == "Wake")

# 1a) Function for getting RECAT Pair Wake distance breakdown:
# for queued data, use wake_data_queued
# for all data, use wake_data

Get_RECAT_Pair_Wake_Distance_Breakdown <- function(data){
  wake_distance_breakdown <- data %>% group_by(RECAT_Wake_Separation_Distance) %>%
    summarise(Count = n())
  wake_distance_breakdown <- filter(wake_distance_breakdown, !is.na(RECAT_Wake_Separation_Distance))
  sum = sum(wake_distance_breakdown$Count)
  wake_distance_breakdown <- wake_distance_breakdown %>% mutate(Percent_2dp = round((Count/sum)*100, digits=2))
  return(wake_distance_breakdown)
}

wake_dist_breakdown <- Get_RECAT_Pair_Wake_Distance_Breakdown(wake_data)
wake_dist_breakdown_queued <- Get_RECAT_Pair_Wake_Distance_Breakdown(wake_data_queued_ICAO)

# 1a) Function for getting RECAT Pair Wake distance distribution from breakdowns:
# for queued data, use wake_dist_breakdown_queued
# for all data, use wake_dist_breakdown

Get_RECAT_Pair_Wake_Distance_Distribution <- function(data){
  plot <- ggplot(data = data, mapping = aes(fill = as.factor(RECAT_Wake_Separation_Distance))) +
    geom_col(mapping = aes(x = as.factor(RECAT_Wake_Separation_Distance), y = Percent_2dp), show.legend=FALSE) + xlab("RECAT-EU Wake Separation Distance") +
    ylab("% of Observations")
  print(plot)
}

Get_RECAT_Pair_Wake_Distance_Distribution(wake_dist_breakdown)
Get_RECAT_Pair_Wake_Distance_Distribution(wake_dist_breakdown_queued)

# 2a)

# Remove Not-In_Trail Pairs
weighting_data <- filter(weighting_data, Landing_Pair_Type != "Not_In_Trail")
weighting_data_queued_ICAO <- filter(weighting_data_queued_ICAO, Landing_Pair_Type != "Not_In_Trail")

# Mean daily arrival pairs
day_breakdown <- weighting_data %>% group_by(FP_Date) %>% summarise(Count=n())
meandaily <- round(sum(day_breakdown$Count)/nrow(day_breakdown), 0)
mediandaily <- median(day_breakdown$Count)

# Mean daily queued arrival pairs
queued_day_breakdown <- weighting_data_queued_ICAO %>% group_by(FP_Date) %>% summarise(Count=n())
meanqueueddaily <- round(sum(queued_day_breakdown$Count)/nrow(queued_day_breakdown), 0)

# Mean daily wake arrival pairs 
wake_day_breakdown <- wake_data %>% group_by(FP_Date) %>% summarise(Count=n())
meandailywake <- round(sum(wake_day_breakdown$Count)/nrow(wake_day_breakdown),0)

# Mean daily queued wake arrival pairs
queued_wake_day_breakdown <- wake_data_queued_ICAO %>% group_by(FP_Date) %>% summarise(Count=n())
meanqueueddailywake <- round(sum(queued_wake_day_breakdown$Count)/nrow(queued_wake_day_breakdown),0)

part2row1 <- data.frame(Metric = "Mean Arrivals (in-trail)", Daily = meandaily, Hourly = round(meandaily/24, 1))
part2row2 <- data.frame(Metric = "Mean Queued Arrivals (in-trail)", Daily = meanqueueddaily, Hourly = round(meanqueueddaily/24, 1))
part2row3 <- data.frame(Metric = "Mean Wake Arrivals (in-trail)", Daily = meandailywake, Hourly = round(meandailywake/24, 1))
part2row4 <- data.frame(Metric = "Mean Queued Wake Arrivals (in-trail)", Daily = meanqueueddailywake, Hourly = round(meanqueueddailywake/24, 1))

part2output <- rbind(part2row1, part2row2, part2row3, part2row4)

fwrite(wake_dist_breakdown, "Wake Pair Proportions.csv")
fwrite(wake_dist_breakdown_queued, "Queued Wake Pair Proportions.csv")
fwrite(part2output, "Arrival Totals.csv")

#-------------------------------------------------------------------------------------------------------------------------------#
# By Day and Hour
queued_wake_hour_day_breakdown <- wake_data_queued_RECAT %>% group_by(FP_Date, Hour) %>% summarise(Count=n())
# By Hour of Day
queued_wake_hour_breakdown <- wake_data_queued_RECAT %>% group_by(Hour) %>% summarise(Count=n())
qhs <- sum(queued_wake_hour_breakdown$Count)
queued_wake_hour_breakdown <- mutate(queued_wake_hour_breakdown, Percent = (Count/qhs)*100)
# By Hour/Month
queued_wake_hour_month_breakdown <- wake_data_queued_RECAT %>% group_by(Month, Hour) %>% summarise(Count=n())

queued_hour_plot <- ggplot(data = queued_wake_hour_breakdown, mapping=aes(x=as.factor(Hour), fill=Count), show.legend=FALSE) + xlab("Hour of Day")
queued_hour_count_plot <- queued_hour_plot + geom_col(mapping = aes(y=Count)) + ylab("Count of Queued Wake Pairs") 
queued_hour_per_plot <- queued_hour_plot + geom_col(mapping = aes(y=Percent)) + ylab("Percentage of Queued Wake Pairs") 

queued_hour_per_plot
#-------------------------------------------------------------------------------------------------------------------------------#
