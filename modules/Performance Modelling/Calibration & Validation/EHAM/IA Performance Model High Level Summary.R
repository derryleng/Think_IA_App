# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Ad hoc Performance Model Outputs                      #
# Version No.    |  1.0                                                   #
# Date Modified  |  30/09/2020                                            #
# Author(s)      |  Michael Cowham                                        #
# Project        |  Schiphol IA                                           #
# Purpose        |  To produce additional plots for the PM analysis       #
# Note           |  Will merge into the wider PM script                   #
#                |                                                        #
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# 1. Version History------------------------------------------------------
#
# 1.  Initial version for production of Circling Analysis v1.ppt
# 2.  Update by George Clarke for corrected Mode C calculations
# 3.  Update by Michael Cowham to calculate the altitude of the follower.  Used
#     for Circling Analysis v2.ppt
# ----------------------------------------------------------------------- #

rm(list = ls())

library(data.table)
library(RODBC)
library(dplyr)
library(ggplot2)
library(smooth)
library(modelr)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(ggrepel)


# ----------------------------------------------------------------------- #
# 1. Configuration ------------------------------------------------------
# ----------------------------------------------------------------------- #

# Version (determines output directory - must be string)
version <- "v1.2"

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
base_dir <- getwd()

# Reference data directory
ref_data <- file.path(base_dir)

# Output directory
out_data <- file.path(base_dir, "Output", version)
if (!dir.exists(out_data)) dir.create(out_data)

# Database dataset - WHEN use_database IS TRUE
db_ip <- "192.168.1.39"
db_name_1 <- "LVNL_UTMA_Validation"
db_name_2 <- "Heathrow_TWA_UTMA_Validation"

# Get Connection
con1 <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, db_name_1, "ruser", "Th!nkruser"
))

# Get Connection
con2 <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, db_name_2, "ruser", "Th!nkruser"
))

# ----------------------------------------------------------------------- #
# 1. Data Load  ------------------------------------------------------
# ----------------------------------------------------------------------- #

pm2 <- fread(file.path(base_dir, "Post-Process Output\\v1.0.2\\IA_Calculations_Filtered.csv"))

# Create a surface headwind group

eham_ref_time <- sqlQuery(con1, "SELECT * FROM tbl_Reference_Recat_Separation_Time", stringsAsFactors=F)
egll_ref_time <- sqlQuery(con2, "SELECT * FROM tbl_Reference_Recat_Separation_Time", stringsAsFactors=F)

wake_conversion <- data.table(UK_Name = c("SUPER", "HEAVY", "UPPER", "MEDIUM", "SMALL", "LIGHT"), RECAT_Name = c("A", "B", "C", "D", "E", "F"))

egll_ref_time <- inner_join(egll_ref_time, wake_conversion, by = c("Leader_WTC" = "UK_Name")) %>% mutate(Leader_WTC = RECAT_Name) %>% inner_join(wake_conversion, by = c("Follower_WTC" = "UK_Name")) %>% mutate(Follower_WTC = RECAT_Name.y) %>% select(Leader_WTC, Follower_WTC, Reference_Wake_Separation_Time)

# Process the PM outputs for the Medians

median_fts_all_wind <- filter(pm2, !is.na(Recat_TBS_Wake_Separation_Time), ROT_Bolster_Flag == 0) %>% group_by(LF_Pair_RECAT) %>% summarise(MedianFT = median(Recat_Perfect_1DME_Time_Separation, na.rm = T)) %>% ungroup() 
median_fts_low_wind <- filter(pm2, !is.na(Recat_TBS_Wake_Separation_Time), ROT_Bolster_Flag == 0, Observed_AGI_Surface_Headwind >= -5, Observed_AGI_Surface_Headwind <= 5) %>% group_by(LF_Pair_RECAT) %>% summarise(MedianFT = median(Recat_Perfect_1DME_Time_Separation, na.rm = T)) %>% ungroup() 

# Check for 1DME vs 0DME differences

median_fts_all_wind_0dme <- filter(pm2, !is.na(Recat_TBS_Wake_Separation_Time), ROT_Bolster_Flag == 0) %>% group_by(LF_Pair_RECAT) %>% summarise(MedianFT = median(Recat_Perfect_0DME_Time_Separation, na.rm = T)) %>% ungroup() 
median_fts_low_wind_0dme <- filter(pm2, !is.na(Recat_TBS_Wake_Separation_Time), ROT_Bolster_Flag == 0, Observed_AGI_Surface_Headwind >= -5, Observed_AGI_Surface_Headwind <= 5) %>% group_by(LF_Pair_RECAT) %>% summarise(MedianFT = median(Recat_Perfect_0DME_Time_Separation, na.rm = T)) %>% ungroup() 

# Join the two sets of refernce times together

combined_ref_time <- inner_join(egll_ref_time, eham_ref_time, by = c("Leader_WTC" = "Leader_WTC", "Follower_WTC" = "Follower_WTC")) %>% rename(Reference_Wake_Time_EGLL = Reference_Wake_Separation_Time.x, Reference_Wake_Time_EHAM = Reference_Wake_Separation_Time.y)
fwrite(combined_ref_time, file.path(out_data, "EHAM vs EGLL Reference Times.csv"))

# Join the EHAM reference times to the medians

eham_ref_vs_median_low <- mutate(eham_ref_time, LF_Pair_RECAT = paste0(Leader_WTC, "-", Follower_WTC)) %>% inner_join(median_fts_low_wind) %>% arrange(Leader_WTC, Follower_WTC) %>% mutate(Difference = MedianFT - Reference_Wake_Separation_Time)
eham_ref_vs_median_all <- mutate(eham_ref_time, LF_Pair_RECAT = paste0(Leader_WTC, "-", Follower_WTC)) %>% inner_join(median_fts_all_wind) %>% arrange(Leader_WTC, Follower_WTC) %>% mutate(Difference = MedianFT - Reference_Wake_Separation_Time)

eham_ref_vs_median_low_0dme <- mutate(eham_ref_time, LF_Pair_RECAT = paste0(Leader_WTC, "-", Follower_WTC)) %>% inner_join(median_fts_low_wind_0dme) %>% arrange(Leader_WTC, Follower_WTC) %>% mutate(Difference = MedianFT - Reference_Wake_Separation_Time)
eham_ref_vs_median_all_0dme <- mutate(eham_ref_time, LF_Pair_RECAT = paste0(Leader_WTC, "-", Follower_WTC)) %>% inner_join(median_fts_all_wind_0dme) %>% arrange(Leader_WTC, Follower_WTC) %>% mutate(Difference = MedianFT - Reference_Wake_Separation_Time)


fwrite(eham_ref_vs_median_low, file.path(out_data, "EHAM Reference versus Perfect Low 1DME.csv"))
fwrite(eham_ref_vs_median_all, file.path(out_data, "EHAM Reference versus Perfect All 1DME.csv"))

fwrite(eham_ref_vs_median_low_0dme, file.path(out_data, "EHAM Reference versus Perfect Low 0DME.csv"))
fwrite(eham_ref_vs_median_all_0dme, file.path(out_data, "EHAM Reference versus Perfect All 0DME.csv"))

# Check the sample size

eham_counts <- filter(pm2, !is.na(Recat_TBS_Wake_Separation_Time), ROT_Bolster_Flag == 0) %>% group_by(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat) %>% summarise(CountN = n()) %>% ungroup() %>% arrange(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat)
fwrite(eham_counts, file.path(out_data, "EHAM PM Counts.csv"))


# Check out the wind effect - why do the values differ from the reference times


  
# ----------------------------------------------------------------------- #
# 2. Plots   ------------------------------------------------------
# ----------------------------------------------------------------------- #

# Plot the two sets of reference times

png(filename = file.path(out_data, "EGLL vs EHAM Reference Times.png"), width = 900, height = 600)
p <- ggplot(data = combined_ref_time)+
  geom_point(mapping = aes(x = Reference_Wake_Time_EGLL, y = Reference_Wake_Time_EHAM))+
  geom_text_repel(mapping = aes(x = Reference_Wake_Time_EGLL, y = Reference_Wake_Time_EHAM, label = paste0(Leader_WTC, " ", Follower_WTC)), segment.size = 0.2, segment.color = "black")+
  geom_abline(slope = 1, intercept = 0, color = "grey")+
  xlim(50, 200)+
  ylim(50, 200)+
  labs(title = "EGLL versus EHAM Wake Reference Times", x = "EGLL Wake Reference Time (s)", y = "EHAM Wake Reference Time (s)")+
  theme_bw()
print(p)
dev.off()

# Plot the Perfect vs Reference Times in Low Winds  

png(filename = file.path(out_data, "EHAM Reference Times vs Perfect Low.png"), width = 900, height = 600)
p <- ggplot(data = eham_ref_vs_median_low)+
  geom_point(mapping = aes(x = Reference_Wake_Separation_Time, y = MedianFT))+
  geom_text_repel(mapping = aes(x = Reference_Wake_Separation_Time, y = MedianFT, label = LF_Pair_RECAT), segment.size = 0.2, segment.color = "black")+
  geom_abline(slope = 1, intercept = 0, color = "grey")+
  xlim(50, 200)+
  ylim(50, 200)+
  labs(title = "EHAM Reference versus Median Perfect 1DME Wake Times in Low Winds", x = "EHAM Wake Reference Time (s)", y = "Median Perfect 1DME Time (s)")+
  theme_bw()
print(p)
dev.off()

# Plot the Perfect vs Reference Times in All Winds  

png(filename = file.path(out_data, "EHAM Reference Times vs Perfect All.png"), width = 900, height = 600)
p <- ggplot(data = eham_ref_vs_median_all)+
  geom_point(mapping = aes(x = Reference_Wake_Separation_Time, y = MedianFT))+
  geom_text_repel(mapping = aes(x = Reference_Wake_Separation_Time, y = MedianFT, label = LF_Pair_RECAT), segment.size = 0.2, segment.color = "black")+
  geom_abline(slope = 1, intercept = 0, color = "grey")+
  xlim(50, 200)+
  ylim(50, 200)+
  labs(title = "EHAM Reference versus Median Perfect 1DME Wake Times in All Winds", x = "EHAM Wake Reference Time (s)", y = "Median Perfect 1DME Time (s)")+
  theme_bw()
print(p)
dev.off()