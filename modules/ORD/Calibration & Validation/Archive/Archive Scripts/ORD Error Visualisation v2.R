# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  ORD Error Visualisation                               #
# Version No.    |  1.0                                                   #
# Date Modified  |  16/09/2020                                            #
# Author(s)      |  Michael Cowham                                        #
# Project        |  Schiphol IA                                           #
# Purpose        |  To investigate large ORD Errors                       #
#                |                                                        #
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

# ----------------------------------------------------------------------- #
# 1. Configuration ------------------------------------------------------
# ----------------------------------------------------------------------- #

# Version (determines output directory - must be string)
version <- "1.0"
user <- "Michael Cowham"

# Set working directory
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  wd <- Sys.glob(file.path("C:", "Users", "*", "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "05 Flying Time Analysis"))
  while (any(!dir.exists(wd), length(wd) == 0)) wd <- readline("Please specify working directory: ")
  setwd(wd)
}
#base_dir <- getwd()

# Reference data directory
#ref_data <- file.path(dirname(base_dir), "Validation - VALR AC")

base_dir <- file.path("C:", "Users", user, "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "Data Analysis", "Outputs", "ORD Output")

#ref_data <- file.path(base_dir, "Validation 26_02_21")
ref_data <- file.path(base_dir, "Validation 05_03_21 TEST (MC)")

prof_data <- file.path(base_dir, "Speed Profiles 24_02_21 Andy 50 THRESH")

#out_data <- file.path(base_dir, "Validation 26_02_21")
out_data <- file.path(base_dir, "Validation 05_03_21 TEST (MC)")


# Output directory
#out_data <- file.path(base_dir, paste0("v", version))
#if (!dir.exists(out_data)) dir.create(out_data)
setwd(out_data)

plots_data <- file.path(out_data, "ORD Plots")
if (!dir.exists(plots_data)) dir.create(plots_data)

#con <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};Server={192.168.1.23};Database={NavCan_TBS_V2};Uid={ruser};Pwd={Th!nkruser};")

con <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};Server={192.168.1.23};Database={NavCan_TBS_V2_Test};Uid={ruser};Pwd={Th!nkruser};")

# Config Parameters

time_offset_position_plot <- 30

m_to_ft <- 3.28084 

# Use database (= T) or previously calculated results file

use_database <- T

# Filter for IAS

filter_ias <- T
ias_max <- 180
ias_min <- 150

# ----------------------------------------------------------------------- #
# 2. Load Data ----------------------------------------------------------
# ----------------------------------------------------------------------- 

# Load the main ORD data set

ord_data <- fread(file.path(ref_data, "Validation Data Post SepN Accuracy.csv"))

#ias_faf <- sqlQuery(con, "SELECT rtpd.Flight_Plan_ID, rtpd.Track_Time, Mode_S_IAS FROM vw_Radar_Track_Point_Derived rtpd INNER JOIN
#(SELECT Flight_Plan_ID, Track_Time, ROW_NUMBER() OVER(PARTITION BY Flight_Plan_ID ORDER BY ABS(Range_To_Threshold - 4.5)) as Row_ID FROM vw_Radar_Track_Point_Derived WHERE Range_To_Threshold is NOT NULL)a
#ON rtpd.Flight_Plan_ID = a.Flight_Plan_ID AND rtpd.Track_Time = a.Track_Time
#WHERE Row_ID = 1 and Mode_S_IAS IS NOT NULL
#ORDER BY Flight_Plan_ID")

#ord_data <- left_join(ord_data, ias_faf, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID"))

# Profile Data

prof_data <- fread(file.path(prof_data, "Approach_Speed_Profiles.csv"))

ord_data <- left_join(ord_data, prof_data, by = c("Leader_Flight_Plan_ID" = "Follower_Flight_Plan_ID"))

# Filter for large errors

#ord_large_errors <- ord_data[Thresh_Accuracy_Perfect_05 == "[0.5NM, Inf)"][order(ORD_Compression_Error, decreasing = T)]
ord_large_errors <- ord_data[ORD_Compression_Error_Rounded >= 0.2 & Forecast_Mean_Follower_Wind_Effect_Error <= 15][order(ORD_Compression_Error, decreasing = T)]

if (filter_ias == T) ord_large_errors <- filter(ord_large_errors, Mode_S_IAS >= ias_min, Mode_S_IAS <= ias_max)
if (filter_ias == T) ord_data <- filter(ord_data, Mode_S_IAS >= ias_min, Mode_S_IAS <= ias_max)

# ----------------------------------------------------------------------- #
# 3. Functions ----------------------------------------------------------
# ----------------------------------------------------------------------- #


plot_ord_profile_ll <- function(lp_id){
  
  #lp_id <- 33154
  #include_gspd <- T
  
  # Get the ORD data line
  
  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)
  
  title_text <- paste0("Leader IAS Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("Leader IAS Error = ", sprintf("%.0f", ord_line$ORD_Leader_IAS_Error), "kt")
  
  p1 <- tryCatch(plot_single_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, F , title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })
  
  title_text <- paste0("Leader GSPD Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("Lead Wind Effect Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error), "kt Follower Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error), "kt")
  
  p2 <- tryCatch(plot_single_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, T , title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })
  
  grid.arrange(p1, p2, nrow = 1)
  
}
    
plot_ord_profile_lf <- function(lp_id, include_gspd){

  #lp_id <- 855810
  #include_gspd <- T
    
  # Get the ORD data line
  
  ord_line <- filter(ord_data, Landing_Pair_ID == lp_id)
  
  title_text <- paste0("Leader IAS Speed Profile for ", ord_line$Leader_Callsign, " ",ord_line$Leader_Aircraft_Type)
  subtitle_text <- paste0("IAS Error = ", sprintf("%.0f", ord_line$ORD_Leader_IAS_Error), " Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Leader_Wind_Effect_Error))
  
  p1 <- tryCatch(plot_ord_aircraft(ord_line$Leader_Flight_Plan_ID, ord_line$Landing_Pair_ID, "L", include_gspd, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })
  
  title_text <- paste0("Follower IAS Speed Profile for ", ord_line$Follower_Callsign, " ",ord_line$Follower_Aircraft_Type)
  subtitle_text <- paste0("IAS Error = ", sprintf("%.0f", ord_line$ORD_Follower_IAS_Error), " Wind Error = ", sprintf("%.0f", ord_line$Forecast_Mean_Follower_Wind_Effect_Error))
  
  p2 <- tryCatch(plot_ord_aircraft(ord_line$Follower_Flight_Plan_ID, ord_line$Landing_Pair_ID, "F", include_gspd, title_text, subtitle_text),
                 error = function(e) {
                   plot.new()
                   text(.5,.5, paste("No data for", lp_id), cex=1, col=rgb(.2,.2,.2,.7))
                 })
  
  grid.arrange(p1, p2, nrow = 1)
  
}

plot_ord_aircraft <- function(fp_id, lp_id, l_or_f, include_gspd, title_text, subtitle_text){
  
  #fp_id <- 247329
  #lp_id <- 855810
  #include_gspd <- F
  #l_or_f <- "L"
  
  leader_track <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))

  # Get the ORD Profile for leader and follower
  
  ord_profile <- sqlQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = '", l_or_f, "'"))
  
  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852, 
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852)
  
  
  if (include_gspd == T){
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD, color = "GSPD"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_GS, color = "ORD GSPD"))+
      
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "GSPD", "ORD IAS", "ORD GSPD"),
                         values = c("IAS" = "black", "GSPD" = "blue", "ORD IAS" = "grey", "ORD GSPD" = "light blue"))+
      theme_bw()+
      theme(legend.position = "bottom")
    
  } else {
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "ORD IAS"),
                         values = c("IAS" = "black", "ORD IAS" = "grey"))+
      theme_bw()+
      theme(legend.position = "bottom")
  }
 
  return(p1)
   
}


plot_single_aircraft <- function(fp_id, lp_id, gspd, title_text, subtitle_text){
  
  #fp_id <- 247329
  #lp_id <- 33154
  #lp_id <- 855810
  #include_gspd <- F
  #l_or_f <- "L"
  
  leader_track <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Flight_Plan_ID = ", fp_id))
  
  # Get the ORD Profile for leader and follower
  
  ord_profile <- sqlQuery(con, paste0("SELECT * FROM tbl_ORD_GS_Profile WHERE Landing_Pair_ID = ", lp_id, " AND This_Pair_Role = 'L'"))
  
  ord_profile <- mutate(ord_profile, Start_IAS = Start_IAS * 3600 / 1852,
                        End_IAS = End_IAS * 3600 / 1852, 
                        Start_GS = Start_GS * 3600 / 1852,
                        End_GS = End_GS * 3600 / 1852,
                        Start_Dist = Start_Dist / 1852,
                        End_Dist = End_Dist / 1852)
  
  
  if (gspd == T){
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD, color = "GSPD"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_GS, color = "ORD GSPD"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("GSPD", "ORD GSPD"),
                         values = c("GSPD" = "blue", "ORD GSPD" = "light blue"))+
      theme_bw()+
      theme(legend.position = "bottom")
    
  } else {
    p1 <- ggplot()+
      geom_point(data = leader_track, mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS, color = "IAS"))+
      geom_line(data = ord_profile, mapping = aes(x = End_Dist, y = End_IAS, color = "ORD IAS"))+
      xlim(0, 10)+
      ylim(100, 200)+
      labs(title = title_text, subtitle = subtitle_text, x = "Range To Threshold (NM)", y = "Speed (kt)")+
      scale_color_manual(name = "Legend",
                         breaks = c("IAS", "ORD IAS"),
                         values = c("IAS" = "black", "ORD IAS" = "grey"))+
      theme_bw()+
      theme(legend.position = "bottom")
  }
  
  return(p1)
  
}

# ----------------------------------------------------------------------- #
# 4. Analysis  ----------------------------------------------------------
# ----------------------------------------------------------------------- #


# Plot the set of large errors

#for (i in 1:nrow(ord_large_errors)){
for (i in 1:100){
  
  #i <- 1
  le_line <- ord_large_errors[i, ]
  
  plot_name <- paste0(i, "-",le_line$Leader_Flight_Plan_ID,"-", le_line$Leader_Callsign, "-", le_line$Follower_Callsign, ".png")
  png(filename = file.path(plots_data, plot_name), width = 900, height = 600)
  plot_ord_profile_ll(le_line$Landing_Pair_ID) 
  dev.off() 
  
}

ord_errors_ias <- filter(ord_large_errors, Mode_S_IAS >= 150)
# ----------------------------------------------------------------------- #
# 4. Profile Analysis  ----------------------------------------------------------
# ----------------------------------------------------------------------- #



ggplot(data = ord_data)+
  geom_histogram(mapping = aes(x = ORD_Compression_Error_Rounded, y = ..density..), color = "black", fill = "red")

ggplot(data = filter(ord_data, Mode_S_IAS >= 150, Mode_S_IAS <= 180))+
  geom_histogram(mapping = aes(x = ORD_Compression_Error_Rounded, y = ..density..), color = "black", fill = "red")


ggplot(data = ias_faf)+
  geom_histogram(mapping  = aes(x = Mode_S_IAS, y = ..density..), color = "black", fill = "blue")+
  xlim(100, 200)

ggplot(data = ias_faf_plus)+
  geom_histogram(mapping  = aes(x = Mode_S_IAS, y = ..density..), color = "black", fill = "blue")+
  xlim(100, 200)

# ----------------------------------------------------------------------- #
# 5. Percentiles   ----------------------------------------------------------
# ----------------------------------------------------------------------- #

pcile_99 <- quantile(filter(ord_data, ORD_Compression_Error_Rounded <= 0.4)$ORD_Compression_Error_Rounded, 0.99, na.rm = T)

ord_data <- mutate(ord_data, ORD_Compression_Error_Adjusted = ORD_Compression_Error_Rounded - pcile_99 + 0.1)

pcile_test <- quantile(ord_data$ORD_Compression_Error_Adjusted, 0.99) 

ggplot(data = ord_data)+
  geom_histogram(mapping = aes(x = ORD_Compression_Error_Adjusted, y = ..density..), color = "black", fill = "red", binwidth = 0.1)


# ----------------------------------------------------------------------- #
# 5. Data Investigation   ----------------------------------------------------------
# ----------------------------------------------------------------------- #

ord_data <- mutate(ord_data, Forecast_Mean_Follower_Wind_Effect_Error = Observed_Mean_Follower_Wind_Effect - Forecast_Mean_Follower_Wind_Effect)

# ORD IAS Profile
# ORD IAS Lead Error
# ORD Follower 

ggplot(data = ord_data)+
  geom_histogram(aes(x = Forecast_Mean_Follower_Wind_Effect_Error, y = ..density..), color = "black", fill = "red", binwidth = 2)


ggplot(data = ord_data)+
  geom_histogram(aes(x = ORD_Leader_IAS_Error, y = ..density..), color = "black", fill = "red", binwidth = 2)

ggplot(data = ord_data)+
  geom_histogram(aes(x = Forecast_Mean_Leader_Wind_Effect_Error, y = ..density..), color = "black", fill = "red", binwidth = 2)


ggplot(data = ord_data)+
  geom_histogram(aes(x = Observed_Mean_Leader_IAS, y = ..density..), color = "black", fill = "red", binwidth = 2)

ggplot(data = ord_data)+
  geom_histogram(aes(x = Leader_FAF_IAS, y = ..density..), color = "black", fill = "red", binwidth = 2)

ggplot(data = ord_data)+
  geom_histogram(aes(x = ORD_Mean_Leader_IAS, y = ..density..), color = "black", fill = "red", binwidth = 2)


ggplot(data = ord_data)+
  geom_boxplot(aes(x = as.factor(ILS_Intercept_Flag), y = Forecast_Mean_Follower_Wind_Effect_Error, color = as.factor(ILS_Intercept_Flag)))

ggplot(data = ord_data)+
  geom_boxplot(aes(x = as.factor(Invalid_Follower_Flag), y = Forecast_Mean_Follower_Wind_Effect_Error, color = as.factor(Invalid_Follower_Flag)))



table(ord_data$ILS_Intercept_Flag)
table(ord_data$Invalid_Follower_Flag)
