

rm(list = ls())

#--------------------------------------------------------------------#
# Imports
#--------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(data.table)
library(RODBC)
library(dplyr)
library(ggplot2)
library(stringr)
library(pastecs)
library(RColorBrewer)
library(gridExtra)


database_name <- "NavCan_TBS_V2"

# SQL Server database connection
con <- RODBC::odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.23", database_name, "ruser", "Th!nkruser"
))


# Name of project ORD directory on Dropbox (add/select as needed)
Root_Dir <- c("C:/Users", "D:", "E:")
User_Dir <- c("", "Derry", "George Clark", "Michael Cowham", "Catherine")
Proj_Dir <- c(
  "Dropbox (Think Research)/NATS Projects/NATS LVNL Schiphol/Phase 2/2. ORD",
  "Dropbox (Think Research)/NATS Projects/NATS NavCanada TBS/ORD Calibration/Output",
  "Dropbox (Think Research)/NATS Projects/NATS NavCanada TBS/Data Analysis/Outputs/ORD Output"
)
Project_Directory <- file.path(Root_Dir[1], User_Dir[4], Proj_Dir[3]) # Select the correct index numbers!

speed_profile_folder <- "Speed Profiles 10_02_21"

modeldata <- fread(file.path(Project_Directory, speed_profile_folder, "Approach_Speed_Profiles.csv"))

modeldata$a1 <- as.numeric(modeldata$a1)
modeldata$a2 <- as.numeric(modeldata$a2)
modeldata$b <- as.numeric(modeldata$b)
modeldata$n1 <- as.numeric(modeldata$n1)
modeldata$n2 <- as.numeric(modeldata$n2)
modeldata$d <- as.numeric(modeldata$d)
modeldata$d2 <- as.numeric(modeldata$d2)

modeldata <- mutate(modeldata, vref = a1 - landing_adjustment, decel_value = b - a2)

a1_min <- 75
a1_max <- 220

a2_min <- 75
a2_max <- 220

b_min <- 100
b_max <- 180

n1_min <- 0
n1_max <- 12

n2_min <- 0
n2_max <- 14

modeldata_filtered <- filter(modeldata, a1 >= a1_min & a1 <= a1_max, a2 >= a2_min & a2 <= a2_max, b >= b_min & b <= b_max, n1 >= n1_min & n1 <= n1_max, n2 >= n2_min & n2 <= n2_max)

plot_ac_type <- function(ac_type){
  
  plot_data <- filter(modeldata_filtered, Follower_Aircraft_Type == ac_type)
  
  p1 <- ggplot(plot_data)+
    geom_point(mapping = aes(x = a2, y = n2, color = b))

    
  p2 <- ggplot(plot_data)+
    geom_point(mapping = aes(x = (b - a2), y = n2, color = a2))
  
  return(grid.arrange(p1, p2))
  
}

plot_single_aircraft <- function(fp_id, gspd){
  
  fp_id <- 2123
  
  plot_data <- sqlQuery(con, paste0("SELECT * FROM vw_Radar_Track_Point_Derived WHERE Range_To_Threshold IS NOT NULL AND Flight_Plan_ID = ", fp_id))
  
  p <- ggplot(plot_data)+
    geom_point(mapping = aes(x = Range_To_Threshold, y = Mode_S_IAS))+
    xlim(0, 10)+
    theme_bw()
  
  if (gspd == T) p <- p + geom_point(mapping = aes(x = Range_To_Threshold, y = Mode_S_GSPD), color = "red")
  
  return(p)
}

plot_ac_type("B738")
plot_single_aircraft(2123, T)

