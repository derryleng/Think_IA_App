# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  A Selection of Ad Hoc Scripts                         #
# Version No.    |  1.0                                                   #
# Date Modified  |  29/04/2021                                            #
# Author(s)      |  Andy Hyde                                             #
# Project        |  Toronto IA                                            #
# Purpose        |  Small scripts for various investigation               #
#                |                                                        #
# ----------------------------------------------------------------------- #


# 1. Quick Parameter Investigation - MC
#      Simulates the Parameter summary script to plot specififc aircraft types


# 2. Plotting Anomalous Values - AH
#      Finds anomolous tracks by looking at unusually fast/slow landings
#      or finds potential go arounds by looking at tracks with a large difference
#      in track time IDs


# 3. Validation Error Investigation - AH
#      Manually plot error types against groupings wake/wind conditions etc.
#      Percentile information by flag type 




#----------------------------------------------------------------------------#
## 1. Quick Parameter Investigation ------------------------------------------
#----------------------------------------------------------------------------#

rm(list = ls())

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

#----------------------------------------------------------------------------#
## 2. Plotting Anomalous Values  ---------------------------------------------
#----------------------------------------------------------------------------#


rm(list = ls())

library(RODBC)
library(plyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggrepel)
library(gridExtra)

# Legacy support
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
base_dir <- getwd()

# Reference data directory
ref_data <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Reference Data")

# Name of project ORD directory on Dropbox (add/select as needed)
Root_Dir <- c("C:/Users", "D:", "E:")
User_Dir <- c("", "Derry", "George Clark", "Michael Cowham", "Catherine", "Andy Hyde")
Proj_Dir <- c(
  "Dropbox (Think Research)/NATS Projects/NATS LVNL Schiphol/Phase 2/2. ORD",
  "Dropbox (Think Research)/NATS Projects/NATS NavCanada TBS/ORD Calibration/Output",
  "Dropbox (Think Research)/NATS Projects/NATS NavCanada TBS/Data Analysis/Outputs/ORD Output"
)
Project_Directory <- file.path(Root_Dir[1], User_Dir[6], Proj_Dir[3]) # Select the correct index numbers!

# Airport ICAO Designation (for adaptation output suffix)
Airport_Code <- "CYYZ"

# Import functions
source("Functions.R", local = T)

database_name <- "NavCan_TBS_V2"

# SQL Server database connection
con <- RODBC::odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.23", database_name, "ruser", "Th!nkruser"
))





Vref_data <- modeldata_filtered_a1


#Calculating Vref for all flights
Vref_data <- mutate(Vref_data,
                    Vref = a1-landing_adjustment_boeing)

###
summary_by_type <- Vref_data %>% group_by(Follower_Aircraft_Type) %>% summarise(vref_mean = mean(Vref, na.rm = T), vref_sd = sd(Vref, na.rm = T)) %>% ungroup()

#CAn use mutate instead of summarise will leavbe data ungrouped


#Creating an empty data frame fo  output, this is slightly Janky, since there is no way of knowing how many
#entries there will be or in which loop it will be initiated I think this is the only way
Anomalous_Flights <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(Anomalous_Flights) <- c("Aircraft Type", "Flight ID", "Speed")

#modeldata_2 %>% select(Follower_Aircraft_Type) %>% distinct()

#Loops for every Aircraft type
for (j in 1:nrow(lss_types)) {

  #Creates a new data set that we can overwrite each iteration
  Desired_Vref <- filter(Vref_data, Follower_Aircraft_Type == paste0(lss_types[j,1]))

  #Calculates the mean and standard deviation for each flight for specific aircraft type
  Vref_sd <- sd(Desired_Vref$Vref)
  Vref_mean <- mean(Desired_Vref$Vref)

  #Taking all final descents outside of 3sd's of the mean
  Fast_landing <- filter(Vref_data, Follower_Aircraft_Type == paste0(lss_types[j,1]) & Vref > (Vref_mean + 4*Vref_sd))
  Fast_id <- Fast_landing$Follower_Flight_Plan_ID

  Slow_landing <- filter(Vref_data, Follower_Aircraft_Type == paste0(lss_types[j,1]) & Vref < (Vref_mean - 3*Vref_sd))
  Slow_id <- Slow_landing$Follower_Flight_Plan_ID


  #Loop to plot the flights that are deemed fast
  #Takes the length of the id vector, if zero will skip,
  #takes the length of the transpose as ncol returns null for a single element
  if(length(t(Fast_id)) > 0) {

    #Loop for all elements in the id entry

    for (i in 1:length(t(Fast_id))){

      #Appending the flight details to the bottom of the table
      FAST <- c(paste0(Desired_Vref[1,4]), paste0(Fast_id[i]), "Fast")
      Anomalous_Flights <- rbind(Anomalous_Flights, FAST)

      #Getting radar data from the datatbase for the desired flight
      query <- paste("SELECT * FROM vw_Radar_Track_Point_Derived
  WHERE Flight_Plan_ID IN (", Fast_id[i],")
  AND Range_To_Threshold IS NOT NULL
  AND Track_Time_Str IS NOT NULL
", sep = "")

      radar_data <- sqlQuery(con, query) %>% as.data.table()

      #Saving to file as png
      png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results", paste0("Fast_", Fast_id[i],"_distance", ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
      print({
        ggplot(radar_data) +
          geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))
      })
      dev.off()

      sorted_radar_data <- radar_data[order(Track_Time_Str)]

      png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results", paste0("Fast_", Fast_id[i],"_time" , ".png")) %>% gsub("%", "%%", .), width = 1500, height = 600)
      print({
        ggplot(radar_data) +
          geom_point(mapping = aes(x=Track_Time_Str, y = Mode_S_IAS))
      })
      dev.off()
    }
  }


  #This loop is essentially identical to the fast equivalent
  if(length(t(Slow_id))>0) {

    for (i in 1:length(t(Slow_id))){

      SLOW <- c(paste0(Desired_Vref[1,4]), paste0(Slow_id[i]), "Slow")
      Anomalous_Flights <- rbind(Anomalous_Flights, SLOW)

      query <- paste("SELECT * FROM vw_Radar_Track_Point_Derived
  WHERE Flight_Plan_ID IN (", Slow_id[i],")
  AND Range_To_Threshold IS NOT NULL
  AND Track_Time_Str IS NOT NULL
", sep = "")

      radar_data <- sqlQuery(con, query) %>% as.data.table()
      #Slow_flight_data <- filter(modeldata_2, Follower_Flight_Plan_ID == Slow_id)
      #ggplot(radar_data) +
      #geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))

      png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results", paste0("Slow_", Slow_id[i],"_distance", ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
      print({
        ggplot(radar_data) +
          geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))
        #labs(title = Slow_id[i], xlab = "Distance to Threshold (NM)", ylab = "Indicated Air Speed (kt)")
        #ggtitle(Slow_id[i])
      })
      dev.off()

      sorted_radar_data <- radar_data[order(Track_Time_Str)]

      png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results", paste0("Slow_", Slow_id[i],"_time" , ".png")) %>% gsub("%", "%%", .), width = 1500, height = 600)
      print({
        ggplot(radar_data) +
          geom_point(mapping = aes(x=Track_Time_Str, y = Mode_S_IAS))
        #labs(title = Slow_id[i], xlab = "Distance to Threshold (NM)", ylab = "Indicated Air Speed (kt)")
        #ggtitle(Slow_id[i])
      })
      dev.off()
    }
  }

}


#Appending with rbind renames columns for some reason
colnames(Anomalous_Flights) <- c("Aircraft Type", "Flight ID", "Speed")



#----------------------------------------------------------------------------#
# Or use a difference in track time -----------------------------------------#


potential_anomalies <- sqlQuery(con, "SELECT DISTINCT Flight_Plan_ID
                                FROM tbl_Mode_S_Wind_Seg
                                WHERE Max_Track_Time -Min_Track_Time > 200") %>% as.data.table()


for (i in 1:length(t(potential_anomalies))) {

  id <- potential_anomalies[i]$Flight_Plan_ID

  query <- paste("SELECT * FROM vw_Radar_Track_Point_Derived
  WHERE Flight_Plan_ID IN (", id,")
  AND Range_To_Threshold IS NOT NULL
  AND Mode_S_IAS IS NOT NULL

", sep = "")

  radar_data <- sqlQuery(con, query) %>% as.data.table()

  png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results Against Distance", paste0(id,"_distance", ".png")) %>% gsub("%", "%%", .), width = 1500, height = 600)
  print({
    ggplot(radar_data) +
      geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))
    #labs(title = Slow_id[i], xlab = "Distance to Threshold (NM)", ylab = "Indicated Air Speed (kt)")
    #ggtitle(Slow_id[i])
  })
  dev.off()

  sorted_radar_data <- radar_data[order(Track_Time_Str)]

  png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results Against Time", paste0(id,"_time" , ".png")) %>% gsub("%", "%%", .), width = 1500, height = 600)
  print({
    ggplot(radar_data) +
      geom_point(mapping = aes(x=Track_Time_Str, y = Mode_S_IAS))
    #labs(title = Slow_id[i], xlab = "Distance to Threshold (NM)", ylab = "Indicated Air Speed (kt)")
    #ggtitle(Slow_id[i])
  })
  dev.off()
}

##We can exclude all flights in the anomaly list with

data_minus_anomalies <- filter(modeldata_filtered_a1, !(Follower_Flight_Plan_ID %in% potential_anomalies$Flight_Plan_ID))



#----------------------------------------------------------------------------#
## 3. Validation Error Investigation  ----------------------------------------
#----------------------------------------------------------------------------#


rm(list = ls())

library(tidyverse)
library(data.table)
library(gridExtra)

user <- "Andy Hyde"

base_dir <- file.path("C:", "Users", user, "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "Data Analysis", "Outputs", "ORD Output")

ref_data <- file.path(base_dir, "Validation 26-04-21 Second Adjustment")

out_data <- file.path(base_dir, "Validation 26-04-21 Second Adjustment")

setwd(out_data)

plots_data <- file.path(out_data, "Error Plots")
if (!dir.exists(plots_data)) dir.create(plots_data)

plots_nested <- file.path(out_data, "Error Plots", "Leader IAS Grouped")
if (!dir.exists(plots_nested)) dir.create(plots_nested)

ord_data <- fread(file.path(ref_data, "Validation Data Post SepN Accuracy.csv"))

adaptation <- fread(file.path("C:/Users/Andy Hyde/Dropbox (Think Research)/NATS Projects/NATS NavCanada TBS/Data Analysis/Outputs/ORD Output/Adaptation V3 15_04_21/Populate_tbl_ORD_Aircraft_Adaptation_New_DecelCYYZ.csv"))
adaptation <- fread(file.path(base_dir, "Validation 19-04-21 AH TEST", "Populate_tbl_ORD_Aircraft_Adaptation_CYYZ.csv"))

# adaptation <- adaptation %>% select(c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead"))

Error_Types <- c("Combined_Wind_Effect_Error", "ORD_Compression_Error", 'ORD_Leader_IAS_Error', "Forecast_Mean_Leader_Wind_Effect_Error", "Forecast_Mean_Follower_Wind_Effect_Error")
Group_Types <- c("Leader_Aircraft_Type", "Follower_Aircraft_Type", "Leader_Standard_Flag", "Leader_Extended_Flag", "Follower_Standard_Flag", "Follower_Extended_Flag", "Leader_ICAO4", "LF_Pair_ICAO4")


Flags <- c("Leader_Standard_Flag", "Leader_Extended_Flag", "Follower_Standard_Flag", "Follower_Extended_Flag")

#-----------------------------------------------------------------------------#
# Printing Percentile information --------------------------------------------#


for (i in Error_Types) {
  for (j in Flags) {

    temp1 <- filter(ord_data, ord_data[[j]] == 0)
    temp2 <- filter(ord_data, ord_data[[j]] == 1)

    print(paste0(i, " quantiles for ", j))
    print(paste0(j, "= 0"))
    print(quantile(temp1[[i]]))
    print(paste0(j, "= 1"))
    print(quantile(temp2[[i]]))
    print("--------------------------------------------------------------------------------")
  }
}

#-----------------------------------------------------------------------------#
# Creating Box Plots by for each error type by each grouping------------------#

for (i in Error_Types) {
  for (j in Group_Types) {

    temp <- select(ord_data, c(paste0(i), paste0(j)))

    png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
    p <- ggplot(temp) +
      geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
    print(p)
    dev.off()

  }
}


#-----------------------------------------------------------------------------#
# Creating box plots for IAS by wake cat--------------------------------------#

Leader_RECAT_types <- ord_data %>% select(Leader_RECAT) %>% distinct()
Leader_RECAT_types <- sort(Leader_RECAT_types$Leader_RECAT)

Error_Types <- c("ORD_Leader_IAS_Error", "Observed_Mean_Leader_IAS", "ORD_Mean_Leader_IAS")

for (i in Error_Types) {
  for (j in Leader_RECAT_types) {

    temp <- filter(ord_data,ord_data$Leader_RECAT == j)

    png(filename = file.path(plots_nested, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
    p <- ggplot(temp) +
      geom_boxplot(aes(x = reorder(Leader_Aircraft_Type, temp[[i]]), y = temp[[i]])) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
    print(p)
    dev.off()

  }
}

#-----------------------------------------------------------------------------#
# Grouping aircraft by type and calculating mean error and 99th percentile ---#
Mean_Summary <- ord_data %>% group_by(Leader_Aircraft_Type) %>% summarise(ORD_Error_mean = mean(ORD_Compression_Error, na.rm = T))

Percentile_Summary <- ord_data %>% group_by(Leader_Aircraft_Type) %>% summarise(ORD_99_percentile = quantile(ORD_Compression_Error, p=0.99))

High_percentile_error <- filter(Percentile_Summary, Percentile_Summary$ORD_99_percentile >= 0.1)

High_percentile_error <- inner_join(High_percentile_error, adaptation %>% select(c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead")), by = c("Leader_Aircraft_Type" = "Aircraft_Type"))

High_percentile_error$Vref_adjustment <- (High_percentile_error$ORD_99_percentile - 0.1) * High_percentile_error$Min_Safe_Landing_Speed_Lead / 4.5

High_percentile_error$new_vref <- High_percentile_error$Min_Safe_Landing_Speed_Lead - High_percentile_error$Vref_adjustment

High_percentile_error$old_adjust <- 0.25*High_percentile_error$Min_Safe_Landing_Speed_Lead / 4.5






ggplot(ord_data) +
  geom_boxplot(aes(x = reorder(Leader_Aircraft_Type, ORD_Compression_Error), y = ORD_Compression_Error)) +
  facet_grid(~Leader_RECAT, scales = "free", space = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


#-----------------------------------------------------------------------------#
#Creating specific plots

i <- "ORD_Compression_Error"
j <- "Follower_Aircraft_Type"

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp) +
  geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
print(p)
dev.off()

i <- "Forecast_Mean_Follower_Wind_Effect_Error"
j <- 'Follower_RECAT'


temp <- select(ord_data, c(paste0(i), paste0(j)))

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp) +
  geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
print(p)
dev.off()

i <- "ORD_Leader_IAS_Error"
j <- 'Leader_RECAT'


temp <- select(ord_data, c(paste0(i), paste0(j)))

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp) +
  geom_boxplot(aes(x = reorder(temp[[j]], temp[[i]]), y = temp[[i]])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste0(i, " by ", j), x = paste0(j), y = paste0(i))
print(p)
dev.off()




i <- "Mean_ORD"
j <- "Leader_Aircraft_Type"

temp1 <- ord_data %>% group_by(Leader_Aircraft_Type) %>% summarise(Mean_ORD = mean(ORD_Compression)) %>% arrange(.$Mean_ORD)
temp1 <- left_join(temp1, ord_data %>% select(Leader_Aircraft_Type, Leader_RECAT) %>% distinct(), by = "Leader_Aircraft_Type")
temp1 <- temp1 %>% group_by(Leader_RECAT)

png(filename = file.path(plots_data, paste0(i, " by ", j, ".png")), width = 1900, height = 1000)
p <- ggplot(temp1) +
  geom_col(aes(x = reorder(Leader_Aircraft_Type, Mean_ORD), y = Mean_ORD, fill=Leader_RECAT)) +
  theme(axis.text.x = element_text(angle = 90))
print(p)
dev.off()





MD11 <- filter(ord_data, ord_data$Leader_Aircraft_Type == "MD11")

IAS <- ggplot(MD11) +
          geom_histogram(aes(x = Observed_Mean_Leader_IAS), binwidth = 0.25*sd(MD11$Observed_Mean_Leader_IAS),
                        col = "red",
                        fill = "blue")

ORD_Error <- ggplot(MD11) +
                geom_histogram(aes(x = ORD_Compression_Error), binwidth = 0.25*sd(MD11$ORD_Compression_Error),
                               col = "red",
                               fill = "blue")
