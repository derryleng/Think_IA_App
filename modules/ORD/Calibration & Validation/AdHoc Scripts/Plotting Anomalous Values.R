
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


#########


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
#----------------------------------------------------------------------------#

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





###############################################################################
#Unused















#Make a loop here to calculate Vref depending on lss type temporarily just using boeing for all types

AC_Type <- "MD11"

Vref_data <- mutate(Vref_data,
          Vref = a1-landing_adjustment_boeing)
Desired_Vref <- filter(Vref_data, Follower_Aircraft_Type == AC_Type)

Vref_sd <- sd(Desired_Vref$Vref)
Vref_mean <- mean(Desired_Vref$Vref)

Fast_landing <- filter(Vref_data, Follower_Aircraft_Type == AC_Type & Vref > (Vref_mean + 3*Vref_sd))
Fast_id <- Fast_landing$Follower_Flight_Plan_ID

Slow_landing <- filter(Vref_data, Follower_Aircraft_Type == AC_Type & Vref < (Vref_mean - 3*Vref_sd))
Slow_id <- Slow_landing$Follower_Flight_Plan_ID

#Taking length of the transposed data frame as length nrows returns NULL for a single integer value

if(length(t(Fast_id)) > 0) {

  for (i in 1:length(t(Fast_id))){
    query <- paste("SELECT * FROM vw_Radar_Track_Point_Derived
  WHERE Flight_Plan_ID IN (", Fast_id[i],")
  AND Range_To_Threshold IS NOT NULL
  AND Track_Time_Str IS NOT NULL
", sep = "")

    radar_data <- sqlQuery(con, query) %>% as.data.table()
    #fast_flight_data <- filter(modeldata_2, Follower_Flight_Plan_ID == Fast_id)
    #ggplot(radar_data) +
    #geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))

    png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results", paste0("Fast_", Fast_id[i],"_distance", ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
    print({
      ggplot(radar_data) +
        geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))
      #labs(title = Slow_id[i], xlab = "Distance to Threshold (NM)", ylab = "Indicated Air Speed (kt)")
      #ggtitle(Slow_id[i])
    })
    dev.off()

    sorted_radar_data <- radar_data[order(Track_Time_Str)]

    png(filename = file.path("C:/Users/Andy Hyde/Documents/NAVCAN Anomalous results", paste0("Fast_", Fast_id[i],"_time" , ".png")) %>% gsub("%", "%%", .), width = 1500, height = 600)
    print({
      ggplot(radar_data) +
        geom_point(mapping = aes(x=Track_Time_Str, y = Mode_S_IAS))
      #labs(title = Slow_id[i], xlab = "Distance to Threshold (NM)", ylab = "Indicated Air Speed (kt)")
      #ggtitle(Slow_id[i])
    })
    dev.off()
  }
}

if(length(t(Slow_id))>0) {

  for (i in 1:length(t(Slow_id))){
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




ggplot(radar_data) +
  geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))

sorted_radar_data <- radar_data[order(Track_Time_Str)]

ggplot(sorted_radar_data) +
  geom_point(mapping = aes(x=Track_Time_Str, y = Mode_S_IAS))

#CRJ2 Anomaly

Fast_crj2 <- filter(modeldata_2, Follower_Aircraft_Type == "CRJ2" & a1 > 160)
id <- Fast_crj2$Follower_Flight_Plan_ID

query <- paste("SELECT * FROM vw_Radar_Track_Point_Derived
  WHERE Flight_Plan_ID IN (", id,")
  AND Range_To_Threshold IS NOT NULL
  AND Track_Time_Str IS NOT NULL
", sep = "")

radar_data <- sqlQuery(con, query) %>% as.data.table()

flight_crj2_data <- filter(modeldata_2, Follower_Flight_Plan_ID == id)

ggplot(radar_data) +
  geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))

sorted_radar_data <- radar_data[order(Track_Time_Str)]

ggplot(sorted_radar_data) +
  geom_point(mapping = aes(x=Track_Time_Str, y = Mode_S_IAS))



Slow_MD11 <- filter(modeldata_2, Follower_Aircraft_Type == "MD11" & (a1 - landing_adjustment_boeing < 135)
id <- Slow_MD11$Follower_Flight_Plan_ID

query <- paste("SELECT * FROM vw_Radar_Track_Point_Derived
  WHERE Flight_Plan_ID IN (", id,")
  AND Range_To_Threshold IS NOT NULL", sep = "")





query <- paste("SELECT * FROM vw_Radar_Track_Point_Derived
  WHERE Flight_Plan_ID = 10116
  AND Range_To_Threshold IS NOT NULL", sep = "")

radar_data <- sqlQuery(con, query) %>% as.data.table()

ggplot(radar_data) +
  geom_point(mapping = aes(x=Range_To_Threshold, y = Mode_S_IAS))
