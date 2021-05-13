# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Data Summary Counts                                   #
#                |                                                        #
# Version No.    |  1.0                                                   #
#                |                                                        #
# Date Modified  |  14/01/2021                                            #
#                |                                                        #
# Author(s)      |  Catherine Mason                                       #
#                |                                                        #
# Project        |  NavCan TBS                                            #
#                |                                                        #
# Purpose        |  Sense check results from data load                    #
#                |                                                        #
# ----------------------------------------------------------------------- #

rm(list = ls())

library(RODBC)
library(data.table)
library(ggplot2)
library(dplyr)
library(getPass)
library(lubridate)

#Use a version number derived from date or define manually
version <- paste0(Sys.Date(), " ","V1.0 (AH)")
# version <- "2021-05-04 V1.0 (AH)"

use_same_input_version <- F

if (use_same_input_version == T) {
  input_version <- version
} else if (use_same_input_version == F) {
  input_version <- "2021-05-04 V1.0 (AH)"   #Manually define this if you want different input output version numbers
}

#Set server  with IP then tied to this
Server <- "Maverick" #or Goose

if (Server == "Maverick") {ip <- "192.168.1.23"}
if (Server == "Goose") {ip <- "192.168.1.39"}

#Find location of script and functions file based for running in shiny or in RSTUDIO

# --------------------------------------------------------------------------- #
ModuleFolder <- "GWCS"
ModuleSubfolder <- "Calibration & Validation"
Script_out <- "Data Summary Output"
OutputFolder <- paste(ModuleFolder, Script_out, version, sep = "/")
# --------------------------------------------------------------------------- #

#Set to 1 when in git structure

FileFlag <- c("global.R", "GlobalPlaceholder.txt")[1]
ResourcesFolder <- c("resources", "GlobalFunctionsPlaceholder")[1]
AlgoResourcesFolder <- c("algorithm_functions", "AlgoFunctionsPlaceholder")[1]
ModulesFolder <- c("modules", "ModulesPlaceholder")[1]

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  Base_Dir <- getwd()
  Global_Dir <- Base_Dir
  Script_Dir <- file.path(Base_Dir)
  while (!file.exists(file.path(Global_Dir, FileFlag))){
    Global_Dir <- file.path(Global_Dir, "..")
  }
} else {
  Global_Dir <- getwd()
  Script_Dir <- file.path(Global_Dir, ModulesFolder, ModuleFolder, ModuleSubfolder)
}

Global_Dir <- file.path(Global_Dir, ResourcesFolder)
Algo_Func_Dir <- file.path(Global_Dir, AlgoResourcesFolder)

# Global Functions, imports & parameters
source(file.path(Global_Dir, "Imports.R"), local = F)
source(file.path(Global_Dir, "unit conversions.R"), local = F)
source(file.path(Global_Dir, "functions.R"), local = F)

project <- as.numeric(getPass(msg = "Choose a Project: NAV TBS = 1,  IA LVNL = 2, Heathrow PWS = 3", noblank = FALSE, forcemask = FALSE))

Base_Dir <- GetSaveDirectory(Project = project, Algorithm = OutputFolder, IorO = "Outputs")
Create_Directory(Base_Dir)

inputs_dir <- GetSaveDirectory(project, ModuleFolder, "Inputs")

out_data <- Base_Dir

#Set the database name for SQL connection
database <- "NavCan_TBS_V3"

con <- Get_RODBC_Database_Connection(IP = ip, Database = database)


#For the future this should be moved to  Data analysis > Inputs

fp_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Think Research)", "NATS Projects", "NATS NavCanada TBS", "15 Mode S Data Collection", "Data Examples", "Flight Plan Data")


# Read in the data
fp <- as.data.table(sqlQuery(con, "SELECT * FROM tbl_Flight_Plan"))
rtp <- as.data.table(sqlQuery(con, "SELECT * FROM tbl_Radar_Track_Point"))



# flight planned arrivals per day - update folder with new flightplan data
fp_files <- list.files(fp_dir, pattern = "*.csv", full.names = T)
Flightplan <- rbindlist(lapply(fp_files, function(i) fread(i, na.strings = "NULL")))
Flightplan$Date <- as.Date(ifelse(
  !is.na(Flightplan$ACTUAL_TIME_OF_ARRIVAL),
  as.Date(Flightplan$ACTUAL_TIME_OF_ARRIVAL, format = "%d/%m/%Y %H:%M"),
  ifelse(
    !is.na(Flightplan$ACTUAL_TIME_OF_DEPARTURE),
    as.Date(Flightplan$ACTUAL_TIME_OF_DEPARTURE, format = "%d/%m/%Y %H:%M"),
    NA
  )
), origin = "1970-01-01")

Flightplan <- Flightplan[Date %in% as.Date(unique(rtp$Track_Date), format = "%d/%m/%Y")]
Flightplan <- Flightplan[Flightplan$DESTINATION_AIRPORT == "CYYZ"]
mean(table(unique(Flightplan[,c("ACID", "Date")])$Date))

# Observed arrivals per day
mean(table(unique(rtp[,c("Flight_Plan_ID", "Track_Date")])$Track_Date))


# Check for missing data


unique_rtp <- rtp[!duplicated(rtp$Flight_Plan_ID),]

counts_rtp <- unique_rtp %>%
  group_by(Track_Date) %>%
  summarise(count_rtp = n())
# counts_rtp$Date <-  as.Date(as.character(counts_rtp$Track_Date), format = "%d/%m/%y")
counts_rtp$Date <-  dmy(counts_rtp$Track_Date)
counts_rtp <- counts_rtp[order(counts_rtp$Date),]

counts_fp <- Flightplan %>%
  group_by(Date) %>%
  summarise(count_fp = n())
counts_fp <- counts_fp[order(counts_fp$Date),]

# test <- cbind(counts_rtp, counts_fp )
test <- full_join(counts_rtp, counts_fp, by = "Date")
test$difference <- test$count_rtp - test$count_fp



#### ac per day rate - based on FP data
# fp_files <- list.files(paste0("C:\\Users\\", user,"\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\15 Mode S Data Collection\\NavCan 2 month data config\\Flight Plans"), pattern="*.csv" , full.names = T)
df <- data.table()
d <- data.table()
for (i in 1:length(fp_files)){
  fp <- fread(fp_files[i], na.strings = "NULL")
  fp$Date <- as.Date(ifelse(
    !is.na(fp$ACTUAL_TIME_OF_ARRIVAL),
    as.Date(fp$ACTUAL_TIME_OF_ARRIVAL, format = "%d/%m/%Y %H:%M"),
    ifelse(
      !is.na(fp$ACTUAL_TIME_OF_DEPARTURE),
      as.Date(fp$ACTUAL_TIME_OF_DEPARTURE, format = "%d/%m/%Y %H:%M"),
      NA
    )
  ), origin = "1970-01-01")

  #Flightplan <- Flightplan[Date %in% as.Date(unique(rtp$Track_Date), format = "%d/%m/%Y")]
  fp <- fp[fp$DESTINATION_AIRPORT == "CYYZ"]
  d$num <- i
  d$mean <- round(mean(table(unique(fp[,c("ACID", "Date")])$Date)),0)
  d$median <- round(median(table(unique(fp[,c("ACID", "Date")])$Date)),0)
  d$min <- round(min(table(unique(fp[,c("ACID", "Date")])$Date)),0)
  d$max <- round(max(table(unique(fp[,c("ACID", "Date")])$Date)),0)
  df <- rbind(df, d)
}



## ac per day rate - based on GWCS 4nm
# gwcs <- fread(paste0("C:\\Users\\", user,"\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Inputs\\GWCS_Input\\2021.04.13\\vw_Mode_S_Wind_Forecast_4nm.csv"))
gwcs <- fread(file.path(inputs_dir, input_version, "vw_Mode_S_Wind_Forecast_4nm.csv"))
gwcs$month <- month(gwcs$FP_Date)
g <- data.table()
g1 <- data.table()
for (m in unique(gwcs$month)){
  gwcs_month <- gwcs[month == m]
  g1$month <- m
  g1$mean <-  round(mean(table(unique(gwcs_month[,c("Callsign", "FP_Date")])$FP_Date)),0)
  g1$median <- round(median(table(unique(gwcs_month[,c("Callsign", "FP_Date")])$FP_Date)),0)
  g1$min <- round(min(table(unique(gwcs_month[,c("Callsign", "FP_Date")])$FP_Date)),0)
  g1$max <- round(max(table(unique(gwcs_month[,c("Callsign", "FP_Date")])$FP_Date)),0)
  g <- rbind(g, g1)
}


lab_months=c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb")
png(file.path(out_data, "Flightplan_count.png"), height = 600, width = 900)
p <- ggplot(df, aes(num,mean))+
  geom_line()+
  theme(text = element_text(size=18))+
  labs(x = "Month", y="Mean per day")
p + scale_x_continuous(breaks = rep(1:7,1), labels=lab_months) +
  scale_y_continuous(breaks = seq(floor(min(df$mean)/10)*10,ceiling(max(df$mean)/10)*10,10))
dev.off()


lab_month <- c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb")
png(file.path(out_data, "GWCS_count.png"), height = 600, width = 900)
f <- ggplot(g, aes(month,mean))+
  geom_line()+
  theme(text = element_text(size=18))+
  labs(x = "Month", y="Mean per day")
f + scale_x_continuous(labels=lab_month) +
  scale_y_continuous(breaks = seq(floor(min(g$mean)/10)*10,ceiling(max(g$mean)/10)*10,5))
dev.off()
