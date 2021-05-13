# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  GWCS VALIDATION                                       #
#                |                                                        #
# Version No.    |  5.3                                                   #
#                |                                                        #
# Date Modified  |  09/12/2020                                            #
#                |                                                        #
# Author(s)      |  James de Beauvoir-Tupper, Catherine Mason             #
#                |                                                        #
# Project        |  eTBS Related Projects (NavCan)                        #
#                |                                                        #
# Purpose        |  GWCS Outlier Investigation                            #
#                |                                                        #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
#
#V1.0 - Draft - imports 1 performance table, manually analyses errors with user input
#
#V2.0 - Imports all performance tables, automatically produces charts and saves to working directory
#
#V4.0 - Editable username in code, slight change in filtering, and plot to word doc
#
#V5.0 - Updating code to work with LVNL data
# Go around flights removed (temporary fix)
# HDG plot adjusted slightly
#
# v5.1 - Updated SegmentData Wind Heading plot y axis to use min and max heading rather than 0 to 360 scale
# (Updated anem surface wind speed to use average between 4DME and threshold) -- Currently using old method with 4DME, change if need to use
# Updated spead and heading plots to include surface wind data
#
# v5.2 - Updating to use TBS reference times from database
#
# V5.3 - Updating for NavCan
#
# v5.4 - Updated to use 3.5 sep dist GWCS tables 
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# Load Packages ----------------------------------------------------------
# ----------------------------------------------------------------------- #
rm(list = ls())

library(RODBC) 
library(ggplot2)
library(lattice)
library(plyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(shiny)
library(data.table)
library(gridExtra)

# ----------------------------------------------------------------------- #
# Set Control Parameters --------------------------------------------------
# ----------------------------------------------------------------------- #
# user <- "Catherine"
user <- "Michael Cowham"
#user <- "Andy Hyde"

#database <- "NavCan_UTMA_Validation_DB2"
#database <- "NavCan_TBS_Analysis_UTMA_Validation"
# database <- "NavCan_TBS_V2_Test"
#database <- "App_Test_NavCan_Fusion"
database <- "NavCan_TBS_V2"

#working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Input
input <- paste0("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Inputs\\GWCS_Input\\22.03.2021")
  
# Output directory - Need to set up in script
out_data <-paste0("C:\\Users\\",user,"\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Outputs\\GWCS\\Outlier Investigation\\07.04.2021\\")
if (!dir.exists(out_data)) dir.create(out_data)  


# Error Threshold (knots)
errthresh<-7.5
#Number of errors to investigatge (i.e. number of loops)
numerr<-200

# RefTimes <- c(3, 4, 5, 6, 7, 8)
# #LVNL Ref Time
# RefTimes <- cbind(RefTimes, c(76, 101, 124, 147, 177, 194))

# ----------------------------------------------------------------------- #
# Load Data --------------------------------------------------------------
# ----------------------------------------------------------------------- #

# Load in the 6 GWCS data sets
for (sepdist in c(3, 3.5, 4:8)){
        gwcs_file <-  paste(input,"\\vw_Mode_S_Wind_Forecast_", sepdist, "nm.csv", sep = "")
        #gwcs_file <-  paste(wd,"\\Inputs\\GWCS_Forecasts_May15_Apr16_0_", sepdist, "nm.csv", sep = "")
        gwcs_dist <- fread(file = gwcs_file, na.strings = c("NA", "NULL"))
        
        if (sepdist == 3){
                gwcsperformance <- gwcs_dist
        } else {
                gwcsperformance <- rbind(gwcsperformance, gwcs_dist)
        }
        rm(gwcs_dist)
}

#Connect to database (IP 192.168.1.39 or 192.168.1.23)
con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", "192.168.1.23", database, "ruser", "Th!nkruser"
))

#rawsegs
rawsegs <- as.data.table(sqlQuery(con, "SELECT * FROM vw_Mode_S_Wind_Seg"))
rawsegs_FPID <-  unique(as.data.table(sqlQuery(con, "SELECT [Flight_Plan_ID], [Mode_S_Wind_Seg_ID] FROM tbl_Mode_S_Wind_Seg")))
rawsegs <- merge(rawsegs,rawsegs_FPID, by = "Mode_S_Wind_Seg_ID" )

#Surface wind data
#sfcwind <- read.csv(file = paste(wd,"\\Anemometer_Data_May15_Apr16.csv", head = TRUE, sep = ",", na.strings = c('NULL')))

# Read in the anemometer data 
anem <- as.data.table(sqlQuery(con, "SELECT 
        FP.Flight_Plan_ID,
	FP.FP_Date,
	FP.FP_Time,
	FP.Callsign,
	FP.Landing_Runway,
	FPD.Time_At_4DME,
	Surface_Wind_SPD = (SELECT TOP 1 Anemo_SPD FROM tbl_Anemometer WHERE Landing_Runway = FP.Landing_Runway AND Anemo_Date = FP.FP_Date AND Anemo_Time > FPD.Time_At_4DME - 300 AND Anemo_Time <= FPD.Time_At_4DME + 300  ORDER BY ABS(Anemo_Time-FPD.Time_At_4DME)) / dbo.fnc_GI_Kts_To_M_Per_Sec(),
	--Surface_Wind_SPD = (SELECT AVG(Anemo_SPD) FROM tbl_Anemometer WHERE Landing_Runway = FP.Landing_Runway AND Anemo_Date = FP.FP_Date AND Anemo_Time > FPD.Time_At_4DME AND Anemo_Time <= FPD.Time_At_1DME + 22.5) / dbo.fnc_GI_Kts_To_M_Per_Sec(),
	Surface_Wind_HDG = (SELECT TOP 1 Anemo_HDG FROM tbl_Anemometer WHERE Landing_Runway = FP.Landing_Runway AND Anemo_Date = FP.FP_Date AND Anemo_Time > FPD.Time_At_4DME - 300 AND Anemo_Time <= FPD.Time_At_4DME + 300  ORDER BY ABS(Anemo_Time-FPD.Time_At_4DME)) / dbo.fnc_GI_Degs_To_Rads()
FROM tbl_Flight_Plan AS FP
JOIN tbl_Flight_Plan_Derived AS FPD 
ON FP.Flight_Plan_ID = FPD.Flight_Plan_ID"))

# Load TBS Reference times
RefTimes <- as.data.table(sqlQuery(con, "SELECT Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852
                                                ,Reference_Wake_Separation_Time
                                         FROM tbl_Reference_TBS_Table_Time"))

# Load the Runway Groups
RunGroups <- as.data.table(sqlQuery(con, "SELECT Runway_Name, Runway_Group FROM tbl_Runway"))

# ----------------------------------------------------------------------- #
# Data Processing---------------------------------------------------------
# ----------------------------------------------------------------------- #
######################  CLEAN DATA ######################
#colnames(gwcsperformance)[1] <- "FP_Date"
rawsegs$FP_Date <- as.character(rawsegs$FP_Date)
#rawsegs$FP_Date <- as.Date(rawsegs$FP_Date, format = "%d/%m/%Y")
#gwcsperformance$FP_Date <- as.Date(gwcsperformance$FP_Date, format = "%d/%m/%Y")
rawsegs$FP_Date <- factor(rawsegs$FP_Date, levels = unique(gwcsperformance$FP_Date))

# MC Update to include RW Group
rawsegs <- merge(rawsegs, RunGroups, by.x = "Landing_Runway", by.y = "Runway_Name")
gwcsperformance <- merge(gwcsperformance, RunGroups, by.x = "Landing_Runway", by.y = "Runway_Name")

####################### Wind Direction correction ##################
#calculates reciprocal of wind direction to change to conventional notation (direction from)
rawsegs1 <- subset(rawsegs, Ave_Wind_HDG > 180)
rawsegs2 <- subset(rawsegs, Ave_Wind_HDG < 180)

rawsegs1$Ave_Wind_HDG <- rawsegs1$Ave_Wind_HDG - 180
rawsegs2$Ave_Wind_HDG <- rawsegs2$Ave_Wind_HDG + 180

# Change into -ve wind for <360 >180
#rawsegs2$Ave_Wind_HDG <- rawsegs2$Ave_Wind_HDG -360

rawsegs<- rbind(rawsegs1, rawsegs2)

######### Remove anomaly - This will remove the 'go-around' flights as it picked up segment data for both runways (put in as a 'quick fix') ########
# Find track time range
rawsegs$track_dur <- rawsegs$Max_Track_Time - rawsegs$Min_Track_Time
#rawsegs_copy <- rawsegs

# find all flights with anomaly (difference in track min, max > 200)
flights_greater_200 <- unique(rawsegs[track_dur > 200, c("Flight_Plan_ID", "FP_Date","Callsign")])
fwrite(flights_greater_200, paste0(out_data, "flights_greater_200.csv"))
###################### ADD IN NEW ERROR FIELDS ######################
#Error in GSPD (kts)
gwcsperformance$ForecastGSPD_ObservedGSPD_Mode_S_Error <- (gwcsperformance$Observed_Ave_Mode_S_GSPD - gwcsperformance$Forecast_GSPD_Observed_IAS)

#Calculate the error distance (nm)
gwcsperformance$Sep_Dist <- gwcsperformance$Forecast_Seg_Max - gwcsperformance$Forecast_Seg_Min + 1
#gwcsperformance$Ref_Time <- RefTimes[match(gwcsperformance$Sep_Dist, RefTimes[,1]), 2]
gwcsperformance$Ref_Time <- RefTimes[match(gwcsperformance$Sep_Dist, RefTimes$Reference_Wake_Separation_Distance), Reference_Wake_Separation_Time]
gwcsperformance$Forecast_GWCS_Error_Distance <- gwcsperformance$Forecast_Wind_Effect_IAS_Error * gwcsperformance$Ref_Time / 3600

# Merge anem data (surface wind needed)
gwcsperformance <- merge(gwcsperformance, anem, by = c("FP_Date", "Time_At_4DME", "Callsign"))
rawsegs <- merge(rawsegs, anem, by = "Flight_Plan_ID")

###################### FIND ERRORS  ######################
#RS changed last one to >20 to coincide with QC flag
#Get and order pos errors
gwcs_errors_pos <- gwcsperformance[gwcsperformance$ForecastGSPD_ObservedGSPD_Mode_S_Error > errthresh & gwcsperformance$Observed_Track_GSPD > 80 & !is.na(gwcsperformance$ForecastGSPD_ObservedGSPD_Mode_S_Error) & gwcsperformance$Observed_Max_RTT >= gwcsperformance$Forecast_Seg_Max-2 & gwcsperformance$Observed_Flying_Time>= (gwcsperformance$Forecast_Seg_Max*20)-40 & gwcsperformance$Observed_Ave_Mode_S_GSPD- gwcsperformance$Observed_Track_GSPD<=20, ]
gwcs_errors_pos <- gwcs_errors_pos[order(-gwcs_errors_pos$ForecastGSPD_ObservedGSPD_Mode_S_Error),]
#Get and order negative errors
gwcs_errors_neg <- gwcsperformance[gwcsperformance$ForecastGSPD_ObservedGSPD_Mode_S_Error < -errthresh & gwcsperformance$Observed_Track_GSPD > 80 & !is.na(gwcsperformance$ForecastGSPD_ObservedGSPD_Mode_S_Error) & gwcsperformance$Observed_Max_RTT >= gwcsperformance$Forecast_Seg_Max-2 & gwcsperformance$Observed_Flying_Time>= (gwcsperformance$Forecast_Seg_Max*20)-40 & gwcsperformance$Observed_Ave_Mode_S_GSPD- gwcsperformance$Observed_Track_GSPD<=20, ]
gwcs_errors_neg <- gwcs_errors_neg[order(-gwcs_errors_neg$ForecastGSPD_ObservedGSPD_Mode_S_Error),]

gwcs_errors <- rbind(gwcs_errors_neg,gwcs_errors_pos) # Combined Pos and neg errors
gwcs_errors$abserror <- abs(gwcs_errors$ForecastGSPD_ObservedGSPD_Mode_S_Error) #Create abs error
gwcs_errors <- gwcs_errors[order(-gwcs_errors$ForecastGSPD_ObservedGSPD_Mode_S_Erro),]  #Order data
gwcs_errors$UID <- paste(gwcs_errors$Callsign,gwcs_errors$FP_Date,sep="_") # add UID field


# Remove anomaly from all tables - This will remove the 'go-around' flights as it picked up segment data for both runways
gwcs_errors <- gwcs_errors[!(paste(gwcs_errors$FP_Date, gwcs_errors$Callsign) %in% paste(flights_greater_200$FP_Date, flights_greater_200$Callsign)),]
gwcsperformance <- gwcsperformance[!(paste(gwcsperformance$FP_Date, gwcsperformance$Callsign) %in% paste(flights_greater_200$FP_Date, flights_greater_200$Callsign)),]
rawsegs <- rawsegs[!(rawsegs$Flight_Plan_ID %in% flights_greater_200$Flight_Plan_ID),]

#########################################################
#################### WIND FILTER ########################
#########################################################
#gwcs_errors<- subset(gwcs_errors, gwcs_errors$Observed_Wind_Effect_IAS>=-5 & gwcs_errors$Observed_Wind_Effect_IAS<=5 )

# only returns errors where the wind effect was -5 to +5 kts
##########################################################
#create unique error table (1 error per aircraft) from top 100 errors
#gwcs_errors_unique <- head(gwcs_errors,numerr)
#gwcs_errors_unique <- gwcs_errors_unique[!duplicated(gwcs_errors_unique$UID),]
gwcs_errors_unique <- gwcs_errors[!duplicated(gwcs_errors$UID),]
##gwcs_errors_unique<-gwcs_errors_unique[order(-gwcs_errors_unique$ForecastGSPD_ObservedGSPD_Mode_S_Error),]

numrow <- nrow(gwcs_errors_unique) #variable used in loop later


#create table with data used in the loop, for easy access to required variables
plotdata <- data.frame(gwcs_errors_unique$Flight_Plan_ID, gwcs_errors_unique$FP_Date,gwcs_errors_unique$Time_At_4DME,gwcs_errors_unique$ForecastGSPD_ObservedGSPD_Mode_S_Error,gwcs_errors_unique$Callsign,gwcs_errors_unique$Sep_Dist,gwcs_errors_unique$Landing_Runway.x,gwcs_errors_unique$Runway_Group,  gwcs_errors_unique$Forecast_GWCS_Error_Distance, gwcs_errors_unique$Surface_Wind_SPD)

#Add wind band info
plotdata$Surface_Wind_SPD_band <- NA
for (i in 1:nrow(plotdata)){
  if(!is.na(plotdata$gwcs_errors_unique.Surface_Wind_SPD[i]) & plotdata$gwcs_errors_unique.Surface_Wind_SPD[i] <= 9 & plotdata$gwcs_errors_unique.Surface_Wind_SPD[i] >= 4){
    plotdata$Surface_Wind_SPD_band[i] <- '[4,9]'
  } else {
    plotdata$Surface_Wind_SPD_band[i] <- NA
  }
}

# Order by Forecast_GWCS_Error_Distance (new addition)
plotdata_gwcs_error_dist <- plotdata[order(-plotdata$gwcs_errors_unique.Forecast_GWCS_Error_Distance),]
plotdata_wind_effect_error <- plotdata[order(-plotdata$gwcs_errors_unique.ForecastGSPD_ObservedGSPD_Mode_S_Error),]
plotdata_wind_effect_error_4_9 <- plotdata[plotdata$Surface_Wind_SPD_band %in% "[4,9]",]
plotdata_wind_effect_error_4_9 <- plotdata_wind_effect_error_4_9[order(-plotdata_wind_effect_error_4_9$gwcs_errors_unique.ForecastGSPD_ObservedGSPD_Mode_S_Error),]

plotdata_gwcs_error_dist$ID <- c(1:numrow)
plotdata_wind_effect_error$ID <- c(1:numrow)
plotdata_wind_effect_error_4_9$ID <- c(1:nrow(plotdata_wind_effect_error_4_9))
#change date to character, so can be used in variable
plotdata_gwcs_error_dist <- rapply(plotdata_gwcs_error_dist,as.character,classes="factor",how="replace")
plotdata_wind_effect_error <- rapply(plotdata_wind_effect_error,as.character,classes="factor",how="replace")
plotdata_wind_effect_error_4_9 <- rapply(plotdata_wind_effect_error_4_9,as.character,classes="factor",how="replace")
#adapt headers for outputting table
names(plotdata_gwcs_error_dist) <- c('Flight_Plan_ID','FP_Date','Time_At_4DME','ForecastGSPD_ObservedGSPD_Mode_S_Error','Callsign','Sep_Dist','Landing_Runway','Forecast_GWCS_Error_Distance','Surface_Wind_SPD','Surface_Wind_SPD_band','ID')
names(plotdata_wind_effect_error) <- c('Flight_Plan_ID','FP_Date','Time_At_4DME','ForecastGSPD_ObservedGSPD_Mode_S_Error','Callsign','Sep_Dist','Landing_Runway','Forecast_GWCS_Error_Distance','Surface_Wind_SPD','Surface_Wind_SPD_band','ID')
names(plotdata_wind_effect_error_4_9) <- c('Flight_Plan_ID','FP_Date','Time_At_4DME','ForecastGSPD_ObservedGSPD_Mode_S_Error','Callsign','Sep_Dist','Landing_Runway','Forecast_GWCS_Error_Distance','Surface_Wind_SPD','Surface_Wind_SPD_band','ID')

# ----------------------------------------------------------------------- #
# Output Plot-------------------------------------------------------------
# ----------------------------------------------------------------------- #
# Run for both gwcs error dist and wind effect error
plotdata <- plotdata_gwcs_error_dist
#plotdata <- plotdata_wind_effect_error
#plotdata <- plotdata_wind_effect_error_4_9

time_offset_plot <- 900
forecast_lookahead <- 510

for (i in (1:20)) {
  #prints flight ID for each loop, to see progress in console when running
   print(plotdata$ID[plotdata$ID==i])

  i <- 1
  
  #variables from plotdata table, for the subsequent plots
  errordate <- plotdata$FP_Date[plotdata$ID==i]
  time1 <- plotdata$Time_At_4DME[plotdata$ID==i]
  ID <- plotdata$ID[plotdata$ID==i]
  callsignvar <- plotdata$Callsign[plotdata$ID==i]
  dmeseg <- plotdata$Sep_Dist[plotdata$ID==i]
  errormag <- plotdata$ForecastGSPD_ObservedGSPD_Mode_S_Error[plotdata$ID==i]
  RW <- plotdata$Landing_Runway[plotdata$ID==i]
  RWG <- plotdata$RUnway_Group[plotdata$ID==i]
  errordist <- plotdata$Forecast_GWCS_Error_Distance[plotdata$ID==i]
   
  
  #RS commented out coz no sfcwind
  # sfcwind_selection<-subset(sfcwind, Anemo_Date==errordate & Anemo_Time<(time1+800) & Anemo_Time>(time1-800) & Landing_Runway==RW)
  # sfcwind_selection$Anemo_HDG<-sfcwind_selection$Anemo_HDG-180
  # sfcwind_selection<-within(sfcwind_selection,Anemo_HDG[Anemo_HDG>=180]<-Anemo_HDG-360)
  # -180->+180
  
  rawsegs_selection <- subset(rawsegs, FP_Date.x==errordate & Time_At_4DME.x<(time1+time_offset_plot) & Time_At_4DME.x>(time1-time_offset_plot)&DME_Seg %%2==0 & DME_Seg<=6 & Landing_Runway.x == RW)
  #rawsegs_selection$Ave_Wind_HDG<- rawsegs_selection$Ave_Wind_HDG-180
  #rawsegs_selection$Ave_Wind_HDG1<-rawsegs_selection$Ave_Wind_HDG
  ##rawsegs_selection<-within(rawsegs_selection,Ave_Wind_HDG[Ave_Wind_HDG>=180 & Ave_Wind_HDG<360]<-Ave_Wind_HDG-360)
  
  rawsegs_selection_by_flight <- subset(rawsegs, FP_Date.x==errordate & Time_At_4DME.x<(time1+time_offset_plot) & Time_At_4DME.x>(time1-time_offset_plot)&DME_Seg %%2==0 & DME_Seg<=10 & Landing_Runway.x == RW)
  #subset tables for graphs, based on variables above
  gwcsperformanceselection <- subset(gwcsperformance, Sep_Dist==dmeseg & FP_Date==errordate & Time_At_4DME<(time1+time_offset_plot) & Time_At_4DME>(time1-time_offset_plot) & Landing_Runway.x == RW)
  rawsegs_flight_selection <- subset(rawsegs, FP_Date.x==errordate & Callsign.x==callsignvar & DME_Seg %%2==0 & Landing_Runway.x == RW)
  performance_flight_selection <- subset(gwcsperformance, FP_Date==errordate & Callsign==callsignvar & Landing_Runway.x == RW)
  
  
  #raw segs selection by flight, same as raw segs selection, but smaller time range to pick up fewer aircraft
  rawsegs_selection$DME_Seg <- as.factor(rawsegs_selection$DME_Seg)
  rawsegs_selection$Callsign <- as.factor(rawsegs_selection$Callsign)
  rawsegs_selection$Global_Flag <- as.factor(rawsegs_selection$Global_Flag)
  
  rawsegs_selection_by_flight$DME_Seg <- as.factor(rawsegs_selection_by_flight$DME_Seg)
  rawsegs_selection_by_flight$Callsign <- as.factor(rawsegs_selection_by_flight$Callsign)
  rawsegs_selection_by_flight$Global_Flag <- as.factor(rawsegs_selection_by_flight$Global_Flag)

  rawsegs_flight_selection$DME_Seg_num <- as.numeric(rawsegs_flight_selection$DME_Seg)
  
  
  anem_selection <- subset(anem[!is.na(Surface_Wind_SPD)], FP_Date==errordate & Time_At_4DME<(time1+900) & Time_At_4DME>(time1-900)& Landing_Runway== RW)
  #creates line at 4DME time of flight with error being investigated
  timeline_y <- c(-360,360)
  timeline_x <- c(time1,time1)
  timeline <- data.frame(timeline_x,timeline_y)

  #+/- 10 min lines
  time_ten_timeline_y <- c(-360,360)
  time_ten_timeline_x <- c(time1-600,time1-600)
  time_ten_timeline <- data.frame(time_ten_timeline_x,time_ten_timeline_y)
  time__ten_timeline_y <- c(-360,360)
  time__ten_timeline_x <- c(time1+600,time1+600)
  time__ten_timeline <- data.frame(time__ten_timeline_x,time__ten_timeline_y)
  
  thresholdlineupper_y <- c(errthresh,errthresh)
  thresholdlineupper_x <- c(time1-900,time1+900)
  thresholdlineupper <- data.frame(thresholdlineupper_x,thresholdlineupper_y)
  thresholdlinelower_y <- c(-errthresh,-errthresh)
  thresholdlinelower_x <- c(time1-900,time1+900)
  thresholdlinelower <- data.frame(thresholdlinelower_x,thresholdlinelower_y)
  
  
  #Performance plot
  perfplot <- (ggplot()+
                 geom_point(data=gwcsperformanceselection, aes(x=Time_At_4DME,y=ForecastGSPD_ObservedGSPD_Mode_S_Error),col='blue')+
                 geom_line(data=thresholdlineupper,aes(x=thresholdlineupper_x,y=thresholdlineupper_y), col='red',linetype = "dashed")+
                 geom_line(data=thresholdlinelower,aes(x=thresholdlinelower_x,y=thresholdlinelower_y), col='red',linetype = "dashed")+
                 geom_line(data=timeline,aes(x=timeline_x,y=timeline_y), col='red')+
                 geom_line(data= time_ten_timeline,aes(x= time_ten_timeline_x,y=time_ten_timeline_y), col='red',linetype = "dashed")+
                 geom_line(data= time__ten_timeline,aes(x= time__ten_timeline_x,y=time__ten_timeline_y), col='red',linetype = "dashed")+
                 ggtitle(paste(callsignvar,"_",errordate, "_#",ID),paste(round(errormag,2),"kts error, ",round(errordist,2),"NM error _",dmeseg,"NM Forecast", sep=""))+
                 coord_cartesian(ylim=c(-40,40))+
                 theme( axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid")))

 #Speed plot 
  spdmax <- max(rawsegs_selection$Ave_Wind_SPD,na.rm=TRUE)
  spdmin <- min(rawsegs_selection$Ave_Wind_SPD,na.rm=TRUE)
  surface_spdmax <- max(anem_selection$Surface_Wind_SPD,na.rm=TRUE)
  surface_spdmin <- min(anem_selection$Surface_Wind_SPD,na.rm=TRUE)
  max_speed <- max(spdmax,surface_spdmax)
  min_speed <- min(spdmin,surface_spdmin)
  spdplot <- ggplot()+
    geom_line(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_SPD,group=DME_Seg,colour=DME_Seg))+
    geom_point(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_SPD,group=DME_Seg,colour=DME_Seg))+
    geom_line(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_SPD,colour='Surface Wind'))+
    geom_point(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_SPD,colour='Surface Wind'))+
    geom_line(data=timeline,aes(x=timeline_x,y=timeline_y), col='red')+
    geom_line(data= time_ten_timeline,aes(x= time_ten_timeline_x,y=time_ten_timeline_y), col='red',linetype = "dashed")+
    geom_line(data= time__ten_timeline,aes(x= time__ten_timeline_x,y=time__ten_timeline_y), col='red',linetype = "dashed")+
    labs(title = "SegmentData Wind Speed", x = "Time_At_4DME")+
    ggtitle("SegmentData Wind Speed")+
    coord_cartesian(ylim=c(min_speed-10,max_speed+10))+
    theme(axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))
  
 
  #HDG plot
  # hdgplot <- ggplot() +
  #   geom_line(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
  #   geom_point(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
  #   geom_line(data=timeline,aes(x=timeline_x,y=c(0,360)), col='red') +
  #   geom_line(data= time_ten_timeline,aes(x= time_ten_timeline_x,y=c(0,360)), col='red',linetype = "dashed") +
  #   geom_line(data= time__ten_timeline,aes(x= time__ten_timeline_x,y=c(0,360)), col='red',linetype = "dashed") +
  #   labs(title = "SegmentData Wind Heading", x = "Time_At_4DME")+
  #   #coord_cartesian(ylim=c(-180,180) )+
  #   #scale_y_continuous(breaks=seq(-210,210,30), labels=c(210,180,210,240,270,300,330,0,30,60,90,120,150,180,210)) + 
  #   scale_y_continuous(breaks=seq(0,360,30))+
  #   theme(legend.position="none")+
  #   theme(axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid"))
  hdgmax <- (ceiling(max(rawsegs_selection$Ave_Wind_HDG,na.rm=TRUE)/10)*10)
  hdgmin <- (floor(min(rawsegs_selection$Ave_Wind_HDG,na.rm=TRUE)/10)*10)
  surface_hdgmax <- (ceiling(max(anem_selection$Surface_Wind_HDG,na.rm=TRUE)/10)*10)
  surface_hdgmin <- (floor(min(anem_selection$Surface_Wind_HDG,na.rm=TRUE)/10)*10)
  max_hdg <- max(hdgmax,surface_hdgmax)
  min_hdg <- min(hdgmin,surface_hdgmin) 
  # Heading plot is split into 3 due to location of crossing with 0/360 point
  if (max_hdg - min_hdg < 270){
    hdgplot <- ggplot() +
      geom_line(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
      geom_point(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
      geom_line(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_HDG,colour='Surface Wind Heading'))+
      geom_point(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_HDG,colour='Surface Wind Heading'))+
      geom_line(data=timeline,aes(x=timeline_x,y=c(min_hdg,max_hdg)), col='red') +
      geom_line(data= time_ten_timeline,aes(x= time_ten_timeline_x,y=c(min_hdg,max_hdg)), col='red',linetype = "dashed") +
      geom_line(data= time__ten_timeline,aes(x= time__ten_timeline_x,y=c(min_hdg,max_hdg)), col='red',linetype = "dashed") +
      labs(title = "SegmentData Wind Heading", x = "Time_At_4DME")+
      #coord_cartesian(ylim=c(-180,180) )+
      #scale_y_continuous(breaks=seq(-210,210,30), labels=c(210,180,210,240,270,300,330,0,30,60,90,120,150,180,210)) + 
      scale_y_continuous(breaks=seq(min_hdg, max_hdg,20))+
      theme(legend.position="none")+
      theme(axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid"))
  } else {
    for (j in 1:nrow(rawsegs_selection)) {
      if (rawsegs_selection$Ave_Wind_HDG[j] > 180) {
        rawsegs_selection$Ave_Wind_HDG[j] = rawsegs_selection$Ave_Wind_HDG[j] - 360
      } else
        rawsegs_selection$Ave_Wind_HDG[j] = rawsegs_selection$Ave_Wind_HDG[j]
    }
    for (k in 1:nrow(anem_selection)) {
      if (anem_selection$Surface_Wind_HDG[k] > 180) {
        anem_selection$Surface_Wind_HDG[k] = anem_selection$Surface_Wind_HDG[k] - 360
      } else
        anem_selection$Surface_Wind_HDG[k] = anem_selection$Surface_Wind_HDG[k]
    }
    hdgmax <- ceiling(max(rawsegs_selection$Ave_Wind_HDG,na.rm=TRUE)/10)*10
    hdgmin <- floor(min(rawsegs_selection$Ave_Wind_HDG,na.rm=TRUE)/10)*10
    surface_hdgmax <- ceiling(max(anem_selection$Surface_Wind_HDG,na.rm=TRUE)/10)*10
    surface_hdgmin <- floor(min(anem_selection$Surface_Wind_HDG,na.rm=TRUE)/10)*10
    max_hdg <- max(hdgmax,surface_hdgmax)
    min_hdg <- min(hdgmin,surface_hdgmin)
    if (max_hdg < 0 & min_hdg < 0){
      hdgplot <- ggplot() +
        geom_line(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
        geom_point(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
        geom_line(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_HDG,colour='Surface Wind Heading'))+
        geom_point(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_HDG,colour='Surface Wind Heading'))+
        geom_line(data=timeline,aes(x=timeline_x,y=c(min_hdg,max_hdg)), col='red') +
        geom_line(data= time_ten_timeline,aes(x= time_ten_timeline_x,y=c(min_hdg,max_hdg)), col='red',linetype = "dashed") +
        geom_line(data= time__ten_timeline,aes(x= time__ten_timeline_x,y=c(min_hdg,max_hdg)), col='red',linetype = "dashed") +
        labs(title = "SegmentData Wind Heading", x = "Time_At_4DME")+
        coord_cartesian(ylim=c(min_hdg,max_hdg) )+
        scale_y_continuous(breaks=seq(min_hdg,max_hdg,20), labels = c(seq(min_hdg + 360, max_hdg + 360 , 20))) +
        theme(legend.position="none")+
        theme(axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid")) 
    } else {
      hdgplot <- ggplot() +
        geom_line(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
        geom_point(data=rawsegs_selection, aes(x = Time_At_4DME.x, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg)) +
        geom_line(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_HDG,colour='Surface Wind Heading'))+
        geom_point(data=anem_selection, aes(x = Time_At_4DME, y = Surface_Wind_HDG,colour='Surface Wind Heading'))+
        geom_line(data=timeline,aes(x=timeline_x,y=c(min_hdg,max_hdg)), col='red') +
        geom_line(data= time_ten_timeline,aes(x= time_ten_timeline_x,y=c(min_hdg,max_hdg)), col='red',linetype = "dashed") +
        geom_line(data= time__ten_timeline,aes(x= time__ten_timeline_x,y=c(min_hdg,max_hdg)), col='red',linetype = "dashed") +
        labs(title = "SegmentData Wind Heading", x = "Time_At_4DME")+
        coord_cartesian(ylim=c(min_hdg,max_hdg) )+
        scale_y_continuous(breaks=seq(min_hdg,max_hdg,10), labels = c(seq(min_hdg + 360, 350, 10), seq(0, max_hdg, 10))) +
        theme(legend.position="none")+
        theme(axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid"))  
    }
  }



  #Forecast_actul speed plot
  forecast_act_plot <- ggplot()+
    geom_line(data=gwcsperformanceselection, aes(x=Time_At_4DME,y=Forecast_Wind_Effect_IAS),col='blue')+
    geom_line(data=gwcsperformanceselection, aes(x=Time_At_4DME,y=Observed_Wind_Effect_IAS),col='red')+
    geom_line(data=timeline,aes(x=timeline_x,y=timeline_y), col='red')+
    geom_line(data= time_ten_timeline,aes(x= time_ten_timeline_x,y=time_ten_timeline_y), col='red',linetype = "dashed")+
    geom_line(data= time__ten_timeline,aes(x= time__ten_timeline_x,y=time__ten_timeline_y), col='red',linetype = "dashed")+
    ggtitle("Forecast/Observed Wind Effect","blue=forecast Wind Effect, red=observed Wind Effect")+
    coord_cartesian(ylim=c(-40,30))+
    theme( axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid"))
  
  
 #wind SPD per flight
  wind_spd_flight_plot <- ggplot() +
    geom_point(data=rawsegs_selection_by_flight,aes(x = DME_Seg, y = Ave_Wind_SPD,colour=Callsign))+
    geom_line(data=rawsegs_selection_by_flight,aes(x =DME_Seg, y = Ave_Wind_SPD,colour=Callsign,group=Callsign))+
    #geom_point(data=rawsegs_selection_by_flight,aes(x =DME_Seg, y = Ave_Wind_SPD,colour=Global_Flag,group=Callsign))+
    #Outlierflight
    geom_line(data=subset(rawsegs_selection_by_flight,Callsign==callsignvar),aes(x =DME_Seg, y = Ave_Wind_SPD,group=1),col="red",size=1,linetype="dashed")+
    geom_point(data=subset(rawsegs_selection_by_flight,Callsign==callsignvar),aes(x =DME_Seg, y = Ave_Wind_SPD,size=1, colour=Global_Flag))+
    scale_fill_discrete(breaks=c(rawsegs_selection_by_flight$callsign))+
    ggtitle("Aircraft Data Wind Speed")+
    theme( axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid"))
 
 
 ##Wind HDG per flight
  wind_hdg_flight_plot <- ggplot() +
    geom_point(data=rawsegs_selection_by_flight,aes(x = DME_Seg, y = Ave_Wind_HDG,colour=Callsign,group=Callsign))+
    geom_line(data=rawsegs_selection_by_flight,aes(x =DME_Seg, y = Ave_Wind_HDG,colour=Callsign,group=Callsign))+
    #Outlierflight
    geom_line(data=subset(rawsegs_selection_by_flight,Callsign==callsignvar),aes(x =DME_Seg, y = Ave_Wind_HDG, group=1),col="red",size=1,linetype="dashed")+
    geom_point(data=subset(rawsegs_selection_by_flight,Callsign==callsignvar),aes(x =DME_Seg, y = Ave_Wind_HDG,group=1,size=1,colour=Global_Flag))+
    #preceeding outlier flight
    scale_fill_discrete(breaks=c(gwcsperformanceselection$callsign))+
    coord_cartesian(ylim=c(-180,180) )+
    scale_y_continuous(breaks=seq(-210,210,30), labels=c(210,180,210,240,270,300,330,0,30,60,90,120,150,180,210)) + 
    ggtitle("Aircraft Data Wind Heading")+
    theme(legend.position="none")+
    theme( axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid"))
  


  ##IAS/GS
  GS_IAS_Plot <- ggplot() +
    geom_line(data=rawsegs_flight_selection,aes(x = DME_Seg_num, y = Ave_Track_SPD),col='red')+
    geom_line(data=rawsegs_flight_selection,aes(x =DME_Seg_num, y = Ave_Mode_S_IAS),col='dark green')+
    geom_point(data=rawsegs_flight_selection,aes(x = DME_Seg_num, y = Ave_Track_SPD),col='red')+
    geom_point(data=rawsegs_flight_selection,aes(x =DME_Seg_num, y = Ave_Mode_S_IAS),col='dark green')+
    #Outlierflight
    #  geom_line(data=subset(rawsegs_selection,Callsign==callsignvar),aes(x =DME_Seg, y = Ave_Wind_HDG, group=1),col="red",size=1,linetype="dashed")+
    #preceeding outlier flight
    #geom_line(data=subset(rawsegs_selection,Callsign=="IBE31PF"),aes(x =DME_Seg, y = Ave_Wind_HDG),col="red",size=1,linetype="dotted")+
    #scale_fill_discrete(breaks=c(gwcsperformanceselection$callsign))+
    ggtitle(callsignvar,paste(errordate," ",dmeseg," DME ",time1," sec _ red=Track speed, green=IAS"))+
    theme( axis.line = element_line(colour = "darkblue",size = 0.5, linetype = "solid"))

  combined_plot <- arrangeGrob(
      perfplot,
      forecast_act_plot,
      hdgplot,
      spdplot,
      nrow = 2,
      widths = c(5, 5)
    )
  
  #grid.arrange(combined_plot)
  
  ggsave(file.path(out_data, paste0(i, "_", callsignvar, ".png")), combined_plot, width=9.5, height=7, units = "in", dpi = 320)
  
}
  


  # ----------------------------------------------------------------------- #
  # OLD Script-------------------------------------------------------------
  # ----------------------------------------------------------------------- #



############### Data distribution plots #################
# 
# #outliers
# ggplot(gwcs_errors,aes(x=ForecastGSPD_ObservedGSPD_Mode_S_Error))+
# geom_histogram(aes(y=..count..),bandwith=30,col="blue")+
# geom_density(aes(y=..count..),col="red")
# 
# 
# ggplot(gwcs_errors,aes(x=abserror))+
#    geom_histogram(aes(y=..count..),bandwith=30,col="blue")+
#   geom_density(aes(y=..count..),col="red")
# 
# 
# 
# #all data
# 
# ggplot(gwcsperformance,aes(x=ForecastGSPD_ObservedGSPD_Mode_S_Error))+
#   geom_histogram(aes(y=..count..),bandwith=1,col="blue")+
#   geom_density(aes(y=..count..),col="red")

#+ rug(gwcsperformance$ForecastGSPD_ObservedGSPD_Mode_S_Error,ticksize=0.5,side=1,lwd=0.5)
  




#2DME Performance Data
# 
# Performancestats_D2<-gwcsperformance[gwcsperformance$Forecast_Seg_Max=='4',]
# Performancestats_D2<-Performancestats_D2[!(is.na(Performancestats_D2$ForecastGSPD_ObservedGSPD_Mode_S_Error) | Performancestats_D2$ForecastGSPD_ObservedGSPD_Mode_S_Error==""), ]
# 
# mean(Performancestats_D2$ForecastGSPD_ObservedGSPD_Mode_S_Error)
# 
# sum(Performancestats_D2$ForecastGSPD_ObservedGSPD_Mode_S_Error>10)
# 



#filtered data count

#filtereddata <- gwcsperformance[  gwcsperformance$Observed_Max_RTT >= gwcsperformance$Forecast_Seg_Max-2 & gwcsperformance$Observed_Flying_Time>= (gwcsperformance$Forecast_Seg_Max*20)-40 & abs(gwcsperformance$Observed_Ave_Mode_S_GSPD- gwcsperformance$Observed_Track_GSPD)<=10 & gwcsperformance$Forecast_Seg_Max=='2', ]



#gwcsperformance$Observed_Track_GSPD > 80 &
#!is.na(gwcsperformance$ForecastGSPD_ObservedGSPD_Mode_S_Error) &





# 
# 
# #table for investigating error f
# 
#  investigationtable<-data.frame(gwcs_errors_unique$FP_Date,gwcs_errors_unique$Callsign,gwcs_errors_unique$Forecast_GSPD_Observed_IAS_Error,gwcs_errors_unique$ForecastGSPD_ObservedGSPD_Mode_S_Error,gwcs_errors_unique$Observed_Track_GSPD,gwcs_errors_unique$Forecast_GSPD_Observed_IAS, gwcs_errors_unique$Observed_Ave_Mode_S_GSPD)
#  investigationtable$ID<-c(1:numrow)
#  investigationtable<- rapply(investigationtable,as.character,classes="factor",how="replace")
# 
# 
# #
# 
# 






############################################################################################
########################################  OLD PLOTS ########################################
############################################################################################

#######   Set Variables for plots   ########
 # errordate<-"25/04/2016"
#  time1<- 61369


#Subset tables for plots
#rawsegs_selection<- subset(rawsegs,FP_Date==errordate & Time_At_4DME<(time1+600) & Time_At_4DME>(time1-600)&DME_Seg %%2==0)
#gwcsperformanceselection<-subset(gwcsperformance,FP_Date==errordate & Time_At_4DME<(time1+600) & Time_At_4DME>(time1-1600))


# 
# 
# timeline_y<- c(-360,360)
# timeline_x<-c(time1,time1)
# timeline<-data.frame(timeline_x,timeline_y)
# #names(timeline)<-c(time,speed)


#################################  Plots  ##########################################
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# 
# 
# #Performance Data
# (  ggplot()
#   +geom_point(data=gwcsperformanceselection, aes(x=Time_At_4DME,y=Forecast_GSPD_Observed_IAS_Error))
#   
#   +geom_line(data=timeline,aes(x=timeline_x,y=timeline_y), col='red')
#   +ggtitle("GWCS Performance, Error")
#   +coord_cartesian(ylim=c(-20,20))
#   +theme( axis.line = element_line(colour = "darkblue", 
#                                   size = 0.5, linetype = "solid"))
# )
# 
# 
# #Heading
# (  ggplot()
#   +geom_line(data=rawsegs_selection, aes(x = Time_At_4DME, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg))
#   
#   +geom_point(data=rawsegs_selection, aes(x = Time_At_4DME, y = Ave_Wind_HDG,group=DME_Seg,colour=DME_Seg))
#   
#   
#   
#   +geom_line(data=timeline,aes(x=timeline_x,y=timeline_y), col='red')
#   +ggtitle("SegmentData Wind Heading")
#   +coord_cartesian(ylim=c(100,200))
#   +theme( axis.line = element_line(colour = "darkblue", 
#                                    size = 0.5, linetype = "solid"))
# )
# 
# 
# #Speed
# (  ggplot()
#   +geom_line(data=rawsegs_selection, aes(x = Time_At_4DME, y = Ave_Wind_SPD,group=DME_Seg,colour=DME_Seg))
#   
#   
#   +geom_point(data=rawsegs_selection, aes(x = Time_At_4DME, y = Ave_Wind_SPD,group=DME_Seg,colour=DME_Seg))
#   
#   +geom_line(data=timeline,aes(x=timeline_x,y=timeline_y), col='red')
#   +ggtitle("SegmentData Wind Speed")
#   +coord_cartesian(ylim=c(0,40
#                       ))
#   +theme( axis.line = element_line(colour = "darkblue", 
#                                    size = 0.5, linetype = "solid"))
# )
# 
# 


# 
# 
#
# 






#Word Doc 


# mydoc<-docx()
# mydoc<-addTitle(mydoc,"Test Document",level=1)
# mydoc<-addParagraph(mydoc,"test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph test paragraph ")
# writeDoc(mydoc,file="mydoc.docx")
# 


