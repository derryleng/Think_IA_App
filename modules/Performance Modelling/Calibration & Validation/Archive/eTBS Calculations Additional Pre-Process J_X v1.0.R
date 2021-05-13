########################################################################
## Created: 21/11/2017                                                    
## Project: eTBS                                                          
## By: MC                                                                 
## Purpose: To enhance the sample size for Small Followers                  
## Version
##
## 1.0  Initial Version to extract the J-X aircraft available for H, U and M
##
########################################################################

rm(list = ls())

########################################################################
## Libraries
########################################################################

library(RODBC) 
library(ggplot2)
library(lattice)
library(plyr)
library(dplyr)

##########################################################################
## Controls
##########################################################################

version_input <- "v2.3"
version <- "v1.0"
save_plots = TRUE
user <- "Michael Cowham"


#######################################################################
## Databases
#######################################################################


dbhandle <- odbcDriverConnect('driver={SQL Server};server=DESKTOP-6UVAQ2R\\SQLEXPRESS;database=eTBS_Reference_Data;trusted_connection=true')

# Grab the tracks data set

profiles <- sqlQuery(dbhandle, "SELECT * FROM calcReferenceData")



#######################################################################
## Data Files
########################################################################

actype <- read.csv(file = paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\P33.C.11.2016 (eTBS)\\5. Validation\\Reference Data\\UK and RECAT WTC Lookup v1.1.csv", sep = ""), head = TRUE, sep = ",") 

# The WTC Scheme Lookup

scheme <- read.csv(file = paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\P33.C.11.2016 (eTBS)\\5. Validation\\Reference Data\\UK and RECAT WT Scheme Lookup v1.2.csv", sep = ""), head = TRUE, sep = ",") 

#data1 <- read.csv(file = paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\eTBS Safety Analysis\\eTBS JEP\\eTBS Performance Model\\Input\\", version_input, "\\eTBS_Performance_Model.csv", sep = ""), head = TRUE, sep = ",")


#######################################################################
## Look up Wake Turbulence Categories
########################################################################

# List of RECAT Types

recatlist <- c('J', 'H', 'U', 'M', 'S', 'L')

# Wake Turbulence Categories

profiles$Follow_UK <- actype$NATS.6CAT.WTC[match(profiles$Follower_Aircraft_Type, actype$NAS.AC.TYPE)]

profiles$Follow_RECAT <- actype$RECAT.EU.WTC[match(profiles$Follower_Aircraft_Type, actype$NAS.AC.TYPE)]

########################################################################
## Select only the Small Follower Aircraft
########################################################################

profiles <- filter(profiles, Follow_RECAT %in% c('HEAVY','UPPER', 'MEDIUM'))

########################################################################
# Set up the database connection for the Anemometer Data
########################################################################

#dbhandle <- odbcDriverConnect('driver={SQL Server};server=DESKTOP-6UVAQ2R\\SQLEXPRESS;database=eTBS_Reference_Data;trusted_connection=true')

# Grab the tracks data set

#anem <- sqlQuery(dbhandle, "SELECT * FROM inptAnemometer")


########################################################################
# Load in the GWCS data sets.  For S Followers this only needs to be
# 4, 5 and 6Nm
########################################################################

for (sepdist in 3:5){
  gwcs_file <-  paste("C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\P33.C.11.2016 (eTBS)\\5. Validation\\Validation_DB_Output\\GWCS_Validation_13_11_17\\GWCS_Forecasts_May15_Apr16_0_", sepdist, "nm.csv", sep = "")
  gwcs_dist <- read.csv(file = gwcs_file, head = TRUE, sep = ",", na.strings = c("NA", "NULL") )
  
  if (sepdist == 3){
    gwcs_data <- gwcs_dist
  } else {
    gwcs_data <- rbind(gwcs_data, gwcs_dist)
  }
  rm(gwcs_dist)
}

names(gwcs_data)[1] <- "FP_Date"

# Set the separation distance

gwcs_data$Sep_Dist <- gwcs_data$Forecast_Seg_Max - gwcs_data$Forecast_Seg_Min + 1

# Set the QC Flag

gwcs_data$qc_flag <- ifelse(gwcs_data$Landing_Runway %in% c('R27L','R09L','R27R','R09R') & gwcs_data$Observed_Max_RTT >= (gwcs_data$Forecast_Seg_Min + gwcs_data$Sep_Dist - 2) & gwcs_data$Observed_Flying_Time >= (gwcs_data$Sep_Dist * 20 - 40) & abs(gwcs_data$Observed_Ave_Mode_S_GSPD - gwcs_data$Observed_Track_GSPD) <= 20, 1, 0)

# Extract the relevant fields from the GWCS data.  Date, Callsign, Sep Distance, GWCS Forecast Value, Anemo Speed and Anemo Heading 

gwcs_merge <- select(gwcs_data, FP_Date, Time_At_4DME, Callsign, Forecast_Wind_Effect_IAS, Sep_Dist)

gwcs_merge <- plyr::rename(gwcs_merge, c("Callsign" = "Follower_Callsign"))


########################################################################
# Merge the GWCS Data and the Profile Data
########################################################################

profiles$DateChr <- as.character(profiles$DateInt)
profiles <- mutate(profiles, FP_Date = paste(substr(DateChr, 7,8), substr(DateChr,5,6), substr(DateChr, 1, 4), sep = "/"))

profiles_merged <- merge(profiles, gwcs_merge, by = c("FP_Date", "Follower_Callsign") )

# Just check to remove any matches where the difference in 4DME times is
# very large (callsigns landing twice in a day)

profiles_merged <- filter(profiles_merged, abs(profiles_merged$Leader_4DME_Time - profiles_merged$Time_At_4DME) < 3600 )

profiles_merged <- select(profiles_merged, FP_Date, Follower_Callsign, Leader_4DME_Time, Follower_Aircraft_Type, Follower_Threshold_Surface_Headwind:Follower_Threshold_Surface_Wind_Heading, Landing_Runway, Follow_UK, Follow_RECAT, Time_At_4DME, Forecast_Wind_Effect_IAS, Sep_Dist, Follower_0DME_RTT)

########################################################################
# Calculate the new eTBS Separation Distance
########################################################################

# Set the Leader RECAT Type to SUPER

profiles_merged$Leader_Recat_Wake_Cat <- "SUPER"

# Find the Leader Type Corresponding to the RECAT Wake Separation Distance

scheme_merge <- select(scheme, -UK_WT_Sep, -UK_WT_Sep_Time)
scheme_merge <- plyr::rename(scheme_merge, c("Lead_WVC" = "Leader_Recat_Wake_Cat", "Follow_WVC" = "Follow_RECAT"))

profiles_merged <- merge(profiles_merged, scheme_merge, on=c("Leader_Recat_Wake_Cat", "Follow_RECAT"))

scheme_merge <- select(scheme, Lead_WVC, Follow_WVC, UK_WT_Sep, UK_WT_Sep_Time)
scheme_merge <- plyr::rename(scheme_merge, c("Lead_WVC" = "Leader_Recat_Wake_Cat", "Follow_WVC" = "Follow_UK"))

profiles_merged <- merge(profiles_merged, scheme_merge, on = c(Leader_Recat_Wake_Cat, Follow_UK))

# Calculate the eTBS Separation Distances and create NA values for fields not being populated

profiles_merged <- mutate(profiles_merged, 
                        Recat_eTBS_0DME_Wake_Separation_Distance = RECAT_WT_Sep_Time * (RECAT_WT_Sep_Spd + Forecast_Wind_Effect_IAS) / 3600,	
                        Recat_eTBS_0DME_ROT_Spacing_Distance = RECAT_ROT_Sep_Time * (RECAT_ROT_Sep_Spd + Forecast_Wind_Effect_IAS) / 3600,
                        Recat_eTBS_0DME_All_Separation_Distance = RECAT_All_Sep_Time * (RECAT_All_Sep_Spd + Forecast_Wind_Effect_IAS) / 3600,
                        Follower_Forecast_TBS_Wind_Effect	 = NA,
                        Forecast_ORD_TBS_Compression	= NA,
                        Forecast_ORD_eTBS_Compression	= NA,
                        Recat_eTBS_4DME_Wake_Separation_Distance	= NA,
                        Recat_eTBS_4DME_ROT_Spacing_Distance	= NA,
                        Recat_eTBS_4DME_All_Separation_Distance	= NA,
                        UK6Cat_TBS_4DME_Wake_Separation_Distance = NA,
                        Leader_0DME_RTT	= NA,
                        Observed_0DME_Separation_Distance	= NA, 
                        Observed_1DME_Separation_Distance	= NA,
                        Observed_4DME_Separation_Distance	= NA,
                        Observed_4DME_Separation_Accuracy	= NA,
                        Observed_0DME_Separation_Time	= NA,
                        Observed_1DME_Separation_Time	= NA,
                        Observed_4DME_Separation_Time	= NA,
                        Observed_Follower_eTBS_IAS	= NA,
                        Observed_Follower_TBS_Wind_Effect	= NA,
                        Observed_Follower_eTBS_Wind_Effect	= NA,
                        Observed_Compression	= NA,
                        Landing_Pair_ID = NA,
                        Leader_Callsign = 'NOCALLSIGN',
                        Leader_Aircraft_Type = 'NOTYPE',
                        Leader_UK_Wake_Cat = Leader_Recat_Wake_Cat,
                        Follower_UK_Wake_Cat = Follow_UK)

# Rename the relevant variables

profiles_merged <- plyr::rename(profiles_merged, 
                                c('Follower_Threshold_Surface_Headwind'='Observed_AGI_Surface_Headwind',
                                  'Follower_Threshold_Surface_Wind_Speed'='Observed_AGI_Surface_Wind_SPD',
                                  'Follower_Threshold_Surface_Wind_Heading'='Observed_AGI_Surface_Wind_HDG',
                                  'Forecast_Wind_Effect_IAS'='Follower_Forecast_eTBS_Wind_Effect',
  #                                'Lead_RECAT'='Leader_Recat_Wake_Cat',
                                  'Follow_RECAT' = 'Follower_Recat_Wake_Cat',
                                 'UK_WT_Sep'='UK6Cat_Separation_Distance',
                                  'RECAT_WT_Sep' = 'Ref_Recat_Wake_Separation_Distance',
                                  'RECAT_ROT_Sep'='Ref_ROT_Spacing_Distance',
                                  'UK_WT_Sep_Time'='UK6Cat_Separation_Time',
                                  'RECAT_WT_Sep_Time'='Ref_Recat_Wake_Separation_Time',
                                  'RECAT_ROT_Sep_Time'='Ref_ROT_Spacing_Time',
                                  'RECAT_WT_Sep_Spd'='Follower_Ass_Recat_Separation_IAS',
                                  'RECAT_ROT_Sep_Spd'='Follower_Ass_ROT_Spacing_IAS',
                                  'RECAT_All_Sep_Spd'='Follower_Ass_IAS'))

# Re-order and select the variables                        

profiles_merged <- select(profiles_merged, Landing_Pair_ID,FP_Date,Leader_Callsign,Leader_Aircraft_Type,Leader_UK_Wake_Cat,Leader_Recat_Wake_Cat,Follower_Callsign,Follower_Aircraft_Type,Follower_UK_Wake_Cat,Follower_Recat_Wake_Cat,UK6Cat_Separation_Distance,Ref_Recat_Wake_Separation_Distance,Ref_ROT_Spacing_Distance,UK6Cat_Separation_Time,Ref_Recat_Wake_Separation_Time,Ref_ROT_Spacing_Time,Follower_Ass_Recat_Separation_IAS,Follower_Ass_ROT_Spacing_IAS,Follower_Ass_IAS,Follower_Forecast_TBS_Wind_Effect,Follower_Forecast_eTBS_Wind_Effect,Observed_AGI_Surface_Headwind,Observed_AGI_Surface_Wind_SPD,Observed_AGI_Surface_Wind_HDG,UK6Cat_TBS_4DME_Wake_Separation_Distance,Recat_eTBS_0DME_Wake_Separation_Distance,Recat_eTBS_0DME_ROT_Spacing_Distance,Recat_eTBS_0DME_All_Separation_Distance,Forecast_ORD_TBS_Compression,Forecast_ORD_eTBS_Compression,Recat_eTBS_4DME_Wake_Separation_Distance,Recat_eTBS_4DME_ROT_Spacing_Distance,Recat_eTBS_4DME_All_Separation_Distance,Leader_0DME_RTT,Observed_0DME_Separation_Distance,Observed_1DME_Separation_Distance,Observed_4DME_Separation_Distance,Observed_4DME_Separation_Accuracy,Leader_4DME_Time,Observed_0DME_Separation_Time,Observed_1DME_Separation_Time,Observed_4DME_Separation_Time,Observed_Follower_eTBS_IAS,Observed_Follower_TBS_Wind_Effect,Observed_Follower_eTBS_Wind_Effect,Observed_Compression)

# Output the data

# Select the <3kt Surface Wind Speed for now

#profiles_merged <- filter(profiles_merged, Observed_AGI_Surface_Wind_SPD <= 3)

write.csv(profiles_merged, file = paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\eTBS Safety Analysis\\eTBS JEP\\eTBS Performance Model\\Input\\", version_input, "\\eTBS_Performance_Model_Additional_J_X.csv", sep = ""), row.names=FALSE)
