# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.6 Generate ORD/WAD Prediction
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Uses tbl_ORD_GS_Profile, tbl_ORD_Aircraft_Profile, tbl_All_Pair_Reference_Data, tbl_ORD_Observation, 
#              tbl_WAD_Observation and potentially new table tbl_ORD_TBS_Distances. Using the GS table as a base,
#              this procedure 1) finds the leader time flown 2) finds the follower distance flown in leader time 
#              3) calculates compression = follower distance - leader distance and Mean L/F IAS/WE across flown sections
#              using trapezium rule.
#               
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# v0: Constructed unsorted working tmemplate. Calculates the same ORD_Compression, ORD_Mean_IAS, Forecast_Mean_Wind_Effects as
#     the database. Requires update to account for configurable Leader delivery time, and partial final leader section, similarly
#     to the follower method.
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Updates Required/Theorized
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# - !!!Are we using TBS distances or Observed Delivery Separation?
# - General tidyup. (Commenting, Variable names, Sectioning) - DONE
# - Take account for configurable delivery distance. Partial final leader section. - DONE
# - Take account for partial initial leader section (FAF). (Just in case Runway FAF is used) - DONE
# - Linking. Link to rest of procedures for seamless runflow.
# - Further modularise and place functions in ORD Functions.R, change to run functions
# - Create WAD version. (Leader: FAF - CC, Follower: FAF + ORD_Compression + ORD_Separation_Distance - Follower_WAD_End)
# - Populate Error Variables
# - Join on relevant variables from other tables
# - Get format of ORD/WAD Prediction
# - Save to SQL/Local
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Forecast Compression Function. Requires Leader (gstl) and Follower (gstf) Groundspeed Profiles 
# With Joined values for Compression Start and End.
Get_Forecast_Compression <- function(gstl, gstf){
  
  # ------------------------ #
  ### --- Leader
  # ------------------------ #
  
  #gstl <- GS_Leader_ORD
  #gstf <- GS_Follower_ORD
  
  # Get flags for all segments within compression range, and one for start and end
  gstl <- mutate(gstl, 
                 In_Range_Flag = ifelse(Start_Dist >= Compression_End & End_Dist < Compression_Start, 1, 0),
                 Compression_End_Flag = ifelse(In_Range_Flag == 1 & End_Dist <= Compression_End, 1, 0),
                 Compression_Start_Flag = ifelse(In_Range_Flag == 1 & Start_Dist >= Compression_Start, 1, 0))
  
  # Filter all segments not within the Compression range
  gstl <- filter(gstl, In_Range_Flag != 0)
  
  # Find the GSPD and IAS differences for adjustment
  gstl <- mutate(gstl, GSPD_Difference = Start_GS - End_GS)
  gstl <- mutate(gstl, IAS_Difference = Start_IAS - End_IAS)
  
  # Adjust Leader Start/End Section values: Compression End Section.
  gstl <- mutate(gstl, Distance_Ratio = (Compression_End - End_Dist)/(Start_Dist - End_Dist))
  gstl <- mutate(gstl, End_GS = ifelse(Compression_End_Flag == 1, End_GS + (GSPD_Difference * Distance_Ratio), End_GS))
  gstl <- mutate(gstl, End_IAS = ifelse(Compression_End_Flag == 1, End_IAS + (IAS_Difference * Distance_Ratio), End_IAS))
  gstl <- mutate(gstl, End_Dist = ifelse(Compression_End_Flag == 1, Compression_End, End_Dist))
  
  # Adjust Leader Start/End Section values: Compression Start Section.
  gstl <- mutate(gstl, Distance_Ratio = (Start_Dist - Compression_Start)/(Start_Dist - End_Dist))
  gstl <- mutate(gstl, Start_GS = ifelse(Compression_Start_Flag == 1, Start_GS - (GSPD_Difference * Distance_Ratio), Start_GS))
  gstl <- mutate(gstl, Start_IAS = ifelse(Compression_Start_Flag == 1, Start_IAS - (IAS_Difference * Distance_Ratio), Start_IAS))
  gstl <- mutate(gstl, Start_Dist = ifelse(Compression_Start_Flag == 1, Compression_Start, Start_Dist))
  
  # Calculate the Leader Section Flying Times
  gstl <- mutate(gstl, Section_Flying_Time = 2 * (Start_Dist - End_Dist)/(Start_GS + End_GS))
  
  # Calculate the Start and End Wind Effects
  gstl <- mutate(gstl, Start_WE = Start_GS - Start_IAS)
  gstl <- mutate(gstl, End_WE = End_GS - End_IAS)
  
  # Calculate the aggregate Mean Leader IAS/Wind Effect Trapezium rule calculations
  gstl <- mutate(gstl, Mean_Leader_IAS = (Start_IAS + End_IAS) * Section_Flying_Time / 2)
  gstl <- mutate(gstl, Forecast_Mean_Leader_Wind_Effect = (Start_WE + End_WE) * Section_Flying_Time / 2) 
  
  # Get the Leader Stats: Flying Time, Flying Disance, Mean IAS and Mean WE (Trapezium)
  Leader_Stats <- gstl %>% group_by(Landing_Pair_ID) %>% 
    summarise(Leader_Flying_Time = sum(Section_Flying_Time, na.rm=T),
              Leader_Flying_Distance = max(Compression_Start - Compression_End, na.rm = T),
              Mean_Leader_IAS = sum(Mean_Leader_IAS, na.rm = T),
              Forecast_Mean_Leader_Wind_Effect = sum(Forecast_Mean_Leader_Wind_Effect, na.rm = T)) %>% ungroup() %>%
    mutate(Mean_Leader_IAS = Mean_Leader_IAS / Leader_Flying_Time,
           Forecast_Mean_Leader_Wind_Effect = Forecast_Mean_Leader_Wind_Effect / Leader_Flying_Time)
  
  # Get the Leader Flying times specifically for the Follower
  Leader_Times <- select(Leader_Stats, Landing_Pair_ID, Leader_Flying_Time)
  
  # ------------------------ #
  ### --- Follower
  # ------------------------ #
  
  # Get flags for Follower being within travel range and it's end section
  gstf <- mutate(gstf, 
                 Viable_Flag = ifelse(Start_Dist >= Follower_End_Distance, 1, 0),
                 Start_Flag = ifelse(Viable_Flag == 1 & End_Dist <= Follower_End_Distance, 1, 0))
  
  # Filter for only Viable follower segments
  gstf <- filter(gstf, Viable_Flag == 1)
  
  # Find the Follower IAS/GSPD Differences for Partial Section Calculations.
  gstf <- mutate(gstf, GSPD_Difference = Start_GS - End_GS)
  gstf <- mutate(gstf, IAS_Difference = Start_IAS - End_IAS)
  
  # Adjust Follower end section values. 
  gstf <- mutate(gstf, Distance_Ratio = (Follower_End_Distance - End_Dist)/(Start_Dist - End_Dist))
  gstf <- mutate(gstf, End_GS = ifelse(Start_Flag == 1, End_GS + (GSPD_Difference * Distance_Ratio), End_GS))
  gstf <- mutate(gstf, End_IAS = ifelse(Start_Flag == 1, End_IAS + (IAS_Difference * Distance_Ratio), End_IAS))
  gstf <- mutate(gstf, End_Dist = ifelse(Start_Flag == 1, Follower_End_Distance, End_Dist))
  
  # Find the first pass of the Section Flying Times. The End section time will be updated later.
  gstf <- mutate(gstf, Section_Flying_Time = 2 * (Start_Dist - End_Dist)/(Start_GS + End_GS))
  
  # Join on the Leader Flying Times for each Landing Pair ID.
  gstf <- left_join(gstf, Leader_Times, by=c("Landing_Pair_ID"))
  
  # Get the Cumulative Flying Times for each Follower. For each segment and it's previous segment.
  gstf <- group_by(gstf, Landing_Pair_ID) %>% mutate(Cumulative_Time = cumsum(Section_Flying_Time)) %>% ungroup()
  gstf <- mutate(gstf, Prev_Cumulative_Time = Cumulative_Time - Section_Flying_Time)
  
  # Create flags for all Valid sections and the Follower start section
  gstf <- mutate(gstf, 
                 Section_Time_Flag = ifelse(Prev_Cumulative_Time <= Leader_Flying_Time, 1, 0),
                 Last_Section_Flag = ifelse(Cumulative_Time >= Leader_Flying_Time & Prev_Cumulative_Time < Leader_Flying_Time, 1, 0))
  
  # Filter only for valid Follower Sections
  gstf <- filter(gstf, Section_Time_Flag == 1)
  
  # Adjust Follower Start Section values. Use Time Ratio.
  gstf <- mutate(gstf, Time_Ratio = (Leader_Flying_Time - Prev_Cumulative_Time)/(Cumulative_Time - Prev_Cumulative_Time))
  gstf <- mutate(gstf, GSPD_Difference = Start_GS - End_GS)
  gstf <- mutate(gstf, IAS_Difference = Start_IAS - End_IAS)
  gstf <- mutate(gstf, Start_GS = ifelse(Last_Section_Flag == 1, End_GS + (GSPD_Difference * Time_Ratio), Start_GS))
  gstf <- mutate(gstf, Start_IAS = ifelse(Last_Section_Flag == 1, End_IAS + (IAS_Difference * Time_Ratio), Start_IAS))
  
  # Adjust Initial section Flying Time, start distance and Cumulative time 
  gstf <- mutate(gstf, Section_Flying_Time = ifelse(Last_Section_Flag == 1, (Leader_Flying_Time - Prev_Cumulative_Time), Section_Flying_Time))
  gstf <- mutate(gstf, Cumulative_Time = ifelse(Last_Section_Flag == 1, Leader_Flying_Time, Cumulative_Time))
  gstf <- mutate(gstf, Start_Dist = ifelse(Last_Section_Flag == 1, End_Dist + (0.5 * Section_Flying_Time * (Start_GS + End_GS)),  Start_Dist))
  
  # Calculate the Start/End Follower Wind Effects
  gstf <- mutate(gstf, Start_WE = Start_GS - Start_IAS)
  gstf <- mutate(gstf, End_WE = End_GS - End_IAS)
  
  # Calculate the aggregate Mean Follower IAS/Wind Effect Trapezium rule calculations
  gstf <- mutate(gstf, Mean_Follower_IAS = (Start_IAS + End_IAS) * Section_Flying_Time / 2)
  gstf <- mutate(gstf, Forecast_Mean_Follower_Wind_Effect = (Start_WE + End_WE) * Section_Flying_Time / 2)  
  
  # Calculate the Cumulative Follower Flying Distance
  gstf <- mutate(gstf, Cumulative_Follower_Distance = Start_Dist - Follower_End_Distance)
  
  # Get the Follower Statistics: Follower Flying Distance/Time, Mean IAS, Mean WE (Trapezium rule)
  Follower_Stats <- group_by(gstf, Landing_Pair_ID) %>% 
    summarise(Follower_Flying_Distance = max(Cumulative_Follower_Distance, na.rm = T),
              Follower_Flying_Time = max(Cumulative_Time, na.rm = T),
              Mean_Follower_IAS = sum(Mean_Follower_IAS, na.rm = T),
              Forecast_Mean_Follower_Wind_Effect = sum(Forecast_Mean_Follower_Wind_Effect, na.rm = T)) %>% ungroup() %>%
    mutate(Mean_Follower_IAS = Mean_Follower_IAS / Follower_Flying_Time,
           Forecast_Mean_Follower_Wind_Effect = Forecast_Mean_Follower_Wind_Effect / Follower_Flying_Time)
  
  # ------------------------ #
  ### --- Results & Output
  # ------------------------ #
  
  # Combine the Leader and Follower Stats
  All_Stats <- left_join(Leader_Stats, Follower_Stats, by = c("Landing_Pair_ID"))
  
  # Calculate Compression
  All_Stats <- mutate(All_Stats, Forecast_Compression = Follower_Flying_Distance - Leader_Flying_Distance)
  
  # Return results
  return(All_Stats)
  
}

# ----------------------------------------------- #
# X.X.X Use SQL Inputs
# ----------------------------------------------- #
# Debug Data
# ----------------------------------------------- #

if (Debug_Mode){
  
  GS_Query <- "SELECT * 
               FROM tbl_ORD_GS_Profile GS
               INNER JOIN tbl_Landing_Pair LP
               ON LP.Landing_Pair_ID = GS.Landing_Pair_ID"
               
  
  AP_Query <- "SELECT *      
               FROM tbl_ORD_Aircraft_Profile AP
               INNER JOIN tbl_Landing_Pair LP
               ON LP.Landing_Pair_ID = AP.Landing_Pair_ID"
  
  SD_Query <- "SELECT 
               OP.Landing_Pair_ID,
               OP.ORD_Separation_Distance
               FROM tbl_ORD_Prediction OP
               INNER JOIN tbl_Landing_Pair LP
               ON LP.Landing_Pair_ID = OP.Landing_Pair_ID"
  
  if (Processing_Period == "Day"){
    GS_Query <- paste0(GS_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
    AP_Query <- paste0(AP_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
    SD_Query <- paste0(SD_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
  }
  
  if (Processing_Period == "Month"){
    GS_Query <- paste0(GS_Query, " WHERE LP.Landing_Pair_Date LIKE '%", Processing_Month, "%'")
    AP_Query <- paste0(AP_Query, " WHERE LP.Landing_Pair_Date LIKE '%", Processing_Month, "%'")
    SP_Query <- paste0(SD_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
  }
               
  ORD_GS_Profile <- sqlQuery(con, GS_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                "Landing_Pair_Date", "Landing_Pair_Type"))
  ORD_Aircraft_Profile <- sqlQuery(con, AP_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                "Landing_Pair_Date", "Landing_Pair_Type"))
  
  Separation_Distances <- sqlQuery(con, SD_Query, stringsAsFactors = F)
  
}

# ----------------------------------------------- #
# Setup
# ----------------------------------------------- #

ORD_GS_Profile <- ORD_GS_Profile[order(ORD_GS_Profile$Landing_Pair_ID, ORD_GS_Profile$This_Pair_Role, ORD_GS_Profile$Section_Number),]

# Begin Compression setup for both ORD/WAD: Split GS Profile into L/F Datasets
ORD_GS_Profile_Leader <- filter(ORD_GS_Profile, This_Pair_Role == "L")
ORD_GS_Profile_Follower <- filter(ORD_GS_Profile, This_Pair_Role == "F")

# Acquire the Necessary Distance Parameters for ORD/WAD

# ORD Separation Distances. For now use Observed_0DME_Separation as per UTMA.
if (!Debug_Mode){
Separation_Distances <- select(Landing_Pair_Reference, Landing_Pair_ID, Observed_Delivery_Separation) %>%
  rename(ORD_Separation_Distance = Observed_Delivery_Separation)
}

# LSTs for ORD Start and WAD End
Local_Stabilisation_Distances <- filter(ORD_Aircraft_Profile, This_Pair_Role == "L") %>%
  select(Landing_Pair_ID, Local_Stabilisation_Distance)

if (WAD_Enabled){
# CCTs - These will need to be taken from Observation - We need either the CCT or the CC Radar Distance
Compression_Commencements_1 <- select(Landing_Pair_Reference, Landing_Pair_ID, Leader_CC_RTT)
Compression_Commencements_2 <- filter(ORD_Aircraft_Profile, This_Pair_Role == "L") %>%
  select(Landing_Pair_ID, Compression_Commencement_Threshold)
Compression_Commencements <- left_join(Compression_Commencements_1, Compression_Commencements_2, by = c("Landing_Pair_ID")) %>%
  mutate(Leader_WAD_Start_Distance = ifelse(Leader_CC_RTT > Compression_Commencement_Threshold, Compression_Commencement_Threshold, Leader_CC_RTT)) %>%
  select(Landing_Pair_ID, Leader_WAD_Start_Distance)

# Remove Other WAD Compression Start Data
rm(Compression_Commencements_1, Compression_Commencements_2)
}

# ----------------------------------------------- #
# ORD Config
# ----------------------------------------------- #

# ------------------------------------------------------------------------------------------------------------------------------------ #
### ORD Compression Parameters
# ------------------------------------------------------------------------------------------------------------------------------------ #

# Join on the Local Stabilisation Distances for ORD Compression Start (Leader Start Distance)
GS_Leader_ORD <- left_join(ORD_GS_Profile_Leader, Local_Stabilisation_Distances, by=c("Landing_Pair_ID")) %>%
  rename(Compression_Start = Local_Stabilisation_Distance)

# Add the Delivery Distance for ORD Compression End (Leader End Distance)
GS_Leader_ORD <- mutate(GS_Leader_ORD, Compression_End = New_Delivery)

# Join on the ORD Separation Distances and add to Delivery (Follower End Distance)
GS_Follower_ORD <- left_join(ORD_GS_Profile_Follower, Separation_Distances, by = c("Landing_Pair_ID")) %>%
  rename(Follower_End_Distance = ORD_Separation_Distance) %>% mutate(Follower_End_Distance = Follower_End_Distance + New_Delivery)

# Get the ORD Compression/Speed/WE Stats
ORD_Predicted_Values <- Get_Forecast_Compression(GS_Leader_ORD, GS_Follower_ORD)

# Select/Rename Variables Appropriately
ORD_Predicted_Values <- select(ORD_Predicted_Values,
                               Landing_Pair_ID,
                               Leader_Flying_Time,
                               Follower_Flying_Time,
                               Leader_Flying_Distance,
                               Follower_Flying_Distance,
                               ORD_Mean_Leader_IAS = Mean_Leader_IAS,
                               ORD_Mean_Follower_IAS = Mean_Follower_IAS,
                               Forecast_ORD_Mean_Leader_Wind_Effect = Forecast_Mean_Leader_Wind_Effect,
                               Forecast_ORD_Mean_Follower_Wind_Effect = Forecast_Mean_Follower_Wind_Effect,
                               ORD_Forecast_Compression = Forecast_Compression)

# Join Predicted ORD Data on to Landing Pair Reference Data
Landing_Pair_Reference <- left_join(Landing_Pair_Reference, ORD_Predicted_Values, by= c("Landing_Pair_ID"))

# Remove unneeded Data
rm(GS_Leader_ORD, GS_Follower_ORD, ORD_Predicted_Values)

# ------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------ #
### WAD Compression Parameters
# ------------------------------------------------------------------------------------------------------------------------------------ #

if (WAD_Enabled){
  
  # Get Local Stabilisation Distances for WAD (Leader End Distance) 
  GS_Leader_WAD <- left_join(GS_Profile_ORD_Leader, Local_Stabilisation_Distances, by = c("Landing_Pair_ID")) %>%
    rename(Compression_End = Local_Stabilisation_Distance)
  
  # Get Compression Commencements for WAD (Leader Start Distance)
  GS_Leader_WAD <- left_join(GS_Leader_WAD, Compression_Commencements, by = c("Landing_Pair_ID")) %>%
    rename(Compression_Start = Leader_WAD_Start_Distance)
  
  # Get necessary data for WAD End (Follower End Distance)
  ORD_Compressions <- select(Landing_Pair_Reference, Landing_Pair_ID, ORD_Forecast_Compression)
  GS_Follower_WAD <- left_join(GS_Profile_ORD_Follower, Local_Stabilisation_Distances, by = c("Landing_Pair_ID"))
  GS_Follower_WAD <- left_join(GS_Follower_WAD, ORD_Compressions, by = c("Landing_Pair_ID"))
  GS_Follower_WAD <- left_join(GS_Follower_WAD, Separation_Distances, by = c("Landing_Pair_ID"))
  
  # Get Assumed Follower DTT at WAD Compression End: ORD Compression + Local Stabilisation Distance + Separation Distance
  GS_Follower_WAD <- mutate(GS_Follower_WAD, Follower_End_Distance = ORD_Forecast_Compression + ORD_Separation_Distance + Local_Stabilisation_Distance)
  
  # Remove unecessary variables and data tables
  GS_Follower_WAD <- select(GS_Follower_WAD, -c("ORD_Forecast_Compression", "ORD_Separation_Distance", "Local_Stabilisation_Distance"))
  
  # Get the ORD Compression/Speed/WE Stats
  WAD_Predicted_Values <- Get_Forecast_Compression(GS_Leader_WAD, GS_Follower_WAD)
  
  # Select/Rename Variables Appropriately
  WAD_Predicted_Values <- select(WAD_Predicted_Values,
                                 Landing_Pair_ID,
                                 WAD_Mean_Leader_IAS = Mean_Leader_IAS,
                                 WAD_Mean_Follower_IAS = Mean_Follower_IAS,
                                 Forecast_WAD_Mean_Leader_Wind_Effect = Forecast_Mean_Leader_Wind_Effect,
                                 Forecast_WAD_Mean_Follower_Wind_Effect = Forecast_Mean_Follower_Wind_Effect,
                                 WAD_Forecast_Compression = Forecast_Compression)
  
  # Join Predicted WAD Data on to Landing Pair Reference Data
  Landing_Pair_Reference <- left_join(Landing_Pair_Reference, WAD_Predicted_Values, by= c("Landing_Pair_ID"))
  
  # Remove unnecessary data
  rm(GS_Leader_WAD, GS_Follower_WAD, WAD_Predicted_Values, ORD_Compressions)

}

# ------------------------------------------------------------------------------------------------------------------------------------ #

# ----------------------------------------------- #
# Tidying
# ----------------------------------------------- #

rm(ORD_GS_Profile_Follower, ORD_GS_Profile_Leader, Separation_Distances, Local_Stabilisation_Distances, Compression_Commencements)






# ----------------------------------------------- #
# Testing
# ----------------------------------------------- #

if (Test_Mode){
  
  # Setup Queries for ORD/WAD Prediction
  ORD_Prediction_SQL_Query <- "SELECT 
                                 OP.Landing_Pair_ID,
                                 OP.ORD_Compression AS SQL_ORD_Compression,
                                 OP.ORD_Mean_Leader_IAS AS SQL_ORD_Mean_Leader_IAS,
                                 OP.ORD_Mean_Follower_IAS AS SQL_ORD_Mean_Follower_IAS,
                                 OP.Forecast_Mean_Leader_Wind_Effect AS SQL_ORD_Forecast_Mean_Leader_Wind_Effect,
                                 OP.Forecast_Mean_Follower_Wind_Effect AS SQL_ORD_Forecast_Mean_Follower_Wind_Effect
                               FROM tbl_ORD_Prediction OP
                               INNER JOIN tbl_Landing_Pair LP
                               ON LP.Landing_Pair_ID = OP.Landing_Pair_ID"
                              
  
  WAD_Prediction_SQL_Query <- "SELECT 
                                 WP.Landing_Pair_ID
                                 WP.WAD_Compression AS SQL_WAD_Compression,
                                 WP.WAD_Mean_Leader_IAS AS SQL_WAD_Mean_Leader_IAS,
                                 WP.WAD_Mean_Follower_IAS AS SQL_WAD_Mean_Follower_IAS,
                                 WP.Forecast_Mean_Leader_Wind_Effect AS SQL_WAD_Forecast_Mean_Leader_Wind_Effect,
                                 WP.Forecast_Mean_Follower_Wind_Effect AS SQL_WAD_Forecast_Mean_Follower_Wind_Effect
                               FROM tbl_WAD_Prediction WP
                               INNER JOIN tbl_Landing_Pair LP
                               ON LP.Landing_Pair_ID = WP.Landing_Pair_ID"
  
  if (Processing_Period == "Day"){
    ORD_Prediction_SQL_Query <- paste0(ORD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
    WAD_Prediction_SQL_Query <- paste0(WAD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
  }
  
  if (Processing_Period == "Month"){
    ORD_Prediction_SQL_Query <- paste0(ORD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date LIKE '%", Processing_Month, "%'")
    WAD_Prediction_SQL_Query <- paste0(WAD_Prediction_SQL_Query, " WHERE LP.Landing_Pair_Date LIKE '%", Processing_Month, "%'")
  }
  
  # Load Required Test ORD PRediction Data
  ORD_Prediction_SQL <- sqlQuery(con, ORD_Prediction_SQL_Query, stringsAsFactors = F)
  
  # Select Relevant Fields from R Calculation
  ORD_Prediction_R <- select(Landing_Pair_Reference,
                             Landing_Pair_ID,
                             ORD_Mean_Leader_IAS,
                             ORD_Mean_Follower_IAS,
                             Forecast_ORD_Mean_Leader_Wind_Effect,
                             Forecast_ORD_Mean_Follower_Wind_Effect,
                             ORD_Forecast_Compression,
                             Leader_Flying_Time,
                             Follower_Flying_Time,
                             Leader_Flying_Distance,
                             Follower_Flying_Distance)

  # Join Sets Together
  ZCOMP_ORD_Prediction <- full_join(ORD_Prediction_SQL, ORD_Prediction_R, by = c("Landing_Pair_ID"))
  
  # Add Comparison Fields
  ZCOMP_ORD_Prediction <- mutate(ZCOMP_ORD_Prediction,
                                   DIFF_ORD_Mean_Leader_IAS = abs(ORD_Mean_Leader_IAS - SQL_ORD_Mean_Leader_IAS),
                                   DIFF_ORD_Mean_Follower_IAS = abs(ORD_Mean_Follower_IAS - SQL_ORD_Mean_Follower_IAS),
                                   DIFF_ORD_Mean_Leader_WE = abs(Forecast_ORD_Mean_Leader_Wind_Effect - SQL_ORD_Forecast_Mean_Leader_Wind_Effect),
                                   DIFF_ORD_Mean_Follower_WE = abs(Forecast_ORD_Mean_Follower_Wind_Effect - SQL_ORD_Forecast_Mean_Follower_Wind_Effect),
                                   DIFF_ORD_Compression = abs(ORD_Forecast_Compression - SQL_ORD_Compression),
                                   FLAG_ORD_Mean_Leader_IAS = ifelse(DIFF_ORD_Mean_Leader_IAS > 0.0001, 1, 0),
                                   FLAG_ORD_Mean_Follower_IAS = ifelse(DIFF_ORD_Mean_Follower_IAS > 0.0001, 1, 0),
                                   FLAG_ORD_Mean_Leader_WE = ifelse(DIFF_ORD_Mean_Leader_WE > 0.0001, 1, 0),
                                   FLAG_ORD_Mean_Follower_WE = ifelse(DIFF_ORD_Mean_Follower_WE > 0.0001, 1, 0),
                                   FLAG_ORD_Compression = ifelse(DIFF_ORD_Compression > 0.01, 1, 0))
  
  # Create Summary
  ZSTAT_ORD_Prediction <- summarise(ZCOMP_ORD_Prediction,
                                    CNT_Total = n(),
                                    CNT_ORD_Mean_Leader_IAS = sum(FLAG_ORD_Mean_Leader_IAS, na.rm = T),
                                    CNT_ORD_Mean_Follower_IAS = sum(FLAG_ORD_Mean_Follower_IAS, na.rm = T),
                                    CNT_ORD_Mean_Leader_WE = sum(FLAG_ORD_Mean_Leader_WE, na.rm = T),
                                    CNT_ORD_Mean_Follower_WE = sum(FLAG_ORD_Mean_Follower_WE, na.rm = T),
                                    CNT_ORD_Compression = sum(FLAG_ORD_Compression, na.rm = T))
  
  # Remove Intermediary Data
  rm(ORD_Prediction_SQL, ORD_Prediction_R)
  
  
  if (WAD_Enabled){
    
    # Load Required Test ORD PRediction Data
    WAD_Prediction_SQL <- sqlQuery(con, WAD_Prediction_SQL_Query, stringsAsFactors = F)
    
    # Select Relevant Fields from R Calculation
    WAD_Prediction_R <- select(Landing_Pair_Reference,
                               Landing_Pair_ID,
                               WAD_Mean_Leader_IAS,
                               WAD_Mean_Follower_IAS,
                               Forecast_WAD_Mean_Leader_Wind_Effect,
                               Forecast_WAD_Mean_Follower_Wind_Effect,
                               WAD_Forecast_Compression)
    
    # Join Sets Together
    ZCOMP_WAD_Prediction <- full_join(WAD_Prediction_SQL, WAD_Prediction_R, by = c("Landing_Pair_ID"))
    
    # Add Comparison Fields
    ZCOMP_WAD_Prediction <- mutate(ZCOMP_WAD_Prediction,
                                     DIFF_WAD_Mean_Leader_IAS = abs(WAD_Mean_Leader_IAS - SQL_WAD_Mean_Leader_IAS),
                                     DIFF_WAD_Mean_Follower_IAS = abs(WAD_Mean_Follower_IAS - SQL_WAD_Mean_Follower_IAS),
                                     DIFF_WAD_Mean_Leader_WE = abs(Forecast_WAD_Mean_Leader_Wind_Effect - SQL_WAD_Forecast_Mean_Leader_Wind_Effect),
                                     DIFF_WAD_Mean_Follower_WE = abs(Forecast_WAD_Mean_Follower_Wind_Effect - SQL_WAD_Forecast_Mean_Follower_Wind_Effect),
                                     DIFF_WAD_Compression = abs(WAD_Forecast_Compression - SQL_WAD_Compression),
                                     FLAG_WAD_Mean_Leader_IAS = ifelse(DIFF_WAD_Mean_Leader_IAS > 0.0001, 1, 0),
                                     FLAG_WAD_Mean_Follower_IAS = ifelse(DIFF_WAD_Mean_Follower_IAS > 0.0001, 1, 0),
                                     FLAG_WAD_Mean_Leader_WE = ifelse(DIFF_WAD_Mean_Leader_WE > 0.0001, 1, 0),
                                     FLAG_WAD_Mean_Follower_WE = ifelse(DIFF_WAD_Mean_Follower_WE > 0.0001, 1, 0),
                                     FLAG_WAD_Compression = ifelse(DIFF_WAD_Compression > 0.01, 1, 0))
    
    # Create Summary
    ZSTAT_WAD_Prediction <- summarise(ZCOMP_WAD_Prediction,
                                      CNT_Total = n(),
                                      CNT_WAD_Mean_Leader_IAS = sum(FLAG_WAD_Mean_Leader_IAS, na.rm = T),
                                      CNT_WAD_Mean_Follower_IAS = sum(FLAG_WAD_Mean_Follower_IAS, na.rm = T),
                                      CNT_WAD_Mean_Leader_WE = sum(FLAG_WAD_Mean_Leader_WE, na.rm = T),
                                      CNT_WAD_Mean_Follower_WE = sum(FLAG_WAD_Mean_Follower_WE, na.rm = T),
                                      CNT_WAD_Compression = sum(FLAG_WAD_Compression, na.rm = T))
    
    # Remove Intermdiary Data
    rm(WAD_Prediction_R, WAD_Prediction_SQL)
    
  }
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

  
  
  
  
  
  
  
  
  
  
  