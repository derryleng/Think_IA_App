# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.2 Generate ORD Aircraft Profile
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: This script generates the ORD Aircraft Profile table and uploads to SQL, as well as saving a local 
#              copy for further use in ORD. This table contains all the necessary adaptation/derived parameters to build 
#              the IAS/GSPD profiles. It begins by joining on relevant ORD adaptation. This is dependent on the ORD_Profile_Type
#              in tbl_Adaptation_Data. If set to Aircraft_Type, all aircraft with ORD calibrated aircraft types 
#              (in tbl_ORD_Aircraft_Adaptation) are matched, and the rest default to wake parameters (tbl_ORD_Wake_Adaptation).
#              The reference wake distances (and MRS if none such apply) are then matched such that if ORD_Profile_Type is set to
#              TBS_Table then we can use these as reference for joining on the DBS adaptation (tbl_ORD_DBS_Adaptation). These parameters
#              are used to calculate the Landing_Stabilisation_Speed, Final_Deceleration_Distance and Start_Initial_Deceleration_Distance
#              and all of these fields are combined to form the end table.
#
#
# Assumptions: The following data is assumed to be loaded: tbl_Adaptation_Data (Adaptation), 
#              tbl_All_Pair_Reference_Data (Landing_Pair_Reference), 
#              
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# 
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Filter Copy of Landing Pair Reference

# ----------------------------------------------- #
# 5.2.2.1a Prepare relevant adaptation
# ----------------------------------------------- #
# Prepare Leader/Follower versions of all ORD
# adaptation types, and get the Config params.
# !!NOTE: Need to change uniformity in ORD
# adaptation parameter names.
# ----------------------------------------------- #

# --- Build Leader/Follower versions of ORD Wake/Aircraft/DBS adaptation to fit tbl_ORD_Aircraft_Profile structure.

# ORD Aircraft Adaptation: Leader
ORD_Aircraft_Leader <- select(ORD_Aircraft, 
                              Aircraft_Type,
                              Compression_Commencement_Threshold,
                              Landing_Stabilisation_Speed_Type = Landing_Stabilisation_Speed_Type_Lead,
                              VRef = Min_Safe_Landing_Speed_Lead,
                              Apply_Gusting = Apply_Gusting_Lead,
                              Local_Stabilisation_Distance = Local_Stabilisation_Distance_Lead,
                              Steady_Procedural_Speed = Steady_Procedural_Speed_Lead,
                              Final_Deceleration = Final_Deceleration_Lead,
                              End_Initial_Deceleration_Distance = End_Initial_Deceleration_Distance_Lead,
                              Initial_Procedural_Speed = Initial_Procedural_Speed_Lead,
                              Initial_Deceleration = Initial_deceleration_Lead)

# ORD Aircraft Adaptation: Follower
ORD_Aircraft_Follower <- select(ORD_Aircraft, 
                                Aircraft_Type,
                                Compression_Commencement_Threshold,
                                Landing_Stabilisation_Speed_Type = Landing_Stabilisation_Speed_Type_Follower,
                                VRef = Min_Safe_Landing_Speed_Follower,
                                Apply_Gusting = Apply_Gusting_Follower,
                                Local_Stabilisation_Distance = Local_Stabilisation_Distance_Follower,
                                Steady_Procedural_Speed = Steady_Procedural_Speed_Follower,
                                Final_Deceleration = Final_Deceleration_Follower,
                                End_Initial_Deceleration_Distance = End_Initial_Deceleration_Distance_Follower,
                                Initial_Procedural_Speed = Initial_Procedural_Speed_Follower,
                                Initial_Deceleration = Initial_deceleration_follower)

# ORD Wake Adaptation: Leader
ORD_Wake_Leader <- select(ORD_Wake, 
                          Wake_Cat,
                          Compression_Commencement_Threshold,
                          Landing_Stabilisation_Speed_Type = Landing_Stabilisation_Speed_Type_Lead,
                          VRef = Min_Safe_Landing_Speed_Lead,
                          Apply_Gusting = Apply_Gusting_Lead,
                          Local_Stabilisation_Distance = Local_Stabilisation_Distance_Lead,
                          Steady_Procedural_Speed = Steady_Procedural_Speed_Lead,
                          Final_Deceleration = Final_Deceleration_Lead,
                          End_Initial_Deceleration_Distance = End_Initial_Deceleration_Distance_Lead,
                          Initial_Procedural_Speed = Initial_Procedural_Speed_Lead,
                          Initial_Deceleration = Initial_deceleration_Lead)

# ORD Wake Adaptation: Follower
ORD_Wake_Follower <- select(ORD_Wake, 
                            Wake_Cat,
                            Compression_Commencement_Threshold,
                            Landing_Stabilisation_Speed_Type = Landing_Stabilisation_Speed_Type_Follower,
                            VRef = Min_Safe_Landing_Speed_Follower,
                            Apply_Gusting = Apply_Gusting_Follower,
                            Local_Stabilisation_Distance = Local_Stabilisation_Distance_Follower,
                            Steady_Procedural_Speed = Steady_Procedural_Speed_Follower,
                            Final_Deceleration = Final_Deceleration_Follower,
                            End_Initial_Deceleration_Distance = End_Initial_Deceleration_Distance_Follower,
                            Initial_Procedural_Speed = Initial_Procedural_Speed_Follower,
                            Initial_Deceleration = Initial_deceleration_Follower)

# ORD DBS Adaptation: Leader
ORD_DBS_Leader <- select(ORD_DBS, 
                         DBS_Distance,
                         Compression_Commencement_Threshold,
                         Landing_Stabilisation_Speed_Type = Landing_Stabilisation_Speed_Type_Lead,
                         VRef = Min_Safe_Landing_Speed_Lead,
                         Apply_Gusting = Apply_Gusting_Lead,
                         Local_Stabilisation_Distance = Local_Stabilisation_Distance_Lead,
                         Steady_Procedural_Speed = Steady_Procedural_Speed_Lead,
                         Final_Deceleration = Final_Deceleration_Lead,
                         End_Initial_Deceleration_Distance = End_Initial_Deceleration_Distance_Lead,
                         Initial_Procedural_Speed = Initial_Procedural_Speed_Lead,
                         Initial_Deceleration = Initial_Deceleration_Lead)

# ORD DBS Adaptation: Follower
ORD_DBS_Follower <- select(ORD_DBS, 
                           DBS_Distance,
                           Compression_Commencement_Threshold,
                           Landing_Stabilisation_Speed_Type = Landing_Stabilisation_Speed_Type_Follower,
                           VRef = Min_Safe_Landing_Speed_Follower,
                           Apply_Gusting = Apply_Gusting_Follower,
                           Local_Stabilisation_Distance = Local_Stabilisation_Distance_Follower,
                           Steady_Procedural_Speed = Steady_Procedural_Speed_Follower,
                           Final_Deceleration = Final_Deceleration_Follower,
                           End_Initial_Deceleration_Distance = End_Initial_Deceleration_Distance_Follower,
                           Initial_Procedural_Speed = Initial_Procedural_Speed_Follower,
                           Initial_Deceleration = Initial_Deceleration_Follower)


# ----------------------------------------------- #
# 5.2.2.1b Prepare/Filter Reference Pair Data
# ----------------------------------------------- #
# Filter LP data based on loaded config.
# Then separate into Leader and Follower LP data.
# !!NOTE: This is based on the Reference_Pair_Data 
# table, which has not been constructed/edited yet!
# ----------------------------------------------- #

# -- Apply config filters.

# Remove Go-Arounds.
Landing_Pair_Reference <- filter(Landing_Pair_Reference, Go_Around_Flag == 0)

# The Original Landing Pair Reference: For Table/PM 
Landing_Pair_Reference_Full <- Landing_Pair_Reference

# Config Filters for Performance Model Stage (in trail only, wake pairs only)
Landing_Pair_Reference_PM <- Landing_Pair_Reference_Full
if (In_Trail_Only){Landing_Pair_Reference_PM <- filter(Landing_Pair_Reference_PM, Landing_Pair_Type != "Not_In_Trail")}
if (Wake_Pairs_Only){Landing_Pair_Reference_PM <- filter(Landing_Pair_Reference_PM, !is.na(Reference_Wake_Separation_Distance))}

# Filter for no Observed Compression to reduce sample size
Landing_Pair_Reference <- filter(Landing_Pair_Reference_PM, !is.na(Observed_ORD_Compression))

# -- Split Landing Pair data into Leader and Follower and rename columns for binding.

# Select Landing Pair Data: Leader
Landing_Pair_Leader <- select(Landing_Pair_Reference, Landing_Pair_ID, Leader_Flight_Plan_ID, Leader_Aircraft_Type, 
                              Leader_Recat_Wake_Cat, DBS_All_Sep_Distance, Leader_Landing_Runway, Forecast_AGI_Surface_Headwind)

# Rename Landing Pair Data: Leader
Landing_Pair_Leader <- rename(Landing_Pair_Leader, Flight_Plan_ID = Leader_Flight_Plan_ID, 
                              Aircraft_Type = Leader_Aircraft_Type, Wake_Cat = Leader_Recat_Wake_Cat,
                              Surface_Headwind = Forecast_AGI_Surface_Headwind)

# Select Landing Pair Data: Follower
Landing_Pair_Follower <- select(Landing_Pair_Reference, Landing_Pair_ID, Follower_Flight_Plan_ID, Follower_Aircraft_Type,
                                Follower_Recat_Wake_Cat, DBS_All_Sep_Distance, Follower_Landing_Runway, Forecast_AGI_Surface_Headwind)

# Rename Landing Pair Data: Follower
Landing_Pair_Follower <- rename(Landing_Pair_Follower, Flight_Plan_ID = Follower_Flight_Plan_ID, 
                                Aircraft_Type = Follower_Aircraft_Type, Wake_Cat = Follower_Recat_Wake_Cat,
                                Surface_Headwind = Forecast_AGI_Surface_Headwind)

# Add on the Pair Roles to each Landing Pair Table
Landing_Pair_Leader <- mutate(Landing_Pair_Leader, This_Pair_Role = "L")
Landing_Pair_Follower <- mutate(Landing_Pair_Follower, This_Pair_Role = "F")

# ----------------------------------------------- #
# 5.2.2.1c Join on the Adaptation
# ----------------------------------------------- #
# Uses Join_ORD_Adaptation from ORD Functions.R
# This joins on the relevant adaptation based on
# ORD_Profile_Selection and Aircraft Type.
# Commence join on Leader and Followers separately.
# ----------------------------------------------- #

# Select Reduced ORD Runway Adaptation Data (For Thousand_Ft_Gate and Gust_Adjustment)
ORD_Runway_Reduced <- select(ORD_Runway, Runway_Name, Thousand_Ft_Gate, Gust_Adjustment)

# Join on Relevant ORD Adaptation: Leader
Landing_Pair_Leader <- Join_ORD_Adaptation(Landing_Pair_Leader, ORD_Profile_Selection, 
                                           ORD_Aircraft_Leader, ORD_Wake_Leader, ORD_DBS_Leader)

# Join on Relevant ORD Adaptation: Follower
Landing_Pair_Follower <- Join_ORD_Adaptation(Landing_Pair_Follower, ORD_Profile_Selection, 
                                           ORD_Aircraft_Follower, ORD_Wake_Follower, ORD_DBS_Follower)

# Join on Reduced ORD Runway Adaptation: Leader
Landing_Pair_Leader <- left_join(Landing_Pair_Leader, ORD_Runway_Reduced, by=c("Leader_Landing_Runway"="Runway_Name"))
Landing_Pair_Leader <- rename(Landing_Pair_Leader, Landing_Runway = Leader_Landing_Runway)

# Join on Reduced ORD Runway Adaptation: Follower
Landing_Pair_Follower <- left_join(Landing_Pair_Follower, ORD_Runway_Reduced, by=c("Follower_Landing_Runway"="Runway_Name"))
Landing_Pair_Follower <- rename(Landing_Pair_Follower, Landing_Runway = Follower_Landing_Runway)

# Update Gust_Adjustment based on Apply_Gusting: Set former to 0 if latter is 0
Landing_Pair_Leader <- mutate(Landing_Pair_Leader, Gust_Adjustment = ifelse(Apply_Gusting == 1, Gust_Adjustment, 0))
Landing_Pair_Follower <- mutate(Landing_Pair_Follower, Gust_Adjustment = ifelse(Apply_Gusting == 1, Gust_Adjustment, 0))


# ----------------------------------------------- #
# 5.2.2.2 Calculate Landing Stabilisation Speed
# ----------------------------------------------- #
# Uses Calculate_Landing_Stabilisation_Speed function from 
# ORD Functions.R. Requires Forecast Surface Headwind.
# LP data is filtered by LSS type and for each type
# LSS is calculated. Apply to Leaders/Followers separately.
# NOTE: None of the functions here are operational.
# ----------------------------------------------- #

# Get the Landing Stabilisation Speed: Leader
Landing_Pair_Leader <- Calculate_Landing_Stabilisation_Speed(Landing_Pair_Leader)

# Get the Landing Stabilisation Speed: Follower
Landing_Pair_Follower <- Calculate_Landing_Stabilisation_Speed(Landing_Pair_Follower)

# ----------------------------------------------- #
# 5.2.2.3 Calculate Deceleration Distances
# ----------------------------------------------- #
# Uses Calculate_Final_Decel_Distance and 
# Calculate_Start_Initial_Decel_Distance from 
# ORD Functions.R. Requires Thousand_Ft_Gate.
# LP data is filtered by LSS type and for each type
# LSS is calculated. Apply to Leaders/Followers separately.
# ----------------------------------------------- #

# Calculate the Start Initial Deceleration Distance: Leader
Landing_Pair_Leader <- Calculate_Start_Initial_Decel_Distance(Landing_Pair_Leader)

# Calculate the Start Initial Deceleration Distance: Follower
Landing_Pair_Follower <- Calculate_Start_Initial_Decel_Distance(Landing_Pair_Follower)

# Calculate the Final Deceleration Distance: Leader
Landing_Pair_Leader <- Calculate_Final_Decel_Distance(Landing_Pair_Leader)

# Calculate the Final Deceleration Distance: Follower
Landing_Pair_Follower <- Calculate_Final_Decel_Distance(Landing_Pair_Follower)

# ----------------------------------------------- #
# 5.2.2.4 Tidy Up & Save Locally & to SQL
# ----------------------------------------------- #
# Remove unwanted fields and bind/reorder to match
# tbl_ORD_Aircraft_Profile table structure.
# Then save locally and to SQL table.
# NOTE: Saving to SQL is not operational currently.
# ----------------------------------------------- #

# Bind the Leader & Follower datasets together.
ORD_Aircraft_Profile <- rbind(Landing_Pair_Leader, Landing_Pair_Follower)

# Order table by Landing_Pair_ID and This_Pair_Role. (L first)
ORD_Aircraft_Profile <- as.data.table(ORD_Aircraft_Profile)
ORD_Aircraft_Profile <- ORD_Aircraft_Profile[order(Landing_Pair_ID, -This_Pair_Role),]
ORD_Aircraft_Profile <- as.data.frame(ORD_Aircraft_Profile)

# Select relevant fields in table order.
ORD_Aircraft_Profile <- select(ORD_Aircraft_Profile,
                               Landing_Pair_ID,
                               This_Pair_Role,
                               Aircraft_Type,
                               Wake_Cat,
                               Compression_Commencement_Threshold,
                               Local_Stabilisation_Distance,
                               VRef,
                               Apply_Gusting,
                               Landing_Stabilisation_Speed,
                               Landing_Stabilisation_Speed_Type,
                               Final_Deceleration,
                               Final_Deceleration_Distance,
                               Steady_Procedural_Speed,
                               End_Initial_Deceleration_Distance,
                               Start_Initial_Deceleration_Distance,
                               Initial_Procedural_Speed)


# Remove Intermediary Data
rm(Landing_Pair_Follower,
   Landing_Pair_Leader,
   ORD_Aircraft_Follower,
   ORD_Aircraft_Leader,
   ORD_DBS_Follower,
   ORD_DBS_Leader,
   ORD_Runway_Reduced,
   ORD_Wake_Follower,
   ORD_Wake_Leader,
   LPR)

# Remove Unnecessary Adaptation
#rm(ORD_Aircraft,
#   ORD_DBS,
#   ORD_Wake)



# ----------------------------------------------- #
# Testing (Validation)
# ----------------------------------------------- #

# ----------------------------------------------- #

if (Test_Mode){

   # Get tbl_ORD_Aircraft_Profile from SQL
   AP_Query <- "SELECT *      
                  FROM tbl_ORD_Aircraft_Profile AP
                  INNER JOIN tbl_Landing_Pair LP
                  ON LP.Landing_Pair_ID = AP.Landing_Pair_ID"
   
   if (Processing_Period == "Day"){
      AP_Query <- paste0(AP_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
   }
   
   if (Processing_Period == "Month"){
      AP_Query <- paste0(AP_Query, " WHERE LP.Landing_Pair_Date LIKE '%", Processing_Month, "%'")
   }
   
   ORD_Aircraft_Profile_SQL <- sqlQuery(con, AP_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                       "Landing_Pair_Type"))
   
   # Assume only testing Calculated Fields for now
   ORD_Aircraft_Profile_SQL <- select(ORD_Aircraft_Profile_SQL, 
                                      Landing_Pair_ID,
                                      Landing_Pair_Date,
                                      This_Pair_Role,
                                      SQL_Landing_Stabilisation_Speed = Landing_Stabilisation_Speed,
                                      SQL_Final_Deceleration_Distance = Final_Deceleration_Distance,
                                      SQL_Start_Initial_Deceleration_Distance = Start_Initial_Deceleration_Distance)
   
   # Get the Required Testing Fields from R Output
   ORD_Aircraft_Profile_R <- select(ORD_Aircraft_Profile,
                                    Landing_Pair_ID,
                                    This_Pair_Role,
                                    Landing_Stabilisation_Speed,
                                    Final_Deceleration_Distance,
                                    Start_Initial_Deceleration_Distance)
   
   # Join on R and SQL results ready for comparing
   ZCOMP_ORD_Aircraft_Profile <- inner_join(ORD_Aircraft_Profile_R, ORD_Aircraft_Profile_SQL, by = c("Landing_Pair_ID", "This_Pair_Role"))
   #ZCOMP_ORD_Aircraft_Profile <- full_join(ORD_Aircraft_Profile_R, ORD_Aircraft_Profile_SQL, by = c("ORD_Tool_Calculation_ID", "This_Pair_Role"))
   
   # Make Difference and Flag variables
   ZCOMP_ORD_Aircraft_Profile <- mutate(ZCOMP_ORD_Aircraft_Profile,
                                        DIFF_Landing_Stabilisation_Speed = abs(Landing_Stabilisation_Speed - SQL_Landing_Stabilisation_Speed),
                                        DIFF_Final_Deceleration_Distance = abs(Final_Deceleration_Distance - SQL_Final_Deceleration_Distance),
                                        DIFF_Start_Initial_Deceleration_Distance = abs(Start_Initial_Deceleration_Distance - SQL_Start_Initial_Deceleration_Distance),
                                        FLAG_Landing_Stabilisation_Speed = ifelse(DIFF_Landing_Stabilisation_Speed > 0.0001, 1, 0),
                                        FLAG_Landing_Stabilisation_Speed = ifelse(is.na(Landing_Stabilisation_Speed) & !is.na(SQL_Landing_Stabilisation_Speed), 1, FLAG_Landing_Stabilisation_Speed),
                                        FLAG_Landing_Stabilisation_Speed = ifelse(!is.na(Landing_Stabilisation_Speed) & is.na(SQL_Landing_Stabilisation_Speed), 1, FLAG_Landing_Stabilisation_Speed),
                                        FLAG_Final_Deceleration_Distance = ifelse(DIFF_Final_Deceleration_Distance > 0.01, 1, 0),
                                        FLAG_Final_Deceleration_Distance = ifelse(is.na(Final_Deceleration_Distance) & !is.na(SQL_Final_Deceleration_Distance), 1, FLAG_Final_Deceleration_Distance),
                                        FLAG_Final_Deceleration_Distance = ifelse(!is.na(Final_Deceleration_Distance) & is.na(SQL_Final_Deceleration_Distance), 1, FLAG_Final_Deceleration_Distance),
                                        FLAG_Start_Initial_Deceleration_Distance = ifelse(DIFF_Start_Initial_Deceleration_Distance > 0.01, 1, 0),
                                        FLAG_Start_Initial_Deceleration_Distance = ifelse(is.na(Start_Initial_Deceleration_Distance) & !is.na(SQL_Start_Initial_Deceleration_Distance), 1, FLAG_Start_Initial_Deceleration_Distance),
                                        FLAG_Start_Initial_Deceleration_Distance = ifelse(!is.na(Start_Initial_Deceleration_Distance) & is.na(SQL_Start_Initial_Deceleration_Distance), 1, FLAG_Start_Initial_Deceleration_Distance))
   
   # Make quick summary table by Landing Pair Date
   ZSTAT_ORD_Aircraft_Profile <- group_by(ZCOMP_ORD_Aircraft_Profile, Landing_Pair_Date) %>%
      summarise(CNT_Landing_Stabilisation_Speed = sum(FLAG_Landing_Stabilisation_Speed, na.rm = T),
                CNT_Final_Deceleration_Distance = sum(FLAG_Final_Deceleration_Distance, na.rm = T),
                CNT_Start_Initial_Deceleration_Distance = sum(FLAG_Start_Initial_Deceleration_Distance, na.rm = T))
   
   # DEBUG
   test <- filter(ZCOMP_ORD_Aircraft_Profile,
                  FLAG_Final_Deceleration_Distance == 1)
   

}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #





  
  
  
  
  