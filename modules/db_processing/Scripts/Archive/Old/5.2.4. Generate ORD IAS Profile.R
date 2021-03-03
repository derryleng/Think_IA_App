# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 0.1. Global Parameters
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Resource file for global parameters for the
#              tool. 
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# v0: Added aviation unit conversion parameters: Time, speed, distance, acceleration, pressure
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ----------------------------------------------- #
# 0.1.1 Data Loading / Prep
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Get the Forecast Surface Headwinds
Surface_Headwinds <- select(Landing_Pair_Reference, Landing_Pair_ID, Forecast_AGI_Surface_Headwind)

# Join the Surface Headwind Data
ORD_Aircraft_Profile_IAS <- left_join(ORD_Aircraft_Profile, Surface_Headwinds, by = c("Landing_Pair_ID"))

# For the purposes of IAS Profile, Surface Headwind must be min 10kts.
ORD_Aircraft_Profile_IAS <- mutate(ORD_Aircraft_Profile_IAS, Surface_Headwind = ifelse(Forecast_AGI_Surface_Headwind < (10*kts_To_mps), 10*kts_To_mps, 
                                                                               Forecast_AGI_Surface_Headwind))
# Landing Pair Runways
Landing_Pair_Runways_Leader <- select(Landing_Pair_Reference, Landing_Pair_ID, Leader_Landing_Runway) %>% 
  rename(Runway = Leader_Landing_Runway)
Landing_Pair_Runways_Follower <- select(Landing_Pair_Reference, Landing_Pair_ID, Follower_Landing_Runway) %>% 
  rename(Runway = Follower_Landing_Runway)

# ----------------------------------------------- #
# 0.1.1 Get the Necessary Parameters
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Time to get the Wind Effects for use in Airbus Sections.
Wind_Effects_0DME <- filter(ORD_Segment_Forecast, DME_Seg == 0 * NM_to_m) %>% select(ID, Wind_Effect_0DME = Forecast_Wind_Effect_IAS)
Wind_Effects_1DME <- filter(ORD_Segment_Forecast, DME_Seg == 1 * NM_to_m) %>% select(ID, Wind_Effect_1DME = Forecast_Wind_Effect_IAS)
Wind_Effects_2DME <- filter(ORD_Segment_Forecast, DME_Seg == 2 * NM_to_m) %>% select(ID, Wind_Effect_2DME = Forecast_Wind_Effect_IAS)
Wind_Effects_3DME <- filter(ORD_Segment_Forecast, DME_Seg == 3 * NM_to_m) %>% select(ID, Wind_Effect_3DME = Forecast_Wind_Effect_IAS)
Wind_Effects_4DME <- filter(ORD_Segment_Forecast, DME_Seg == 4 * NM_to_m) %>% select(ID, Wind_Effect_4DME = Forecast_Wind_Effect_IAS)

# Join on the Wind Effects
ORD_Aircraft_Profile_IAS <- left_join(ORD_Aircraft_Profile_IAS, Wind_Effects_0DME, by = c("Landing_Pair_ID" = "ID"))
ORD_Aircraft_Profile_IAS <- left_join(ORD_Aircraft_Profile_IAS, Wind_Effects_1DME, by = c("Landing_Pair_ID" = "ID"))
ORD_Aircraft_Profile_IAS <- left_join(ORD_Aircraft_Profile_IAS, Wind_Effects_2DME, by = c("Landing_Pair_ID" = "ID"))
ORD_Aircraft_Profile_IAS <- left_join(ORD_Aircraft_Profile_IAS, Wind_Effects_3DME, by = c("Landing_Pair_ID" = "ID"))
ORD_Aircraft_Profile_IAS <- left_join(ORD_Aircraft_Profile_IAS, Wind_Effects_4DME, by = c("Landing_Pair_ID" = "ID"))

# Get the Leader/Follower Aircraft PRofiles.
ORD_Aircraft_Profile_Leader <- filter(ORD_Aircraft_Profile_IAS, This_Pair_Role == "L") 
ORD_Aircraft_Profile_Follower <- filter(ORD_Aircraft_Profile_IAS, This_Pair_Role == "F")

# Join on relevant Runways. Keep Leader runway on both for now. (But follower should have follower in future)
ORD_Aircraft_Profile_Leader <- left_join(ORD_Aircraft_Profile_Leader, Landing_Pair_Runways_Leader, by = c("Landing_Pair_ID"))
ORD_Aircraft_Profile_Follower <- left_join(ORD_Aircraft_Profile_Follower, Landing_Pair_Runways_Leader, by = c("Landing_Pair_ID"))

# Join on Thousand Ft. Gate. 
Thousand_Ft_Gates <- select(ORD_Runway, Runway_Name, Thousand_Ft_Gate, Six_Hundred_Ft_AAL, Four_Hundred_Ft_AAL, Max_DTT)
ORD_Aircraft_Profile_Leader <- left_join(ORD_Aircraft_Profile_Leader, Thousand_Ft_Gates, by = c("Runway" = "Runway_Name"))
ORD_Aircraft_Profile_Follower <- left_join(ORD_Aircraft_Profile_Follower, Thousand_Ft_Gates, by = c("Runway" = "Runway_Name"))

# Get the Adjusted Final Decel and 3/4DME IAS For Airbus Sections.
ORD_Aircraft_Profile_Leader <- mutate(ORD_Aircraft_Profile_Leader,
                                      Adjusted_Final_Decel = (Landing_Stabilisation_Speed - Steady_Procedural_Speed) / (Thousand_Ft_Gate - Final_Deceleration_Distance),
                                      IAS_3DME = Adjusted_Final_Decel * ((3 * NM_to_m) - Final_Deceleration_Distance) + Steady_Procedural_Speed,
                                      IAS_4DME = Adjusted_Final_Decel * ((4 * NM_to_m) - Final_Deceleration_Distance) + Steady_Procedural_Speed)
ORD_Aircraft_Profile_Follower <- mutate(ORD_Aircraft_Profile_Follower,
                                      Adjusted_Final_Decel = (Landing_Stabilisation_Speed - Steady_Procedural_Speed) / (Thousand_Ft_Gate - Final_Deceleration_Distance),
                                      IAS_3DME = Adjusted_Final_Decel * ((3 * NM_to_m) - Final_Deceleration_Distance) + Steady_Procedural_Speed,
                                      IAS_4DME = Adjusted_Final_Decel * ((4 * NM_to_m) - Final_Deceleration_Distance) + Steady_Procedural_Speed)

# Remove The Standalone Wind Effects
rm(Wind_Effects_0DME, Wind_Effects_1DME, Wind_Effects_2DME, Wind_Effects_3DME, Wind_Effects_4DME)


# ----------------------------------------------- #
# 0.1.1 Build the IAS Profile
# ----------------------------------------------- #
# IAS Profile Functions in ORD Functions.R
# ----------------------------------------------- #

# Build all IAS Leader Profiles 
IAS_Profile_Leader <- Build_Full_IAS_Profile(ORD_Aircraft_Profile_Leader)

# Build all IAS Follower Profiles
IAS_Profile_Follower <- Build_Full_IAS_Profile(ORD_Aircraft_Profile_Follower)

# Bind together and order.
ORD_IAS_Profile <- rbind(IAS_Profile_Leader, IAS_Profile_Follower)
ORD_IAS_Profile <- ORD_IAS_Profile[order(ORD_IAS_Profile$Landing_Pair_ID, ORD_IAS_Profile$This_Pair_Role, ORD_IAS_Profile$End_Dist),]

# ----------------------------------------------- #
# 0.1.1 Building the GS Profile
# ----------------------------------------------- #
# We need to join the ORD wind segments to the IAS
# profile. The GS profile has a section for each
# wind segment and "partial" sections for the 
# boundaries of the IAS profile that are within segment
# boundaries. First, we loop through each type of section
# filter the IAS profile for this section and join on 
# IAS values/distances to Wind segments. We then find
# if a wind segment is "within" this section, or "on top"
# or "on bottom" using flags. If within, these segments are
# used directly. If on topo/bottom, the start/end distances
# of segment are adjusted to match IAS profile boundaries
# and added too. IAS speeds are then adjusted by using
# y = mx + c. Start/End GS is then calculated by adding
# the Forecast Wind Effect to Start/End IAS. 
# ----------------------------------------------- #

# Make the start and end dist. (Should probably tune to seg size.)
ORD_Segment_Forecast_GS <- rename(ORD_Segment_Forecast, End_Dist_Wind = DME_Seg) %>% mutate(Start_Dist_Wind = End_Dist_Wind + (1 * NM_to_m))

# Attempt at building
GS_Profile_Leader <- Build_GSPD_Profile(IAS_Profile_Leader, ORD_Segment_Forecast_GS)
GS_Profile_Follower <- Build_GSPD_Profile(IAS_Profile_Follower, ORD_Segment_Forecast_GS)

# Build complete GSPD
ORD_GS_Profile <- rbind(GS_Profile_Leader, GS_Profile_Follower)
ORD_GS_Profile <- ORD_GS_Profile[order(ORD_GS_Profile$Landing_Pair_ID, ORD_GS_Profile$This_Pair_Role, ORD_GS_Profile$Section_Number),]

# Remove the Leader/Follower Versions
rm(GS_Profile_Leader, GS_Profile_Follower, ORD_Segment_Forecast_GS)

# Remove Leader/Follower IAS Profiles
rm(IAS_Profile_Leader, IAS_Profile_Follower)

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ----------------------------------------------- #
# 0.1.1 IAS Profile Testing
# ----------------------------------------------- #

if (Test_Mode){

  #ORD_GS_Profile_R <- ORD_GS_Profile
  ORD_IAS_Profile_R <- ORD_IAS_Profile
  
  IAS_Query <- "SELECT * 
                 FROM tbl_ORD_IAS_Profile GS
                 INNER JOIN tbl_Landing_Pair LP
                 ON LP.Landing_Pair_ID = GS.Landing_Pair_ID"
  
  
  if (Processing_Period == "Day"){
    IAS_Query <- paste0(IAS_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
  }
  
  if (Processing_Period == "Month"){
    IAS_Query <- paste0(IAS_Query, " WHERE LP.Landing_Pair_Date LIKE '%", Processing_Month, "%'")
  }
  
  ORD_IAS_Profile_SQL <- sqlQuery(con, IAS_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                 "Landing_Pair_Type"))
  
  ORD_IAS_Profile_SQL <- select(ORD_IAS_Profile_SQL,
                                Landing_Pair_ID,
                                This_Pair_Role,
                                Section_Number,
                                Landing_Pair_Date,
                                SQL_Profile_Section = Profile_Section,
                                SQL_Profile_Type = Profile_Type,
                                SQL_Start_IAS = Start_IAS,
                                SQL_End_IAS = End_IAS,
                                SQL_Start_Dist = Start_Dist,
                                SQL_End_Dist = End_Dist)
  
  # Join Together
  ZCOMP_ORD_IAS_Profile <- full_join(ORD_IAS_Profile_SQL, ORD_IAS_Profile_R, by = c("Landing_Pair_ID", "This_Pair_Role", "Section_Number"))
  
  # Add Comparison Fields
  ZCOMP_ORD_IAS_Profile <- mutate(ZCOMP_ORD_IAS_Profile,
                                  DIFF_Start_IAS = abs(Start_IAS - SQL_Start_IAS),
                                  DIFF_End_IAS = abs(End_IAS - SQL_End_IAS),
                                  DIFF_Start_Dist = abs(Start_Dist - SQL_Start_Dist),
                                  DIFF_End_Dist = abs(End_Dist - SQL_End_Dist),
                                  FLAG_Profile_Section = ifelse(Profile_Section != SQL_Profile_Section, 1, 0),
                                  FLAG_Profile_Section = ifelse(is.na(Profile_Section) & !is.na(SQL_Profile_Section), 1, FLAG_Profile_Section),
                                  FLAG_Profile_Section = ifelse(!is.na(Profile_Section) & is.na(SQL_Profile_Section), 1, FLAG_Profile_Section),
                                  FLAG_Profile_Type = ifelse(Profile_Type != SQL_Profile_Type, 1, 0),
                                  FLAG_Profile_Type = ifelse(is.na(Profile_Type) & !is.na(SQL_Profile_Type), 1, FLAG_Profile_Type),
                                  FLAG_Profile_Type = ifelse(!is.na(Profile_Type) & is.na(SQL_Profile_Type), 1, FLAG_Profile_Type),
                                  FLAG_Start_IAS = ifelse(DIFF_Start_IAS > 0.001, 1, 0),
                                  FLAG_Start_IAS = ifelse(is.na(Start_IAS) & !is.na(SQL_Start_IAS), 1, FLAG_Start_IAS),
                                  FLAG_Start_IAS = ifelse(!is.na(Start_IAS) & is.na(SQL_Start_IAS), 1, FLAG_Start_IAS),
                                  FLAG_End_IAS = ifelse(DIFF_End_IAS > 0.001, 1, 0),
                                  FLAG_End_IAS = ifelse(is.na(End_IAS) & !is.na(SQL_End_IAS), 1, FLAG_End_IAS),
                                  FLAG_End_IAS = ifelse(!is.na(End_IAS) & is.na(SQL_End_IAS), 1, FLAG_End_IAS),
                                  FLAG_Start_Dist = ifelse(DIFF_Start_Dist > 0.01, 1, 0),
                                  FLAG_Start_Dist = ifelse(is.na(Start_Dist) & !is.na(SQL_Start_Dist), 1, FLAG_Start_Dist),
                                  FLAG_Start_Dist = ifelse(!is.na(Start_Dist) & is.na(SQL_Start_Dist), 1, FLAG_Start_Dist),
                                  FLAG_End_Dist = ifelse(DIFF_End_Dist > 0.01, 1, 0),
                                  FLAG_End_Dist = ifelse(is.na(End_Dist) & !is.na(SQL_End_Dist), 1, FLAG_End_Dist),
                                  FLAG_End_Dist = ifelse(!is.na(End_Dist) & is.na(SQL_End_Dist), 1, FLAG_End_Dist))
                                  
  
  # Make Summary Stats
  ZSTAT_ORD_IAS_Profile <- group_by(ZCOMP_ORD_IAS_Profile, Landing_Pair_Date) %>%
    summarise(CNT_Profile_Section = sum(FLAG_Profile_Section, na.rm = T),
              CNT_Profile_Type = sum(FLAG_Profile_Type, na.rm = T),
              CNT_Start_IAS = sum(FLAG_Start_IAS, na.rm = T),
              CNT_End_IAS = sum(FLAG_End_IAS, na.rm = T),
              CNT_Start_Dist = sum(FLAG_Start_Dist, na.rm = T),
              CNT_End_Dist = sum(FLAG_End_Dist, na.rm = T))

                              
}

# ----------------------------------------------- #
# 0.1.1 GS Profile Testing
# ----------------------------------------------- #

if (Test_Mode){
  
  ORD_GS_Profile_R <- ORD_GS_Profile
  
  GS_Query <- "SELECT * 
                 FROM tbl_ORD_GS_Profile GS
                 INNER JOIN tbl_Landing_Pair LP
                 ON LP.Landing_Pair_ID = GS.Landing_Pair_ID"
  
  
  if (Processing_Period == "Day"){
    GS_Query <- paste0(GS_Query, " WHERE LP.Landing_Pair_Date = '", Processing_Date, "'")
  }
  
  if (Processing_Period == "Month"){
    GS_Query <- paste0(GS_Query, " WHERE LP.Landing_Pair_Date LIKE '%", Processing_Month, "%'")
  }
  
  ORD_GS_Profile_SQL <- sqlQuery(con, GS_Query, stringsAsFactors = F) %>% select(-c("Leader_Flight_Plan_ID", "Follower_Flight_Plan_ID",
                                                                                      "Landing_Pair_Type"))
  
  ORD_GS_Profile_SQL <- select(ORD_GS_Profile_SQL,
                                Landing_Pair_ID,
                                This_Pair_Role,
                                Section_Number,
                                Landing_Pair_Date,
                                SQL_Profile_Section = Profile_Section,
                                SQL_Profile_Type = Profile_Type,
                                SQL_Start_IAS = Start_IAS,
                                SQL_End_IAS = End_IAS,
                                SQL_Start_GS = Start_GS,
                                SQL_End_GS = End_GS,
                                SQL_Start_Dist = Start_Dist,
                                SQL_End_Dist = End_Dist)
  
  # Join Together
  ZCOMP_ORD_GS_Profile <- full_join(ORD_GS_Profile_SQL, ORD_GS_Profile_R, by = c("Landing_Pair_ID", "This_Pair_Role", "Section_Number"))
  
  # Add Comparison Fields
  ZCOMP_ORD_GS_Profile <- mutate(ZCOMP_ORD_GS_Profile,
                                  DIFF_Start_IAS = abs(Start_IAS - SQL_Start_IAS),
                                  DIFF_End_IAS = abs(End_IAS - SQL_End_IAS),
                                  DIFF_Start_GS = abs(Start_GS - SQL_Start_GS),
                                  DIFF_End_GS = abs(End_GS - SQL_End_GS),
                                  DIFF_Start_Dist = abs(Start_Dist - SQL_Start_Dist),
                                  DIFF_End_Dist = abs(End_Dist - SQL_End_Dist),
                                  FLAG_Profile_Section = ifelse(Profile_Section != SQL_Profile_Section, 1, 0),
                                  FLAG_Profile_Section = ifelse(is.na(Profile_Section) & !is.na(SQL_Profile_Section), 1, FLAG_Profile_Section),
                                  FLAG_Profile_Section = ifelse(!is.na(Profile_Section) & is.na(SQL_Profile_Section), 1, FLAG_Profile_Section),
                                  FLAG_Profile_Type = ifelse(Profile_Type != SQL_Profile_Type, 1, 0),
                                  FLAG_Profile_Type = ifelse(is.na(Profile_Type) & !is.na(SQL_Profile_Type), 1, FLAG_Profile_Type),
                                  FLAG_Profile_Type = ifelse(!is.na(Profile_Type) & is.na(SQL_Profile_Type), 1, FLAG_Profile_Type),
                                  FLAG_Start_IAS = ifelse(DIFF_Start_IAS > 0.001, 1, 0),
                                  FLAG_Start_IAS = ifelse(is.na(Start_IAS) & !is.na(SQL_Start_IAS), 1, FLAG_Start_IAS),
                                  FLAG_Start_IAS = ifelse(!is.na(Start_IAS) & is.na(SQL_Start_IAS), 1, FLAG_Start_IAS),
                                  FLAG_End_IAS = ifelse(DIFF_End_IAS > 0.001, 1, 0),
                                  FLAG_End_IAS = ifelse(is.na(End_IAS) & !is.na(SQL_End_IAS), 1, FLAG_End_IAS),
                                  FLAG_End_IAS = ifelse(!is.na(End_IAS) & is.na(SQL_End_IAS), 1, FLAG_End_IAS),
                                  FLAG_Start_GS = ifelse(DIFF_Start_GS > 0.001, 1, 0),
                                  FLAG_Start_GS = ifelse(is.na(Start_GS) & !is.na(SQL_Start_GS), 1, FLAG_Start_GS),
                                  FLAG_Start_GS = ifelse(!is.na(Start_GS) & is.na(SQL_Start_GS), 1, FLAG_Start_GS),
                                  FLAG_End_GS = ifelse(DIFF_End_GS > 0.001, 1, 0),
                                  FLAG_End_GS = ifelse(is.na(End_GS) & !is.na(SQL_End_GS), 1, FLAG_End_GS),
                                  FLAG_End_GS = ifelse(!is.na(End_GS) & is.na(SQL_End_GS), 1, FLAG_End_GS),
                                  FLAG_Start_Dist = ifelse(DIFF_Start_Dist > 0.01, 1, 0),
                                  FLAG_Start_Dist = ifelse(is.na(Start_Dist) & !is.na(SQL_Start_Dist), 1, FLAG_Start_Dist),
                                  FLAG_Start_Dist = ifelse(!is.na(Start_Dist) & is.na(SQL_Start_Dist), 1, FLAG_Start_Dist),
                                  FLAG_End_Dist = ifelse(DIFF_End_Dist > 0.01, 1, 0),
                                  FLAG_End_Dist = ifelse(is.na(End_Dist) & !is.na(SQL_End_Dist), 1, FLAG_End_Dist),
                                  FLAG_End_Dist = ifelse(!is.na(End_Dist) & is.na(SQL_End_Dist), 1, FLAG_End_Dist))
  
  
  # Make Summary Stats
  ZSTAT_ORD_GS_Profile <- group_by(ZCOMP_ORD_GS_Profile, Landing_Pair_Date) %>%
    summarise(CNT_Profile_Section = sum(FLAG_Profile_Section, na.rm = T),
              CNT_Profile_Type = sum(FLAG_Profile_Type, na.rm = T),
              CNT_Start_IAS = sum(FLAG_Start_IAS, na.rm = T),
              CNT_End_IAS = sum(FLAG_End_IAS, na.rm = T),
              CNT_Start_GS = sum(FLAG_Start_GS, na.rm = T),
              CNT_End_GS = sum(FLAG_End_GS, na.rm = T),
              CNT_Start_Dist = sum(FLAG_Start_Dist, na.rm = T),
              CNT_End_Dist = sum(FLAG_End_Dist, na.rm = T))
  
  date <- '05/11/2018'
  timehigh <- 23930
  timelow <- timehigh - 1000
  rg <- 'RSo'
  DME <- 3704
  
  test <- filter(ZCOMP_ORD_IAS_Profile, FLAG_Start_IAS == 1)
  
  testlp <- select(Landing_Pair_Reference, Landing_Pair_ID, Landing_Pair_Date, Prediction_Time, Leader_Landing_Runway) %>% 
    filter(Landing_Pair_ID %in% test$Landing_Pair_ID)
  
  testseg <- filter(Forecast_Seg, Runway_Group == rg & Forecast_Date == date & Forecast_Time > timelow & Forecast_Time < timehigh & DME_Seg == DME)
  testseg <- testseg[order(testseg$DME_Seg, testseg$Forecast_Time),]
}


# Testing Profile section sample sizes - all correct
#GS_Outlier_Test_R <- group_by(GS_R, Landing_Pair_ID, This_Pair_Role, Profile_Section) %>% summarise(Count_R = n()) %>% ungroup()
#GS_Outlier_Test_SQL <- group_by(GS_SQL, Landing_Pair_ID, This_Pair_Role, Profile_Section) %>% summarise(Count_SQL = n()) %>% ungroup()
#GS_Outlier_Test <- full_join(GS_Outlier_Test_R, GS_Outlier_Test_SQL, by = c("Landing_Pair_ID", "This_Pair_Role", "Profile_Section")) %>%
#  filter(abs(Count_R - Count_SQL) > 0)

#rm(GS_Outlier_Test_R, GS_Outlier_Test_SQL, GS_Outlier_Test)

#GS_R <- GS_R[order(GS_R$Landing_Pair_ID, GS_R$This_Pair_Role, GS_R$Section_Number),]
#GS_SQL <- GS_SQL[order(GS_SQL$Landing_Pair_ID, GS_SQL$This_Pair_Role, GS_SQL$Section_Number),]

#GS_SQL <- select(GS_SQL, -c("DME_Seg", "End_Wind_Effect"))

#GS_R <- inner_join(select(GS_SQL, -c("Profile_Section")),
 #                            select(GS_R, -c("Profile_Section")), 
#                             by = c("Landing_Pair_ID", "This_Pair_Role", "Section_Number")) %>%
#  mutate(Start_Dist_Diff = abs(Start_Dist.x - Start_Dist.y),
#         End_Dist_Diff = abs(End_Dist.x - End_Dist.y),
#         Start_IAS_Diff = abs(Start_IAS.x - Start_IAS.y),
#         End_IAS_Diff = abs(End_IAS.x - End_IAS.y),
#         Start_GS_Diff = abs(Start_GS.x - Start_GS.y),
#         End_GS_Diff = abs(End_GS.x - End_GS.y))

#filt <- filter(GS_R, Start_Dist_Diff > 0.001)
#filt <- filter(GS_R, End_Dist_Diff > 0.001)
#filt <- filter(GS_R, Start_IAS_Diff > 0.0001)
#filt <- filter(GS_R, End_IAS_Diff > 0.0001)
#filt <- filter(GS_R, Start_GS_Diff > 0.0001)
#filt <- filter(GS_R, End_GS_Diff > 0.0001)

#GS_SQL <- sqlQuery(con, "SELECT * FROM tbl_ORD_GS_Profile", stringsAsFactors = F)
#GS_R <- GS_R[order(GS_R$Landing_Pair_ID, GS_R$This_Pair_Role, GS_R$Section_Number),]
#GS_SQL <- GS_SQL[order(GS_SQL$Landing_Pair_ID, GS_SQL$This_Pair_Role, GS_SQL$Section_Number),]

#Landing_Pair_Reference <- fread("Landing_Pair_Reference.csv")
#Forecast_Segs1 <- fread("ORD_Forecast_Segs.csv")
#ORD_IAS_Profile <- fread("ORD_IAS_Profile.csv")
#GS_SQL <- fread("ORD_GS_Profile_SQL.csv")
#GS_R <- fread("ORD_GS_Profile_R.csv")

#rm(ORD_GS_Profile_R, ORD_GS_Profile_SQL)


  