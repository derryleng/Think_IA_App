# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 5.2.8. Setup Performance Model
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: 
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
# - 
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ----------------------------------------------- #
# X.X.X Required Fields
# ----------------------------------------------- #
# 
# ----------------------------------------------- #

# Landing_Pair_ID
# FP_Date
# Leader_Callsign
# Leader_Aircraft_Type
# Leader_UK_Wake_Cat
# Leader_Recat_Wake_Cat
# UK6Cat_Separation_Distance
# Ref_Recat_Wake_Separation_Distance
# Ref_ROT_Spacing_Distance
# UK6Cat_Separation_Time
# Ref_Recat_Wake_Separation_Time
# Ref_ROT_Spacing_Time
# Follower_Ass_Recat_Separation_IAS
# Follower_Ass_ROT_Spacing_IAS
# Follower_Ass_IAS
#### Follower_Forecast_TBS_Wind_Effect
#### Follower_Forecast_eTBS_Wind_Effect
# Observed_AGI_Surface_Headwind
# Observed_AGI_Surface_Wind_SPD
# Observed_AGI_Surface_Wind_HDG
#### UK6Cat_TBS_4DME_Wake_Separation_Distance
#### Recat_eTBS_0DME_Wake_Separation_Distance
#### Recat_eTBS_0DME_ROT_Spacing_Distance
#### Recat_eTBS_0DME_All_Separation_Distance
# Forecast_ORD_TBS_Compression
# Forecast_ORD_eTBS_Compression
#### Recat_eTBS_4DME_Wake_Separation_Distance
#### Recat_eTBS_4DME_ROT_Spacing_Distance
#### Recat_eTBS_4DME_All_Separation_Distance
# Leader_0DME_RTT
#### Observed_0DME_Separation_Distance
#### Observed_1DME_Separation_Distance
#### Observed_4DME_Separation_Distance
#### Observed_4DME_Separation_Accuracy
# Leader_4DME_Time
#### Observed_0DME_Separation_Time
#### Observed_1DME_Separation_Time
#### Observed_4DME_Separation_Time
#### Observed_Follower_eTBS_IAS
#### Observed_Follower_TBS_Wind_Effect
#### Observed_Follower_eTBS_Wind_Effect
# Observed_Compression

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# GENERATE: IA Performance Model Setup
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Generate_IA_Performance_Model_Setup <- function(con, LP_Primary_Key, Landing_Pair, Radar, ORD_Segment_Forecast){
  
  # Get Initial Time
  Proc_Initial_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message("Generating Performance Model Setup Data...")

  # ----------------------------------------------- #
  # Parameters
  # ----------------------------------------------- #
  # 
  # ----------------------------------------------- #
  
  # Distance Caps
  Min_Recat_0DME <- 2.5 * NM_to_m
  Min_UK_4DME <- 2.5 * NM_to_m
  Min_Recat_4DME <- 3.0 * NM_to_m
  
  # Flag to remove all non-wake pairs.
  Wake_Pairs_Only <- as.logical(ADAP_Config$Process_Wake_Pairs_Only)
  
  # Flag to remove Not-In_Trail pairs. 
  In_Trail_Only <- as.logical(ADAP_Config$Process_In_Trail_Pairs_Only)
  
  # Get Performance Flag
  Landing_Pair <- Create_Filter_Flag_Performance(Landing_Pair, Wake_Pairs_Only, In_Trail_Only)
  
  # Get DME Seg Size
  GWCS_Adaptation <- Load_Adaptation_Table(con, "tbl_Mode_S_Wind_Adaptation")
  Seg_Size <- GWCS_Adaptation$DME_Seg_Size
  
  # ----------------------------------------------- #
  # Wind Effects
  # ----------------------------------------------- #
  
  # TEMP: Amend Forecast ORD Compression to only consider those with Observed Compression
  #Landing_Pair <- mutate(Landing_Pair, Forecast_ORD_Compression = ifelse(is.na(Observed_ORD_Compression), NA, Forecast_ORD_Compression))
  
  # Get the TBS Distances Without Wind Effect (Keep to RECAT Operations for Now)
  Landing_Pair <- mutate(Landing_Pair,
                         Reference_Recat_Wake_Separation_TBS_Distance = Reference_Recat_Wake_Separation_Time * Reference_Recat_Wake_Separation_IAS,
                         Reference_Recat_ROT_Spacing_TBS_Distance = Reference_Recat_ROT_Spacing_Time * Reference_Recat_ROT_Spacing_IAS)
  
  # Get the Average Recat Wake Wind effect
  Landing_Pair <- Get_Average_Forecast_Wind_Effect(Landing_Pair, ORD_Segment_Forecast,
                                                   Prefix = "Recat_Wake",
                                                   ID_Var = "Landing_Pair_ID",
                                                   Start_Dist_Var = "Reference_Recat_Wake_Separation_TBS_Distance",
                                                   End_Dist_Var = "Delivery_Distance",
                                                   Speed_Var = "Reference_Recat_Wake_Separation_IAS",
                                                   Seg_Size)
  
  # Get the Average Recat ROT Wind effect
  Landing_Pair <- Get_Average_Forecast_Wind_Effect(Landing_Pair, ORD_Segment_Forecast,
                                                   Prefix = "Recat_ROT",
                                                   ID_Var = "Landing_Pair_ID",
                                                   Start_Dist_Var = "Reference_Recat_ROT_Spacing_TBS_Distance",
                                                   End_Dist_Var = "Delivery_Distance",
                                                   Speed_Var = "Reference_Recat_ROT_Spacing_IAS",
                                                   Seg_Size)
  
  # Placeholder for TBS Wind Effect
  Landing_Pair <- mutate(Landing_Pair, Follower_Forecast_TBS_Wind_Effect = NA)
  
  # Placeholder for eTBS Compression - Requires ORD Compression Procedure in TBS Distance (Very doable!!)
  Landing_Pair <- mutate(Landing_Pair, Forecast_ORD_eTBS_Compression = NA)
  
  # Placeholder for TBS Reference Time
  Landing_Pair <- mutate(Landing_Pair, Reference_Legacy_Wake_Separation_Time = NA)
  
  
  # ----------------------------------------------- #
  # Distances
  # ----------------------------------------------- #
  
  # Get the RECAT 0DME Distances
  Landing_Pair <- mutate(Landing_Pair, 
                         Recat_eTBS_0DME_Wake_Separation_Distance = Reference_Recat_Wake_Separation_Time *
                           (Reference_Recat_Wake_Separation_IAS + Follower_Forecast_Recat_Wake_Wind_Effect),
                         Recat_eTBS_0DME_ROT_Spacing_Distance = Reference_Recat_ROT_Spacing_Time *
                           (Reference_Recat_ROT_Spacing_IAS + Follower_Forecast_Recat_ROT_Wind_Effect))
  
  # Get the "Winning" Distance RECAT @ 0DME and Cap it.
  Landing_Pair <- Get_Max_Valid_Value_2Var(Landing_Pair, 
                                           "Recat_eTBS_0DME_All_Separation_Distance",
                                           "Recat_eTBS_0DME_Wake_Separation_Distance",
                                           "Recat_eTBS_0DME_ROT_Spacing_Distance")
  
  Landing_Pair <- Get_Max_Valid_Value_Const(Landing_Pair,
                                            "Recat_eTBS_0DME_All_Separation_Distance",
                                            "Recat_eTBS_0DME_All_Separation_Distance",
                                            Min_Recat_0DME)
  
  # Get the 4DME RECAT Distances By Adding on the ORD Forecast Compression.
  Landing_Pair <- mutate(Landing_Pair,
                         Recat_eTBS_4DME_Wake_Separation_Distance = Recat_eTBS_0DME_Wake_Separation_Distance + Forecast_ORD_Compression,
                         Recat_eTBS_4DME_ROT_Spacing_Distance = Recat_eTBS_0DME_ROT_Spacing_Distance + Forecast_ORD_Compression)
  
  # Make a placeholder for the UK6Cat 4DME Distance
  Landing_Pair <- mutate(Landing_Pair, UK6Cat_TBS_4DME_Wake_Separation_Distance = NA)
  
  # Get the "Winning" Distance RECAT @ 4DME and Cap it.
  Landing_Pair <- Get_Max_Valid_Value_2Var(Landing_Pair, 
                                           "Recat_eTBS_4DME_All_Separation_Distance",
                                           "Recat_eTBS_4DME_Wake_Separation_Distance",
                                           "Recat_eTBS_4DME_ROT_Spacing_Distance")
  
  Landing_Pair <- Get_Max_Valid_Value_Const(Landing_Pair,
                                            "Recat_eTBS_4DME_All_Separation_Distance",
                                            "Recat_eTBS_4DME_All_Separation_Distance",
                                            Min_Recat_4DME)
  
  # Get the "Winning" Distance UK @ 4DME and Cap it. Will Always be constant
  Landing_Pair <- Get_Max_Valid_Value_Const(Landing_Pair,
                                            "UK6Cat_TBS_4DME_Wake_Separation_Distance",
                                            "UK6Cat_TBS_4DME_Wake_Separation_Distance",
                                            Min_UK_4DME)
  
  # Get the "Winning" eTBS IAS/WE Values
  Landing_Pair <- Get_Value_If_Equal_2Var(Landing_Pair,
                                          New_Var = "Follower_Forecast_eTBS_Wind_Effect", Dep_Var = "Recat_eTBS_0DME_All_Separation_Distance", 
                                          Check_Var_1 = "Recat_eTBS_0DME_Wake_Separation_Distance", Check_Var_2 = "Recat_eTBS_0DME_ROT_Spacing_Distance",
                                          Set_Var_1 = "Follower_Forecast_Recat_Wake_Wind_Effect", Set_Var_2 = "Follower_Forecast_Recat_ROT_Wind_Effect")
  
  Landing_Pair <- Get_Value_If_Equal_2Var(Landing_Pair,
                                          New_Var = "Follower_Ass_IAS", Dep_Var = "Recat_eTBS_0DME_All_Separation_Distance", 
                                          Check_Var_1 = "Recat_eTBS_0DME_Wake_Separation_Distance", Check_Var_2 = "Recat_eTBS_0DME_ROT_Spacing_Distance",
                                          Set_Var_1 = "Reference_Recat_Wake_Separation_IAS", Set_Var_2 = "Reference_Recat_ROT_Spacing_IAS")
  
  
  # TEMP: Get the Observed 4DME Separation Accuracy as Observed 4DME - UK6Cat 4DME
  Landing_Pair <- mutate(Landing_Pair, 
                         Observed_4DME_Separation_Accuracy = Observed_4DME_Separation_Distance - UK6Cat_TBS_4DME_Wake_Separation_Distance) 
  
  
  # ----------------------------------------------- #
  # Observed IAS/WE Values
  # ----------------------------------------------- #
  
  # Split up Data into In-Trail and Not-In-Trail for Observed Speed/WE Calcs
  Landing_Pair_I <- Get_In_Trail(Landing_Pair)
  Landing_Pair_N <- Get_Not_In_Trail(Landing_Pair)
  
  # Recat eTBS Observed Wind Effect/Speeds
  Landing_Pair_I <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_I, Radar,
                                                         Prefix = "eTBS",
                                                         LorF = "Follower",
                                                         TimeorRange = "Range", 
                                                         Start_Var = "Recat_eTBS_0DME_All_Separation_Distance",
                                                         End_Var = "Delivery_Distance")
  
  # UK6CAT TBS Observed Wind Effect/Speeds
  Landing_Pair_I <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_I, Radar,
                                                         Prefix = "TBS",
                                                         LorF = "Follower",
                                                         TimeorRange = "Range", 
                                                         Start_Var = "UK6Cat_TBS_4DME_Wake_Separation_Distance",
                                                         End_Var = "Delivery_Distance")
  
  # Recat eTBS Observed Wind Effect/Speeds
  Landing_Pair_N <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_N, Radar,
                                                           Prefix = "eTBS",
                                                           LorF = "Follower",
                                                           TimeorRange = "Range", 
                                                           Start_Var = "Recat_eTBS_0DME_All_Separation_Distance",
                                                           End_Var = "Delivery_Distance")
  
  # UK6CAT TBS Observed Wind Effect/Speeds
  Landing_Pair_N <- Get_Average_Observed_Mode_S_Parameters(Landing_Pair_N, Radar,
                                                           Prefix = "TBS",
                                                           LorF = "Follower",
                                                           TimeorRange = "Range", 
                                                           Start_Var = "UK6Cat_TBS_4DME_Wake_Separation_Distance",
                                                           End_Var = "Delivery_Distance")
  
  # Bind Datasets Together and Reorder
  Landing_Pair <- rbind(Landing_Pair_I, Landing_Pair_N)
  Landing_Pair <- Order_Landing_Pairs(Landing_Pair, "Landing_Pair_ID")
  rm(Landing_Pair_I, Landing_Pair_N)
  
  # How long did it take?
  Proc_End_Time <- Convert_Time_String_to_Seconds(substr(Sys.time(), 12, 19))
  message(paste0("Generated Performance Model Setup Data in ", seconds_to_period(Proc_End_Time - Proc_Initial_Time), "."))
  
  return(Landing_Pair)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# COMPARE: IA Performance Model Setup
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Compare_IA_Performance_Model_Setup <- function(con, LP_Primary_Key, PROC_Period, PROC_Criteria, Landing_Pair){
  
  ### ------------------  SQL Data Query ------------------------------------------------
  PM_Query <- "SELECT
  PM.Landing_Pair_ID,
  PM.Ref_Recat_Wake_Separation_Time AS SQL_Reference_Recat_Wake_Separation_Time,
  PM.Ref_Recat_Wake_Separation_Distance AS SQL_Reference_Recat_Wake_Separation_Distance,
  PM.Recat_eTBS_0DME_Wake_Separation_Distance AS SQL_Recat_eTBS_0DME_Wake_Separation_Distance,
  PM.Recat_eTBS_0DME_ROT_Spacing_Distance AS SQL_Recat_eTBS_0DME_ROT_Spacing_Distance,
  PM.Recat_eTBS_0DME_All_Separation_Distance AS SQL_Recat_eTBS_0DME_All_Separation_Distance,
  PM.Observed_0DME_Separation_Distance AS SQL_Observed_0DME_Separation_Distance,
  PM.Observed_1DME_Separation_Distance AS SQL_Observed_1DME_Separation_Distance,
  PM.Observed_4DME_Separation_Distance AS SQL_Observed_4DME_Separation_Distance,
  PM.Observed_0DME_Separation_Time AS SQL_Observed_0DME_Separation_Time,
  PM.Observed_1DME_Separation_Time AS SQL_Observed_1DME_Separation_Time,
  PM.Observed_4DME_Separation_Time AS SQL_Observed_4DME_Separation_Time,
  PM.Follower_Forecast_eTBS_Wind_Effect AS SQL_Follower_Forecast_eTBS_Wind_Effect,
  PM.Follower_Ass_IAS AS SQL_Follower_Ass_IAS,
  PM.UK6Cat_TBS_4DME_Wake_Separation_Distance AS SQL_UK6Cat_TBS_4DME_Wake_Separation_Distance,
  PM.Recat_eTBS_4DME_Wake_Separation_Distance AS SQL_Recat_eTBS_4DME_Wake_Separation_Distance,
  PM.Recat_eTBS_4DME_ROT_Spacing_Distance AS SQL_Recat_eTBS_4DME_ROT_Spacing_Distance,
  PM.Recat_eTBS_4DME_All_Separation_Distance AS SQL_Recat_eTBS_4DME_All_Separation_Distance,
  PM.Observed_Follower_eTBS_IAS AS SQL_Observed_Follower_eTBS_IAS,
  PM.Observed_Follower_TBS_Wind_Effect AS SQL_Observed_Follower_TBS_Wind_Effect,
  PM.Observed_Follower_eTBS_Wind_Effect AS SQL_Observed_Follower_eTBS_Wind_Effect
  FROM tbl_eTBS_Performance_Model PM"
  
  if (PROC_Period == "Day"){
    PM_Query <- paste0(PM_Query, " WHERE PM.FP_Date = '", PROC_Criteria, "'")
  }
  
  if (PROC_Period == "Month"){
    PM_Query <- paste0(PM_Query, " WHERE PM.FP_Date LIKE '%", PROC_Criteria, "%'")
  }
  ### ---------------------  Get R & SQL Data  ------------------------------------------
  
  Performance_Model_SQL <- sqlQuery(con, PM_Query, stringsAsFactors = F)
  
  Performance_Model_R <- select(Landing_Pair,
                                Landing_Pair_ID,
                                Landing_Pair_Date,
                                Performance_Flag,
                                Reference_Recat_Wake_Separation_Distance,
                                Reference_Recat_Wake_Separation_Time,
                                Recat_eTBS_0DME_Wake_Separation_Distance,
                                Recat_eTBS_0DME_ROT_Spacing_Distance,
                                Recat_eTBS_0DME_All_Separation_Distance,
                                Recat_eTBS_4DME_Wake_Separation_Distance,
                                Recat_eTBS_4DME_ROT_Spacing_Distance,
                                Recat_eTBS_4DME_All_Separation_Distance,
                                UK6Cat_TBS_4DME_Wake_Separation_Distance,
                                Observed_0DME_Separation_Distance,
                                Observed_1DME_Separation_Distance,
                                Observed_4DME_Separation_Distance,
                                Observed_0DME_Separation_Time,
                                Observed_1DME_Separation_Time,
                                Observed_4DME_Separation_Time,
                                Follower_Forecast_eTBS_Wind_Effect,
                                Follower_Ass_IAS,
                                Observed_Follower_eTBS_IAS,
                                Observed_Follower_eTBS_Wind_Effect,
                                Observed_Follower_TBS_Wind_Effect)
  
  Performance_Model_R <- filter(Performance_Model_R, Performance_Flag == 0)
  
  ### -------------------- Create Comparison Table --------------------------------------
  
  # Full Join two Datasets
  ZCOMP_Performance_Model <- full_join(Performance_Model_SQL, Performance_Model_R, by = c("Landing_Pair_ID"))
  
  ZCOMP_Performance_Model <- ZCOMP_Performance_Model %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Reference_Recat_Wake_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Reference_Recat_Wake_Separation_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Recat_eTBS_0DME_Wake_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Recat_eTBS_0DME_ROT_Spacing_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Recat_eTBS_0DME_All_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Recat_eTBS_4DME_Wake_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Recat_eTBS_4DME_ROT_Spacing_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Recat_eTBS_4DME_All_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "UK6Cat_TBS_4DME_Wake_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_0DME_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_1DME_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_4DME_Separation_Distance", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_0DME_Separation_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_1DME_Separation_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_4DME_Separation_Time", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Follower_Forecast_eTBS_Wind_Effect", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Follower_Ass_IAS", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Follower_eTBS_IAS", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Follower_eTBS_Wind_Effect", Tolerance = 0.01) %>%
    Add_Test_Variable(Type = "Numeric", Parameter = "Observed_Follower_TBS_Wind_Effect", Tolerance = 0.01)
  
  ### -----------------------------------------------------------------------------------
  
  return(ZCOMP_Performance_Model)

}
  

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# SUMMARY: IA Performance Model Setup
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #


Summary_IA_Performance_Model_Setup <- function(LP_Primary_Key, ZCOMP_Performance_Model){
  
  # Summary Table
  ZSTAT_Performance_Model <- group_by(ZCOMP_Performance_Model, Landing_Pair_Date) %>% summarise(
    CNT_Reference_Recat_Wake_Separation_Distance = sum(FLAG_Reference_Recat_Wake_Separation_Distance, na.rm = T),
    CNT_Reference_Recat_Wake_Separation_Time = sum(FLAG_Reference_Recat_Wake_Separation_Time, na.rm = T),
    CNT_Recat_eTBS_0DME_Wake_Separation_Distance = sum(FLAG_Recat_eTBS_0DME_Wake_Separation_Distance, na.rm = T),
    CNT_Recat_eTBS_0DME_ROT_Spacing_Distance = sum(FLAG_Recat_eTBS_0DME_ROT_Spacing_Distance, na.rm = T),
    CNT_Recat_eTBS_0DME_All_Separation_Distance = sum(FLAG_Recat_eTBS_0DME_All_Separation_Distance, na.rm = T),
    CNT_Recat_eTBS_4DME_Wake_Separation_Distance = sum(FLAG_Recat_eTBS_4DME_Wake_Separation_Distance , na.rm = T),
    CNT_Recat_eTBS_4DME_ROT_Spacing_Distance = sum(FLAG_Recat_eTBS_4DME_ROT_Spacing_Distance , na.rm = T),
    CNT_Recat_eTBS_4DME_All_Separation_Distance = sum(FLAG_Recat_eTBS_4DME_All_Separation_Distance , na.rm = T),
    CNT_Observed_0DME_Separation_Distance = sum(FLAG_Observed_0DME_Separation_Distance , na.rm = T),
    CNT_Observed_1DME_Separation_Distance = sum(FLAG_Observed_1DME_Separation_Distance , na.rm = T),
    CNT_Observed_4DME_Separation_Distance = sum(FLAG_Observed_4DME_Separation_Distance , na.rm = T),
    CNT_Observed_0DME_Separation_Time = sum(FLAG_Observed_0DME_Separation_Time , na.rm = T),
    CNT_Observed_1DME_Separation_Time = sum(FLAG_Observed_1DME_Separation_Time , na.rm = T),
    CNT_Observed_4DME_Separation_Time = sum(FLAG_Observed_4DME_Separation_Time , na.rm = T),
    CNT_Follower_Forecast_eTBS_Wind_Effect = sum(FLAG_Follower_Forecast_eTBS_Wind_Effect , na.rm = T),
    CNT_Follower_Ass_IAS = sum(FLAG_Follower_Ass_IAS , na.rm = T),
    CNT_Observed_Follower_eTBS_IAS = sum(FLAG_Observed_Follower_eTBS_IAS , na.rm = T),
    CNT_Observed_Follower_eTBS_Wind_Effect = sum(FLAG_Observed_Follower_eTBS_Wind_Effect , na.rm = T),
    CNT_Observed_Follower_TBS_Wind_Effect = sum(FLAG_Observed_Follower_TBS_Wind_Effect , na.rm = T)) %>% ungroup()
  
  return(ZSTAT_Performance_Model)
  
}


# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CONSTRUCT: IA Performance Model Setup
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Construct_IA_Performance_Model_Setup <- function(LP_Primary_Key, Landing_Pair){
  
  # Filter for Valid Performance Flag
  Performance_Model <- filter(Landing_Pair, Performance_Flag == 0)
  
  # Select Relevant Fields
  Performance_Model <- select(Performance_Model,
                              Landing_Pair_ID,
                              FP_Date = Landing_Pair_Date,
                              Leader_Callsign,
                              Leader_Aircraft_Type,
                              Leader_UK_Wake_Cat = Leader_Legacy_Wake_Cat,
                              Leader_Recat_Wake_Cat,
                              Follower_Callsign,
                              Follower_Aircraft_Type,
                              Follower_UK_Wake_Cat = Follower_Legacy_Wake_Cat,
                              Follower_Recat_Wake_Cat,
                              UK6Cat_Separation_Distance = Reference_Legacy_Wake_Separation_Distance,
                              Ref_Recat_Wake_Separation_Distance = Reference_Recat_Wake_Separation_Distance,
                              Ref_ROT_Spacing_Distance = Reference_Recat_ROT_Spacing_Distance,
                              UK6Cat_Separation_Time = Reference_Legacy_Wake_Separation_Time,
                              Ref_Recat_Wake_Separation_Time = Reference_Recat_Wake_Separation_Time,
                              Ref_ROT_Spacing_Time = Reference_Recat_ROT_Spacing_Time,
                              Follower_Ass_Recat_Separation_IAS = Reference_Recat_Wake_Separation_IAS,
                              Follower_Ass_ROT_Spacing_IAS = Reference_Recat_ROT_Spacing_IAS,
                              Follower_Ass_IAS,
                              Follower_Forecast_TBS_Wind_Effect,
                              Follower_Forecast_eTBS_Wind_Effect,
                              Observed_AGI_Surface_Headwind,
                              Observed_AGI_Surface_Wind_SPD,
                              Observed_AGI_Surface_Wind_HDG,
                              UK6Cat_TBS_4DME_Wake_Separation_Distance,
                              Recat_eTBS_0DME_Wake_Separation_Distance,
                              Recat_eTBS_0DME_ROT_Spacing_Distance,
                              Recat_eTBS_0DME_All_Separation_Distance,
                              Forecast_ORD_TBS_Compression = Forecast_ORD_Compression,
                              Forecast_ORD_eTBS_Compression,
                              Recat_eTBS_4DME_Wake_Separation_Distance,
                              Recat_eTBS_4DME_ROT_Spacing_Distance,
                              Recat_eTBS_4DME_All_Separation_Distance,
                              Leader_0DME_RTT,
                              Observed_0DME_Separation_Distance,
                              Observed_1DME_Separation_Distance,
                              Observed_4DME_Separation_Distance,
                              Observed_4DME_Separation_Accuracy,
                              Leader_4DME_Time,
                              Observed_0DME_Separation_Time,
                              Observed_1DME_Separation_Time,
                              Observed_4DME_Separation_Time,
                              Observed_Follower_eTBS_IAS,
                              Observed_Follower_TBS_Wind_Effect,
                              Observed_Follower_eTBS_Wind_Effect,
                              Observed_Compression = Observed_ORD_Compression)
  
  return(Performance_Model)
                              
                              
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# CLEAR: IA Performance Model Setup
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Clear_IA_Performance_Model_Setup <- function(con, PROC_Period, PROC_Criteria){
  
  # Build SQL Query
  Query <- "DELETE FROM tbl_eTBS_Performance_Model"
  
  # Edit Query based on processing period/criteria
  if (PROC_Period == "Day"){Query <- paste0(Query, " WHERE FP_Date = '", PROC_Criteria, "'")}
  if (PROC_Period == "Month"){Query <- paste0(Query, " WHERE FP_Date LIKE '%", PROC_Criteria, "%'")}
  
  # Execute Query
  sqlQuery(con, Query)
  
}

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# POPULATE: IA Performance Model Setup
# ------------------------------------------------------------------------------------------------------------------------------------------ #



# ------------------------------------------------------------------------------------------------------------------------------------------ #

Populate_IA_Performance_Model_Setup <- function(con, IA_Performance_Model_Setup){
  
}
  

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

