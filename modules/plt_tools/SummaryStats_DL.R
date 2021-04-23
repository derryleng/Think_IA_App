# Adapted from ...\IA Capability Development\3. Phase 3 - Implementing New Processes and Tools\1. PLT tools\PLT scripts\Summary Stats\SummaryStats.R

Output_Summary_Stats <- function(dbi_con) {
  
  Make_Query_Count <- function(TABLE) {
    
    qry <- paste0("DECLARE @Correlated_FP_Count real
SELECT @Correlated_FP_Count = COUNT(*) FROM ", TABLE, " 

DECLARE @PLT_Count real
SELECT @PLT_Count = COUNT(*) FROM ", TABLE, "
WHERE CHARINDEX('Aircraft not tracked', PLT_Abnormal_Transition_Report) = 0

DECLARE @No_Base_Leg_Detected real
SET @No_Base_Leg_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('No base leg detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @No_Base_Or_Intercept_Leg_Detected real
SET @No_Base_Or_Intercept_Leg_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('No base or intercept leg detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Missed_Approach_Detected real
SET @Missed_Approach_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Missed approach detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Go_Around_Or_ILS_Circuit_Detected real
SET @Go_Around_Or_ILS_Circuit_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Go around or ILS circuit detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Abnormal_Final_Path_Leg real
SET @Abnormal_Final_Path_Leg = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Abnormal Final Path Leg', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Suspect_Runway_Switch_Detected real
SET @Suspect_Runway_Switch_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Suspect runway switch detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Normal_Runway_Switch_Detected real
SET @Normal_Runway_Switch_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Normal runway switch detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Unusual_Transition_From_Runway_Switch real
SET @Unusual_Transition_From_Runway_Switch = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Unusual transition from runway switch', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Listed_Military_Aircraft_Type real
SET @Listed_Military_Aircraft_Type = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Listed military aircraft type', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Unusual_Transition_To_Null real
SET @Unusual_Transition_To_Null = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Unusual transition to null', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @DW_To_Null real
SET @DW_To_Null = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('_DW_', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Probable_Turn_On_Inside_FAF real
SET @Probable_Turn_On_Inside_FAF = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Probable turn on inside FAF', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Probable_Low_Level_Crosser real
SET @Probable_Low_Level_Crosser = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Probable low level crosser', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Probable_Premature_Base_Or_Intercept_Detection real
SET @Probable_Premature_Base_Or_Intercept_Detection = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Probable premature base or intercept detection', PLT_Abnormal_Transition_Report) <> 0)


SELECT
Total_Correlated_FP_Count = @Correlated_FP_Count,
Total_PLT_Count = @PLT_Count,
Aircraft_Not_Tracked = @Correlated_FP_Count - @PLT_Count,
No_Base_Leg_Detected = @No_Base_Leg_Detected,
No_Base_Or_Intercept_Leg_Detected = @No_Base_Or_Intercept_Leg_Detected,
Missed_Approach_Detected = @Missed_Approach_Detected,
Go_Around_Or_ILS_Circuit_Detected = @Go_Around_Or_ILS_Circuit_Detected,
Abnormal_Final_Path_Leg = @Abnormal_Final_Path_Leg,
--Suspect_Runway_Switch_Detected = @Suspect_Runway_Switch_Detected,
--Normal_Runway_Switch_Detected = @Normal_Runway_Switch_Detected,
--Unusual_Transition_From_Runway_Switch = @Unusual_Transition_From_Runway_Switch,
Listed_Military_Aircraft_Type = @Listed_Military_Aircraft_Type,
Unusual_Transition_To_Null = @Unusual_Transition_To_Null,
DW_To_Null = @DW_To_Null,
Probable_Turn_On_Inside_FAF = @Probable_Turn_On_Inside_FAF,
Probable_Low_Level_Crosser = @Probable_Low_Level_Crosser,
Probable_Premature_Base_Or_Intercept_Detection = @Probable_Premature_Base_Or_Intercept_Detection,
Residual_Transitions_To_Null = @Unusual_Transition_To_Null - @Probable_Turn_On_Inside_FAF - @Probable_Low_Level_Crosser - @Probable_Premature_Base_Or_Intercept_Detection

")
    
    return(qry)
    
  }
  Make_Query_Percent <- function(TABLE) {
    
    qry <- paste0("DECLARE @Correlated_FP_Count real
SELECT @Correlated_FP_Count = COUNT(*) FROM ", TABLE, " 

DECLARE @PLT_Count real
SELECT @PLT_Count = COUNT(*) FROM ", TABLE, "
WHERE CHARINDEX('Aircraft not tracked', PLT_Abnormal_Transition_Report) = 0

DECLARE @No_Base_Leg_Detected real
SET @No_Base_Leg_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('No base leg detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @No_Base_Or_Intercept_Leg_Detected real
SET @No_Base_Or_Intercept_Leg_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('No base or intercept leg detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Missed_Approach_Detected real
SET @Missed_Approach_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Missed approach detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Go_Around_Or_ILS_Circuit_Detected real
SET @Go_Around_Or_ILS_Circuit_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Go around or ILS circuit detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Abnormal_Final_Path_Leg real
SET @Abnormal_Final_Path_Leg = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Abnormal Final Path Leg', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Suspect_Runway_Switch_Detected real
SET @Suspect_Runway_Switch_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Suspect runway switch detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Normal_Runway_Switch_Detected real
SET @Normal_Runway_Switch_Detected = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Normal runway switch detected', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Unusual_Transition_From_Runway_Switch real
SET @Unusual_Transition_From_Runway_Switch = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Unusual transition from runway switch', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Listed_Military_Aircraft_Type real
SET @Listed_Military_Aircraft_Type = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Listed military aircraft type', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Unusual_Transition_To_Null real
SET @Unusual_Transition_To_Null = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Unusual transition to null', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @DW_To_Null real
SET @DW_To_Null = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('_DW_', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Probable_Turn_On_Inside_FAF real
SET @Probable_Turn_On_Inside_FAF = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Probable turn on inside FAF', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Probable_Low_Level_Crosser real
SET @Probable_Low_Level_Crosser = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Probable low level crosser', PLT_Abnormal_Transition_Report) <> 0)

DECLARE @Probable_Premature_Base_Or_Intercept_Detection real
SET @Probable_Premature_Base_Or_Intercept_Detection = (SELECT COUNT(*) FROM ", TABLE, " WHERE CHARINDEX('Probable premature base or intercept detection', PLT_Abnormal_Transition_Report) <> 0)

SELECT
Total_Correlated_FP_Count = @Correlated_FP_Count,
Total_PLT_Count = @PLT_Count,
Aircraft_Not_Tracked = ((@Correlated_FP_Count - @PLT_Count) / @Correlated_FP_Count) * 100,
No_Base_Leg_Detected = (@No_Base_Leg_Detected / @PLT_Count) * 100,
No_Base_Or_Intercept_Leg_Detected = (@No_Base_Or_Intercept_Leg_Detected / @PLT_Count) * 100,
Missed_Approach_Detected = (@Missed_Approach_Detected / @PLT_Count) * 100,
Go_Around_Or_ILS_Circuit_Detected = (@Go_Around_Or_ILS_Circuit_Detected / @PLT_Count) * 100,
Abnormal_Final_Path_Leg = (@Abnormal_Final_Path_Leg / @PLT_Count) * 100,
--Suspect_Runway_Switch_Detected = (@Suspect_Runway_Switch_Detected / @PLT_Count) * 100,
--Normal_Runway_Switch_Detected = (@Normal_Runway_Switch_Detected / @PLT_Count) * 100,
--Unusual_Transition_From_Runway_Switch = (@Unusual_Transition_From_Runway_Switch / @PLT_Count) * 100,
Listed_Military_Aircraft_Type = (@Listed_Military_Aircraft_Type / @PLT_Count) * 100,
Unusual_Transition_To_Null = (@Unusual_Transition_To_Null / @PLT_Count) * 100,
DW_To_Null = (@DW_To_Null / @PLT_Count) * 100,
Probable_Turn_On_Inside_FAF = (@Probable_Turn_On_Inside_FAF / @PLT_Count) * 100,
Probable_Low_Level_Crosser = (@Probable_Low_Level_Crosser / @PLT_Count) * 100,
Probable_Premature_Base_Or_Intercept_Detection = (@Probable_Premature_Base_Or_Intercept_Detection / @PLT_Count) * 100,
Residual_Transitions_To_Null = ((@Unusual_Transition_To_Null - @Probable_Turn_On_Inside_FAF - @Probable_Low_Level_Crosser - @Probable_Premature_Base_Or_Intercept_Detection) / @PLT_Count) * 100

")
    
    return(qry)
    
  }
  
  V1_Count <- dbGetQuery(dbi_con, Make_Query_Count("tbl_PLT_Analysis_Report")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(., "Statistic") %>% rename("V1_Count" = "V1")
  
  V1_Percent <- dbGetQuery(dbi_con, Make_Query_Percent("tbl_PLT_Analysis_Report")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(., "Statistic") %>% rename("V1_Percentage" = "V1")
  
  V2_Count <- dbGetQuery(dbi_con, Make_Query_Count("tbl_PLT_Analysis_Report_2")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(., "Statistic") %>% rename("V2_Count" = "V1")
  
  V2_Percent <- dbGetQuery(dbi_con, Make_Query_Percent("tbl_PLT_Analysis_Report_2")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(., "Statistic") %>% rename("V2_Percentage" = "V1")
  
  V3_Count <- dbGetQuery(dbi_con, Make_Query_Count("tbl_PLT_Analysis_Report_3")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(., "Statistic") %>% rename("V3_Count" = "V1")
  
  V3_Percent <- dbGetQuery(dbi_con, Make_Query_Percent("tbl_PLT_Analysis_Report_3")) %>% t() %>% as.data.frame() %>% tibble::rownames_to_column(., "Statistic") %>% rename("V3_Percentage" = "V1")
  
  Output_Summary_Stats <- full_join(V1_Count, V1_Percent, by = "Statistic") %>%
    full_join(V2_Count, by = "Statistic") %>% full_join(V2_Percent, by = "Statistic") %>%
    full_join(V3_Count, by = "Statistic") %>% full_join(V3_Percent, by = "Statistic")
  
  return(Output_Summary_Stats)
}
