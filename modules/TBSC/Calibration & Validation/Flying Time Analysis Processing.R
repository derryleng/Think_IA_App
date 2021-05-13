
### Output Local Adaptation
Local_Adaptation_Parameters <- c(
  "Script Version",
  "IP Used",
  "Database",
  "Full Output Version",
  "FTA View Version",
  "Recat ROT Distance Version",
  "Primary Delivery Point",
  "Secondary Delivery Point", 
  "Use IAS Filter",
  "Minimum IAS Value",
  "Maximum IAS Value",
  "Use GSPD Filter",
  "Minimum GSPD Value",
  "Maximum GSPD Value",
  "Use Achieved Separation Filter",
  "Assumed MRS",
  "Assumed Legacy Delivery Point",
  "Separation Accuracy Regime",
  "Separation Accuracy Max (NM)",
  "Separation Accuracy Max Alt (NM)",
  "Maximum Reference Abs Surface Headwind",
  "Maximum Reference Surface Wind Speed",
  "Remove Not-In-Trail Pairs",
  "Remove Flagged Segments",
  "Speed Type Used"
)

Local_Adaptation_Values <- c(
  as.character(Script_Version),
  as.character(IP),
  as.character(Database),
  as.character(Local_Iteration_Version),
  as.character(FTA_View_Version),
  as.character(Recat_ROT_Dist_Version),
  as.character(Delivery_Primary),
  as.character(Delivery_Secondary),
  as.character(Use_Filter_IAS),
  as.character(IAS_Min),
  as.character(IAS_Max),
  as.character(Use_Filter_GSPD),
  as.character(GSPD_Min),
  as.character(GSPD_Max),
  as.character(Use_Filter_Separation_Accuracy),
  as.character(MRS),
  as.character(Legacy_Delivery),
  as.character(Sep_Accuracy_Max_Used),
  as.character(ifelse(Sep_Accuracy_Max_Used == "Standard", Sep_Accuracy_Max, Sep_Accuracy_Max_Tight)),
  as.character(ifelse(Sep_Accuracy_Max_Used == "Standard", Sep_Accuracy_Alt_Max, Sep_Accuracy_Alt_Max_Tight)),
  as.character(Ref_Wind_SHW_Max),
  as.character(Ref_Wind_SWS_Max),
  as.character(Remove_Not_In_Trail),
  as.character(Remove_Global_Flags),
  as.character(Speed_Type)
)

if (Operation == "IA PWS"){
  
  Local_Adaptation_Parameters_PWS <- c(
    "Remove Observations at Grouping Level",
    "Operator Grouping Enabled",
    "Operator ROT Distance Version",
    "Aircraft ROT Distance Version",
    "Minimum Operator Observations",
    "Minimum Aircraft Type Observations")
  Local_Adaptation_Values_PWS <- c(
    as.character(Obs_Removal_At_Level),
    as.character(Operator_Enabled),
    as.character(Operator_ROT_Dist_Version),
    as.character(Aircraft_ROT_Dist_Version),
    as.character(Min_Obs_Operator),
    as.character(Min_Obs_Aircraft))
  
  Local_Adaptation_Parameters <- append(
    Local_Adaptation_Parameters, Local_Adaptation_Parameters_PWS) 
  
  Local_Adaptation_Values <- append(
    Local_Adaptation_Values, Local_Adaptation_Values_PWS)
  
}

Local_Adaptation <- data.frame(
  Parameter = Local_Adaptation_Parameters,
  Value = Local_Adaptation_Values
)


fwrite(Local_Adaptation, file.path(Local_Iteration_Dir, paste0("Local Adaptation v", Local_Iteration_Version, ".csv")))



## Get Speeds, Times & General Summaries

#-----------------------------------------------------------------------------------------------------------#
# Pre-processing
# TO ADD: Filtering Counts/Stats, Generalise Achieved Separation Filter - Add TBS for PWS??
#-----------------------------------------------------------------------------------------------------------#

# This prepares the runway data in the required format
Runway <- Runway %>% 
  select(Runway = Runway_Name, Runway_Group, Heading) %>% mutate(Heading = Heading / deg_to_rad, Min_Rdr = MRS)

## TEMP: Manually convert Distances (Create Views!)
Recat_Wake_Dist <- mutate(Recat_Wake_Dist, Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / NM_to_m)
Recat_ROT_Dist <- mutate(Recat_ROT_Dist, Reference_ROT_Spacing_Distance = Reference_ROT_Spacing_Distance / NM_to_m)
Legacy_Wake_Dist <- mutate(Legacy_Wake_Dist, WT_Separation_Distance = WT_Separation_Distance / NM_to_m)

# Get Distance Parameters
Unique_Distances <- Get_Unique_Distances(Recat_Wake_Dist, Recat_ROT_Dist, Aircraft_Wake_Dist, Aircraft_ROT_Dist, Operator_Wake_Dist, Operator_ROT_Dist)
Displacement_Max <- max(ceiling(Unique_Distances))
Displacement_Min <- min(floor(Unique_Distances))
Half_Distances <- Get_Half_Distances(Unique_Distances)

# First remove all duplicate wind segs
FT_Data <- FTA_View %>% 
  group_by(Mode_S_Wind_Seg_ID) %>% mutate(ID = row_number()) %>% ungroup() %>% filter(ID == 1) %>% select(-ID)

# Add Surface wind and runway fields with Apply_New_Fields
FT_Data <- rename(FT_Data, Runway = Landing_Runway)
FT_Data <- left_join(FT_Data, Runway, by = c("Runway"))
FT_Data <- mutate(FT_Data, 
                  Surface_Headwind = calculate_headwind_component(Heading, Surface_Wind_HDG, Surface_Wind_SPD),
                  Surface_Headwind_Group = cut(Surface_Headwind, breaks = SHW_Groups),
                  Surface_Wind_Group = cut(Surface_Wind_SPD, breaks = SWS_Groups))

# Add Legacy Fields
FT_Data <- left_join(FT_Data, Legacy_AC_To_Wake, by=c("Leader_Aircraft_Type"="Aircraft_Type")) %>% 
  rename(Leader_Legacy_Wake_Cat=Wake)
FT_Data <- left_join(FT_Data, Legacy_AC_To_Wake, by=c("Aircraft_Type"="Aircraft_Type")) %>% 
  rename(Follower_Legacy_Wake_Cat=Wake)

FT_Data <- left_join(FT_Data, Legacy_Wake_Dist, by = c("Leader_Legacy_Wake_Cat" = "Leader_WVI", "Follower_Legacy_Wake_Cat" = "Follower_WVI"))

FT_Data <- rename(FT_Data, Reference_Legacy_Wake_Separation_Distance = WT_Separation_Distance)
FT_Data <- mutate(FT_Data, Reference_Legacy_Wake_Separation_Distance = ifelse(is.na(Reference_Legacy_Wake_Separation_Distance), MRS, Reference_Legacy_Wake_Separation_Distance))
FT_Data <- mutate(FT_Data,
                  Separation_Accuracy_0DME = (Delivered_0DME_Separation/1852) - Reference_Legacy_Wake_Separation_Distance,
                  Separation_Accuracy_1DME = (Delivered_1DME_Separation/1852) - Reference_Legacy_Wake_Separation_Distance,
                  Separation_Accuracy_4DME = (Delivered_4DME_Separation/1852) - Reference_Legacy_Wake_Separation_Distance)

FT_Data <- filter(FT_Data, !is.na(Wake_Cat), !is.na(Surface_Headwind_Group), !is.na(Surface_Wind_Group), Wake_Cat != "")

# Filter Table
Original_Size_Segs <- nrow(FT_Data)
Original_Size_AC <- Get_Single_Flight_Count(FT_Data)

Filter_Table <- data.frame(
  Filter = c("Original"),
  New_Seg_Sample_Size = c(Original_Size_Segs),
  Seg_Total_Occurences = c(NA),
  Seg_Removed_Occurences = c(0),
  New_AC_Sample_Size = c(Original_Size_AC),
  AC_Total_Occurences = c(NA),
  AC_Removed_Occurences = c(0)
)

# Initial Filters to remove Not in Trail pairs and Flagged segments
FT_Data_Filt <- FT_Data


if (Remove_Not_In_Trail){
  FT_Data_Filt_Prev <- FT_Data_Filt
  FT_Data_Ext <- filter(FT_Data, Landing_Pair_Type != "Not_In_Trail")
  FT_Data_Filt <- filter(FT_Data_Filt, Landing_Pair_Type != "Not_In_Trail")
  Filter_Table <- Add_To_Filter_Table(Filter_Table, FT_Data, FT_Data_Ext, FT_Data_Filt_Prev, FT_Data_Filt, "Remove Not In Trail Pairs")
  }
if (Remove_Global_Flags){
  FT_Data_Filt_Prev <- FT_Data_Filt
  FT_Data_Ext <- filter(FT_Data, Global_Flag == F)
  FT_Data_Filt <- filter(FT_Data_Filt, Global_Flag == F)
  Filter_Table <- Add_To_Filter_Table(Filter_Table, FT_Data, FT_Data_Ext, FT_Data_Filt_Prev, FT_Data_Filt, "Remove Flagged Segments")
}

# Filter for queued arrivals (pressured operations)
if (Use_Filter_Separation_Accuracy){
  FT_Data_Filt_Prev <- FT_Data_Filt
  if (Sep_Accuracy_Max_Used == "Standard") {
    SA_Max <- Sep_Accuracy_Max
    SA_Alt_Max <- Sep_Accuracy_Alt_Max} else {
    SA_Max <- Sep_Accuracy_Max_Tight
    SA_Alt_Max <- Sep_Accuracy_Alt_Max_Tight}
  FT_Data_Ext <- Apply_Achieved_Separation_Filter(FT_Data, Legacy_Delivery, SA_Max, SA_Alt_Max)
  FT_Data_Filt <- Apply_Achieved_Separation_Filter(FT_Data_Filt, Legacy_Delivery, SA_Max, SA_Alt_Max)
  Filter_Table <- Add_To_Filter_Table(Filter_Table, FT_Data, FT_Data_Ext, FT_Data_Filt_Prev, FT_Data_Filt, "Queued Aircraft Only")
}

# Extra filtering if necessary
if (Use_Filter_IAS){
  FT_Data_Ext <-  filter(FT_Data, Ave_Mode_S_IAS <= IAS_Max & Ave_Mode_S_IAS >= IAS_Min)
  FT_Data_Filt_Prev <- FT_Data_Filt
  FT_Data_Filt <- filter(FT_Data_Filt, Ave_Mode_S_IAS <= IAS_Max & Ave_Mode_S_IAS >= IAS_Min)
  Filter_Table <- Add_To_Filter_Table(Filter_Table, FT_Data, FT_Data_Ext, FT_Data_Filt_Prev, FT_Data_Filt, "Remove Excessive IAS Values")}
if (Use_Filter_GSPD){
  FT_Data_Ext <- filter(FT_Data, Ave_Mode_S_GSPD <= GSPD_Max & Ave_Mode_S_GSPD >= GSPD_Min)
  FT_Data_Filt_Prev <- FT_Data_Filt
  FT_Data_Filt <- filter(FT_Data_Filt, Ave_Mode_S_GSPD <= GSPD_Max & Ave_Mode_S_GSPD >= GSPD_Min)
  Filter_Table <- Add_To_Filter_Table(Filter_Table, FT_Data, FT_Data_Ext, FT_Data_Filt_Prev, FT_Data_Filt, "Remove Excessive GSPD Values")}

## Need to Add Filter Stats: Cumulative, Overall or Both? Perhaps Matrix to see evolution of sample size concerning different filters over time
Filter_Table <- Filter_Table %>%
  mutate(Cumulative_Removed_Segs = cumsum(Seg_Removed_Occurences),
         Cumulative_Removed_AC = cumsum(AC_Removed_Occurences))

fwrite(Filter_Table, file.path(Local_Iteration_Dir, paste0("Filter Stats v", Local_Iteration_Version, ".csv")))

#-----------------------------------------------------------------------------------------------------------#
# Processing
# TO ADD: Complete Remove_Level_Observations
# Update Create_seps function to allow for NA tables
# Write All Summaries
#-----------------------------------------------------------------------------------------------------------#

# Load Filtered Flying Time Analysis

# Choose What speed you want to analyse. Use IAS for LVNL
input <- FT_Data_Filt %>% Use_Speed_Parameter(Speed_Type)

# Get Average speed values across separation distances
IAS_Values_Primary <- Get_Speeds(input, Delivery_Primary, Displacement_Min, Displacement_Max, Half_Distances)
IAS_Values_Secondary <- Get_Speeds(input, Delivery_Secondary, Displacement_Min, Displacement_Max, Half_Distances)
fwrite(IAS_Values_Primary, file.path(Inter_Dir, paste0("IAS Values Primary v", Local_Iteration_Version, ".csv")))
fwrite(IAS_Values_Secondary, file.path(Inter_Dir, paste0("IAS Values Secondary v", Local_Iteration_Version, ".csv")))

# Split into different branches 
IAS_Values_Primary_SHW <- IAS_Values_Primary
IAS_Values_Secondary_SHW <- IAS_Values_Secondary
IAS_Values_Primary_SWS <- IAS_Values_Primary
IAS_Values_Secondary_SWS <- IAS_Values_Secondary

if (Operation == "IA PWS" & Operator_Enabled){
  Summary_Primary_SHW_Operator <- Create_Summary_Main(IAS_Values_Primary, "Aircraft_Type", "Operator", NA, "Surface_Headwind_Group", Delivery_Primary)
  Summary_Secondary_SHW_Operator <- Create_Summary_Main(IAS_Values_Secondary, "Aircraft_Type", "Operator", NA, "Surface_Headwind_Group", Delivery_Secondary)
  Summary_Primary_SWS_Operator <- Create_Summary_Main(IAS_Values_Primary, "Aircraft_Type", "Operator", NA, "Surface_Wind_Group", Delivery_Primary)
  Summary_Secondary_SWS_Operator <- Create_Summary_Main(IAS_Values_Secondary, "Aircraft_Type", "Operator", NA, "Surface_Wind_Group", Delivery_Secondary)
  
  if (Obs_Removal_At_Level){
    IAS_Values_Primary_SHW <- Remove_Level_Observations(IAS_Values_Primary_SHW, Summary_Primary_SHW_Operator, Min_Obs_Operator)
    IAS_Values_Secondary_SHW <- Remove_Level_Observations(IAS_Values_Secondary_SHW, Summary_Secondary_SHW_Operator, Min_Obs_Operator)
    IAS_Values_Primary_SWS <- Remove_Level_Observations(IAS_Values_Primary_SWS, Summary_Primary_SWS_Operator, Min_Obs_Operator)
    IAS_Values_Secondary_SWS <- Remove_Level_Observations(IAS_Values_Secondary_SWS, Summary_Secondary_SWS_Operator, Min_Obs_Operator)
  }
  
  Separations_Primary_SHW_Operator <- Create_seps(Summary_Primary_SHW_Operator, Operator_Wake_Dist, Operator_ROT_Dist, Runway, "Operator", "Surface_Headwind_Group", Ref_Wind_SHW_Max)
  Separations_Secondary_SHW_Operator <- Create_seps(Summary_Secondary_SHW_Operator, Operator_Wake_Dist, Operator_ROT_Dist, Runway, "Operator", "Surface_Headwind_Group", Ref_Wind_SHW_Max)
  Separations_Primary_SWS_Operator <- Create_seps(Summary_Primary_SWS_Operator, Operator_Wake_Dist, Operator_ROT_Dist, Runway, "Operator", "Surface_Wind_Group", Ref_Wind_SWS_Max)
  Separations_Secondary_SWS_Operator <- Create_seps(Summary_Secondary_SWS_Operator, Operator_Wake_Dist, Operator_ROT_Dist, Runway, "Operator", "Surface_Wind_Group", Ref_Wind_SWS_Max)
  
}

if (Operation == "IA PWS"){
  Summary_Primary_SHW_Aircraft <- Create_Summary_Main(IAS_Values_Primary_SHW, "Aircraft_Type", NA, NA, "Surface_Headwind_Group", Delivery_Primary)
  Summary_Secondary_SHW_Aircraft <- Create_Summary_Main(IAS_Values_Secondary_SHW, "Aircraft_Type", NA, NA, "Surface_Headwind_Group", Delivery_Secondary)
  Summary_Primary_SWS_Aircraft <- Create_Summary_Main(IAS_Values_Primary_SWS, "Aircraft_Type", NA, NA, "Surface_Wind_Group", Delivery_Primary)
  Summary_Secondary_SWS_Aircraft <- Create_Summary_Main(IAS_Values_Secondary_SWS, "Aircraft_Type", NA, NA, "Surface_Wind_Group", Delivery_Secondary)
  
  if (Obs_Removal_At_Level){
    IAS_Values_Primary_SHW <- Remove_Level_Observations(IAS_Values_Primary_SHW, Summary_Primary_SHW_Aircraft, Min_Obs_Aircraft)
    IAS_Values_Secondary_SHW <- Remove_Level_Observations(IAS_Values_Secondary_SHW, Summary_Secondary_SHW_Aircraft, Min_Obs_Aircraft)
    IAS_Values_Primary_SWS <- Remove_Level_Observations(IAS_Values_Primary_SWS, Summary_Primary_SWS_Aircraft, Min_Obs_Aircraft)
    IAS_Values_Secondary_SWS <- Remove_Level_Observations(IAS_Values_Secondary_SWS, Summary_Secondary_SWS_Aircraft, Min_Obs_Aircraft)
  }
  
  Separations_Primary_SHW_Aircraft <- Create_seps(Summary_Primary_SHW_Aircraft, Aircraft_Wake_Dist, Aircraft_ROT_Dist, Runway, "Aircraft", "Surface_Headwind_Group", Ref_Wind_SHW_Max)
  Separations_Secondary_SHW_Aircraft <- Create_seps(Summary_Secondary_SHW_Aircraft, Aircraft_Wake_Dist, Aircraft_ROT_Dist, Runway, "Aircraft", "Surface_Headwind_Group", Ref_Wind_SHW_Max)
  Separations_Primary_SWS_Aircraft <- Create_seps(Summary_Primary_SWS_Aircraft, Aircraft_Wake_Dist, Aircraft_ROT_Dist, Runway, "Aircraft", "Surface_Wind_Group", Ref_Wind_SWS_Max)
  Separations_Secondary_SWS_Aircraft <- Create_seps(Summary_Secondary_SWS_Aircraft, Aircraft_Wake_Dist, Aircraft_ROT_Dist, Runway, "Aircraft", "Surface_Wind_Group", Ref_Wind_SWS_Max)
  
}

Summary_Primary_SHW_Wake <- Create_Summary_Main(IAS_Values_Primary_SHW, "Wake_Cat", NA, NA, "Surface_Headwind_Group", Delivery_Primary)
Summary_Secondary_SHW_Wake <- Create_Summary_Main(IAS_Values_Secondary_SHW, "Wake_Cat", NA, NA, "Surface_Headwind_Group", Delivery_Secondary)
Summary_Primary_SWS_Wake <- Create_Summary_Main(IAS_Values_Primary_SWS, "Wake_Cat", NA, NA, "Surface_Wind_Group", Delivery_Primary)
Summary_Secondary_SWS_Wake <- Create_Summary_Main(IAS_Values_Secondary_SWS, "Wake_Cat", NA, NA, "Surface_Wind_Group", Delivery_Secondary)

fwrite(Summary_Primary_SHW_Wake, file.path(Inter_Dir, paste0("FTA Summary Primary SHW (Recat) v", Local_Iteration_Version, ".csv")))
fwrite(Summary_Secondary_SHW_Wake, file.path(Inter_Dir, paste0("FTA Summary Secondary SHW (Recat) v", Local_Iteration_Version, ".csv")))
fwrite(Summary_Primary_SWS_Wake, file.path(Inter_Dir, paste0("FTA Summary Primary SWS (Recat) v", Local_Iteration_Version, ".csv")))
fwrite(Summary_Secondary_SWS_Wake, file.path(Inter_Dir, paste0("FTA Summary Secondary SWS (Recat) v", Local_Iteration_Version, ".csv")))

Separations_Primary_SHW_Wake <- Create_seps(Summary_Primary_SHW_Wake, Recat_Wake_Dist, Recat_ROT_Dist, Runway, "Wake", "Surface_Headwind_Group", Ref_Wind_SHW_Max)
Separations_Secondary_SHW_Wake <- Create_seps(Summary_Secondary_SHW_Wake, Recat_Wake_Dist, Recat_ROT_Dist, Runway, "Wake", "Surface_Headwind_Group", Ref_Wind_SHW_Max)
Separations_Primary_SWS_Wake <- Create_seps(Summary_Primary_SWS_Wake, Recat_Wake_Dist, Recat_ROT_Dist, Runway, "Wake", "Surface_Wind_Group", Ref_Wind_SWS_Max)
Separations_Secondary_SWS_Wake <- Create_seps(Summary_Secondary_SWS_Wake, Recat_Wake_Dist, Recat_ROT_Dist, Runway, "Wake", "Surface_Wind_Group", Ref_Wind_SWS_Max)

fwrite(Separations_Primary_SHW_Wake, file.path(Inter_Dir, paste0("FTA Separations Primary SHW (Recat) v", Local_Iteration_Version, ".csv")))
fwrite(Separations_Secondary_SHW_Wake, file.path(Inter_Dir, paste0("FTA Separations Secondary SHW (Recat) v", Local_Iteration_Version, ".csv")))
fwrite(Separations_Primary_SWS_Wake, file.path(Inter_Dir, paste0("FTA Separations Primary SWS (Recat) v", Local_Iteration_Version, ".csv")))
fwrite(Separations_Secondary_SWS_Wake, file.path(Inter_Dir, paste0("FTA Separations Secondary SWS (Recat) v", Local_Iteration_Version, ".csv")))

## Analysis for Primary/Secondary and SHW/SWS
Separations1 <- select(Separations_Primary_SHW_Wake, 
                       Leader_WTC, Follower_WTC, Separation_Distance,
                       Count_Primary_SHW = Count,
                       Speed_Primary_SHW = Median_Speed,
                       Time_Primary_SHW = Flying_Time) %>% unique()

Separations2 <- select(Separations_Primary_SWS_Wake, 
                       Leader_WTC, Follower_WTC, Separation_Distance,
                       Count_Primary_SWS = Count,
                       Speed_Primary_SWS = Median_Speed,
                       Time_Primary_SWS = Flying_Time) %>% unique()

Separations3 <- select(Separations_Secondary_SHW_Wake,
                       Leader_WTC, Follower_WTC, Separation_Distance,
                       Count_Secondary_SHW = Count,
                       Speed_Secondary_SHW = Median_Speed,
                       Time_Secondary_SHW = Flying_Time) %>% unique()

Separations4 <- select(Separations_Secondary_SWS_Wake,
                       Leader_WTC, Follower_WTC, Separation_Distance,
                       Count_Secondary_SWS = Count,
                       Speed_Secondary_SWS = Median_Speed,
                       Time_Secondary_SWS = Flying_Time) %>% unique()

Separations_Compare <- Separations1 %>%
  full_join(Separations2, by = c("Leader_WTC", "Follower_WTC", "Separation_Distance")) %>%
  full_join(Separations3, by = c("Leader_WTC", "Follower_WTC", "Separation_Distance")) %>%
  full_join(Separations4, by = c("Leader_WTC", "Follower_WTC", "Separation_Distance"))

Separations_Compare_Count <- select(Separations_Compare,
                                    Leader_WTC, Follower_WTC, Separation_Distance,
                                    Count_Primary_SWS,
                                    Count_Secondary_SWS,
                                    Count_Primary_SHW,
                                    Count_Secondary_SHW
                                    )

Separations_Compare_Primary <- select(Separations_Compare,
                                      Leader_WTC, Follower_WTC, Separation_Distance,
                                      Count_Primary_SWS,
                                      Count_Primary_SHW,
                                      Speed_Primary_SWS,
                                      Speed_Primary_SHW,
                                      Time_Primary_SWS,
                                      Time_Primary_SHW) %>%
  mutate(Count_Diff = Count_Primary_SWS - Count_Primary_SHW,
         Speed_Diff = round((Speed_Primary_SWS - Speed_Primary_SHW), 0),
         Time_Diff = round((Time_Primary_SWS - Time_Primary_SHW), 0))


Separations_Compare_SHW <- select(Separations_Compare,
                                      Leader_WTC, Follower_WTC, Separation_Distance,
                                      Count_Primary_SHW,
                                      Count_Secondary_SHW,
                                      Speed_Primary_SHW,
                                      Speed_Secondary_SHW,
                                      Time_Primary_SHW,
                                      Time_Secondary_SHW) %>%
  mutate(Count_Diff = Count_Primary_SHW - Count_Secondary_SHW,
         Speed_Diff = round((Speed_Primary_SHW - Speed_Secondary_SHW), 0),
         Time_Diff = round((Time_Primary_SHW - Time_Secondary_SHW), 0))

fwrite(Separations_Compare, file.path(Initial_Compare_Dir, paste0("Separations Comparison Full v", Local_Iteration_Version, ".csv")))
fwrite(Separations_Compare_Count, file.path(Initial_Compare_Dir, paste0("Separations Comparison Counts v", Local_Iteration_Version, ".csv")))
fwrite(Separations_Compare_Primary, file.path(Initial_Compare_Dir, paste0("Separations Comparison Primary (SHW & SWS) v", Local_Iteration_Version, ".csv")))
fwrite(Separations_Compare_Primary, file.path(Initial_Compare_Dir, paste0("Separations Comparison SHW (P & S) v", Local_Iteration_Version, ".csv")))

