### ML Task Data Prep

Dir_Path <- file.path("Intermediary Data")
ML_Dir <- file.path(Base_Dir, "ML Inputs")


# Get the TBS Speeds
TBS_Version <- "7.0-1-1"
TBS_Speeds <- Load_Output_Data("IAS Values Primary", Airfield_Dir, Dir_Path, TBS_Version)

# Get the DBS Speeds
DBS_Version <- "6.0-0-0"
DBS_Speeds <- Load_Output_Data("IAS Values Primary", Airfield_Dir, Dir_Path, DBS_Version)

con_dbs <- Get_RODBC_Database_Connection(IP = "192.168.1.39", Database = "NavCan_UTMA_Validation_DB2")
con_tbs <- Get_RODBC_Database_Connection(IP = "192.168.1.23", Database = "NavCan_TBS_V3")

lp_dbs <- sqlQuery(con_dbs, "SELECT * FROM tbl_Landing_Pair", stringsAsFactors = F)
fp_dbs <- sqlQuery(con_dbs, "SELECT Flight_Plan_ID, Time_At_1DME FROM tbl_Flight_Plan_Derived", stringsAsFactors = F)

DBS_Speeds_Alt <- left_join(DBS_Speeds, lp_dbs, by = c("Flight_Plan_ID" = "Leader_Flight_Plan_ID")) %>% select(-c("Landing_Pair_ID", "Landing_Pair_Type", "Wake_Cat", "Surface_Wind_Group")) %>%
  rename(Leader_Flight_Plan_ID = Flight_Plan_ID, Leader_Surface_Headwind_Group = Surface_Headwind_Group, Leader_Average_Speed = Ave_SPD, Leader_Aircraft_Type = Aircraft_Type)

DBS_Speeds_Join <- select(DBS_Speeds,
                          Follower_Flight_Plan_ID = Flight_Plan_ID,
                          Separation_Distance,
                          Follower_Aircraft_Type = Aircraft_Type,
                          Follower_Surface_Headwind_Group = Surface_Headwind_Group,
                          Follower_Average_Speed = Ave_SPD)

DBS_Speeds_Alt <- inner_join(DBS_Speeds_Alt, DBS_Speeds_Join, by = c("Follower_Flight_Plan_ID", "Separation_Distance")) %>%
  mutate(GSPD_Diff = Leader_Average_Speed - Follower_Average_Speed)

# Filter for Same Wind Band
# DBS_Speeds_Alt <- filter(DBS_Speeds_Alt, Leader_Surface_Headwind_Group == Follower_Surface_Headwind_Group) %>%
#   select(-Follower_Surface_Headwind_Group) %>% rename(Surface_Headwind_Group = Leader_Surface_Headwind_Group)

# Keep Only Reference Winds
Only_Reference <- F
if (Only_Reference){
  DBS_Speeds_Alt <- DBS_Speeds_Alt %>%
    filter(Surface_Headwind_Group == "(-5,5]") %>%
    select(-Surface_Headwind_Group)
}

# Filter by Time At 1DME
Max_Time_Diff <- 15 * 60
DBS_Speeds_Alt <- inner_join(DBS_Speeds_Alt, fp_dbs, by = c("Leader_Flight_Plan_ID" = "Flight_Plan_ID")) %>% rename(Leader_Time_At_1DME = Time_At_1DME)
DBS_Speeds_Alt <- inner_join(DBS_Speeds_Alt, fp_dbs, by = c("Follower_Flight_Plan_ID" = "Flight_Plan_ID")) %>% rename(Follower_Time_At_1DME = Time_At_1DME)
DBS_Speeds_Alt <- DBS_Speeds_Alt %>%
  mutate(Time_Diff = Follower_Time_At_1DME - Leader_Time_At_1DME) #%>%
  #filter(Time_Diff <= Max_Time_Diff) %>%
  #select(-c("Time_Diff", "Leader_Time_At_1DME", "Follower_Time_At_1DME"))


fwrite(DBS_Speeds_Alt, "DBS Speed Data.csv")
fwrite(TBS_Speeds, "TBS Speed Data.csv")





