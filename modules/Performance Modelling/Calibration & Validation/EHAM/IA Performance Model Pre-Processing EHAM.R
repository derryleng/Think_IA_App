# ----------------------------------------- #
# IA Performance Model: Pre-Processing
# ----------------------------------------- #

# ----------------------------------------- #
# This script is tobe used before the main performance model outputs are generated
# This will take SQL qind segment data, GWCS data and Performance model sql outputs
# This script will conform the performance model SQL outputs to IA and adds an option to bolster data.
# ----------------------------------------- #

# ----------------------------------------- #
# 0. Imports
# ----------------------------------------- #
library(data.table)
library(dplyr)
library(officer)
library(ggplot2)
library(gridExtra)
library(RODBC)

# ----------------------------------------- #
# 1. Config
# ----------------------------------------- #

# ------- Script version config

# Script version
version <- "v1.0.6"

# User to access the GWCS data
#user <- "George Clark"
user <- "Michael Cowham"


# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Generate sub-directory in Output folder for this run
outdir <- file.path("Pre-Process Output", version)
dir.create(outdir)

# ------- Database config

# Database configuration
db_ip <- "192.168.1.39"
db_name <- "LVNL_UTMA_Validation"

# Get database connection
con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, db_name, "ruser", "Th!nkruser"
))

# ------- Local/Online Config

# Local use of main datasets (Performance model, Wind segments)
local_main <- F

# Local use of Adaptation files
local_adap <- F

# Save local versions of main/adap files
save_pm <- F # Performance Model SQL Output
save_segs <- F # Segment Data SQL Output
save_adap <- F # Adaptation SQL Output

# Local file/directory config

# Folder for local versions of main datasets (segments and PM)
ref_folder <- "LVNL Exports 2020-10-05"
refdir <- file.path("Reference", ref_folder)

# Folder for local versions of Adaptation
adap_folder <- file.path("Pre-Process Input")

# file names for main datasets 
pm_csv <- "pm.csv"
segs_csv <- "pm_segment_data.csv"

# file names for adaptation
ref_dist_csv <- "Reference_Recat_Separation_Dist.csv"
ref_times_csv <- "Reference_Recat_Separation_Time.csv"
ref_speeds_csv <- "Assumed_Recat_Separation_IAS.csv"
legacy_dist_csv <- "Reference_Legacy_Separation_Dist.csv"
ref_actowake_csv <- "Recat_Aircraft_Type_To_Wake.csv"
legacy_actowake_csv <- "Legacy_Aircraft_Type_To_Wake.csv"
landing_pair_csv <- "Landing_Pair_Data.csv"

# ------- Settings

# Flag for allowing the bolstering of ROT values
bolster_rot <- T

# Flag for allowing the bolstering of small sample sizes
bolster_data <- T

# Bolstering set to manual (T) or automatic (F)
manual_bolstering <- T

# Flag for the use of Wake Pairs only
only_wake_pairs <- F

# Flag for use of Average IAS filtering
use_ias_filter <- F

# Flag for use of Achieved Separation Filter
use_achieved_sep <- F

# ----------------------------------------- #
# 2. Adaptable Parameters
# ----------------------------------------- #

# ------- Wake Cats
wake_cats <- as.data.table(c("A", "B", "C", "D", "E", "F"))
names(wake_cats) <- c("Wake")

# ------- Bolstering

# Max number of total observations taken for a bolstered sample
max_obs <- 1000

# Manual bolstering: Leaders & Followers
leader_list <- c("A")
follower_list <- c("F")

# Automatic bolstering: max observations to be considered (automatic bolstering not yet available)
bolster_cutoff <- 50

# ------- Filtering

# Filter toonly use specific dates
date_filter <- NA

# min/max values of IAS for IAS filter
min_ias <- 50
max_ias <- 200

# ------- Capping

# Minimum value for RECAT Wake TBS 0DME Separation
wake_cap_0 <- 3

# Minimum value for RECAT Wake TBS 4DME Separation
wake_cap_4 <- 3

# Minimum value for RECAT ROT TBS 0DME Separation
rot_cap_0 <- 3

# Minimum value for RECAT ROT TBS 4DME Separation
rot_cap_4 <- 3

# Minimum value for forecast ORD compression
comp_cap <- 0

# Minimum radar separation by law
radar_min_sep <- 3

# ----------------------------------------- #
# 3. Data Loading
# ----------------------------------------- #

# ------- Reference

# --- FROM SQL

if (local_adap == F){

# Get reference Wake data
ref_dist <- sqlQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Dist", stringsAsFactors = F) %>% mutate(Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852)
ref_speeds <- sqlQuery(con, "SELECT * FROM tbl_Assumed_Recat_Separation_IAS", stringsAsFactors = F) %>% mutate(Assumed_Wake_Separation_IAS = Assumed_Wake_Separation_IAS/1852*3600)
ref_times <- sqlQuery(con, "SELECT * FROM tbl_Reference_Recat_Separation_Time", stringsAsFactors = F)

# Get reference ROT data
ref_rot_dist <- sqlQuery(con, "SELECT * FROM tbl_Reference_ROT_Spacing_Dist", stringsAsFactors = F) %>% mutate(Reference_ROT_Spacing_Distance = Reference_ROT_Spacing_Distance / 1852)
ref_rot_speeds <- sqlQuery(con, "SELECT * FROM tbl_Assumed_ROT_Spacing_IAS", stringsAsFactors = F) %>% mutate(Assumed_ROT_Spacing_IAS = Assumed_ROT_Spacing_IAS/1852*3600)
ref_rot_times <- sqlQuery(con, "SELECT * FROM tbl_Reference_ROT_Spacing_Time", stringsAsFactors = F)

#ref_rot_dist <- fread(file.path(adap_folder, "tmp_rot_distance.csv"))
#ref_rot_speeds <- fread(file.path(adap_folder, "tmp_rot_speed.csv"))
#ref_rot_times <- fread(file.path(adap_folder, "tmp_rot_time.csv"))

# Get landing pair data for Bolstering process
lp_string <- "SELECT lp.Landing_Pair_ID, lp.Leader_Flight_Plan_ID, lp.Follower_Flight_Plan_ID, FPDL.Time_At_4DME AS Leader_Time_At_4DME,
              FPDF.Time_At_4DME AS Follower_Time_At_4DME FROM tbl_Landing_Pair lp 
              LEFT JOIN tbl_Flight_Plan_Derived FPDL 
              ON lp.Leader_Flight_Plan_ID = FPDL.Flight_Plan_ID
              LEFT JOIN tbl_Flight_Plan_Derived FPDF
              ON lp.Follower_Flight_Plan_ID = FPDF.Flight_Plan_ID"
landing_pair_times <- sqlQuery(con, lp_string, stringsAsFactors = F)

# Get the ICAO4 Wake category pair distances
dbs_wake_turbulence <- sqlQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence", stringsAsFactors = F)

# Get the ICAO4 Aircraft type to wake mapping
aircraft_type_to_wake_legacy <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy", stringsAsFactors = F) %>% unique()

# Get the RECAT Aircraft Type to Wake for matching with Heathrow
aircraft_type_to_wake <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake", stringsAsFactors = F) %>% unique() %>% select(-c("Aircraft_Class"))

}

# --- FROM LOCAL

if (local_adap == T){
  
  # Get reference recat wake data
  ref_dist <- fread(file.path(adap_folder, ref_dist_csv))
  ref_speeds <- fread(file.path(adap_folder, ref_speeds_csv))
  ref_times <- fread(file.path(adap_folder, ref_times_csv))
  
  # Get landing pair data
  landing_pair_times <- fread(file.path(adap_folder, landing_pair_csv))
  
  # Get Legacy wake data
  dbs_wake_turbulence <- fread(file.path(adap_folder, legacy_dist_csv))
  
  # Get AC type to Wake mappings (RECAT / Legacy)
  aircraft_type_to_wake <- fread(file.path(adap_folder, ref_actowake_csv))
  aircraft_type_to_wake_legacy <- fread(file.path(adap_folder, legacy_actowake_csv))
}

# --- SAVE TO LOCAL

if (save_adap){
  fwrite(ref_dist, file.path(adap_folder, ref_dist_csv))
  fwrite(ref_speeds, file.path(adap_folder, ref_speeds_csv))
  fwrite(ref_times, file.path(adap_folder, ref_times_csv))
  fwrite(landing_pair_times, file.path(adap_folder, landing_pair_csv))
  fwrite(dbs_wake_turbulence, file.path(adap_folder, legacy_dist_csv))
  fwrite(aircraft_type_to_wake, file.path(adap_folder, ref_actowake_csv))
  fwrite(aircraft_type_to_wake_legacy, file.path(adap_folder, legacy_actowake_csv))
}

# ------- Main Datasets

# -------------------------- Performance Model -------------------------- #

# 1.4 Get the Performance Model from SQL
# Note: We want to add Leader_Flight_Plan_ID, Follower_Flight_Plan_ID, Leader Runway, Follower Runway to PM View

pm_query_str <- "SELECT * FROM vw_eTBS_Performance_Model"

tryCatch(
  if (!local_main) {
    if (!is.na(date_filter)) {
      pm <- sqlQuery(con, sprintf("%s WHERE FP_Date IN ('%s')", pm_query_str, paste(date_filter, collapse = "','")))
    } else {
      pm <- sqlQuery(con, pm_query_str)
    }
  } else {
    pm <- fread(file.path(refdir, pm_csv))
  },
  error = function(e) stop("Performance Model not found!"))

# Set up Original copy of performance model and save if necessary
pm_orig <- pm

if (save_pm) {
  fwrite(pm_orig, file.path(refdir, pm_csv))
}

# -------------------------- GWCS DATA -------------------------- #

# Load in the 6 GWCS data sets
for (sepdist in 3:8){
  gwcs_file <-  paste("C:\\Users\\", user, "\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\3. GWCS\\LVNL Data\\vw_Mode_S_Wind_Forecast_", sepdist, "nm.csv", sep = "")
  gwcs_dist <- fread(file = gwcs_file, na.strings = c("NA", "NULL") )
  
  if (sepdist == 3){
    gwcs_data <- gwcs_dist
  } else {
    gwcs_data <- rbind(gwcs_data, gwcs_dist)
  }
  rm(gwcs_dist)
}


gwcs_data <- mutate(gwcs_data,  Sep_Dist = Forecast_Seg_Max - Forecast_Seg_Min + 1, 
                    qc_flag = ifelse(Observed_Max_RTT >= (Forecast_Seg_Min + Sep_Dist - 2) & Observed_Flying_Time >= (Sep_Dist * 20 - 40) & abs(Observed_Ave_Mode_S_GSPD - Observed_Track_GSPD) <= 20, 1, 0))

# Extract the relevant fields from the GWCS data.  Date, Callsign, Sep Distance, GWCS Forecast Value, Anemo Speed and Anemo Heading 

gwcs_merge <- select(gwcs_data, FP_Date, Time_At_4DME, Callsign, Forecast_Wind_Effect_IAS, Sep_Dist)
#gwcs_merge <- select(gwcs_data, FP_Date, Callsign, Forecast_Wind_Effect_IAS, Sep_Dist)

# -------------------------- Segment DATA -------------------------- #

# ----------------------------------------- #
# 4. Initial Cleaning/Filtering
# ----------------------------------------- #

# Reload the original Performance Model before being pre-processed
pm <- pm_orig

# Remove all SQL columns that have no relevance
pm <- select(pm, -c("Leader_UK_Wake_Cat", "Follower_UK_Wake_Cat", "UK6Cat_Separation_Distance", "UK6Cat_Separation_Time",
                    "Follower_Forecast_TBS_Wind_Effect", "UK6Cat_TBS_4DME_Wake_Separation_Distance", "Forecast_ORD_eTBS_Compression", 
                    "Observed_4DME_Separation_Accuracy"))

# Filter out data that have no wind segments
#pm <- filter(pm, Landing_Pair_ID %in% unique(test_segs$Landing_Pair_ID))

# Join on Landing Pair Data to get Leader/Follower Flight Plan ID and Time At 4DME for GWCS joins
pm <- left_join(pm, landing_pair_times, by=c("Landing_Pair_ID"))

# Remove NA values where there is no forecast compressions
pre_pm_nrow <- nrow(pm)
pm <- pm[!is.na(pm$Forecast_ORD_TBS_Compression),]
post_pm_nrow <- nrow(pm)
message("Removed ", pre_pm_nrow - post_pm_nrow, " out of ", pre_pm_nrow, " performance model rows with NA Forecast_ORD_TBS_Compression.")

# Remove all negative Observed 1DME Separation Time - go arounds
pre_pm_nrow <- nrow(pm)
pm <- pm[pm$Observed_1DME_Separation_Time > 0,]
post_pm_nrow <- nrow(pm)
message("Removed ", pre_pm_nrow - post_pm_nrow, " out of ", pre_pm_nrow, " performance model rows with Observed_1DME_Separation_Time <= 0.")

# Remove all non-wake pairs if only_wake_pairs is true
if (only_wake_pairs == T){
  pre_pm_nrow <- nrow(pm)
  pm <- pm[!is.na(pm$Ref_Recat_Wake_Separation_Distance),]
  post_pm_nrow <- nrow(pm)
  message("Removed ", pre_pm_nrow - post_pm_nrow, " out of ", pre_pm_nrow, " performance model rows with NA Ref_Recat_Wake_Separation_Distance")
}

# Apply IAS filter if use_ias_filter set to TRUE (will likely move to main PM script as only use of seg data here)
if (use_ias_filter){
  av_segs <- filter(test_segs, DME_Seg > (max_ias_filter_dme - 1)) %>% group_by(Landing_Pair_ID) %>% 
    summarise(Leader_Ave_Flight_IAS = mean(Leader_Average_IAS, na.rm=T),
              Follower_Ave_Flight_IAS = mean(Follower_Average_IAS, na.rm=T)) %>% ungroup()
  
  pm2 <- left_join(pm2, av_segs, by=c("Landing_Pair_ID")) %>% filter(Leader_Ave_Flight_IAS >= min_ias & Leader_Ave_Flight_IAS <= max_ias &
                                                                       Follower_Ave_Flight_IAS >= min_ias & Follower_Ave_Flight_IAS <= max_ias)
}

# ----------------------------------------------------------------------- #
# 5. Initial Pre-Processing
# ----------------------------------------------------------------------- #

# Create a flag for whether real or bolstered data, the same for ROT bolsters
pm <- mutate(pm, Bolster_Flag = 0)
pm <- mutate(pm, ROT_Bolster_Flag = 0)
pm <- mutate(pm, ROT_Other_Bolster_Flag = 0)

# Rename Columns for consistency. 0DME TBS distance columns are "uncapped" and will be capped next.
pm <- rename(pm, 
             Recat_DBS_Wake_Separation_Distance = Ref_Recat_Wake_Separation_Distance,
             Recat_DBS_ROT_Spacing_Distance = Ref_ROT_Spacing_Distance,
             Recat_TBS_0DME_Wake_Separation_Distance_Uncapped = Recat_eTBS_0DME_Wake_Separation_Distance,
             Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped = Recat_eTBS_0DME_ROT_Spacing_Distance,
             Recat_TBS_0DME_All_Separation_Distance_Uncapped = Recat_eTBS_0DME_All_Separation_Distance,
             Recat_TBS_4DME_Wake_Separation_Distance_Uncapped = Recat_eTBS_4DME_Wake_Separation_Distance,
             Recat_TBS_4DME_ROT_Spacing_Distance_Uncapped = Recat_eTBS_4DME_ROT_Spacing_Distance,
             Recat_TBS_4DME_All_Separation_Distance_Uncapped = Recat_eTBS_4DME_All_Separation_Distance,
             Forecast_ORD_Compression_Uncapped = Forecast_ORD_TBS_Compression,
             Recat_TBS_Wake_Separation_Time = Ref_Recat_Wake_Separation_Time,
             Recat_TBS_ROT_Spacing_Time = Ref_ROT_Spacing_Time,
             Recat_TBS_Wake_Separation_IAS = Follower_Ass_Recat_Separation_IAS,
             Recat_TBS_ROT_Spacing_IAS = Follower_Ass_ROT_Spacing_IAS,
             Follower_Forecast_IA_Wind_Effect = Follower_Forecast_eTBS_Wind_Effect
)

# Option to remove Uncapped 4DME columns as they are not used
pm <- select(pm, -c("Recat_TBS_4DME_Wake_Separation_Distance_Uncapped", "Recat_TBS_4DME_ROT_Spacing_Distance_Uncapped", 
                "Recat_TBS_4DME_All_Separation_Distance_Uncapped"))

# ----------------------------------------------------------------------- #
# 6a. Pre-Processing for Bolstered ROT Data
# ----------------------------------------------------------------------- #

# Get unique ROT pairs & distances
rot <- inner_join(ref_rot_dist, ref_rot_times, by=c("Leader_WTC", "Follower_WTC", "Runway")) %>%
  inner_join(ref_rot_speeds, by=c("Leader_WTC", "Follower_WTC", "Runway"))
rot_unique_dist <- group_by(rot, Leader_WTC, Follower_WTC, Reference_ROT_Spacing_Distance, Reference_ROT_Spacing_Time,
                            Assumed_ROT_Spacing_IAS) %>% summarise() %>% ungroup()

if (bolster_rot){

# Get Wake/MRS versions of all pairs with ROT constraints
pm_rotwake <- filter(pm, !is.na(Recat_TBS_0DME_Wake_Separation_Distance_Uncapped) & !is.na(Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped))
pm_rotwake <- mutate(pm_rotwake, 
                     Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped = NA,
                     ROT_Other_Bolster_Flag = 1,
                     ROT_Bolster_Flag = 1,
                     Recat_DBS_ROT_Spacing_Distance = NA)
pm_justrot <- filter(pm, is.na(Recat_TBS_0DME_Wake_Separation_Distance_Uncapped) & !is.na(Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped))
pm_justrot <- mutate(pm_justrot, 
                     Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped = NA,
                     Recat_TBS_0DME_Wake_Separation_Distance_Uncapped = 3,
                     ROT_Other_Bolster_Flag = 1,
                     ROT_Bolster_Flag = 1,
                     Recat_DBS_ROT_Spacing_Distance = NA)

# Bind these two datasets
pm_rotother <- rbind(pm_rotwake, pm_justrot)

# Set the All Separation Distance for these cases to be the Wake Distance
pm_rotother <- mutate(pm_rotother, Recat_TBS_0DME_All_Separation_Distance_Uncapped = Recat_TBS_0DME_Wake_Separation_Distance_Uncapped)
  
# Get ROT bolstered data set
for (i in 1:nrow(rot_unique_dist)){
  lead <- rot_unique_dist$Leader_WTC[i]
  foll <- rot_unique_dist$Follower_WTC[i]
  dist <- rot_unique_dist$Reference_ROT_Spacing_Distance[i]
  time <- rot_unique_dist$Reference_ROT_Spacing_Time[i]
  speed <- rot_unique_dist$Assumed_ROT_Spacing_IAS[i]
  #pm_pair <- filter(pm, Leader_Recat_Wake_Cat == lead & Follower_Recat_Wake_Cat == foll)
  pm_pair <- filter(pm, Leader_Recat_Wake_Cat == lead & Follower_Recat_Wake_Cat == foll & Recat_DBS_ROT_Spacing_Distance != dist &
                      !is.na(Recat_DBS_ROT_Spacing_Distance))
  pm_pair <- mutate(pm_pair, 
                    Recat_DBS_ROT_Spacing_Distance = dist,
                    Recat_TBS_ROT_Spacing_Time = time,
                    Recat_TBS_ROT_Spacing_IAS = speed,
                    ROT_Bolster_Flag = 1,
                    Recat_TBS_0DME_Wake_Separation_Distance_Uncapped = NA
                    #Recat_TBS_4DME_Wake_Separation_Distance_Uncapped = NA
                    )
  if (i != 1){pm_rot <- rbind(pm_rot, pm_pair)} else {pm_rot <- pm_pair}
}

# Create Floor and Ceiling Distances so we can take averages of half mile GWCS
pm_rot <- mutate(pm_rot, Recat_DBS_ROT_Spacing_Distance_Floor = floor(Recat_DBS_ROT_Spacing_Distance),
                         Recat_DBS_ROT_Spacing_Distance_Ceiling = ceiling(Recat_DBS_ROT_Spacing_Distance))

# Remove Wind Effect field and repopulate with the GWCS data
pm_rot <- select(pm_rot, -c("Follower_Forecast_IA_Wind_Effect"))
pm_rot <- left_join(pm_rot, gwcs_merge, by=c("FP_Date", "Follower_Callsign"="Callsign",
                                                    "Recat_DBS_ROT_Spacing_Distance_Floor"="Sep_Dist",
                                                    "Follower_Time_At_4DME"="Time_At_4DME")) %>% 
  rename(Follower_Forecast_IA_Wind_Effect_Floor = Forecast_Wind_Effect_IAS)
pm_rot <- left_join(pm_rot, gwcs_merge, by=c("FP_Date", "Follower_Callsign"="Callsign",
                                             "Recat_DBS_ROT_Spacing_Distance_Ceiling"="Sep_Dist",
                                             "Follower_Time_At_4DME"="Time_At_4DME")) %>% 
  rename(Follower_Forecast_IA_Wind_Effect_Ceiling = Forecast_Wind_Effect_IAS)

pm_rot <- mutate(pm_rot, Follower_Forecast_IA_Wind_Effect = (Follower_Forecast_IA_Wind_Effect_Floor + Follower_Forecast_IA_Wind_Effect_Ceiling)/2) %>%
  select(-c("Follower_Forecast_IA_Wind_Effect_Floor", "Follower_Forecast_IA_Wind_Effect_Ceiling",
            "Recat_DBS_ROT_Spacing_Distance_Ceiling", "Recat_DBS_ROT_Spacing_Distance_Floor"))

# Get the Assumed follower GSPD
pm_rot <- mutate(pm_rot, Follower_Reference_GSPD = Follower_Forecast_IA_Wind_Effect + Recat_TBS_ROT_Spacing_IAS)

# Get the Time based IA 0DME distance separation
pm_rot <- mutate(pm_rot, Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped = Follower_Reference_GSPD * Recat_TBS_ROT_Spacing_Time / 3600)

# Remove any other fields so that columns match between two pm sets
pm_rot <- select(pm_rot, -c("Follower_Reference_GSPD"))


# Set All Separation Values to ROT values
pm_rot <- mutate(pm_rot, Recat_TBS_0DME_All_Separation_Distance_Uncapped = Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped)
#pm_rot <- mutate(pm_rot, Recat_TBS_0DME_All_Separation_Distance_Uncapped = ifelse(is.na(Recat_TBS_0DME_Wake_Separation_Distance_Uncapped),
#                                                                                  Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped,
#                                                                                  ifelse(Recat_TBS_0DME_Wake_Separation_Distance_Uncapped <= 
#                                                                                           Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped,
#                                                                                         Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped,
#                                                                                         Recat_TBS_0DME_Wake_Separation_Distance_Uncapped)))

# Bind the Wake/MRS only sample bolsters for ROT pairs
pm_rot <- rbind(pm_rot, pm_rotother)

# Bind the original PM and ROT bolster together
pm <- rbind(pm, pm_rot)
            
}


# ----------------------------------------------------------------------- #
# 6b. Pre-Processing for Bolstered Data
# ----------------------------------------------------------------------- #

if (bolster_data){

# First remove all the ROT bolsters if there are any  
pm1 <- filter(pm, ROT_Bolster_Flag == 0)
  
# Get a table of the wake pairs we want to bolster
a <- merge(leader_list, wake_cats) 
names(a) <- c("Leader_WTC", "Follower_WTC")
b <- merge(follower_list, wake_cats) %>% select(2, everything())
names(b) <- c("Leader_WTC", "Follower_WTC")
bolster_pairs <- rbind(a, b) %>% unique()
rm(a, b)

# Code to retrieve the relevant pairs
for (i in 1:nrow(bolster_pairs)){
  fol_wake <- bolster_pairs$Follower_WTC[i]
  lead_wake <- bolster_pairs$Leader_WTC[i]
  #pm_iter <- filter(pm, Follower_Recat_Wake_Cat == fol_wake) %>% mutate(Leader_Recat_Wake_Cat = lead_wake)
  pm_iter <- filter(pm1, Follower_Recat_Wake_Cat == fol_wake & Leader_Recat_Wake_Cat != lead_wake) %>% mutate(Leader_Recat_Wake_Cat = lead_wake)
  pm_iter <- mutate(pm_iter, Leader_Aircraft_Type = ifelse(lead_wake == "A", "A388", Leader_Aircraft_Type))
  pm_iter <- mutate(pm_iter, Follower_Aircraft_Type = ifelse(fol_wake == "A", "A388", Follower_Aircraft_Type))
  pm_iter <- mutate(pm_iter, Bolster_Flag = 1)
  if (nrow(pm_iter) > max_obs){pm_iter <- pm_iter[1:max_obs,]}
  if (i == 1){pm_bolst <- pm_iter} else {pm_bolst <- rbind(pm_bolst, pm_iter)}
}

# Remove the columns from pm_bolst that will be recalculated/joined
pm_bolst_test <- select(pm_bolst, -c("Recat_TBS_Wake_Separation_Time", "Recat_TBS_Wake_Separation_IAS", "Recat_DBS_Wake_Separation_Distance",
                                     "Follower_Forecast_IA_Wind_Effect"))

# Get the reference distances
pm_bolst_test <- left_join(pm_bolst_test, ref_dist, by=c("Leader_Recat_Wake_Cat"="Leader_WTC", "Follower_Recat_Wake_Cat"="Follower_WTC")) %>% 
  rename(Recat_DBS_Wake_Separation_Distance = Reference_Wake_Separation_Distance)

# Get the reference times
pm_bolst_test <- left_join(pm_bolst_test, ref_times, by=c("Leader_Recat_Wake_Cat"="Leader_WTC", "Follower_Recat_Wake_Cat"="Follower_WTC")) %>% 
  rename(Recat_TBS_Wake_Separation_Time = Reference_Wake_Separation_Time)

# Get the reference Speeds
pm_bolst_test <- left_join(pm_bolst_test, ref_speeds, by=c("Leader_Recat_Wake_Cat"="Leader_WTC", "Follower_Recat_Wake_Cat"="Follower_WTC")) %>% 
  rename(Recat_TBS_Wake_Separation_IAS = Assumed_Wake_Separation_IAS)

# Get the wind effects 
pm_bolst_test <- left_join(pm_bolst_test, gwcs_merge, by=c("FP_Date", "Follower_Callsign"="Callsign",
                                                           "Recat_DBS_Wake_Separation_Distance"="Sep_Dist",
                                                           "Follower_Time_At_4DME"="Time_At_4DME")) %>% 
  rename(Follower_Forecast_IA_Wind_Effect = Forecast_Wind_Effect_IAS)

# Get the Assumed follower GSPD
pm_bolst_test <- mutate(pm_bolst_test, Follower_Reference_GSPD = Follower_Forecast_IA_Wind_Effect + Recat_TBS_Wake_Separation_IAS)

# Get the Time based IA 0DME distance separation and capped version at 3NM
pm_bolst_test <- mutate(pm_bolst_test, Recat_TBS_0DME_Wake_Separation_Distance_Uncapped = Follower_Reference_GSPD * Recat_TBS_Wake_Separation_Time / 3600)

# Remove any other fields so that columns match between two pm sets
pm_bolst_test <- select(pm_bolst_test, -c("Follower_Reference_GSPD"))

# Merge the bolstered/original datasets
pm <- rbind(pm, pm_bolst_test)

}

# ----------------------------------------------------------------------- #
# 6a. Cap Data and add Fields
# ----------------------------------------------------------------------- #

# Add ICAO4 wake categories
pm <- left_join(pm, aircraft_type_to_wake_legacy, by=c("Leader_Aircraft_Type"="Aircraft_Type")) %>% rename(Leader_ICAO_Wake_Cat = Wake)
pm <- left_join(pm, aircraft_type_to_wake_legacy, by=c("Follower_Aircraft_Type"="Aircraft_Type")) %>% rename(Follower_ICAO_Wake_Cat = Wake)
pm$LF_Pair_ICAO4 <- paste0(pm$Leader_ICAO_Wake_Cat, "-", pm$Follower_ICAO_Wake_Cat)

# Update ICAO4 wake separation distances (with 3NM MRS)
dbs_join <- mutate(dbs_wake_turbulence, LF_Pair_ICAO4 = paste0(Leader_WVI, "-", Follower_WVI)) %>% select(-c("Leader_WVI", "Follower_WVI"))
pm <- left_join(pm, dbs_join, by=c("LF_Pair_ICAO4")) %>% rename(ICAO_DBS_Wake_Separation_Distance = WT_Separation_Distance)
pm <- mutate(pm, 
             ICAO_DBS_Wake_Separation_Distance = ICAO_DBS_Wake_Separation_Distance/1852,
             ICAO_DBS_All_Separation_Distance = ifelse(is.na(ICAO_DBS_Wake_Separation_Distance), radar_min_sep, ICAO_DBS_Wake_Separation_Distance))
rm(dbs_join)

# Add RECAT L-F Pair
pm <- mutate(pm, LF_Pair_RECAT = paste0(Leader_Recat_Wake_Cat, "-", Follower_Recat_Wake_Cat))

# Get ICAO4 Separation Accuracies for 0DME, 1DME, 4DME
pm <- mutate(pm,
             ICAO_DBS_0DME_Separation_Accuracy = Observed_0DME_Separation_Distance - ICAO_DBS_All_Separation_Distance,
             ICAO_DBS_1DME_Separation_Accuracy = Observed_1DME_Separation_Distance - ICAO_DBS_All_Separation_Distance,
             ICAO_DBS_4DME_Separation_Accuracy = Observed_4DME_Separation_Distance - ICAO_DBS_All_Separation_Distance)

# Add Capped fields 
pm <- mutate(pm, 
             Forecast_ORD_Compression = ifelse(Forecast_ORD_Compression_Uncapped < comp_cap, comp_cap, Forecast_ORD_Compression_Uncapped),
             Recat_TBS_0DME_Wake_Separation_Distance = ifelse(Recat_TBS_0DME_Wake_Separation_Distance_Uncapped < wake_cap_0, wake_cap_0, Recat_TBS_0DME_Wake_Separation_Distance_Uncapped),
             Recat_TBS_0DME_ROT_Spacing_Distance = ifelse(Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped < rot_cap_0, rot_cap_0, Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped),
             Recat_TBS_0DME_All_Separation_Distance = ifelse(Recat_TBS_0DME_All_Separation_Distance_Uncapped < radar_min_sep, radar_min_sep, Recat_TBS_0DME_All_Separation_Distance_Uncapped),
             Recat_TBS_4DME_Wake_Separation_Distance = Recat_TBS_0DME_Wake_Separation_Distance + Forecast_ORD_Compression,
             Recat_TBS_4DME_ROT_Spacing_Distance = Recat_TBS_0DME_ROT_Spacing_Distance + Forecast_ORD_Compression,
             Recat_TBS_4DME_All_Separation_Distance =  Recat_TBS_0DME_All_Separation_Distance + Forecast_ORD_Compression)

# ----------------------------------------------------------------------- #
# 7. Save Data
# ----------------------------------------------------------------------- #

if (bolster_data){fwrite(pm, file.path(outdir, "IA_PM_Preprocessed.csv"))}
if (!bolster_data){fwrite(pm, file.path(outdir, "IA_PM_Preprocessed.csv"))}


# temp

sel_rel_fields <- function(df){
  df <- select(df, 
               Landing_Pair_ID,
               Leader_Recat_Wake_Cat,
               Follower_Recat_Wake_Cat,
               Recat_DBS_Wake_Separation_Distance,
               Recat_DBS_ROT_Spacing_Distance,
               Recat_TBS_0DME_Wake_Separation_Distance_Uncapped,
               Recat_TBS_0DME_ROT_Spacing_Distance_Uncapped,
               Recat_TBS_0DME_All_Separation_Distance_Uncapped,
               Recat_TBS_0DME_Wake_Separation_Distance,
               Recat_TBS_0DME_ROT_Spacing_Distance,
               Recat_TBS_0DME_All_Separation_Distance,
               Forecast_ORD_Compression,
               Recat_TBS_4DME_Wake_Separation_Distance,
               Recat_TBS_4DME_ROT_Spacing_Distance,
               Recat_TBS_4DME_All_Separation_Distance,
               ROT_Bolster_Flag,
               ROT_Other_Bolster_Flag)
  return(df)
}

#pmtest <- filter(pm, ROT_Bolster_Flag == 1 & ROT_Other_Bolster_Flag == 0) %>% sel_rel_fields()
#pmtest2 <- filter(pm, ROT_Other_Bolster_Flag == 1) %>% sel_rel_fields()

#pmtestg <-pmtest%>% group_by(Landing_Pair_ID, Recat_DBS_ROT_Spacing_Distance) %>% summarise(Count = n()) %>% filter(Count > 1)
#un <- unique(pmtestg$Landing_Pair_ID)

#pmtestf <- pmtest %>% filter(Landing_Pair_ID %in% un)
#pmtestf <- pmtestf[order(pmtestf$Landing_Pair_ID),]

#djm <- filter(pm, Bolster_Flag == 1 & ROT_Bolster_Flag == 1)

#pmtestg2 <- pmtest2 %>% group_by(Landing_Pair_ID, Recat_DBS_ROT_Spacing_Distance) %>% summarise(Count = n()) %>% filter(Count > 1)
#pmtestg3 <- rbind(pmtest, pmtest2) %>% group_by(Landing_Pair_ID, Recat_DBS_ROT_Spacing_Distance) %>% summarise(Count = n()) %>% filter(Count > 1)
