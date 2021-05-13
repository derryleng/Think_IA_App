# ----------------------------------------------------------------------- #
# Project Configuration ---------------------------------------------------
# ----------------------------------------------------------------------- #
# Run this section first before running any other sections!

rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Name of project database in SQL server
# Project_Database <- "LVNL_UTMA_Validation"
Project_Database <- "LVNL_UTMA_Validation"

# Name of project ORD directory on Dropbox
# Project_Directory <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD"
# Project_Directory <- "C:\\Users\\George Clark\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD"
# Project_Directory <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Output"

Project_Directory <- "C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD"


# Airport ICAO Designation
Airport_Code <- "EHAM"

# Import global variables, functions and database connection
source("ORD Resources.R", local = T)





# ----------------------------------------------------------------------- #
# Approach Speed Profiling ------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of output folder in project directory ---------------------------- #
outdir_approach_speed_profiling <- "Speed Profiles - Sprint 11 v2"

# Reference LSS type file ----------------------------------------------- #
ref_lss_type_table <- "01a ORD Configuration Output Type v1.7.csv"

# Reference wake lookup file -------------------------------------------- #
ref_wake_aircraft_table <- "UK and RECAT WTC Lookup.csv"

# Reference default wake adaptation file -------------------------------- #
ref_wake_adaptation <- "01c ORD Configuration Output Wake v1.4.csv"

# WTC type -------------------------------------------------------------- #
# Options:
#   "REF_DATA"
#   "DATABASE"
wake_type <- "DATABASE"

# Speed type ------------------------------------------------------------ #
# Options:
#   "Mode_S_IAS" - preferred speed type
#   "Track_Speed" - use only when Mode S IAS not available
#   "Calculated_Speed" - use only when above options unsuitable
speed_type <- "Mode_S_IAS"

# Airport altitude (ft asl) --------------------------------------------- #
# NB: Used when speed_type != "Mode_S_IAS"
airport_alt <- 550

# Set speed filtering parameters ---------------------------------------- #
# NB: Used when speed_type == "Calculated_Speed"
# For each flight, filters out calculated speeds which are not within
#   [max(min(Track_Speed)*(speed_filter_perc/100), speed_filter_limit_low), 
#    min(max(Track_Speed)*(1+speed_filter_perc/100), speed_filter_limit_high)]
speed_filter_perc <- 50
speed_filter_limit_low <- 50
speed_filter_limit_high <- 200

# Start day number ------------------------------------------------------ #
# Set to 1 to run script for all days
start_day_num <- 1

# Run Approach Speed Profiling ------------------------------------------ #
source("Approach Speed Profiling.R", local = T)




# ----------------------------------------------------------------------- #
# Parameter Summary -------------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of speed profile folder to use ----------------------------------- #
speed_profile_folder <- "Speed Profiles - Sprint 11 v3"

# Name of output folder in project directory ---------------------------- #
outdir_parameter_summary <- "Adaptation - Sprint 11 - IA-75 v5 MC"

# Generate new validation list? (Defaults to TRUE if list not found) ---- #
validation_generation <- F

# Probability of dates being reserved for validation -------------------- #
validation_threshold <- 0.20

# Parameter filter settings (*_filter variable must be set to TRUE) ----- #
a1_filter <- T
a1_min <- 75
a1_max <- 220

a2_filter <- T
a2_min <- 75
a2_max <- 220

b_filter <- F
b_min <- 100
b_max <- 180

n1_filter <- T
n1_min <- 0
n1_max <- 12

n2_filter <- T
n2_min <- 0
n2_max <- 14

d_filter <- F
d_min <- 0
d_max <- 50

# Run Parameter Summary ------------------------------------------------- #
source("Parameter Summary.R", local = T)




# ----------------------------------------------------------------------- #
# Vref Comparison ---------------------------------------------------------
# ----------------------------------------------------------------------- #
# "Comparison is the thief of joy" - Theodore Roosevelt

# Directory of Populate_tbl_ORD_Aircraft_Adaptation_*.csv --------------- #
adaptation_folder <- "Adaptation - Sprint 11 - IA-75 v5"


# Run Vref Comparison --------------------------------------------------- #
source("Vref Comparison.R", local = T)




# ----------------------------------------------------------------------- #
# Validation Analysis -----------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of output folder in project directory ---------------------------- #
outdir_validation_analysis <- "Validation - Sprint 14 New ORD v4"

# Validation View source ------------------------------------------------ #
# File path to backed up ORD_Validation_View.csv or set as NA for database
# validation_view_source <- NA
# validation_view_source <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Database Backup\\DB2 2020-06-02 ORD_Wake\\ORD_Validation_View.csv"
validation_view_source <- "C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD\\Validation - Sprint 14 New ORD v4\\ORD_Validation_View.csv"

# eTBS Performance Model View source ------------------------------------ #
# File path to backed up eTBS_Performance_View.csv or set as NA for database
# performance_model_source <- NA
# performance_model_source <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Database Backup\\DB2 2020-06-02 ORD_Wake\\eTBS_Performance_View.csv"
performance_model_source <- "C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD\\Validation - Sprint 14 New ORD v4\\eTBS_Performance_View.csv"

# Reference ICAO 4 aircraft wake lookup --------------------------------- #
# MC Add 23/9.  Use database for ICAO references instead
# ref_aircraft_wake_icao4 <- "reference_wake_category_icao.csv"

# Reference ICAO 4 wake separation table -------------------------------- #
# ref_ref_wake_icao4 <- "reference_wake_separation_dist_icao.csv"
# End Add

# Performance Measure --------------------------------------------------- #
#operational_hour_multiplier <- 7.04167
#operational_hour_multiplier <- 3.6
operational_hour_multiplier <- 21.1

# Compression Metric ---------------------------------------------------- #
# Choose "New" for the updated Compression calcs, anything else for DB values
ORD_Compression_Type <- "New"

# Use Validation_Date_List.csv to subset data? -------------------------- #
val <- F
# Validation_Date_List.csv directory (Parameter Summary folder)
# or specify file path inside project folder
# or specify full file path
# valset_folder <- "v3.0"
#valset_folder <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Reference Data\\DB2 New Dates.csv"
#valset_folder <- "C:\\Users\\George Clark\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Reference Data\\DB2 New Dates.csv"
valset_folder <- "C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD\\Adaptation - Sprint 13 - IA-187 v2\\Validation_Date_List.csv"


# T/F Delivery to ROT Indicator/Wake Indicator (tighter) ---------------- #
delivery_to_rot <- T

# Actual behaviour of the leader and follower --------------------------- #
obs_lead_ias_min <- 80
obs_lead_ias_max <- 180
obs_follow_ias_min <- 100
obs_follow_ias_max <- 200
obs_follow_ias_max_tight <- 200

# ORD behaviours (to exclude current issues with algorithm) ------------- #
ord_lead_ias_min <- 80
ord_lead_ias_max <- 180
ord_follow_ias_min <- 100
ord_follow_ias_max <- 200

# Separation Accuracy: Entire Dataset ----------------------------------- #
sep_accuracy_max <- 3
sep_accuracy_max_a380 <- 6

# Separation Accuracy: Tight Dataset ------------------------------------ #
sep_accuracy_max_tight <- 2.5
sep_accuracy_max_a380_tight <- 5.5

# Aircraft types to report performance metrics -------------------------- #
# NB: Leave blank to use all aircraft types in validation view
report_performance_actypes <- c(
  
)


# Adjustments for Separation Accuracy ----------------------------------- #
# MC Add 16/9.  Separation Adjustment Parameters for modelled accuracy
sep_adjust_file <- "separation_adjustment_parameters_v2.csv"

egll_mean <- 0.55
egll_sd <- 0.46
pc_under <- 0.03
# End Add


# Run Validation Analysis ----------------------------------------------- #
source("Validation Analysis - MC.R", local = T)
