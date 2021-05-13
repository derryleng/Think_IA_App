# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Validation Analysis                                   #
#                |                                                        #
# Version No.    |  3.5                                                   #
#                |                                                        #
# Date Modified  |  19/02/2020                                            #
#                |                                                        #
# Author(s)      |  Michael Cowham, Derry Leng                            #
#                |                                                        #
# Project        |  eTBS Related Projects                                 #
#                |                                                        #
# Purpose        |  Assess performance requirements of adaptation data    #
#                |                                                        #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
#
# 3.5  Removed all GSPD difference related metrics/filtering (no longer 
#       required as this was only used for NavCan DBS Calibration).
#      Multiple minor changes and additional metrics added.
#
# 3.4  Adjusted for new directory (NATS eTBS Adaptation Support)
#      Moved functions and global variables to ORD Resources.R
#      Moved configurations to Run Scripts.R
#      Corrected max_Follower_GSPD_Diff_ac and max_Follower_GSPD_Diff_0_8
#       to min_Follower_GSPD_Diff_ac and min_Follower_GSPD_Diff_0_8
#
# 3.3  Updated wake lookup table.
#      Added GSPD_Diff filtering.
#
# 3.2  Updated with ICAO 4 WTC.
#      Modified for NavCan.
#      Renamed file from ORD - Data Analysis to Validation Analysis.
#
# 3.1  Added density based calculations for P(X>0) and P(X>0.5). Temporary use
#      of .csv files (which have had CL35 data merged into them)
#
# 3.0  Removed redundant output tables and processing.
#
# 2.9  Updated based on new requirements for NATS eTBS Adaptation Support.
#
# 2.6  Significant updates to model the actual threshold accuracy, linking
#      to the TBS logs for current day accuracy and adjusting for A380s
#      There are still probably some small issues with the modelled 4DME
#      accuracy for A380 leaders.
#
# 2.5  Updated to replicate the operational rounding.
#      ORD Compression Error = ROUND(Observed Compression-ROUND(ORD,1),1)
#      Update the under-separation test to be >= 0.5 not >0.5 on the
#      rounded values to reflect the controller procedures.
#
# 2.4  Updated for the 13/11 data drop.
#
# 2.3  Updated, still using v1.4 but with the corrected ORD Calculations.
#
# 2.2  Analysis of v1.4 configuration data, including use of validation
#      data based on days selected in 03 ORD Parameter Summary (v1.3).
#
# 2.1  Investigation with compression error to 1DME, using eTBS
#      Performance Model data.
#
# 2.0  Updated to produce all output files for analysis.
#
# 1.5  1.3 analysis.  No headers loaded.
#
# 1.4  Further updtaes for 1.2.  Some changes to filtering parameters.
#
# 1.3  Updated to look at multiple ORD outputs: AC Type, Table and Wake.
#      Still from configuration v1.1
#
# 1.2  Updated for the second run of the ORD Algorithms.
#      Configuration Version 1.1 for Aircraft Types.
#
# 1.1  Updated with new data from Rob Wood.
#      Reads in the WTC lookup files.
#
# ----------------------------------------------------------------------- #

# Output directory
out_data <- file.path(Project_Directory, outdir_validation_analysis)
if (!dir.exists(out_data)) dir.create(out_data)

# ----------------------------------------------------------------------- #
# 3. Load Data ------------------------------------------------------------
# ----------------------------------------------------------------------- #

data1 <- if (is.na(validation_view_source)) {
  sqlQuery(con, sprintf("SELECT * FROM vw_ORD_Validation_View")) %>% as.data.table()
} else {
  fread(validation_view_source)
}

# Filter not-in-trail pairs on 06L/R and 24L/R for CYYZ
if (Airport_Code == "CYYZ") {
  data1 <- data1[!(Landing_Pair_Type == "Not_In_Trail" & Landing_Runway %in% c("R24R", "R24L", "R06L", "R06R"))]
}

pdata1 <- if (is.na(performance_model_source)) {
  sqlQuery(con, sprintf("SELECT * FROM vw_eTBS_Performance_Model")) %>% as.data.table()
} else {
  fread(performance_model_source)
}

# MC Edit 23/9: Replace the ICAO wake table look up with a query on the database
# aircraft_wake_icao4 <- fread(file.path(ref_data, ref_aircraft_wake_icao4))
aircraft_wake_icao4 <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy") %>% rename(ICAO_WTC = Wake) %>% mutate(Aircraft_Type = as.character(Aircraft_Type), ICAO_WTC = as.character(ICAO_WTC)) %>% as.data.table()
ref_wake_icao4 <- sqlQuery(con, "SELECT * FROM tbl_DBS_Wake_Turbulence") %>% rename (Leader_ICAO_WTC = Leader_WVI, Follower_ICAO_WTC = Follower_WVI, Reference_Wake_Separation_Distance = WT_Separation_Distance) %>% mutate (Reference_Wake_Separation_Distance = Reference_Wake_Separation_Distance / 1852) %>% as.data.table()
#ref_wake_icao4 <- fread(file.path(ref_data, ref_ref_wake_icao4))
# End Edit

ref_wake <- sqlQuery(con, sprintf("
  SELECT
  	Leader_WTC,
  	Follower_WTC,
  	Reference_Wake_Separation_Distance/1852 AS Reference_Wake_Separation_Distance
  FROM tbl_Reference_Recat_Separation_Dist
  ")) %>% as.data.table()

if (val) {
  valset_dir <- file.path(Project_Directory, valset_folder, "Validation_Date_List.csv")
  if (!file.exists(valset_dir)) {
    valset_dir <- file.path(valset_folder)
  }
  valset <- fread(valset_dir)
}


# MC Add 17/9.  It would be useful to output some of the adaptation with the results

adaptation <- as.data.frame(list("Validation_Set" = val,
                                 "Delivery_To_ROT" = delivery_to_rot,
                                 "obs_lead_ias_min" = obs_lead_ias_min,
                                 "obs_lead_ias_max" = obs_lead_ias_max,
                                 "obs_follow_ias_min" = obs_follow_ias_min,
                                 "obs_follow_ias_max" = obs_follow_ias_max,
                                 "obs_follow_ias_max_tight" = obs_follow_ias_max_tight,
                                 "ord_lead_ias_min" = ord_lead_ias_max,
                                 "ord_lead_ias_max" = ord_lead_ias_max,
                                 "ord_follow_ias_min" = ord_follow_ias_min,
                                 "ord_follow_ias_max" = ord_follow_ias_max,
                                 "sep_accuracy_max" = sep_accuracy_max,
                                 "sep_accuracy_max_a380" = sep_accuracy_max_a380,
                                 "sep_accuracy_max_tight" = sep_accuracy_max_tight,
                                 "sep_accuracy_max_a380_tight" = sep_accuracy_max_a380_tight,
                                 "operational_hour_multiplier" = operational_hour_multiplier,
                                 "sep_adjust_file" = sep_adjust,
                                 "egll_mean" = egll_mean,
                                 "egll_sd" = egll_sd,
                                 "pc_under" = pc_under))

# MC Add 23/9: Make the density calculations optional to speed up the calcs
include_density <- F
# End Add

# MD Add 7/10
# Reference EGLL data
egll_data1 <- fread(file.path(Project_Directory, "Reference EGLL Outputs", "ORD_Validation_Data.csv"), na.strings = c("NULL"))
egll_pdata1 <- fread(file.path(Project_Directory, "Reference EGLL Outputs", "eTBS_Performance_Model.csv"), na.strings = c("NULL"))


fwrite(adaptation, file.path(out_data, "adaptation.csv"))
# ----------------------------------------------------------------------- #
# 4. Data Processing ------------------------------------------------------
# ----------------------------------------------------------------------- #

# Operator

data1$Leader_Operator <- substr(data1$Leader_Callsign, 1, 3)
data1$Follower_Operator <- substr(data1$Follower_Callsign, 1, 3)

# Update eTBS 4DME Separation Distance using ICAO 4 WTC Separation Distances

data1$Leader_ICAO4 <- aircraft_wake_icao4[match(data1$Leader_Aircraft_Type, Aircraft_Type)]$ICAO_WTC
data1$Follower_ICAO4 <- aircraft_wake_icao4[match(data1$Follower_Aircraft_Type, Aircraft_Type)]$ICAO_WTC

data1$eTBS_4DME_Separation_Distance <- ref_wake_icao4[match(paste(data1$Leader_ICAO4, data1$Follower_ICAO4), paste(Leader_ICAO_WTC, Follower_ICAO_WTC))]$Reference_Wake_Separation_Distance

# MC Added 16/9.  Need a flag for whether this is a legacy Wake or Non-Wake Pair
# Also, need to get rid of the L/M aircraft type and add the ICAO4 L-F pair 
data1 <- mutate(data1, Legacy_Wake = ifelse(is.na(eTBS_4DME_Separation_Distance), "Legacy Non-Wake", "Legacy Wake"),
                       RECAT_Wake = ifelse((paste(Leader_RECAT, Follower_RECAT) %in% paste(ref_wake$Leader_WTC, ref_wake$Follower_WTC)), "RECAT Wake", "RECAT Non-Wake"),
                       Leader_ICAO4 = ifelse(Leader_Aircraft_Type == "SW4", "M", Leader_ICAO4),
                       LF_Pair_ICAO4 = paste0(Leader_ICAO4, "-", Follower_ICAO4))
# End Add

data1$eTBS_4DME_Separation_Distance <- ifelse(is.na(data1$eTBS_4DME_Separation_Distance), 3, data1$eTBS_4DME_Separation_Distance)

# MC - This section has been moved from later in the script.  This is 
# because the ORD Speed Calculations need the updated separation accuracy

# Calculate the Separation Accuracy based on RECAT EU.

if ("Delivered_4DME_Separation" %in% names(data1)) {
  data1$Required_Separation_Accuracy <- data1$Delivered_4DME_Separation - data1$eTBS_4DME_Separation_Distance
} else {
  data1$Required_Separation_Accuracy <- data1$Delivered_FAF_Separation - data1$eTBS_4DME_Separation_Distance
}

# MC Add 23/9
# Calculate the Threshold Delivered Separation
data1 <- mutate(data1, Thresh_Separation = Follower_Stop_RTT - Leader_0DME_RTT,
                Required_Thresh_Separation_Accuracy = Thresh_Separation - eTBS_4DME_Separation_Distance,
                Required_Thresh_Separation_Accuracy_Rounded = round(Required_Thresh_Separation_Accuracy, 1))
# End Add

if (ORD_Compression_Type == "New"){
  
  # Then adjust the required separation accuracy using the formula from the
  # valdation plan:
  # rsa = mean_egll + sd_egll * ((rsa - mean_eham) / sd_eham)
  
  # Join the sep_adjust parameters.  These depend on the L-F pair
  # Calculate the adjusted separation accuracy
  
  data1 <- inner_join(data1, sep_adjust, by = c("Legacy_Wake", "LF_Pair_ICAO4"))
  data1 <- mutate(data1, Required_Separation_Accuracy_Std = egll_sd * ((Required_Separation_Accuracy - Mean_Accuracy) / SD_Accuracy))  
  
  pc_under_0 <- filter(data1, Required_Separation_Accuracy <= 3) %>%
    group_by(Legacy_Wake, LF_Pair_ICAO4) %>%
    summarise(Qle_0 = quantile(Required_Separation_Accuracy_Std, pc_under)) %>% ungroup()
  
  data1 <- inner_join(data1, pc_under_0, by = c("Legacy_Wake", "LF_Pair_ICAO4"))
  data1 <- mutate(data1,  Required_Separation_Accuracy_Original = Required_Separation_Accuracy,
                          Required_Separation_Accuracy = Required_Separation_Accuracy_Std - Qle_0 - 0.05) %>% filter(Required_Separation_Accuracy >= -0.54)
  
  fwrite(pc_under_0, file.path(Project_Directory, "quantile_adjustment.csv"))
  
}

# MC End Move

# Secondary option for ORD Compression values
# Need to add ORD_Compression_Type parameter below as a global variable
# Need to double check ord_follower_ias_adjustment, maybe make this global too

if (ORD_Compression_Type == "New"){

  #data1 <- mutate(data1, ORD_Mean_Follower_IAS_Adjusted = ifelse(Observed_Mean_Follower_IAS < ORD_Mean_Follower_IAS, Observed_Mean_Follower_IAS, ORD_Mean_Follower_IAS))
  
  data1 <- mutate(data1, Modelled_Follower_Speed_Error = -4.2 + 9 * Required_Separation_Accuracy_Original,
                  Modelled_Follower_Speed_Error_Residual = ORD_Follower_IAS_Error - Modelled_Follower_Speed_Error,
                  New_Modelled_Follower_Speed_Error_Base = -4.2 + 9 * Required_Separation_Accuracy,
                  ORD_Follower_IAS_Error_New = New_Modelled_Follower_Speed_Error_Base + Modelled_Follower_Speed_Error_Residual,
                  Observed_Mean_Follower_IAS_Adjusted = ORD_Mean_Follower_IAS + ORD_Follower_IAS_Error_New)
  
  
  ord_follower_ias_adjustment <- -2
  data1$Observed_Compression <- ((data1$Leader_FAF_RTT - data1$Leader_0DME_RTT) / (data1$Observed_Mean_Leader_IAS + data1$Observed_Mean_Leader_Wind_Effect)) * 
    ((data1$Observed_Mean_Follower_IAS_Adjusted + data1$Observed_Mean_Follower_Wind_Effect) - 
       (data1$Observed_Mean_Leader_IAS + data1$Observed_Mean_Leader_Wind_Effect))
  data1$ORD_Compression <- ((data1$Leader_FAF_RTT - data1$Leader_0DME_RTT)/(data1$ORD_Mean_Leader_IAS + data1$Forecast_Mean_Leader_Wind_Effect)) * 
    ((data1$ORD_Mean_Follower_IAS + data1$Forecast_Mean_Follower_Wind_Effect) - 
       (data1$ORD_Mean_Leader_IAS + data1$Forecast_Mean_Leader_Wind_Effect))
  data1$ORD_Compression_Error <- data1$Observed_Compression - data1$ORD_Compression
  
}

# Update any ORD Compression values less than zero

data1$ORD_Compression_Error <- ifelse(data1$ORD_Compression < 0, data1$Observed_Compression, data1$ORD_Compression_Error)

data1$ORD_Compression <- ifelse(data1$ORD_Compression < 0, 0, data1$ORD_Compression)

# Create the 1DME Observed Compression and ROT Additional Distance

pdata1$Additional_1DME_Compression <- pdata1$Observed_1DME_Separation_Distance - pdata1$Observed_0DME_Separation_Distance

pdata1$Additional_ROT_Distance <- pdata1$Recat_eTBS_4DME_ROT_Spacing_Distance - pdata1$Recat_eTBS_4DME_Wake_Separation_Distance

data1$ORD_1DME_Additional <- pdata1[match(data1$Landing_Pair_ID, Landing_Pair_ID)]$Additional_1DME_Compression

data1$Additional_ROT_Distance <- pdata1[match(data1$Landing_Pair_ID, Landing_Pair_ID)]$Additional_ROT_Distance

# Update the ORD Compression Values to equal Observed - ROUND(Forecast,1)

data1$Observed_Compression_1DME <- data1$Observed_Compression - data1$ORD_1DME_Additional

data1$ORD_Compression_Error_1DME <- data1$Observed_Compression_1DME - round(data1$ORD_Compression, 1)

data1$ORD_Compression_Error <- data1$Observed_Compression - round(data1$ORD_Compression, 1)

# Lookup the Surface Wind Speed and Heading

data1$Observed_AGI_Surface_Wind_SPD <- pdata1[match(data1$Landing_Pair_ID, Landing_Pair_ID)]$Observed_AGI_Surface_Wind_SPD

data1$Observed_AGI_Surface_Wind_HDG <- pdata1[match(data1$Landing_Pair_ID, Landing_Pair_ID)]$Observed_AGI_Surface_Wind_HDG

data1$Observed_AGI_Surface_Headwind_Check <- pdata1[match(data1$Landing_Pair_ID, Landing_Pair_ID)]$Observed_AGI_Surface_Headwind

# Calculate the Wind Groups

data1$Observed_AGI_Surface_Wind_SPD_Group <- cut(data1$Observed_AGI_Surface_Wind_SPD, breaks = c(0,4,7,10,13,16,Inf), right=FALSE)

data1$Observed_AGI_Surface_Headwind_Group <- cut(data1$Observed_AGI_Surface_Headwind, breaks = c(-Inf, -2, 0, 2, 4, 6, 8, 10, 12, 14, Inf), right = FALSE)

data1$Observed_Mean_Leader_Wind_Effect_Group <- cut(data1$Observed_Mean_Leader_Wind_Effect, breaks = c(-Inf, -25, -20, -15, -10, -5, 0, 5, Inf))

# Add on the ROT distance if delivery to ROT indicators is also considered.
# This means that for wake pairs where ROT is present, there is additional buffer
# before a loss of (wake) separation occurs.

if (delivery_to_rot){
  data1$Required_Separation_Accuracy_Wake <- ifelse(is.na(data1$Additional_ROT_Distance), data1$Required_Separation_Accuracy, data1$Required_Separation_Accuracy + data1$Additional_ROT_Distance)
} else {
  data1$Required_Separation_Accuracy_Wake <- data1$Required_Separation_Accuracy
}

# Set up the 1DME Compression Metrics. To match the Operational System,
# the ORD value is rounded before the Line 0 / 4DME Calculation

data1$ORD_Compression_Error_1DME_Rounded <- round(data1$ORD_Compression_Error_1DME, 1)

data1$Modelled_Thresh_Accuracy_1DME <- data1$Required_Separation_Accuracy_Wake - data1$ORD_Compression_Error_1DME
data1$Modelled_Thresh_Accuracy_1DME_Rounded <- round(data1$Modelled_Thresh_Accuracy_1DME, 1)

data1$Thresh_Accuracy_0_1DME <- factor(ifelse(data1$Modelled_Thresh_Accuracy_1DME_Rounded <= 0, 0, 1), labels = c("(-Inf, 0NM]", "(0NM, Inf)"))
data1$Thresh_Accuracy_05_1DME <- factor(ifelse(data1$Modelled_Thresh_Accuracy_1DME_Rounded <= -0.5, 0, ifelse(data1$Modelled_Thresh_Accuracy_1DME_Rounded <= 0, 1, 2)), labels = c("(-Inf, -0.5NM]", "(-0.5NM, 0NM]", "(0NM, Inf)"))
data1$Thresh_Accuracy_1_1DME <- factor(ifelse(data1$Modelled_Thresh_Accuracy_1DME_Rounded <= -1, 0, ifelse(data1$Modelled_Thresh_Accuracy_1DME_Rounded <= -0.5, 1, ifelse(data1$Modelled_Thresh_Accuracy_1DME_Rounded <= 0, 2, 3))), labels = c("(-Inf, -1NM]", "(-1NM, -0.5NM]", "(-0.5NM, 0NM]", "(0NM, Inf)"))

data1$Thresh_Accuracy_Perfect_0_1DME <- factor(ifelse(data1$ORD_Compression_Error_1DME_Rounded >= 0, 0, 1), labels = c("[0NM, Inf)", "(-Inf, 0NM)"))
data1$Thresh_Accuracy_Perfect_05_1DME <- factor(ifelse(data1$ORD_Compression_Error_1DME_Rounded >= 0.5, 0, ifelse(data1$ORD_Compression_Error_1DME_Rounded >= 0, 1, 2)), labels = c("[0.5NM, Inf)", "[0NM, 0.5NM)", "(-Inf, 0NM)"))
data1$Thresh_Accuracy_Perfect_1_1DME <- factor(ifelse(data1$ORD_Compression_Error_1DME_Rounded >= 1, 0, ifelse(data1$ORD_Compression_Error_1DME_Rounded >= 0.5, 1, ifelse(data1$ORD_Compression_Error_1DME_Rounded >= 0, 2, 3))), labels = c("[1NM, Inf)", "[0.5NM, 1NM)", "[0NM, 0.5NM)", "(-Inf, 0NM)"))

# Set up the Threshold Compression Metrics

data1$ORD_Compression_Error_Rounded <- round(data1$ORD_Compression_Error, 1)

data1$Modelled_Thresh_Accuracy <- data1$Required_Separation_Accuracy_Wake - data1$ORD_Compression_Error
# data1$Modelled_Thresh_Accuracy <- density(data1$ORD_Compression_Error, n = length(data1$ORD_Compression_Error))$x
data1$Modelled_Thresh_Accuracy_Rounded <- round(data1$Modelled_Thresh_Accuracy, 1)

data1$Thresh_Accuracy_0 <- factor(ifelse(data1$Modelled_Thresh_Accuracy_Rounded <= 0, 0, 1), labels = c("(-Inf, 0NM]", "(0NM, Inf)"))
data1$Thresh_Accuracy_05 <- factor(ifelse(data1$Modelled_Thresh_Accuracy_Rounded <= -0.5, 0, ifelse(data1$Modelled_Thresh_Accuracy_Rounded <= 0, 1, 2)), labels = c("(-Inf, -0.5NM]", "(-0.5NM, 0NM]", "(0NM, Inf)"))
data1$Thresh_Accuracy_1 <- factor(ifelse(data1$Modelled_Thresh_Accuracy_Rounded <= -1, 0, ifelse(data1$Modelled_Thresh_Accuracy_Rounded <= -0.5, 1, ifelse(data1$Modelled_Thresh_Accuracy_Rounded <= 0, 2, 3))), labels = c("(-Inf, -1NM]", "(-1NM, -0.5NM]", "(-0.5NM, 0NM]", "(0NM, Inf)"))

data1$Thresh_Accuracy_Perfect_0 <- factor(ifelse(data1$ORD_Compression_Error_Rounded >= 0, 0, 1), labels = c("[0NM, Inf)", "(-Inf, 0NM)"))
data1$Thresh_Accuracy_Perfect_05 <- factor(ifelse(data1$ORD_Compression_Error_Rounded >= 0.5, 0, ifelse(data1$ORD_Compression_Error_Rounded >= 0, 1, 2)), labels = c("[0.5NM, Inf)", "[0NM, 0.5NM)", "(-Inf, 0NM)"))
data1$Thresh_Accuracy_Perfect_1 <- factor(ifelse(data1$ORD_Compression_Error_Rounded >= 1, 0, ifelse(data1$ORD_Compression_Error_Rounded >= 0.5, 1, ifelse(data1$ORD_Compression_Error_Rounded >= 0, 2, 3))), labels = c("[1NM, Inf)", "[0.5NM, 1NM)", "[0NM, 0.5NM)", "(-Inf, 0NM)"))

# Leader and Follower Speed Accuracy

data1$Speed_Accuracy_Leader_05 <- factor(ifelse(data1$ORD_Leader_IAS_Error < -9, 0, ifelse(data1$ORD_Leader_IAS_Error < 0, 1, 2)), labels = c("(0.25NM, Inf)", "(0NM, 0.25NM]", "(-Inf, 0NM]"))

data1$Speed_Accuracy_Follower_05 <- factor(ifelse(data1$ORD_Follower_IAS_Error > 9, 0, ifelse(data1$ORD_Follower_IAS_Error > 0, 1, 2)), labels = c("(0.25NM, Inf)", "(0NM, 0.25NM]", "(-Inf, 0NM]"))

# Combined Wind Error

data1$Combined_GWCS_Error <- data1$Forecast_Mean_Follower_Wind_Effect_Error - data1$Forecast_Mean_Leader_Wind_Effect_Error

# Lookup the validation date flag (Use only validation set reserved dates, or all dates)

if (val) {
  # data1$Validation_Include <- valset$in_out[match(data1$FP_Date, valset$dates)]
  data1$Validation_Include <- ifelse(data1$FP_Date %in% valset[Reserve == F]$Date, 1, 0)
} else {
  data1$Validation_Include <- 1
}

# Set up an exclude flag

data1$Exclude_d1 <- ifelse(
  (data1$Leader_Aircraft_Type == "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max_a380) |
    (data1$Leader_Aircraft_Type != "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max) |
    (data1$ORD_Mean_Leader_IAS < ord_lead_ias_min | data1$ORD_Mean_Leader_IAS > ord_lead_ias_max) |
    (data1$ORD_Mean_Follower_IAS < ord_follow_ias_min | data1$ORD_Mean_Follower_IAS > ord_follow_ias_max) |
    (data1$Observed_Mean_Leader_IAS < obs_lead_ias_min | data1$Observed_Mean_Leader_IAS > obs_lead_ias_max) |
    (data1$Observed_Mean_Follower_IAS < obs_follow_ias_min | data1$Observed_Mean_Follower_IAS > obs_follow_ias_max),
  1,
  0
)

data1$Exclude_d2 <- ifelse(
  (data1$Leader_Aircraft_Type == "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max_a380_tight) |
    (data1$Leader_Aircraft_Type != "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max_tight) |
    (data1$ORD_Mean_Leader_IAS < ord_lead_ias_min | data1$ORD_Mean_Leader_IAS > ord_lead_ias_max) |
    (data1$ORD_Mean_Follower_IAS < ord_follow_ias_min | data1$ORD_Mean_Follower_IAS > ord_follow_ias_max) |
    (data1$Observed_Mean_Leader_IAS < obs_lead_ias_min | data1$Observed_Mean_Leader_IAS > obs_lead_ias_max) |
    (data1$Observed_Mean_Follower_IAS < obs_follow_ias_min | data1$Observed_Mean_Follower_IAS > obs_follow_ias_max_tight),
  1,
  0
)

data1 <- as.data.table(data1)

# exclude_check <- data1[Leader_Aircraft_Type == "A388" & (Exclude_d1 | Exclude_d2), c("Exclude_d1", "Exclude_d2", "Required_Separation_Accuracy", "ORD_Mean_Leader_IAS", "ORD_Mean_Follower_IAS", "Observed_Mean_Leader_IAS", "Observed_Mean_Follower_IAS")]
# exclude_check$above_sep_accuracy_max_a380 <- ifelse(exclude_check$Required_Separation_Accuracy > sep_accuracy_max_a380, T, F)
# exclude_check$above_sep_accuracy_max_a380_tight <- ifelse(exclude_check$Required_Separation_Accuracy > sep_accuracy_max_a380_tight, T, F)
# exclude_check$below_ord_lead_ias_min <- ifelse(exclude_check$ORD_Mean_Leader_IAS < ord_lead_ias_min, T, F)
# exclude_check$above_ord_lead_ias_max <- ifelse(exclude_check$ORD_Mean_Leader_IAS > ord_lead_ias_max, T, F)
# exclude_check$below_ord_follow_ias_min <- ifelse(exclude_check$ORD_Mean_Follower_IAS < ord_follow_ias_min, T, F)
# exclude_check$above_ord_follow_ias_max <- ifelse(exclude_check$ORD_Mean_Follower_IAS > ord_follow_ias_max, T, F)
# exclude_check$below_obs_lead_ias_min <- ifelse(exclude_check$Observed_Mean_Leader_IAS < obs_lead_ias_min, T, F)
# exclude_check$above_obs_lead_ias_max <- ifelse(exclude_check$Observed_Mean_Leader_IAS > obs_lead_ias_max, T, F)
# exclude_check$below_obs_follow_ias_min <- ifelse(exclude_check$Observed_Mean_Follower_IAS < obs_follow_ias_min, T, F)
# exclude_check$above_obs_lead_ias_max <- ifelse(exclude_check$Observed_Mean_Follower_IAS > obs_lead_ias_max, T, F)
# exclude_check$above_obs_follow_ias_max_tight <- ifelse(exclude_check$Observed_Mean_Follower_IAS > obs_follow_ias_max_tight, T, F)
# View(exclude_check)

# Create filter summary table

exclude <- data.table(

  # A388 SepN Accuracy
  A380_acc = ifelse(data1$Leader_Aircraft_Type == "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max_a380, 1, 0),
  Other_acc = ifelse(data1$Leader_Aircraft_Type != "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max, 1, 0),

  # A388 SepN Accuracy Tight
  A380_acc_tight = ifelse(data1$Leader_Aircraft_Type == "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max_a380_tight, 1, 0),
  Other_acc_tight = ifelse(data1$Leader_Aircraft_Type != "A388" & data1$Required_Separation_Accuracy > sep_accuracy_max_tight, 1, 0),

  # Leader ORD IAS Range
  Lead_ORD_IAS = ifelse(data1$ORD_Mean_Leader_IAS < ord_lead_ias_min | data1$ORD_Mean_Leader_IAS > ord_lead_ias_max,1,0),

  # Follower ORD IAS Range
  Follower_ORD_IAS = ifelse(data1$ORD_Mean_Follower_IAS < ord_follow_ias_min | data1$ORD_Mean_Follower_IAS > ord_follow_ias_max, 1,0),

  # Observed Ranges - Leader IAS
  Lead_Actual_IAS = ifelse(data1$Observed_Mean_Leader_IAS != 0 & (data1$Observed_Mean_Leader_IAS < obs_lead_ias_min | data1$Observed_Mean_Leader_IAS) > obs_lead_ias_max, 1,0),

  # Observed Ranges - Follower IAS
  Follower_Actual_IAS = ifelse(data1$Observed_Mean_Follower_IAS != 0 & (data1$Observed_Mean_Follower_IAS < obs_follow_ias_min | data1$Observed_Mean_Follower_IAS > obs_follow_ias_max), 1,0),

  Lead_IAS_Zero = ifelse(data1$Observed_Mean_Leader_IAS == 0, 1,0),

  Follower_IAS_Zero = ifelse(data1$Observed_Mean_Follower_IAS == 0, 1,0),

  All_d1 = data1$Exclude_d1,
  All_d2 = data1$Exclude_d2

)

# Remove pairs with excessve ORD values based on ORD forecast
temp1 <- nrow(data1)
data1_pre_ias_filter <- data1
data1 <- data1[ORD_Mean_Leader_IAS >= ord_lead_ias_min & ORD_Mean_Leader_IAS <= ord_lead_ias_max]
data1 <- data1[ORD_Mean_Follower_IAS >= ord_follow_ias_min & ORD_Mean_Follower_IAS <= ord_follow_ias_max]
temp2 <- nrow(data1)
message("Removed ", temp1, " - ", temp2, " = ", temp1-temp2, " pairs with excessive Mean IAS values based on forecast.")

# Remove pairs based on excessive Observed Values
temp1 <- nrow(data1)
data1_pre_ias_filter2 <- data1
data1 <- data1[Observed_Mean_Leader_IAS >= obs_lead_ias_min & Observed_Mean_Leader_IAS <= obs_lead_ias_max]
data1 <- data1[Observed_Mean_Follower_IAS >= obs_follow_ias_min & Observed_Mean_Follower_IAS <= obs_follow_ias_max]
temp2 <- nrow(data1)
message("Removed ", temp1, " - ", temp2, " = ", temp1-temp2, " pairs with excessive Observed Mean IAS values.")

# Copy data1 before the restrictions for SepN Accuracy are put in place
data2 <- data1

fwrite(data2, file = file.path(out_data, "Validation Data Pre SepN Accuracy.csv"))

# Remove pairs with spacing not close to minima for data1: the bulk of the dataset
temp1 <- nrow(data1)
data1 <- rbind((data1[Leader_Aircraft_Type == "A388" & Required_Separation_Accuracy <= sep_accuracy_max_a380]),
               (data1[Leader_Aircraft_Type != "A388" & Required_Separation_Accuracy <= sep_accuracy_max]))
temp2 <- nrow(data1)
message("Removed ", temp1, " - ", temp2, " = ", temp1-temp2, " pairs with spacing not close to minima.")

# Remove pairs with spacing not close to minima for data2: the "tight" delivered separation
temp1 <- nrow(data2)
data2 <- rbind((data2[Leader_Aircraft_Type == "A388" & Required_Separation_Accuracy <= sep_accuracy_max_a380_tight]),
               (data2[Leader_Aircraft_Type != "A388" & Required_Separation_Accuracy <= sep_accuracy_max_tight]))
temp2 <- nrow(data2)
message("Removed ", temp1, " - ", temp2, " = ", temp1-temp2, " pairs with spacing not close to tight minima.")

# For data set 2 only, remove aircraft with fast follower IAS
temp1 <- nrow(data2)
data2 <- data2[Observed_Mean_Follower_IAS <= obs_follow_ias_max_tight]
temp2 <- nrow(data2)
message("Removed ", temp1, " - ", temp2, " = ", temp1-temp2, " pairs with fast follower IAS.")

# Filter validation set
if (val) {
  data1 <- subset(data1, data1$Validation_Include == 0)
  data2 <- subset(data2, data2$Validation_Include == 0)
}

fwrite(data1, file = file.path(out_data, "Validation Data Post SepN Accuracy.csv"))

# ----------------------------------------------------------------------- #
# 5. Tables for Report ----------------------------------------------------
# ----------------------------------------------------------------------- #

actypes <- if (length(report_performance_actypes) > 0) {
  report_performance_actypes
} else {
  sort(unique(unique(data1$Leader_Aircraft_Type), unique(data2$Leader_Aircraft_Type)))
}

d_post_SepN <- data1
d_post_SepN_In_Trail <- data1[Landing_Pair_Type %in% c("In_Trail", "In_Trail_Non_Sequential")]
d_post_SepN_Not_In_Trail <- data1[Landing_Pair_Type %in% c("Not_In_Trail")]

# Graph of Mean ORD compression by adaptation actypes
png(file.path(out_data, "Mean ORD Compression by Adaptation AC Type.png"), width = 1800, height = 600)
ggplot(
  data = ddply(
    d_post_SepN_In_Trail[Leader_Aircraft_Type %in% actypes],
    "Leader_Aircraft_Type",
    summarise,
    ORD_Compression = mean(ORD_Compression, na.rm=TRUE)
  ) %>% as.data.table() %>% .[order(ORD_Compression)],
  aes(y = ORD_Compression, x = reorder(Leader_Aircraft_Type, ORD_Compression), fill = ORD_Compression)
) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = brewer.pal(11, "Spectral")) +
  scale_y_continuous(expand=c(0,0)) +
  labs(
    x = "Leader Aircraft Type",
    y = "Mean ORD Compression (NM)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()

d_pre_SepN <- data2
d_pre_SepN_In_Trail <- d_pre_SepN[Landing_Pair_Type %in% c("In_Trail", "In_Trail_Non_Sequential")]
d_pre_SepN_In_Trail_WakePair <- d_pre_SepN_In_Trail[paste(Leader_RECAT, Follower_RECAT) %in% paste(ref_wake$Leader_WTC, ref_wake$Follower_WTC)]
d_pre_SepN_Not_In_Trail <- d_pre_SepN[Landing_Pair_Type %in% c("Not_In_Trail")]

# Create GSPD Diff bands
# d$Leader_GSPD_Diff_0_8_Group <- cut(d$Leader_GSPD_Diff_0_8, breaks = seq(-70, 70, 5), right = F)
# d$Leader_GSPD_Diff_ac_Group <- cut(d$Leader_GSPD_Diff_ac, breaks = seq(-70, 70, 5), right = F)

Performance_Overall_In_Trail_1DME <- data.table(
  `N` = nrow(d_pre_SepN_In_Trail),
  `Mean ORD Error` = mean(d_pre_SepN_In_Trail$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
  `Count >1NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
  `Count >0.5NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
  `Count >0NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
  `>1NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0.5NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  # `Modelled Count >1NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
  # `Modelled Count >0.5NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
  # `Modelled Count >0NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
  # `Modelled >1NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  # `Modelled >0.5NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  # `Modelled >0NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper()
)
fwrite(Performance_Overall_In_Trail_1DME, file = file.path(out_data, "Performance_Overall_In_Trail_1DME.csv"))

Performance_Overall_In_Trail_THR <- data.table(
  `N` = nrow(d_pre_SepN_In_Trail),
  `Mean ORD Error` = mean(d_pre_SepN_In_Trail$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
  `Count >1NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
  `Count >0.5NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
  `Count >0NM` = table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
  `>1NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0.5NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0NM Error` = (table(d_pre_SepN_In_Trail$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_pre_SepN_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper()
)
fwrite(Performance_Overall_In_Trail_THR, file = file.path(out_data, "Performance_Overall_In_Trail_THR.csv"))

Performance_Overall_Not_In_Trail_1DME <- data.table(
  `N` = nrow(d_pre_SepN_Not_In_Trail),
  `Mean ORD Error` = mean(d_pre_SepN_Not_In_Trail$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
  `Count >1NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
  `Count >0.5NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
  `Count >0NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
  `>1NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0.5NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  # `Modelled Count >1NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
  # `Modelled Count >0.5NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
  # `Modelled Count >0NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
  # `Modelled >1NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  # `Modelled >0.5NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  # `Modelled >0NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper()
)
fwrite(Performance_Overall_Not_In_Trail_1DME, file = file.path(out_data, "Performance_Overall_Not_In_Trail_1DME.csv"))

Performance_Overall_Not_In_Trail_THR <- data.table(
  `N` = nrow(d_pre_SepN_Not_In_Trail),
  `Mean ORD Error` = mean(d_pre_SepN_Not_In_Trail$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
  `Count >1NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
  `Count >0.5NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
  `Count >0NM` = table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
  `>1NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0.5NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0NM Error` = (table(d_pre_SepN_Not_In_Trail$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_pre_SepN_Not_In_Trail)) %>% formatC(., format = "e", digits = 2) %>% toupper()
)
fwrite(Performance_Overall_Not_In_Trail_THR, file = file.path(out_data, "Performance_Overall_Not_In_Trail_THR.csv"))

Performance_Overall_In_Trail_WakePair_1DME <- data.table(
  `N` = nrow(d_pre_SepN_In_Trail_WakePair),
  `Mean ORD Error` = mean(d_pre_SepN_In_Trail_WakePair$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
  `Count >1NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
  `Count >0.5NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
  `Count >0NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
  `>1NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0.5NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  # `Modelled Count >1NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
  # `Modelled Count >0.5NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
  # `Modelled Count >0NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
  # `Modelled >1NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  # `Modelled >0.5NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  # `Modelled >0NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper()
)
fwrite(Performance_Overall_In_Trail_WakePair_1DME, file = file.path(out_data, "Performance_Overall_In_Trail_WakePair_1DME.csv"))

Performance_Overall_In_Trail_WakePair_THR <- data.table(
  `N` = nrow(d_pre_SepN_In_Trail_WakePair),
  `Mean ORD Error` = mean(d_pre_SepN_In_Trail_WakePair$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
  `Count >1NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
  `Count >0.5NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
  `Count >0NM` = table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
  `>1NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0.5NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
  `>0NM Error` = (table(d_pre_SepN_In_Trail_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_pre_SepN_In_Trail_WakePair)) %>% formatC(., format = "e", digits = 2) %>% toupper()
)
fwrite(Performance_Overall_In_Trail_WakePair_THR, file = file.path(out_data, "Performance_Overall_In_Trail_WakePair_THR.csv"))

Performance_WTC_In_Trail_1DME <- rbindlist(lapply(LETTERS[1:7], function(x) {
  d_x <- d_pre_SepN_In_Trail[Leader_RECAT == x]
  return(data.table(
    `Leader WTC` = x,
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
    # `Modelled Count >1NM` = table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
    # `Modelled Count >0.5NM` = table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
    # `Modelled Count >0NM` = table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
    # `Modelled >1NM Error` = (table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0.5NM Error` = (table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0NM Error` = (table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_WTC_In_Trail_1DME, file = file.path(out_data, "Performance_WTC_In_Trail_1DME.csv"))

Performance_WTC_In_Trail_THR <- rbindlist(lapply(LETTERS[1:7], function(x) {
  d_x <- d_pre_SepN_In_Trail[Leader_RECAT == x]
  return(data.table(
    `Leader WTC` = x,
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_WTC_In_Trail_THR, file = file.path(out_data, "Performance_WTC_In_Trail_THR.csv"))

Performance_Runway_In_Trail_1DME <- rbindlist(lapply(sort(unique(d_pre_SepN$Landing_Runway)), function(x) {
  d_x <- d_pre_SepN_In_Trail[Landing_Runway == x]
  return(data.table(
    `Rwy` = substring(x, 2),
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
    # `Modelled Count >1NM` = table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
    # `Modelled Count >0.5NM` = table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
    # `Modelled Count >0NM` = table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
    # `Modelled >1NM Error` = (table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0.5NM Error` = (table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0NM Error` = (table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Runway_In_Trail_1DME, file = file.path(out_data, "Performance_Runway_In_Trail_1DME.csv"))

Performance_Runway_In_Trail_THR <- rbindlist(lapply(sort(unique(d_pre_SepN$Landing_Runway)), function(x) {
  d_x <- d_pre_SepN_In_Trail[Landing_Runway == x]
  return(data.table(
    `Rwy` = substring(x, 2),
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Runway_In_Trail_THR, file = file.path(out_data, "Performance_Runway_In_Trail_THR.csv"))

Performance_Runway_Not_In_Trail_1DME <- rbindlist(lapply(sort(unique(d_pre_SepN$Landing_Runway)), function(x) {
  d_x <- d_pre_SepN_Not_In_Trail[Landing_Runway == x]
  return(data.table(
    `Rwy` = substring(x, 2),
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
    # `Modelled Count >1NM` = table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
    # `Modelled Count >0.5NM` = table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
    # `Modelled Count >0NM` = table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
    # `Modelled >1NM Error` = (table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0.5NM Error` = (table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0NM Error` = (table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Runway_Not_In_Trail_1DME, file = file.path(out_data, "Performance_Runway_Not_In_Trail_1DME.csv"))

Performance_Runway_Not_In_Trail_THR <- rbindlist(lapply(sort(unique(d_pre_SepN$Landing_Runway)), function(x) {
  d_x <- d_pre_SepN_Not_In_Trail[Landing_Runway == x]
  return(data.table(
    `Rwy` = substring(x, 2),
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Runway_Not_In_Trail_THR, file = file.path(out_data, "Performance_Runway_Not_In_Trail_THR.csv"))

Performance_Actype_1DME <- rbindlist(lapply(actypes, function(x) {
  d_x <- d_pre_SepN_In_Trail[Leader_Aircraft_Type == x]
  return(data.table(
    `Leader` = x,
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
    # `Modelled Count >1NM` = table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
    # `Modelled Count >0.5NM` = table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
    # `Modelled Count >0NM` = table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
    # `Modelled >1NM Error` = (table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0.5NM Error` = (table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0NM Error` = (table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Actype_1DME, file = file.path(out_data, "Performance_Actype_1DME.csv"))

Performance_Actype_THR <- rbindlist(lapply(actypes, function(x) {
  d_x <- d_pre_SepN_In_Trail[Leader_Aircraft_Type == x]
  return(data.table(
    `Leader` = x,
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Actype_THR, file = file.path(out_data, "Performance_Actype_THR.csv"))

if (include_density == T){
  Performance_Actype_Density_1DME <- rbindlist(lapply(actypes, function(x) {
    d_x <- d_pre_SepN_In_Trail[Leader_Aircraft_Type == x]
    d_x2 <- d_post_SepN_In_Trail[Leader_Aircraft_Type == x]
    if (length(d_x2$ORD_Compression_Error_1DME %>% .[!is.na(.)]) > 1) {
      ORD_1DME_Error_x_Density <- density(d_x2$ORD_Compression_Error_1DME %>% .[!is.na(.)], n = 1e6)
      ORD_1DME_Error_x_Density_1_1DME <- density_area(ORD_1DME_Error_x_Density, 1, Inf)
      ORD_1DME_Error_x_Density_05_1DME <- density_area(ORD_1DME_Error_x_Density, 0.5, Inf)
      ORD_1DME_Error_x_Density_0_1DME <- density_area(ORD_1DME_Error_x_Density, 0, Inf)
    } else {
      ORD_1DME_Error_x_Density_1_1DME <- 0
      ORD_1DME_Error_x_Density_05_1DME <- 0
      ORD_1DME_Error_x_Density_0_1DME <- 0
    }
    message("Completed ", x, " density calculation (", which(actypes == x), "/", length(actypes), ")")
    return(data.table(
      `Leader` = x,
      `N` = nrow(d_x),
      `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>1NM Density` = ORD_1DME_Error_x_Density_1_1DME %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0.5NM Density` = ORD_1DME_Error_x_Density_05_1DME %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0NM Density` = ORD_1DME_Error_x_Density_0_1DME %>% formatC(., format = "e", digits = 2) %>% toupper()
    ))
  }))
  fwrite(Performance_Actype_Density_1DME, file = file.path(out_data, "Performance_Actype_Density_1DME.csv"))
  
  
  Performance_Actype_Density_THR <- rbindlist(lapply(actypes, function(x) {
    d_x <- d_pre_SepN_In_Trail[Leader_Aircraft_Type == x]
    d_x2 <- d_post_SepN_In_Trail[Leader_Aircraft_Type == x]
    if (length(d_x2$ORD_Compression_Error %>% .[!is.na(.)]) > 1) {
      ORD_THR_Error_x_Density <- density(d_x2$ORD_Compression_Error %>% .[!is.na(.)], n = 1e6)
      ORD_THR_Error_x_Density_1 <- density_area(ORD_THR_Error_x_Density, 1, Inf)
      ORD_THR_Error_x_Density_05 <- density_area(ORD_THR_Error_x_Density, 0.5, Inf)
      ORD_THR_Error_x_Density_0 <- density_area(ORD_THR_Error_x_Density, 0, Inf)
    } else {
      ORD_THR_Error_x_Density_1 <- 0
      ORD_THR_Error_x_Density_05 <- 0
      ORD_THR_Error_x_Density_0 <- 0
    }
    message("Completed ", x, " density calculation (", which(actypes == x), "/", length(actypes), ")")
    return(data.table(
      `Leader` = x,
      `N` = nrow(d_x),
      `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>1NM Density` = ORD_THR_Error_x_Density_1 %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0.5NM Density` = ORD_THR_Error_x_Density_05 %>% formatC(., format = "e", digits = 2) %>% toupper(),
      `>0NM Density` = ORD_THR_Error_x_Density_0 %>% formatC(., format = "e", digits = 2) %>% toupper()
    ))
  }))
  fwrite(Performance_Actype_Density_THR, file = file.path(out_data, "Performance_Actype_Density_THR.csv"))
}

surface_headwind_groups <- unique(d_pre_SepN_In_Trail$Observed_AGI_Surface_Headwind_Group)

Performance_Surface_Headwind_Group_1DME <- rbindlist(lapply(surface_headwind_groups, function(x) {
  d_x <- d_pre_SepN_In_Trail[Observed_AGI_Surface_Headwind_Group == x]
  return(data.table(
    `Band` = x,
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error_1DME, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
    # `Modelled Count >1NM` = table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]],
    # `Modelled Count >0.5NM` = table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]],
    # `Modelled Count >0NM` = table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]],
    # `Modelled >1NM Error` = (table(d_x$Thresh_Accuracy_1_1DME)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0.5NM Error` = (table(d_x$Thresh_Accuracy_05_1DME)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    # `Modelled >0NM Error` = (table(d_x$Thresh_Accuracy_0_1DME)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Surface_Headwind_Group_1DME, file = file.path(out_data, "Performance_Surface_Headwind_Group_1DME.csv"))

Performance_Surface_Headwind_Group_THR <- rbindlist(lapply(surface_headwind_groups, function(x) {
  d_x <- d_pre_SepN_In_Trail[Observed_AGI_Surface_Headwind_Group == x]
  return(data.table(
    `Band` = x,
    `N` = nrow(d_x),
    `Mean ORD Error` = mean(d_x$ORD_Compression_Error, na.rm = T) %>% round(., digits = 2),
    `Count >1NM` = table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
    `Count >0.5NM` = table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
    `Count >0NM` = table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
    `>1NM Error` = (table(d_x$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0.5NM Error` = (table(d_x$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper(),
    `>0NM Error` = (table(d_x$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / nrow(d_x)) %>% formatC(., format = "e", digits = 2) %>% toupper()
  ))
}))
fwrite(Performance_Surface_Headwind_Group_THR, file = file.path(out_data, "Performance_Surface_Headwind_Group_THR.csv"))

# ORD Large Errors (> 0.5NM) in order of decreasing 1DME ORD error
ORD_Large_Errors_1DME <- d_pre_SepN[Thresh_Accuracy_Perfect_05_1DME == "[0.5NM, Inf)"][order(ORD_Compression_Error_1DME, decreasing = T)]
fwrite(ORD_Large_Errors_1DME, file = file.path(out_data, "ORD_Large_Errors_1DME.csv"))

# ORD Large Errors (> 0.5NM) in order of decreasing THR ORD error
ORD_Large_Errors_THR <- d_pre_SepN[Thresh_Accuracy_Perfect_05 == "[0.5NM, Inf)"][order(ORD_Compression_Error, decreasing = T)]
fwrite(ORD_Large_Errors_THR, file = file.path(out_data, "ORD_Large_Errors_THR.csv"))

# ----------------------------------------------------------------------- #
# 6. Summary Statistics ---------------------------------------------------
# ----------------------------------------------------------------------- #

original_data1 <- data1
original_data2 <- data2


# MC Add 29/9.  Additional Graphs for the Analysis

qile <- quantile(original_data2$Modelled_Thresh_Accuracy_Rounded, 0.001 / operational_hour_multiplier) + 0.05

# Histogram of Modelled Threshold Separation Accuracy

png(filename = file.path(out_data, "Threshold SepN Accuracy.png"), width = 900, height = 600)
p <-ggplot(data = original_data2)+
  geom_histogram(mapping = aes(x = Modelled_Thresh_Accuracy_Rounded, y = ..density..), binwidth = 0.1, fill = "skyblue", color = "black")+
  geom_vline(xintercept = -0.45)+
  geom_text(x = -0.45, y = 0, label = "0.5NM", vjust = -1)+
  geom_vline(xintercept = qile, linetype = 2)+
  geom_text(x = qile, y = 0, label = "Target", vjust = -1)+
  scale_x_continuous(breaks = seq(-1, 3, 0.5), limits = c(-1, 3))+
  labs(title = "Histogram of Modelled Threshold Accuracy", x = "Modelled Threshold SepN Accuracy (NM)")+
  theme_bw()
print(p)
dev.off()

# Histogram of Threshold ORD Compression Accuracy

png(filename = file.path(out_data, "Threshold ORD Error.png"), width = 900, height = 600)
p<-ggplot(data = original_data2)+
  geom_histogram(mapping = aes(x = ORD_Compression_Error_Rounded, y = ..density..), binwidth = 0.1, fill = "springgreen", color = "black")+
  geom_vline(xintercept = 0.45)+
  geom_text(x = 0.45, y = 0, label = "0.5NM", vjust = -1)+
  scale_x_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1))+
  labs(title = "Histogram of Threshold ORD Accuracy (NM)", x = "Threshold ORD Accuracy (NM)")+
  theme_bw()
print(p)
dev.off()

# Boxplot of Threshold ORD Compression Accuracy by Aircraft Type (10 Obs+)

summary_obs <- group_by(original_data2, Leader_Aircraft_Type) %>% summarise(TotalN = n()) %>% ungroup()
original_data2_join <- inner_join(original_data2, summary_obs)

png(filename = file.path(out_data, "Boxplot ORD Error AC Type.png"), width = 1100, height = 600)
p<-ggplot(data = filter(original_data2_join, TotalN >= 10))+
  geom_boxplot(aes(x=reorder(Leader_Aircraft_Type, ORD_Compression_Error), y = ORD_Compression_Error))+
  scale_y_continuous(breaks = seq(-2, 1, 0.5), limits = c(-2, 1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Boxplot of ORD Threshold Error", x = "Leader Aircraft Type", y = "ORD Threshold Erro (NM)")
print(p)
dev.off() 

rm(original_data2_join)

# Histogram of Actual Threshold Separation Accuracy (to ICAO) 

png(filename = file.path(out_data, "Historic Threshold SepN Accuracy.png"), width = 900, height = 600)
p<-ggplot(data = original_data2)+
  geom_histogram(mapping = aes(x = Required_Thresh_Separation_Accuracy_Rounded, y = ..density..), binwidth = 0.1, fill = "thistle", color = "black")+
  geom_vline(xintercept = -0.45)+
  geom_text(x = -0.45, y = 0, label = "0.5NM", vjust = -1)+
  scale_x_continuous(breaks = seq(-1, 3, 0.5), limits = c(-1, 3))+
  labs(title = "Histogram of Historic Threshold Accuracy", x = "Historic Threshold SepN Accuracy (NM)")+
  theme_bw()
print(p)
dev.off()

# Comparison of before and after IA

p0 <-ggplot(data = original_data2)+
  geom_histogram(mapping = aes(x = Modelled_Thresh_Accuracy_Rounded, y = ..density..), binwidth = 0.1, fill = "skyblue", color = "black")+
  geom_vline(xintercept = -0.45)+
  geom_text(x = -0.45, y = 0, label = "0.5NM", vjust = -1)+
  scale_x_continuous(breaks = seq(-1, 3, 0.5), limits = c(-1, 3))+
  labs(title = "Histogram of Modelled Threshold Accuracy", x = "Threshold SepN Accuracy (NM)")+
  theme_bw()+
  facet_wrap(~RECAT_Wake)

p1<-ggplot(data = original_data2)+
  geom_histogram(mapping = aes(x = Required_Thresh_Separation_Accuracy_Rounded, y = ..density..), binwidth = 0.1, fill = "thistle", color = "black")+
  geom_vline(xintercept = -0.45)+
  geom_text(x = -0.45, y = 0, label = "0.5NM", vjust = -1)+
  scale_x_continuous(breaks = seq(-1, 3, 0.5), limits = c(-1, 3))+
  labs(title = "Histogram of Historic Threshold Accuracy", x = "Threshold SepN Accuracy (NM)")+
  theme_bw()+
  facet_wrap(~Legacy_Wake)

png(filename = file.path(out_data, "SepN Accuracy Comparison.png"), width = 900, height = 600)
p <- grid.arrange(p0, p1, nrow = 2)
print(p)
dev.off()

# End Add

performance_measures <- c(
  "Mean ORD THR Error",
  "ORD THR 0NM Error N",
  "ORD THR 0NM Error Rate",
  "ORD THR 0.5NM Error N",
  "ORD THR 0.5NM Error Rate",
  "ORD THR 1.0NM Error N",
  "ORD THR 1.0NM Error Rate",
  "Est THR 0NM Error N",
  "Est THR 0NM Error Rate",
  "Est THR 0.5NM Error N",
  "Est THR 0.5NM Error Rate",
  "Est THR 1.0NM Error N",
  "Est THR 1.0NM Error Rate",
  "Mean ORD 1DME Error",
  "ORD 1DME 0NM Error N",
  "ORD 1DME 0NM Error Rate",
  "ORD 1DME 0.5NM Error N",
  "ORD 1DME 0.5NM Error Rate",
  "ORD 1DME 1.0NM Error N",
  "ORD 1DME 1.0NM Error Rate",
  "Est 1DME 0NM Error N",
  "Est 1DME 0NM Error Rate",
  "Est 1DME 0.5NM Error N",
  "Est 1DME 0.5NM Error Rate",
  "Est 1DME 1.0NM Error N",
  "Est 1DME 1.0NM Error Rate"
)

wake_cats <- LETTERS[1:7]

wb <- createBook()

for (landing_pair_type in c("All Trails", "In Trail", "Not In Trail")) {

  message("Generating ", landing_pair_type, " sheets...")

  # Filter data by landing pair type

  if (landing_pair_type == "All Trails") {
    data1 <- original_data1
    data2 <- original_data2
  } else if (landing_pair_type == "In Trail") {
    data1 <- original_data1[Landing_Pair_Type %in% c("In_Trail", "In_Trail_Non_Sequential")]
    data2 <- original_data2[Landing_Pair_Type %in% c("In_Trail", "In_Trail_Non_Sequential")]
  } else if (landing_pair_type == "Not In Trail") {
    data1 <- original_data1[Landing_Pair_Type %in% c("Not_In_Trail")]
    data2 <- original_data2[Landing_Pair_Type %in% c("Not_In_Trail")]
  }

  # Get wake pair only data

  data1_WakePair <- data1[paste(Leader_RECAT, Follower_RECAT) %in% paste(ref_wake$Leader_WTC, ref_wake$Follower_WTC)]
  data2_WakePair <- data2[paste(Leader_RECAT, Follower_RECAT) %in% paste(ref_wake$Leader_WTC, ref_wake$Follower_WTC)]

  # Get non-wake pair only data
  
  data1_NonWakePair <- data1[!(paste(Leader_RECAT, Follower_RECAT) %in% paste(ref_wake$Leader_WTC, ref_wake$Follower_WTC))]
  data2_NonWakePair <- data2[!(paste(Leader_RECAT, Follower_RECAT) %in% paste(ref_wake$Leader_WTC, ref_wake$Follower_WTC))]
  
  # Overall performance measures

  Mean_ORD_THR_Error <- mean(data2$ORD_Compression_Error, na.rm = T)
  ORD_THR_0NM_Error <- table(data2$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]]
  ORD_THR_0NM_Error_Rate <- table(data2$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / sum(table(data2$Thresh_Accuracy_Perfect_0))
  ORD_THR_0.5NM_Error <- table(data2$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]]
  ORD_THR_0.5NM_Error_Rate <- table(data2$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / sum(table(data2$Thresh_Accuracy_Perfect_05))
  ORD_THR_1.0NM_Error <- table(data2$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]]
  ORD_THR_1.0NM_Error_Rate <- table(data2$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / sum(table(data2$Thresh_Accuracy_Perfect_1))
  Est_THR_0NM_Error <- table(data1$Thresh_Accuracy_0)[["(-Inf, 0NM]"]]
  Est_THR_0NM_Error_Rate <- table(data1$Thresh_Accuracy_0)[["(-Inf, 0NM]"]]/ sum(table(data1$Thresh_Accuracy_0))
  Est_THR_0.5NM_Error <- table(data1$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]]
  Est_THR_0.5NM_Error_Rate <- table(data1$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]] / sum(table(data1$Thresh_Accuracy_05))
  Est_THR_1.0NM_Error <- table(data1$Thresh_Accuracy_1)[["(-Inf, -1NM]"]]
  Est_THR_1.0NM_Error_Rate <- table(data1$Thresh_Accuracy_1)[["(-Inf, -1NM]"]] / sum(table(data1$Thresh_Accuracy_1))
  Mean_ORD_1DME_Error <- mean(data2$ORD_Compression_Error_1DME, na.rm = T)
  ORD_1DME_0NM_Error <- table(data2$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]]
  ORD_1DME_0NM_Error_Rate <- table(data2$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / sum(table(data2$Thresh_Accuracy_Perfect_0_1DME))
  ORD_1DME_0.5NM_Error <- table(data2$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]]
  ORD_1DME_0.5NM_Error_Rate <- table(data2$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / sum(table(data2$Thresh_Accuracy_Perfect_05_1DME))
  ORD_1DME_1.0NM_Error <- table(data2$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]]
  ORD_1DME_1.0NM_Error_Rate <- table(data2$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / sum(table(data2$Thresh_Accuracy_Perfect_1_1DME))
  Est_1DME_0NM_Error <- table(data1$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]]
  Est_1DME_0NM_Error_Rate <- table(data1$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]]/ sum(table(data1$Thresh_Accuracy_0_1DME))
  Est_1DME_0.5NM_Error <- table(data1$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]]
  Est_1DME_0.5NM_Error_Rate <- table(data1$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]] / sum(table(data1$Thresh_Accuracy_05_1DME))
  Est_1DME_1.0NM_Error <- table(data1$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]]
  Est_1DME_1.0NM_Error_Rate <- table(data1$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]] / sum(table(data1$Thresh_Accuracy_1_1DME))
  # ORD_1DME_Error_Density <- density(data1$ORD_Compression_Error_1DME %>% .[!is.na(.)], n = 1e6)
  # Est_1DME_0NM_Error_Rate <- density_area(ORD_1DME_Error_Density, 0, Inf)
  # Est_1DME_0.5NM_Error_Rate <- density_area(ORD_1DME_Error_Density, 0.5, Inf)
  # Est_1DME_1.0NM_Error_Rate  <- density_area(ORD_1DME_Error_Density, 1, Inf)

  Mean_ORD_THR_Error_WakePair <- mean(data2_WakePair$ORD_Compression_Error, na.rm = T)
  ORD_THR_0NM_Error_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]]
  ORD_THR_0NM_Error_Rate_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / sum(table(data2_WakePair$Thresh_Accuracy_Perfect_0))
  ORD_THR_0.5NM_Error_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]]
  ORD_THR_0.5NM_Error_Rate_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / sum(table(data2_WakePair$Thresh_Accuracy_Perfect_05))
  ORD_THR_1.0NM_Error_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]]
  ORD_THR_1.0NM_Error_Rate_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / sum(table(data2_WakePair$Thresh_Accuracy_Perfect_1))
  Est_THR_0NM_Error_WakePair <- table(data1_WakePair$Thresh_Accuracy_0)[["(-Inf, 0NM]"]]
  Est_THR_0NM_Error_Rate_WakePair <- table(data1_WakePair$Thresh_Accuracy_0)[["(-Inf, 0NM]"]]/ sum(table(data1_WakePair$Thresh_Accuracy_0))
  Est_THR_0.5NM_Error_WakePair <- table(data1_WakePair$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]]
  Est_THR_0.5NM_Error_Rate_WakePair <- table(data1_WakePair$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]] / sum(table(data1_WakePair$Thresh_Accuracy_05))
  Est_THR_1.0NM_Error_WakePair <- table(data1_WakePair$Thresh_Accuracy_1)[["(-Inf, -1NM]"]]
  Est_THR_1.0NM_Error_Rate_WakePair <- table(data1_WakePair$Thresh_Accuracy_1)[["(-Inf, -1NM]"]] / sum(table(data1_WakePair$Thresh_Accuracy_1))
  Mean_ORD_1DME_Error_WakePair <- mean(data2_WakePair$ORD_Compression_Error_1DME, na.rm = T)
  ORD_1DME_0NM_Error_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]]
  ORD_1DME_0NM_Error_Rate_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / sum(table(data2_WakePair$Thresh_Accuracy_Perfect_0_1DME))
  ORD_1DME_0.5NM_Error_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]]
  ORD_1DME_0.5NM_Error_Rate_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / sum(table(data2_WakePair$Thresh_Accuracy_Perfect_05_1DME))
  ORD_1DME_1.0NM_Error_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]]
  ORD_1DME_1.0NM_Error_Rate_WakePair <- table(data2_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / sum(table(data2_WakePair$Thresh_Accuracy_Perfect_1_1DME))
  Est_1DME_0NM_Error_WakePair <- table(data1_WakePair$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]]
  Est_1DME_0NM_Error_Rate_WakePair <- table(data1_WakePair$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]]/ sum(table(data1_WakePair$Thresh_Accuracy_0_1DME))
  Est_1DME_0.5NM_Error_WakePair <- table(data1_WakePair$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]]
  Est_1DME_0.5NM_Error_Rate_WakePair <- table(data1_WakePair$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]] / sum(table(data1_WakePair$Thresh_Accuracy_05_1DME))
  Est_1DME_1.0NM_Error_WakePair <- table(data1_WakePair$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]]
  Est_1DME_1.0NM_Error_Rate_WakePair <- table(data1_WakePair$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]] / sum(table(data1_WakePair$Thresh_Accuracy_1_1DME))
  # ORD_1DME_Error_WakePair_Density <- density(data1_WakePair$ORD_Compression_Error_1DME %>% .[!is.na(.)], n = 1e6)
  # Est_1DME_0NM_Error_Rate_WakePair <- density_area(ORD_1DME_Error_WakePair_Density, 0, Inf)
  # Est_1DME_0.5NM_Error_Rate_WakePair <- density_area(ORD_1DME_Error_WakePair_Density, 0.5, Inf)
  # Est_1DME_1.0NM_Error_Rate_WakePair  <- density_area(ORD_1DME_Error_WakePair_Density, 1, Inf)

  Overall_Perf <- data.table(
    `Performance Measure` = performance_measures,
    `Per Indicator` = c(
      Mean_ORD_THR_Error,
      ORD_THR_0NM_Error,
      ORD_THR_0NM_Error_Rate,
      ORD_THR_0.5NM_Error,
      ORD_THR_0.5NM_Error_Rate,
      ORD_THR_1.0NM_Error,
      ORD_THR_1.0NM_Error_Rate,
      Est_THR_0NM_Error,
      Est_THR_0NM_Error_Rate,
      Est_THR_0.5NM_Error,
      Est_THR_0.5NM_Error_Rate,
      Est_THR_1.0NM_Error,
      Est_THR_1.0NM_Error_Rate,
      Mean_ORD_1DME_Error,
      ORD_1DME_0NM_Error,
      ORD_1DME_0NM_Error_Rate,
      ORD_1DME_0.5NM_Error,
      ORD_1DME_0.5NM_Error_Rate,
      ORD_1DME_1.0NM_Error,
      ORD_1DME_1.0NM_Error_Rate,
      Est_1DME_0NM_Error,
      Est_1DME_0NM_Error_Rate,
      Est_1DME_0.5NM_Error,
      Est_1DME_0.5NM_Error_Rate,
      Est_1DME_1.0NM_Error,
      Est_1DME_1.0NM_Error_Rate
    )
  )
  
  Overall_Perf$`Per Operational Hour` <- Overall_Perf$`Per Indicator` * operational_hour_multiplier

  Overall_Perf_WakePair <- data.table(
    `Performance Measure` = performance_measures,
    `Per Indicator` = c(
      Mean_ORD_THR_Error_WakePair,
      ORD_THR_0NM_Error_WakePair,
      ORD_THR_0NM_Error_Rate_WakePair,
      ORD_THR_0.5NM_Error_WakePair,
      ORD_THR_0.5NM_Error_Rate_WakePair,
      ORD_THR_1.0NM_Error_WakePair,
      ORD_THR_1.0NM_Error_Rate_WakePair,
      Est_THR_0NM_Error_WakePair,
      Est_THR_0NM_Error_Rate_WakePair,
      Est_THR_0.5NM_Error_WakePair,
      Est_THR_0.5NM_Error_Rate_WakePair,
      Est_THR_1.0NM_Error_WakePair,
      Est_THR_1.0NM_Error_Rate_WakePair,
      Mean_ORD_1DME_Error_WakePair,
      ORD_1DME_0NM_Error_WakePair,
      ORD_1DME_0NM_Error_Rate_WakePair,
      ORD_1DME_0.5NM_Error_WakePair,
      ORD_1DME_0.5NM_Error_Rate_WakePair,
      ORD_1DME_1.0NM_Error_WakePair,
      ORD_1DME_1.0NM_Error_Rate_WakePair,
      Est_1DME_0NM_Error_WakePair,
      Est_1DME_0NM_Error_Rate_WakePair,
      Est_1DME_0.5NM_Error_WakePair,
      Est_1DME_0.5NM_Error_Rate_WakePair,
      Est_1DME_1.0NM_Error_WakePair,
      Est_1DME_1.0NM_Error_Rate_WakePair
    )
  )
  
  Overall_Perf_WakePair$`Per Operational Hour` <- Overall_Perf_WakePair$`Per Indicator` * operational_hour_multiplier

  # ICAO 7 WTC performance measures

  Wake_Perf <- data.table(`Performance Measure` = performance_measures)

  Wake_Perf_WakePair <- data.table(`Performance Measure` = performance_measures)

  for (n in wake_cats) {

    data1_n <- data1[Leader_RECAT == n]
    data2_n <- data2[Leader_RECAT == n]

    data1_n_WakePair <- data1_WakePair[Leader_RECAT == n]
    data2_n_WakePair <- data2_WakePair[Leader_RECAT == n]

    # if (length(data1_n$ORD_Compression_Error_1DME %>% .[!is.na(.)]) > 1) {
    #   ORD_1DME_Error_n_Density <- density(data1_n$ORD_Compression_Error_1DME %>% .[!is.na(.)], n = 1e6)
    #   ORD_1DME_Error_n_Density_0_1DME <- density_area(ORD_1DME_Error_n_Density, 0, Inf)
    #   ORD_1DME_Error_n_Density_05_1DME <- density_area(ORD_1DME_Error_n_Density, 0.5, Inf)
    #   ORD_1DME_Error_n_Density_1_1DME <- density_area(ORD_1DME_Error_n_Density, 1, Inf)
    # } else {
    #   ORD_1DME_Error_n_Density_0_1DME <- 0
    #   ORD_1DME_Error_n_Density_05_1DME <- 0
    #   ORD_1DME_Error_n_Density_1_1DME <- 0
    # }
    # 
    # if (length(data1_n_WakePair$ORD_Compression_Error_1DME %>% .[!is.na(.)]) > 1) {
    #   ORD_1DME_Error_n_WakePair_Density <- density(data1_n_WakePair$ORD_Compression_Error_1DME %>% .[!is.na(.)], n = 1e6)
    #   ORD_1DME_Error_n_WakePair_Density_0_1DME <- density_area(ORD_1DME_Error_n_Density, 0, Inf)
    #   ORD_1DME_Error_n_WakePair_Density_05_1DME <- density_area(ORD_1DME_Error_n_Density, 0.5, Inf)
    #   ORD_1DME_Error_n_WakePair_Density_1_1DME <- density_area(ORD_1DME_Error_n_Density, 1, Inf)
    # } else {
    #   ORD_1DME_Error_n_WakePair_Density_0_1DME <- 0
    #   ORD_1DME_Error_n_WakePair_Density_05_1DME <- 0
    #   ORD_1DME_Error_n_WakePair_Density_1_1DME <- 0
    # }

    Wake_Perf[[n]] <- c(
      mean(data2_n$ORD_Compression_Error, na.rm = T),
      table(data2_n$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_0)),
      table(data2_n$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_05)),
      table(data2_n$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_1)),
      table(data1_n$Thresh_Accuracy_0)[["(-Inf, 0NM]"]],
      table(data1_n$Thresh_Accuracy_0)[["(-Inf, 0NM]"]] / sum(table(data1_n$Thresh_Accuracy_0)),
      table(data1_n$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]],
      table(data1_n$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]] / sum(table(data1_n$Thresh_Accuracy_05)),
      table(data1_n$Thresh_Accuracy_1)[["(-Inf, -1NM]"]],
      table(data1_n$Thresh_Accuracy_1)[["(-Inf, -1NM]"]] / sum(table(data1_n$Thresh_Accuracy_1)),
      mean(data2_n$ORD_Compression_Error_1DME, na.rm = T),
      table(data2_n$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_0_1DME)),
      table(data2_n$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_05_1DME)),
      table(data2_n$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_1_1DME)),
      table(data1_n$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]],
      table(data1_n$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]] / sum(table(data1_n$Thresh_Accuracy_0_1DME)),
      table(data1_n$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]],
      table(data1_n$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]] / sum(table(data1_n$Thresh_Accuracy_05_1DME)),
      table(data1_n$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]],
      table(data1_n$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]] / sum(table(data1_n$Thresh_Accuracy_1_1DME))
      # ORD_1DME_Error_n_Density_0_1DME,
      # ORD_1DME_Error_n_Density_05_1DME,
      # ORD_1DME_Error_n_Density_1_1DME
    )

    Wake_Perf_WakePair[[n]] <- c(
      mean(data2_n_WakePair$ORD_Compression_Error, na.rm = T),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_0)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_05)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_1)),
      table(data1_n_WakePair$Thresh_Accuracy_0)[["(-Inf, 0NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_0)[["(-Inf, 0NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_0)),
      table(data1_n_WakePair$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_05)),
      table(data1_n_WakePair$Thresh_Accuracy_1)[["(-Inf, -1NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_1)[["(-Inf, -1NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_1)),
      mean(data2_n_WakePair$ORD_Compression_Error_1DME, na.rm = T),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_0_1DME)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_05_1DME)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_1_1DME)),
      table(data1_n_WakePair$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_0_1DME)),
      table(data1_n_WakePair$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_05_1DME)),
      table(data1_n_WakePair$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_1_1DME))
      # ORD_1DME_Error_n_WakePair_Density_0_1DME,
      # ORD_1DME_Error_n_WakePair_Density_05_1DME,
      # ORD_1DME_Error_n_WakePair_Density_1_1DME
    )

  }

  # Aircraft type performance measures

  Type_Perf <- data.table(`Performance Measure` = performance_measures)

  Type_Perf_WakePair <- data.table(`Performance Measure` = performance_measures)

  for (n in actypes) {

    data1_n <- data1[Leader_Aircraft_Type == n]
    data2_n <- data2[Leader_Aircraft_Type == n]

    data1_n_WakePair <- data1_WakePair[Leader_Aircraft_Type == n]
    data2_n_WakePair <- data2_WakePair[Leader_Aircraft_Type == n]
    
    data1_n_NonWakePair <- data1_NonWakePair[Leader_Aircraft_Type == n]
    data2_n_NonWakePair <- data2_NonWakePair[Leader_Aircraft_Type == n]
    
    # if (length(data1_n$ORD_Compression_Error_1DME %>% .[!is.na(.)]) > 1) {
    #   ORD_1DME_Error_n_Density <- density(data1_n$ORD_Compression_Error_1DME %>% .[!is.na(.)], n = 1e6)
    #   ORD_1DME_Error_n_Density_0_1DME <- density_area(ORD_1DME_Error_n_Density, 0, Inf)
    #   ORD_1DME_Error_n_Density_05_1DME <- density_area(ORD_1DME_Error_n_Density, 0.5, Inf)
    #   ORD_1DME_Error_n_Density_1_1DME <- density_area(ORD_1DME_Error_n_Density, 1, Inf)
    # } else {
    #   ORD_1DME_Error_n_Density_0_1DME <- 0
    #   ORD_1DME_Error_n_Density_05_1DME <- 0
    #   ORD_1DME_Error_n_Density_1_1DME <- 0
    # }
    # 
    # if (length(data1_n_WakePair$ORD_Compression_Error_1DME %>% .[!is.na(.)]) > 1) {
    #   ORD_1DME_Error_n_WakePair_Density <- density(data1_n_WakePair$ORD_Compression_Error_1DME %>% .[!is.na(.)], n = 1e6)
    #   ORD_1DME_Error_n_WakePair_Density_0_1DME <- density_area(ORD_1DME_Error_n_Density, 0, Inf)
    #   ORD_1DME_Error_n_WakePair_Density_05_1DME <- density_area(ORD_1DME_Error_n_Density, 0.5, Inf)
    #   ORD_1DME_Error_n_WakePair_Density_1_1DME <- density_area(ORD_1DME_Error_n_Density, 1, Inf)
    # } else {
    #   ORD_1DME_Error_n_WakePair_Density_0_1DME <- 0
    #   ORD_1DME_Error_n_WakePair_Density_05_1DME <- 0
    #   ORD_1DME_Error_n_WakePair_Density_1_1DME <- 0
    # }

    Type_Perf[[n]] <- c(
      mean(data2_n$ORD_Compression_Error, na.rm = T),
      table(data2_n$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_0)),
      table(data2_n$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_05)),
      table(data2_n$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_1)),
      table(data1_n$Thresh_Accuracy_0)[["(-Inf, 0NM]"]],
      table(data1_n$Thresh_Accuracy_0)[["(-Inf, 0NM]"]] / sum(table(data1_n$Thresh_Accuracy_0)),
      table(data1_n$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]],
      table(data1_n$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]] / sum(table(data1_n$Thresh_Accuracy_05)),
      table(data1_n$Thresh_Accuracy_1)[["(-Inf, -1NM]"]],
      table(data1_n$Thresh_Accuracy_1)[["(-Inf, -1NM]"]] / sum(table(data1_n$Thresh_Accuracy_1)),
      mean(data2_n$ORD_Compression_Error_1DME, na.rm = T),
      table(data2_n$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_0_1DME)),
      table(data2_n$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_05_1DME)),
      table(data2_n$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
      table(data2_n$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / sum(table(data2_n$Thresh_Accuracy_Perfect_1_1DME)),
      table(data1_n$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]],
      table(data1_n$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]] / sum(table(data1_n$Thresh_Accuracy_0_1DME)),
      table(data1_n$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]],
      table(data1_n$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]] / sum(table(data1_n$Thresh_Accuracy_05_1DME)),
      table(data1_n$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]],
      table(data1_n$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]] / sum(table(data1_n$Thresh_Accuracy_1_1DME))
      # ORD_1DME_Error_n_Density_0_1DME,
      # ORD_1DME_Error_n_Density_05_1DME,
      # ORD_1DME_Error_n_Density_1_1DME
    )

    Type_Perf_WakePair[[n]] <- c(
      mean(data2_n_WakePair$ORD_Compression_Error, na.rm = T),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0)[["[0NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_0)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05)[["[0.5NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_05)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1)[["[1NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_1)),
      table(data1_n_WakePair$Thresh_Accuracy_0)[["(-Inf, 0NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_0)[["(-Inf, 0NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_0)),
      table(data1_n_WakePair$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_05)[["(-Inf, -0.5NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_05)),
      table(data1_n_WakePair$Thresh_Accuracy_1)[["(-Inf, -1NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_1)[["(-Inf, -1NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_1)),
      mean(data2_n_WakePair$ORD_Compression_Error_1DME, na.rm = T),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_0_1DME)[["[0NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_0_1DME)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_05_1DME)[["[0.5NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_05_1DME)),
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]],
      table(data2_n_WakePair$Thresh_Accuracy_Perfect_1_1DME)[["[1NM, Inf)"]] / sum(table(data2_n_WakePair$Thresh_Accuracy_Perfect_1_1DME)),
      table(data1_n_WakePair$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_0_1DME)[["(-Inf, 0NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_0_1DME)),
      table(data1_n_WakePair$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_05_1DME)[["(-Inf, -0.5NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_05_1DME)),
      table(data1_n_WakePair$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]],
      table(data1_n_WakePair$Thresh_Accuracy_1_1DME)[["(-Inf, -1NM]"]] / sum(table(data1_n_WakePair$Thresh_Accuracy_1_1DME))
      # ORD_1DME_Error_n_WakePair_Density_0_1DME,
      # ORD_1DME_Error_n_WakePair_Density_05_1DME,
      # ORD_1DME_Error_n_WakePair_Density_1_1DME
    )

  }

  ORD_Large_Errors_List <- data2[Thresh_Accuracy_Perfect_05_1DME == "[0.5NM, Inf)"]

  # ORD Summary Stats

  Summary_Stats <- data.table(
    `Stats - All Pairs` =  c(
      "ORD Compression",
      "ORD Compression Error",
      "ORD Compression Error 1DME",
      "Leader IAS Error",
      "Follower IAS Error",
      "Combined GWCS Error"
    ),
    rbind(
      stat.desc(data1$ORD_Compression),
      stat.desc(data2$ORD_Compression_Error),
      stat.desc(data2$ORD_Compression_Error_1DME),
      stat.desc(data1$ORD_Leader_IAS_Error),
      stat.desc(data2$ORD_Follower_IAS_Error),
      stat.desc(data2$Combined_GWCS_Error)
    )
  )

  Summary_Stats_WakePair <- data.table(
    `Stats - All Pairs` =  c(
      "ORD Compression",
      "ORD Compression Error",
      "ORD Compression Error 1DME",
      "Leader IAS Error",
      "Follower IAS Error",
      "Combined GWCS Error"
    ),
    rbind(
      stat.desc(data1_WakePair$ORD_Compression),
      stat.desc(data2_WakePair$ORD_Compression_Error),
      stat.desc(data2_WakePair$ORD_Compression_Error_1DME),
      stat.desc(data1_WakePair$ORD_Leader_IAS_Error),
      stat.desc(data2_WakePair$ORD_Follower_IAS_Error),
      stat.desc(data2_WakePair$Combined_GWCS_Error)
    )
  )
  
  Summary_Stats_NonWakePair <- data.table(
    `Stats - All Pairs` =  c(
      "ORD Compression",
      "ORD Compression Error",
      "ORD Compression Error 1DME",
      "Leader IAS Error",
      "Follower IAS Error",
      "Combined GWCS Error"
    ),
    rbind(
      stat.desc(data1_NonWakePair$ORD_Compression),
      stat.desc(data2_NonWakePair$ORD_Compression_Error),
      stat.desc(data2_NonWakePair$ORD_Compression_Error_1DME),
      stat.desc(data1_NonWakePair$ORD_Leader_IAS_Error),
      stat.desc(data2_NonWakePair$ORD_Follower_IAS_Error),
      stat.desc(data2_NonWakePair$Combined_GWCS_Error)
    )
  )

  # Generate workbook

  # Sheet 1

  wb_1 <- createSheet(wb, sheetName = paste(landing_pair_type, "Pairs - Tables"))

  wb_row <- 1

  writeCell(wb_1, wb_row, 1, "Overall Performance Measures")
  wb_row <- wb_row + 2

  writeCell(wb_1, wb_row, 1, "All Pairs")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Overall_Perf)
  wb_row <- wb_row + 2 + nrow(Overall_Perf)

  writeCell(wb_1, wb_row, 1, "Wake Pairs Only")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Overall_Perf_WakePair)
  wb_row <- wb_row + 2 + nrow(Overall_Perf_WakePair)

  writeCell(wb_1, wb_row, 1, "Leader ICAO 7 WTC Performance Measures")
  wb_row <- wb_row + 2

  writeCell(wb_1, wb_row, 1, "All Pairs")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Wake_Perf)
  wb_row <- wb_row + 2 + nrow(Wake_Perf)

  writeCell(wb_1, wb_row, 1, "Wake Pairs Only")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Wake_Perf_WakePair)
  wb_row <- wb_row + 2 + nrow(Wake_Perf_WakePair)

  writeCell(wb_1, wb_row, 1, "Leader Aircraft Type Performance Measures")
  wb_row <- wb_row + 2

  writeCell(wb_1, wb_row, 1, "All Pairs")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Type_Perf)
  wb_row <- wb_row + 2 + nrow(Type_Perf)

  writeCell(wb_1, wb_row, 1, "Wake Pairs Only")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Type_Perf_WakePair)
  wb_row <- wb_row + 2 + nrow(Type_Perf_WakePair)

  writeCell(wb_1, wb_row, 1, "ORD Large Errors List")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, ORD_Large_Errors_List)
  wb_row <- wb_row + 2 + nrow(ORD_Large_Errors_List)

  writeCell(wb_1, wb_row, 1, "ORD Summary Statistics")
  wb_row <- wb_row + 2

  writeCell(wb_1, wb_row, 1, "All Pairs")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Summary_Stats)
  wb_row <- wb_row + 2 + nrow(Summary_Stats)

  writeCell(wb_1, wb_row, 1, "Wake Pairs Only")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Summary_Stats_WakePair)
  wb_row <- wb_row + 2 + nrow(Summary_Stats_WakePair)

  writeCell(wb_1, wb_row, 1, "Non-Wake Pairs Only")
  wb_row <- wb_row + 1
  writeTable(wb_1, wb_row, 1, Summary_Stats_NonWakePair)
  wb_row <- wb_row + 2 + nrow(Summary_Stats_NonWakePair)
  
  # Sheet 2

  wb_2 <- createSheet(wb, sheetName = paste(landing_pair_type, "Pairs - Plots"))

  wb_row <- 1
  wb_col <- 1

  # ORD Compression Error - Overall

  if (length(data2$ORD_Compression_Error) > 0) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    tryCatch(
    hist(
      data2$ORD_Compression_Error,
      main="ORD Compression Error",
      xlab="Observed - Forecast Compression (NM)",
      xlim=c(-1.5,1.5),
      breaks = seq(-10, 10, 0.05),
      col=rgb(0,0,1,1/4),
      prob = TRUE
    ), error = function(e) {
      plot.new()
      text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
    })
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
  }
  wb_col <- wb_col + 8

  # ORD Compression Error - WTC

  for (n in wake_cats) {
    temp_img <- tempfile(fileext = ".png")
    den <- tryCatch(
      density(data2[Leader_RECAT %in% n]$ORD_Compression_Error),
      error = function(e) NA
    )
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data2[Leader_RECAT %in% n]$ORD_Compression_Error,
        main=paste(n, "- ORD Compression Error"),
        xlab="Observed - Forecast Compression (NM)",
        xlim=c(-1.5,1.5),
        breaks = seq(-10, 10, 0.05),
        col=rgb(0,0,1,1/4),
        prob = TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    if (!all(is.na(den))){
      tryCatch(lines(den))
    }
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  # ORD Compression Error - Aircraft Type

  for (n in actypes) {
    temp_img <- tempfile(fileext = ".png")
    den <- tryCatch(
      density(data2[Leader_Aircraft_Type %in% n]$ORD_Compression_Error),
      error = function(e) NA
    )
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data2[Leader_Aircraft_Type %in% n]$ORD_Compression_Error,
        main=paste(n, "- ORD Compression Error"),
        xlab="Observed - Forecast Compression (NM)",
        xlim=c(-1.5,1.5),
        breaks = seq(-10, 10, 0.05),
        col=rgb(0,0,1,1/4),
        prob = TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    if (!all(is.na(den))){
      tryCatch(lines(den))
    }
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  wb_row <- wb_row + 18
  wb_col <- 1

  # ORD Compression Error 1DME - Overall

  if (length(data2$ORD_Compression_Error_1DME) > 0) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    tryCatch(
    hist(
      data2$ORD_Compression_Error_1DME,
      main="1DME ORD Compression Error",
      xlab="Observed - Forecast Compression (NM)",
      xlim=c(-1.5,1.5),
      breaks = seq(-10, 10, 0.05),
      col=rgb(0,0,1,1/4),
      prob = TRUE
    ),error = function(e) {
      plot.new()
      text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
    })
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
  }
  wb_col <- wb_col + 8

  # ORD Compression Error 1DME - WTC

  for (n in wake_cats) {
    temp_img <- tempfile(fileext = ".png")
    den <- tryCatch(
      density(data2[Leader_RECAT %in% n]$ORD_Compression_Error_1DME),
      error = function(e) NA
    )
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data2[Leader_RECAT %in% n]$ORD_Compression_Error_1DME,
        main=paste(n, "- 1DME ORD Compression Error"),
        xlab="Observed - Forecast Compression (NM)",
        xlim=c(-1.5,1.5),
        breaks = seq(-10, 10, 0.05),
        col=rgb(0,0,1,1/4),
        prob = TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    if (!all(is.na(den))){
      tryCatch(lines(den))
    }
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  # ORD Compression Error 1DME - Aircraft Type

  for (n in actypes) {
    temp_img <- tempfile(fileext = ".png")
    den <- tryCatch(
      density(data2[Leader_Aircraft_Type %in% n]$ORD_Compression_Error_1DME),
      error = function(e) NA
    )
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data2[Leader_Aircraft_Type %in% n]$ORD_Compression_Error_1DME,
        main=paste(n, "- 1DME ORD Compression Error"),
        xlab="Observed - Forecast Compression (NM)",
        xlim=c(-1.5,1.5),
        breaks = seq(-10, 10, 0.05),
        col=rgb(0,0,1,1/4),
        prob = TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    if (!all(is.na(den))){
      tryCatch(lines(den))
    }
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  wb_row <- wb_row + 18
  wb_col <- 1

  # Leader IAS Error - Overall

  if (length(data1$ORD_Leader_IAS_Error) > 0) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    hist(
      data1$ORD_Leader_IAS_Error,
      main="Leader Mean IAS Error",
      xlab="Observed - Forecast IAS (kts)",
      xlim=c(floor(min(data1$ORD_Leader_IAS_Error, na.rm = T)), ceiling(max(data1$ORD_Leader_IAS_Error, na.rm = T))),
      breaks = seq(-100, 100, 1),
      col=rgb(0,0,1,1/4),
      prob=TRUE
    )
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
  }
  wb_col <- wb_col + 8

  # Leader IAS Error - WTC

  for (n in wake_cats) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data1[Leader_RECAT %in% n]$ORD_Leader_IAS_Error,
        main=paste(n, "- Leader Mean IAS Error"),
        xlab="Observed - Forecast IAS (kts)",
        xlim=c(floor(min(data1$ORD_Leader_IAS_Error, na.rm = T)), ceiling(max(data1$ORD_Leader_IAS_Error, na.rm = T))),
        breaks = seq(-100, 100, 1),
        col=rgb(0,0,1,1/4),
        prob=TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  # Leader IAS Error - Aircraft Type

  for (n in actypes) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data1[Leader_Aircraft_Type %in% n]$ORD_Leader_IAS_Error,
        main=paste(n, "- Leader Mean IAS Error"),
        xlab="Observed - Forecast IAS (kts)",
        xlim=c(floor(min(data1$ORD_Leader_IAS_Error, na.rm = T)), ceiling(max(data1$ORD_Leader_IAS_Error, na.rm = T))),
        breaks = seq(-100, 100, 1),
        col=rgb(0,0,1,1/4),
        prob=TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  wb_row <- wb_row + 18
  wb_col <- 1

  # Follower IAS Error - Overall

  if (length(data1$ORD_Leader_IAS_Error) > 0) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    hist(
      data1$ORD_Leader_IAS_Error,
      main="Follower Mean IAS Error",
      xlab="Observed - Forecast IAS (kts)",
      xlim=c(floor(min(data1$ORD_Follower_IAS_Error, na.rm = T)), ceiling(max(data1$ORD_Follower_IAS_Error, na.rm = T))),
      breaks = seq(-100, 100, 1),
      col=rgb(0,0,1,1/4),
      prob=TRUE
    )
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
  }
  wb_col <- wb_col + 8

  # Follower IAS Error - WTC

  for (n in wake_cats) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data1[Follower_RECAT %in% n]$ORD_Follower_IAS_Error,
        main=paste(n, "- Follower Mean IAS Error"),
        xlab="Observed - Forecast IAS (kts)",
        xlim=c(floor(min(data1$ORD_Follower_IAS_Error, na.rm = T)), ceiling(max(data1$ORD_Follower_IAS_Error, na.rm = T))),
        breaks = seq(-100, 100, 1),
        col=rgb(0,0,1,1/4),
        prob=TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  # Follower IAS Error - Aircraft Type

  for (n in actypes) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 512, height = 360)
    tryCatch(
      hist(
        data1[Follower_Aircraft_Type %in% n]$ORD_Follower_IAS_Error,
        main=paste(n, "- Follower Mean IAS Error"),
        xlab="Observed - Forecast IAS (kts)",
        xlim=c(floor(min(data1$ORD_Follower_IAS_Error, na.rm = T)), ceiling(max(data1$ORD_Follower_IAS_Error, na.rm = T))),
        breaks = seq(-100, 100, 1),
        col=rgb(0,0,1,1/4),
        prob=TRUE
      ),
      error = function(e) {
        plot.new()
        text(.5,.5, paste("No error data for", n), cex=1, col=rgb(.2,.2,.2,.7))
      }
    )
    dev.off()
    writeImage(wb_2, wb_row, wb_col, temp_img)
    wb_col <- wb_col + 8
  }

  wb_row <- wb_row + 18
  wb_col <- 1

  # # Combined GWCS Error - Overall
  #
  # temp_img <- tempfile(fileext = ".png")
  # png(temp_img, width = 512, height = 360)
  # hist(
  #   data2$Combined_GWCS_Error,
  #   main="Combined GWCS Error",
  #   xlab="Observed - Forecast GWCS (kts)",
  #   xlim=c(floor(min(data2$Combined_GWCS_Error, na.rm = T)), ceiling(max(data2$Combined_GWCS_Error, na.rm = T))),
  #   breaks=seq(-100,100,1),
  #   col=rgb(0,0,1,1/4),
  #   prob=TRUE
  # )
  # dev.off()
  # writeImage(wb_2, wb_row, wb_col, temp_img)
  # wb_col <- wb_col + 8
  #
  # # Combined GWCS Error - WTC
  #
  # for (n in wake_cats) {
  #   temp_img <- tempfile(fileext = ".png")
  #   png(temp_img, width = 512, height = 360)
  #   hist(
  #     data2[Follower_RECAT %in% n]$Combined_GWCS_Error,
  #     main=paste(n, "- Combined GWCS Error"),
  #     xlab="Observed - Forecast GWCS (kts)",
  #     xlim=c(floor(min(data2$Combined_GWCS_Error, na.rm = T)), ceiling(max(data2$Combined_GWCS_Error, na.rm = T))),
  #     breaks=seq(-100,100,1),
  #     col=rgb(0,0,1,1/4),
  #     prob=TRUE
  #   )
  #   dev.off()
  #   writeImage(wb_2, wb_row, wb_col, temp_img)
  #   wb_col <- wb_col + 8
  # }
  #
  # # Combined GWCS Error - Aircraft Type
  #
  # for (n in actypes) {
  #   temp_img <- tempfile(fileext = ".png")
  #   png(temp_img, width = 512, height = 360)
  #   hist(
  #     data2[Follower_Aircraft_Type %in% n]$Combined_GWCS_Error,
  #     main=paste(n, "- Combined GWCS Error"),
  #     xlab="Observed - Forecast GWCS (kts)",
  #     xlim=c(floor(min(data2$Combined_GWCS_Error, na.rm = T)), ceiling(max(data2$Combined_GWCS_Error, na.rm = T))),
  #     breaks=seq(-100,100,1),
  #     col=rgb(0,0,1,1/4),
  #     prob=TRUE
  #   )
  #   dev.off()
  #   writeImage(wb_2, wb_row, wb_col, temp_img)
  #   wb_col <- wb_col + 8
  # }
  #
  # wb_row <- wb_row + 18
  # wb_col <- 1

  # ORD Error 1DME by Lead Aircraft Type
  a2 <- ddply(data2, "Leader_Aircraft_Type", summarise,
              N = length(ORD_Compression_Error_1DME),
              mean = mean(ORD_Compression_Error_1DME, na.rm=TRUE),
              median = median(ORD_Compression_Error_1DME, na.rm=TRUE),
              sd = sd(ORD_Compression_Error_1DME, na.rm=TRUE),
              IQR = IQR(ORD_Compression_Error_1DME, na.rm =TRUE))
  if (nrow(a2) > 0) {
    data1$Leader_Type_Total<-a2$N[match(data1$Leader_Aircraft_Type, a2$Leader_Aircraft_Type)]
    a2_bpdata = subset(data1, Leader_Type_Total >= 10)
    a2_bpdata$Leader_Aircraft_Type.f=factor(a2_bpdata$Leader_Aircraft_Type, levels = a2$Leader_Aircraft_Type[order(a2$median)])
    a2_bpdata$Leader_Aircraft_Type.f <-droplevels(a2_bpdata$Leader_Aircraft_Type.f)
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 1024, height = 720)
    boxplot(ORD_Compression_Error_1DME ~ Leader_Aircraft_Type.f, data = a2_bpdata, main="ORD Error 1DME by Lead Aircraft Type", xlab="Lead Aircraft Type", ylab="O-F(NM)", cex.axis = 0.7, las = 2)
    dev.off()
    addPicture(temp_img, wb_2, scale = 1, startRow = wb_row, startColumn = 1)
  }

  # ORD Error 1DME by Follower Aircraft Type
  cc2_data2 <- data2
  cc2 <- ddply(cc2_data2, "Follower_Aircraft_Type", summarise,
               N = length(ORD_Compression_Error_1DME),
               mean = mean(ORD_Compression_Error_1DME, na.rm=TRUE),
               median = median(ORD_Compression_Error_1DME, na.rm=TRUE),
               sd = sd(ORD_Compression_Error_1DME, na.rm=TRUE),
               IQR = IQR(ORD_Compression_Error_1DME, na.rm =TRUE))
  if (nrow(cc2) > 0) {
    cc2_data2$Follower_Type_Total <- cc2$N[match(cc2_data2$Follower_Aircraft_Type, cc2$Follower_Aircraft_Type)]
    cc2_bpdata <- cc2_data2[Follower_Type_Total >= 10]
    cc2_bpdata$Follower_Aircraft_Type.f <- factor(cc2_bpdata$Follower_Aircraft_Type, levels = cc2$Follower_Aircraft_Type[order(cc2$median)])
    cc2_bpdata$Follower_Aircraft_Type.f <- droplevels(cc2_bpdata$Follower_Aircraft_Type.f)
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 1024, height = 720)
    boxplot(
      ORD_Compression_Error_1DME ~ Follower_Aircraft_Type.f,
      data = cc2_bpdata,
      main="ORD Error 1DME by Follower Aircraft Type",
      lab="Follower Aircraft Type",
      ylab="O-F(NM)",
      cex.axis = 0.7,
      las = 2
    )
    dev.off()
    addPicture(temp_img, wb_2, scale = 1, startRow = wb_row, startColumn = 17)
  }

  wb_row <- wb_row + 36

  # ORD Error 1DME by Operator & Lead Aircraft Type
  a3 <- ddply(data2, c("Leader_Aircraft_Type", "Leader_Operator"), summarise,
              N = length(ORD_Compression_Error_1DME),
              mean = mean(ORD_Compression_Error_1DME, na.rm=TRUE),
              median = median(ORD_Compression_Error_1DME, na.rm=TRUE),
              sd = sd(ORD_Compression_Error_1DME, na.rm=TRUE),
              IQR = IQR(ORD_Compression_Error_1DME, na.rm =TRUE))
  if (nrow(a3) > 0) {
    major_operators <- a3[2:3] %>% group_by(Leader_Operator) %>% summarise(N = sum(N)) %>% as.data.table() %>% .[N >= 10] %>% .$Leader_Operator
    a3_bpdata <- data1[Leader_Operator %in% major_operators]
    a3_bpdata$Leader_Operator_Aircraft_Type <- paste(a3_bpdata$Leader_Operator, a3_bpdata$Leader_Aircraft_Type)
    a3_bpdata$Leader_Operator_Aircraft_Type.f <- factor(a3_bpdata$Leader_Operator_Aircraft_Type, levels=unique(a3_bpdata$Leader_Operator_Aircraft_Type[order(a3_bpdata$Leader_Aircraft_Type, a3_bpdata$Leader_Operator)]))
    a3_bpdata$Leader_Operator_Aircraft_Type.f <- droplevels(a3_bpdata$Leader_Operator_Aircraft_Type.f)
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 2048, height = 720)
    boxplot(
      ORD_Compression_Error_1DME ~ Leader_Operator_Aircraft_Type.f,
      data = a3_bpdata,
      main="ORD Error 1DME by Operator & Lead Aircraft Type",
      xlab="Operator & Lead Aircraft Type",
      ylab="O-F(NM)",
      cex.axis = 0.7,
      las = 2,
      col = dodgy_colour_function(rep(sort(unique(a3_bpdata$Leader_Aircraft_Type)), as.vector(table(unlist(strsplit(names(table(a3_bpdata$Leader_Operator_Aircraft_Type.f)), split=" "))[c(F, T)]))))
    )
    dev.off()
    addPicture(temp_img, wb_2, scale = 1, startRow = wb_row, startColumn = 1)
  }

  wb_row <- wb_row + 36

  # Leader IAS Error by Lead Aircraft Type
  bb1_lead_data1 <- data1
  bb1_lead <- ddply(bb1_lead_data1, "Leader_Aircraft_Type", summarise,
                    N = length(ORD_Leader_IAS_Error),
                    mean = mean(ORD_Leader_IAS_Error, na.rm=TRUE),
                    median = median(ORD_Leader_IAS_Error, na.rm=TRUE),
                    sd = sd(ORD_Leader_IAS_Error, na.rm=TRUE),
                    IQR = IQR(ORD_Leader_IAS_Error, na.rm =TRUE))
  if (nrow(bb1_lead) > 0) {
    bb1_lead_data1$Leader_Type_Total<-bb1_lead$N[match(bb1_lead_data1$Leader_Aircraft_Type, bb1_lead$Leader_Aircraft_Type)]
    bb1_lead_bpdata <- subset(bb1_lead_data1, Leader_Type_Total >= 10)
    bb1_lead_bpdata$Leader_Aircraft_Type.f=factor(bb1_lead_bpdata$Leader_Aircraft_Type, levels = bb1_lead$Leader_Aircraft_Type[order(bb1_lead$median)])
    bb1_lead_bpdata$Leader_Aircraft_Type.f <-droplevels(bb1_lead_bpdata$Leader_Aircraft_Type.f)
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 1024, height = 720)
    boxplot(
      ORD_Leader_IAS_Error ~ Leader_Aircraft_Type.f,
      data = bb1_lead_bpdata,
      main="ORD Leader IAS Error by Lead Aircraft Type",
      xlab="Lead Aircraft Type",
      ylab="O-F(Kts)",
      cex.axis = 0.7,
      las = 2
    )
    dev.off()
    addPicture(temp_img, wb_2, scale = 1, startRow = wb_row, startColumn = 1)
  }

  # Follower IAS Error by Follower Aircraft Type
  bb1_foll_data1 <- data1
  bb1_foll <- ddply(bb1_foll_data1, "Follower_Aircraft_Type", summarise,
                    N = length(ORD_Follower_IAS_Error),
                    mean = mean(ORD_Follower_IAS_Error, na.rm=TRUE),
                    median = median(ORD_Follower_IAS_Error, na.rm=TRUE),
                    sd = sd(ORD_Follower_IAS_Error, na.rm=TRUE),
                    IQR = IQR(ORD_Follower_IAS_Error, na.rm =TRUE))
  if (nrow(bb1_foll) > 0) {
    bb1_foll_data1$Leader_Type_Total<-bb1_foll$N[match(bb1_foll_data1$Follower_Aircraft_Type, bb1_foll$Follower_Aircraft_Type)]
    bb1_foll_bpdata <- subset(bb1_foll_data1, Leader_Type_Total >= 10)
    bb1_foll_bpdata$Follower_Aircraft_Type.f <- factor(bb1_foll_bpdata$Follower_Aircraft_Type, levels = bb1_foll$Follower_Aircraft_Type[order(bb1_foll$median)])
    bb1_foll_bpdata$Follower_Aircraft_Type.f <- droplevels(bb1_foll_bpdata$Follower_Aircraft_Type.f)
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 1024, height = 720)
    boxplot(
      ORD_Leader_IAS_Error ~ Follower_Aircraft_Type.f,
      data = bb1_foll_bpdata,
      main="ORD Leader IAS Error by Follower Aircraft Type",
      xlab="Follower Aircraft Type",
      ylab="O-F(Kts)",
      cex.axis = 0.7,
      las = 2
    )
    dev.off()
    addPicture(temp_img, wb_2, scale = 1, startRow = wb_row, startColumn = 17)
  }

  wb_row <- wb_row + 36

  # Compression Error 1DME by Surface Headwind
  if (length(data2$ORD_Compression_Error_1DME) > 0) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 1024, height = 720)
    boxplot(
      ORD_Compression_Error_1DME ~ Observed_AGI_Surface_Headwind_Group,
      data = data2,
      main="ORD Compression Error 1DME by Surface Head Wind",
      xlab="Surface Headwind",
      ylab="O-F(NM)",
      cex.axis = 0.7,
      las = 2
    )
    dev.off()
    addPicture(temp_img, wb_2, scale = 1, startRow = wb_row, startColumn = 1)
  }
  
  # Compression Error 1DME by GWCS
  if (length(data2$ORD_Compression_Error_1DME) > 0) {
    temp_img <- tempfile(fileext = ".png")
    png(temp_img, width = 1024, height = 720)
    boxplot(
      ORD_Compression_Error_1DME ~ Observed_Mean_Leader_Wind_Effect_Group,
      data = data2,
      main="ORD Compression Error 1DME by GWCS",
      xlab="GWCS",
      ylab="O-F(NM)",
      cex.axis = 0.7,
      las = 2
    )
    dev.off()
    addPicture(temp_img, wb_2, scale = 1, startRow = wb_row, startColumn = 17)
  }

}

saveBook(wb, file.path(out_data, "ORD Summary Performance.xlsx"))
