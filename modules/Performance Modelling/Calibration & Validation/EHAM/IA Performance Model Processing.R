# ----------------------------------------- #
# IA Performance Model: Post-Processing
# ----------------------------------------- #

# ----------------------------------------- #
# This script is to be used after the Pre-processing script.
# This script uses the output file from the above script.
# It calculates assumed separation accuracies / "actual" separations
# It then loops over each observation and calculates Perfect/Actual time separations
# these are calculated for different scenarios as shown in the performance model plan
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

# Script versions
pre_process_version <- "v1.0.6"
version <- "v1.0.4"

# Set working directory to directory of this script (requires RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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

# Local use of main datasets (Wind segments)
local_main <- F

# Local use of Adaptation files
local_adap <- F

# Save local versions of main/adap files
save_segs <- F # Segment Data SQL Output
save_adap <- F # Adaptation SQL Output

# Local file/directory config

# Pre-processed output
predir <- file.path("Pre-Process Output", pre_process_version)

# Post process input
inpdir <- file.path("Post-Process Input")

# Reference input (for segs)
ref_folder <- "LVNL Exports 2020-11-05"
refdir <- file.path("Reference", ref_folder)

# Generate sub-directory in Output folder for this run
outdir <- file.path("Post-Process Output", version)
dir.create(outdir)

# Folder for local versions of Adaptation
adap_folder <- file.path("Pre-Process Input")

# file names for main datasets 
pm_pre_csv <- "IA_PM_Preprocessed.csv"
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

# Flag for use of Average IAS filtering
use_ias_filter <- T

# Flag for use of Achieved Separation Filter
use_achieved_sep <- F

# Flag for calculation of Under Separated Time spacings 
generate_under_seps <- F

# Method for calculating adjusted separation accuracy
# 1 = Use Mean, 2 = Use Quantiles
sep_adj_method <- 2

# Flag to use adjusted distance for A388 Leader Time Spacing calcs
A388_Adj <- F

# ----------------------------------------- #
# 2. Adaptable Parameters
# ----------------------------------------- #

# ------- Wake Cats
wake_cats <- as.data.table(c("A", "B", "C", "D", "E", "F"))
names(wake_cats) <- c("Wake")

# ------- Filtering

# Filter to only use specific dates
date_filter <- NA

# min/max values of IAS for IAS filter, As well as min dist to thresh calc considers
min_ias <- 50
max_ias <- 200
max_ias_filter_dme <- 4

# Maximum DME_Seg limit for segment flying time calculation
DME_Seg_Max <- 20

# Low Surface Wind filtering parameter (For Low surface wind histogram outputs)
low_sw_lim <- 2
low_shw_lim <- 5

# ------- Capping

# Minimum radar separation by law
radar_min_sep <- 3

# ------- Separation Adjustment Parameters

# Heathrow Mean (1) and Standard deviation (2) 4DME Separation Accuracy
mu_h <- 0.55
sigma_h <- 0.46

# Maximum allowed ICAO 0DME Separation accuracy for filtered data (only used for adjustment)
icao_max_sep_acc <- 3

# Percent of observations under 0 Separation accuracy for Heathrow
pc_under <- 0.03

# Maximum permissable under-separation
lambda <- -0.54

# ------- Time spacing calculation parameters

# Delivery distances for Actual Spacings
All_Actual <- 4
A388_Actual <- 5

# ----------------------------------------------------------------------- #
# Functions
# ----------------------------------------------------------------------- #

# Get the flying time between 2 segments
segmentFlyingTime <- function(df, start, end, units = "h") {
  # df: must be a data.frame/data.table containing two columns,
  #     with first column containing DME segment number,
  #     and second column containing average speed in segments.
  #
  # start: segment to start calculation of flying time.
  #
  # end: segment to end calculation of flying time.
  #
  # units: (use either h, m or s) the units in which the
  #        calculated flying time is returned as.
  #
  # NOTES
  # - Segment number should refer to the lower integer bound of
  #   the segment, e.g. segment number 3 refers to the segment
  #   between 3DME and 4DME.
  # - Start and end segments are right inclusive, i.e. start = 1
  #   and end = 6 will calculate flying time between 1 and 7DME,
  #   and start = 3, end = 3 will calculate between 3 and 4DME.
  if (!is.na(start)){
    if (start < end) {
      message("\nWarning: The start segment number must be greater than end segment, returning 0.\n")
      return(0)
    }
    names(df)[c(1,2)] <- c("V1", "V2")
    segs <- seq(end, start, 1)
    missing_segs <- setdiff(segs, df$V1)
    if (length(missing_segs) > 0) {
      message("\nWarning: Missing segment information for segments ", paste0(missing_segs, collapse = ", "), "\n")
      return(NA)
    }
    new <- filter(df, V1 %in% segs) %>% mutate(t = 1/V2)
    return(sum(new$t, na.rm=T))
    #return(sum(1/df[df$V1 %in% segs,]$V2, na.rm = T))
  }
  else{return(NA)}
}

Get_GSPD_Diff <- function(Leader_Segs, Follower_Segs, Seg){
  if (Seg %in% Leader_Segs$DME_Seg & Seg %in% Follower_Segs$DME_Seg){
    Leader_GSPD <- filter(Leader_Segs, DME_Seg == Seg)$Leader_Average_GSPD
    Follower_GSPD <- filter(Follower_Segs, DME_Seg == Seg)$Follower_Average_GSPD
    GSPD_Diff <- Leader_GSPD - Follower_GSPD
    return(GSPD_Diff)}
  else{message(paste0("Leader/Follower have no segment data for ", Seg, "-", Seg+1, "DME, returning NA GSPD Difference." ))
    return(NA)}
}

# Mike's generic plotting function
plot_time_histogram <- function(data, plot_title, x_title, line_value, fill_colour){
  
  if (data$Leader_Recat_Wake_Cat[1] == "A"){ x_lim = c(40, 240)} else {x_lim = c(40, 220)}
  
  plot_object <-ggplot(data, aes(plot_data, y=..density..) ) +
    geom_histogram(breaks = seq(0,240,5), na.rm = TRUE, fill = fill_colour, col = "black")+
    xlab(x_title)+
    scale_colour_brewer(palette = "Set1")+
    #   xlim(c(40,180))+
    scale_x_continuous(breaks = seq(40, 240, 10), limits = x_lim)+
    theme_bw()+
    geom_vline(xintercept = line_value)+
    theme(legend.position = "bottom", legend.title=element_blank())+
    ggtitle(plot_title)
  
  return(plot_object)
}

plot_time_histogram_2 <- function(data, plot_title, x_title, fill_colour){
  if (data$Leader_Recat_Wake_Cat[1] == "A"){ x_lim = c(40, 240)} else {x_lim = c(40, 220)}
  plot_object <-ggplot(data, aes(plot_data, y=..density..) ) +
    geom_histogram(breaks = seq(0,240,5), na.rm = TRUE, fill = fill_colour, col = "black")+
    xlab(x_title)+
    scale_colour_brewer(palette = "Set1")+
    #   xlim(c(40,180))+
    scale_x_continuous(breaks = seq(40, 240, 10), limits = x_lim)+
    theme_bw()+
    theme(legend.position = "bottom", legend.title=element_blank())+
    ggtitle(plot_title)
  
  return(plot_object)
}

# Get a Perfect Time Spacing
Get_Perfect_Time_Spacing <- function(Follower_Segs, Delivery, Separation_Distance, Under_Sep){
  
  #Follower_Segs <- follower_segs
  #Delivery <- 0
  #Separation_Distance <- D_RRT
  #Under_Sep <- 0
  #Extra_GSPD <- Follower_Segs[Follower_Segs$DME_Seg == GSPD_Index,]$Follower_Average_GSPD # Get the GSPD From this segment
  
  
  Separation_Distance <- Separation_Distance + Delivery - 1
  TF <- segmentFlyingTime(Follower_Segs, floor(Separation_Distance - Under_Sep), Delivery) # Get the Full Portion of Follower Flying Time
  if (is.na(TF)){return(NA)}
  if ((TF > 0 & ceiling(Separation_Distance - Under_Sep) %in% Follower_Segs$DME_Seg) == T){
    if (Separation_Distance %% 1 == 0){return(TF*3600)}
    Extra_Dist <- (Separation_Distance - Under_Sep) - floor(Separation_Distance - Under_Sep) # Get the Extra Follower Distance
    GSPD_Index <- ceiling(Separation_Distance - Under_Sep)
    Extra_GSPD <- Follower_Segs[Follower_Segs$DME_Seg == GSPD_Index,]$Follower_Average_GSPD # Get the GSPD From this segment
    Extra_Time <- Extra_Dist/Extra_GSPD # Get the Partial Extra Follower Flying Time
    return((TF + Extra_Time)*3600)
  } else {return(NA)}
}

# Get an actual time spacing
Get_Actual_Time_Spacing <- function(Follower_Segs, Leader_Segs, Leader_Aircraft, Delivery, Separation_Distance, Under_Sep){
  if (Leader_Aircraft == "A388" & A388_Adj == T){Leader_Start <- (A388_Actual - 1 - Under_Sep)} 
  else {Leader_Start <- (All_Actual - 1 - Under_Sep)}
  Follower_Start <- Leader_Start + Separation_Distance
  TL <- segmentFlyingTime(Leader_Segs, Leader_Start, Delivery)
  TF <- segmentFlyingTime(Follower_Segs, floor(Follower_Start), Delivery) # Get the Full Portion of Follower Flying Time
  if (is.na(TF)){return(NA)}
  if (is.na(TL)){return(NA)}
  if (Separation_Distance %% 1 == 0){return((TF - TL)*3600)}
  if ((TF > 0 & TL > 0 & ceiling(Follower_Start) %in% Follower_Segs$DME_Seg) == T){
    Extra_Dist <- (Separation_Distance - Under_Sep) - floor(Separation_Distance - Under_Sep) # Get the Extra Follower Distance
    GSPD_Index <- ceiling(Follower_Start)
    Extra_GSPD <- Follower_Segs[Follower_Segs$DME_Seg == GSPD_Index,]$Follower_Average_GSPD # Get the GSPD From this segment
    Extra_Time <- Extra_Dist/Extra_GSPD # Get the Partial Extra Follower Flying Time
    return((TF + Extra_Time - TL)*3600)
  } else {return(NA)}
}

# Output pair of histograms to microsoft word function
# note: requires document to be opened (before) and printed (after)

Output_To_Word <- function(doc, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header){
  
  doc <- body_add_par(doc, paste0(header), style = "heading 2")
  
  for (i in 1:nrow(sep_scheme)){
    
    # Select the relevant Category pair.
    cat_pair <- sep_scheme$LF_Pair[i]
    
    # Grab the Reference Time if required.
    if(time_required){time1 <- sep_scheme$RECAT_Time_Reference[i]}
    if(tr2){time2 <- sep_scheme_2$RECAT_Time_Reference[i]}
    
    # Filter for the relevant category pair
    if(scheme == "RECAT"){plot_data_1 <- filter(data, RECAT_Wake_Pair == cat_pair)} else {plot_data_1 <- filter(data, ICAO_Wake_Pair == cat_pair)}
    
    # Make a copy of the plot_data
    if(scheme == "RECAT"){plot_data_2 <- filter(data2, RECAT_Wake_Pair == cat_pair)} else {plot_data_2 <- filter(data2, ICAO_Wake_Pair == cat_pair)}
    
    # Apply relevant distances to plot data fields to work with 
    #plot_data_1$plot_data <- plot_data_1$plot_data_1
    #plot_data_2$plot_data <- plot_data_2$plot_data_2
    
    # filter to remove NA pairs
    plot_data_1 <- filter(plot_data_1, !is.na(plot_data))
    plot_data_2 <- filter(plot_data_2, !is.na(plot_data))
    
    # Get the value counts for the plot
    nrow1 = nrow(plot_data_1)
    nrow2 = nrow(plot_data_2)
    
    # Get the Plot titles for this pair
    plot_title_1_new <- paste0(plot_title_1, cat_pair, " (n = ", nrow1, ")")
    plot_title_2_new <- paste0(plot_title_2, cat_pair, " (n = ", nrow2, ")")
    
    # ---------------------------------------------------- #
    # Jump to next iteration if certain criteria are met
    
    # If no data for either plots
    if (nrow1 == 0 & nrow2 == 0){next}
    
    # If doing RECAT wake pairs and no ref time is available
    #if (time_required){if (is.na(time)){next}}
    
    # ---------------------------------------------------- #
    
    # ---------------------------------------------------- #
    # Generate Plots
    
    if(nrow1 > 0){
      
      if( time_required ){ plot1 <- plot_time_histogram(plot_data_1, plot_title_1_new, x_title_1, time1, "light blue") }
      else { plot1 <- plot_time_histogram_2(plot_data_1, plot_title_1_new, x_title_1, "light blue") }
      
    } else { plot1 <- ggplot() }
    
    if(nrow2 > 0){
      
      if( tr2 ){ plot2 <- plot_time_histogram(plot_data_2, plot_title_2_new, x_title_2, time2, "light green") }
      else { plot2 <- plot_time_histogram_2(plot_data_2, plot_title_2_new, x_title_2, "light green") }
      
    } else { plot2 <- ggplot() }
    
    message("Printing for ", cat_pair)
    
    # ---------------------------------------------------- #
    
    doc <- body_add_par(doc, paste0(plot_header, cat_pair), style = "heading 3")
    
    src <- tempfile(fileext = ".png")
    png(filename = src, width = 6, height = 6, units = 'in', res = 300)
    grid.arrange(plot1, plot2, ncol = 1)
    dev.off()
    
    doc <- body_add_img(doc, src = src, width = 6, height = 6, style = "centered")
    
  }
  return(doc)
}

# ----------------------------------------- #
# 3. Data Loading
# ----------------------------------------- #

# ------------------------------- Reference ----------------------------- #

# 1.3 Get the Schiphol Separation Adjustment parameters
sep_adjust <- fread(file.path("Input", "separation_adjustment_parameters_v2.csv"))

# 1.4 Get the Heathrow eTBS Performance data for comparison to Schiphol distributions
heathrow_data <- fread(file.path("Input", "eTBS_Calculations_12Months_New.csv"))

# 1.5 Get the RECAT Aircraft Type to Wake for matching with Heathrow
aircraft_type_to_wake <- sqlQuery(con, "SELECT * FROM tbl_Aircraft_Type_To_Wake", stringsAsFactors = F) %>%
  unique() %>% select(-c("Aircraft_Class"))

# 1.6 Get The Schiphol Quantile Adjustment data
sep_adjust_quantiles <- fread(file.path("Input", "quantile_adjustment.csv"))

# ------- Main Datasets

# Performance Model Pre-Processed
pm <- fread(file.path(predir, pm_pre_csv))

# Segment Data - LOCAL
if (local_main){
  segs <- fread(file.path(refdir, segs_csv))
}

# Segment Data - SQL
if (!local_main){
  
  Segment_Data_SQL <- paste0("SELECT

Landing_Pair_ID,
Landing_Pair_Type,
Landing_Pair_Date,
DME_Seg = LWS.DME_Seg / dbo.fnc_GI_Nm_To_M() ,
Leader_Flight_Plan_ID,
Leader_Wind_Segment_ID = LWS.Mode_S_Wind_Seg_ID,
Leader_Average_IAS = LWS.Ave_Mode_S_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec() ,
Leader_Average_GSPD = LWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec(),
--Leader_Flying_Time = 3600/(LWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec()),
Leader_Average_Wind_Effect = LWS.Ave_Wind_Effect_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec(),
Leader_Global_Flag = LWSF.Global_Flag,
Follower_Flight_Plan_ID,
Follower_Average_IAS = FWS.Ave_Mode_S_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec(),
Follower_Average_GSPD = FWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec(),
--Follower_Flying_Time = 3600/(FWS.Ave_Mode_S_GSPD / dbo.fnc_GI_Kts_To_M_Per_Sec()),
Follower_Average_Wind_Effect = FWS.Ave_Wind_Effect_IAS / dbo.fnc_GI_Kts_To_M_Per_Sec(),
Follower_Global_Flag = FWSF.Global_Flag

FROM tbl_Landing_Pair LP

INNER JOIN tbl_Mode_S_Wind_Seg LWS
ON LP.Leader_Flight_Plan_ID = LWS.Flight_Plan_ID

INNER JOIN tbl_Mode_S_Wind_Seg_Flags LWSF
ON LWS.Mode_S_Wind_Seg_ID = LWSF.Mode_S_Wind_Seg_ID

INNER JOIN tbl_Mode_S_Wind_Seg FWS
ON LP.Follower_Flight_Plan_ID = FWS.Flight_Plan_ID AND LWS.DME_Seg = FWS.DME_Seg

INNER JOIN tbl_Mode_S_Wind_Seg_Flags FWSF
ON FWS.Mode_S_Wind_Seg_ID = FWSF.Mode_S_Wind_Seg_ID

ORDER BY Landing_Pair_ID, DME_Seg")
  
  segs <- sqlQuery(con, Segment_Data_SQL, stringsAsFactors = F)
}

# Write the segment data if initially ran from SQL
if (save_segs == T){fwrite(test_segs, file.path(refdir, segs_csv))}

# ------- Save Original Datasets

# Performance Model
pm_orig <- pm

# Heathrow Data
heathrow_orig <- heathrow_data

# ----------------------------------------------------------------------- #
# 4. Adjusted Separation Accuracy Calculations
# ----------------------------------------------------------------------- #

# Get a copy of the original pre-processed PM data
pm <- pm_orig

# Get Schiphol 0DME Separation Accuracy Standard Deviation
pm <- left_join(pm, sep_adjust, by=c("LF_Pair_ICAO4"))

# Get Assumed Delivery Separation Accuracy at 4DME using joined Schiphol Accuracy Mean and Standard Deviation data
pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy_STD = sigma_h*((ICAO_DBS_4DME_Separation_Accuracy - Mean_Accuracy)/SD_Accuracy)) #USE 4DME

# Filter PM data to only include pairs with sep accuracy below icao_max_sep_acc
pm_filt <- filter(pm, ICAO_DBS_4DME_Separation_Accuracy <= icao_max_sep_acc)

# Use method from Plan (1) or Mike's method with Quantiles (2)
if (sep_adj_method == 1){
  pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy = mu_h + ICAO_Assumed_4DME_Separation_Accuracy_STD)
} else {
  
  # Get percentage of Separation accuracy under 0 and find averages per Wake Cat
  #pc_under_0 <- pm_filt %>% group_by(Legacy_Wake, LF_Pair_ICAO4)
  #pc_under_0 <- summarise(pc_under_0, Quantile_0 = quantile(ICAO_Assumed_4DME_Separation_Accuracy_STD, pc_under)) %>% ungroup() #USE 4DME
  
  # Join to PM data and use as Mean
  pm <- left_join(pm, sep_adjust_quantiles, by=c("LF_Pair_ICAO4", "Legacy_Wake"))
  pm <- mutate(pm, ICAO_Assumed_4DME_Separation_Accuracy = ICAO_Assumed_4DME_Separation_Accuracy_STD - Qle_0 - 0.05)
}

# filter out results with a sep accuracy < lambda
pm1 <- filter(pm, Bolster_Flag == 1)
pm <- filter(pm, ICAO_Assumed_4DME_Separation_Accuracy > lambda)
pm <- rbind(pm1, pm) %>% unique()

# Initial plots of Adjusted Separation Accuracy
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_DBS_4DME_Separation_Accuracy, y = ..density..)) + xlim(-2, 3)
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_DBS_4DME_Separation_Accuracy, y = ..density..)) + xlim(-2, 3) + facet_wrap(~LF_Pair_ICAO4)
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_Assumed_4DME_Separation_Accuracy, y = ..density..)) + xlim(-3, 3)
ggplot(data = pm) + geom_histogram(mapping=aes(x = ICAO_Assumed_4DME_Separation_Accuracy, y = ..density..)) + xlim(-3, 3) + facet_wrap(~LF_Pair_ICAO4)

# TEMP

# Add 0DME/1DME Separation Difference
pm <- mutate(pm, Thr_1DME_Observed_Time_Sep_Diff = Observed_1DME_Separation_Time - Observed_0DME_Separation_Time)

# Get D_E4 = Actual_IA_4DME_Wake_Separation
# Get D_E4M = Actual IA 4DME All Separation

pm <- mutate(pm, 
             Actual_4DME_Wake_Separation_Distance = Recat_TBS_4DME_Wake_Separation_Distance + ICAO_Assumed_4DME_Separation_Accuracy,
             Actual_4DME_All_Separation_Distance = Recat_TBS_4DME_All_Separation_Distance + ICAO_Assumed_4DME_Separation_Accuracy)


# ----------------------------------------------------------------------- #                                                                

# ----------------------------------------------------------------------- #
# Get Remaining Outputs/Under Separated Outputs
# ----------------------------------------------------------------------- #

## Calculate time separations by match id to raw data and subset, then find time by adding the inverse ground speed of each segment ##
no_of_iterations <- nrow(pm)
#no_of_iterations <- 10

pm_list <- lapply(1:no_of_iterations, function(i) {
  
  # ----------------------------------------------------------------------- #
  # Get Relevant Data
  # ----------------------------------------------------------------------- #
  # Get i-th row of performance model
  
  
  pm_i <- pm[i,]
  
  # Get i-th Landing_Pair_ID & Leader Aircraft Type
  lpid <- as.numeric(pm_i$Landing_Pair_ID)
  Leader_Aircraft <- pm_i$Leader_Aircraft_Type
  
  # Filter for Leader/Follower segments in the i-th Landing Pair
  segs1 <- segs[segs$Landing_Pair_ID == lpid,]
  leader_segs <- segs1[,c("DME_Seg", "Leader_Average_GSPD")]
  follower_segs <- segs1[,c("DME_Seg", "Follower_Average_GSPD")]
  
  # Get relevant distances
  D_RT <- pm_i$Recat_TBS_0DME_Wake_Separation_Distance
  D_RRT <- pm_i$Recat_TBS_0DME_All_Separation_Distance
  D_E4 <- pm_i$Actual_4DME_Wake_Separation_Distance
  D_E4M <- pm_i$Actual_4DME_All_Separation_Distance
  D_ICAOD <- pm_i$ICAO_DBS_Wake_Separation_Distance 
  
  # Get the 0-1DME GSPD Difference for sense checking Actual 1DME and 0DME values
  pm_i$Thr_1DME_GSPD_Diff <- Get_GSPD_Diff(leader_segs, follower_segs, 0)
  
  # Check to see if this observation hasbeen "bolstered" (for standard pair sample size and for ROT)
  bolstered <- pm_i$Bolster_Flag
  rot_bolstered <- pm_i$ROT_Bolster_Flag
  
  # ----------------------------------------------------------------------- #
  # Get Relevant Outputs (Not Under-Separated)
  # ----------------------------------------------------------------------- #
  
  if (bolstered == 0 & rot_bolstered == 0){
    pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0)
    pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RT, Under_Sep=0)
    pm_i$ICAO_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_ICAOD, Under_Sep=0)
    pm_i$Actual_1DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 0)
    pm_i$Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0)
  }
  
  if (bolstered == 1){
    pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0)
    pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RT, Under_Sep=0)
    pm_i$ICAO_Perfect_1DME_Time_Separation <- NA
    pm_i$Actual_1DME_Wake_Time_Separation <- NA
    pm_i$Actual_0DME_All_Time_Separation <- NA
  }

  if (rot_bolstered == 1){
    pm_i$Recat_Perfect_1DME_Time_Separation <- NA
    pm_i$Recat_Perfect_0DME_Time_Separation <- NA
    pm_i$ICAO_Perfect_1DME_Time_Separation <- NA
    pm_i$Actual_1DME_Wake_Time_Separation <- NA
    pm_i$Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0)
  }
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-0.5)
  # ----------------------------------------------------------------------- #
  
  if (generate_under_seps){
    
    if (bolstered == 0 & rot_bolstered == 0){
      pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0.5)
      pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RT, Under_Sep=0.5)
      pm_i$ICAO_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_ICAOD, Under_Sep=0.5)
      pm_i$Actual_1DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 0.5)
      pm_i$Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0.5)
    }
    
    if (bolstered == 1){
      pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=0.5)
      pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RT, Under_Sep=0.5)
      pm_i$ICAO_Perfect_1DME_Time_Separation <- NA
      pm_i$Actual_1DME_Wake_Time_Separation <- NA
      pm_i$Actual_0DME_All_Time_Separation <- NA
    }
    
    if (rot_bolstered == 1){
      pm_i$Recat_Perfect_1DME_Time_Separation <- NA
      pm_i$Recat_Perfect_0DME_Time_Separation <- NA
      pm_i$ICAO_Perfect_1DME_Time_Separation <- NA
      pm_i$Actual_1DME_Wake_Time_Separation <- NA
      pm_i$Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 0.5)
    }
    
  }
  
  # ----------------------------------------------------------------------- #
  # Get Underseparated Outputs (-1.0)
  # ----------------------------------------------------------------------- #
  
  if (generate_under_seps){
    
    if (bolstered == 0 & rot_bolstered == 0){
      pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=1)
      pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RT, Under_Sep=1)
      pm_i$ICAO_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_ICAOD, Under_Sep=1)
      pm_i$Actual_1DME_Wake_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 1, D_E4, 1)
      pm_i$Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 1)
    }
    
    if (bolstered == 1){
      pm_i$Recat_Perfect_1DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=1, Separation_Distance=D_RT, Under_Sep=1)
      pm_i$Recat_Perfect_0DME_Time_Separation <- Get_Perfect_Time_Spacing(follower_segs, Delivery=0, Separation_Distance=D_RT, Under_Sep=1)
      pm_i$ICAO_Perfect_1DME_Time_Separation <- NA
      pm_i$Actual_1DME_Wake_Time_Separation <- NA
      pm_i$Actual_0DME_All_Time_Separation <- NA
    }
    
    if (rot_bolstered == 1){
      pm_i$Recat_Perfect_1DME_Time_Separation <- NA
      pm_i$Recat_Perfect_0DME_Time_Separation <- NA
      pm_i$ICAO_Perfect_1DME_Time_Separation <- NA
      pm_i$Actual_1DME_Wake_Time_Separation <- NA
      pm_i$Actual_0DME_All_Time_Separation <- Get_Actual_Time_Spacing(follower_segs, leader_segs, Leader_Aircraft, 0, D_E4M, 1)
    }
    
  }
  
  # Print message for completed landing pair
  message("Completed calculations for ", paste0("Landing Pair ID ", lpid, " (", i, "/", no_of_iterations, ")"))
  
  # return this row
  return(pm_i)
  
})

if (!generate_under_seps) {message("Under Separation Calculations not completed for time reasons.")}

# Bind these results to new PM table
pm2 <- rbindlist(pm_list)

## Export Total Calculations for Sense Checking ##
fwrite(pm2, file.path(outdir, "IA_Calculations.csv"))

# ----------------------------------------------------------------------- #
# Final filtering & Data format Preparation
# ----------------------------------------------------------------------- #

# If IAS Filter active, remove pairs where leader/follower flies on average outside of min/max IAS bounds to 4DME
if (use_ias_filter){
  av_segs <- filter(segs, DME_Seg > (max_ias_filter_dme - 1)) %>% group_by(Landing_Pair_ID) %>% 
    summarise(Leader_Ave_Flight_IAS = mean(Leader_Average_IAS, na.rm=T),
              Follower_Ave_Flight_IAS = mean(Follower_Average_IAS, na.rm=T)) %>% ungroup()
  
  pm2 <- left_join(pm2, av_segs, by=c("Landing_Pair_ID")) %>% filter(Leader_Ave_Flight_IAS >= min_ias & Leader_Ave_Flight_IAS <= max_ias &
                                                                       Follower_Ave_Flight_IAS >= min_ias & Follower_Ave_Flight_IAS <= max_ias)
}

# Export filtered calculations
fwrite(pm2, file.path(outdir, "IA_Calculations_Filtered.csv"))

# ----------------------------------------------------------------------- #
# 2. eTBS Performance Analysis --------------------------------------------
# ----------------------------------------------------------------------- #

# Import from eTBS calculations
data1 <- pm2

# 2.1 Definitions ---------------------------------------------------------

wakes <- LETTERS[1:6]
icao_wakes <- c("J", "H", "M", "L")

wake_levels <- paste0(rep(wakes, each = length(wakes)), "-", rep(wakes, times = length(wakes)))
icao_levels <- paste0(rep(icao_wakes, each = length(icao_wakes)), "-", rep(icao_wakes, times = length(icao_wakes)))

#sep_scheme_recat_wake <- fread("Input/Separation Schemes.csv") # this needs updating
#sep_scheme_recat_wake$LF_Pair <- factor(sep_scheme_recat_wake$LF_Pair, levels = wake_levels)

# ------------------------------------------------------------------------ #
# EHAM
con <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, db_name, "ruser", "Th!nkruser"
))

# EGLL
con2 <- odbcDriverConnect(connection=sprintf(
  "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
  "SQL Server", db_ip, "Heathrow_TWA_UTMA_Validation", "ruser", "Th!nkruser"
))

wake_time_str <- "SELECT * FROM tbl_Reference_Recat_Separation_Time"
icao_dist_str <- "SELECT * FROM tbl_DBS_Wake_Turbulence"

uk_recat_map <- data.frame(UK=c("SUPER", "HEAVY", "UPPER", "MEDIUM", "SMALL", "LIGHT"), RECAT=LETTERS[1:6])

sep_scheme_heathrow_wake <- sqlQuery(con2, wake_time_str, stringsAsFactors=F)
sep_scheme_heathrow_wake <- inner_join(sep_scheme_heathrow_wake, uk_recat_map, by=c("Leader_WTC"="UK")) %>% select(-c("Leader_WTC")) %>% rename(Leader_WTC=RECAT)
sep_scheme_heathrow_wake <- inner_join(sep_scheme_heathrow_wake, uk_recat_map, by=c("Follower_WTC"="UK")) %>% select(-c("Follower_WTC")) %>% rename(Follower_WTC=RECAT)
sep_scheme_heathrow_wake <- mutate(sep_scheme_heathrow_wake, LF_Pair = paste0(Leader_WTC, "-", Follower_WTC)) %>% select(-c("Leader_WTC", "Follower_WTC")) %>% 
  rename(RECAT_Time_Reference = Reference_Wake_Separation_Time) %>% mutate(LF_Pair = factor(LF_Pair, levels = wake_levels))
sep_scheme_heathrow_wake <- sep_scheme_heathrow_wake[order(sep_scheme_heathrow_wake$LF_Pair),] %>% select(LF_Pair, everything())

sep_scheme_recat_wake <- sqlQuery(con, wake_time_str, stringsAsFactors=F) #use Database (0DME)
#sep_scheme_recat_wake <- fread(file.path("Input", "sep_scheme_recat_wake.csv")) #use local (1DME)

sep_scheme_recat_wake <- mutate(sep_scheme_recat_wake, LF_Pair = paste0(Leader_WTC, "-", Follower_WTC)) %>% select(-c("Leader_WTC", "Follower_WTC")) %>% 
  rename(RECAT_Time_Reference = Reference_Wake_Separation_Time) %>% mutate(LF_Pair = factor(LF_Pair, levels = wake_levels))
sep_scheme_recat_wake <- sep_scheme_recat_wake[order(sep_scheme_recat_wake$LF_Pair),] %>% select(LF_Pair, everything())

sep_scheme_recat_all <- as.data.frame(wake_levels) %>% rename(LF_Pair = wake_levels)

sep_scheme_icao_wake <- sqlQuery(con, icao_dist_str, stringsAsFactors=F) %>% mutate(LF_Pair = paste0(Leader_WVI, "-", Follower_WVI)) %>% select(-c("Leader_WVI", "Follower_WVI"))
sep_scheme_icao_wake <- mutate(sep_scheme_icao_wake, LF_Pair = factor(LF_Pair, levels = icao_levels))
sep_scheme_icao_wake <- sep_scheme_icao_wake[order(sep_scheme_icao_wake$LF_Pair),] %>% select(LF_Pair, everything())


# 2.2 Calculate new columns -----------------------------------------------

data1 <- mutate(
  data1,
  RECAT_Wake_Type_Pair = paste(Leader_Recat_Wake_Cat, Follower_Aircraft_Type, sep = "-"),
  RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels),
  ICAO_Wake_Pair = factor(paste(Leader_ICAO_Wake_Cat, Follower_ICAO_Wake_Cat, sep = "-"), levels = icao_levels))

# Do some stuff to the Heathrow data to allow updates
heathrow_data <- heathrow_orig
heathrow_data <- select(heathrow_data, -c("Leader_Recat_Wake_Cat", "Follower_Recat_Wake_Cat"))
heathrow_data <- inner_join(heathrow_data, aircraft_type_to_wake, by=c("Leader_Aircraft_Type"="Aircraft_Type")) %>% rename(Leader_Recat_Wake_Cat=Wake)
heathrow_data <- inner_join(heathrow_data, aircraft_type_to_wake, by=c("Follower_Aircraft_Type"="Aircraft_Type")) %>% rename(Follower_Recat_Wake_Cat=Wake)
heathrow_data <- mutate(heathrow_data, RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels))

# Get Accuracies & Improvement values
data1 <- mutate(
 data1,
  RECAT_Wake_Type_Pair = paste(Leader_Recat_Wake_Cat, Follower_Aircraft_Type, sep = "-"),
 RECAT_Wake_Pair = factor(paste(Leader_Recat_Wake_Cat, Follower_Recat_Wake_Cat, sep = "-"), levels = wake_levels),
  Actual_DBS_1DME_Time_Accuracy = Actual_1DME_Wake_Time_Separation - Recat_TBS_Wake_Separation_Time,
  Actual_TBS_1DME_Time_Accuracy = Actual_1DME_Wake_Time_Separation - Recat_Perfect_1DME_Time_Separation,
  Actual_TBS_0DME_Time_Accuracy = Actual_0DME_All_Time_Separation - Recat_Perfect_0DME_Time_Separation,
  Actual_4DME_Wake_Distance_Accuracy = Observed_4DME_Separation_Distance - Actual_4DME_Wake_Separation_Distance,
  Actual_4DME_All_Distance_Accuracy = Observed_4DME_Separation_Distance - Actual_4DME_All_Separation_Distance,
  ICAO_Perfect_Time_Accuracy = Actual_0DME_All_Time_Separation - ICAO_Perfect_1DME_Time_Separation 
)

# Segment Accuracy

data1 <- mutate(
  data1,
  Actual_DBS_1DME_Time_Accuracy_Group = cut(Actual_DBS_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)),
  Actual_TBS_1DME_Time_Accuracy_Group = cut(Actual_TBS_1DME_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf)),
  Actual_4DME_Wake_Distance_Accuracy_Group = cut(Actual_4DME_Wake_Distance_Accuracy, breaks = c(-Inf, -0.55, -0.05, Inf)),
  Actual_4DME_All_Distance_Accuracy_Group = cut(Actual_4DME_All_Distance_Accuracy, breaks = c(-Inf, -0.55, -0.05, Inf)),
  Observed_AGI_Surface_Wind_SPD_Group = cut(Observed_AGI_Surface_Wind_SPD, breaks = c(0,4,7,10,13,16,Inf), right=FALSE),
  ICAO_Perfect_Time_Accuracy_Group = cut(ICAO_Perfect_Time_Accuracy, breaks = c(-Inf, -11.25, 0, Inf))
)


# ------------------------------------------------------------------------- #
# 2.4 Generate Word Document ----------------------------------------------
# ------------------------------------------------------------------------- #

#########################
# BARE IN MIND FOR THESE PLOTS TO GENERATE THE SEP SCHEMES' LF PAIRS BEING USED MUST MATCH ORDER EXACTLY!!!!
#########################

# Create data subset with low surface wind
#data1_low <- filter(data1, Observed_AGI_Surface_Wind_SPD >= -low_sw_lim & Observed_AGI_Surface_Wind_SPD <= low_sw_lim)
data1_low <- filter(data1, Observed_AGI_Surface_Headwind >= -low_shw_lim & Observed_AGI_Surface_Headwind <= low_shw_lim)

# Create data subset specifically for RECAT ROT pairs only, and a low wind form
data1_rot <- filter(data1, !is.na(Recat_TBS_0DME_ROT_Spacing_Distance))
data1_rot_low <- filter(data1_low, !is.na(Recat_TBS_0DME_ROT_Spacing_Distance))

# create data subset for Heathrow low winds
heathrow_data_low <- filter(heathrow_data, Observed_AGI_Surface_Headwind >= -low_shw_lim & Observed_AGI_Surface_Headwind <= low_shw_lim)


##################################################################################
# Create a word document for RECAT
##################################################################################
doc <- read_docx()
doc <- body_add_par(doc, "Distributions of RECAT Perfect/Actual Time Separations for IA", style = "heading 1")

# ------------------------------------------------------------------------- #
# 2.4.1 RECAT Wake 1DME Perfect/Actual Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- data
scheme <- "RECAT"
time_required <- T
tr2 <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Actual_1DME_Wake_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual Wake Time Separation at 1DME (All Wind) for "
header <- "RECAT-EU Perfect/Actual Wake Time Separation at 1DME (All Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.2 RECAT Wake 1DME Perfect/Actual Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- data
scheme <- "RECAT"
time_required <- T
tr2 <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Actual_1DME_Wake_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual Wake Time Separation at 1DME (Low Wind) for "
header <- "RECAT-EU Perfect/Actual Wake Time Separation at 1DME (Low Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.3 RECAT All 0DME Perfect/Actual Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- data
scheme <- "RECAT"
time_required <- T
tr2 <- F
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_0DME_Time_Separation
data2$plot_data <- data2$Actual_0DME_All_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "0DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual All Time Separation at 0DME (All Wind) for "
header <- "RECAT-EU Perfect/Actual All Time Separation at 0DME (All Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.4 RECAT All 0DME Perfect/Actual Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- data
scheme <- "RECAT"
time_required <- T
tr2 <- F
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$Recat_Perfect_0DME_Time_Separation
data2$plot_data <- data2$Actual_0DME_All_Time_Separation
plot_title_1 <- "RECAT Perfect Time Wake Separation: "
plot_title_2 <- "RECAT Actual Time Wake Separation: "
x_title_1 <- "0DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of RECAT-EU Perfect/Actual All Time Separation at 0DME (Low Wind) for "
header <- "RECAT-EU Perfect/Actual All Time Separation at 0DME (Low Wind)"

doc <- Output_To_Word(doc, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# Export document
print(doc, target = file.path(outdir, "IA_Wake_Pair_Distributions (RECAT).docx"))

##################################################################################
# Create a word document for ICAO
##################################################################################
doc2 <- read_docx()
doc2 <- body_add_par(doc2, "Distributions of ICAO Perfect/Observed Time Separations for IA", style = "heading 1")

# ------------------------------------------------------------------------- #
# 2.4.5 ICAO 1DME Perfect/Observed Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- data
scheme <- "ICAO"
time_required <- F
tr2 <- F
sep_scheme <- sep_scheme_icao_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$ICAO_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Observed_1DME_Separation_Time
plot_title_1 <- "ICAO Perfect Time Wake Spacing: "
plot_title_2 <- "ICAO Observed Time Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of ICAO4 Perfect/Observed Wake Time Separation at 1DME (All Wind) for "
header <- "ICAO4 Perfect/Observed Wake Time Separation at 1DME (All Wind)"

doc2 <- Output_To_Word(doc2, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.6 ICAO 1DME Perfect/Observed Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- data
scheme <- "ICAO"
time_required <- F
tr2 <- F
sep_scheme <- sep_scheme_icao_wake
sep_scheme_2 <- sep_scheme
data$plot_data <- data$ICAO_Perfect_1DME_Time_Separation
data2$plot_data <- data2$Observed_1DME_Separation_Time
plot_title_1 <- "ICAO Perfect Time Wake Spacing: "
plot_title_2 <- "ICAO Observed Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms of ICAO4 Perfect/Observed Wake Time Separation at 1DME (Low Wind) for "
header <- "ICAO4 Perfect/Observed Wake Time Separation at 1DME (Low Wind)"

doc2 <- Output_To_Word(doc2, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# Export document
print(doc2, target = file.path(outdir, "IA_Wake_Pair_Distributions (ICAO).docx"))

##################################################################################
# Create a word document for Schiphol/Heathrow comparisons
##################################################################################
doc3 <- read_docx()
doc3 <- body_add_par(doc3, "Distributions of Perfect & Actual Schiphol/Heathrow 1DME Time Separations for IA", style = "heading 1")

# ------------------------------------------------------------------------- #
# 2.4.7 Schiphol/Heathrow Perfect Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- heathrow_data
scheme <- "RECAT"
time_required <- T
tr2 <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$eTBS_perfect_1DME_Time_Separation
plot_title_1 <- "Schiphol Perfect Time Wake Spacing: "
plot_title_2 <- "Heathrow Perfect Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Perfect RECAT Wake Time Separation at 1DME (All Wind) for "
header <- "RECAT (Schiphol/Heathrow) Perfect Wake Time Separation at 1DME (All Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.8 Schiphol/Heathrow Perfect Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- heathrow_data_low
scheme <- "RECAT"
time_required <- T
tr2 <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Recat_Perfect_1DME_Time_Separation
data2$plot_data <- data2$eTBS_perfect_1DME_Time_Separation
plot_title_1 <- "Schiphol Perfect Time Wake Spacing: "
plot_title_2 <- "Heathrow Perfect Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Perfect RECAT Wake Time Separation at 1DME (Low Wind) for "
header <- "RECAT (Schiphol/Heathrow) Perfect Wake Time Separation at 1DME (Low Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.9 Schiphol/Heathrow Actual Time Separation (All Wind)
# ------------------------------------------------------------------------- #

data <- data1
data2 <- heathrow_data
scheme <- "RECAT"
time_required <- T
tr2 <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Actual_1DME_Wake_Time_Separation
data2$plot_data <- data2$eTBS_actual_1DME_Time_Separation
plot_title_1 <- "Schiphol Actual Time Wake Spacing: "
plot_title_2 <- "Heathrow Actual Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Actual RECAT Wake Time Separation at 1DME (All Wind) for "
header <- "RECAT (Schiphol/Heathrow) Actual Wake Time Separation at 1DME (All Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# ------------------------------------------------------------------------- #
# 2.4.10 Schiphol/Heathrow Actual Time Separation (Low Wind)
# ------------------------------------------------------------------------- #

data <- data1_low
data2 <- heathrow_data_low
scheme <- "RECAT"
time_required <- T
tr2 <- T
sep_scheme <- sep_scheme_recat_wake
sep_scheme_2 <- sep_scheme_heathrow_wake
data$plot_data <- data$Actual_1DME_Wake_Time_Separation
data2$plot_data <- data2$eTBS_actual_1DME_Time_Separation
plot_title_1 <- "Schiphol Actual Time Wake Spacing: "
plot_title_2 <- "Heathrow Actual Time Wake Spacing: "
x_title_1 <- "1DME Time Separation (s)"
x_title_2 <- x_title_1
plot_header <- "Histograms (Schiphol/Heathrow) of Actual RECAT Wake Time Separation at 1DME (Low Wind) for "
header <- "RECAT (Schiphol/Heathrow) Actual Wake Time Separation at 1DME (Low Wind)"

doc3 <- Output_To_Word(doc3, data, data2, scheme, time_required, tr2, sep_scheme, sep_scheme_2, plot_title_1, plot_title_2, x_title_1, x_title_2, plot_header, header)

# Export document
print(doc3, target = file.path(outdir, "IA_Wake_Pair_Distributions (w-Heathrow).docx"))

# ------------------------------------------------------------------------- #
# 2.5 Generate Additional Tables / Plots ----------------------------------
# ------------------------------------------------------------------------- #

# Sample size tables for Simon

#Schiphol RECAT Perfect/Actual 1DME Time Counts

sample_1 <- data1 %>% group_by(RECAT_Wake_Pair) %>% summarise(EHAM_Obs_Count = n(),
                                                              EHAM_Perfect_1DME_Count = sum(!is.na(Recat_Perfect_1DME_Time_Separation)),
                                                              EHAM_Actual_1DME_Count = sum(!is.na(Actual_1DME_Wake_Time_Separation))) %>% ungroup() %>% filter(RECAT_Wake_Pair %in% sep_scheme_recat_wake$LF_Pair)


sample_2 <- data1 %>% group_by(RECAT_Wake_Pair) %>% summarise(EHAM_Obs_Count = n(),
                                                              EHAM_Perfect_0DME_Count = sum(!is.na(Recat_Perfect_0DME_Time_Separation)),
                                                              EHAM_Actual_0DME_Count = sum(!is.na(Actual_0DME_All_Time_Separation))) %>% ungroup()

sample_3 <- heathrow_data %>% group_by(RECAT_Wake_Pair) %>% summarise(EGLL_Obs_Count = n(),
                                                                      EGLL_Perfect_1DME_Count = sum(!is.na(eTBS_perfect_1DME_Time_Separation)),
                                                                      EGLL_Actual_1DME_Count = sum(!is.na(eTBS_actual_1DME_Time_Separation))) %>% ungroup()


fwrite(sample_1, file.path(outdir, "EHAM Perfect-Actual Wake Counts.csv"))
fwrite(sample_2, file.path(outdir, "EHAM Perfect-Actual 0DME All Counts.csv"))
fwrite(sample_3, file.path(outdir, "EGLL Perfect-Actual Wake Counts.csv"))


# Tables for the Wake - Type Pairs and Time Under-separation Rate

# t5 # Recat Wake-Type Pair Accuracy STATISTICS against Recat DBS Distance Reference Time Spacing (Actual 1DME Time)
t5 <- group_by(data1, RECAT_Wake_Type_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_DBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t5), file.path(outdir, "1DME_DBS_Time_Accuracy_Stats_Wake_Type.csv"))

# t6 # Recat Wake-Type Pair Accuracy STATISTICS against Recat TBS Distance Perfect Time Spacing (Actual 1DME Time)
t6 <- group_by(data1, RECAT_Wake_Type_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_TBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t6), file.path(outdir, "1DME_TBS_Time_Accuracy_Stats_Wake_Type.csv"))

# t7 # Recat Wake Pair Accuracy STATISTICS against Recat DBS Distance Reference Time Spacing (Actual 1DME Time)
t7 <- group_by(data1, RECAT_Wake_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_DBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_DBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t7), file.path(outdir, "1DME_DBS_Time_Accuracy_Stats_Wake_Wake.csv"))

# t8 # Recat Wake Pair Accuracy STATISTICS against Recat TBS Distance Perfect Time Spacing (Actual 1DME Time)
t8 <- group_by(data1, RECAT_Wake_Pair) %>%
  summarise(        Count = sum(!is.na(Actual_TBS_1DME_Time_Accuracy)),
                    Mean = mean(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Median = median(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE),
                    Std = sd(Actual_TBS_1DME_Time_Accuracy, na.rm = TRUE)) %>%
  as.data.table()
fwrite(as.data.table(t8), file.path(outdir, "1DME_TBS_Time_Accuracy_Stats_Wake_Wake.csv"))



t9 <- table(data1$RECAT_Wake_Pair, data1$Observed_AGI_Surface_Wind_SPD_Group)

## Histograms

# 1 DME Time Accuracy

hist_data <- select(data1, RECAT_Wake_Type_Pair ,RECAT_Wake_Pair, UK_Wake_Type_Pair, UK_Wake_Pair,eTBS_actual_1DME_Time_Accuracy, UK_actual_1DME_Time_Accuracy)
hist_data = melt(hist_data, id=c("RECAT_Wake_Type_Pair", "RECAT_Wake_Pair","UK_Wake_Type_Pair", "UK_Wake_Pair"))
hist_data$Wake_Pair <- factor(ifelse(hist_data$variable == "eTBS_actual_1DME_Time_Accuracy", as.character(hist_data$RECAT_Wake_Pair), as.character(hist_data$UK_Wake_Pair)), levels = levels(hist_data$RECAT_Wake_Pair))

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 5, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-50,100))+
  xlab("1DME Time SepN Accuracy(s)")+
  scale_fill_discrete(name = "Title",
                      labels = c("A", "B"))+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Time Separation Accuray")

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 5, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-50,100))+
  ylim(c(0,0.1))+
  xlab("1DME Time SepN Accuracy(s)")+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Time Separation Accuray")+
  facet_wrap(~ Wake_Pair, nrow = 3)

# 1 DME Distance  Accuracy

hist_data <- select(data1, RECAT_Wake_Type_Pair ,RECAT_Wake_Pair, UK_Wake_Type_Pair, UK_Wake_Pair,eTBS_actual_1DME_Distance_Accuracy, UK_actual_1DME_Distance_Accuracy)

hist_data = melt(hist_data, id=c("RECAT_Wake_Type_Pair", "RECAT_Wake_Pair","UK_Wake_Type_Pair", "UK_Wake_Pair"))
hist_data$Wake_Pair <- ifelse(hist_data$variable == "eTBS_actual_1DME_Distance_Accuracy", hist_data$RECAT_Wake_Pair, hist_data$UK_Wake_Pair)
hist_data$Wake_Pair <- factor(ifelse(hist_data$variable == "eTBS_actual_1DME_Time_Accuracy", as.character(hist_data$RECAT_Wake_Pair), as.character(hist_data$UK_Wake_Pair)), levels = levels(hist_data$RECAT_Wake_Pair))

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 0.1, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-1,2))+
  xlab("1DME Distance SepN Accuracy(s)")+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Distance Separation Accuray")

ggplot(hist_data, aes(value, color = variable, y=..density..) ) +
  geom_freqpoly(binwidth = 0.1, na.rm = TRUE, size = 1)+
  scale_colour_brewer(palette = "Set1")+
  xlim(c(-1,2))+
  ylim(c(0,1.5))+
  xlab("1DME Distance SepN Accuracy(s)")+
  theme(legend.position = "bottom", legend.title=element_blank())+
  ggtitle("1DME Distance Separation Accuray")+
  facet_wrap(~ Wake_Pair, nrow = 3)




### temp
data5 <- fread("IA_Calculations_Filtered3.csv")
data8 <- fread("IA_Calculations_Filtered.csv")

data7 <- fread("pm09.csv")
data7 <- filter(data7, Leader_Callsign == "THY8MH", FP_Date == "09/02/2019")

data6 <- filter(data1, Landing_Pair_ID == '63892') %>% select(DME_Seg, Leader_Average_GSPD)

Leader_Segs <- filter(segs, Landing_Pair_ID == 63892) %>% select(DME_Seg, Leader_Average_GSPD)
Foll_Segs <- filter(segs, Landing_Pair_ID == 63892) %>% select(DME_Seg, Follower_Average_GSPD)



mean(filter())



