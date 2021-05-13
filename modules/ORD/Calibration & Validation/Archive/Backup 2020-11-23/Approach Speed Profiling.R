# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Approach Speed Profiling                              #
#                |                                                        #
# Version No.    |  3.5                                                   #
#                |                                                        #
# Date Modified  |  19/02/2020                                            #
#                |                                                        #
# Author(s)      |  Derry Leng, Michael Cowham                            #
#                |                                                        #
# Project        |  eTBS Related Projects                                 #
#                |                                                        #
# Purpose        |  Creates individual aircraft approach speed profiles   #
#                |  using calibration view from eTBS database.            #
#                |                                                        #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
#
# 3.5  Adjusted for new directory (NATS eTBS Adaptation Support)
#      Moved functions and global variables to ORD Resources.R
#      Moved configurations to Run Scripts.R
#      Added option to use Mode S IAS for speed profile calibration
#
# 3.4  Added option to select between calculating speed data or using raw
#       speed data from database.
#      Added speed filter adjustment for calculating speed data.
#
# 3.3  Corrected a, a1, a2 nomenclature.
#      Fixed calibration skipped FPIDs not being recorded.
#      Renamed script file name from "ORD - Tracks and Speeds" to
#       "Approach Speed Profiling".
#      Major changes to accomodate new calibration view.
#      Restructured output folder layout and adjusted output file names.
#
# 3.2  Added option to change wake categorisation type.
#
# 3.1  Used derived GSPD (assumed low headwind TAS equiv.) for NAVCAN.
#
# 3.0  Updated based on new requirements for NATS eTBS Adaptation Support.
#
# 2.3  Iteratively developed version up to v1.3 of configuration data.
#
# 2.4  Version tidied for Configuration Data update (25/9/2017).
#
# ----------------------------------------------------------------------- #

# Create output directory folder if not exists
out_dir <- file.path(Project_Directory, outdir_approach_speed_profiling)
if (!dir.exists(out_dir)) dir.create(out_dir)

out_profiles <- file.path(out_dir, "Approach_Speed_Profiles")
if (!dir.exists(out_profiles)) dir.create(out_profiles)

# ----------------------------------------------------------------------- #
# Input Data --------------------------------------------------------------
# ----------------------------------------------------------------------- #

# Get dates and flight plan IDs
flight_id <- sqlQuery(con, "
  SET DATEFORMAT dmy
  SELECT DISTINCT
    FP_Date,
    CAST(FP_Date AS datetime) AS Date,
    Follower_Flight_Plan_ID 
  FROM vw_ORD_Calibration_View
  ORDER BY CAST(FP_Date AS datetime), Follower_Flight_Plan_ID
") %>% as.data.table()

# Get LSS type lookup data
lss_types <- fread(file.path(ref_data, ref_lss_type_table))

# Get default wake adaptation lookup data
if (wake_type == "REF_DATA") {
  
  wake_aircraft_table <- fread(file.path(ref_data, ref_wake_aircraft_table))
  
  wake_adaptation <- fread(file.path(ref_data, ref_wake_adaptation))
  
  wake_data <- wake_aircraft_table[,c("NAS AC TYPE", "RECAT EU WTC")]
  names(wake_data) <- c("aircraft_type", "wake")
  names(wake_adaptation)[1] <- "wake"
  wake_data <- wake_data[wake_adaptation, on = "wake"]
  
} else if (wake_type == "DATABASE") {
  
  wake_aircraft_table <- sqlQuery(con, sprintf("
    SELECT * FROM tbl_Aircraft_Type_To_Wake
  ")) %>% as.data.table()
  
  wake_adaptation <- sqlQuery(con, sprintf("
    EXEC usp_GI_Get_ORD_Wake_Adaptation_Data
  ")) %>% as.data.table()
  
  wake_data <- wake_aircraft_table
  names(wake_data) <- c("aircraft_type", "wake")
  names(wake_adaptation)[1] <- "wake"
  names(wake_adaptation) <- tolower(names(wake_adaptation))
  wake_data <- wake_data[wake_adaptation, on = "wake"]
  
}

# ----------------------------------------------------------------------- #
# Aircraft Approach Speed Profiling ---------------------------------------
# ----------------------------------------------------------------------- #

# Record flights not profiled
skipped_fpids <- data.table()

# Record mean processing duration
mean_duration <- numeric()

for (day in unique(flight_id$FP_Date) %>% .[start_day_num:length(.)]) {
  
  # Record loop start time
  t1 <- Sys.time()
  
  # Get flight plan IDs of the day
  day_flight_id <- flight_id[FP_Date == day]$Follower_Flight_Plan_ID
  
  # Get tracks for day
  day_tracks <- sqlQuery(con, sprintf("
    SELECT * FROM vw_ORD_Calibration_View WHERE FP_Date = '%s'
  ", as.character(day))) %>% as.data.table()
  
  # Generate table of model outputs
  output_models <- list()
  
  for (i in 1:length(day_flight_id)) {
    
    # Get raw tracks for flight i
    raw_tracks <- day_tracks[Follower_Flight_Plan_ID == day_flight_id[i]]
    
    # Get LSS type of flight
    lss <- lss_types[aircraft_type == unique(raw_tracks$Follower_Aircraft_Type)]$landing_stabilisation_speed_type %>% ifelse(length(.) > 0, ., 0)
    
    # Get surface headwind
    surface_headwind <- suppressWarnings(min(raw_tracks$Follower_Threshold_Surface_Headwind, na.rm = T)) %>% ifelse(is.infinite(.), 0, .)
    
    # Filter track RTT for speed profile modelling
    tracks <- raw_tracks[Follower_Range_To_Threshold >= 0 & Follower_Range_To_Threshold <= 6 & Track_Speed > 0][order(Track_Time)]
    
    if (nrow(tracks) > 1) {
      
      if (speed_type == "Mode_S_IAS") {
        
        x <- tracks$Follower_Range_To_Threshold
        y <- tracks$Mode_S_IAS
        
      } else if (speed_type == "Track_Speed") {
        
        x <- tracks$Follower_Range_To_Threshold
        y <- (tracks$Track_Speed+tracks$Follower_Threshold_Surface_Headwind)/(1+airport_alt/60000)
        
      } else if (speed_type == "Calculated_Speed") {
        
        # Filter track for stationary points
        k <- 2
        while (k <= nrow(tracks)) {
          # same_range <- tracks$Follower_Range_To_Threshold[k] == tracks$Follower_Range_To_Threshold[k-1]
          same_x <- tracks$X_Position[k] == tracks$X_Position[k-1]
          same_y <- tracks$Y_Position[k] == tracks$Y_Position[k-1]
          same_time <- tracks$Track_Time[k] == tracks$Track_Time[k-1]
          if (same_time | (same_x & same_y)) {
            tracks <- tracks[-k]
          } else {
            k <- k + 1
          }
        }
        
        # Calculate speed based on positional difference between timestamps
        tracks_new <- tracks[,c("Track_Time", "Follower_Threshold_Surface_Headwind", "Follower_Range_To_Threshold", "X_Position", "Y_Position", "Altitude", "Track_Speed")]
        tracks_new$Distance_Travelled <- NA
        for (k in 2:nrow(tracks_new)) {
          tracks_new$Distance_Travelled[k] <- sqrt((tracks_new$X_Position[k] - tracks_new$X_Position[k-1])^2 + (tracks_new$Y_Position[k] - tracks_new$Y_Position[k-1])^2 + ((tracks_new$Altitude[k] - tracks_new$Altitude[k-1])/6076.12)^2)
          if (k == 2) {
            tracks_new$Distance_Travelled[k-1] <- tracks_new$Distance_Travelled[k]
          }
        }
        tracks_new$Point_Speed <- NA
        tracks_new$Point_Speed[1] <- tracks_new$Track_Speed[1]
        for (k in 2:nrow(tracks_new)) {
          tracks_new$Point_Speed[k] <- tracks_new$Distance_Travelled[k]/((tracks_new$Track_Time[k]-tracks_new$Track_Time[k-1])/3600)
        }
        
        # Filter strange speeds (x% above or below Track_Speed range)
        tracks_new <- tracks_new[
          Point_Speed >= max(min(tracks_new$Track_Speed, na.rm=T)*(speed_filter_perc/100), speed_filter_limit_low, na.rm=T) &
            Point_Speed <= min(max(tracks_new$Track_Speed, na.rm=T)*(1+speed_filter_perc/100), speed_filter_limit_high, na.rm=T)
          ]
        
        # Get tracks x and y vectors
        x <- tracks_new$Follower_Range_To_Threshold
        y <- (tracks_new$Point_Speed+tracks_new$Follower_Threshold_Surface_Headwind)/(1+airport_alt/60000)
        
        # Catch possible discrepancy in length of x and y
        if (length(x) < length(y)) {
          y <- y[1:length(x)]
        } else if (length(x) > length(y)) {
          x <- x[1:length(y)]
        }
        
      }
      
    } else {
      
      # Skip flight all points are filtered out during stationarity check
      skipped_fpids <- rbindlist(list(skipped_fpids, data.table(Date = day, Follower_Flight_Plan_ID = day_flight_id[i], Reason = "Stationary track data")))
      next
      
    }
    
    # Skip flight if RTT range too small 
    if (min(x) >= 1 | max(x) < 4) {
      skipped_fpids <- rbindlist(list(skipped_fpids, data.table(Date = day, Follower_Flight_Plan_ID = day_flight_id[i], Reason = "Insufficient RTT")))
      next
    }
    
    # Generate NLS model
    m <- tryCatch(
      suppressWarnings(nls(
        y ~ airspeed_model_vector_break(x, a, a1, b, n1, n2),
        start = list(a = 140, a1 = 140, b = 160, n1 = 3, n2 = 4),
        control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
      )),
      error = function(e) NULL
    )
    model_type <- "NLS"
      
    # Generate Simplified NLS model if first one fails
    if (is.null(m)) {
      x <- x %>% .[. <= 2]
      y <- y[1:length(x)]
      m <- tryCatch(
        suppressWarnings(nls(
          y ~ airspeed_model_vector_break_simplified(x, a, a1),
          start = list(a = 140, a1 = 140),
          control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
        )),
        error = function(e) NULL
      )
      model_type <- "NLS_Simplified"
    }
    
    # Generate Even More Simplified (TM) NLS model if previous ones fail
    if (is.null(m)) {
      m <- tryCatch(
        suppressWarnings(nls(
          y ~ a,
          start = list(a = 140),
          control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
        )),
        error = function(e) NULL
      )
      model_type <- "NLS_Simplified_2"
    }
    
    # Skip flight if NLS modelling fails
    if (is.null(m)) {
      skipped_fpids <- rbindlist(list(skipped_fpids, data.table(Date = day, Follower_Flight_Plan_ID = day_flight_id[i], Reason = "NLS modelling failed")))
      next
    }
    
    # Lookup wake data
    lookup_ac_type <- unique(tracks$Follower_Aircraft_Type)
    
    wake_i <- wake_data[aircraft_type == unique(tracks$Follower_Aircraft_Type)][1]
    
    # Model output table row
    model_i <- data.table(
      FP_Date = unique(tracks$FP_Date),
      Follower_Flight_Plan_ID = unique(tracks$Follower_Flight_Plan_ID),
      Follower_Callsign = unique(tracks$Follower_Callsign),
      Follower_Aircraft_Type = unique(tracks$Follower_Aircraft_Type),
      Landing_Runway = unique(tracks$Landing_Runway),
      Surface_Headwind = surface_headwind,
      lss_type = lss,
      wake = as.character(wake_i$wake),
      a1 = coef(m)[1],
      a2 = coef(m)[2],
      b = coef(m)[3],
      n1 = coef(m)[4],
      n2 = coef(m)[5],
      landing_adjustment = calc_landing_adjustment(lss, surface_headwind),
      landing_adjustment_boeing = calc_landing_adjustment(0, surface_headwind),
      d = wake_i$final_deceleration_follower,
      model_type = model_type,
      initial_deceleration_foll = wake_i$initial_deceleration_follower
    )
    
    output_models[[length(output_models)+1]] <- model_i
    
  }
  
  # Output day's modelled aircraft
  fwrite(rbindlist(output_models, fill = T, use.names = T), file = file.path(out_profiles, paste0("Approach_Speed_Profiles_", gsub("-", "_", as.Date(day, format="%d/%m/%Y")), ".csv")))
  
  # Record loop end time
  t2 <- Sys.time()
  
  # Estimated time remaining
  duration <- as.numeric(difftime(t2, t1, units="mins"))
  mean_duration <- ifelse(length(mean_duration) > 0, mean(mean_duration, duration), duration)
  message("Completed ", length(day_flight_id), " flights on ", day, " (", which(unique(flight_id$FP_Date) == day), "/", length(unique(flight_id$FP_Date)), " day - estimated time remaining: ", round(mean_duration*(length(unique(flight_id$FP_Date))-which(unique(flight_id$FP_Date) == day)), 2), " mins)")
  
}

# Export list of skipped flights
fwrite(skipped_fpids, file = file.path(out_dir, "Approach_Speed_Profiles_Skipped.csv"))

# Combine flights from all calibrated days and export
out_profiles %>%
  list.files(pattern = "Approach_Speed_Profiles_[0-9]{4}_[0-9]{2}_[0-9]{2}.csv", full.names = T) %>%
  lapply(fread) %>%
  rbindlist(fill = T, use.names = T) %>%
  fwrite(file = file.path(out_dir, "Approach_Speed_Profiles.csv"))
