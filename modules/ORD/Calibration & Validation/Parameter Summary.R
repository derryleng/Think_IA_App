# ----------------------------------------------------------------------- #
#                |                                                        #
# Title          |  Parameter Summary                                     #
#                |                                                        #
# Version No.    |  5.0                                                   #
#                |                                                        #
# Date Modified  |  13/07/2021                                            #
#                |                                                        #
# Author(s)      |  Derry Leng, Michael Cowham                            #
#                |                                                        #
# Project        |  eTBS Related Projects                                 #
#                |                                                        #
# Purpose        |  Produces ORD adaptation data and diagnostic plots     #
#                |                                                        #
# ----------------------------------------------------------------------- #

# Version History ------------------------------------------------------- #
# 5.0  Added PWS operation, calibration on medians and using buffers as difference
#         between median and 1st percentile + 10
# 4.3  Added a adjustment to Vref or deceleration rate to account for the
#        1000ft gate limitation, normalising ORD time of flight with observed
#        time of flight (median) adjusting decel rate if the aircraft stabilises
#        before 1000ft gate adjusting Vtgt for aircraft stabilising after 1000ft
#        gate - AH
#
# 4.2  Removed hardcoding of the threshold number of observations (50) now
#        defined in wrapper script
#      Taking number of counts for aircraft categories from GWCS script for
#         dbs adaptation table. - AH
#
# 4.1  Removed Hardcoding of parameters for adaptation tables, defined in the
#        wrapper script.
#      Added a go around filter (discards flights with a very large track time)
#       - AH
#
# 4.0  Commented out the additional LVNL calculations for aircraft near the decel
#      limit
# 3.9  Corrected all Min_Safe_Landing_Speed_Follower: max(vref+5,median(a1))
#
# 3.8  Corrected DBS adaptation (IA-187), now all DBS distances use same
#       "overall" adaptation only using pairs with valid Wake and ROT
#       spacings from database.
#
# 3.7  Revert "DBS adaptation" to overall adaptation
#      Added new DBS adaptation calculation (IA-187) based on wake
#       adaptation parameters weighted by proportion of wake pairs for
#       each DBS distance.
#      Corrected wake adaptation column name from Aircraft_Type to
#       Wake_Cat.
#
# 3.6  Changed Compression_Commencement_Threshold to 4
#      Changed Initial_Procedural_Speed_Lead to 160
#      Changed Initial_Procedural_Speed_Follower to 162
#
# 3.5  Updated wake param calcs to use “boeing” speed type
#      Added summary stats output for Surface Wind vs Landing Speed plots
#      Added additional columns for TWA adaptation:
#           Compression_Commencement_Threshold = 10
#           End_Initial_Deceleration_Distance_Lead = 12 (same as foll)
#           Initial_Procedural_Speed_Lead = 180 (same as foll)
#           Initial_Deceleration_Lead = d2 (same as foll)
#
# 3.4  Adjusted for new directory (NATS eTBS Adaptation Support)
#      Moved functions and global variables to ORD Resources.R
#      Moved configurations to Run Scripts.R
#
# 3.3  Corrected a, a1, a2 nomenclature.
#      Renamed script file name from "ORD - Parameter Summary" to
#       "Parameter Summary"
#
# 3.2  Major optimisation of source code.
#      Added option to use existing validation date list.
#      Added ICAO 7 adaptation file generation.
#      Fixed ORD Parameters * Summary p10 values.
#
# 3.1  Major revisions.
#      Added aircraft type and RECAT-EU adaptation file generation.
#
# 3.0  Various additions and fixes.
#
# 2.0  Updated based on new requirements for NATS eTBS Adaptation Support.
#
# 1.4  Now loads in the V1.4 config data to produce plots of the overall
#       LSS performance.
#
# 1.3  Updated to use the density plot for small sample estimation, and
#       select a 75% validation data set.
#
# 1.2  Updated for full year data set.
#
# 1.1  Iteratively developed up to v1.3 configuration data.
#
# ----------------------------------------------------------------------- #

# Create output directory folder if not exists
out_dir <- file.path(ord_dir, outdir_parameter_summary)
if (!dir.exists(out_dir)) dir.create(out_dir)

out_plot1 <- file.path(out_dir, "Vref Distributions - Aircraft Type")
if (!dir.exists(out_plot1)) dir.create(out_plot1)
out_plot2 <- file.path(out_dir, "Surface Wind vs Landing Speed - Aircraft Type")
if (!dir.exists(out_plot2)) dir.create(out_plot2)

out_plot3 <- file.path(out_dir, "Vref Distributions - Wake")
if (!dir.exists(out_plot3)) dir.create(out_plot3)

out_plot4 <- file.path(out_dir, "Surface Wind vs Landing Speed - Wake")
if (!dir.exists(out_plot4)) dir.create(out_plot4)


# ----------------------------------------------------------------------- #
# Import Data -------------------------------------------------------------
# ----------------------------------------------------------------------- #

modeldata <- fread(file.path(ord_dir, speed_profile_folder, "Approach_Speed_Profiles.csv"))
modeldata$a1 <- as.numeric(modeldata$a1)
modeldata$a2 <- as.numeric(modeldata$a2)
modeldata$b <- as.numeric(modeldata$b)
modeldata$n1 <- as.numeric(modeldata$n1)
modeldata$n2 <- as.numeric(modeldata$n2)
modeldata$d <- as.numeric(modeldata$d)
modeldata$d2 <- as.numeric(modeldata$d2)


# Get LSS type lookup data
lss_types <- fread(file.path(ref_data, ref_lss_type_table))

# Get GWCS sample weighting pair
Wake_Pair_Counts <- fread(file.path(ref_data, Wake_Pair_Counts_input))

# Simple go around filtering, flights with a very long track time

potential_anomalies <- dbGetQuery(con, "SELECT DISTINCT Flight_Plan_ID
                                FROM tbl_Mode_S_Wind_Seg
                                WHERE Max_Track_Time -Min_Track_Time > 200") %>% as.data.table

modeldata <- filter(modeldata, !(Follower_Flight_Plan_ID %in% potential_anomalies$Flight_Plan_ID))

# LVNL Only Filtering
if (grepl("^LVNL_.*$", database)) {

  plane_check <- fread(file.path(ref_data, "Aircraft_Type_RECAT-EU_Lookup_04-2020.csv"))

  # Filter helicopter
  modeldata <- modeldata[Follower_Aircraft_Type %in% plane_check[Aircraft_Class != "Helicopter"]$Aircraft_Type]

  # Get Leader_RECAT
  leader_RECAT_joins <- "
  SELECT
  	Follower_Flight_Plan_ID,
  	Leader_Recat_Wake_Cat AS Leader_RECAT
  FROM tbl_Landing_Pair AS t1
  LEFT JOIN (
  	SELECT
  		Landing_Pair_ID,
  		Leader_Recat_Wake_Cat
  	FROM tbl_All_Pair_Reference_Data
  ) AS t2 ON t1.Landing_Pair_ID = t2.Landing_Pair_ID
  " %>% dbGetQuery(con, .) %>% as.data.table()

  modeldata <- merge(modeldata, leader_RECAT_joins, by = "Follower_Flight_Plan_ID", all.x = T)

  # Re-adjust LSS types on modeldata from approach speed profiling run
  # modeldata[Follower_Aircraft_Type == "Example_Type"] <- readjust_lss_type(modeldata[Follower_Aircraft_Type == "Example_Type"], 5)

  # Update the following types to have LSS of 0

  for (type in c("C56X", "CL35", "CRJ9", "GLEX", "GLF5", "PC12", "SF50", "TBM8", "TBM9", "H25B")){
    modeldata[Follower_Aircraft_Type == type] <- readjust_lss_type(modeldata[Follower_Aircraft_Type == type], 0)
  }

  # These are types to calibrate even if they fail the sample size criteria

  additional_types <- c("PC12", "SF50", "TBM8", "TBM9", "H25B")

  # These are types to remove from the Wake Calibration even if they pass the sample size criteria

  removed_types <- c("PC12", "SF50", "TBM8", "TBM9", "H25B")

  # Update all F types to have a LSS of zero

  modeldata[wake == "F"] <- readjust_lss_type(modeldata[wake == "F"], 0)

  # Read in the operator list

  op_list <- fread(file.path(Project_Directory, "airlines.csv"))
  operators <- mutate(modeldata, Operator = substr(Follower_Callsign, 1, 3)) %>% group_by(Follower_Aircraft_Type, Operator) %>% summarise(TotalN = n()) %>% ungroup() %>%
    left_join(op_list, by = c("Operator" = "ICAO"))

  cargo_carriers <- c("ABW", "CKK", "AZG", "CKS", "CLX", "GTI", "MPH", "NCR", "TAY", "YZR")

  # Drop some B744 operators

  modeldata <- modeldata[!(paste0(Follower_Aircraft_Type, substr(Follower_Callsign, 1, 3)) %in% c("B744TNT", "B744CKS"))]

}


#-------------------------------------------------------------------------#
#Calculating and adding landing adjustments data --------------------------
#-------------------------------------------------------------------------#

#See which aircraft observed don't have lss types
unique_aircraft_types <- modeldata %>% select(Follower_Aircraft_Type) %>% distinct()
unique_aircraft_types <- left_join(unique_aircraft_types, lss_types, by = c("Follower_Aircraft_Type" = "aircraft_type"))

# land_temp <- list()
#
# for (i in 1:nrow(landing_adjustments)) {
#
#   land_temp[[i]] <- landing_adjustments[i]
#
#   land_temp[[i]]$landing_adjustment <- calc_landing_adjustment(landing_adjustments$landing_stabilisation_speed_type[i], landing_adjustments$Surface_Headwind[i])
#   land_temp[[i]]$landing_adjustment_boeing <- calc_landing_adjustment(0, landing_adjustments$Surface_Headwind[i])
#
# }
#
# land_temp <- rbindlist(land_temp, fill = T)

missing_lss_type <- unique_aircraft_types[is.na(unique_aircraft_types$landing_stabilisation_speed_type)]


for (i in missing_lss_type$Follower_Aircraft_Type) {

  lss <- data.table(
    aircraft_type = i,
    landing_stabilisation_speed_type = 0
  )

  lss_types <- rbind(lss_types, lss)

  message("No lss type found for ", i, ": Therefore setting to type 0 (Default Beoing).")

}

landing_adjustments <- modeldata %>% select(Follower_Flight_Plan_ID, Follower_Aircraft_Type, Surface_Headwind)
landing_adjustments <- left_join(landing_adjustments, lss_types, by = c("Follower_Aircraft_Type" = "aircraft_type"))

landing_adjustments <- landing_adjustments %>% add_column(landing_adjustment = NA)
landing_adjustments <- landing_adjustments %>% add_column(landing_adjustment_boeing = NA)

# for (i in 1:nrow(landing_adjustments)) {
# 
#   landing_adjustments$landing_adjustment[i] <- calc_landing_adjustment(landing_adjustments$landing_stabilisation_speed_type[i], landing_adjustments$Surface_Headwind[i])
#   landing_adjustments$landing_adjustment_boeing[i] <- calc_landing_adjustment(0, landing_adjustments$Surface_Headwind[i])
# 
# }

landing_adjustments <- calc_landing_adjustment_vect(landing_adjustments)

modeldata <- select(modeldata, c(-landing_adjustment, -landing_adjustment_boeing, -lss_type))

modeldata <- left_join(modeldata, select(landing_adjustments, c(Follower_Flight_Plan_ID, landing_stabilisation_speed_type, landing_adjustment, landing_adjustment_boeing)), by = "Follower_Flight_Plan_ID")

modeldata <- modeldata %>% rename(lss_type = landing_stabilisation_speed_type)


# ----------------------------------------------------------------------- #
# Exclude Dates for Validation Dataset ------------------------------------
# ----------------------------------------------------------------------- #

valset_path <- file.path(out_dir, "Validation_Date_List.csv")

if (!validation_generation & file.exists(valset_path)) {
  valset <- fread(valset_path)
} else {
  if (!validation_generation) {
    message("Validation date list not found, generating new list...")
  }
  set.seed(1234)
  valset <- data.table(
    Date = as.character(unique(modeldata$FP_Date)),
    Reserve = runif(length(unique(modeldata$FP_Date))) > 1 - validation_threshold
  )
  cat("Days to reserve (TRUE) for validation set:")
  print(table(valset$Reserve))
  fwrite(valset, valset_path)
}

modeldata <- modeldata[FP_Date %in% valset[Reserve == F]$Date]

# ----------------------------------------------------------------------- #
# Rename columns for historic reasons -------------------------------------
# ----------------------------------------------------------------------- #

# Identify WTC column
modeldata$Follow_RECAT <- modeldata$wake

# Rename initial_deceleration_foll to d2
modeldata$d2 <- modeldata$initial_deceleration_foll

# ----------------------------------------------------------------------- #
# Subset modeldata based on parameter filter ------------------------------
# ----------------------------------------------------------------------- #

modeldata_filtered_a1 <- if (a1_filter) {
  modeldata[a1 >= a1_min & a1 <= a1_max]
} else {
  modeldata
}

modeldata_filtered_a2 <- if (a2_filter) {
  modeldata[a2 >= a2_min & a2 <= a2_max]
} else {
  modeldata
}

modeldata_filtered_b <- if (b_filter) {
  modeldata[b >= b_min & b <= b_max]
} else {
  modeldata
}

modeldata_filtered_n1 <- if (n1_filter) {
  modeldata[n1 >= n1_min & n1 <= n1_max]
} else {
  modeldata
}

modeldata_filtered_n2 <- if (n2_filter) {
  modeldata[n2 >= n2_min & n2 <= n2_max]
} else {
  modeldata
}

modeldata_filtered_d <- if (d_filter) {
  modeldata[d >= d_min & d <= d_max]
} else {
  modeldata
}



# ----------------------------------------------------------------------- #
# Aircraft Type Parameter Summary & Adaptation Legacy ---------------------
# ----------------------------------------------------------------------- #

if (Operation != "PWS") {
  
  type_params <- rbindlist(list(
    # parameter_summary(modeldata_filtered_a1, "Follower_Aircraft_Type", "a1"),
    # parameter_summary(modeldata_filtered_a2, "Follower_Aircraft_Type", "a2"),
    # parameter_summary(modeldata_filtered_b, "Follower_Aircraft_Type", "b"),
    # parameter_summary(modeldata_filtered_n1, "Follower_Aircraft_Type", "n1"),
    # parameter_summary(modeldata_filtered_n2, "Follower_Aircraft_Type", "n2"),
    # parameter_summary(modeldata_filtered_d, "Follower_Aircraft_Type", "d"),
    # parameter_summary(modeldata, "Follower_Aircraft_Type", "d2")
    parameter_summary(modeldata_filtered_a1[,c("Follower_Aircraft_Type", "a1")]),
    parameter_summary(modeldata_filtered_a2[,c("Follower_Aircraft_Type", "a2")]),
    parameter_summary(modeldata_filtered_b[,c("Follower_Aircraft_Type", "b")]),
    parameter_summary(modeldata_filtered_n1[,c("Follower_Aircraft_Type", "n1")]),
    parameter_summary(modeldata_filtered_n2[,c("Follower_Aircraft_Type", "n2")]),
    parameter_summary(modeldata_filtered_d[,c("Follower_Aircraft_Type", "d")]),
    parameter_summary(modeldata[,c("Follower_Aircraft_Type", "d2")])
  ))
  
  fwrite(type_params, file.path(out_dir, "Parameter_Summary_Aircraft_Type.csv"))
  
  actypes <- unique(type_params$Follower_Aircraft_Type)
  
  type_params_req <- list()
  type_adaptation <- list()
  type_adaptation_modified <- list()
  type_params <- as.data.table(type_params)
  modeldata <- as.data.table(modeldata)
  
  sink(file.path(out_plot2, "summary.txt"))
  sink()
  
  # adding a row for counts by ac type and ordering by ac type
  
  unique_aircraft_types$N <- rep(0, nrow(unique_aircraft_types))
  unique_aircraft_types <- unique_aircraft_types %>% arrange(Follower_Aircraft_Type)
  
  
  # Initialising a df to display and calculate all parameters corresponding to the
  # adjustment to account for 1000ft gate limitation
  
  time_per_ac_type <- unique_aircraft_types %>% select(., c("Follower_Aircraft_Type"))
  
  time_per_ac_type$time_lead_observed_alternate <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$time_foll_observed_alternate <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$a_lead <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$a_foll <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$a_original <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$a1 <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$Vtgt_lead <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$Vtgt_foll <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$adjusted_Vtgt_lead <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$adjusted_Vtgt_foll <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$n1 <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$n2 <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$m2_lead <- rep(NA, nrow(time_per_ac_type))
  time_per_ac_type$m2_foll <- rep(NA, nrow(time_per_ac_type))
  
  
  
  for (i in 1:length(actypes)) {
  
    dat <- type_params[Follower_Aircraft_Type == actypes[i]]
    dat2 <- modeldata_filtered_a1[Follower_Aircraft_Type == actypes[i]]
    dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment), 0, dat2$landing_adjustment)
  
    # Add counts to the unique_aircraft_types table
    if (actypes[i] == unique_aircraft_types$Follower_Aircraft_Type[i]) {
      unique_aircraft_types$N[i] <- nrow(dat2[Follower_Aircraft_Type == actypes[i]])
    } else {message("Mismatched Aircraft Type for ", i, " either ", actypes[i], " or ", unique_aircraft_types$Follower_Aircraft_Type[i])}
  
    N <- dat[Type == "a1"]$N %>% ifelse(length(.) > 0, ., NA)
    a1 <- dat[Type == "a1"]$median %>% ifelse(length(.) > 0, ., NA)
    a2 <- dat[Type == "a2"]$median %>% ifelse(length(.) > 0, ., NA)
    b <- dat[Type == "b"]$median %>% ifelse(length(.) > 0, ., NA)
    n1 <- dat[Type == "n1"]$median %>% ifelse(length(.) > 0, ., NA)
    n2 <- dat[Type == "n2"]$median %>% ifelse(length(.) > 0, ., NA)
    # d <- ifelse(b - a2 > 0 & n2 != n1, (b-a2)/(n2-n1), dat[Type == "d"]$median) %>% ifelse(length(.) > 0, ., NA)
    d <- ifelse(type_adaptation_input_table$Steady_Procedural_Speed_Lead - a2 > 0 & n2 != n1, (type_adaptation_input_table$Steady_Procedural_Speed_Lead-a2)/(n2-n1), dat[Type == "d"]$median) %>% ifelse(length(.) > 0, ., NA)
    d2 <- dat[Type == "d2"]$median %>% ifelse(length(.) > 0, ., NA)
  
    # Generate normal distribution using mean and sd of vref
    # vref_dist <- rnorm(1e6, mean = mean(dat2$vref), sd = sd(dat2$vref))
  
    if (grepl("^LVNL_.*$", database)) {
  
      if (length(dat2$vref) > 1 & (N >= observation_threshold | actypes[i] %in% additional_types)) {
        dat_density <- density(dat2$vref %>% .[!is.na(.)], n = max(512, length(dat2$vref %>% .[!is.na(.)])))
      } else {
        message("Not enough aircrafts for ", actypes[i])
        next
      }
  
    } else {
  
      if (length(dat2$vref) > 1 & N >= observation_threshold) {
        dat_density <- density(dat2$vref %>% .[!is.na(.)], n = max(512, length(dat2$vref %>% .[!is.na(.)])))
      } else {
        message("Not enough aircrafts for ", actypes[i])
        next
      }
  
    }
  
    if (nrow(dat2[Follower_Aircraft_Type == actypes[i]]) >= empirical_threshold) {
      message("Calibrating ", actypes[i], " using empirical distribution.")
      vref_selection <- "Empirical"
      vref <- ifelse(
        quantile(dat2$vref, 0.01) + 10 <= median(dat2$vref),
        quantile(dat2$vref, 0.01) + 10,
        median(dat2$vref))
      pcile <- quantile(dat2$vref, 0.01)
    } else {
      message("Calibrating ", actypes[i], " using fitted distribution.")
      vref_selection <- "Fitted"
      vref <- ifelse(
        quantile(dat_density, 0.01) > 0 & quantile(dat_density, 0.01) + 10 <= quantile(dat_density, 0.5),
        quantile(dat_density, 0.01) + 10,
        quantile(dat_density, 0.5))
      pcile <- quantile(dat_density, 0.01)
    }
  
  
  
    png(filename = file.path(out_plot1, paste0(actypes[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
    hist(
      dat2$vref,
      main=paste0("Vref Distribution - ", actypes[i]),
      xlab="Vref (kts)",
      breaks = seq(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5, 1),
      prob = T,
      right = F
    )
    lines(dat_density, col = "blue")
    abline(v = vref, col = "red", lty = 2)
    abline(v = pcile, col = "red", lty = 2)
    dev.off()
  
    type_params_req[[i]] <- data.table(
      Follower_Aircraft_Type = actypes[i],
      N = N %>% ifelse(length(.) > 0, ., NA),
      a1 = a1 %>% ifelse(length(.) > 0, ., NA),
      a2 = a2 %>% ifelse(length(.) > 0, ., NA),
      b = b %>% ifelse(length(.) > 0, ., NA),
      n1 = n1 %>% ifelse(length(.) > 0, ., NA),
      n2 = n2 %>% ifelse(length(.) > 0, ., NA),
      vref = vref %>% ifelse(length(.) > 0, ., NA),
      vref_selection = vref_selection
    )
  
    
    
    
    #Set the Vref for leader and follower and then Vtgt as these Vref with the wind adjustment added back
  
    vref_lead <- as.numeric(vref)
  
    vref_foll <- max(vref + 5, median(dat2$vref), na.rm = T)
  
    Vtgt_lead <- vref_lead + calc_landing_adjustment(dat2 %>% select(lss_type) %>% distinct(), reference_wind)
    Vtgt_foll <- vref_foll + calc_landing_adjustment(dat2 %>% select(lss_type) %>% distinct(), reference_wind)
  
  
    #Assigning the predetermined values (from wrapper function)
    type_adaptation[[i]] <- type_adaptation_input_table
    type_adaptation_modified[[i]] <- type_adaptation_input_table
  
    #Calculating an observed time of flight (12 DME to thresh) from median positions and speeds
  
  
  
  
    time_per_ac_type$time_lead_observed_alternate[[i]] <- get_time_of_flight(type_adaptation_input_table$End_Initial_Deceleration_Distance_Lead,
                                                                             n2, n1, type_adaptation_input_table$Steady_Procedural_Speed_Lead,
                                                                             Vtgt_lead)
  
    time_per_ac_type$time_foll_observed_alternate[[i]] <- get_time_of_flight(type_adaptation_input_table$End_Initial_Deceleration_Distance_Follower,
                                                                             n2, n1, type_adaptation_input_table$Steady_Procedural_Speed_Follower,
                                                                             Vtgt_foll)
  
    # Using a deceleration rate adjustment for aircraft stabilising before the 1000ft gate
  
  
  
    if (n1 >= thousand_ft_gate) {
  
      time_per_ac_type$m2_lead[[i]] <- get_decel_dist(type_adaptation_input_table$End_Initial_Deceleration_Distance_Lead,
                                                      time_per_ac_type$time_lead_observed_alternate[[i]],
                                                      thousand_ft_gate,
                                                      type_adaptation_input_table$Steady_Procedural_Speed_Lead,
                                                      Vtgt_lead)
  
      time_per_ac_type$m2_foll[[i]] <- get_decel_dist(type_adaptation_input_table$End_Initial_Deceleration_Distance_Follower,
                                                      time_per_ac_type$time_foll_observed_alternate[[i]],
                                                      thousand_ft_gate,
                                                      type_adaptation_input_table$Steady_Procedural_Speed_Follower,
                                                      Vtgt_foll)
  
      time_per_ac_type$a_lead[[i]] <- get_decel(time_per_ac_type$m2_lead[[i]],
                                                thousand_ft_gate,
                                                type_adaptation_input_table$Steady_Procedural_Speed_Lead,
                                                Vtgt_lead)
  
      time_per_ac_type$a_foll[[i]] <- get_decel(time_per_ac_type$m2_foll[[i]],
                                                thousand_ft_gate,
                                                type_adaptation_input_table$Steady_Procedural_Speed_Follower,
                                                Vtgt_foll)
    }
  
    # Using a Vtgt adjustment for aircraft stabilising after the 1000ft gate
  
    if (n1 < thousand_ft_gate) {
  
      time_per_ac_type$a_lead[[i]] <- d
      time_per_ac_type$adjusted_Vtgt_lead[[i]] <- adjust_Vref(type_adaptation_input_table$End_Initial_Deceleration_Distance_Lead, thousand_ft_gate, n2, type_adaptation_input_table$Steady_Procedural_Speed_Lead, time_per_ac_type$time_lead_observed_alternate[[i]], vref_lead)
      vref_lead <- time_per_ac_type$adjusted_Vtgt_lead[[i]] - calc_landing_adjustment(dat2 %>% select(lss_type) %>% distinct(), reference_wind)
  
      time_per_ac_type$a_foll[[i]] <- d
      time_per_ac_type$adjusted_Vtgt_foll[[i]] <- adjust_Vref(type_adaptation_input_table$End_Initial_Deceleration_Distance_Follower, thousand_ft_gate, n2, type_adaptation_input_table$Steady_Procedural_Speed_Follower, time_per_ac_type$time_foll_observed_alternate[[i]], vref_foll)
      vref_foll <- time_per_ac_type$adjusted_Vtgt_foll[[i]] - calc_landing_adjustment(dat2 %>% select(lss_type) %>% distinct(), reference_wind)
    }
  
  
    time_per_ac_type$a1[[i]] <- a1
    time_per_ac_type$Vtgt_lead[[i]] <- Vtgt_lead
    time_per_ac_type$Vtgt_foll[[i]] <- Vtgt_foll
    time_per_ac_type$n1[[i]] <- n1
    time_per_ac_type$n2[[i]] <- n2
  
  
    time_per_ac_type$a_original[[i]] <- round(d, 1)
  
  
    type_adaptation[[i]]$Aircraft_Type <- actypes[i]
    type_adaptation[[i]]$Landing_Stabilisation_Speed_Type_Lead <- unique(dat2$lss_type)
    type_adaptation[[i]]$Landing_Stabilisation_Speed_Type_Follower <- unique(dat2$lss_type)
    type_adaptation[[i]]$Min_Safe_Landing_Speed_Lead <- round(vref, 1)
    type_adaptation[[i]]$Min_Safe_Landing_Speed_Follower <- max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T)
    type_adaptation[[i]]$Final_Deceleration_Lead <- round(d, 1)
    type_adaptation[[i]]$Final_Deceleration_Follower <- round(d, 1)
  
  
    type_adaptation_modified[[i]]$Aircraft_Type <- actypes[i]
    type_adaptation_modified[[i]]$Landing_Stabilisation_Speed_Type_Lead <- unique(dat2$lss_type)
    type_adaptation_modified[[i]]$Landing_Stabilisation_Speed_Type_Follower <- unique(dat2$lss_type)
    type_adaptation_modified[[i]]$Min_Safe_Landing_Speed_Lead <- round(vref_lead, 1)
    type_adaptation_modified[[i]]$Min_Safe_Landing_Speed_Follower <- round(vref_foll, 1)
    type_adaptation_modified[[i]]$Final_Deceleration_Lead <- round(time_per_ac_type$a_lead[[i]], 1)
    type_adaptation_modified[[i]]$Final_Deceleration_Follower <- round(time_per_ac_type$a_foll[[i]], 1)
  
  
    png(filename = file.path(out_plot2, paste0(actypes[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
    print({
      ggplot(data = dat2, aes(x = Surface_Headwind, y = a1)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        scale_y_continuous(expand = c(0, 0.5)) +
        labs(title = actypes[i], x = "Surface Headwind (kts)", y = "Landing Speed / Fitted parameter a1 (kts)") +
        theme_classic()
    })
    dev.off()
  
    sink(file.path(out_plot2, "summary.txt"), append = T)
    cat(actypes[i], "\n")
    print(summary(lm(a1 ~ Surface_Headwind, data = dat2)))
    cat("# ----------------------------------------------------------------------- #\n\n")
    sink()
  
  }
  
  
  # Filter out all aircraft below empirical_threshold for the type adaptation
  # or not in the list of aircraft to create an adaptation for manually
  
  if(use_Vref_Adjust == T) {
    type_adaptation_all_aircraft <- type_adaptation_modified
    type_adaptation_out <- rbindlist(type_adaptation_modified) %>%
      filter(Aircraft_Type %in% (unique_aircraft_types %>% filter(N >= empirical_threshold) %>% .$Follower_Aircraft_Type) | Aircraft_Type %in% additional_aircraft_to_output)
  } else {
    type_adaptation_all_aircraft <- type_adaptation
    type_adaptation_out <- rbindlist(type_adaptation) %>%
      filter(Aircraft_Type %in% (unique_aircraft_types %>% filter(N >= empirical_threshold) %>% .$Follower_Aircraft_Type) | Aircraft_Type %in% additional_aircraft_to_output)
  }
  
  
  
  fwrite(rbindlist(type_params_req, use.names=T, fill=T), file.path(out_dir, "Parameters_Aircraft_Type.csv"))
  
  fwrite(type_adaptation_out, file.path(out_dir, paste0("Populate_tbl_ORD_Aircraft_Adaptation_", Airport_Code, ".csv")))
  
  fwrite(rbindlist(type_adaptation_all_aircraft), file.path(out_dir, paste0("All_Aircraft_Adaptation_Parameters_", Airport_Code, ".csv")))
  
  fwrite(time_per_ac_type, file.path(out_dir, paste0("Aircraft_Types_m2_decel_", Airport_Code, ".csv")))
  
  zipr(zipfile = paste0(out_plot1, ".zip"), files = list.files(out_plot1, pattern = ".png", full.names = T))
  
  zipr(zipfile = paste0(out_plot2, ".zip"), files = list.files(out_plot2, pattern = ".png", full.names = T))
  
  # # ----------------------------------------------------------------------- #
  # # Second Pass at Type Params to filter for the decel values near vref -----
  # # ----------------------------------------------------------------------- #
  # # MC 16/02 Commented Out for NAV WORK - wasn't used in the end for LVNL
  #
  # type_data <- rbindlist(type_adaptation)[, c("Aircraft_Type", "Min_Safe_Landing_Speed_Lead")]
  #
  #
  # modeldata_filtered_b$vref <- modeldata_filtered_b$a1 - modeldata_filtered_b$landing_adjustment
  # modeldata_filtered_b <- merge(modeldata_filtered_b, type_data, by.x = c("Follower_Aircraft_Type"), by.y = c("Aircraft_Type"))
  #
  # modeldata_filtered_n1$vref <- modeldata_filtered_n1$a1 - modeldata_filtered_n1$landing_adjustment
  # modeldata_filtered_n1 <- merge(modeldata_filtered_n1, type_data, by.x = c("Follower_Aircraft_Type"), by.y = c("Aircraft_Type"))
  #
  # modeldata_filtered_n2$vref <- modeldata_filtered_n2$a1 - modeldata_filtered_n2$landing_adjustment
  # modeldata_filtered_n2 <- merge(modeldata_filtered_n2, type_data, by.x = c("Follower_Aircraft_Type"), by.y = c("Aircraft_Type"))
  #
  # modeldata_filtered_d$vref <- modeldata_filtered_d$a1 - modeldata_filtered_d$landing_adjustment
  # modeldata_filtered_d <- merge(modeldata_filtered_d, type_data, by.x = c("Follower_Aircraft_Type"), by.y = c("Aircraft_Type"))
  #
  # type_params_2 <- rbindlist(list(
  #   parameter_summary(modeldata_filtered_a1[,c("Follower_Aircraft_Type", "a1")]),
  #   parameter_summary(modeldata_filtered_a2[,c("Follower_Aircraft_Type", "a2")]),
  #   parameter_summary(modeldata_filtered_b[vref >= Min_Safe_Landing_Speed_Lead - min_vref & vref <= Min_Safe_Landing_Speed_Lead + max_vref,c("Follower_Aircraft_Type", "b")]),
  #   parameter_summary(modeldata_filtered_n1[vref >= Min_Safe_Landing_Speed_Lead - min_vref & vref <= Min_Safe_Landing_Speed_Lead + max_vref,c("Follower_Aircraft_Type", "n1")]),
  #   parameter_summary(modeldata_filtered_n2[vref >= Min_Safe_Landing_Speed_Lead - min_vref & vref <= Min_Safe_Landing_Speed_Lead + max_vref,c("Follower_Aircraft_Type", "n2")]),
  #   parameter_summary(modeldata_filtered_d[vref >= Min_Safe_Landing_Speed_Lead - min_vref & vref <= Min_Safe_Landing_Speed_Lead + max_vref,c("Follower_Aircraft_Type", "d")]),
  #   parameter_summary(modeldata[,c("Follower_Aircraft_Type", "d2")])
  # ))
  #
  # actypes_2 <- unique(type_params_2$Follower_Aircraft_Type)
  #
  # type_params_req_2 <- list()
  # type_adaptation_2 <- list()
  # type_params_2 <- as.data.table(type_params_2)
  # modeldata_2 <- as.data.table(modeldata)
  #
  # fwrite(type_params_2, file.path(out_dir, "Parameter_Summary_Aircraft_Type_Decel.csv"))
  #
  # for (i in 1:length(actypes_2)) {
  #
  #   dat <- type_params_2[Follower_Aircraft_Type == actypes_2[i]]
  #   dat2 <- modeldata_filtered_a1[Follower_Aircraft_Type == actypes_2[i]]
  #   dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment), 0, dat2$landing_adjustment)
  #
  #   N <- dat[Type == "a1"]$N %>% ifelse(length(.) > 0, ., NA)
  #   a1 <- dat[Type == "a1"]$median %>% ifelse(length(.) > 0, ., NA)
  #   a2 <- dat[Type == "a2"]$median %>% ifelse(length(.) > 0, ., NA)
  #   b <- dat[Type == "b"]$median %>% ifelse(length(.) > 0, ., NA)
  #   n1 <- dat[Type == "n1"]$median %>% ifelse(length(.) > 0, ., NA)
  #   n2 <- dat[Type == "n2"]$median %>% ifelse(length(.) > 0, ., NA)
  #   d <- ifelse(b - a2 > 0 & n2 != n1, (b-a2)/(n2-n1), dat[Type == "d"]$median) %>% ifelse(length(.) > 0, ., NA)
  #   d2 <- dat[Type == "d2"]$median %>% ifelse(length(.) > 0, ., NA)
  #
  #   # Generate normal distribution using mean and sd of vref
  #   # vref_dist <- rnorm(1e6, mean = mean(dat2$vref), sd = sd(dat2$vref))
  #
  #   if (grepl("^LVNL_.*$", database)) {
  #
  #     if (length(dat2$vref) > 1 & (N >= observation_threshold | actypes[i] %in% additional_types)) {
  #       dat_density <- density(dat2$vref %>% .[!is.na(.)], n = max(512, length(dat2$vref %>% .[!is.na(.)])))
  #     } else {
  #       message("Not enough aircrafts for ", actypes_2[i])
  #       next
  #     }
  #
  #   } else {
  #
  #     if (length(dat2$vref) > 1 & N >= observation_threshold) {
  #       dat_density <- density(dat2$vref %>% .[!is.na(.)], n = max(512, length(dat2$vref %>% .[!is.na(.)])))
  #     } else {
  #       message("Not enough aircrafts for ", actypes_2[i])
  #       next
  #     }
  #
  #   }
  #
  #   if (nrow(dat2[Follower_Aircraft_Type == actypes_2[i]]) >= empirical_threshold) {
  #     message("Calibrating ", actypes_2[i], " using empirical distribution.")
  #     vref_selection <- "Empirical"
  #     vref <- ifelse(
  #       quantile(dat2$vref, 0.01) + 10 <= median(dat2$vref),
  #       quantile(dat2$vref, 0.01) + 10,
  #       median(dat2$vref))
  #     pcile <- quantile(dat2$vref, 0.01)
  #   } else {
  #     message("Calibrating ", actypes_2[i], " using fitted distribution.")
  #     vref_selection <- "Fitted"
  #     vref <- ifelse(
  #       quantile(dat_density, 0.01) > 0 & quantile(dat_density, 0.01) + 10 <= quantile(dat_density, 0.5),
  #       quantile(dat_density, 0.01) + 10,
  #       quantile(dat_density, 0.5))
  #     pcile <- quantile(dat_density, 0.01)
  #   }
  #
  #   # Remove vref outliers
  #   vref <- vref[!(vref %in% boxplot(vref, plot = F)$out)]
  #
  #   # png(filename = file.path(out_plot1, paste0(actypes_2[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
  #   # hist(
  #   #   dat2$vref,
  #   #   main=paste0("Vref Distribution - ", actypes[i]),
  #   #   xlab="Vref (kts)",
  #   #   breaks = seq(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5, 1),
  #   #   prob = T,
  #   #   right = F
  #   # )
  #   # lines(dat_density, col = "blue")
  #   # abline(v = vref, col = "red", lty = 2)
  #   # abline(v = pcile, col = "red", lty = 2)
  #   # dev.off()
  #
  #   type_params_req_2[[i]] <- data.table(
  #     Follower_Aircraft_Type = actypes_2[i],
  #     N = N %>% ifelse(length(.) > 0, ., NA),
  #     a1 = a1 %>% ifelse(length(.) > 0, ., NA),
  #     a2 = a2 %>% ifelse(length(.) > 0, ., NA),
  #     b = b %>% ifelse(length(.) > 0, ., NA),
  #     n1 = n1 %>% ifelse(length(.) > 0, ., NA),
  #     n2 = n2 %>% ifelse(length(.) > 0, ., NA),
  #     vref = vref %>% ifelse(length(.) > 0, ., NA),
  #     vref_selection = vref_selection
  #   )
  #
  #   type_adaptation_2[[i]] <- data.table(
  #     Aircraft_Type = actypes_2[i],
  #     Compression_Commencement_Threshold = 10,
  #     Landing_Stabilisation_Speed_Type_Lead = unique(dat2$lss_type),
  #     Landing_Stabilisation_Speed_Type_Follower = unique(dat2$lss_type),
  #     Min_Safe_Landing_Speed_Lead = round(vref, 1),
  #     Min_Safe_Landing_Speed_Follower = max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T),
  #     Apply_Gusting_Lead = 1,
  #     Apply_Gusting_Follower = 1,
  #     Local_Stabilisation_Distance_Lead = 45,
  #     Local_Stabilisation_Distance_Follower = 4.5,
  #     Steady_Procedural_Speed_Lead = 170,
  #     Steady_Procedural_Speed_Follower = 172,
  #     Final_Deceleration_Lead = round(d, 1),
  #     Final_Deceleration_Follower = round(d, 1),
  #     End_Initial_Deceleration_Distance_Lead = 12,
  #     End_Initial_Deceleration_Distance_Follower = 12,
  #     Initial_Procedural_Speed_Lead = 170,
  #     Initial_Procedural_Speed_Follower = 172,
  #     Initial_Deceleration_Lead = d2,
  #     Initial_Deceleration_Follower = d2
  #   )
  #
  #   # png(filename = file.path(out_plot2, paste0(actypes_2[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
  #   # print({
  #   #   ggplot(data = dat2, aes(x = Surface_Headwind, y = a1)) +
  #   #     geom_point() +
  #   #     geom_smooth(method = "lm", se = F) +
  #   #     scale_y_continuous(expand = c(0, 0.5)) +
  #   #     labs(title = actypes[i], x = "Surface Headwind (kts)", y = "Landing Speed / Fitted parameter a1 (kts)") +
  #   #     theme_classic()
  #   # })
  #   # dev.off()
  #
  #   # sink(file.path(out_plot2, "summary.txt"), append = T)
  #   # cat(actypes[i], "\n")
  #   # print(summary(lm(a1 ~ Surface_Headwind, data = dat2)))
  #   # cat("# ----------------------------------------------------------------------- #\n\n")
  #   # sink()
  #
  # }
  #
  # fwrite(rbindlist(type_params_req_2, use.names=T, fill=T), file.path(out_dir, "Parameters_Aircraft_Type_Decel.csv"))
  #
  # fwrite(rbindlist(type_adaptation_2), file.path(out_dir, paste0("Populate_tbl_ORD_Aircraft_Adaptation_", Airport_Code, "_Decel.csv")))
  
  # ----------------------------------------------------------------------- #
  # Wake Category Parameter Summary & Adaptation ----------------------------
  # ----------------------------------------------------------------------- #
  
  if (grepl("^LVNL_.*$", database)) {
  
    wake_params <- rbindlist(list(
      # parameter_summary(modeldata_filtered_a1, "Follow_RECAT", "a1"),
      # parameter_summary(modeldata_filtered_a2, "Follow_RECAT", "a2"),
      # parameter_summary(modeldata_filtered_b, "Follow_RECAT", "b"),
      # parameter_summary(modeldata_filtered_n1, "Follow_RECAT", "n1"),
      # parameter_summary(modeldata_filtered_n2, "Follow_RECAT", "n2"),
      # parameter_summary(modeldata_filtered_d, "Follow_RECAT", "d"),
      # parameter_summary(modeldata, "Follow_RECAT", "d2")
      parameter_summary(modeldata_filtered_a1[!(Follower_Aircraft_Type %in% removed_types),c("Follow_RECAT", "a1")]),
      parameter_summary(modeldata_filtered_a2[!(Follower_Aircraft_Type %in% removed_types),c("Follow_RECAT", "a2")]),
      parameter_summary(modeldata_filtered_b[!(Follower_Aircraft_Type %in% removed_types),c("Follow_RECAT", "b")]),
      parameter_summary(modeldata_filtered_n1[!(Follower_Aircraft_Type %in% removed_types),c("Follow_RECAT", "n1")]),
      parameter_summary(modeldata_filtered_n2[!(Follower_Aircraft_Type %in% removed_types),c("Follow_RECAT", "n2")]),
      parameter_summary(modeldata_filtered_d[!(Follower_Aircraft_Type %in% removed_types),c("Follow_RECAT", "d")]),
      parameter_summary(modeldata[,c("Follow_RECAT", "d2")])
    ))
  
  } else {
  
    wake_params <- rbindlist(list(
      # parameter_summary(modeldata_filtered_a1, "Follow_RECAT", "a1"),
      # parameter_summary(modeldata_filtered_a2, "Follow_RECAT", "a2"),
      # parameter_summary(modeldata_filtered_b, "Follow_RECAT", "b"),
      # parameter_summary(modeldata_filtered_n1, "Follow_RECAT", "n1"),
      # parameter_summary(modeldata_filtered_n2, "Follow_RECAT", "n2"),
      # parameter_summary(modeldata_filtered_d, "Follow_RECAT", "d"),
      # parameter_summary(modeldata, "Follow_RECAT", "d2")
      parameter_summary(modeldata_filtered_a1[,c("Follow_RECAT", "a1")]),
      parameter_summary(modeldata_filtered_a2[,c("Follow_RECAT", "a2")]),
      parameter_summary(modeldata_filtered_b[,c("Follow_RECAT", "b")]),
      parameter_summary(modeldata_filtered_n1[,c("Follow_RECAT", "n1")]),
      parameter_summary(modeldata_filtered_n2[,c("Follow_RECAT", "n2")]),
      parameter_summary(modeldata_filtered_d[,c("Follow_RECAT", "d")]),
      parameter_summary(modeldata[,c("Follow_RECAT", "d2")])
    ))
  
  }
  
  fwrite(wake_params, file.path(out_dir, "Parameter_Summary_Wake.csv"))
  
  wake_cats <- unique(wake_params$Follow_RECAT)
  
  wake_params_req <- list()
  wake_adaptation_old <- list()
  wake_adaptation <- list()
  
  sink(file.path(out_plot4, "summary.txt"))
  sink()
  
  # Initialising a df to display and calculate all parameters corresponding to the
  # adjustment to account for 1000ft gate limitation
  
  time_per_wake_cat <- wake_cats %>% enframe() %>% rename(Wake_Cat = value) %>% select("Wake_Cat")
  
  time_per_wake_cat$time_lead_observed_alternate <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$time_foll_observed_alternate <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$a_lead <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$a_foll <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$a_original <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$a1 <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$Vtgt_lead <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$Vtgt_foll <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$adjusted_Vtgt_lead <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$adjusted_Vtgt_foll <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$n1 <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$n2 <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$m2_lead <- rep(NA, nrow(time_per_wake_cat))
  time_per_wake_cat$m2_foll <- rep(NA, nrow(time_per_wake_cat))
  
  for (i in 1:length(wake_cats)) {
  
    dat <- as.data.table(wake_params)[Follow_RECAT == wake_cats[i]]
  
    if (grepl("^LVNL_.*$", database)) {
      dat2 <- as.data.table(modeldata_filtered_a2)[Follow_RECAT == wake_cats[i] & !(Follower_Aircraft_Type %in% removed_types)]
    } else {
      dat2 <- as.data.table(modeldata_filtered_a2)[Follow_RECAT == wake_cats[i]]
    }
  
    dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment_boeing), 0, dat2$landing_adjustment_boeing)
  
    # Generate normal distribution using mean and sd of vref
    # vref_dist <- rnorm(1e6, mean = mean(dat2$vref), sd = sd(dat2$vref))
  
    dat_density <- density(dat2$vref %>% .[!is.na(.)], n = max(512, length(dat2$vref %>% .[!is.na(.)])))
  
    N <- dat[Type == "a1"]$N %>% ifelse(length(.) > 0, ., NA)
    a1 <- dat[Type == "a1"]$median %>% ifelse(length(.) > 0, ., NA)
    a2 <- dat[Type == "a2"]$median %>% ifelse(length(.) > 0, ., NA)
    b <- dat[Type == "b"]$median %>% ifelse(length(.) > 0, ., NA)
    n1 <- dat[Type == "n1"]$median %>% ifelse(length(.) > 0, ., NA)
    n2 <- dat[Type == "n2"]$median %>% ifelse(length(.) > 0, ., NA)
    # d <- ifelse(b - a2 > 0 & n2 != n1, (b-a2)/(n2-n1), dat[Type == "d"]$median) %>% ifelse(length(.) > 0, ., NA)
    d <- ifelse(type_adaptation_input_table$Steady_Procedural_Speed_Lead - a2 > 0 & n2 != n1, (type_adaptation_input_table$Steady_Procedural_Speed_Lead-a2)/(n2-n1), dat[Type == "d"]$median) %>% ifelse(length(.) > 0, ., NA)
    d2 <- dat[Type == "d2"]$median %>% ifelse(length(.) > 0, ., NA)
  
  
    if (nrow(dat2[Follow_RECAT == wake_cats[i]]) >= empirical_threshold) {
      message("Calibrating ", wake_cats[i], " using empirical distribution.")
      vref_selection <- "Empirical"
      vref <- ifelse(
        quantile(dat2$vref, 0.01) + 10 <= median(dat2$vref),
        quantile(dat2$vref, 0.01) + 10,
        median(dat2$vref))
      pcile <- quantile(dat2$vref, 0.01)
    } else {
      message("Calibrating ", wake_cats[i], " using fitted distribution.")
      vref_selection <- "Fitted"
      vref <- ifelse(
        quantile(dat_density, 0.01) > 0 & quantile(dat_density, 0.01) + 10 <= quantile(dat_density, 0.5),
        quantile(dat_density, 0.01) + 10,
        quantile(dat_density, 0.5))
      pcile <- quantile(dat_density, 0.01)
    }
    
  
    png(filename = file.path(out_plot3, paste0(wake_cats[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
    hist(
      dat2$vref,
      main=paste0("Vref Distribution - ", wake_cats[i]),
      xlab="Vref (kts)",
      breaks = seq(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5, 1),
      prob = T,
      right = F
    )
    lines(dat_density, col = "blue")
    abline(v = vref, col = "red", lty = 2)
    abline(v = pcile, col = "red", lty = 2)
    dev.off()
  
    wake_params_req[[i]] <- data.table(
      Follow_RECAT = wake_cats[i],
      N = ifelse(length(N) > 0, N, NA),
      a1 = ifelse(length(a1) > 0, a1, NA),
      a2 = ifelse(length(a2) > 0, a2, NA),
      b = ifelse(length(b) > 0, b, NA),
      n1 = ifelse(length(n1) > 0, n1, NA),
      n2 = ifelse(length(n2) > 0, n2, NA),
      vref = ifelse(length(vref) > 0, vref, NA),
      vref_selection = vref_selection
    )
  
    vref_lead <- as.numeric(vref)
    vref_foll <- max(vref + 5, median(dat2$vref), na.rm = T)
  
    Vtgt_lead <- vref_lead + calc_landing_adjustment(0, reference_wind)
    Vtgt_foll <- vref_foll + calc_landing_adjustment(0, reference_wind)
  
  
    #Setting the parameters pre-defined in the table (from wraper script)
    wake_adaptation_old[[i]] <- wake_adaptation_input_table
    wake_adaptation[[i]] <- wake_adaptation_input_table
  
    #Calculating a median time of flight per wake category
  
    time_per_wake_cat$time_lead_observed_alternate[[i]] <- get_time_of_flight(wake_adaptation_input_table$End_Initial_Deceleration_Distance_Lead,
                                                                             n2, n1, wake_adaptation_input_table$Steady_Procedural_Speed_Lead,
                                                                             Vtgt_lead)
  
    time_per_wake_cat$time_foll_observed_alternate[[i]] <- get_time_of_flight(wake_adaptation_input_table$End_Initial_Deceleration_Distance_Follower,
                                                                             n2, n1, wake_adaptation_input_table$Steady_Procedural_Speed_Follower,
                                                                             Vtgt_foll)
  
    if (n1 >= thousand_ft_gate) {
  
      time_per_wake_cat$m2_lead[[i]] <- get_decel_dist(wake_adaptation_input_table$End_Initial_Deceleration_Distance_Lead,
                                                       time_per_wake_cat$time_lead_observed_alternate[[i]],
                                                       thousand_ft_gate,
                                                       wake_adaptation_input_table$Steady_Procedural_Speed_Lead,
                                                       Vtgt_lead)
  
      time_per_wake_cat$m2_foll[[i]] <- get_decel_dist(wake_adaptation_input_table$End_Initial_Deceleration_Distance_Follower,
                                                       time_per_wake_cat$time_foll_observed_alternate[[i]],
                                                       thousand_ft_gate,
                                                       wake_adaptation_input_table$Steady_Procedural_Speed_Follower,
                                                       Vtgt_foll)
  
      time_per_wake_cat$a_lead[[i]] <- get_decel(time_per_wake_cat$m2_lead[[i]],
                                                 thousand_ft_gate,
                                                 wake_adaptation_input_table$Steady_Procedural_Speed_Lead,
                                                 Vtgt_lead)
  
      time_per_wake_cat$a_foll[[i]] <- get_decel(time_per_wake_cat$m2_foll[[i]],
                                                 thousand_ft_gate,
                                                 wake_adaptation_input_table$Steady_Procedural_Speed_Follower,
                                                 Vtgt_foll)
    }
  
    if (n1 < thousand_ft_gate) {
  
      time_per_wake_cat$a_lead[[i]] <- d
      time_per_wake_cat$adjusted_Vtgt_lead[[i]] <- adjust_Vref(wake_adaptation_input_table$End_Initial_Deceleration_Distance_Lead,
                                                               thousand_ft_gate,
                                                               n2,
                                                               wake_adaptation_input_table$Steady_Procedural_Speed_Lead,
                                                               time_per_wake_cat$time_lead_observed_alternate[[i]],
                                                               vref_lead)
  
      vref_lead <- time_per_wake_cat$adjusted_Vtgt_lead[[i]] - calc_landing_adjustment(0, reference_wind)
  
      time_per_wake_cat$a_foll[[i]] <- d
      time_per_wake_cat$adjusted_Vtgt_foll[[i]] <- adjust_Vref(wake_adaptation_input_table$End_Initial_Deceleration_Distance_Follower,
                                                               thousand_ft_gate,
                                                               n2,
                                                               wake_adaptation_input_table$Steady_Procedural_Speed_Follower,
                                                               time_per_wake_cat$time_foll_observed_alternate[[i]],
                                                               vref_foll)
  
      vref_foll <- time_per_wake_cat$adjusted_Vtgt_foll[[i]] - calc_landing_adjustment(0, reference_wind)
    }
  
    time_per_wake_cat$a1[[i]] <- a1
    time_per_wake_cat$Vtgt_lead[[i]] <- Vtgt_lead
    time_per_wake_cat$Vtgt_foll[[i]] <- Vtgt_foll
    time_per_wake_cat$n1[[i]] <- n1
    time_per_wake_cat$n2[[i]] <- n2
    time_per_wake_cat$a_original[[i]] <- round(d, 1)
  
    #Setting parameters not defined in the wrapper
    wake_adaptation_old[[i]]$Wake_Cat <- wake_cats[i]
    wake_adaptation_old[[i]]$Min_Safe_Landing_Speed_Lead <- round(vref, 1)
    wake_adaptation_old[[i]]$Min_Safe_Landing_Speed_Follower <- max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T)
    wake_adaptation_old[[i]]$Final_Deceleration_Lead <- round(d, 1)
    wake_adaptation_old[[i]]$Final_Deceleration_Follower <- round(d, 1)
    # wake_adaptation[[i]]$Initial_Deceleration_Lead <- d2
    # wake_adaptation[[i]]$Initial_Deceleration_Follower <- d2
  
    wake_adaptation[[i]]$Wake_Cat <- wake_cats[i]
    wake_adaptation[[i]]$Landing_Stabilisation_Speed_Type_Lead <- 0
    wake_adaptation[[i]]$Landing_Stabilisation_Speed_Type_Follower <- 0
    wake_adaptation[[i]]$Min_Safe_Landing_Speed_Lead <- round(vref_lead, 1)
    wake_adaptation[[i]]$Min_Safe_Landing_Speed_Follower <- round(vref_foll, 1)
    wake_adaptation[[i]]$Final_Deceleration_Lead <- round(time_per_wake_cat$a_lead[[i]], 1)
    wake_adaptation[[i]]$Final_Deceleration_Follower <- round(time_per_wake_cat$a_foll[[i]], 1)
  
    #Convert back to a table with:
    ######     wake_adaptation <- rbindlist(wake_adaptation)
  
  
    # wake_adaptation[[i]] <- data.table(
    #   Wake_Cat = wake_cats[i],
    #   Compression_Commencement_Threshold = 4,
    #   Landing_Stabilisation_Speed_Type_Lead = 0,
    #   Landing_Stabilisation_Speed_Type_Follower = 0,
    #   Min_Safe_Landing_Speed_Lead = round(vref, 1),
    #   Min_Safe_Landing_Speed_Follower = max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T),
    #   Apply_Gusting_Lead = 1,
    #   Apply_Gusting_Follower = 1,
    #   Local_Stabilisation_Distance_Lead = 4,
    #   Local_Stabilisation_Distance_Follower = 4,
    #   Steady_Procedural_Speed_Lead = 160,
    #   Steady_Procedural_Speed_Follower = 162,
    #   Final_Deceleration_Lead = round(d, 1),
    #   Final_Deceleration_Follower = round(d, 1),
    #   End_Initial_Deceleration_Distance_Lead = 12,
    #   End_Initial_Deceleration_Distance_Follower = 12,
    #   Initial_Procedural_Speed_Lead = 160,
    #   Initial_Procedural_Speed_Follower = 162,
    #   Initial_Deceleration_Lead = d2,
    #   Initial_Deceleration_Follower = d2
    # )
  
  
  
    png(filename = file.path(out_plot4, paste0(wake_cats[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
    print({
      ggplot(data = dat2, aes(x = Surface_Headwind, y = a1)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        scale_y_continuous(expand = c(0, 0.5)) +
        labs(title = wake_cats[i], x = "Surface Headwind (kts)", y = "Landing Speed / Fitted parameter a1 (kts)") +
        theme_classic()
    })
    dev.off()
  
    sink(file.path(out_plot4, "summary.txt"), append = T)
    cat(wake_cats[i], "\n")
    print(summary(lm(a1 ~ Surface_Headwind, data = dat2)))
    cat("# ----------------------------------------------------------------------- #\n\n")
    sink()
  
  }
  
  if(use_Vref_Adjust == T) {
    wake_adaptation <- rbindlist(wake_adaptation)
  } else {
    wake_adaptation <- rbindlist(wake_adaptation_old)
  }
  
  fwrite(rbindlist(wake_params_req, use.names=T, fill=T), file.path(out_dir, "Parameters_Wake.csv"))
  
  # fwrite(rbindlist(wake_adaptation), file.path(out_dir, paste0("Populate_tbl_ORD_Wake_Adaptation_", Airport_Code, ".csv")))
  fwrite(wake_adaptation, file.path(out_dir, paste0("Populate_tbl_ORD_Wake_Adaptation_", Airport_Code, ".csv")))
  
  zipr(zipfile = paste0(out_plot3, ".zip"), files = list.files(out_plot3, pattern = ".png", full.names = T))
  
  zipr(zipfile = paste0(out_plot4, ".zip"), files = list.files(out_plot4, pattern = ".png", full.names = T))
  
  run_extended <- F
  if (run_extended == T){
  
    f_type_list <- filter(modeldata, wake == "F", !(Follower_Aircraft_Type %in% c("TBM8", "TBM9", "PC12", "SF50", "H25B"))) %>% select(Follower_Aircraft_Type) %>% distinct()
  
    tbm8 <- generate_aircraft_parameters(c("TBM8", "TBM9"), "TBM8", "F", F)
    tbm9 <- generate_aircraft_parameters(c("TBM8", "TBM9"), "TBM9", "F", F)
    pc12 <- generate_aircraft_parameters(c("PC12"), "PC12", "E", F)
    sf50 <- generate_aircraft_parameters(c("SF50"), "SF50", "F", F)
    h25b <- generate_aircraft_parameters(c("H25B"), "H25B", "F", F)
    C56x <- generate_aircraft_parameters(c("C56X"), "C56X", "E", F)
    CL35 <- generate_aircraft_parameters(c("CL35"), "CL35", "E", F)
    CRJ9 <- generate_aircraft_parameters(c("CRJ9"), "CRJ9", "E", F)
    GLEX <- generate_aircraft_parameters(c("GLEX"), "GLEX", "E", F)
    GLF5 <- generate_aircraft_parameters(c("GLF5"), "GLF5", "E", F)
  
  
    f_types <- generate_aircraft_parameters(f_type_list$Follower_Aircraft_Type, "F", "E", T)
  
    new_adaptation <- rbindlist(list(tbm8[[1]], tbm8[[1]], pc12[[1]], sf50[[1]], h25b[[1]], C56x[[1]], CL35[[1]], CRJ9[[1]], GLEX[[1]], GLF5[[1]], f_types[[1]]), use.names = T)
    fwrite(new_adaptation, file.path(out_dir, "additional_adaptation_params.csv"))
  
    compare_aircraft_parameters(c("B77L", "B78X"), "E", F)
  
    b78x <- filter(modeldata, Follower_Aircraft_Type == "B78X")
  
    generate_aircraft_parameters <- function(type_list, actype, fit, use_boeing) {
  
      dat <- filter(type_params, Follower_Aircraft_Type %in% type_list)
      dat2 <- filter(modeldata_filtered_a1, Follower_Aircraft_Type %in% type_list)
  
      dat <- as.data.table(dat)
      dat2 <- as.data.table(dat2)
  
      #dat <- type_params[Follower_Aircraft_Type == actypes[i]]
      #dat2 <- modeldata_filtered_a1[Follower_Aircraft_Type == actypes[i]]
      if (use_boeing == T){
        dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment_boeing), 0, dat2$landing_adjustment_boeing)
      } else {
        dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment), 0, dat2$landing_adjustment)
  
      }
      #actype = dat2[1, ]$Follower_Aircraft_Type
  
      N <- dat[Type == "a1"]$N %>% ifelse(length(.) > 0, ., NA)
      a1 <- dat[Type == "a1"]$median %>% ifelse(length(.) > 0, ., NA)
      a2 <- dat[Type == "a2"]$median %>% ifelse(length(.) > 0, ., NA)
      b <- dat[Type == "b"]$median %>% ifelse(length(.) > 0, ., NA)
      n1 <- dat[Type == "n1"]$median %>% ifelse(length(.) > 0, ., NA)
      n2 <- dat[Type == "n2"]$median %>% ifelse(length(.) > 0, ., NA)
      d <- ifelse(b - a2 > 0 & n2 != n1, (b-a2)/(n2-n1), dat[Type == "d"]$median) %>% ifelse(length(.) > 0, ., NA)
      d2 <- dat[Type == "d2"]$median %>% ifelse(length(.) > 0, ., NA)
  
      # Generate normal distribution using mean and sd of vref
      # vref_dist <- rnorm(1e6, mean = mean(dat2$vref), sd = sd(dat2$vref))
  
      if (length(dat2$vref) > 1 & N >= 5) {
        dat_density <- density(dat2$vref %>% .[!is.na(.)], n = max(512, length(dat2$vref %>% .[!is.na(.)])))
      } else {
        message("Not enough aircrafts for ", actype)
        next
      }
  
  
      if (fit == "E") {
        message("Calibrating ", actype, " using empirical distribution.")
        vref_selection <- "Empirical"
        vref <- ifelse(
          quantile(dat2$vref, 0.01) + 10 <= median(dat2$vref),
          quantile(dat2$vref, 0.01) + 10,
          median(dat2$vref))
        pcile <- quantile(dat2$vref, 0.01)
      } else {
        message("Calibrating ", actype, " using fitted distribution.")
        vref_selection <- "Fitted"
        vref <- ifelse(
          quantile(dat_density, 0.01) > 0 & quantile(dat_density, 0.01) + 10 <= quantile(dat_density, 0.5),
          quantile(dat_density, 0.01) + 10,
          quantile(dat_density, 0.5))
        pcile <- quantile(dat_density, 0.01)
      }
  
  
      # Remove vref outliers
      #vref <- vref[!(vref %in% boxplot(vref, plot = F)$out)]
  
      # p1 <- hist(
      #   dat2$vref,
      #   main=paste0("Vref Distribution - ", actypes[i]),
      #   xlab="Vref (kts)",
      #   breaks = seq(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5, 1),
      #   prob = T,
      #   right = F
      # )
      # lines(dat_density, col = "blue")
      # abline(v = vref, col = "red", lty = 2)
      # abline(v = pcile, col = "red", lty = 2)
  
      p1 <- ggplot(data = dat2)+
        geom_histogram(aes(x = vref, y = ..density..), color  = "black", fill = "skyblue")+
        geom_density(aes(x = vref), alpha = 0.5, color = "red")+
        geom_vline(xintercept = vref, colour = "red", linetype = "dashed")+
        geom_vline(xintercept = pcile, color = "red", linetype = "dashed")+
        scale_x_continuous(breaks = seq(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5, 5), limits = c(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5))+
        labs(main = paste0("Vref Distribution - ", actype), x = "Vref (kts)")+
        theme_bw()
  
      type_params_req <- data.table(
        Follower_Aircraft_Type = actype,
        N = N %>% ifelse(length(.) > 0, ., NA),
        a1 = a1 %>% ifelse(length(.) > 0, ., NA),
        a2 = a2 %>% ifelse(length(.) > 0, ., NA),
        b = b %>% ifelse(length(.) > 0, ., NA),
        n1 = n1 %>% ifelse(length(.) > 0, ., NA),
        n2 = n2 %>% ifelse(length(.) > 0, ., NA),
        vref = vref %>% ifelse(length(.) > 0, ., NA),
        vref_selection = vref_selection
      )
  
  
  
      type_adaptation <- data.table(
        Aircraft_Type = actype,
        Compression_Commencement_Threshold = 4,
        Landing_Stabilisation_Speed_Type_Lead = unique(dat2$lss_type),
        Landing_Stabilisation_Speed_Type_Follower = unique(dat2$lss_type),
        Min_Safe_Landing_Speed_Lead = round(vref, 1),
        Min_Safe_Landing_Speed_Follower = max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T),
        Apply_Gusting_Lead = 1,
        Apply_Gusting_Follower = 1,
        Local_Stabilisation_Distance_Lead = 4,
        Local_Stabilisation_Distance_Follower = 4,
        Steady_Procedural_Speed_Lead = 160,
        Steady_Procedural_Speed_Follower = 162,
        Final_Deceleration_Lead = round(d, 1),
        Final_Deceleration_Follower = round(d, 1),
        End_Initial_Deceleration_Distance_Lead = 12,
        End_Initial_Deceleration_Distance_Follower = 12,
        Initial_Procedural_Speed_Lead = 160,
        Initial_Procedural_Speed_Follower = 162,
        Initial_Deceleration_Lead = d2,
        Initial_Deceleration_Follower = d2
      )
  
      p2 <- ggplot(data = dat2, aes(x = Surface_Headwind, y = a1)) +
        geom_point() +
        geom_smooth(method = "lm", se = F) +
        scale_y_continuous(expand = c(0, 0.5)) +
        labs(title = actype, x = "Surface Headwind (kts)", y = "Landing Speed / Fitted parameter a1 (kts)") +
        theme_bw()
  
      return(list("adaptation" = type_adaptation, "params" = type_params, "phist" = p1, "pscatter" = p2))
  
    }
  
  
    compare_aircraft_parameters <- function(type_list, fit, use_boeing) {
  
      dat2 <- filter(modeldata_filtered_a1, Follower_Aircraft_Type %in% type_list)
  
      if (use_boeing == T){
        dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment_boeing), 0, dat2$landing_adjustment_boeing)
      } else {
        dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment), 0, dat2$landing_adjustment)
  
      }
  
      vrefs <- group_by(dat2, Follower_Aircraft_Type) %>%
        summarise(vref = ifelse(quantile(vref, 0.01) + 10 <= median(vref),quantile(vref, 0.01) + 10, median(vref)),
                  pcile = quantile(vref, 0.01)) %>% ungroup()
  
      p1 <- ggplot(data = dat2)+
        geom_histogram(aes(x = vref, y = ..density..), color  = "black", fill = "skyblue")+
        geom_density(aes(x = vref), alpha = 0.5, color = "red")+
        geom_vline(data = vrefs, aes(xintercept = vref), colour = "red", linetype = "dashed")+
        geom_vline(data = vrefs, aes(xintercept = pcile), color = "red", linetype = "dashed")+
        scale_x_continuous(breaks = seq(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5, 5), limits = c(120, 160))+
        labs(main = paste0("Vref Distributions"), x = "Vref (kts)")+
        theme_bw()+
        facet_wrap(~Follower_Aircraft_Type)
  
      print(p1)
  
      return(p1)
  
    }
  
  
    boxplot_carrier <- function(aircraft_type){
  
      dat2 <- filter(modeldata_filtered_a1, Follower_Aircraft_Type == aircraft_type) %>% mutate(Carrier = as.factor(substr(Follower_Callsign, 1, 3)))
      dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment_boeing), 0, dat2$landing_adjustment_boeing)
      dat2 <- mutate(dat2, Carrier = fct_reorder(Carrier, vref))
  
      p1 <- ggplot(data = dat2)+
        geom_boxplot(mapping = aes(x = Carrier, y = vref, fill = Carrier))+
        theme(legend.position = "none")
  
      print(p1)
    }
  
    boxplot_carrier("B744")
    boxplot_carrier("B748")
  
    B748 <- filter(modeldata_filtered_a1, Follower_Aircraft_Type == "B748") %>% mutate(vref = a1 - landing_adjustment)
  
    quantile(filter(mutate(modeldata_filtered_a1,vref = a1 - landing_adjustment), Follower_Aircraft_Type == "B748", substr(Follower_Callsign, 1, 3)!="QTR",substr(Follower_Callsign, 1, 3)!="CPA" )$vref, 0.01) + 10
    quantile(filter(B748, vref >= 135)$vref, 0.01) + 10
  
    quantile(filter(mutate(modeldata_filtered_a1,vref = a1 - landing_adjustment), Follower_Aircraft_Type == "B744", substr(Follower_Callsign, 1, 3)!="TAY",substr(Follower_Callsign, 1, 3)!="CKS" )$vref, 0.01) + 10
  
  
  }
  
  # ----------------------------------------------------------------------- #
  # DBS adaptation parameters -----------------------------------------------
  # ----------------------------------------------------------------------- #
  
  if (use_weighted_average) {
  
    ifelse(use_Vref_Adjust == T, wake_adaptation <- wake_adaptation, wake_adaptation <- rbindlist(wake_adaptation_old))
  
    wake_dbs_lookup <- dbGetQuery(con, "EXEC usp_GI_Get_Reference_Recat_Separation_Dist_Data") %>% as.data.table()
    setnames(wake_dbs_lookup, "Reference_Wake_Separation_Distance", "DBS_Distance")
  
    #Taking historical formatting
    wake_pairs <- wake_dbs_lookup
  
    #assigning the counts from GWCS script output
    wake_pairs$Count <- Wake_Pair_Counts$n
  
    fwrite(wake_pairs, file.path(out_dir, paste0("DBS_Distance_Wake_Pairs_Count.csv")))
  
    #Presetting empty list
    dbs_adaptation_list <- list()
  
    #Creating a vector to represent unique entries in the dbs table
  
    unique_DBS_distance <- wake_pairs %>% select(DBS_Distance) %>% distinct()
    unique_DBS_distance <- sort(unique_DBS_distance$DBS_Distance)
  
  
    for (i in 1:length(unique_DBS_distance)) {
  
      wake_pairs_leader <- wake_pairs[DBS_Distance == unique_DBS_distance[i], .(Count = sum(Count)), by = Leader_WTC]
      Min_Safe_Landing_Speed_Lead_i <- sum(wake_adaptation[match(wake_pairs_leader$Leader_WTC, Wake_Cat)]$Min_Safe_Landing_Speed_Lead * wake_pairs_leader$Count) / sum(wake_pairs_leader$Count)
      Final_Deceleration_Lead_i <- sum(wake_adaptation[match(wake_pairs_leader$Leader_WTC, Wake_Cat)]$Final_Deceleration_Lead * wake_pairs_leader$Count) / sum(wake_pairs_leader$Count)
      Initial_Deceleration_Lead_i <- sum(wake_adaptation[match(wake_pairs_leader$Leader_WTC, Wake_Cat)]$Initial_Deceleration_Lead * wake_pairs_leader$Count) / sum(wake_pairs_leader$Count)
  
      wake_pairs_follower <- wake_pairs[DBS_Distance == unique_DBS_distance[i], .(Count = sum(Count)), by = Follower_WTC]
      Min_Safe_Landing_Speed_Follower_i <- sum(wake_adaptation[match(wake_pairs_follower$Follower_WTC, Wake_Cat)]$Min_Safe_Landing_Speed_Follower * wake_pairs_follower$Count) / sum(wake_pairs_follower$Count)
      Final_Deceleration_Foll_i <- sum(wake_adaptation[match(wake_pairs_follower$Follower_WTC, Wake_Cat)]$Final_Deceleration_Follower * wake_pairs_follower$Count) / sum(wake_pairs_follower$Count)
      Initial_Deceleration_Foll_i <- sum(wake_adaptation[match(wake_pairs_follower$Follower_WTC, Wake_Cat)]$Initial_Deceleration_Follower * wake_pairs_follower$Count) / sum(wake_pairs_follower$Count)
  
      #Setting the predetermined variables
      dbs_adaptation_list[[i]] <- dbs_adaptation_input_table
  
      #Setting the parameters not predefined
      dbs_adaptation_list[[i]]$DBS_Distance <- unique_DBS_distance[i]
      dbs_adaptation_list[[i]]$Min_Safe_Landing_Speed_Lead <- Min_Safe_Landing_Speed_Lead_i
      dbs_adaptation_list[[i]]$Min_Safe_Landing_Speed_Follower <- Min_Safe_Landing_Speed_Follower_i
      dbs_adaptation_list[[i]]$Final_Deceleration_Lead <- Final_Deceleration_Lead_i
      dbs_adaptation_list[[i]]$Final_Deceleration_Follower <- Final_Deceleration_Foll_i
      dbs_adaptation_list[[i]]$Initial_Deceleration_Lead <- Initial_Deceleration_Lead_i
      dbs_adaptation_list[[i]]$Initial_Deceleration_Follower <- Initial_Deceleration_Foll_i
  
    }
  
    dbs_adaptation <- rbindlist(dbs_adaptation_list)
    fwrite(dbs_adaptation, file.path(out_dir, paste0("Populate_tbl_ORD_DBS_Adaptation_", Airport_Code, ".csv")))
  
  } else {
  
    # Wake and ROT spacing distance from database
    rot_spacing_dist <- dbGetQuery(con, "EXEC usp_GI_Get_Reference_ROT_Spacing_Dist_Data") %>% as.data.table()
    wake_dbs_lookup <- dbGetQuery(con, "EXEC usp_GI_Get_Reference_Recat_Separation_Dist_Data") %>% as.data.table()
    spacings_joined <- merge(rot_spacing_dist, wake_dbs_lookup, by = c("Leader_WTC", "Follower_WTC"), all.x = T)
    spacings_joined_2 <- spacings_joined[!is.na(Reference_ROT_Spacing_Distance) & !is.na(Reference_Wake_Separation_Distance)][order(Runway, Leader_WTC, Follower_WTC)]
  
    unique_pairs <- unique(spacings_joined_2[,paste(Leader_WTC, Follower_WTC)])
  
    # Review action add begins
    unique_pairs <- unique(wake_dbs_lookup[,paste(Leader_WTC, Follower_WTC)])
    # Review action add ends
  
    all_params <- lapply(c("a1", "a2", "b", "n1", "n2", "d", "d2"), function(i) {
      x <- if (i == "a1") {
        modeldata_filtered_a1[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
      } else if (i == "a2") {
        modeldata_filtered_a2[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
      } else if (i == "b") {
        modeldata_filtered_b[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
      } else if (i == "n1") {
        modeldata_filtered_n1[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
      } else if (i == "n2") {
        modeldata_filtered_n2[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
      } else if (i == "d") {
        modeldata_filtered_d[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
      } else if (i == "d2") {
        modeldata[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
      }
      x_i <- as.numeric(x[[i]])
      return(data.table(
        N = length(x_i),
        mean = mean(x_i, na.rm = T),
        median = median(x_i, na.rm = T),
        sd = sd(x_i, na.rm = T),
        p5 = quantile(x_i, 0.05, na.rm = T),
        p10 = quantile(x_i, 0.1, na.rm = T),
        Type = i
      ))
    }) %>% rbindlist()
  
    dat <- all_params
    dat2 <- modeldata_filtered_a1[paste(Leader_RECAT, Follow_RECAT) %in% unique_pairs]
    dat2$vref <- dat2$a1 - ifelse(is.na(dat2$landing_adjustment), 0, dat2$landing_adjustment)
  
    fwrite(all_params, file.path(out_dir, "Parameter_Summary_Overall.csv"))
  
    N <- dat[Type == "a1"]$N %>% ifelse(length(.) > 0, ., NA)
    a1 <- dat[Type == "a1"]$median %>% ifelse(length(.) > 0, ., NA)
    a2 <- dat[Type == "a2"]$median %>% ifelse(length(.) > 0, ., NA)
    b <- dat[Type == "b"]$median %>% ifelse(length(.) > 0, ., NA)
    n1 <- dat[Type == "n1"]$median %>% ifelse(length(.) > 0, ., NA)
    n2 <- dat[Type == "n2"]$median %>% ifelse(length(.) > 0, ., NA)
    d <- ifelse(b - a2 > 0 & n2 != n1, (b-a2)/(n2-n1), dat[Type == "d"]$median) %>% ifelse(length(.) > 0, ., NA)
    d2 <- dat[Type == "d2"]$median %>% ifelse(length(.) > 0, ., NA)
  
    # Generate normal distribution using mean and sd of vref
    # vref_dist <- rnorm(1e6, mean = mean(dat2$vref), sd = sd(dat2$vref))
  
    if (length(dat2$vref) > 1 & N >= observation_threshold) {
      dat_density <- density(dat2$vref %>% .[!is.na(.)], n = max(512, length(dat2$vref %>% .[!is.na(.)])))
    } else {
      stop("Not enough aircrafts to generate vref distribution")
    }
  
  
    if (nrow(dat2) >= empirical_threshold) {
      message("Calibrating using empirical distribution.")
      vref_selection <- "Empirical"
      vref <- ifelse(
        quantile(dat2$vref, 0.01) + 10 <= median(dat2$vref),
        quantile(dat2$vref, 0.01) + 10,
        median(dat2$vref))
      pcile <- quantile(dat2$vref, 0.01)
    } else {
      message("Calibrating using fitted distribution.")
      vref_selection <- "Fitted"
      vref <- ifelse(
        quantile(dat_density, 0.01) > 0 & quantile(dat_density, 0.01) + 10 <= quantile(dat_density, 0.5),
        quantile(dat_density, 0.01) + 10,
        quantile(dat_density, 0.5))
      pcile <- quantile(dat_density, 0.01)
    }
  
  
    png(filename = file.path(out_dir, "Vref Distribution Overall.png"), width = 900, height = 600)
    hist(
      dat2$vref,
      main="Vref Distribution",
      xlab="Vref (kts)",
      breaks = seq(floor(min(dat2$vref)) -5, ceiling(max(dat2$vref)) +5, 1),
      prob = T,
      right = F
    )
    lines(dat_density, col = "blue")
    abline(v = vref, col = "red", lty = 2)
    abline(v = pcile, col = "red", lty = 2)
    dev.off()
  
    all_params_req <- data.table(
      N = N %>% ifelse(length(.) > 0, ., NA),
      a1 = a1 %>% ifelse(length(.) > 0, ., NA),
      a2 = a2 %>% ifelse(length(.) > 0, ., NA),
      b = b %>% ifelse(length(.) > 0, ., NA),
      n1 = n1 %>% ifelse(length(.) > 0, ., NA),
      n2 = n2 %>% ifelse(length(.) > 0, ., NA),
      vref = vref %>% ifelse(length(.) > 0, ., NA),
      vref_selection = vref_selection
    )
  
    dbs_adaptation <- list()
  
    for (i in 1:length(unique_DBS_distance)) {
  
      #predefined variables
      dbs_adaptation[[i]] <- dbs_adaptation_input_table
  
      #non predefined parameters
      dbs_adaptation[[i]]$DBS_Distance <- unique_DBS_distance[i]
      dbs_adaptation[[i]]$Min_Safe_Landing_Speed_Lead <- round(vref, 1)
      dbs_adaptation[[i]]$Min_Safe_Landing_Speed_Follower <- max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T)
      dbs_adaptation[[i]]$Final_Deceleration_Lead <- round(d, 1)
      dbs_adaptation[[i]]$Final_Deceleration_Follower <- round(d, 1)
      # dbs_adaptation[[i]]$Initial_Deceleration_Lead <- d2
      # dbs_adaptation[[i]]$Initial_Deceleration_Follower <- d2
    }
  
    # for (i in seq(3, 8, 1)) {
    #
    #   #predefined variables
    #   dbs_adaptation[[i]] <- dbs_adaptation_input_table
    #
    #   #non predefined parameters
    #   dbs_adaptation[[i]]$DBS_Distance <- i
    #   dbs_adaptation[[i]]$Min_Safe_Landing_Speed_Lead <- round(vref, 1)
    #   dbs_adaptation[[i]]$Min_Safe_Landing_Speed_Follower <- max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T)
    #   dbs_adaptation[[i]]$Final_Deceleration_Lead <- round(d, 1)
    #   dbs_adaptation[[i]]$Final_Deceleration_Follower <- round(d, 1)
    #   dbs_adaptation[[i]]$Initial_Deceleration_Lead <- d2
    #   dbs_adaptation[[i]]$Initial_Deceleration_Follower <- d2
    # }
  
    dbs_adaptation <- rbindlist(dbs_adaptation)
  
  
  
    # dbs_adaptation <- data.table(
    #   DBS_Distance = seq(3, 8, 1),
    #   Compression_Commencement_Threshold = 4,
    #   Landing_Stabilisation_Speed_Type_Lead = 0,
    #   Landing_Stabilisation_Speed_Type_Follower = 0,
    #   Min_Safe_Landing_Speed_Lead = round(vref, 1),
    #   Min_Safe_Landing_Speed_Follower = max(round(vref + 5, 1), round(median(dat2$vref), 1), na.rm = T),
    #   Apply_Gusting_Lead = 1,
    #   Apply_Gusting_Follower = 1,
    #   Local_Stabilisation_Distance_Lead = 4,
    #   Local_Stabilisation_Distance_Follower = 4,
    #   Steady_Procedural_Speed_Lead = 160,
    #   Steady_Procedural_Speed_Follower = 162,
    #   Final_Deceleration_Lead = round(d, 1),
    #   Final_Deceleration_Follower = round(d, 1),
    #   End_Initial_Deceleration_Distance_Lead = 12,
    #   End_Initial_Deceleration_Distance_Follower = 12,
    #   Initial_Procedural_Speed_Lead = 160,
    #   Initial_Procedural_Speed_Follower = 162,
    #   Initial_Deceleration_Lead = d2,
    #   Initial_Deceleration_Follower = d2
    # )
  
    # png(filename = file.path(out_dir, "Surface Headwind vs a1.png"), width = 900, height = 600)
    # print({
    #   ggplot(data = dat2, aes(x = Surface_Headwind, y = a1)) +
    #     geom_point() +
    #     geom_smooth(method = "lm", se = F) +
    #     scale_y_continuous(expand = c(0, 0.5)) +
    #     labs(title = NA, x = "Surface Headwind (kts)", y = "Landing Speed / Fitted parameter a1 (kts)") +
    #     theme_classic()
    # })
    # dev.off()
  
    fwrite(all_params_req, file.path(out_dir, "Parameters_Overall.csv"))
  
    fwrite(dbs_adaptation, file.path(out_dir, paste0("Populate_tbl_ORD_DBS_Adaptation_", Airport_Code, ".csv")))
  
  }
}

# ----------------------------------------------------------------------- #
# Aircraft Type Parameter Summary & Adaptation PWS ------------------------
# ----------------------------------------------------------------------- #

if (Operation == 'PWS') {
  
  
  
  modeldata_filtered_a1 <- modeldata_filtered_a1 %>% mutate(per_flight_vref = a1 - ifelse(is.na(landing_adjustment), 0, landing_adjustment)) %>%
                                                     select(-wake) %>%
                                                     left_join(dbGetQuery(con, "SELECT Aircraft_Type, Wake FROM tbl_Aircraft_Type_To_Wake"), by = c("Follower_Aircraft_Type" = "Aircraft_Type"))
  
  
  
  aircraft_adaptation_int <- modeldata_filtered_a1 %>% group_by(Follower_Aircraft_Type) %>%
    summarise(N = n(),
              a1 = median(a1, na.rm = T),
              a2 = median(a2, na.rm = T),
              b = median(b, na.rm = T),
              n1 = median(n1, na.rm = T),
              n2 = median(n2, na.rm = T),
              wake_decel = median(d, na.rm = T),
              d2 = median(d2, na.rm = T),
              vref_med = median(per_flight_vref, na.rm = T),
              vref_percentile = as.numeric(quantile(per_flight_vref, 0.01, na.rm = T) + 10)) %>%
    left_join(lss_types, by = c("Follower_Aircraft_Type" = "aircraft_type")) %>%
    rename("Landing_Stabilisation_Speed_Type_Lead" = "landing_stabilisation_speed_type") %>%
    mutate(Landing_Stabilisation_Speed_Type_Follower = Landing_Stabilisation_Speed_Type_Lead,
           Compression_Commencement_Threshold = type_adaptation_input_table$Compression_Commencement_Threshold,
           Min_Safe_Landing_Speed_Lead = pmin(vref_percentile, vref_med),
           Min_Safe_Landing_Speed_Follower = pmax(Min_Safe_Landing_Speed_Lead + 5, vref_med, na.rm = T),
           Min_Safe_Landing_Speed_Buffer_Follower = vref_med - Min_Safe_Landing_Speed_Follower,
           Apply_Gusting_Lead = type_adaptation_input_table$Apply_Gusting_Lead,
           Apply_Gusting_Follower = type_adaptation_input_table$Apply_Gusting_Follower,
           Local_Stabilisation_Distance_Lead = type_adaptation_input_table$Local_Stabilisation_Distance_Lead,
           Local_Stabilisation_Distance_Follower = type_adaptation_input_table$Local_Stabilisation_Distance_Follower,
           Steady_Procedural_Speed_Lead = type_adaptation_input_table$Steady_Procedural_Speed_Lead,
           Steady_Procedural_Speed_Follower = type_adaptation_input_table$Steady_Procedural_Speed_Follower,
           Steady_Procedural_Speed_Buffer_Follower = 0,     #This needs setting, how are we calculating this buffer? difference between median and proc?
           Final_Deceleration_Lead = ifelse(Steady_Procedural_Speed_Lead - a2 > 0 & n2 != n1, (Steady_Procedural_Speed_Lead-a2)/(n2-n1), wake_decel),
           Final_Deceleration_Follower = ifelse(Steady_Procedural_Speed_Follower - a2 > 0 & n2 != n1, (Steady_Procedural_Speed_Follower-a2)/(n2-n1), wake_decel),
           End_Final_Deceleration_Distance_Lead = n2,
           End_Final_Deceleration_Distance_Follower = n2,
           End_Initial_Deceleration_Distance_Lead = type_adaptation_input_table$End_Initial_Deceleration_Distance_Lead,
           End_Initial_Deceleration_Distance_Follower = type_adaptation_input_table$End_Initial_Deceleration_Distance_Follower,
           Initial_Procedural_Speed_Lead = type_adaptation_input_table$Initial_Procedural_Speed_Lead,
           Initial_Procedural_Speed_Follower = type_adaptation_input_table$Initial_Procedural_Speed_Follower,
           Initial_Procedural_Speed_Buffer_Follower = 0,    #This needs setting, how are we calculating this buffer?
           Initial_Deceleration_Lead = type_adaptation_input_table$Initial_Deceleration_Lead,
           Initial_Deceleration_Follower = type_adaptation_input_table$Initial_Deceleration_Follower)
  
  wake_adaptation_int <- modeldata_filtered_a1 %>% group_by(Wake) %>%
    summarise(N = n(),
              a1 = median(a1, na.rm = T),
              a2 = median(a2, na.rm = T),
              b = median(b, na.rm = T),
              n1 = median(n1, na.rm = T),
              n2 = median(n2, na.rm = T),
              wake_decel = median(d, na.rm = T),
              d2 = median(d2, na.rm = T),
              vref_med = median(per_flight_vref, na.rm = T),
              vref_percentile = as.numeric(quantile(per_flight_vref, 0.01, na.rm = T) + 10)) %>%
    mutate(Landing_Stabilisation_Speed_Type_Lead = wake_adaptation_input_table$Landing_Stabilisation_Speed_Type_Lead,
           Landing_Stabilisation_Speed_Type_Follower = wake_adaptation_input_table$Landing_Stabilisation_Speed_Type_Follower,
           Compression_Commencement_Threshold = wake_adaptation_input_table$Compression_Commencement_Threshold,
           Min_Safe_Landing_Speed_Lead = pmin(vref_percentile, vref_med),
           Min_Safe_Landing_Speed_Follower = pmax(Min_Safe_Landing_Speed_Lead + 5, vref_med, na.rm = T),
           Min_Safe_Landing_Speed_Buffer_Follower = vref_med - Min_Safe_Landing_Speed_Follower,
           Apply_Gusting_Lead = wake_adaptation_input_table$Apply_Gusting_Lead,
           Apply_Gusting_Follower = wake_adaptation_input_table$Apply_Gusting_Follower,
           Local_Stabilisation_Distance_Lead = wake_adaptation_input_table$Local_Stabilisation_Distance_Lead,
           Local_Stabilisation_Distance_Follower = wake_adaptation_input_table$Local_Stabilisation_Distance_Follower,
           Steady_Procedural_Speed_Lead = wake_adaptation_input_table$Steady_Procedural_Speed_Lead,
           Steady_Procedural_Speed_Follower = wake_adaptation_input_table$Steady_Procedural_Speed_Follower,
           Steady_Procedural_Speed_Buffer_Follower = 0,     #This needs setting, how are we calculating this buffer? difference between median and proc?
           Final_Deceleration_Lead = ifelse(Steady_Procedural_Speed_Lead - a2 > 0 & n2 != n1, (Steady_Procedural_Speed_Lead-a2)/(n2-n1), wake_decel),
           Final_Deceleration_Follower = ifelse(Steady_Procedural_Speed_Follower - a2 > 0 & n2 != n1, (Steady_Procedural_Speed_Follower-a2)/(n2-n1), wake_decel),
           End_Final_Deceleration_Distance_Lead = n2,
           End_Final_Deceleration_Distance_Follower = n2,
           End_Initial_Deceleration_Distance_Lead = wake_adaptation_input_table$End_Initial_Deceleration_Distance_Lead,
           End_Initial_Deceleration_Distance_Follower = wake_adaptation_input_table$End_Initial_Deceleration_Distance_Follower,
           Initial_Procedural_Speed_Lead = wake_adaptation_input_table$Initial_Procedural_Speed_Lead,
           Initial_Procedural_Speed_Follower = wake_adaptation_input_table$Initial_Procedural_Speed_Follower,
           Initial_Procedural_Speed_Buffer_Follower = 0,    #This needs setting, how are we calculating this buffer?
           Initial_Deceleration_Lead = wake_adaptation_input_table$Initial_Deceleration_Lead,
           Initial_Deceleration_Follower = wake_adaptation_input_table$Initial_Deceleration_Follower) %>%
    rename(Wake_Cat = Wake)
  
  
  
  wake_dbs_lookup <- dbGetQuery(con, "EXEC usp_GI_Get_Reference_Recat_Separation_Dist_Data") %>% as.data.table()
  setnames(wake_dbs_lookup, "Reference_Wake_Separation_Distance", "DBS_Distance")
  
  #Taking historical formatting
  wake_pairs <- wake_dbs_lookup
  
  #assigning the counts from GWCS script output
  wake_pairs$Count <- Wake_Pair_Counts$n
  
  fwrite(wake_pairs, file.path(out_dir, paste0("DBS_Distance_Wake_Pairs_Count.csv")))
  
  
  
  #Creating a vector to represent unique entries in the dbs table
  
  unique_DBS_distance <- wake_pairs %>% select(DBS_Distance) %>% distinct()
  unique_DBS_distance <- sort(unique_DBS_distance$DBS_Distance)
  
  dbs_names <- c("DBS_Distance",
                 "Compression_Commencement_Threshold",
                 "Landing_Stabilisation_Speed_Type_Lead",
                 "Landing_Stabilisation_Speed_Type_Follower",
                 "Min_Safe_Landing_Speed_Lead",
                 "Min_Safe_Landing_Speed_Follower",
                 "Min_Safe_Landing_Speed_Buffer_Follower",
                 "Apply_Gusting_Lead",
                 "Apply_Gusting_Follower",
                 "Local_Stabilisation_Distance_Lead",
                 "Local_Stabilisation_Distance_Follower",
                 "Steady_Procedural_Speed_Lead",
                 "Steady_Procedural_Speed_Follower",
                 "Steady_Procedural_Speed_Buffer_Follower",
                 "Final_Deceleration_Lead",
                 "Final_Deceleration_Follower",
                 "End_Final_Deceleration_Distance_Lead",
                 "End_Final_Deceleration_Distance_Follower",
                 "End_Initial_Deceleration_Distance_Lead",
                 "End_Initial_Deceleration_Distance_Follower",
                 "Initial_Procedural_Speed_Lead",
                 "Initial_Procedural_Speed_Follower",
                 "Initial_Procedural_Speed_Buffer_Follower",
                 "Initial_Deceleration_Lead",
                 "Initial_Deceleration_Follower")
  
  dbs_out <- data.frame(matrix(NA, nrow = length(unique_DBS_distance), ncol = length(dbs_names))) 
  names(dbs_out) <- dbs_names
  
  for (i in 1:length(unique_DBS_distance)) {
    
    wake_pairs_leader <- wake_pairs[DBS_Distance == unique_DBS_distance[i], .(Count = sum(Count)), by = Leader_WTC]
    
    dbs_lead <- right_join(wake_adaptation_int, wake_pairs_leader, by = c("Wake_Cat" = "Leader_WTC"))
    
    Min_Safe_Landing_Speed_Lead_i <- sum(dbs_lead$Min_Safe_Landing_Speed_Lead * dbs_lead$Count) / sum(dbs_lead$Count)
    Final_Deceleration_Lead_i <- sum(dbs_lead$Final_Deceleration_Lead * dbs_lead$Count) / sum(dbs_lead$Count)
    Initial_Deceleration_Lead_i <- sum(dbs_lead$Initial_Deceleration_Lead * dbs_lead$Count) / sum(dbs_lead$Count)
    End_Final_Deceleration_Distance_Lead_i <- sum(dbs_lead$End_Final_Deceleration_Distance_Lead * dbs_lead$Count) / sum(dbs_lead$Count)
    
    wake_pairs_follower <- wake_pairs[DBS_Distance == unique_DBS_distance[i], .(Count = sum(Count)), by = Follower_WTC]
    
    dbs_foll <- right_join(wake_adaptation_int, wake_pairs_follower, by = c("Wake_Cat" = "Follower_WTC"))
    
    Min_Safe_Landing_Speed_Follower_i <- sum(dbs_foll$Min_Safe_Landing_Speed_Follower * dbs_foll$Count) / sum(dbs_foll$Count)
    Min_Safe_Landing_Speed_Buffer_Follower_i <- sum(dbs_foll$Min_Safe_Landing_Speed_Buffer_Follower * dbs_foll$Count) / sum(dbs_foll$Count)
    Initial_Procedural_Speed_Buffer_Follower_i <- sum(dbs_foll$Initial_Procedural_Speed_Buffer_Follower * dbs_foll$Count) / sum(dbs_foll$Count)
    Final_Deceleration_Foll_i <- sum(dbs_foll$Final_Deceleration_Follower * dbs_foll$Count) / sum(dbs_foll$Count)
    Initial_Deceleration_Foll_i <- sum(dbs_foll$Initial_Deceleration_Follower * dbs_foll$Count) / sum(dbs_foll$Count)
    End_Final_Deceleration_Distance_Foll_i <- sum(dbs_foll$End_Final_Deceleration_Distance_Follower * dbs_foll$Count) / sum(dbs_foll$Count)
    Steady_Procedural_Speed_Buffer_Foll_i <- sum(dbs_foll$Steady_Procedural_Speed_Buffer_Follower * dbs_foll$Count) / sum(dbs_foll$Count)
    
    dbs_out$DBS_Distance[i] <- unique_DBS_distance[i]
    dbs_out$Compression_Commencement_Threshold[i] <- dbs_adaptation_input_table$Compression_Commencement_Threshold
    dbs_out$Landing_Stabilisation_Speed_Type_Lead[i] <- dbs_adaptation_input_table$Landing_Stabilisation_Speed_Type_Lead
    dbs_out$Landing_Stabilisation_Speed_Type_Follower[i] <- dbs_adaptation_input_table$Landing_Stabilisation_Speed_Type_Follower
    dbs_out$Min_Safe_Landing_Speed_Lead[i] <- Min_Safe_Landing_Speed_Lead_i
    dbs_out$Min_Safe_Landing_Speed_Follower[i] <- Min_Safe_Landing_Speed_Follower_i
    dbs_out$Min_Safe_Landing_Speed_Buffer_Follower[i] <- Min_Safe_Landing_Speed_Buffer_Follower_i
    dbs_out$Apply_Gusting_Lead[i] <- dbs_adaptation_input_table$Apply_Gusting_Lead
    dbs_out$Apply_Gusting_Follower[i] <- dbs_adaptation_input_table$Apply_Gusting_Follower
    dbs_out$Local_Stabilisation_Distance_Lead[i] <- dbs_adaptation_input_table$Local_Stabilisation_Distance_Lead
    dbs_out$Local_Stabilisation_Distance_Follower[i] <- dbs_adaptation_input_table$Local_Stabilisation_Distance_Follower
    dbs_out$Steady_Procedural_Speed_Lead[i] <- dbs_adaptation_input_table$Steady_Procedural_Speed_Lead
    dbs_out$Steady_Procedural_Speed_Follower[i] <- dbs_adaptation_input_table$Steady_Procedural_Speed_Follower
    dbs_out$Steady_Procedural_Speed_Buffer_Follower[i] <- Steady_Procedural_Speed_Buffer_Foll_i
    dbs_out$Final_Deceleration_Lead[i] <- Final_Deceleration_Lead_i
    dbs_out$Final_Deceleration_Follower[i] <- Final_Deceleration_Foll_i
    dbs_out$End_Final_Deceleration_Distance_Lead[i] <- End_Final_Deceleration_Distance_Lead_i
    dbs_out$End_Final_Deceleration_Distance_Follower[i] <- End_Final_Deceleration_Distance_Foll_i
    dbs_out$End_Initial_Deceleration_Distance_Lead[i] <- dbs_adaptation_input_table$End_Initial_Deceleration_Distance_Lead
    dbs_out$End_Initial_Deceleration_Distance_Follower[i] <- dbs_adaptation_input_table$End_Initial_Deceleration_Distance_Follower
    dbs_out$Initial_Procedural_Speed_Lead[i] <- dbs_adaptation_input_table$Initial_Procedural_Speed_Lead
    dbs_out$Initial_Procedural_Speed_Follower[i] <- dbs_adaptation_input_table$Initial_Procedural_Speed_Follower
    dbs_out$Initial_Procedural_Speed_Buffer_Follower[i] <- Initial_Procedural_Speed_Buffer_Follower_i
    dbs_out$Initial_Deceleration_Lead[i] <- dbs_adaptation_input_table$Initial_Deceleration_Lead
    dbs_out$Initial_Deceleration_Follower[i] <- dbs_adaptation_input_table$Initial_Deceleration_Follower
  }
  
  
  for (i in 1:nrow(aircraft_adaptation_int)) {
    if (aircraft_adaptation_int$N[i] >= observation_threshold) {
      
      dat2 <- modeldata_filtered_a1[Follower_Aircraft_Type == aircraft_adaptation_int$Follower_Aircraft_Type[i]]
      dat_density <- density(dat2$per_flight_vref %>% .[!is.na(.)], n = max(512, length(dat2$per_flight_vref %>% .[!is.na(.)])))
      
      #Vref distribution plots
      
      png(filename = file.path(out_plot1, paste0(aircraft_adaptation_int$Follower_Aircraft_Type[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
      hist(
        dat2$per_flight_vref,
        main=paste0("Vref Distribution - ", aircraft_adaptation_int$Follower_Aircraft_Type[i]),
        xlab="Vref (kts)",
        breaks = seq(floor(min(dat2$per_flight_vref)) -5, ceiling(max(dat2$per_flight_vref)) +5, 1),
        prob = T,
        right = F
      )
      lines(dat_density, col = "blue")
      abline(v = aircraft_adaptation_int$Min_Safe_Landing_Speed_Lead[i], col = "red", lty = 2)
      abline(v = ifelse(aircraft_adaptation_int$N >= empirical_threshold, quantile(dat2$per_flight_vref, 0.01), quantile(dat_density, 0.01)), col = "red", lty = 2)
      dev.off()
      
      #Vref against surface headwind
      
      png(filename = file.path(out_plot2, paste0(aircraft_adaptation_int$Follower_Aircraft_Type[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
      print({
        ggplot(data = dat2, aes(x = Surface_Headwind, y = a1)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          scale_y_continuous(expand = c(0, 0.5)) +
          labs(title = aircraft_adaptation_int$Follower_Aircraft_Type[i], x = "Surface Headwind (kts)", y = "Landing Speed / Fitted parameter a1 (kts)") +
          theme_classic()
      })
      dev.off()
      
      sink(file.path(out_plot2, "summary.txt"), append = T)
      cat(aircraft_adaptation_int$Follower_Aircraft_Type[i], "\n")
      print(summary(lm(a1 ~ Surface_Headwind, data = dat2)))
      cat("# ----------------------------------------------------------------------- #\n\n")
      sink()
      
    }
  }
  
  for (i in 1:nrow(wake_adaptation_int)) {
    if (wake_adaptation_int$N[i] >= observation_threshold) {
      
      dat2 <- modeldata_filtered_a1[Wake == wake_adaptation_int$Wake_Cat[i]]
      dat_density <- density(dat2$per_flight_vref %>% .[!is.na(.)], n = max(512, length(dat2$per_flight_vref %>% .[!is.na(.)])))
      
      #Plot for Vref Distributions
      
      png(filename = file.path(out_plot3, paste0(wake_adaptation_int$Wake_Cat[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
      hist(
        dat2$per_flight_vref,
        main=paste0("Vref Distribution - ", wake_adaptation_int$Wake_Cat[i]),
        xlab="Vref (kts)",
        breaks = seq(floor(min(dat2$per_flight_vref)) -5, ceiling(max(dat2$per_flight_vref)) +5, 1),
        prob = T,
        right = F
      )
      lines(dat_density, col = "blue")
      abline(v = wake_adaptation_int$Min_Safe_Landing_Speed_Lead[i], col = "red", lty = 2)
      abline(v = ifelse(wake_adaptation_int$N >= empirical_threshold, quantile(dat2$per_flight_vref, 0.01), quantile(dat_density, 0.01)), col = "red", lty = 2)
      dev.off()
      
      #Vref by surface headwind
      
      png(filename = file.path(out_plot4, paste0(wake_adaptation_int$Wake_Cat[i], ".png")) %>% gsub("%", "%%", .), width = 900, height = 600)
      print({
        ggplot(data = dat2, aes(x = Surface_Headwind, y = a1)) +
          geom_point() +
          geom_smooth(method = "lm", se = F) +
          scale_y_continuous(expand = c(0, 0.5)) +
          labs(title = wake_adaptation_int$Wake_Cat[i], x = "Surface Headwind (kts)", y = "Landing Speed / Fitted parameter a1 (kts)") +
          theme_classic()
      })
      dev.off()
      
      sink(file.path(out_plot4, "summary.txt"), append = T)
      cat(wake_adaptation_int$Wake_Cat[i], "\n")
      print(summary(lm(a1 ~ Surface_Headwind, data = dat2)))
      cat("# ----------------------------------------------------------------------- #\n\n")
      sink()
    }
  }
  
  
  type_out <- aircraft_adaptation_int %>% filter(N >= empirical_threshold | 
                                                   (Follower_Aircraft_Type %in% additional_aircraft_to_output & N >= observation_threshold)) %>%
    select(-c(N, a1, b, a2, n1, n2, wake_decel, d2, vref_med, vref_percentile)) %>%
    rename(Aircraft_Type = Follower_Aircraft_Type)
  
  wake_out <- wake_adaptation_int %>% select(-c(N, a1, b, a2, n1, n2, wake_decel, d2, vref_med, vref_percentile))
  
  fwrite(type_out, file.path(out_dir, paste0("Populate_tbl_ORD_Aircraft_Adaptation_", Airport_Code, ".csv")))
  fwrite(wake_out, file.path(out_dir, paste0("Populate_tbl_ORD_Wake_Adaptation_", Airport_Code, ".csv")))
  fwrite(dbs_out, file.path(out_dir, paste0("Populate_tbl_ORD_DBS_Adaptation_", Airport_Code, ".csv")))
  
}
