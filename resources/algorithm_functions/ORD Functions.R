"%!in%" <- function(x,y) !("%in%"(x,y))

calc_landing_adjustment <- function(landing_type, headwind) {
  return(
    if (landing_type %in% c(0, 10, 11, 12)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(1, 2, 3, 4, 5)) {
      sapply(headwind / 3, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 15, 15, hw_adj)))
    } else if (landing_type %in% c(6)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 0, 0, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(7)) {
      sapply(headwind, function(hw_adj) 10)
    } else if (landing_type %in% c(8)) {
      sapply(headwind, function(hw_adj) 0)
    } else if (landing_type %in% c(9)) {
      sapply(headwind, function(hw_adj) ifelse(hw_adj > 20, 15, ifelse(hw_adj > 10, 10, 5)))
    }
  )
}

airspeed_model_break <- function(x, a, a1, b, n1, n2) {
  if (n1 < 1) n1 <- 1
  if (n2 < n1 | abs(n2 - n1) < 0.1) n2 <- n1
  return(
    if (x < 1) {
      a
    } else if (x >= 1 & x < n1) {
      a1
    } else if (x >= n1 & x <= n2 & abs(n2 - n1) < 0.1) {
      a1
    } else if (x >= n1 & x <= n2) {
      a1 + (x - n1) * (b - a1) / (n2 - n1)
    } else {
      b
    }
  )
}

airspeed_model_vector_break <- function(x, a, a1, b, n1, n2){
  sapply(x, airspeed_model_break, a = a, a1 = a1, b = b, n1 = n1, n2 = n2, simplify = T)
}

airspeed_model_break_simplified <- function(x, a, a1) {
  return(
    if (x < 1) {
      a
    } else {
      a1
    }
  )
}

airspeed_model_vector_break_simplified <- function(x, a, a1) {
  sapply(x, airspeed_model_break_simplified, a = a, a1 = a1, simplify = T)
}

parameter_summary <- function(dat) {
  cal_param <- names(dat)[2]
  names(dat)[2] <- "V2"
  dat$V2 <- as.numeric(dat$V2)
  x <- ddply(
    dat, names(dat)[1], summarise,
    N = length(V2),
    mean = mean(V2, na.rm = T),
    median = median(V2, na.rm = T),
    sd = sd(V2, na.rm = T),
    p5 = quantile(V2, 0.05, na.rm = T),
    p10 = quantile(V2, 0.1, na.rm = T)
  )
  x$Type <- cal_param
  return(x)
}

readjust_lss_type <- function(d, new_lss_type) {
  new_d <- d
  new_d$lss_type <- new_lss_type
  new_d$landing_adjustment = calc_landing_adjustment(new_lss_type, new_d$Surface_Headwind)
  return(new_d)
}

dodgy_colour_function <- function(aircraft_type) {
  x_char <- unlist(strsplit(as.character(aircraft_type), split="")) %>% ifelse(grepl("^[A-Z]$", .), match(., LETTERS), .) %>% as.numeric()
  x_tabl <- x_char %>% data.table(
    V1 = .[c(T, F, F, F)],
    V2 = .[c(F, T, F, F)],
    V3 = .[c(F, F, T, F)],
    V4 = .[c(F, F, F, T)],
    keep.rownames = F
  )
  x_prod <- paste0(x_tabl$V1, x_tabl$V2, x_tabl$V3, x_tabl$V4) %>% as.numeric()
  x_conv <- floor((x_prod - mean(x_prod)) / sd(x_prod) * 256^3 - 1)
  x_chex <- paste0("#", as.hexmode(x_conv))
  return(x_chex)
}

createBook <- function() {
  createWorkbook(type="xlsx")
}

writeCell <- function(worksheet, row_index, col_index, value) {
  r <- createCell(createRow(worksheet, rowIndex = row_index), colIndex = col_index)
  setCellValue(r[[1,1]], value)
}

writeRow <- function(worksheet, row_index, col_indices, values) {
  r <- createCell(createRow(worksheet, rowIndex = row_index), colIndex = 1:max(col_indices))
  for (i in 1:length(col_indices)) {
    setCellValue(r[[1,col_indices[i]]], values[i])
  }
}

writeTable <- function(worksheet, row_index, col_index, data_table) {
  addDataFrame(data_table, worksheet, startRow = row_index, startColumn = col_index, row.names = F)
}

writeImage <- function(worksheet, row_index, col_index, img) {
  addPicture(img, worksheet, scale = 1, startRow = row_index, startColumn = col_index)
}

saveBook <- function(workbook, filename) {
  saveWorkbook(workbook, filename)
}

# Find area under density curve using trapezoidal rule
density_area <- function(den, min, max, min_include = T, max_include = T) {
  den_x_range <- if (min_include & max_include) {
    den$x[den$x >= min & den$x <= max]
  } else if (min_include & !max_include) {
    den$x[den$x >= min & den$x < max]
  } else if (!min_include & max_include) {
    den$x[den$x > min & den$x <= max]
  } else if (!min_include & !max_include) {
    den$x[den$x > min & den$x < max]
  }
  den_y_range <- den$y[which(den$x %in% den_x_range)]
  if (length(den_y_range) > 1) {
    trapezoidal_areas <- sapply(2:length(den_y_range), function(i) (den_x_range[i] - den_x_range[i-1]) * (den_y_range[i] + den_y_range[i-1]) / 2)
    total_area <- sum(trapezoidal_areas)
    return(total_area)
  } else {
    return(0)
  }
}

adjust_Vref <- function(d1, d2, m2, v2, t, vref) {
  
  a <- (d1 - m2)/v2 - t
  b <- d1 + m2 - d2 -v2*t
  c <- v2*d2
  
  a <- as.complex(a)
  quad <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
            (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  
  adjusted_vref <- NA
  
  if (quad[1] == quad[2] & Im(quad[1]) == 0) {adjusted_vref <- Re(quad[1])}
  
  if (Im(quad[1]) == 0 & Im(quad[2]) != 0) {adjusted_vref <- Re(quad[1])}
  if (Im(quad[1]) != 0 & Im(quad[2]) == 0) {adjusted_vref <- Re(quad[2])}
  
  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) > 0 & Re(quad[2]) < 0)  {adjusted_vref <- Re(quad[1])}
  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) < 0 & Re(quad[2]) > 0)  {adjusted_vref <- Re(quad[2])}
  
  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) > 0 & Re(quad[2]) > 0 & abs(Re(quad[1]) - vref_lead) < abs(Re(quad[2]) - vref))  {adjusted_vref <- Re(quad[1])}
  if (Im(quad[1]) == 0 & Im(quad[2]) == 0 & Re(quad[1]) > 0 & Re(quad[2]) > 0 & abs(Re(quad[1]) - vref_lead) > abs(Re(quad[2]) - vref))  {adjusted_vref <- Re(quad[2])}
  
  return(adjusted_vref)
  
}



##### ORD PROXY FUNCTIONS!
Get_Average_Observed_Mode_S_Parameters <- function(LPR, Radar, Prefix, LorF, TimeorRange, Start_Var, End_Var){
  
  # Get Variable Names
  FPID <- paste0(LorF, "_Flight_Plan_ID")
  
  # Get Relevant Pair Data
  Pair_Data <- select(LPR, !!sym(FPID), !!sym(Start_Var), !!sym(End_Var)) %>% rename("Flight_Plan_ID" := !!sym(FPID))
  
  # Join on the distances by Flight Plan ID
  Radar <- left_join(Radar, Pair_Data, by = c("Flight_Plan_ID"))
  
  # Filter for RTT within the Distance Bounds
  if (TimeorRange == "Range"){Radar <- rename(Radar, "End_Distance" := !!sym(End_Var), "Start_Distance" := !!sym(Start_Var))
  Radar <- filter(Radar, Range_To_Threshold >= End_Distance & Range_To_Threshold <= Start_Distance)}
  
  # Filter for Track_Time within the Time Bounds
  if (TimeorRange == "Time"){Radar <- rename(Radar, "End_Time" := !!sym(End_Var), "Start_Time" := !!sym(Start_Var))
  Radar <- filter(Radar, Track_Time <= End_Time & Track_Time >= Start_Time)}
  
  # Filter to remove NA Wind Effect/IAS Values
  Radar <- filter(Radar, !is.na(Mode_S_IAS) & !is.na(Wind_Effect_IAS))
  
  # Order by Flight Plan ID & Track Time
  Radar <- Order_Radar(Radar)
  
  # Get a Sequence Number
  Radar <- group_by(Radar, Flight_Plan_ID) %>% mutate(Sequence_Number = row_number()) %>% ungroup()
  
  # Take Required Fields from Radar
  Radar2 <- select(Radar, Flight_Plan_ID, Sequence_Number, Track_Time, Mode_S_IAS, Wind_Effect_IAS)
  
  # Change Sequence number to next number. Change names of parameters.
  Radar2 <- mutate(Radar2, Sequence_Number = Sequence_Number + 1) %>%
    rename(Previous_Track_Time = Track_Time, Previous_Mode_S_IAS = Mode_S_IAS, Previous_Wind_Effect_IAS = Wind_Effect_IAS)
  
  # Join on the Previous Parameters
  Radar <- left_join(Radar, Radar2, by = c("Flight_Plan_ID", "Sequence_Number"))
  
  # Remove Radar2
  rm(Radar2)
  
  # Get the Delta beween Track_Time and Previous_Track_Time
  Radar <- mutate(Radar, Track_Time_Delta = Track_Time - Previous_Track_Time)
  
  # Get each Observation's Contribution to the Trapezium sum: IAS
  Radar <- mutate(Radar, Observed_Mean_IAS = Track_Time_Delta * (Mode_S_IAS + Previous_Mode_S_IAS) / 2)
  
  # Get each Observation's Contribution to the Trapezium sum: Wind Effect
  Radar <- mutate(Radar, Observed_Mean_Wind_Effect = Track_Time_Delta * (Wind_Effect_IAS + Previous_Wind_Effect_IAS) / 2)
  
  # Sum Track Time Delta, Observed Mean IAS/Wind Effect by Flight Plan ID
  Radar <- group_by(Radar, Flight_Plan_ID) %>% summarise(Total_Track_Time_Delta = sum(Track_Time_Delta, na.rm=T),
                                                         Observed_Mean_IAS = sum(Observed_Mean_IAS, na.rm=T),
                                                         Observed_Mean_Wind_Effect = sum(Observed_Mean_Wind_Effect, na.rm=T)) %>% ungroup()
  
  # Divide the Observed sums by the Track Time delta to get the Trapezium rule average
  Radar <- mutate(Radar,
                  Observed_Mean_IAS = Observed_Mean_IAS / Total_Track_Time_Delta,
                  Observed_Mean_Wind_Effect = Observed_Mean_Wind_Effect / Total_Track_Time_Delta) %>%
    select(-Total_Track_Time_Delta)
  
  # Get Variable Names
  IAS_Var <- paste0("Observed_", LorF, "_", Prefix, "_IAS")
  WE_Var <- paste0("Observed_", LorF, "_", Prefix, "_Wind_Effect")
  
  # Rename Appropriately
  Radar <- rename(Radar, 
                  !!sym(IAS_Var) := "Observed_Mean_IAS",
                  !!sym(WE_Var) := "Observed_Mean_Wind_Effect")
  
  # Join on to Landing Pair Reference
  LPR <- left_join(LPR, Radar, by = setNames("Flight_Plan_ID", FPID))
  
  # Return the Observed parameters.
  return(LPR)
  
}

Order_Radar <- function(Radar){
  Radar <- Radar[order(Radar$Flight_Plan_ID, Radar$Track_Time),]
  return(Radar)
}


RecalculateRadarValuesORD <-  function(Radar, RTTPathLegs, WEPathLegs, MaxILSRange){
  
  ## Step 1: Recalculate Range to Threshold with Adaptable Parameters.
  Radar <- Radar %>%
    mutate(ILSRangeFlag = ifelse(Range_To_ILS <= MaxILSRange, T, F),
           PLFlag = ifelse(Path_Leg_Type %in% RTTPathLegs, T, F),
           RTT_New = ifelse(ILSRangeFlag & PLFlag & is.na(Range_To_Threshold), ILS_Locus_RTT, Range_To_Threshold),
           RTTPRoxyFlag = ifelse(!is.na(RTT_New) & is.na(Range_To_Threshold), 1, 0),
           Range_To_Threshold = ifelse(RTTPRoxyFlag == 1, RTT_New, Range_To_Threshold)) %>%
    select(-ILSRangeFlag, -PLFlag, -RTT_New)
  
  ## Filter for Range to Threshold only.
  Radar <- filter(Radar, !is.na(Range_To_Threshold))
  
  ## Convert to SI Units for WE Calculation. (Runway Heading already in SI, Mode S IAS required non-SI after so left out)
  Radar <- Radar %>%
    mutate(Wind_SPD = Wind_SPD * kts_To_mps,
           Wind_HDG = (Wind_HDG * deg_to_rad) + (pi),
           Mode_S_TAS = Mode_S_TAS * kts_To_mps)
  
  ## Recalculation of the Wind Effect.
  Radar <- Radar %>%
    mutate(SWC = (Wind_SPD / Mode_S_TAS) * sin(Wind_HDG - Runway_HDG),
           Aircraft_HDG = Runway_HDG + asin(SWC),
           WE_New = sqrt((Mode_S_TAS^2) + (Wind_SPD^2) - (2*Mode_S_TAS*Wind_SPD) * cos(Aircraft_HDG - Wind_HDG)) - (Mode_S_IAS*kts_To_mps),
           WEProxyFlag = ifelse(WE_New != Wind_Effect_IAS & Path_Leg_Type %in% WEPathLegs, 1, 0),
           Wind_Effect_IAS = ifelse(WEProxyFlag == 1, WE_New/kts_To_mps, Wind_Effect_IAS)) %>%
    select(-SWC, -Aircraft_HDG, -WE_New, -Wind_SPD, -Wind_HDG, -Mode_S_TAS, -Runway_HDG)
  
  return(Radar)
  
}

GenerateProxyWindEffect <- function(Data, Radar, Algo, LorFIn, LorFOut, MaxInsideBuffer, FAF_Distance, RemoveOld){
  
  ## Get Variables
  IASVar <- paste0("Observed_Mean_", LorFOut, "_IAS")
  WEVar <- paste0("Observed_Mean_", LorFOut, "_Wind_Effect")
  
  ## Step 1: Put aside existing values. If a value has already been calculated then it is valid.
  
  if (RemoveOld){
    Data <- Data %>%
      mutate(Follower_Start_RTT = NA,
             Follower_Stop_RTT = NA)
  }
  
  if ("ExistFlag" %in% names(Data)){
    GoodData <- filter(Data, !is.na(!!sym(WEVar)))
    Data <- filter(Data, is.na(!!sym(WEVar)))
  } else {
    GoodData <- filter(Data, !is.na(Follower_Start_RTT) & !is.na(Follower_Stop_RTT)) %>% mutate(ExistFlag = 1)
    Data <- filter(Data, is.na(Follower_Start_RTT) | is.na(Follower_Stop_RTT)) %>% mutate(ExistFlag = 0)
  }
  
  ## Split into trails 
  for (Trail in c("In_Trail", "In_Trail_Non_Sequential", "Not_In_Trail")){
    
    # Filter pairs for Trail Type. (Purely for single ID matching)
    TrailData <- filter(Data, Landing_Pair_Type == Trail)
    
    # Generate the Forecast start and end distance bounds for use in the average WE/Speed calculations
    TrailData <- GetForecastDistances(TrailData, Algo, LorFOut, Delivery_Point)
    
    # Get the Max RTT values from the aircraft, to make sure the aircraft doesn't start too far inside the bound.
    TrailData <- GetMaxRTTs(TrailData, Radar, LorFIn)
    
    # Create a flag to display the above criteria. 
    TrailData <- TrailData %>% mutate(Flag = ifelse(Max_RTT < (Forecast_Start_Distance - MaxInsideBuffer), 1, 0),
                                      Flag = ifelse(is.na(Flag), 1, Flag))
    
    # Rename the Max_RTT Variable just in case we do for both L/F.
    TrailData <- rename(TrailData, !!sym(paste0(LorFIn, "_Max_RTT")) := Max_RTT)
    
    # Recalculate Flag if using Leader - Discount if Leader time is before prediction time. (What time are we using?)
    if (LorFIn == "Leader" & LorFOut == "Follower"){TrailData <- TrailData %>% mutate(Flag = ifelse(!!sym(GetLeaderLatestTimeVar(Algo)) < Prediction_Time, 1, Flag))}
    
    # Split the Data into "Bad" (we can't proxy) and "Good" (we can proxy)
    TrailData <- TrailData %>% mutate(!!sym(IASVar) := NA, !!sym(WEVar) := NA)
    BadTrailData <- filter(TrailData, Flag == 1)
    TrailData <- filter(TrailData, Flag == 0)
    
    # Prepare for the WE/Speed recalculation by removing the existing fields.
    TrailData <- TrailData %>% select(-!!sym(WEVar), -!!sym(IASVar))
    
    # Recalculate the Observed Wind Effect/Speed using the provided parameters.
    TrailData <- Get_Average_Observed_Mode_S_Parameters(TrailData, Radar, Prefix = "Temp", LorF = LorFIn, TimeorRange = "Range", Start_Var = "Forecast_Start_Distance", End_Var = "Forecast_End_Distance") %>%
      rename(!!sym(IASVar) := !!sym(paste0("Observed_", LorFIn, "_Temp_IAS"))) %>%
      rename(!!sym(WEVar) := !!sym(paste0("Observed_", LorFIn, "_Temp_Wind_Effect")))
    
    # Bind all of the "Good" data together and all of the "Bad" data together.
    TrailData <- rbind(TrailData, BadTrailData)
    if (!exists("NewData")){NewData <- TrailData} else {NewData <- rbind(NewData, TrailData)}
    
  }
  
  ## Remove Extra Fields from NewData.
  NewData <- NewData %>% select(-Forecast_Start_Distance, -Forecast_End_Distance, -!!sym(paste0(LorFIn, "_Max_RTT")))
  
  ## Add the extra flags to the original "Good" data.
  GoodData <- mutate(GoodData, Flag = 0) ## Flag of Finally Invalid
  
  ## Bind together all Datasets.
  Data <- rbind(GoodData, NewData)
  
  ## Rename Flags for clarity between Leader/Follower proxies.
  Data <- Data %>%
    rename(!!sym(paste0(LorFIn, "_Invalid_Flag")) := Flag)
  
  return(Data)
  
}

GetForecastDistances <- function(Data, Algo, LorFOut, Delivery_Point){
  
  ### NOTE: CCT SHOULD BE IN WAD VIEW AND NOT HARDCODED HERE.
  CCT <- 10
  
  ## Not generalised for now - just in case method changes. Assumes LorFOut is Follower.
  if (Algo == "ORD" & LorFOut == "Follower"){
    Data <- Data %>%
      mutate(Forecast_Start_Distance = ORD_Compression + ORD_Separation_Distance + FAF_Distance,
             Forecast_End_Distance = ORD_Separation_Distance + Delivery_Point)
  }
  if (Algo == "WAD" & LorFOut == "Follower"){
    Data <- Data %>%
      mutate(Forecast_Start_Distance = WAD_Compression + WAD_Separation_Distance + (Leader_CC_RTT - Leader_FAF_RTT),
             Forecast_End_Distance = WAD_Separation_Distance)
  }
  if (Algo == "Both" & LorFOut == "Follower"){
    Data <- Data %>%
      mutate(Forecast_Start_Distance = WAD_Compression + WAD_Separation_Distance + (Leader_CC_RTT - Leader_FAF_RTT),
             Forecast_End_Distance = ORD_Separation_Distance + Delivery_Point)
  }
  if (Algo == "ORD" & LorFOut == "Leader"){
    Data <- Data %>%
      mutate(Forecast_Start_Distance = FAF_Distance,
             Forecast_End_Distance = Delivery_Point)
  }
  if (Algo == "WAD" & LorFOut == "Leader"){
    Data <- Data %>%
      mutate(Forecast_Start_Distance = Leader_CC_RTT,
             Forecast_End_Distance = Leader_FAF_RTT)
  }
  if (Algo == "Both" & LorFOut == "Leader"){
    Data <- Data %>%
      mutate(Forecast_Start_Distance = Leader_CC_RTT,
             Forecast_End_Distance = Delivery_Point)
  }
  
  return(Data)
}

GetLeaderLatestTimeVar <- function(Algo){
  
  if (Algo %in% c("ORD", "Both")){return("Leader_0DME_Time")}
  if (Algo == "WAD"){return("Leader_FAF_Time")}
  
}

GetMaxRTTs <- function(Data, Radar, LorFIn){
  
  # Get the Variable Names (ID, Max RTT).
  IDVar <- paste0(LorFIn, "_Flight_Plan_ID")
  
  # Get the Maximum RTTs via the Radar.
  MaxRTTs <- group_by(Radar, Flight_Plan_ID) %>%
    summarise(Max_RTT = max(Range_To_Threshold, na.rm=T))
  
  # Join on to the Landing Pair Data.
  Data <- Data %>%
    left_join(MaxRTTs, by = setNames("Flight_Plan_ID", IDVar))
  
  return(Data)
  
}

ORDRealignFlags <- function(Data, LorFOut){
  
  if (LorFOut == "Follower"){
    if ("Leader_Invalid_Flag" %in% names(Data)){
      Data <- Data %>%
        mutate(OriginalFlag = ExistFlag,
               CalcFlagFollower = ifelse(ExistFlag == 0 & Leader_Invalid_Flag == 0 & Follower_Invalid_Flag == 0, 1, 0),
               CalcFlagLeader = ifelse(ExistFlag == 0 & Leader_Invalid_Flag == 0 & Follower_Invalid_Flag == 1, 1, 0)) %>%
        mutate(Calc_Type = ifelse(OriginalFlag == 1, "Original", "Unavailable"),
               Calc_Type = ifelse(CalcFlagLeader == 1, "Leader Proxy", Calc_Type),
               Calc_Type = ifelse(CalcFlagFollower == 1, "Follower Proxy", Calc_Type)) %>%
        select(-ExistFlag, -Leader_Invalid_Flag, -Follower_Invalid_Flag, -OriginalFlag, -CalcFlagLeader, -CalcFlagFollower)
    } else {
      Data <- Data %>%
        mutate(OriginalFlag = ExistFlag,
               CalcFlagFollower = ifelse(ExistFlag == 0 & Follower_Invalid_Flag == 0, 1, 0)) %>%
        mutate(Calc_Type = ifelse(OriginalFlag == 1, "Original", "Unavailable"),
               Calc_Type = ifelse(CalcFlagFollower == 1, "Follower Proxy", Calc_Type)) %>%
        select(-ExistFlag, -Follower_Invalid_Flag, -OriginalFlag, -CalcFlagFollower)
    }
  } else {
    if ("Follower_Invalid_Flag" %in% names(Data)){
      Data <- Data %>%
        mutate(OriginalFlag = ExistFlag,
               CalcFlagFollower = ifelse(ExistFlag == 0 & Leader_Invalid_Flag == 1 & Follower_Invalid_Flag == 0, 1, 0),
               CalcFlagLeader = ifelse(ExistFlag == 0 & Leader_Invalid_Flag == 0 & Follower_Invalid_Flag == 0, 1, 0)) %>%
        mutate(Calc_Type = ifelse(OriginalFlag == 1, "Original", "Unavailable"),
               Calc_Type = ifelse(CalcFlagLeader == 1, "Leader Proxy", Calc_Type),
               Calc_Type = ifelse(CalcFlagFollower == 1, "Follower Proxy", Calc_Type)) %>%
        select(-ExistFlag, -Leader_Invalid_Flag, -Follower_Invalid_Flag, -OriginalFlag, -CalcFlagLeader, -CalcFlagFollower)
    } else {
      Data <- Data %>%
        mutate(OriginalFlag = ExistFlag,
               CalcFlagLeader = ifelse(ExistFlag == 0 & Leader_Invalid_Flag == 0, 1, 0)) %>%
        mutate(Calc_Type = ifelse(OriginalFlag == 1, "Original", "Unavailable"),
               Calc_Type = ifelse(CalcFlagLeader == 1, "Leader Proxy", Calc_Type)) %>%
        select(-ExistFlag, -Leader_Invalid_Flag, -OriginalFlag, -CalcFlagLeader)
    }
  }
  
  # Rename
  Data <- Data %>% rename(!!sym(paste0(LorFOut, "_Calc_Type")) := Calc_Type)

  return(Data)
  
} 

GetORDAnalysisRadarQuery <- function(){
  
  Query <- "SELECT 
  RTPD.Flight_Plan_ID,
  Track_Time,
  Range_To_Threshold,
  ILS_Locus_RTT,
  Range_To_ILS,
  Path_Leg_Type,
  Wind_Effect_IAS,
  Mode_S_IAS,
  Mode_S_TAS,
  Wind_SPD,
  Wind_HDG,
  R2.Heading AS Runway_HDG
  FROM vw_Radar_Track_Point_Derived RTPD
  LEFT JOIN tbl_Flight_Plan FP
  ON RTPD.Flight_Plan_ID = FP.Flight_Plan_ID
  LEFT JOIN tbl_Flight_Plan_Derived FPD
  ON RTPD.Flight_Plan_ID = FPD.Flight_Plan_ID
  LEFT JOIN tbl_Runway R1
  ON R1.Runway_Name = RTPD.Mode_S_Wind_Localiser_Capture
  LEFT JOIN tbl_Runway R2
  ON R2.Runway_Name = FP.Landing_Runway
  WHERE (FP.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
  OR FPD.Landing_Runway = RTPD.Mode_S_Wind_Localiser_Capture
  OR R1.Runway_Group = R2.Runway_Group
  OR RTPD.Mode_S_Wind_Localiser_Capture IS NULL)
  AND Wind_Effect_IAS IS NOT NULL
  ORDER BY Flight_Plan_ID, Track_Time"
  
  return(Query)
  
}

QuickProxyTablePlot <- function(Data, Algo, ReturnWhat, LorFOut){
  
  Table <- Data %>% group_by(!!sym(paste0(LorFOut, "_Calc_Type"))) %>% summarise(Count = n()) %>% ungroup() %>%
    mutate(Percent = round(100*(Count / sum(Count)), 2))
  
  Plot <- ggplot() + geom_col(data = Table, mapping = aes(y = Count, x = !!sym(paste0(LorFOut, "_Calc_Type")))) +
    labs(title = paste0("Calculation Type Counts for Proxy Method (", Algo, ")"), x = "Calculation Type", y = "Total Number")
  print(Plot)
  
  if (ReturnWhat == "Plot"){return(Plot)} else {return(Table)}
  
}


get_time_of_flight <- function(end_initial_decel, n2, n1, steady_procedural_speed, Vtgt){
  
  t <- (end_initial_decel - n2)/steady_procedural_speed +
    2*(n2 - n1)/(steady_procedural_speed + Vtgt) +
    n1/Vtgt
  
  return(t)
  
}


get_decel_dist <- function(end_initial_decel, t, thou, steady_procedural_speed, Vtgt){
  
  #thou = thousand_ft_gate
  m2 <- ((steady_procedural_speed*(t - thou/Vtgt) - end_initial_decel) *
           (steady_procedural_speed + Vtgt) + 
           2 * steady_procedural_speed * thou) / 
           (steady_procedural_speed - Vtgt)
  
  return(m2)
  
}


get_decel <- function(m2, thou, steady_procedural_speed, Vtgt){
  
  d <- (steady_procedural_speed - Vtgt)/(m2 - thou)  
  
  return(d)
  
}


