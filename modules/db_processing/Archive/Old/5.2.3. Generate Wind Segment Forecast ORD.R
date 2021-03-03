# ------------------------------------------------------------------------------------------------------------------------------------------ #
#
# Think IA Validation/Verification Tool 
#
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# 4.1.1 Generate Forecast ORD Segments
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# Summary
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# Version: v0
#
# Authors: George Clark
# 
# Description: Gets the complete forecast of wind segments for each Landing Pair. Currently has a single forecast for both aircraft
#              in the pair. Uses a rolling join to find segments from forecast list within the stale time for each Segment range to threshold
#              then performs TBS/ORD Extrapolation algorithms as required.
#              
#
# Use Guide Section: 
# ------------------------------------------------------------------------------------------------------------------------------------------ #


# Version History
# ------------------------------------------------------------------------------------------------------------------------------------------ # 
#
# v0: 
#     
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------------------------------------ #

# ---------------------------
# 1.6. Functions
# ---------------------------




# Segment Data Preparation. This converts ORD Validation, GWCS Validation and GWCS Verification data into
# common format for universal processing. ORD Verification does not require this algorithm.
Prepare_Segment_Data <- function(Type, Data, Seg_Size, Forecast_Seg_Max, Lookahead_Time){
  
  # ORD Validation. Uses Landing Pair Reference Table - IDs are Landing Pair IDs. Uses Leader Landing Runway.
  if (Type == "ORD"){
    Segment_Forecast <- select(Data, c("Landing_Pair_ID", "Landing_Pair_Date", "Leader_Landing_Runway", "Runway_Group", "Prediction_Time"))
    Segment_Forecast <- rename(Segment_Forecast, c("ID" = "Landing_Pair_ID", "Forecast_Time" = "Prediction_Time",
                                                   "Runway" = "Leader_Landing_Runway", "Date" = "Landing_Pair_Date"))
    
  }
  
  # GWCS Validation. Uses Flight Plan/Derived. IDs are Flight Plan IDs and Forecast Times are Time at 4DME - Lookahead Time.
  # Requires Runway Group to be joined on beforehand.
  if (Type == "GWCS Validation"){
    Segment_Forecast <- select(Data, c("Flight_Plan_ID", "FP_Date", "Landing_Runway", "Runway_Group", "Time_At_4DME"))
    Segment_Forecast <- rename(Segment_Forecast, c("ID" = "Flight_Plan_ID", "Forecast_Time" = "Time_At_4DME",
                                                   "Runway" = "Landing_Runway", "Date" = "FP_Date"))
  }
  
  # GWCS Verification. Leave alone for now.
  if (Type == "GWCS Verification"){}
  
  # Adjust the Forecast Times by subtracting the Lookahead Time. (For ORD this will be 0)
  Segment_Forecast <- mutate(Segment_Forecast, Forecast_Time = Forecast_Time - Lookahead_Time)
  
  # Make a Data Frame for all segments up to Forecast_Seg_Max. (Incorporate Seg Size here?)
  Total_Segments <- data.frame(DME_Seg = 0:(Forecast_Seg_Max / NM_to_m))
  
  # Merge the two Dataframes Together.
  Segment_Forecast <- merge(Segment_Forecast, Total_Segments)
  
  # Convert the DME Segments to SI units.
  Segment_Forecast <- mutate(Segment_Forecast, DME_Seg = DME_Seg * NM_to_m)
  
  return(Segment_Forecast)
  
}


Get_Non_Stale_Segments <- function(Segment_Forecast, Forecast_Seg, Stale_Time){
  
  # -- Add duplicates of the join parameters to not lose anything. 
  
  # For our Segment Forecast Table. Add a minor offset as to match segments correctly.
  Segment_Forecast <- mutate(Segment_Forecast,
                            Join_Date = Date,
                            Join_Runway = Runway_Group,
                            Join_Time = Forecast_Time + 0.00001)
  
  # For tbl_Mode_S_Wind_Seg_Forecast.
  Forecast_Seg <- mutate(Forecast_Seg,
                         Join_Date = Forecast_Date,
                         Join_Runway = Runway_Group,
                         Join_Time = Forecast_Time)
  
  # -- Perform a Rolling Join
  
  # Convert data to data.tables to allow rolling joins.
  Segment_Forecast <- as.data.table(Segment_Forecast)
  Forecast_Seg <- as.data.table(Forecast_Seg)
  
  # Set Keys for Rolling Join. Join_Time will be the roll.
  setkey(Segment_Forecast, Join_Date, Join_Runway, DME_Seg, Join_Time)
  setkey(Forecast_Seg, Join_Date, Join_Runway, DME_Seg, Join_Time)
  
  # Perform the Rolling Join, with the max roll being the Stale Time. 
  Segment_Forecast <- Forecast_Seg[Segment_Forecast, roll = Stale_Time]
  
  # Return to data.frame format and select relevant fields.
  Segment_Forecast <- as.data.frame(Segment_Forecast) %>% select(ID, Date, Join_Runway, DME_Seg, Join_Time, Segment_Time = Forecast_Time,
                                                             Forecast_Wind_Effect_IAS, Forecast_Wind_SPD, Forecast_Wind_HDG,
                                                             Forecast_Aircraft_Type) %>% rename(Runway_Group = Join_Runway,
                                                                                                Forecast_Time = Join_Time)
  # Order data by ID and DME_Seg.
  Segment_Forecast <- Segment_Forecast[order(Segment_Forecast$ID, Segment_Forecast$DME_Seg),]
  
  return(Segment_Forecast)
}

# Function for Transferring Extrpolated Data to updated segment table. Extrapolation_Type = ("ORD", "TBS")
Transfer_Extrapolated_Segment_Data <- function(Segment_Forecast, Extrapolation_Type){
  
  if (Extrapolation_Type == "TBS"){
  Segment_Forecast <- mutate(Segment_Forecast, 
                                 TBS_Interpolated_Seg.x = ifelse(!is.na(Forecast_Wind_Effect_IAS.y), DME_Seg.y, TBS_Interpolated_Seg.x),
                                 Forecast_Time.x = Forecast_Time.y,
                                 Forecast_Wind_Effect_IAS.x = Forecast_Wind_Effect_IAS.y,
                                 Forecast_Wind_SPD.x = Forecast_Wind_SPD.y,
                                 Forecast_Wind_HDG.x = Forecast_Wind_HDG.y,
                                 Forecast_Aircraft_Type.x = Forecast_Aircraft_Type.y) %>%
    select(ID, Date = Date.x, Runway_Group = Runway_Group.x, DME_Seg = DME_Seg.x, Segment_Time = Segment_Time.x,
           Forecast_Time = Forecast_Time.x, Forecast_Wind_Effect_IAS = Forecast_Wind_Effect_IAS.x, Forecast_Wind_SPD = Forecast_Wind_SPD.x,
           Forecast_Wind_HDG = Forecast_Wind_HDG.x, Forecast_Aircraft_Type = Forecast_Aircraft_Type.x, TBS_Interpolated_Seg = TBS_Interpolated_Seg.x)
  }
  
  if (Extrapolation_Type == "ORD"){
    Segment_Forecast <- mutate(Segment_Forecast, 
                         ORD_Extrapolated_Seg.x = ifelse(!is.na(Forecast_Wind_Effect_IAS.y), DME_Seg.y, ORD_Extrapolated_Seg.x),
                         TBS_Interpolated_Seg.x = ifelse(!is.na(Forecast_Wind_Effect_IAS.y), TBS_Interpolated_Seg.y, TBS_Interpolated_Seg.x),
                         Forecast_Time.x = Forecast_Time.y,
                         Forecast_Wind_Effect_IAS.x = Forecast_Wind_Effect_IAS.y,
                         Forecast_Wind_SPD.x = Forecast_Wind_SPD.y,
                         Forecast_Wind_HDG.x = Forecast_Wind_HDG.y,
                         Forecast_Aircraft_Type.x = Forecast_Aircraft_Type.y) %>%
      select(ID, Date = Date.x, Runway_Group = Runway_Group.x, DME_Seg = DME_Seg.x, Segment_Time = Segment_Time.x,
             Forecast_Time = Forecast_Time.x, Forecast_Wind_Effect_IAS = Forecast_Wind_Effect_IAS.x, Forecast_Wind_SPD = Forecast_Wind_SPD.x,
             Forecast_Wind_HDG = Forecast_Wind_HDG.x, Forecast_Aircraft_Type = Forecast_Aircraft_Type.x, TBS_Interpolated_Seg = TBS_Interpolated_Seg.x,
             ORD_Extrapolated_Seg = ORD_Extrapolated_Seg.x)
  }
  
  return(Segment_Forecast)
  
}


TBS_Extrapolate_Segments <- function(Segment_Forecast, Max_Seg_Extrapolation, Extrapolation_Seg_Min, Separation_Forecast_Seg_Max){
  
  # Make a variable for the origin of TBS Interpolated Segment
  Segment_Forecast <- mutate(Segment_Forecast, TBS_Interpolated_Seg = NA)
  
  # Keep the Segs that do not allow extrapolation separately
  Unextrapolated_Forecast <- filter(Segment_Forecast, DME_Seg < Extrapolation_Seg_Min)
  
  # TBS Segs are between Forecast Seg Min and Separation Forecast Seg Max. We Want these + maximum Seg Extrapolation range
  TBS_Segment_Forecast <- filter(Segment_Forecast, DME_Seg >= Extrapolation_Seg_Min & DME_Seg <= (Separation_Forecast_Seg_Max + (Max_Seg_Extrapolation * NM_to_m)))
  
  # Loop through each segment number. Should be updated to reflect segment size. 
  for (i in (Extrapolation_Seg_Min / NM_to_m) : (Separation_Forecast_Seg_Max / NM_to_m)){
    
    # Filter for DME Seg in loop, separate raw segments from ones to be interpolated
    TBS_Segments_This_Seg <- filter(TBS_Segment_Forecast, DME_Seg == i * NM_to_m)
    TBS_Segments_This_Seg_Valid <- filter(TBS_Segments_This_Seg, !is.na(Forecast_Wind_Effect_IAS))
    TBS_Segments_This_Seg_Null <- filter(TBS_Segments_This_Seg, is.na(Forecast_Wind_Effect_IAS))
    
    # Bind the raw segments for this DME to the complete processed segments
    if (i == (Extrapolation_Seg_Min / NM_to_m)){TBS_Segs_Complete <- TBS_Segments_This_Seg_Valid} else {
      TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Valid)}
    
    # Create new loop for extrapolation search. Go outward from 1 to Max_Seg_Extrapolation in either direction
    for (j in 1 : Max_Seg_Extrapolation){
      
      # -- Prioritise lower RTT values, but only do if segment within allowed interpolating range.
      if ((i - j) * NM_to_m >= Extrapolation_Seg_Min){
        
        # Filter for the Segment (i - j). e.g. For Segment 5, will start at Segment 4. 
        TBS_Segments_Extrapolate <- filter(TBS_Segment_Forecast, DME_Seg == (i - j) * NM_to_m)
        
        # Join the Invalid segments to attempt to extrapolate with this segment
        TBS_Segments_Compare <- inner_join(TBS_Segments_This_Seg_Null, TBS_Segments_Extrapolate, by = c("ID")) # and all extra manipulation
        
        # Perform the TBS Extrapolation data transfer.
        TBS_Segments_Compare <- Transfer_Extrapolated_Segment_Data(TBS_Segments_Compare, "TBS")
        
        # Isolate remaining NULL segments.
        TBS_Segments_This_Seg_Null <- filter(TBS_Segments_Compare, is.na(Forecast_Wind_Effect_IAS))
        
        # Isolate successfully extrapolated segments
        TBS_Segments_This_Seg_Valid <- filter(TBS_Segments_Compare, !is.na(Forecast_Wind_Effect_IAS))
        
        # Add extrapolated segments to complete segment list.
        TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Valid)
      }
      
      # -- Then do higher values
      
      # Filter for segment (i + j). 
      TBS_Segments_Extrapolate <- filter(TBS_Segment_Forecast, DME_Seg == (i + j) * NM_to_m)
      
      # Join the Invalid segments to attempt to extrapolate with this segment
      TBS_Segments_Compare <- inner_join(TBS_Segments_This_Seg_Null, TBS_Segments_Extrapolate, by = c("ID"))
      
      # Perform the TBS Extrapolation data transfer.
      TBS_Segments_Compare <- Transfer_Extrapolated_Segment_Data(TBS_Segments_Compare, "TBS")
      
      # Isolate remaining NULL segments.
      TBS_Segments_This_Seg_Null <- filter(TBS_Segments_Compare, is.na(Forecast_Wind_Effect_IAS))
      
      # Isolate successfully extrapolated segments
      TBS_Segments_This_Seg_Valid <- filter(TBS_Segments_Compare, !is.na(Forecast_Wind_Effect_IAS))
      
      # Add extrapolated segments to complete segment list.
      TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Valid)
      
      # If final attempt at extrapolation for ths segment, bind on the remaining NULL forecasts.
      if (j == Max_Seg_Extrapolation){TBS_Segs_Complete <- rbind(TBS_Segs_Complete, TBS_Segments_This_Seg_Null)}
      
    }
    
  }
  
  # Remove Old segments in the TBS Extrapolation range
  Segment_Forecast <- filter(Segment_Forecast, DME_Seg > Separation_Forecast_Seg_Max)
  
  # Add on thenew TBS Extrapolated Segments, as well as the Untouched segments
  Segment_Forecast <- rbind(Segment_Forecast, Unextrapolated_Forecast) %>% rbind(TBS_Segs_Complete)
  
  # Order Segments as before
  Segment_Forecast <- Segment_Forecast[order(Segment_Forecast$ID, Segment_Forecast$DME_Seg),]
  
  return(Segment_Forecast)
}



ORD_Extrapolate_Segments <- function(Segment_Forecast, Forecast_Seg_Max, Separation_Forecast_Seg_Max){
  
  # Add a new parameter to signify which segment was used for ORD Extrapolation.
  Segment_Forecast <- mutate(Segment_Forecast, ORD_Extrapolated_Seg = NA)
  
  # ORD Segs are between Separation Forecast Seg Max - Extrap range and Forecast_Valid_Seg_Max.
  ORD_Segs <- filter(Segment_Forecast, DME_Seg <= Forecast_Seg_Max & DME_Seg >= (Separation_Forecast_Seg_Max))
  
  # We want to loop through all segments, starting with Separation_Forecast_Seg_Max + 1 
  for (i in ((Separation_Forecast_Seg_Max / NM_to_m) + 1) : (Forecast_Seg_Max / NM_to_m)){
    
    # Select Data for this segment and the previous segment separately.
    ORD_Segs_This_Seg <- filter(ORD_Segs, DME_Seg == (i * NM_to_m))
    ORD_Segs_Prev_Seg <- filter(ORD_Segs, DME_Seg == ((i - 1) * NM_to_m))
    
    # Select Populated and Non-Populated data from This Segment separately.
    ORD_Segs_This_Seg_Valid <- filter(ORD_Segs_This_Seg, !is.na(Forecast_Wind_Effect_IAS))
    ORD_Segs_This_Seg_Null <- filter(ORD_Segs_This_Seg, is.na(Forecast_Wind_Effect_IAS))
    
    # Join on the Invalid This Segment Data to the Previous Segment Data.
    ORD_Segs_This_Seg_Compare <- inner_join(ORD_Segs_This_Seg_Null, ORD_Segs_Prev_Seg, by=c("ID"))
    
    # Perform the ORD Extrapolation data transfer.
    ORD_Segs_This_Seg_Compare <- Transfer_Extrapolated_Segment_Data(ORD_Segs_This_Seg_Compare, "ORD")
    
    # Bind the previously valid segs with those attempted to be extrapolated.
    ORD_Segs_This_Seg <- rbind(ORD_Segs_This_Seg_Valid, ORD_Segs_This_Seg_Compare)
    
    # Filter out this DME Segment from ORD_Segs.
    ORD_Segs <- filter(ORD_Segs, DME_Seg != (i * NM_to_m))
    
    # Add in the extrapolated data for this segment.
    ORD_Segs <- rbind(ORD_Segs, ORD_Segs_This_Seg)
  }
  
  # Remove the old ORD Segs
  Segment_Forecast <- filter(Segment_Forecast, DME_Seg < Separation_Forecast_Seg_Max)
  
  # Add new ORD Segs
  Segment_Forecast <- rbind(Segment_Forecast, ORD_Segs)
  
  # Order as before
  Segment_Forecast <- Segment_Forecast[order(Segment_Forecast$ID, Segment_Forecast$DME_Seg),]
  
  return(Segment_Forecast)
}


# Function that changes the Wind entries depending on GWCS Wind Selection. Requires Extrapolation Complete.
# !!! Do we need to change Wind Speed, HDG and Aircraft Type? See for GWCS.
Treat_Default_Wind_Segments <- function(Segment_Forecast, Default_Wind, GWCS_Wind_Selection){
  
  # Join on the Default Wind
  Segment_Forecast <- left_join(Segment_Forecast, Default_Wind, by = c("DME_Seg" = "Wind_Segment_Start"))
  
  # If GWCS_Wind_Selection is Always_Default, replace ALL Wind Effect values with Default
  if (GWCS_Wind_Selection == "Always_Default"){Segment_Forecast <- mutate(Segment_Forecast, Forecast_Wind_Effect_IAS = Wind_Effect)}
  
  # If GWCS_Wind_Selection is Always_Zero, replace ALL Wind Effect values with 0.
  if (GWCS_Wind_Selection == "Always_Zero"){Segment_Forecast <- mutate(Segment_Forecast, Forecast_Wind_Effect_IAS = 0)}
  
  # If GWCS_Wind_Selection is Auto_Default, replace NA Wind Effect values with Default. (After Extrapolation)
  if (GWCS_Wind_Selection == "Auto_Default"){
    Segment_Forecast <- mutate(Segment_Forecast, 
                               Forecast_Wind_Effect_IAS = ifelse(is.na(Forecast_Wind_Effect_IAS), Wind_Effect, Forecast_Wind_Effect_IAS))
  }
  
  # Remove Default Parameters.
  Segment_Forecast <- select(Segment_Forecast, -c("Wind_Effect", "Wind_Segment_End"))
  
  return(Segment_Forecast)
  
}  


# ---------------------------
# 1.6. Processing
# ---------------------------

# Prepare the Segment Data
ORD_Segment_Forecast <- Prepare_Segment_Data("ORD", Landing_Pair_Reference_Full, Seg_Size, Forecast_Seg_Max, 0)

# Get the Non-Stale Segments
ORD_Segment_Forecast <- Get_Non_Stale_Segments(ORD_Segment_Forecast, Forecast_Seg, Stale_Time)

# Perform TBS Extrapolation
ORD_Segment_Forecast <- TBS_Extrapolate_Segments(ORD_Segment_Forecast, Max_Seg_Extrapolation, Extrapolation_Seg_Min, Separation_Forecast_Seg_Max)

# Perform ORD Extrapolation
ORD_Segment_Forecast <- ORD_Extrapolate_Segments(ORD_Segment_Forecast, Forecast_Seg_Max, Separation_Forecast_Seg_Max)

# Update Wind Effect Values based on GWCS Wind Selection.
ORD_Segment_Forecast <- Treat_Default_Wind_Segments(ORD_Segment_Forecast, Default_Wind, GWCS_Wind_Selection)

# ---------------------------
# 1.6. Testing
# ---------------------------
# Fidelity of testing is not ideal as there is no table to directly compare results to.
# The closest option to compare wind effect values is to join onto the GS Profile table
# and assess the values that are available.
# We are aware of 2 issues:
# 1) In rare cases (22/8m) there are two forecast segments with the same time/date for a given runway group (different runways)
# 2) The SQL GS Profile gives some values (~600/8m) NA whereas these segments are NOT NA. This is not actually an issue.
# When populating the GSPD profile in R these will also not populate - the lack of values come from the LSS not being calculated
# due to no access to a valid surface wind value.
# Issue 1) is deemed acceptable as it is within algorithm scope.
# ---------------------------

#GS_SQL <- sqlQuery(con, "SELECT * FROM tbl_ORD_GS_Profile", stringsAsFactors = F)

#GS_SQL <- mutate(GS_SQL, DME_Seg = floor(End_Dist/1852)*1852,
 #                End_Wind_Effect = End_GS - End_IAS)

#forecast_test <- select(Forecast_Segs1, Landing_Pair_ID = ID, DME_Seg, Forecast_Wind_Effect_IAS)

#GS_TEST <- left_join(GS_SQL, forecast_test, by=c("Landing_Pair_ID", "DME_Seg"))

#fucked0 <- filter(GS_TEST, abs(Forecast_Wind_Effect_IAS - End_Wind_Effect) > 0.0001)
#fucked1 <- filter(GS_TEST, !is.na(Forecast_Wind_Effect_IAS) & is.na(End_Wind_Effect))
#fucked2 <- filter(GS_TEST, is.na(Forecast_Wind_Effect_IAS) & !is.na(End_Wind_Effect))
#fucked <- rbind(fucked0, fucked1) %>% rbind(fucked2)
#fucked <- fucked[order(fucked$Landing_Pair_ID, fucked$End_Dist),]
#fuckedids <- unique(fucked$Landing_Pair_ID)
#fucked0ids <- unique(fucked0$Landing_Pair_ID)
#fucked1ids <- unique(fucked1$Landing_Pair_ID)
#View(fucked1ids)

#predtest <- filter(ORD_Prediction_SQL, Landing_Pair_ID %in% fucked1ids)

#fuckedgs <- filter(GS_TEST, Landing_Pair_ID %in% fucked0ids)
#fuckedgs <- fuckedgs[order(fuckedgs$Landing_Pair_ID, fuckedgs$End_Dist),]

#forecasttest2 <- filter(Forecast_Segs1, ID %in% fucked1ids)
#forecasttest2 <- filter(Forecast_Segs1, DME_Seg == 0 & is.na(Forecast_Wind_Effect_IAS))

#testseg <- filter(Forecast_Seg, DME_Seg == 18520, Forecast_Date == "04/11/2018", Runway_Group == "RSo")
#testseg <- testseg[order(testseg$Forecast_Time),]





