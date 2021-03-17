# ----------------------------------------------------------------------- #
# Import Config Functions -------------------------------------------------
# ----------------------------------------------------------------------- #

Populate_tbl_Adaptation <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Mag_Var <- x$Mag_Var * fnc_GI_Degs_To_Rads()
  x$Grid_Projection_Origin_Lat <- fnc_GI_Latlong_String_To_Radians(x$Grid_Projection_Origin_Lat)
  x$Grid_Projection_Origin_Lon <- fnc_GI_Latlong_String_To_Radians(x$Grid_Projection_Origin_Lon)
  x$Load_X_Range <- as.numeric(x$Load_X_Range) * fnc_GI_Nm_To_M()
  x$Load_Y_Range <- as.numeric(x$Load_Y_Range) * fnc_GI_Nm_To_M()
  x$Load_Min_Alt <- as.numeric(x$Load_Min_Alt) * fnc_GI_Ft_To_M()
  x$Load_Max_Alt <- as.numeric(x$Load_Max_Alt) * fnc_GI_Ft_To_M()
  x$Diff_Mode_S_To_Radar_Track_Max <- as.numeric(x$Diff_Mode_S_To_Radar_Track_Max) * fnc_GI_Degs_To_Rads()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Adaptation_Data...")
  dbWriteTable(dbi_con, "tbl_Adaptation_Data", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Adaptation_Data")
}

Populate_tbl_Airfield <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Airfield...")
  dbWriteTable(dbi_con, "tbl_Airfield", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Airfield")
}

Populate_tbl_Runway <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  
  x$Threshold_Lat <- as.numeric(x$Threshold_Lat) * fnc_GI_Degs_To_Rads()
  x$Threshold_Lon <- as.numeric(x$Threshold_Lon) * fnc_GI_Degs_To_Rads()
  x$Heading <- as.numeric(x$Heading) * fnc_GI_Degs_To_Rads() + tbl_Adaptation_Data$Mag_Var
  x$Glideslope_Angle <- as.numeric(x$Glideslope_Angle) * fnc_GI_Degs_To_Rads()
  x$Elevation <- as.numeric(x$Elevation) * fnc_GI_Ft_To_M()
  
  Converted_XY <- usp_GI_Latlong_To_XY(x$Threshold_Lat, x$Threshold_Lon, tbl_Adaptation_Data)
  x$Threshold_X_Pos <- Converted_XY$Position_X
  x$Threshold_Y_Pos <- Converted_XY$Position_Y
  
  Corresponding_Rwy <- runway_Opposite_End(x$Runway_Name)
  for (i in 1:nrow(x)) {
    x$NODE_Heading_Offset[i] <- -atan(
      (as.numeric(x$Threshold_Y_Pos[i]) - x[Runway_Name == Corresponding_Rwy[i]]$Threshold_Y_Pos) /
        (as.numeric(x$Threshold_X_Pos[i]) - x[Runway_Name == Corresponding_Rwy[i]]$Threshold_X_Pos)
    )
  }
  
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Runway...")
  dbWriteTable(dbi_con, "tbl_Runway", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Runway")
}

Populate_tbl_Mode_S_Wind_Localiser_Capture <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  x$Min_Heading <- (as.numeric(x$Min_Heading) + round(tbl_Runway[x$Runway_Name, on="Runway_Name"]$NODE_Heading_Offset / fnc_GI_Degs_To_Rads(), 0)) * fnc_GI_Degs_To_Rads()
  x$Max_Heading <- (as.numeric(x$Max_Heading) + round(tbl_Runway[x$Runway_Name, on="Runway_Name"]$NODE_Heading_Offset / fnc_GI_Degs_To_Rads(), 0)) * fnc_GI_Degs_To_Rads()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Mode_S_Wind_Localiser_Capture...")
  dbWriteTable(dbi_con, "tbl_Mode_S_Wind_Localiser_Capture", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Mode_S_Wind_Localiser_Capture")
}

Populate_Airspace_Volumes <- function(LogFilePath, dbi_con) {
  
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  
  volumes <- data.table(
    Volume_Name = x$Volume_Name,
    Runway_Name = gsub("^([A-Z0-9]{1,})_.*$", "R\\1", x$Volume_Name),
    Volume_Type = x$Volume_Type,
    Min_Altitude = as.numeric(x$Min_Altitude) * fnc_GI_Ft_To_M(),
    Max_Altitude = as.numeric(x$Max_Altitude) * fnc_GI_Ft_To_M()
  )
  
  message("[",Sys.time(),"] ", "Appending ", nrow(volumes), " rows to tbl_Volume...")
  dbWriteTable(dbi_con, "tbl_Volume", volumes, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(volumes), " rows to tbl_Volume")
  
  polygons <- configVolume_To_pointSequence(x, dbi_con)
  
  message("[",Sys.time(),"] ", "Appending ", nrow(polygons), " rows to tbl_Polygon...")
  dbWriteTable(dbi_con, "tbl_Polygon", polygons, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(polygons), " rows to tbl_Polygon")
  
}

Populate_Airspace_Volumes_DW <- function(Route_Fix_Path, Route_Point_Path, dbi_con, Volume_Width = 1) {
  
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  
  Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
  
  Volume_Width <- Volume_Width * fnc_GI_Nm_To_M()
  
  message("[",Sys.time(),"] ", "Processing additional downwind routes...")
  
  route_fix <- fread(Route_Fix_Path)
  
  route_fix$Latitude <- as.numeric(route_fix$Latitude) * fnc_GI_Degs_To_Rads()
  route_fix$Longitude <- as.numeric(route_fix$Longitude)  * fnc_GI_Degs_To_Rads()
  
  Converted_XY <- usp_GI_Latlong_To_XY(route_fix$Latitude, route_fix$Longitude, tbl_Adaptation_Data)
  route_fix$X_Pos <- Converted_XY$Position_X
  route_fix$Y_Pos <- Converted_XY$Position_Y
  
  route_point <- fread(Route_Point_Path)
  route_point$LH_Vert_X <- as.numeric(route_point$LH_Vert_X)
  route_point$LH_Vert_Y <- as.numeric(route_point$LH_Vert_Y)
  route_point$RH_Vert_X <- as.numeric(route_point$RH_Vert_X)
  route_point$RH_Vert_Y <- as.numeric(route_point$RH_Vert_Y)
  route_point$Hdg_To_Next_Point <- as.numeric(route_point$Hdg_To_Next_Point)
  route_point$Volume_Name <- paste0(route_point$Route_Name, route_point$Point_Sequence)
  
  route_point_list <- lapply(unique(route_point$Route_Name), function(i) {
    
    route_point_i <- route_point[Route_Name == i][order(Point_Sequence)]
    
    if (nrow(route_point_i) < 2) return(route_point_i)
    
    route_fix_sequence <- route_fix[route_point_i$Fix_Name, on = "Fix_Name"]
    
    for (j in 1:nrow(route_fix_sequence)) {
      
      V1_X <- route_fix_sequence$X_Pos[j] - route_fix_sequence$X_Pos[j-1]
      V1_Y <- route_fix_sequence$Y_Pos[j] - route_fix_sequence$Y_Pos[j-1]
      V2_X <- route_fix_sequence$X_Pos[j+1] - route_fix_sequence$X_Pos[j]
      V2_Y <- route_fix_sequence$Y_Pos[j+1] - route_fix_sequence$Y_Pos[j]
      
      if (j == 1) {
        V1_X <- V2_X
        V1_Y <- V2_Y
      } else if (j == nrow(route_fix_sequence)) {
        V2_X <- V1_X
        V2_Y <- V1_Y
      }
      
      V1_Hdg <- fnc_GI_To_Vector_Angle(V1_X, V1_Y)
      V2_Hdg <- fnc_GI_To_Vector_Angle(V2_X, V2_Y)
      
      Ave_Route_Hdg <- (V1_Hdg + V2_Hdg) / 2
      
      LH_Vert_Hdg <- Ave_Route_Hdg - pi / 2
      RH_Vert_Hdg <- Ave_Route_Hdg + pi / 2
      
      Alpha <- (pi - (V2_Hdg - V1_Hdg)) / 2
      Vertex_Dist <- (Volume_Width / 2) / sin(Alpha)
      
      Vec_LH_Vert_X <- fnc_GI_To_Vector_X(Vertex_Dist, LH_Vert_Hdg)
      Vec_LH_Vert_Y <- fnc_GI_To_Vector_Y(Vertex_Dist, LH_Vert_Hdg)
      Vec_RH_Vert_X <- fnc_GI_To_Vector_X(Vertex_Dist, RH_Vert_Hdg)
      Vec_RH_Vert_Y <- fnc_GI_To_Vector_Y(Vertex_Dist, RH_Vert_Hdg)
      
      route_point_i[j]$LH_Vert_X <- route_fix_sequence$X_Pos[j] + Vec_LH_Vert_X
      route_point_i[j]$LH_Vert_Y <- route_fix_sequence$Y_Pos[j] + Vec_LH_Vert_Y
      route_point_i[j]$RH_Vert_X <- route_fix_sequence$X_Pos[j]+ Vec_RH_Vert_X
      route_point_i[j]$RH_Vert_Y <- route_fix_sequence$Y_Pos[j] + Vec_RH_Vert_Y
      route_point_i[j]$Hdg_To_Next_Point <- V2_Hdg
    }
    
    return(route_point_i)
    
  })
  
  route_point <- rbindlist(route_point_list)
  
  volumes = data.table(
    Volume_Name = route_point$Volume_Name,
    Airfield_Name = Airfield_Name,
    Runway_Name = NA,
    Volume_Type = "PLT",
    Min_Altitude = -500 * fnc_GI_Ft_To_M(),
    Max_Altitude = 7500 * fnc_GI_Ft_To_M()
  )
  
  volumes_remove_ind <- sapply( 1:(nrow(volumes) - 1), function(i) {
    if (gsub("^(.*)[0-9]{1}$", "\\1", volumes$Volume_Name[i]) != gsub("^(.*)[0-9]{1}$", "\\1", volumes$Volume_Name[i+1])) {
      return(i)
    } else {
      return(NA)
    }
  }, simplify = T) %>% .[!is.na(.)] %>% c(., nrow(volumes))
  
  volumes <- volumes[-volumes_remove_ind]
  
  message("[",Sys.time(),"] ", "Appending ", nrow(volumes), " rows of additional downwind routes to tbl_Volume...")
  dbWriteTable(dbi_con, "tbl_Volume", volumes, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(volumes), " rows to tbl_Volume")
  
  polygons_list <- lapply(1:(nrow(route_point)-1), function(k) {
    
    if (route_point$Route_Name[k] != route_point$Route_Name[k+1]) return(NULL)
    
    data.table(
      Volume_Name = route_point$Volume_Name[k],
      Airfield_Name = "",
      Point_Sequence = 1:5,
      Point_X = c(route_point$LH_Vert_X[k], route_point$LH_Vert_X[k+1], route_point$RH_Vert_X[k+1], route_point$RH_Vert_X[k], route_point$LH_Vert_X[k]),
      Point_Y = c(route_point$LH_Vert_Y[k], route_point$LH_Vert_Y[k+1], route_point$RH_Vert_Y[k+1], route_point$RH_Vert_Y[k], route_point$LH_Vert_Y[k])
    )
    
  })
  
  polygons <- rbindlist(polygons_list, use.names = T)
  polygons$Airfield_Name <- Airfield_Name
  
  Converted_LatLon <- usp_GI_Latlong_From_XY(polygons$Point_X, polygons$Point_Y, tbl_Adaptation_Data)
  polygons$Latitude <- Converted_LatLon$PositionLatitude
  polygons$Longitude <- Converted_LatLon$PositionLongitude
  
  message("[",Sys.time(),"] ", "Appending ", nrow(polygons), " rows of additional downwind routes to tbl_Polygon...")
  dbWriteTable(dbi_con, "tbl_Polygon", polygons, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(polygons), " rows to tbl_Polygon")
  
}

Populate_tbl_Mode_S_Wind_Adaptation <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Mode_S_GSPD_Min <- as.numeric(x$Mode_S_GSPD_Min) * fnc_GI_Kts_To_M_Per_Sec()
  x$Mode_S_GSPD_Max <- as.numeric(x$Mode_S_GSPD_Max) * fnc_GI_Kts_To_M_Per_Sec()
  x$Mode_S_IAS_Min <- as.numeric(x$Mode_S_IAS_Min) * fnc_GI_Kts_To_M_Per_Sec()
  x$Mode_S_IAS_Max <- as.numeric(x$Mode_S_IAS_Max) * fnc_GI_Kts_To_M_Per_Sec()
  x$Mode_S_TAS_Min <- as.numeric(x$Mode_S_TAS_Min) * fnc_GI_Kts_To_M_Per_Sec()
  x$Mode_S_TAS_Max <- as.numeric(x$Mode_S_TAS_Max) * fnc_GI_Kts_To_M_Per_Sec()
  x$Mode_S_Roll_Angle_Max <- as.numeric(x$Mode_S_Roll_Angle_Max) * fnc_GI_Degs_To_Rads()
  x$DME_Seg_Min <- as.numeric(x$DME_Seg_Min) * fnc_GI_Nm_To_M()
  x$DME_Seg_Max <- as.numeric(x$DME_Seg_Max) * fnc_GI_Nm_To_M()
  x$DME_Seg_Size <- as.numeric(x$DME_Seg_Size) * fnc_GI_Nm_To_M()
  x$Altitude_Tolerance <- as.numeric(x$Altitude_Tolerance) * fnc_GI_Ft_To_M()
  x$Diff_Track_To_Runway_HDG_Max <- as.numeric(x$Diff_Track_To_Runway_HDG_Max) * fnc_GI_Degs_To_Rads()
  x$Diff_HDG_To_Runway_HDG_Max <- as.numeric(x$Diff_HDG_To_Runway_HDG_Max) * fnc_GI_Degs_To_Rads()
  x$Diff_Mode_S_To_Radar_Track_Max <- as.numeric(x$Diff_Mode_S_To_Radar_Track_Max) * fnc_GI_Degs_To_Rads()
  x$Diff_Mode_S_To_Radar_GSPD_Max <- as.numeric(x$Diff_Mode_S_To_Radar_GSPD_Max) * fnc_GI_Kts_To_M_Per_Sec()
  x$Max_Wind_Effect <- as.numeric(x$Max_Wind_Effect) * fnc_GI_Kts_To_M_Per_Sec()
  x$Max_Wind_SPD <- as.numeric(x$Max_Wind_SPD) * fnc_GI_Kts_To_M_Per_Sec()
  x$Forecast_Seg_Min <- as.numeric(x$Forecast_Seg_Min) * fnc_GI_Nm_To_M()
  x$Forecast_Seg_Max <- as.numeric(x$Forecast_Seg_Max) * fnc_GI_Nm_To_M()
  x$Extrapolation_Seg_Min <- as.numeric(x$Extrapolation_Seg_Min) * fnc_GI_Nm_To_M()
  x$Separation_Forecast_Seg_Max <- as.numeric(x$Separation_Forecast_Seg_Max) * fnc_GI_Nm_To_M()
  x$Mode_S_BPS_Min <- as.numeric(x$Mode_S_BPS_Min) * fnc_GI_Mbar_To_Pa()
  x$Mode_S_BPS_Max <- as.numeric(x$Mode_S_BPS_Max) * fnc_GI_Mbar_To_Pa()
  x$Mode_S_BPS_DME_Min <- as.numeric(x$Mode_S_BPS_DME_Min) * fnc_GI_Nm_To_M()
  x$Mode_S_BPS_DME_Max <- as.numeric(x$Mode_S_BPS_DME_Max) * fnc_GI_Nm_To_M()
  x$Max_RTT_Null_Derived_QNH <- as.numeric(x$Max_RTT_Null_Derived_QNH) * fnc_GI_Nm_To_M()
  x$Mode_S_BPS_Delta_Max <- as.numeric(x$Mode_S_BPS_Delta_Max) * fnc_GI_Mbar_To_Pa()
  x$Mode_S_BPS_Delta_Check <- as.numeric(x$Mode_S_BPS_Delta_Check) * fnc_GI_Mbar_To_Pa()
  x$Mode_S_BPS_Alt_Diff_Max <- as.numeric(x$Mode_S_BPS_Alt_Diff_Max) * fnc_GI_Ft_To_M()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Mode_S_Wind_Adaptation...")
  dbWriteTable(dbi_con, "tbl_Mode_S_Wind_Adaptation", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Mode_S_Wind_Adaptation")
}

Populate_tbl_Path_Leg <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath, na.strings = c(""))
  if ("Airfield_Name" %!in% names(x)) {
    Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
    x$Airfield_Name <- Airfield_Name
  }
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Path_Leg...")
  dbWriteTable(dbi_con, "tbl_Path_Leg", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Path_Leg")
}

Populate_tbl_Path_Leg_Transition <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath, na.strings = c(""))
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  
  for (i in 1:nrow(x)) {
    
    # if (is.na(x$Runway_Name[i]) & !is.na(x$Associated_Runway[i])) {
    #   x$Runway_Name[i] <- x$Associated_Runway[i]
    # } else if (is.na(x$Runway_Name[i]) & is.na(x$Associated_Runway[i])) {
    #   if (!is.na(x$Volume_Name[i])) {
    #     x$Runway_Name[i] <- paste0("R", unlist(strsplit(x$Volume_Name[i], "_"))[1])
    #   } else if (!is.na(x$Current_Path_Leg[i])) {
    #     x$Runway_Name[i] <- gsub("^.*_R?([0-3]{1}[0-9]{1}[L|R]?).*$", "R\\1", x$Current_Path_Leg[i])
    #   } else if (!is.na(x$New_Path_Leg[i])) {
    #     x$Runway_Name[i] <- gsub("^.*_R?([0-3]{1}[0-9]{1}[L|R]?).*$", "R\\1", x$New_Path_Leg[i])
    #   }
    # } else {
    #   warning("Required Runway_Name but none found on row ", i, "!")
    # }
    
    if (x$Min_Heading[i] != 0 & x$Max_Heading[i] != 360 & !is.na(x$Associated_Runway[i])) {
      x$Min_Heading[i] <- x$Min_Heading[i] + round(tbl_Runway[Runway_Name == x$Associated_Runway[i]]$NODE_Heading_Offset[1] / fnc_GI_Degs_To_Rads(), 0)
      x$Max_Heading[i] <- x$Max_Heading[i] + round(tbl_Runway[Runway_Name == x$Associated_Runway[i]]$NODE_Heading_Offset[1] / fnc_GI_Degs_To_Rads(), 0)
    }
    if (x$Min_Heading[i] > 0 & x$Max_Heading[i] > 360) {
      x$Min_Heading[i] <- x$Min_Heading[i] - 360
      x$Max_Heading[i] <- x$Max_Heading[i] - 360
    }
    if (x$Min_Heading[i] < -360 & x$Max_Heading[i] < 0) {
      x$Min_Heading[i] <- x$Min_Heading[i] + 360
      x$Max_Heading[i] <- x$Max_Heading[i] + 360
    }
  }
  if (nrow(x[Min_Heading >= Max_Heading | abs(Min_Heading) > 360 | abs(Max_Heading) > 360 | abs(Max_Heading - Min_Heading) > 360]) > 0) {
    stop("ADAPTATION DATA ERROR: Invalid transition heading range detected")
  }
  
  if ("Airfield_Name" %!in% names(x)) {
    Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
    x$Airfield_Name <- Airfield_Name
  }
  
  x$Min_Heading <- x$Min_Heading * fnc_GI_Degs_To_Rads()
  x$Max_Heading <- x$Max_Heading * fnc_GI_Degs_To_Rads()
  x$Min_Sustained_RoCD <- x$Min_Sustained_RoCD * fnc_GI_Ft_Per_Min_To_M_Per_Sec()
  
  # Deprecated parameters
  # x$Min_Range_To_ILS <- x$Min_Range_To_ILS * fnc_GI_Nm_To_M()
  # x$Max_Range_To_ILS <- x$Max_Range_To_ILS * fnc_GI_Nm_To_M()
  # x$Max_Altitude <- x$Max_Altitude * fnc_GI_Ft_To_M()

  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Path_Leg_Transition...")
  dbWriteTable(dbi_con, "tbl_Path_Leg_Transition", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Path_Leg_Transition")
}

Populate_tbl_ORD_Runway_Adaptation <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  x$Local_Stabilisation_Distance <- x$Local_Stabilisation_Distance * fnc_GI_Nm_To_M()
  x$Compression_Commencement_Threshold <- x$Compression_Commencement_Threshold * fnc_GI_Nm_To_M()
  x$Max_DTT <- x$Max_DTT * fnc_GI_Nm_To_M()
  x$Four_Hundred_Ft_AAL <- x$Four_Hundred_Ft_AAL * fnc_GI_Ft_To_M() / tan(tbl_Runway[x$Runway_Name, on="Runway_Name"]$Glideslope_Angle) - tbl_Runway[x$Runway_Name, on="Runway_Name"]$Touchdown_Offset
  x$Six_Hundred_Ft_AAL <- x$Six_Hundred_Ft_AAL * fnc_GI_Ft_To_M() / tan(tbl_Runway[x$Runway_Name, on="Runway_Name"]$Glideslope_Angle) - tbl_Runway[x$Runway_Name, on="Runway_Name"]$Touchdown_Offset
  x$Thousand_Ft_Gate <- x$Thousand_Ft_Gate * fnc_GI_Ft_To_M() / tan(tbl_Runway[x$Runway_Name, on="Runway_Name"]$Glideslope_Angle) - tbl_Runway[x$Runway_Name, on="Runway_Name"]$Touchdown_Offset
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_ORD_Runway_Adaptation...")
  dbWriteTable(dbi_con, "tbl_ORD_Runway_Adaptation", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_ORD_Runway_Adaptation")
}

Populate_tbl_Aircraft_Type_To_Wake <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  names(x)[1:2] <- c("Aircraft_Type", "Wake") # To match database column names
  x[Aircraft_Class == ""]$Aircraft_Class <- NA
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Aircraft_type_To_Wake...")
  dbWriteTable(dbi_con, "tbl_Aircraft_type_To_Wake", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Aircraft_type_To_Wake")
}

Populate_tbl_Aircraft_Type_To_Wake_Legacy <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  names(x)[1:2] <- c("Aircraft_Type", "Wake") # To match database column names
  
  existing <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Aircraft_Type_To_Wake_Legacy"))
  
  x_new <- x[Aircraft_Type %!in% existing$Aircraft_Type]
  x_old <- x[Aircraft_Type %in% existing$Aircraft_Type]
  
  if (nrow(x_old) > 0){
    for (i in 1:nrow(x_old)) {
      if (x_old$Wake[i] != existing[Aircraft_Type == x_old$Aircraft_Type[i]]$Wake[1]) {
        x_new <- rbind(x_new, x_old[i])
      }
    }
  }

  
  existing_keep <- existing[Aircraft_Type %!in% x_new$Aircraft_Type]
  
  out <- rbind(x_new, existing_keep)
  
  message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Aircraft_Type_To_Wake_Legacy...")
  dbWriteTable(dbi_con, "tbl_Aircraft_Type_To_Wake_Legacy", out, overwrite = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Aircraft_Type_To_Wake_Legacy")
}

Populate_tbl_Assumed_Recat_Separation_IAS <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Assumed_Wake_Separation_IAS <- x$Assumed_Wake_Separation_IAS * fnc_GI_Kts_To_M_Per_Sec()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Assumed_Recat_Separation_IAS...")
  dbWriteTable(dbi_con, "tbl_Assumed_Recat_Separation_IAS", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Assumed_Recat_Separation_IAS")
}

Populate_tbl_Assumed_ROT_Spacing_IAS <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Assumed_ROT_Spacing_IAS <- x$Assumed_ROT_Spacing_IAS * fnc_GI_Kts_To_M_Per_Sec()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Assumed_ROT_Spacing_IAS...")
  dbWriteTable(dbi_con, "tbl_Assumed_ROT_Spacing_IAS", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Assumed_ROT_Spacing_IAS")
}

Populate_tbl_Assumed_TBS_Table_IAS <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Reference_Wake_Separation_Distance <- x$Reference_Wake_Separation_Distance * fnc_GI_Nm_To_M()
  x$Assumed_Wake_Separation_IAS <- x$Assumed_Wake_Separation_IAS * fnc_GI_Kts_To_M_Per_Sec()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Assumed_TBS_Table_IAS...")
  dbWriteTable(dbi_con, "tbl_Assumed_TBS_Table_IAS", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Assumed_TBS_Table_IAS")
}

Populate_tbl_DBS_Wake_Turbulence <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$WT_Separation_Distance <- x$WT_Separation_Distance * fnc_GI_Nm_To_M()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_DBS_Wake_Turbulence...")
  dbWriteTable(dbi_con, "tbl_DBS_Wake_Turbulence", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_DBS_Wake_Turbulence")
}

Populate_tbl_Mode_S_Wind_Default_Wind_Effect_Segments <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Wind_Segment_Start <- x$Wind_Segment_Start * fnc_GI_Nm_To_M()
  x$Wind_Segment_End <- x$Wind_Segment_End * fnc_GI_Nm_To_M()
  x$Wind_Effect <- x$Wind_Effect * fnc_GI_Kts_To_M_Per_Sec()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Mode_S_Wind_Default_Wind_Effect_Segments...")
  dbWriteTable(dbi_con, "tbl_Mode_S_Wind_Default_Wind_Effect_Segments", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Mode_S_Wind_Default_Wind_Effect_Segments")
}

Populate_tbl_ORD_Aircraft_Adaptation <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Min_Safe_Landing_Speed_Lead <- x$Min_Safe_Landing_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Min_Safe_Landing_Speed_Follower <- x$Min_Safe_Landing_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Compression_Commencement_Threshold <- x$Compression_Commencement_Threshold * fnc_GI_Nm_To_M()
  x$Local_Stabilisation_Distance_Lead <- x$Local_Stabilisation_Distance_Lead * fnc_GI_Nm_To_M()
  x$Local_Stabilisation_Distance_Follower <- x$Local_Stabilisation_Distance_Follower * fnc_GI_Nm_To_M()
  x$Steady_Procedural_Speed_Lead <- x$Steady_Procedural_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Steady_Procedural_Speed_Follower <- x$Steady_Procedural_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Final_Deceleration_Lead <- x$Final_Deceleration_Lead * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$Final_Deceleration_Follower <- x$Final_Deceleration_Follower * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$End_Initial_Deceleration_Distance_Lead <- x$End_Initial_Deceleration_Distance_Lead * fnc_GI_Nm_To_M()
  x$End_Initial_Deceleration_Distance_Follower <- x$End_Initial_Deceleration_Distance_Follower * fnc_GI_Nm_To_M()
  x$Initial_Procedural_Speed_Lead <- x$Initial_Procedural_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Initial_Procedural_Speed_Follower <- x$Initial_Procedural_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Initial_Deceleration_Lead <- x$Initial_Deceleration_Lead * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$Initial_Deceleration_Follower <- x$Initial_Deceleration_Follower * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_ORD_Aircraft_Adaptation...")
  dbWriteTable(dbi_con, "tbl_ORD_Aircraft_Adaptation", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_ORD_Aircraft_Adaptation")
}

Populate_tbl_ORD_DBS_Adaptation <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$DBS_Distance <- as.numeric(x$DBS_Distance) * fnc_GI_Nm_To_M()
  x$Min_Safe_Landing_Speed_Lead <- x$Min_Safe_Landing_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Min_Safe_Landing_Speed_Follower <- x$Min_Safe_Landing_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Compression_Commencement_Threshold <- x$Compression_Commencement_Threshold * fnc_GI_Nm_To_M()
  x$Local_Stabilisation_Distance_Lead <- x$Local_Stabilisation_Distance_Lead * fnc_GI_Nm_To_M()
  x$Local_Stabilisation_Distance_Follower <- x$Local_Stabilisation_Distance_Follower * fnc_GI_Nm_To_M()
  x$Steady_Procedural_Speed_Lead <- x$Steady_Procedural_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Steady_Procedural_Speed_Follower <- x$Steady_Procedural_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Final_Deceleration_Lead <- x$Final_Deceleration_Lead * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$Final_Deceleration_Follower <- x$Final_Deceleration_Follower * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$End_Initial_Deceleration_Distance_Lead <- x$End_Initial_Deceleration_Distance_Lead * fnc_GI_Nm_To_M()
  x$End_Initial_Deceleration_Distance_Follower <- x$End_Initial_Deceleration_Distance_Follower * fnc_GI_Nm_To_M()
  x$Initial_Procedural_Speed_Lead <- x$Initial_Procedural_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Initial_Procedural_Speed_Follower <- x$Initial_Procedural_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Initial_Deceleration_Lead <- x$Initial_Deceleration_Lead * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$Initial_Deceleration_Follower <- x$Initial_Deceleration_Follower * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_ORD_DBS_Adaptation...")
  dbWriteTable(dbi_con, "tbl_ORD_DBS_Adaptation", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_ORD_DBS_Adaptation")
}

Populate_tbl_ORD_Wake_Adaptation <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Min_Safe_Landing_Speed_Lead <- x$Min_Safe_Landing_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Min_Safe_Landing_Speed_Follower <- x$Min_Safe_Landing_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Compression_Commencement_Threshold <- x$Compression_Commencement_Threshold * fnc_GI_Nm_To_M()
  x$Local_Stabilisation_Distance_Lead <- x$Local_Stabilisation_Distance_Lead * fnc_GI_Nm_To_M()
  x$Local_Stabilisation_Distance_Follower <- x$Local_Stabilisation_Distance_Follower * fnc_GI_Nm_To_M()
  x$Steady_Procedural_Speed_Lead <- x$Steady_Procedural_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Steady_Procedural_Speed_Follower <- x$Steady_Procedural_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Final_Deceleration_Lead <- x$Final_Deceleration_Lead * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$Final_Deceleration_Follower <- x$Final_Deceleration_Follower * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$End_Initial_Deceleration_Distance_Lead <- x$End_Initial_Deceleration_Distance_Lead * fnc_GI_Nm_To_M()
  x$End_Initial_Deceleration_Distance_Follower <- x$End_Initial_Deceleration_Distance_Follower * fnc_GI_Nm_To_M()
  x$Initial_Procedural_Speed_Lead <- x$Initial_Procedural_Speed_Lead * fnc_GI_Kts_To_M_Per_Sec()
  x$Initial_Procedural_Speed_Follower <- x$Initial_Procedural_Speed_Follower * fnc_GI_Kts_To_M_Per_Sec()
  x$Initial_Deceleration_Lead <- x$Initial_Deceleration_Lead * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$Initial_Deceleration_Follower <- x$Initial_Deceleration_Follower * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  
  if ("Aircraft_Type" %in% names(x)) setnames(x, "Aircraft_Type", "Wake_Cat")
  setnames(x, "Initial_Deceleration_Lead", "Initial_deceleration_Lead")
  setnames(x, "Initial_Deceleration_Follower", "Initial_deceleration_Follower")
  
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_ORD_Wake_Adaptation...")
  dbWriteTable(dbi_con, "tbl_ORD_Wake_Adaptation", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_ORD_Wake_Adaptation")
}

Populate_tbl_Reference_Recat_Separation_Dist <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Reference_Wake_Separation_Distance <- x$Reference_Wake_Separation_Distance * fnc_GI_Nm_To_M()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Reference_Recat_Separation_Dist...")
  dbWriteTable(dbi_con, "tbl_Reference_Recat_Separation_Dist", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Reference_Recat_Separation_Dist")
}

Populate_tbl_Reference_Recat_Separation_Time <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Reference_Recat_Separation_Time...")
  dbWriteTable(dbi_con, "tbl_Reference_Recat_Separation_Time", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Reference_Recat_Separation_Time")
}

Populate_tbl_Reference_ROT_Spacing_Dist <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Reference_ROT_Spacing_Distance <- x$Reference_ROT_Spacing_Distance * fnc_GI_Nm_To_M()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Reference_ROT_Spacing_Dist...")
  dbWriteTable(dbi_con, "tbl_Reference_ROT_Spacing_Dist", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Reference_ROT_Spacing_Dist")
}

Populate_tbl_Reference_ROT_Spacing_Time <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Reference_ROT_Spacing_Time...")
  dbWriteTable(dbi_con, "tbl_Reference_ROT_Spacing_Time", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Reference_ROT_Spacing_Time")
}

Populate_tbl_Reference_TBS_Table_Time <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  x$Reference_Wake_Separation_Distance <- x$Reference_Wake_Separation_Distance * fnc_GI_Nm_To_M()
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Reference_TBS_Table_Time...")
  dbWriteTable(dbi_con, "tbl_Reference_TBS_Table_Time", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Reference_TBS_Table_Time")
}

#configDir="C:\\Users\\Catherine\\Dropbox (Think Research)\\Think IA App\\Toronto_IA_Config_Dev_App_Test"
import_Config <- function(configDir, dbi_con, load_legacy_wake = T, load_DW_volumes = T) {
  # Important! The functions must be called in correct order due to dependencies!
  message("[",Sys.time(),"] ", "Import config from: ", configDir)
  Populate_tbl_Adaptation(list.files(configDir, ".*Populate_tbl_Adaptation_?D?a?t?a?.csv", full.names = T), dbi_con)
  Populate_tbl_Airfield(list.files(configDir, ".*Populate_tbl_Airfield.csv", full.names = T), dbi_con)
  Populate_tbl_Runway(list.files(configDir, ".*Populate_tbl_Runway.csv", full.names = T), dbi_con)
  Populate_tbl_Mode_S_Wind_Localiser_Capture(list.files(configDir, ".*Populate_tbl_Mode_S_Wind_Localiser_Capture.csv", full.names = T), dbi_con)
  Populate_Airspace_Volumes(list.files(configDir, ".*Populate_Airspace_Volumes.csv", full.names = T), dbi_con)
  if (load_DW_volumes) {
    Populate_Airspace_Volumes_DW(list.files(configDir, ".*Route_Fix.csv", full.names = T), list.files(configDir, ".*Route_Point.csv", full.names = T), dbi_con)
  }
  Populate_tbl_Mode_S_Wind_Adaptation(list.files(configDir, ".*Populate_tbl_Mode_S_Wind_Adaptation.csv", full.names = T), dbi_con)
  Populate_tbl_Path_Leg(list.files(configDir, ".*Populate_tbl_Path_Leg.csv", full.names = T), dbi_con)
  Populate_tbl_Path_Leg_Transition(list.files(configDir, ".*Populate_tbl_Path_Leg_Transition.csv", full.names = T), dbi_con)
  Populate_tbl_ORD_Runway_Adaptation(list.files(configDir, ".*Populate_tbl_ORD_Runway_Adaptation.csv", full.names = T), dbi_con)
  Populate_tbl_Aircraft_Type_To_Wake(list.files(configDir, "Populate_tbl_Aircraft_Type_To_Wake.*.csv", full.names = T) %>% .[. != file.path(configDir, "Populate_tbl_Aircraft_Type_To_Wake_Legacy.csv")], dbi_con)
  if (load_legacy_wake) {
    Populate_tbl_Aircraft_Type_To_Wake_Legacy(file.path(configDir, "Populate_tbl_Aircraft_Type_To_Wake_Legacy.csv"), dbi_con)
  }
  Populate_tbl_Assumed_Recat_Separation_IAS(list.files(configDir, "Populate_tbl_Assumed_Recat_Separation_IAS.*.csv", full.names = T), dbi_con)
  Populate_tbl_Assumed_ROT_Spacing_IAS(list.files(configDir, "Populate_tbl_Assumed_ROT_Spacing_IAS.*.csv", full.names = T), dbi_con)
  Populate_tbl_Assumed_TBS_Table_IAS(list.files(configDir, "Populate_tbl_Assumed_TBS_Table_IAS.*.csv", full.names = T), dbi_con)
  Populate_tbl_DBS_Wake_Turbulence(file.path(configDir, "Populate_tbl_DBS_Wake_Turbulence.csv"), dbi_con)
  Populate_tbl_Mode_S_Wind_Default_Wind_Effect_Segments(file.path(configDir, "Populate_tbl_Mode_S_Wind_Default_Wind_Effect_Segments.csv"), dbi_con)
  Populate_tbl_ORD_Aircraft_Adaptation(list.files(configDir, "Populate_tbl_ORD_Aircraft_Adaptation.*.csv", full.names = T), dbi_con)
  Populate_tbl_ORD_DBS_Adaptation(list.files(configDir, "Populate_tbl_ORD_DBS_Adaptation.*.csv", full.names = T), dbi_con)
  Populate_tbl_ORD_Wake_Adaptation(list.files(configDir, "Populate_tbl_ORD_Wake_Adaptation.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_Recat_Separation_Dist(list.files(configDir, "Populate_tbl_Reference_Recat_Separation_Dist.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_Recat_Separation_Time(list.files(configDir, "Populate_tbl_Reference_Recat_Separation_Time.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_ROT_Spacing_Dist(list.files(configDir, "Populate_tbl_Reference_ROT_Spacing_Dist.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_ROT_Spacing_Time(list.files(configDir, "Populate_tbl_Reference_ROT_Spacing_Time.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_TBS_Table_Time(list.files(configDir, "Populate_tbl_Reference_TBS_Table_Time.*.csv", full.names = T), dbi_con)
  dbSendQuery(dbi_con, read_SQL_File("modules/data_loader/Update_tbl_Path_Leg_Transition.sql"))
  Sys.sleep(1)
  message("[",Sys.time(),"] ", "Finished importing new configuration data.")
}

# ----------------------------------------------------------------------- #
# Export Config Functions -------------------------------------------------
# ----------------------------------------------------------------------- #

xml_airspace <- function(OutputPath, dbi_con) {
  
  # Redundant dependencies:
  # usp_GI_Get_Airfield_Data
  # usp_GI_Get_Adaptation_Data
  # usp_GI_Get_Area_Of_Interest_Data
  # usp_GI_Get_Runway_Group_Data
  # usp_GI_Get_Runway_Data
  # usp_GI_Get_Path_Leg_Data
  # usp_GI_Get_Volume_Data
  # usp_GI_Get_Polygon_Data
  # usp_GI_Get_Path_leg_Transition_Data
  
  tbl_Airfield <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield"))
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  tbl_Path_Leg <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Path_Leg"))
  tbl_Volume <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Volume"))
  tbl_Polygon <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Polygon"))
  tbl_Path_Leg_Transition <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Path_Leg_Transition"))
  
  area_of_interest <- data.table(
    Min_X = mean(tbl_Runway$Threshold_X_Pos) - tbl_Adaptation_Data$Load_X_Range[1],
    Max_X = mean(tbl_Runway$Threshold_X_Pos) + tbl_Adaptation_Data$Load_X_Range[1],
    Min_Y = mean(tbl_Runway$Threshold_Y_Pos) - tbl_Adaptation_Data$Load_Y_Range[1],
    Max_Y = mean(tbl_Runway$Threshold_Y_Pos) + tbl_Adaptation_Data$Load_Y_Range[1]
  )
  area_of_interest_min <- usp_GI_Latlong_From_XY(area_of_interest$Min_X, area_of_interest$Min_Y, tbl_Adaptation_Data)
  area_of_interest_max <- usp_GI_Latlong_From_XY(area_of_interest$Max_X, area_of_interest$Max_Y, tbl_Adaptation_Data)
  area_of_interest_list <- list(
    min_lat = round(area_of_interest_min$PositionLatitude / fnc_GI_Degs_To_Rads(), 6),
    max_lat = round(area_of_interest_max$PositionLatitude / fnc_GI_Degs_To_Rads(), 6),
    min_long = round(area_of_interest_min$PositionLongitude / fnc_GI_Degs_To_Rads(), 6),
    max_long = round(area_of_interest_max$PositionLongitude / fnc_GI_Degs_To_Rads(), 6),
    min_alt = round(tbl_Adaptation_Data$Load_Min_Alt / fnc_GI_Ft_To_M(), 2)[1],
    max_alt = round(tbl_Adaptation_Data$Load_Max_Alt / fnc_GI_Ft_To_M(), 2)[1]
  )
  
  runways <- tbl_Runway[Airfield_Name == tbl_Airfield$Airfield_Name[1] | is.na(Airfield_Name)][order(Runway_Name)]
  runways$Threshold_X_Pos <- as.integer(runways$Threshold_X_Pos)
  runways$Threshold_Y_Pos <- as.integer(runways$Threshold_Y_Pos)
  runways$Heading <- runways$Heading / fnc_GI_Degs_To_Rads()
  runways$Elevation <- runways$Elevation / fnc_GI_Ft_To_M()
  runways$Glideslope_Angle <- runways$Glideslope_Angle / fnc_GI_Degs_To_Rads()
  runways$Threshold_Lat <- runways$Threshold_Lat / fnc_GI_Degs_To_Rads()
  runways$Threshold_Lon <- runways$Threshold_Lon / fnc_GI_Degs_To_Rads()
  
  runway_groups <- sort(unique(runways$Runway_Group))
  runway_groups_list <- lapply(runway_groups, function(i) list(runway_group_name = i))
  names(runway_groups_list) <- rep("runway_group", length(runway_groups_list))
  
  runways_list <- lapply(1:nrow(runways), function(i) {
    runways_i <- runways[i]
    return(list(
      runway_name = runways_i$Runway_Name,
      threshold_position = list(
        local_x = runways_i$Threshold_X_Pos,
        local_y = runways_i$Threshold_Y_Pos
      ),
      runway_heading = round(runways_i$Heading, 2),
      runway_group = runways_i$Runway_Group,
      elevation = round(runways_i$Elevation, 2),
      touchdown_offset = runways_i$Touchdown_Offset,
      glideslope_angle = round(runways_i$Glideslope_Angle, 2)
    ))
  })
  names(runways_list) <- rep("runway", length(runways_list))
  
  path_legs <- tbl_Path_Leg[Airfield_Name == tbl_Airfield$Airfield_Name[1] | is.na(Airfield_Name)][order(Path_Leg_Name)]
  path_legs_list <- lapply(1:nrow(path_legs), function(i){
    path_legs_i <- path_legs[i]
    list_i <- list(path_leg_name = path_legs_i$Path_Leg_Name)
    if (!is.na(path_legs_i$Landing_Runway)) {
      list_i <- c(list_i, landing_runway = path_legs_i$Landing_Runway)
    }
    if (!is.na(path_legs_i$Path_Leg_Type)) {
      list_i <- c(list_i, path_leg_type = toupper(path_legs_i$Path_Leg_Type))
    }
    return(list_i)
  })
  names(path_legs_list) <- rep("path_leg", length(path_legs_list))
  
  volumes <- tbl_Volume[Runway_Name %in% runways$Runway_Name]
  volumes$Min_Altitude <- volumes$Min_Altitude / fnc_GI_Ft_To_M()
  volumes$Max_Altitude <- volumes$Max_Altitude / fnc_GI_Ft_To_M()
  
  polygons <- tbl_Polygon[Volume_Name %in% volumes$Volume_Name]
  polygons$Latitude <- as.numeric(polygons$Latitude) / fnc_GI_Degs_To_Rads()
  polygons$Longitude <- as.numeric(polygons$Longitude) / fnc_GI_Degs_To_Rads()
  
  volumes_list <- lapply(1:nrow(volumes), function(i) {
    volumes_i <- volumes[i]
    polygons_i <- polygons[Volume_Name == volumes_i$Volume_Name][order(Point_Sequence)]
    if (nrow(polygons_i) < 4) stop("Minimum 4 volume_points required!")
    list_i <- list(
      volume_name = volumes_i$Volume_Name,
      min_altitude = round(volumes_i$Min_Altitude, 2),
      max_altitude = round(volumes_i$Max_Altitude, 2)
    )
    for (j in 1:nrow(polygons_i)) {
      list_i <- append(list_i, list(volume_point = list(local_x = polygons_i$Point_X[j], local_y = polygons_i$Point_Y[j])))
    }
    return(list_i)
  })
  names(volumes_list) <- rep("volume", length(volumes_list))
  
  path_leg_transitions <- tbl_Path_Leg_Transition[Airfield_Name == tbl_Airfield$Airfield_Name[1] | is.na(Airfield_Name)][order(PLT_ID)]
  path_leg_transitions_list <- lapply(1:nrow(path_leg_transitions), function(i) {
    path_leg_transitions_i <- path_leg_transitions[i]
    list_i <- list()
    if (!is.na(path_leg_transitions_i$Current_Path_Leg)) {
      list_i <- c(list_i, current_path_leg = path_leg_transitions_i$Current_Path_Leg)
    }
    if (!is.na(path_leg_transitions_i$New_Path_Leg)) {
      list_i <- c(list_i, new_path_leg = path_leg_transitions_i$New_Path_Leg)
    }
    if (!is.na(path_leg_transitions_i$Min_Heading) & !is.na(path_leg_transitions_i$Max_Heading)) {
      list_i <- append(list_i, list(heading = list(
        from = round(path_leg_transitions_i$Min_Heading / fnc_GI_Degs_To_Rads(), 2),
        to = round(path_leg_transitions_i$Max_Heading / fnc_GI_Degs_To_Rads(), 2)
      )))
    }
    if (!is.na(path_leg_transitions_i$Volume_Name)) {
      list_i <- c(list_i, volume_name = path_leg_transitions_i$Volume_Name)
    }
    if (!is.na(path_leg_transitions_i$Runway_Name)) {
      list_i <- c(list_i, landing_runway = path_leg_transitions_i$Runway_Name)
    } else if (!is.na(path_leg_transitions_i$Difference_Runway)) {
      list_i <- c(list_i, difference_runway = path_leg_transitions_i$Difference_Runway)
    }
    if (!is.na(path_leg_transitions_i$Min_Sustained_RoCD)) {
      list_i <- c(list_i, min_sustained_rocd = round(path_leg_transitions_i$Min_Sustained_RoCD / fnc_GI_Ft_Per_Min_To_M_Per_Sec(), 2))
    }
    return(list_i)
  })
  names(path_leg_transitions_list) <- rep("path_leg_transition", length(path_leg_transitions_list))
  
  out_list <- list(
    `ia:airspace` = list(
      plt_diff_mode_s_to_radar_track = round(tbl_Adaptation_Data$Diff_Mode_S_To_Radar_Track_Max[1] / fnc_GI_Degs_To_Rads(), 0),
      plt_max_mode_s_data_age = tbl_Adaptation_Data$Max_Mode_S_Data_Age[1],
      area_of_interest = area_of_interest_list,
      runway_groups = runway_groups_list,
      runways = runways_list,
      path_legs = path_legs_list,
      volumes = volumes_list,
      path_leg_transitions = path_leg_transitions_list
    ),
    `ia:airspace.attr` = list(
      `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
      `xsi:noNamespaceSchemaLocation` = "AirspaceAdaptation.xsd"
    )
  )
  
  out <- data.table(c(
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>",
    "<!-- _________________________________________________________________ -->",
    "<!-- NATS IA Tool TDPS Adaptation File from Think                      -->",
    "<!-- Format: AirspaceAdaptation.xsd                                    -->",
    "<!-- _________________________________________________________________ -->",
    List_To_XML(out_list)
  ))
  
  fwrite(out, file = OutputPath, col.names = F, quote = F)
  
}

xml_gwcs <- function(OutputPath, dbi_con) {
  
  # Redundant dependencies:
  # usp_GI_Get_Mode_S_Wind_Adaptation_Data
  # usp_GI_Get_Adaptation_Data
  # usp_GI_Get_Mode_S_Wind_Localiser_Capture_Data
  
  tbl_Mode_S_Wind_Adaptation <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Mode_S_Wind_Adaptation"))
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
    
  gwcs_localiser_captures <- as.data.table(dbGetQuery(dbi_con, "
    SELECT * FROM tbl_Mode_S_Wind_Localiser_Capture
    WHERE Runway_Name IN (
      SELECT Runway_Name FROM tbl_Runway
      WHERE Airfield_Name IS NULL
      OR Airfield_Name = (SELECT Airfield_Name FROM tbl_Airfield)
    )                              
    ORDER BY Runway_Name
  "))
  gwcs_localiser_captures_list <- lapply(1:nrow(gwcs_localiser_captures), function(i) {
    gwcs_localiser_captures_i <- gwcs_localiser_captures[i]
    return(list(
      runway_name = gwcs_localiser_captures_i$Runway_Name,
      volume_name = gwcs_localiser_captures_i$Volume_Name,
      heading = list(
        from = round(gwcs_localiser_captures_i$Min_Heading / fnc_GI_Degs_To_Rads(), 0),
        to = round(gwcs_localiser_captures_i$Max_Heading / fnc_GI_Degs_To_Rads(), 0)
      )
    ))
  })
  names(gwcs_localiser_captures_list) <- rep("gwcs_localiser_capture", length(gwcs_localiser_captures_list))
  
  out_list <- list(
    `ia:gwcs` = list(
      mode_s_gspd_min = round(tbl_Mode_S_Wind_Adaptation$Mode_S_GSPD_Min / fnc_GI_Kts_To_M_Per_Sec(), 2),
      mode_s_gspd_max = round(tbl_Mode_S_Wind_Adaptation$Mode_S_GSPD_Max / fnc_GI_Kts_To_M_Per_Sec(), 2),
      mode_s_ias_min = round(tbl_Mode_S_Wind_Adaptation$Mode_S_IAS_Min / fnc_GI_Kts_To_M_Per_Sec(), 2),
      mode_s_ias_max = round(tbl_Mode_S_Wind_Adaptation$Mode_S_IAS_Max / fnc_GI_Kts_To_M_Per_Sec(), 2),
      mode_s_tas_min = round(tbl_Mode_S_Wind_Adaptation$Mode_S_TAS_Min / fnc_GI_Kts_To_M_Per_Sec(), 2),
      mode_s_tas_max = round(tbl_Mode_S_Wind_Adaptation$Mode_S_TAS_Max / fnc_GI_Kts_To_M_Per_Sec(), 2),
      mode_s_roll_angle_max = round(tbl_Mode_S_Wind_Adaptation$Mode_S_Roll_Angle_Max / fnc_GI_Degs_To_Rads(), 2),
      extrapolation_seg_min = round(tbl_Mode_S_Wind_Adaptation$Extrapolation_Seg_Min / fnc_GI_Nm_To_M(), 2),
      range_to_threshold_min = round(tbl_Mode_S_Wind_Adaptation$DME_Seg_Min / fnc_GI_Nm_To_M(), 2),
      range_to_threshold_max = round(tbl_Mode_S_Wind_Adaptation$DME_Seg_Max / fnc_GI_Nm_To_M(), 2),
      wind_segment_size = round(tbl_Mode_S_Wind_Adaptation$DME_Seg_Size / fnc_GI_Nm_To_M(), 2),
      altitude_tolerance = round(tbl_Mode_S_Wind_Adaptation$Altitude_Tolerance / fnc_GI_Ft_To_M(), 2),
      seg_duration_min = tbl_Mode_S_Wind_Adaptation$Seg_Duration_Min,
      seg_duration_max = tbl_Mode_S_Wind_Adaptation$Seg_Duration_Max,
      seg_diff_track_to_runway_hdg_max = round(tbl_Mode_S_Wind_Adaptation$Diff_Track_To_Runway_HDG_Max / fnc_GI_Degs_To_Rads(), 2),
      seg_diff_hdg_to_runway_hdg_max = round(tbl_Mode_S_Wind_Adaptation$Diff_HDG_To_Runway_HDG_Max / fnc_GI_Degs_To_Rads(), 2),
      seg_diff_mode_s_to_radar_track_max = round(tbl_Mode_S_Wind_Adaptation$Diff_Mode_S_To_Radar_Track_Max / fnc_GI_Degs_To_Rads(), 2),
      seg_diff_mode_s_to_radar_gspd_max = round(tbl_Mode_S_Wind_Adaptation$Diff_Mode_S_To_Radar_GSPD_Max / fnc_GI_Kts_To_M_Per_Sec(), 2),
      seg_max_wind_effect = round(tbl_Mode_S_Wind_Adaptation$Max_Wind_Effect / fnc_GI_Kts_To_M_Per_Sec(), 2),
      seg_max_wind_spd = round(tbl_Mode_S_Wind_Adaptation$Max_Wind_SPD / fnc_GI_Kts_To_M_Per_Sec(), 2),
      forecast_valid_seg_min = round(tbl_Mode_S_Wind_Adaptation$Forecast_Seg_Min / fnc_GI_Nm_To_M(), 2),
      forecast_valid_seg_max = round(tbl_Mode_S_Wind_Adaptation$Forecast_Seg_Max / fnc_GI_Nm_To_M(), 2),
      max_segment_extrapolation = tbl_Mode_S_Wind_Adaptation$Max_Seg_Extrapolation,
      separation_forecast_seg_max = tbl_Mode_S_Wind_Adaptation$Separation_Forecast_Seg_Max / fnc_GI_Nm_To_M(),
      forecast_stale_time = tbl_Mode_S_Wind_Adaptation$Forecast_Stale_Time,
      bps_min = tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Min / fnc_GI_Mbar_To_Pa(),
      bps_max = tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Max / fnc_GI_Mbar_To_Pa(),
      bps_rtt_min = round(tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_DME_Min / fnc_GI_Nm_To_M(), 2),
      bps_rtt_max = round(tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_DME_Max / fnc_GI_Nm_To_M(), 2),
      max_rtt_null_derived_qnh = round(tbl_Mode_S_Wind_Adaptation$Max_RTT_Null_Derived_QNH / fnc_GI_Nm_To_M(), 2),
      bps_delta_max = tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Delta_Max / fnc_GI_Mbar_To_Pa(),
      bps_delta_check = tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Delta_Check / fnc_GI_Mbar_To_Pa(),
      bps_stale_time = tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Stale_Time,
      bps_altitude_diff_max = round(tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Alt_Diff_Max / fnc_GI_Ft_To_M(), 2),
      gwcs_bps_stable_confirm = tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Stability_Count,
      gwcs_bps_update_max_time = tbl_Mode_S_Wind_Adaptation$Mode_S_BPS_Update_Max_Time,
      mag_var = round(tbl_Adaptation_Data$Mag_Var / fnc_GI_Degs_To_Rads(), 2),
      max_mode_s_data_age = tbl_Adaptation_Data$Max_Mode_S_Data_Age,
      gwcs_localiser_captures = gwcs_localiser_captures_list
    ),
    `ia:gwcs.attr` = list(
      `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
      `xsi:noNamespaceSchemaLocation` = "GwcsAdaptation.xsd"
    )
  )
  
  out <- data.table(c(
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>",
    "<!-- _________________________________________________________________ -->",
    "<!-- NATS IA Tool TDPS Adaptation File from Think                      -->",
    "<!-- Format: GwcsAdaptation.xsd                                        -->",
    "<!-- _________________________________________________________________ -->",
    List_To_XML(out_list)
  ))
  
  fwrite(out, file = OutputPath, col.names = F, quote = F)
  
}

xml_ord <- function(OutputPath, dbi_con) {
  
  # Redundant dependencies:
  # usp_GI_Get_ORD_Runway_Adaptation_Data
  # usp_GI_Get_ORD_Wake_Adaptation_Data
  # usp_GI_Get_ORD_Aircraft_Adaptation_Data
  # usp_GI_Get_ORD_DBS_Adaptation_Data
  # usp_GI_Get_Mode_S_Wind_Default_Wind_Effect_Segments
  
  tbl_ORD_Wake_Adaptation <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_ORD_Wake_Adaptation ORDER BY Wake_Cat"))
  tbl_ORD_Aircraft_Adaptation <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_ORD_Aircraft_Adaptation ORDER BY Aircraft_Type"))
  tbl_ORD_DBS_Adaptation <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_ORD_DBS_Adaptation ORDER BY DBS_Distance"))
  tbl_Mode_S_Wind_Default_Wind_Effect_Segments <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Mode_S_Wind_Default_Wind_Effect_Segments ORDER BY Wind_Segment_Start"))
  
  ord_runways <- as.data.table(dbGetQuery(dbi_con, "
    SELECT * FROM tbl_ORD_Runway_Adaptation
    WHERE Runway_Name IN (
      SELECT Runway_Name FROM tbl_Runway
      WHERE Airfield_Name IS NULL
      OR Airfield_Name = (SELECT Airfield_Name FROM tbl_Airfield)
    )                              
    ORDER BY Runway_Name
  "))
  ord_runways_list <- lapply(1:nrow(ord_runways), function(i) {
    ord_runways_i <- ord_runways[i]
    return(list(
      runway_name = ord_runways_i$Runway_Name,
      max_dtt = round(ord_runways_i$Max_DTT / fnc_GI_Nm_To_M(), 2),
      four_hundred_ft_aal = round(ord_runways_i$Four_Hundred_Ft_AAL / fnc_GI_Nm_To_M(), 2),
      six_hundred_ft_aal = round(ord_runways_i$Six_Hundred_Ft_AAL / fnc_GI_Nm_To_M(), 2),
      thousand_ft_gate = round(ord_runways_i$Thousand_Ft_Gate / fnc_GI_Nm_To_M(), 2),
      gust_adjustment = round(ord_runways_i$Gust_Adjustment / fnc_GI_Kts_To_M_Per_Sec(), 2)
    ))
  })
  names(ord_runways_list) <- rep("ord_runway", length(ord_runways_list))
  
  ord_wake_categories_list <- lapply(1:nrow(tbl_ORD_Wake_Adaptation), function(i) {
    tbl_ORD_Wake_Adaptation_i <- tbl_ORD_Wake_Adaptation[i]
    return(list(
      wake_category = tbl_ORD_Wake_Adaptation_i$Wake_Cat,
      compression_commencement_threshold = round(tbl_ORD_Wake_Adaptation_i$Compression_Commencement_Threshold / fnc_GI_Nm_To_M(), 2),
      lead = list(
        landing_stabilisation_speed_type = tbl_ORD_Wake_Adaptation_i$Landing_Stabilisation_Speed_Type_Lead,
        minimum_safe_landing_speed = round(tbl_ORD_Wake_Adaptation_i$Min_Safe_Landing_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        apply_gusting = tbl_ORD_Wake_Adaptation_i$Apply_Gusting_Lead,
        local_stabilisation_threshold = round(tbl_ORD_Wake_Adaptation_i$Local_Stabilisation_Distance_Lead / fnc_GI_Nm_To_M(), 2),
        initial_deceleration = round(tbl_ORD_Wake_Adaptation_i$Initial_deceleration_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        end_initial_decel = round(tbl_ORD_Wake_Adaptation_i$End_Initial_Deceleration_Distance_Lead / fnc_GI_Nm_To_M(), 2),
        initial_procedural_speed = round(tbl_ORD_Wake_Adaptation_i$Initial_Procedural_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        steady_procedural_speed = round(tbl_ORD_Wake_Adaptation_i$Steady_Procedural_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        final_deceleration = round(tbl_ORD_Wake_Adaptation_i$Final_Deceleration_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2)
      ),
      foll = list(
        landing_stabilisation_speed_type = tbl_ORD_Wake_Adaptation_i$Landing_Stabilisation_Speed_Type_Follower,
        minimum_safe_landing_speed = round(tbl_ORD_Wake_Adaptation_i$Min_Safe_Landing_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        apply_gusting = tbl_ORD_Wake_Adaptation_i$Apply_Gusting_Follower,
        local_stabilisation_threshold = round(tbl_ORD_Wake_Adaptation_i$Local_Stabilisation_Distance_Follower / fnc_GI_Nm_To_M(), 2),
        initial_deceleration = round(tbl_ORD_Wake_Adaptation_i$Initial_deceleration_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        end_initial_decel = round(tbl_ORD_Wake_Adaptation_i$End_Initial_Deceleration_Distance_Follower / fnc_GI_Nm_To_M(), 2),
        initial_procedural_speed = round(tbl_ORD_Wake_Adaptation_i$Initial_Procedural_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        steady_procedural_speed = round(tbl_ORD_Wake_Adaptation_i$Steady_Procedural_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        final_deceleration = round(tbl_ORD_Wake_Adaptation_i$Final_Deceleration_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2)
      )
    ))
  })
  names(ord_wake_categories_list) <- rep("ord_wake_category", length(ord_wake_categories_list))
  
  ord_aircraft_types_list <- lapply(1:nrow(tbl_ORD_Aircraft_Adaptation), function(i) {
    tbl_ORD_Aircraft_Adaptation_i <- tbl_ORD_Aircraft_Adaptation[i]
    return(list(
      ac_type = tbl_ORD_Aircraft_Adaptation_i$Aircraft_Type,
      compression_commencement_threshold = round(tbl_ORD_Aircraft_Adaptation_i$Compression_Commencement_Threshold / fnc_GI_Nm_To_M(), 2),
      lead = list(
        landing_stabilisation_speed_type = tbl_ORD_Aircraft_Adaptation_i$Landing_Stabilisation_Speed_Type_Lead,
        minimum_safe_landing_speed = round(tbl_ORD_Aircraft_Adaptation_i$Min_Safe_Landing_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        apply_gusting = tbl_ORD_Aircraft_Adaptation_i$Apply_Gusting_Lead,
        local_stabilisation_threshold = round(tbl_ORD_Aircraft_Adaptation_i$Local_Stabilisation_Distance_Lead / fnc_GI_Nm_To_M(), 2),
        initial_deceleration = round(tbl_ORD_Aircraft_Adaptation_i$Initial_deceleration_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        end_initial_decel = round(tbl_ORD_Aircraft_Adaptation_i$End_Initial_Deceleration_Distance_Lead / fnc_GI_Nm_To_M(), 2),
        initial_procedural_speed = round(tbl_ORD_Aircraft_Adaptation_i$Initial_Procedural_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        steady_procedural_speed = round(tbl_ORD_Aircraft_Adaptation_i$Steady_Procedural_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        final_deceleration = round(tbl_ORD_Aircraft_Adaptation_i$Final_Deceleration_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2)
      ),
      foll = list(
        landing_stabilisation_speed_type = tbl_ORD_Aircraft_Adaptation_i$Landing_Stabilisation_Speed_Type_Follower,
        minimum_safe_landing_speed = round(tbl_ORD_Aircraft_Adaptation_i$Min_Safe_Landing_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        apply_gusting = tbl_ORD_Aircraft_Adaptation_i$Apply_Gusting_Follower,
        local_stabilisation_threshold = round(tbl_ORD_Aircraft_Adaptation_i$Local_Stabilisation_Distance_Follower / fnc_GI_Nm_To_M(), 2),
        initial_deceleration = round(tbl_ORD_Aircraft_Adaptation_i$Initial_deceleration_follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        end_initial_decel = round(tbl_ORD_Aircraft_Adaptation_i$End_Initial_Deceleration_Distance_Follower / fnc_GI_Nm_To_M(), 2),
        initial_procedural_speed = round(tbl_ORD_Aircraft_Adaptation_i$Initial_Procedural_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        steady_procedural_speed = round(tbl_ORD_Aircraft_Adaptation_i$Steady_Procedural_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        final_deceleration = round(tbl_ORD_Aircraft_Adaptation_i$Final_Deceleration_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2)
      )
    ))
  })
  names(ord_aircraft_types_list) <- rep("ord_aircraft_type", length(ord_aircraft_types_list))
  
  ord_tbs_table_values_list <- lapply(1:nrow(tbl_ORD_DBS_Adaptation), function(i) {
    tbl_ORD_DBS_Adaptation_i <- tbl_ORD_DBS_Adaptation[i]
    return(list(
      dbs_distance_value = tbl_ORD_DBS_Adaptation_i$DBS_Distance,
      compression_commencement_threshold = round(tbl_ORD_DBS_Adaptation_i$Compression_Commencement_Threshold / fnc_GI_Nm_To_M(), 2),
      lead = list(
        landing_stabilisation_speed_type = tbl_ORD_DBS_Adaptation_i$Landing_Stabilisation_Speed_Type_Lead,
        minimum_safe_landing_speed = round(tbl_ORD_DBS_Adaptation_i$Min_Safe_Landing_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        apply_gusting = tbl_ORD_DBS_Adaptation_i$Apply_Gusting_Lead,
        local_stabilisation_threshold = round(tbl_ORD_DBS_Adaptation_i$Local_Stabilisation_Distance_Lead / fnc_GI_Nm_To_M(), 2),
        initial_deceleration = round(tbl_ORD_DBS_Adaptation_i$Initial_Deceleration_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        end_initial_decel = round(tbl_ORD_DBS_Adaptation_i$End_Initial_Deceleration_Distance_Lead / fnc_GI_Nm_To_M(), 2),
        initial_procedural_speed = round(tbl_ORD_DBS_Adaptation_i$Initial_Procedural_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        steady_procedural_speed = round(tbl_ORD_DBS_Adaptation_i$Steady_Procedural_Speed_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2),
        final_deceleration = round(tbl_ORD_DBS_Adaptation_i$Final_Deceleration_Lead / fnc_GI_Kts_To_M_Per_Sec(), 2)
      ),
      foll = list(
        landing_stabilisation_speed_type = tbl_ORD_DBS_Adaptation_i$Landing_Stabilisation_Speed_Type_Follower,
        minimum_safe_landing_speed = round(tbl_ORD_DBS_Adaptation_i$Min_Safe_Landing_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        apply_gusting = tbl_ORD_DBS_Adaptation_i$Apply_Gusting_Follower,
        local_stabilisation_threshold = round(tbl_ORD_DBS_Adaptation_i$Local_Stabilisation_Distance_Follower / fnc_GI_Nm_To_M(), 2),
        initial_deceleration = round(tbl_ORD_DBS_Adaptation_i$Initial_Deceleration_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        end_initial_decel = round(tbl_ORD_DBS_Adaptation_i$End_Initial_Deceleration_Distance_Follower / fnc_GI_Nm_To_M(), 2),
        initial_procedural_speed = round(tbl_ORD_DBS_Adaptation_i$Initial_Procedural_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        steady_procedural_speed = round(tbl_ORD_DBS_Adaptation_i$Steady_Procedural_Speed_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2),
        final_deceleration = round(tbl_ORD_DBS_Adaptation_i$Final_Deceleration_Follower / fnc_GI_Kts_To_M_Per_Sec(), 2)
      )
    ))
  })
  names(ord_tbs_table_values_list) <- rep("ord_tbs_table_value", length(ord_tbs_table_values_list))
  
  wind_effect_segment_entries_list <- lapply(1:nrow(tbl_Mode_S_Wind_Default_Wind_Effect_Segments), function(i) {
    tbl_Mode_S_Wind_Default_Wind_Effect_Segments_i <- tbl_Mode_S_Wind_Default_Wind_Effect_Segments[i]
    return(list(
      wind_segment_start = round(tbl_Mode_S_Wind_Default_Wind_Effect_Segments_i$Wind_Segment_Start / fnc_GI_Nm_To_M(), 2),
      wind_segment_end = round(tbl_Mode_S_Wind_Default_Wind_Effect_Segments_i$Wind_Segment_End / fnc_GI_Nm_To_M(), 2),
      wind_effect = round(tbl_Mode_S_Wind_Default_Wind_Effect_Segments_i$Wind_Effect / fnc_GI_Kts_To_M_Per_Sec(), 2)
    ))
  })
  names(wind_effect_segment_entries_list) <- rep("wind_effect_segment_entry", length(wind_effect_segment_entries_list))
  
  out_list <- list(
    `ia:ord` = list(
      ord_runways = ord_runways_list,
      ord_wake_categories = ord_wake_categories_list,
      ord_aircraft_types = ord_aircraft_types_list,
      ord_tbs_table_values = ord_tbs_table_values_list,
      wind_effect_segment_entries = wind_effect_segment_entries_list
    ),
    `ia:ord.attr` = list(
      `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
      `xsi:noNamespaceSchemaLocation` = "OrdAdaptation.xsd"
    )
  )
  
  out <- data.table(c(
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>",
    "<!-- _________________________________________________________________ -->",
    "<!-- NATS IA Tool TDPS Adaptation File from Think                      -->",
    "<!-- Format: OrdAdaptation.xsd                                         -->",
    "<!-- _________________________________________________________________ -->",
    List_To_XML(out_list)
  ))
  
  fwrite(out, file = OutputPath, col.names = F, quote = F)
  
}

xml_sasai <- function(OutputPath, dbi_con) {
  
  # Redundant dependencies:
  # usp_GI_Get_Airfield_Data
  # usp_GI_Get_Runway_Data
  # usp_GI_Get_Reference_ROT_Spacing_Time_Data
  # usp_GI_Get_Assumed_ROT_Spacing_IAS_Data
  # usp_GI_Get_Reference_ROT_Spacing_Dist_Data
  # usp_GI_Get_Assumed_Recat_Separation_IAS_Data
  # usp_GI_Get_Reference_Recat_Separation_Time_Data
  # usp_GI_Get_Reference_Recat_Separation_Dist_Data
  # usp_GI_Get_Reference_TBS_Table_Time_Data
  # usp_GI_Get_Assumed_TBS_Table_IAS_Data
  
  tbl_Airfield <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield"))
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  
  tbl_Reference_ROT_Spacing_Time <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Reference_ROT_Spacing_Time"))
  tbl_Assumed_ROT_Spacing_IAS <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Assumed_ROT_Spacing_IAS"))
  tbl_Reference_ROT_Spacing_Dist <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Reference_ROT_Spacing_Dist"))
  tbl_Assumed_Recat_Separation_IAS <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Assumed_Recat_Separation_IAS"))
  
  tbl_Reference_Recat_Separation_Time <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Reference_Recat_Separation_Time"))
  tbl_Reference_Recat_Separation_Dist <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Reference_Recat_Separation_Dist"))
  
  tbl_Reference_TBS_Table_Time <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Reference_TBS_Table_Time"))
  tbl_Assumed_TBS_Table_IAS <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Assumed_TBS_Table_IAS"))
  
  runway_wvc_pair <- merge(
    tbl_Assumed_Recat_Separation_IAS,
    merge(
      tbl_Reference_ROT_Spacing_Time, 
      merge(
        tbl_Assumed_ROT_Spacing_IAS,
        tbl_Reference_ROT_Spacing_Dist,
        by = c("Runway", "Leader_WTC", "Follower_WTC"),
        all = T
      ),
      by = c("Runway", "Leader_WTC", "Follower_WTC"),
      all = T
    ),
    by = c("Leader_WTC", "Follower_WTC"),
    all = T
  )[Runway %in% tbl_Runway[Airfield_Name == tbl_Airfield$Airfield_Name[1] | is.na(Airfield_Name)]$Runway_Name][order(Runway, Leader_WTC, Follower_WTC)]
  runway_wvc_pair$Assumed_ROT_Spacing_IAS <- runway_wvc_pair$Assumed_ROT_Spacing_IAS / fnc_GI_Kts_To_M_Per_Sec()
  runway_wvc_pair$Reference_ROT_Spacing_Distance <- runway_wvc_pair$Reference_ROT_Spacing_Distance / fnc_GI_Nm_To_M()
  runway_wvc_pair$Assumed_Wake_Separation_IAS <- runway_wvc_pair$Assumed_Wake_Separation_IAS / fnc_GI_Kts_To_M_Per_Sec()
  
  runway_rules_list <- lapply(unique(runway_wvc_pair$Runway), function(rwy) {
    
    runway_wvc_pair_rwy <- runway_wvc_pair[Runway == rwy]
    
    runway_wvc_pair_rules_rwy <- lapply(1:nrow(runway_wvc_pair_rwy), function(i) {
      
      runway_wvc_pair_rules_rwy_i <- runway_wvc_pair_rwy[i]
      
      list_i <- list(
        leader_wt = runway_wvc_pair_rules_rwy_i$Leader_WTC,
        follower_wt = runway_wvc_pair_rules_rwy_i$Follower_WTC
      )
      if (!is.na(runway_wvc_pair_rules_rwy_i$Assumed_Wake_Separation_IAS)) {
        list_i <- c(list_i, wake_ias = round(runway_wvc_pair_rules_rwy_i$Assumed_Wake_Separation_IAS, 0))
      }
      if (!is.na(runway_wvc_pair_rules_rwy_i$Assumed_ROT_Spacing_IAS)) {
        list_i <- c(list_i, rot_ias = round(runway_wvc_pair_rules_rwy_i$Assumed_ROT_Spacing_IAS, 0))
      }
      if (!is.na(runway_wvc_pair_rules_rwy_i$Reference_ROT_Spacing_Time)) {
        list_i <- c(list_i, rot_spacing_time = runway_wvc_pair_rules_rwy_i$Reference_ROT_Spacing_Time)
      }
      if (!is.na(runway_wvc_pair_rules_rwy_i$Reference_ROT_Spacing_Distance)) {
        list_i <- c(list_i, rot_spacing_distance = format(round(runway_wvc_pair_rules_rwy_i$Reference_ROT_Spacing_Distance, 1), nsmall = 1))
      }
      
      return(list_i)
    })
    names(runway_wvc_pair_rules_rwy) <- rep("runway_wvc_pair_rule", length(runway_wvc_pair_rules_rwy))
    
    return(list(
      runway_name = rwy,
      runway_wvc_pair_rules = runway_wvc_pair_rules_rwy
    ))
    
  })
  names(runway_rules_list) <- rep("runway_rule", length(runway_rules_list))
  
  wvc_pair <- merge(
    tbl_Reference_Recat_Separation_Time,
    tbl_Reference_Recat_Separation_Dist,
    by = c("Leader_WTC", "Follower_WTC"),
    all = T
  )[order(Leader_WTC, Follower_WTC)]
  wvc_pair$Reference_Wake_Separation_Distance <- wvc_pair$Reference_Wake_Separation_Distance / fnc_GI_Nm_To_M()
  
  wvc_pair_rules_list <- lapply(1:nrow(wvc_pair), function(i) {
    wvc_pair_i <- wvc_pair[i]
    return(list(
      leader_wt = wvc_pair_i$Leader_WTC,
      follower_wt = wvc_pair_i$Follower_WTC,
      wake_separation_time = wvc_pair_i$Reference_Wake_Separation_Time,
      wake_separation_distance = format(round(wvc_pair_i$Reference_Wake_Separation_Distance, 1), nsmall = 1)
    ))
  })
  names(wvc_pair_rules_list) <- rep("wvc_pair_rule", length(wvc_pair_rules_list))
  
  tbs_reference_table <- merge(
    tbl_Reference_TBS_Table_Time,
    tbl_Assumed_TBS_Table_IAS,
    by = "Reference_Wake_Separation_Distance",
    all = T
  )[order(Reference_Wake_Separation_Distance)]
  tbs_reference_table$Reference_Wake_Separation_Distance <- tbs_reference_table$Reference_Wake_Separation_Distance / fnc_GI_Nm_To_M()
  tbs_reference_table$Assumed_Wake_Separation_IAS <- tbs_reference_table$Assumed_Wake_Separation_IAS / fnc_GI_Kts_To_M_Per_Sec()
  
  tbs_reference_table_list <- lapply(1:nrow(tbs_reference_table), function(i) {
    tbs_reference_table_i <- tbs_reference_table[i]
    return(list(
      dbs = tbs_reference_table_i$Reference_Wake_Separation_Distance,
      tbs = tbs_reference_table_i$Reference_Wake_Separation_Time,
      assumed_ias = round(tbs_reference_table_i$Assumed_Wake_Separation_IAS, 0)
    ))
  })
  names(tbs_reference_table_list) <- rep("tbs_reference_table_entry", length(tbs_reference_table_list))
  
  out_list <- list(
    `ia:sasai` = list(
      runway_rules = runway_rules_list,
      wvc_pair_rules = wvc_pair_rules_list,
      tbs_reference_table = tbs_reference_table_list
    ),
    `ia:sasai.attr` = list(
      `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
      `xsi:noNamespaceSchemaLocation` = "SasaiAdaptation.xsd"
    )
  )
  
  out <- data.table(c(
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>",
    "<!-- _________________________________________________________________ -->",
    "<!-- NATS IA Tool TDPS Adaptation File from Think                      -->",
    "<!-- Format: SasaiAdaptation.xsd                                       -->",
    "<!-- _________________________________________________________________ -->",
    List_To_XML(out_list)
  ))
  
  fwrite(out, file = OutputPath, col.names = F, quote = F)
  
}

xml_tdps <- function(OutputPath, dbi_con) {
  
  # Redundant dependencies:
  # usp_GI_Get_Adaptation_Data
  
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  
  out_list <- list(
    `ia:tdps` = list(
      intermediate_projection_origin_lat = round(tbl_Adaptation_Data$Grid_Projection_Origin_Lat / fnc_GI_Degs_To_Rads(), 6),
      intermediate_projection_origin_long = round(tbl_Adaptation_Data$Grid_Projection_Origin_Lon / fnc_GI_Degs_To_Rads(), 6),
      false_northing = tbl_Adaptation_Data$Grid_Offset_X,
      false_easting = tbl_Adaptation_Data$Grid_Offset_Y
    ),
    `ia:tdps.attr` = list(
      `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance",
      `xsi:noNamespaceSchemaLocation` = "TdpsAdaptation.xsd"
    )
  )
  
  out <- data.table(c(
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>",
    "<!-- _________________________________________________________________ -->",
    "<!-- NATS IA Tool TDPS Adaptation File from Think                      -->",
    "<!-- Format: TdpsAdaptation.xsd                                        -->",
    "<!-- _________________________________________________________________ -->",
    List_To_XML(out_list)
  ))
  
  fwrite(out, file = OutputPath, col.names = F, quote = F)
  
}

export_Config <- function(configDir, dbi_con, ver_str = "V0.0.0") {
  
  message("[",Sys.time(),"] ", "Exporting config XML to: ", configDir)
  Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
  
  xml_airspace(file.path(configDir, paste0("airspace-", Airfield_Name, "-", ver_str, ".xml")), dbi_con)
  xml_gwcs(file.path(configDir, paste0("gwcs-", Airfield_Name, "-", ver_str, ".xml")), dbi_con)
  xml_ord(file.path(configDir, paste0("ord-", Airfield_Name, "-", ver_str, ".xml")), dbi_con)
  xml_sasai(file.path(configDir, paste0("sasai-", Airfield_Name, "-", ver_str, ".xml")), dbi_con)
  xml_tdps(file.path(configDir, paste0("tdps-", Airfield_Name, "-", ver_str, ".xml")), dbi_con)
  
  message("[",Sys.time(),"] ", "Finished exporting configuration XML files.")
  
}
