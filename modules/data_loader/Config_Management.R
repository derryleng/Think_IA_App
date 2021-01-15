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
  
  x$Threshold_Lat <- as.numeric(x$Threshold_Lat) * fnc_GI_Degs_To_Rads()
  x$Threshold_Lon <- as.numeric(x$Threshold_Lon) * fnc_GI_Degs_To_Rads()
  
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
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
  x <- fread(LogFilePath, header = F)
  
  volumes_names <- as.character(x[V1 == "Volume"][1])
  volumes <- x[V1 == "Volume"][-1]
  names(volumes) <- volumes_names
  volumes$Min_Altitude <- as.numeric(volumes$Min_Altitude) * fnc_GI_Ft_To_M()
  volumes$Max_Altitude <- as.numeric(volumes$Max_Altitude) * fnc_GI_Ft_To_M()
  
  message("[",Sys.time(),"] ", "Appending ", nrow(volumes), " rows to tbl_Volume...")
  dbWriteTable(dbi_con, "tbl_Volume", volumes, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(volumes), " rows to tbl_Volume")
  
  polygons_names <- as.character(x[V1 == "Point"][1])
  polygons <- x[V1 == "Point"][-1]
  names(polygons) <- polygons_names
  polygons$Airfield_Name <- as.vector(unlist(dbGetQuery(dbi_con, "SELECT * FROM tbl_Airfield")$Airfield_Name))
  polygons$Point_X <- as.numeric(polygons$Point_X) * fnc_GI_Nm_To_M()
  polygons$Point_Y <- as.numeric(polygons$Point_Y) * fnc_GI_Nm_To_M()
  polygons$Runway_Name <- volumes[polygons$Volume_Name, on="Volume_Name"]$Runway_Name
  
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  theta <- tbl_Runway[polygons$Runway_Name, on="Runway_Name"]$NODE_Heading_Offset
  polygons$Point_X <- (polygons$Point_X * cos(theta) + polygons$Point_Y * sin(theta)) + tbl_Runway[polygons$Runway_Name, on="Runway_Name"]$Threshold_X_Pos
  polygons$Point_Y <- (-polygons$Point_X * sin(theta) + polygons$Point_Y * cos(theta)) + tbl_Runway[polygons$Runway_Name, on="Runway_Name"]$Threshold_Y_Pos
  
  tbl_Adaptation_Data <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Adaptation_Data"))
  Converted_LatLon <- usp_GI_Latlong_From_XY(polygons$Point_X, polygons$Point_Y, tbl_Adaptation_Data)
  
  polygons$Latitude <- Converted_LatLon$PositionLatitude
  polygons$Longitude <- Converted_LatLon$PositionLongitude
  
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
  
  message("[",Sys.time(),"] ", "Appending ", nrow(volumes), " rows of additional downwind routes to tbl_Volume...")
  dbWriteTable(dbi_con, "tbl_Volume", volumes[,-c("Volume")], append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(volumes), " rows to tbl_Volume")
  
  polygons_list <- lapply(1:(nrow(route_point)-1), function(k) {
    
    if (route_point$Route_Name[k] != route_point$Route_Name[k+1]) return(NULL)
    
    data.table(
      Volume_Name = route_point$Volume_Name[k],
      Airfield_Name = NA,
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
  dbWriteTable(dbi_con, "tbl_Polygon", polygons[,-c("Point", "Runway_Name")], append = T)
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
  x <- fread(LogFilePath)
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Path_Leg...")
  dbWriteTable(dbi_con, "tbl_Path_Leg", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Path_Leg")
}

Populate_tbl_Path_Leg_Transition <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  tbl_Runway <- as.data.table(dbGetQuery(dbi_con, "SELECT * FROM tbl_Runway"))
  for (i in 1:nrow(x)) {
    if (x$Min_Heading[i] != 0 & x$Max_Heading[i] != 360) {
      x$Min_Heading[i] <- x$Min_Heading[i] + round(tbl_Runway[Runway_Name == x$Runway_Name[i]]$NODE_Heading_Offset[1] / fnc_GI_Degs_To_Rads(), 0)
      x$Max_Heading[i] <- x$Max_Heading[i] + round(tbl_Runway[Runway_Name == x$Runway_Name[i]]$NODE_Heading_Offset[1] / fnc_GI_Degs_To_Rads(), 0)
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
  x$Min_Heading <- x$Min_Heading * fnc_GI_Degs_To_Rads()
  x$Max_Heading <- x$Max_Heading * fnc_GI_Degs_To_Rads()
  x$Min_Range_To_ILS <- x$Min_Range_To_ILS * fnc_GI_Nm_To_M()
  x$Max_Range_To_ILS <- x$Max_Range_To_ILS * fnc_GI_Nm_To_M()
  x$Max_Altitude <- x$Max_Altitude * fnc_GI_Ft_To_M()
  x$Min_Sustained_RoCD <- x$Min_Sustained_RoCD * fnc_GI_Ft_Per_Min_To_M_Per_Sec()
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
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Aircraft_type_To_Wake...")
  dbWriteTable(dbi_con, "tbl_Aircraft_type_To_Wake", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Aircraft_type_To_Wake")
}

Populate_tbl_Aircraft_Type_To_Wake_Legacy <- function(LogFilePath, dbi_con) {
  message("[",Sys.time(),"] ", "Reading ", LogFilePath)
  x <- fread(LogFilePath)
  names(x)[1:2] <- c("Aircraft_Type", "Wake") # To match database column names
  message("[",Sys.time(),"] ", "Appending ", nrow(x), " rows to tbl_Aircraft_Type_To_Wake_Legacy...")
  dbWriteTable(dbi_con, "tbl_Aircraft_Type_To_Wake_Legacy", x, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(x), " rows to tbl_Aircraft_Type_To_Wake_Legacy")
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
  x$Initial_deceleration_Lead <- x$Initial_deceleration_Lead * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  x$Initial_deceleration_Follower <- x$Initial_deceleration_Follower * (fnc_GI_Kts_To_M_Per_Sec() / fnc_GI_Nm_To_M())
  
  # Change names because it is not capitalised in database!!! PLS FIX!
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

import_Config <- function(cd, dbi_con, load_legacy_wake = T, load_DW_volumes = T) {
  message("[",Sys.time(),"] ", "Import config from: ", cd)
  Populate_tbl_Adaptation(list.files(cd, ".*Populate_tbl_Adaptation_?D?a?t?a?.csv", full.names = T), dbi_con)
  Populate_tbl_Airfield(list.files(cd, ".*Populate_tbl_Airfield.csv", full.names = T), dbi_con)
  Populate_tbl_Runway(list.files(cd, ".*Populate_tbl_Runway.csv", full.names = T), dbi_con)
  Populate_tbl_Mode_S_Wind_Localiser_Capture(list.files(cd, ".*Populate_tbl_Mode_S_Wind_Localiser_Capture.csv", full.names = T), dbi_con)
  Populate_Airspace_Volumes(list.files(cd, ".*Populate_Airspace_Volumes.csv", full.names = T), dbi_con)
  if (load_DW_volumes) {
    Populate_Airspace_Volumes_DW(list.files(cd, ".*Route_Fix.csv", full.names = T), list.files(cd, ".*Route_Point.csv", full.names = T), dbi_con)
  }
  Populate_tbl_Mode_S_Wind_Adaptation(list.files(cd, ".*Populate_tbl_Mode_S_Wind_Adaptation.csv", full.names = T), dbi_con)
  Populate_tbl_Path_Leg(list.files(cd, ".*Populate_tbl_Path_Leg.csv", full.names = T), dbi_con)
  Populate_tbl_Path_Leg_Transition(list.files(cd, ".*Populate_tbl_Path_Leg_Transition.csv", full.names = T), dbi_con)
  Populate_tbl_ORD_Runway_Adaptation(list.files(cd, ".*Populate_tbl_ORD_Runway_Adaptation.csv", full.names = T), dbi_con)
  Populate_tbl_Aircraft_Type_To_Wake(list.files(cd, "Populate_tbl_Aircraft_Type_To_Wake.*.csv", full.names = T) %>% .[. != file.path(cd, "Populate_tbl_Aircraft_Type_To_Wake_Legacy.csv")], dbi_con)
  if (load_legacy_wake) {
    Populate_tbl_Aircraft_Type_To_Wake_Legacy(file.path(cd, "Populate_tbl_Aircraft_Type_To_Wake_Legacy.csv"), dbi_con)
  }
  Populate_tbl_Assumed_Recat_Separation_IAS(list.files(cd, "Populate_tbl_Assumed_Recat_Separation_IAS.*.csv", full.names = T), dbi_con)
  Populate_tbl_Assumed_ROT_Spacing_IAS(list.files(cd, "Populate_tbl_Assumed_ROT_Spacing_IAS.*.csv", full.names = T), dbi_con)
  Populate_tbl_Assumed_TBS_Table_IAS(list.files(cd, "Populate_tbl_Assumed_TBS_Table_IAS.*.csv", full.names = T), dbi_con)
  Populate_tbl_DBS_Wake_Turbulence(file.path(cd, "Populate_tbl_DBS_Wake_Turbulence.csv"), dbi_con)
  Populate_tbl_Mode_S_Wind_Default_Wind_Effect_Segments(file.path(cd, "Populate_tbl_Mode_S_Wind_Default_Wind_Effect_Segments.csv"), dbi_con)
  Populate_tbl_ORD_Aircraft_Adaptation(list.files(cd, "Populate_tbl_ORD_Aircraft_Adaptation.*.csv", full.names = T), dbi_con)
  Populate_tbl_ORD_DBS_Adaptation(list.files(cd, "Populate_tbl_ORD_DBS_Adaptation.*.csv", full.names = T), dbi_con)
  Populate_tbl_ORD_Wake_Adaptation(list.files(cd, "Populate_tbl_ORD_Wake_Adaptation.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_Recat_Separation_Dist(list.files(cd, "Populate_tbl_Reference_Recat_Separation_Dist.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_Recat_Separation_Time(list.files(cd, "Populate_tbl_Reference_Recat_Separation_Time.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_ROT_Spacing_Dist(list.files(cd, "Populate_tbl_Reference_ROT_Spacing_Dist.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_ROT_Spacing_Time(list.files(cd, "Populate_tbl_Reference_ROT_Spacing_Time.*.csv", full.names = T), dbi_con)
  Populate_tbl_Reference_TBS_Table_Time(list.files(cd, "Populate_tbl_Reference_TBS_Table_Time.*.csv", full.names = T), dbi_con)
  dbSendQuery(dbi_con, read_SQL_File("modules/data_loader/Update_tbl_Path_Leg_Transition.sql"))
  Sys.sleep(1)
  message("[",Sys.time(),"] ", "Finished importing new configuration data.")
}

# ----------------------------------------------------------------------- #
# Export Config Functions -------------------------------------------------
# ----------------------------------------------------------------------- #

# list(
#   ord_runways = list(
#     ord_runway = list(
#       runway_name = "R06",
#       max_dtt = "30.00",
#       four_hundred_ft_aal = "1.04",
#       six_hundred_ft_aal = "1.67",
#       thousand_ft_gate = "2.93",
#       gust_adjustment = "0.0"
#     ),
#     ord_runway = list(
#       runway_name = "R09",
#       max_dtt = "30.00",
#       four_hundred_ft_aal = "1.05",
#       six_hundred_ft_aal = "1.68",
#       thousand_ft_gate = "2.94",
#       gust_adjustment = "0.0"
#     )
#     # more ord_runway = list(...) here
#   ),
#   ord_wake_categories = list(
#     ord_wake_category = list(
#       # ...
#     ),
#     ord_wake_category = list(
#       # ...
#     ),
#     ord_wake_category = list(
#       # ...
#     )
#   ),
#   ord_aircraft_types = list(
#     ord_aircraft_type = list(
#       # ...
#     ),
#     ord_aircraft_type = list(
#       # ...
#     ),
#     ord_aircraft_type = list(
#       # ...
#     )
#   ),
#   ord_tbs_table_values = list(
#     ord_tbs_table_value = list(
#       #
#     ),
#     ord_tbs_table_value = list(
#       #
#     ),
#     ord_tbs_table_value = list(
#       #
#     )
#   ),
#   wind_effect_segment_entries = list(
#     wind_effect_segment_entry = list(
#       #
#     ),
#     wind_effect_segment_entry = list(
#       #
#     ),
#     wind_effect_segment_entry = list(
#       #
#     )
#   )
# )

