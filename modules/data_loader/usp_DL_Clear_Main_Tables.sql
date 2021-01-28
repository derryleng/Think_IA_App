-- Note that order is important, ie reverse order of hierarchies.
  DELETE FROM tbl_Mode_S_Wind_Seg_Forecast
  IF IDENT_CURRENT('tbl_Mode_S_Wind_Seg_Forecast') >= 1
  	DBCC CHECKIDENT (tbl_Mode_S_Wind_Seg_Forecast, RESEED, 0)
  
  DELETE FROM tbl_Mode_S_Wind_Point_Filter_Stats
  IF IDENT_CURRENT('tbl_Mode_S_Wind_Point_Filter_Stats') >= 1
  	DBCC CHECKIDENT (tbl_Mode_S_Wind_Point_Filter_Stats, RESEED, 0)
  
  DELETE FROM tbl_Mode_S_Wind_Forecast
  IF IDENT_CURRENT('tbl_Mode_S_Wind_Forecast') >= 1
  	DBCC CHECKIDENT (tbl_Mode_S_Wind_Forecast, RESEED, 0)
  
  DELETE FROM tbl_Mode_S_Wind_Observation
  IF IDENT_CURRENT('tbl_Mode_S_Wind_Observation') >= 1
  	DBCC CHECKIDENT (tbl_Mode_S_Wind_Observation, RESEED, 0)
  
  DELETE FROM tbl_Mode_S_Wind_Seg_Flags
  IF IDENT_CURRENT('tbl_Mode_S_Wind_Seg_Flags') >= 1
  	DBCC CHECKIDENT (tbl_Mode_S_Wind_Seg_Flags, RESEED, 0)
  
  DELETE FROM tbl_Mode_S_Wind_Seg
  IF IDENT_CURRENT('tbl_Mode_S_Wind_Seg') >= 1
  	DBCC CHECKIDENT (tbl_Mode_S_Wind_Seg, RESEED, 0)
  
  DELETE FROM tbl_PLT_Abnormal_Transition
  IF IDENT_CURRENT('tbl_PLT_Abnormal_Transition') >= 1
  	DBCC CHECKIDENT (tbl_PLT_Abnormal_Transition, RESEED, 0)
  
  DELETE FROM tbl_Previous_Radar_Track_Point_Derived
  IF IDENT_CURRENT('tbl_Previous_Radar_Track_Point_Derived') >= 1
  	DBCC CHECKIDENT (tbl_Previous_Radar_Track_Point_Derived, RESEED, 0)
  
  DELETE FROM tbl_Radar_Track_Point_Derived
  IF IDENT_CURRENT('tbl_Radar_Track_Point_Derived') >= 1
  	DBCC CHECKIDENT (tbl_Radar_Track_Point_Derived, RESEED, 0)
  
  DELETE FROM tbl_Radar_Track_Point
  IF IDENT_CURRENT('tbl_Radar_Track_Point') >= 1
  	DBCC CHECKIDENT (tbl_Radar_Track_Point, RESEED, 0)
  
  DELETE FROM tbl_PLT_Analysis_Report
  IF IDENT_CURRENT('tbl_PLT_Analysis_Report') >= 1
  	DBCC CHECKIDENT (tbl_PLT_Analysis_Report, RESEED, 0)
  
  DELETE FROM tbl_Previous_PLT_Analysis_Report
  IF IDENT_CURRENT('tbl_Previous_PLT_Analysis_Report') >= 1
  	DBCC CHECKIDENT (tbl_Previous_PLT_Analysis_Report, RESEED, 0)
  
  DELETE FROM tbl_Flight_Plan_Derived
  IF IDENT_CURRENT('tbl_Flight_Plan_Derived') >= 1
  	DBCC CHECKIDENT (tbl_Flight_Plan_Derived, RESEED, 0)
  
  DELETE FROM tbl_Flight_Plan
  IF IDENT_CURRENT('tbl_Flight_Plan') >= 1
  	DBCC CHECKIDENT (tbl_Flight_Plan, RESEED, 0)
  
  DELETE FROM tbl_ORD_Aircraft_Profile
  IF IDENT_CURRENT('tbl_ORD_Aircraft_Profile') >= 1
  	DBCC CHECKIDENT (tbl_ORD_Aircraft_Profile, RESEED, 0)
  
  DELETE FROM tbl_ORD_IAS_Profile
  IF IDENT_CURRENT('tbl_ORD_IAS_Profile') >= 1
  	DBCC CHECKIDENT (tbl_ORD_IAS_Profile, RESEED, 0)
  
  DELETE FROM tbl_ORD_GS_Profile
  IF IDENT_CURRENT('tbl_ORD_GS_Profile') >= 1
  	DBCC CHECKIDENT (tbl_ORD_GS_Profile, RESEED, 0)
  
  DELETE FROM tbl_ORD_Prediction
  IF IDENT_CURRENT('tbl_ORD_Prediction') >= 1
  	DBCC CHECKIDENT (tbl_ORD_Prediction, RESEED, 0)
  
  DELETE FROM tbl_ORD_Observation
  IF IDENT_CURRENT('tbl_ORD_Observation') >= 1
  	DBCC CHECKIDENT (tbl_ORD_Observation, RESEED, 0)
  
  DELETE FROM tbl_eTBS_Performance_Model
  IF IDENT_CURRENT('tbl_eTBS_Performance_Model') >= 1
  	DBCC CHECKIDENT (tbl_eTBS_Performance_Model, RESEED, 0)
  
  DELETE FROM tbl_All_Pair_Reference_Data
  IF IDENT_CURRENT('tbl_All_Pair_Reference_Data') >= 1
  	DBCC CHECKIDENT (tbl_All_Pair_Reference_Data, RESEED, 0)
  
  DELETE FROM tbl_All_Pair_Radar_Track_Point
  IF IDENT_CURRENT('tbl_All_Pair_Radar_Track_Point') >= 1
  	DBCC CHECKIDENT (tbl_All_Pair_Radar_Track_Point, RESEED, 0)
  
  DELETE FROM tbl_Landing_Pair
  IF IDENT_CURRENT('tbl_Landing_Pair') >= 1
  	DBCC CHECKIDENT (tbl_Landing_Pair, RESEED, 0)
  
  DELETE FROM tbl_Departure_Pair_Separation_Evolution
  IF IDENT_CURRENT('tbl_Departure_Pair_Separation_Evolution') >= 1
  	DBCC CHECKIDENT (tbl_Departure_Pair_Separation_Evolution, RESEED, 0)
  
  DELETE FROM tbl_Departure_Pair_Separation
  IF IDENT_CURRENT('tbl_Departure_Pair_Separation') >= 1
  	DBCC CHECKIDENT (tbl_Departure_Pair_Separation, RESEED, 0)
  
  DELETE FROM tbl_Departure_Pair
  IF IDENT_CURRENT('tbl_Departure_Pair') >= 1
  	DBCC CHECKIDENT (tbl_Departure_Pair, RESEED, 0)
  
  DELETE FROM tbl_Baro
  IF IDENT_CURRENT('tbl_Baro') >= 1
  	DBCC CHECKIDENT (tbl_Baro, RESEED, 0)
  
  DELETE FROM tbl_Anemometer
  IF IDENT_CURRENT('tbl_Anemometer') >= 1
  	DBCC CHECKIDENT (tbl_Anemometer, RESEED, 0)  

