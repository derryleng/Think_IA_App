-- Airspace
DELETE FROM tbl_Departure_Wake_Turbulence
DELETE FROM tbl_SID_Group
DELETE FROM tbl_SID
DELETE FROM tbl_Ground_Movement_Gate
DELETE FROM tbl_Path_Leg_Transition
IF IDENT_CURRENT('tbl_Path_Leg_Transition') >= 1
	DBCC CHECKIDENT (tbl_Path_Leg_Transition, RESEED, 0)
DELETE FROM tbl_Path_Leg
DELETE FROM tbl_Mode_S_Wind_Adaptation
DELETE FROM tbl_Polygon
DELETE FROM tbl_Volume
DELETE FROM tbl_Mode_S_Wind_Localiser_Capture
DELETE FROM tbl_Runway
DELETE FROM tbl_Airfield
DELETE FROM tbl_Adaptation_Data
DELETE FROM tbl_Mode_S_Wind_Default_Wind_Effect_Segments -- New for IA-115

-- ORD
DELETE FROM tbl_ORD_Runway_Adaptation
DELETE FROM tbl_ORD_Aircraft_Adaptation
DELETE FROM tbl_ORD_Wake_Adaptation
DELETE FROM tbl_ORD_DBS_Adaptation

-- eTBS Calc Data
DELETE FROM tbl_Reference_Recat_Separation_Dist
DELETE FROM tbl_Reference_Recat_Separation_Time
DELETE FROM tbl_Assumed_Recat_Separation_IAS
DELETE FROM tbl_Reference_ROT_Spacing_Dist
DELETE FROM tbl_Reference_ROT_Spacing_Time
DELETE FROM tbl_Assumed_ROT_Spacing_IAS
DELETE FROM tbl_Reference_TBS_Table_Time
DELETE FROM tbl_Assumed_TBS_Table_IAS

DELETE FROM tbl_DBS_Wake_Turbulence
DELETE FROM tbl_TBS_Wake_Turbulence

DELETE FROM [RECAT EU vs NAS ACHR - 27-06-17]
DELETE FROM tbl_Aircraft_Type_To_Wake