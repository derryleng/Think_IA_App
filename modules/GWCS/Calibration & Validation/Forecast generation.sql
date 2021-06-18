--GWCS algorithm procedures

--Delete GWCS data tables
--Run all delete statements if completely clearing out GWCS processing (e.g. new adaptation)

-- Note that order is important, ie reverse order of hierarchies.

/*
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
*/
-- Set the processing date (Eg dd/mm/yy or null for all dates).
-- If the parameter is empty or not substituted, defaults to null.
DECLARE @Log_Date varchar(50)
	SET @Log_Date = null

--Run GWCS algorithm procedures
--Executed in data loader (ORD pre processing)

--EXEC usp_GWCS_Generate_Mode_S_Wind_Point_Filter_Stats @Log_Date
--EXEC usp_GWCS_Generate_Mode_S_Wind_Seg @Log_Date
--EXEC usp_GWCS_Generate_Mode_S_Wind_Seg_Flags @Log_Date
--EXEC usp_GWCS_Generate_Mode_S_Wind_Seg_Forecast_Option_2 @Log_Date

--Run the GWCS forecast and observations per aircraft pair
--For Sep_Dist = 3.5, adjust Forecast_Seg_Max equation (remove '- seg_size')
DECLARE @Sep_Dist real
SET @Sep_Dist = 8 * dbo.fnc_GI_Nm_To_M()     -- Set separation distance (between 3 and 8)

DECLARE @Seg_Size real
SET @Seg_Size = 1 * dbo.fnc_GI_Nm_To_M()

UPDATE tbl_Mode_S_Wind_Adaptation SET Forecast_Seg_Min = 0 * dbo.fnc_GI_Nm_To_M()				
UPDATE tbl_Mode_S_Wind_Adaptation SET Forecast_Seg_Max = Forecast_Seg_Min + @Sep_Dist - @Seg_Size

DELETE FROM tbl_Mode_S_Wind_Forecast
IF IDENT_CURRENT('tbl_Mode_S_Wind_Forecast') >= 1
	DBCC CHECKIDENT (tbl_Mode_S_Wind_Forecast, RESEED, 0)

DELETE FROM tbl_Mode_S_Wind_Observation
IF IDENT_CURRENT('tbl_Mode_S_Wind_Observation') >= 1
	DBCC CHECKIDENT (tbl_Mode_S_Wind_Observation, RESEED, 0)


EXEC usp_GWCS_Generate_Mode_S_Wind_Observation @Log_Date
EXEC usp_GWCS_Generate_Mode_S_Wind_Forecast 'Option_2', '', @Log_Date

--EXEC usp_GWCS_Get_Mode_S_Wind_Forecast

--Output tables/views

-- Save this View
SELECT * FROM vw_Mode_S_Wind_Forecast

/***********************************************

--Adaptation
SELECT * FROM tbl_Mode_S_Wind_Adaptation
SELECT * FROM tbl_Mode_S_Wind_Localiser_Capture 

--GWCS tables
SELECT * FROM tbl_Mode_S_Wind_Point_Filter_Stats
SELECT * FROM tbl_Mode_S_Wind_Seg
SELECT * FROM tbl_Mode_S_Wind_Seg_Flags
SELECT * FROM tbl_Mode_S_Wind_Seg_Forecast
SELECT * FROM tbl_Mode_S_Wind_Forecast
SELECT * FROM tbl_Mode_S_Wind_Observation

***********************************************/




