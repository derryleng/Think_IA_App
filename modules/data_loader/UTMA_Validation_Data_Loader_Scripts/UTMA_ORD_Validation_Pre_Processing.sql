
-- This script processes the UTMA Validation Database for eTBS ORD Validation
-- pre-processing for Heathrow.

-- Assumes the relevant initialisation scripts have been run
-- and the surveillance and flight data loaded (Eg using UTMA Validation Data Loader).

/****************************************************************************/

-- Set the processing date (Eg dd/mm/yy or null for all dates).
-- If the parameter is empty or not substituted, defaults to null.
DECLARE @Log_Date varchar(50)
SET @Log_Date = '$$PARAMETER$$'

IF @Log_Date = '' OR SUBSTRING(@Log_Date, 1, 2) = '$$'
	SET @Log_Date = null

/****************************************************************************/

-- Clear data to be reprocessed (if any).
EXEC usp_DL_Clear_Derived_Data_By_Date @Log_Date
EXEC usp_DL_Clear_Mode_S_Wind_Data_By_Date @Log_Date

/****************************************************************************/

-- Calculate flight plan derived fields.
EXEC usp_DF_Generate_Flight_Plan_Derived @Log_Date
EXEC usp_DF_Derive_FP_Aircraft_Time_At_DMEs @Log_Date


-- Purge FPs and tracks NOT arriving or departing the specified airport.
DECLARE @Airfield_Name varchar(50)
SET @Airfield_Name = (SELECT TOP 1 Airfield_Name FROM tbl_Airfield)

EXEC usp_DL_Purge_Flights_By_Airfield @Log_Date, null, @Airfield_Name  -- Params = Date, Departure Airfield, Arrival Airfield  (NOT to purge)

/****************************************************************************/

-- Calculate the radar track derived fields.
EXEC usp_DF_Generate_Radar_Track_Point_Derived @Log_Date
EXEC usp_DF_Derive_RTP_Corrected_Mode_C @Log_Date
EXEC usp_DF_Derive_RTP_ILS_Relative_Fields @Log_Date
EXEC usp_DF_Derive_RTP_Min_Sustained_RoCD @Log_Date
EXEC usp_DF_Derive_RTP_Path_Leg @Log_Date
EXEC usp_DF_Derive_RTP_Mode_S_Wind_Localiser_Capture @Log_Date
EXEC usp_DF_Derive_RTP_Range_To_Threshold @Log_Date
EXEC usp_DF_Derive_RTP_Wind @Log_Date

/****************************************************************************/

-- Generate GWCS wind data for general reference.
EXEC usp_GWCS_Generate_Mode_S_Wind_Point_Filter_Stats @Log_Date
EXEC usp_GWCS_Generate_Mode_S_Wind_Seg @Log_Date
EXEC usp_GWCS_Generate_Mode_S_Wind_Seg_Flags @Log_Date
EXEC usp_GWCS_Generate_Mode_S_Wind_Seg_Forecast_Option_2 @Log_Date

/****************************************************************************/
