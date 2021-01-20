
-- This script processes the UTMA Validation Database for eTBS ORD Validation
-- processing for Heathrow.

-- Assumes the relevant initialisation scripts have been run
-- and the surveillance and flight data loaded (Eg using UTMA Validation Data Loader)
-- and the baseline processing has been run (Eg UTMA_ORD_Validation_Pre_Processing
-- for the calendar date specified.

/****************************************************************************/

-- Set the processing date (Eg dd/mm/yy or null for all dates).
-- If the parameter is empty or not substituted, defaults to null.
DECLARE @Log_Date varchar(50)
SET @Log_Date = '$$PARAMETER$$'

IF @Log_Date = '' OR SUBSTRING(@Log_Date, 1, 2) = '$$'
	SET @Log_Date = null

/****************************************************************************/

-- Delete the landing pairs (if any).
-- This will cascade delete all the ORD/TBS data to be re-processed.
DELETE FROM tbl_Landing_Pair WHERE (Landing_Pair_Date = @Log_Date OR @Log_Date IS null)

-- Processing for ORD Validation.
EXEC usp_DF_Generate_Landing_Pair @Log_Date
EXEC usp_TBS_Generate_ORD_Observation @Log_Date
EXEC usp_TBS_Generate_ORD_Prediction @Log_Date
EXEC usp_TBS_Generate_Performance_Model @Log_Date
EXEC usp_TBS_Generate_All_Pair_Reference_Data @Log_Date

/****************************************************************************/
