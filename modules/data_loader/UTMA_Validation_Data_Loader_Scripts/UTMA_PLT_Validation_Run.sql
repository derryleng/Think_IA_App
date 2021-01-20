
-- This script processes the UTMA Validation Database eTBS Path Leg Tracking
-- Validation processing  for Heathrow.

-- This processing should be re-run after PLT adaptation data changes to re-generate
-- PLT data and comparison tables.

-- Assumes the relevant initialisation scripts have been run
-- and the surveillance and flight data loaded (Eg using UTMA Validation Data Loader)
-- and the baseline processing has been run (Eg UTMA_PLT_Validation_Pre_Processing)
-- for the calendar date specified.

/****************************************************************************/

-- Set the processing date (Eg dd/mm/yy or null for all dates).
-- If the parameter is empty or not substituted, defaults to null.
DECLARE @Log_Date varchar(50)
SET @Log_Date = '$$PARAMETER$$'

IF @Log_Date = '' OR SUBSTRING(@Log_Date, 1, 2) = '$$'
	SET @Log_Date = null

/****************************************************************************/

-- Copy last processed data for comparison (if any).
DELETE FROM tbl_Previous_Radar_Track_Point_Derived
DELETE FROM tbl_Previous_PLT_Analysis_Report

INSERT INTO tbl_Previous_Radar_Track_Point_Derived
	SELECT * FROM tbl_Radar_Track_Point_Derived

INSERT INTO tbl_Previous_PLT_Analysis_Report
	SELECT * FROM tbl_PLT_Analysis_Report

-- Reset the path leg values.
-- Note will reset ALL path leg values.
UPDATE tbl_Radar_Track_Point_Derived
SET Path_Leg = null

-- Clear the PLT analysis output.
DELETE FROM tbl_PLT_Analysis_Report
DELETE FROM tbl_PLT_Abnormal_Transition

-- Re-generate the PLT path legs (specified date only).
EXEC usp_DF_Derive_RTP_Path_Leg @Log_Date

-- Generate search results for PLT Abnormal Transition Search and Analysis.
EXEC usp_PLT_Generate_Analysis_Report @Log_Date

/****************************************************************************/
