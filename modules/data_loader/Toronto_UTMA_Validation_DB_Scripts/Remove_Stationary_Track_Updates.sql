
-- This script processes all radar track updates for all flights looking
-- for stationary aircraft positions.  This issue has been observed in
-- Toronto data and will affect subsequent calculations especially involving
-- distance and time.  Any time consecutive track updates with an identical position
-- are deleted from the track update table.

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- This script should be run at least twice until there are no more deletions
-- (appear to be some alternating identical positions).
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

/****************************************************************************/

-- Set the processing date (Eg dd/mm/yy or null for all dates).
-- If the parameter is empty or not substituted, defaults to null.
DECLARE @Log_Date varchar(50)
SET @Log_Date = '$$PARAMETER$$'

IF @Log_Date = '' OR SUBSTRING(@Log_Date, 1, 2) = '$$'
	SET @Log_Date = null

/****************************************************************************/

-- Declare cursor data.
DECLARE @Flight_Plan_ID int
SET @Flight_Plan_ID = null

DECLARE @Radar_Track_Point_ID int
SET @Radar_Track_Point_ID = null

DECLARE @Track_Time real
SET @Track_Time = null

DECLARE @X_Pos real
SET @X_Pos = null

DECLARE @Y_Pos real
SET @Y_Pos = null

-- Declare cursor loop data.
DECLARE @Previous_Flight_Plan_ID int
SET @Previous_Flight_Plan_ID = null

DECLARE @Previous_Track_Time real
SET @Previous_Track_Time = null

DECLARE @Previous_X_Pos real
SET @Previous_X_Pos = null

DECLARE @Previous_Y_Pos real
SET @Previous_Y_Pos = null

DECLARE @Deletion_Count int
SET @Deletion_Count = 0


DECLARE CUR01 CURSOR FAST_FORWARD FOR
	SELECT
		FP.Flight_Plan_ID,
		Radar_Track_Point_ID,
		Track_Time,
		X_Pos,
		Y_Pos
	FROM tbl_Radar_Track_Point AS RTP

	INNER JOIN tbl_Flight_Plan AS FP
	ON FP.Flight_Plan_ID = RTP.Flight_Plan_ID
		
	WHERE FP_Date = @Log_Date OR @Log_Date IS null

	ORDER BY FP.Flight_Plan_ID, Track_Time, Radar_Track_Point_ID

OPEN CUR01

-- Loop on all radar track points for each aircraft in time order.
FETCH NEXT FROM CUR01 INTO
				@Flight_Plan_ID,
				@Radar_Track_Point_ID,
				@Track_Time,
				@X_Pos,
				@Y_Pos
WHILE (@@FETCH_STATUS = 0)

BEGIN

	-- Reset if a new flight.
	IF @Flight_Plan_ID <> @Previous_Flight_Plan_ID OR @Flight_Plan_ID IS null
	BEGIN
		SET @Previous_Track_Time = null
		SET @Previous_X_Pos = null
		SET @Previous_Y_Pos = null
	END

	ELSE

	BEGIN
		-- Test for stationary update, but not for a new flight.
		IF @Previous_Track_Time = @Track_Time OR (@X_Pos = @Previous_X_Pos AND @Y_Pos = @Previous_Y_Pos)
		BEGIN
			DELETE FROM tbl_Radar_Track_Point_Derived
			WHERE Radar_Track_Point_ID = @Radar_Track_Point_ID

			DELETE FROM tbl_Radar_Track_Point
			WHERE Radar_Track_Point_ID = @Radar_Track_Point_ID

			SET @Deletion_Count = @Deletion_Count + 1

			PRINT CAST(@Deletion_Count AS varchar(50)) + ' ' +
				  'This / Previous (Flight_Plan_ID, Time, X, Y): ' +
				  CAST(@Flight_Plan_ID AS varchar(50)) + ' / ' + 
  				  CAST(@Previous_Flight_Plan_ID AS varchar(50)) + ', ' + 
				  CAST(@Track_Time AS varchar(50)) + ' / ' +
				  CAST(@Previous_Track_Time AS varchar(50)) + ', ' +
				  CAST(@X_Pos AS varchar(50)) + ' / ' +
				  CAST(@Previous_X_Pos AS varchar(50)) + ', ' +
				  CAST(@Y_Pos AS varchar(50)) + ' / ' +
				  CAST(@Previous_Y_Pos AS varchar(50))

		END

	END
		
	SET @Previous_Flight_Plan_ID = @Flight_Plan_ID
	SET @Previous_Track_Time = @Track_Time
	SET @Previous_X_Pos = @X_Pos
	SET @Previous_Y_Pos = @Y_Pos

 	FETCH NEXT FROM CUR01 INTO
					@Flight_Plan_ID,
					@Radar_Track_Point_ID,
					@Track_Time,
					@X_Pos,
					@Y_Pos
END
CLOSE CUR01
DEALLOCATE CUR01

PRINT 'Deletion_Count = ' + CAST(@Deletion_Count AS varchar(50))
