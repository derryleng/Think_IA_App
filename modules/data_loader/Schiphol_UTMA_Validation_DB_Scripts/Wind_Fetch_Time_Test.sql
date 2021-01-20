
-- Test script to test the time taken to fetch wind data etc.

--EXEC sp_updatestats

/*****************************************************************************/

	DECLARE @Seg_Forecast_Wind_Effect_IAS real
	DECLARE @Seg_Forecast_Time real
	DECLARE @Seg_Forecast_Aircraft_Type varchar(50)
	DECLARE @Seg_Forecast_Interpolated bit
	DECLARE @Seg_Forecast_Wind_SPD real
	DECLARE @Seg_Forecast_Wind_HDG real

	DECLARE @Count int
	SET @Count = 1000

	WHILE @Count > 0
	BEGIN

	SELECT
		@Seg_Forecast_Wind_Effect_IAS = Forecast_Wind_Effect_IAS,
		@Seg_Forecast_Time = Forecast_Time,
		@Seg_Forecast_Aircraft_Type = Forecast_Aircraft_Type,
		@Seg_Forecast_Interpolated = 0,
		@Seg_Forecast_Wind_SPD = Forecast_Wind_SPD,
		@Seg_Forecast_Wind_HDG = Forecast_Wind_HDG
	FROM tbl_Mode_S_Wind_Seg_Forecast
	WHERE
		Forecast_Type = 'Option_2' AND
		dbo.fnc_GI_Is_Same_Approach(Landing_Runway, 'R18R') = 1 AND
		DME_Seg = 5 * dbo.fnc_GI_Nm_To_M() AND
		Forecast_Date = '05/11/2018' AND
		--dbo.fnc_GI_Is_In_Bounds(Forecast_Time,
		--						40000 - 900,
		--						40000,
		--						0) = 1 AND
		Forecast_Time >= 40000 - 900 AND
		Forecast_Time <= 40000 AND
		Forecast_Interpolated = 0
	ORDER BY Forecast_Time ASC  -- Equiv TOP 1 DESC

	SET @Count = @Count - 1
	END

/*
SET DATEFORMAT dmy
SELECT * FROM tbl_Radar_Track_Point
ORDER BY CAST(Track_Date AS datetime) DESC
*/

/*
Before any changes: 1:31, 1:30, 1:31
Add Forcast_Type to index: 1:31
Is Unique=Yes: 1:31
Unique Key: 1:31
Column Store Index: 1:31
Replace function call dbo.fnc_GI_Is_In_Bounds: 0:03 !!!!!!!!!!!!!!!!!
*/

/*****************************************************************************/

	DECLARE @Profile_Section varchar(50)

	DECLARE @Count int
	SET @Count = 10000000

	WHILE @Count > 0
	BEGIN

		SELECT
		@Profile_Section = Profile_Section
		FROM tbl_ORD_IAS_Profile
		WHERE Landing_Pair_ID = 9417 AND
		This_Pair_Role = 'L'
		ORDER BY Section_Number

		SET @Count = @Count - 1
	END

/*
Before changes: 2:16, 2:18
Unique + Clustered: 0:58
*/

/*****************************************************************************/

	DECLARE @Track_Time real

	DECLARE @Count int
	SET @Count = 10000

	WHILE @Count > 0
	BEGIN
		SELECT @Track_Time = Track_Time
		FROM tbl_Radar_Track_Point AS RTP

		INNER JOIN tbl_Flight_Plan AS FP
		ON FP.Flight_Plan_ID = RTP.Flight_Plan_ID

		WHERE FP_Date = '05/11/2018'

		ORDER BY RTP.Flight_Plan_ID, Track_Time ASC
		
		SET @Count = @Count - 1
	END

/*
Before changes: 2:31, 2:30
Add FP_ID + FP_Date index: 2:31, 2:30
*/