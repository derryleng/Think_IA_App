
-- This script creates a surface wind data entry for R24 and R04
-- runways at Schiphol from nearby R36R threshold.

IF OBJECT_ID('tempdb..#tmp_Anemo') is not null 
DROP TABLE #tmp_Anemo

DELETE FROM tbl_Anemometer WHERE Landing_Runway = 'R24' OR Landing_Runway = 'R04'

SELECT * INTO #tmp_Anemo FROM tbl_Anemometer WHERE Landing_Runway = 'R36R'

UPDATE #tmp_Anemo
SET Landing_Runway = 'R24'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R04'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

/*
SET DATEFORMAT dmy
SELECT * FROM tbl_Anemometer
ORDER BY CAST(Anemo_Date AS datetime), Anemo_Time, Landing_Runway
*/