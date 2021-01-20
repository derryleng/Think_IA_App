
-- This script creates a surface wind data entry for each Toronto
-- runway from each single airport wide (no runway designation)

IF OBJECT_ID('tempdb..#tmp_Anemo') is not null 
DROP TABLE #tmp_Anemo

DELETE FROM tbl_Anemometer WHERE Landing_Runway <> ''

SELECT * INTO #tmp_Anemo FROM tbl_Anemometer

UPDATE #tmp_Anemo
SET Landing_Runway = 'R23'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R05'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R24L'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R24R'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R06L'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R06R'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R33L'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R33R'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R15L'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

UPDATE #tmp_Anemo
SET Landing_Runway = 'R15R'
INSERT INTO tbl_Anemometer SELECT * FROM #tmp_Anemo

/*
SET DATEFORMAT dmy
SELECT * FROM tbl_Anemometer
ORDER BY CAST(Anemo_Date AS datetime), Anemo_Time, Landing_Runway
*/