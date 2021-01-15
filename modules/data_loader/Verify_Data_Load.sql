-- Script in SMSS/ADS targeting relevant database

-- Flight Plan
SELECT TOP (10) * FROM tbl_Flight_Plan ORDER BY Flight_Plan_ID DESC
SELECT COUNT(*) AS tbl_Flight_Plan_Rows FROM tbl_Flight_Plan

-- Surface Wind Logs
SELECT TOP (10) * FROM tbl_Anemometer
SELECT COUNT(*) AS tbl_Anemometer_Rows FROM tbl_Anemometer

-- QNH Logs
SELECT TOP (10) * FROM tbl_Baro
SELECT COUNT(*) AS tbl_Baro_Rows FROM tbl_Baro

-- Tracks
SELECT TOP (10) * FROM tbl_Radar_Track_Point
SELECT COUNT(*) AS tbl_Radar_Track_Point_Rows FROM tbl_Radar_Track_Point
