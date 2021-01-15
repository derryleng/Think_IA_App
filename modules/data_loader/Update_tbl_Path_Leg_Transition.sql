--  Rotate any heading ranges outside -360..+360 range (because they could have been rotated out of valid range).
UPDATE tbl_Path_Leg_Transition
SET Min_Heading = Min_Heading - 360.0,
Max_Heading = Max_Heading - 360.0
WHERE Min_Heading > 0.0 AND Max_Heading > 360.0

UPDATE tbl_Path_Leg_Transition
SET Min_Heading = Min_Heading + 360.0,
Max_Heading = Max_Heading + 360.0
WHERE Min_Heading < -360.0 AND Max_Heading < 0.0


-- Final check heading ranges are within correct limits.
IF EXISTS (SELECT * FROM tbl_Path_Leg_Transition
	WHERE (Min_Heading >= Max_Heading OR ABS(Min_Heading) > 360 OR ABS(Max_Heading) > 360 OR ABS(Max_Heading - Min_Heading) > 360)   )
BEGIN
	THROW 50001, 'ADAPTATION DATA ERROR: Invalid transition heading range detected', 1
END
