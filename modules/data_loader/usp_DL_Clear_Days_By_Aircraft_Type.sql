DECLARE @Aircraft_Type_To_Keep varchar(50)
SET @Aircraft_Type_To_Keep = ?actype

-- Declare cursor data.
DECLARE @FP_Date varchar(50)
SET @FP_Date = null

DECLARE CUR01 CURSOR FAST_FORWARD FOR
    SELECT DISTINCT(FP_Date)
    FROM tbl_Flight_Plan
OPEN CUR01

-- Loop on all dates.
FETCH NEXT FROM CUR01 INTO @FP_Date
WHILE (@@FETCH_STATUS = 0)

BEGIN
    IF NOT EXISTS (SELECT * FROM tbl_Flight_Plan WHERE FP_Date = @FP_Date AND Aircraft_Type = @Aircraft_Type_To_Keep) AND
        @Aircraft_Type_To_Keep <> ''
        --PRINT @FP_Date  -- Test code.
        EXEC usp_DL_Clear_Day_By_Date @FP_Date
    
    FETCH NEXT FROM CUR01 INTO @FP_Date
END
CLOSE CUR01
DEALLOCATE CUR01
