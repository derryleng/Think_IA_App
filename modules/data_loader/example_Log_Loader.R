process_Example_1 <- function(LogFilePath, dbi_con) {
  
  # Required arguments:
  # LogFilePath - full path to file to be loaded, e.g. C:/Data/IA/Log.csv
  # dbi_con - database connection
  
  # Print message to console indicating start of processing.
  message("[",Sys.time(),"] ", "Begin processing example log file...")
  
  # Imports data from the raw log file, if this is not in a CSV format,
  # you will need to use another function and process accordingly.
  x <- fread(LogFilePath)

  # Print message to console regarding number of lines read in.  
  message("[",Sys.time(),"] ", "Read ", nrow(x), " lines.")

  # Add additional lines for data processing where needed.
    
  # This table is in a format ready to be written to the database,
  # therefore it must match the column names and data requirements of the
  # corresponding table in the database.
  out <- data.table(
    Example_Name = x$Name,
    Example_Date = format(as.Date(x$Date, "%d-%m-%Y"), "%d/%m/%Y")
  )
  
  # Append new data to table in the database.
  message("[",Sys.time(),"] ", "Appending ", nrow(out), " rows to tbl_Example...")
  dbWriteTable(dbi_con, "tbl_Example", out, append = T)
  message("[",Sys.time(),"] ", "Successfully appended ", nrow(out), " rows to tbl_Example")
  
}

