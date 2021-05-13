"%!in%" <- function(x,y) !("%in%"(x,y))

calc_landing_adjustment <- function(landing_type, headwind) {
  return(
    if (landing_type %in% c(0, 10, 11, 12)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(1, 2, 3, 4, 5)) {
      sapply(headwind / 3, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 15, 15, hw_adj)))
    } else if (landing_type %in% c(6)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 0, 0, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(7)) {
      sapply(headwind, function(hw_adj) 10)
    } else if (landing_type %in% c(8)) {
      sapply(headwind, function(hw_adj) 0)
    } else if (landing_type %in% c(9)) {
      sapply(headwind, function(hw_adj) ifelse(hw_adj > 20, 15, ifelse(hw_adj > 10, 10, 5)))
    }
  )
}

airspeed_model_break <- function(x, a, a1, b, n1, n2) {
  if (n1 < 1) n1 <- 1
  if (n2 < n1 | abs(n2 - n1) < 0.1) n2 <- n1
  return(
    if (x < 1) {
      a
    } else if (x >= 1 & x < n1) {
      a1
    } else if (x >= n1 & x <= n2 & abs(n2 - n1) < 0.1) {
      a1
    } else if (x >= n1 & x <= n2) {
      a1 + (x - n1) * (b - a1) / (n2 - n1)
    } else {
      b
    }
  )
}

airspeed_model_vector_break <- function(x, a, a1, b, n1, n2){
  sapply(x, airspeed_model_break, a = a, a1 = a1, b = b, n1 = n1, n2 = n2, simplify = T)
}

airspeed_model_break_simplified <- function(x, a, a1) {
  return(
    if (x < 1) {
      a
    } else {
      a1
    }
  )
}

airspeed_model_vector_break_simplified <- function(x, a, a1) {
  sapply(x, airspeed_model_break_simplified, a = a, a1 = a1, simplify = T)
}

parameter_summary <- function(dat) {
  cal_param <- names(dat)[2]
  names(dat)[2] <- "V2"
  dat$V2 <- as.numeric(dat$V2)
  x <- ddply(
    dat, names(dat)[1], summarise,
    N = length(V2),
    mean = mean(V2, na.rm = T),
    median = median(V2, na.rm = T),
    sd = sd(V2, na.rm = T),
    p5 = quantile(V2, 0.05, na.rm = T),
    p10 = quantile(V2, 0.1, na.rm = T)
  )
  x$Type <- cal_param
  return(x)
}

readjust_lss_type <- function(d, new_lss_type) {
  new_d <- d
  new_d$lss_type <- new_lss_type
  new_d$landing_adjustment = calc_landing_adjustment(new_lss_type, new_d$Surface_Headwind)
  return(new_d)
}

dodgy_colour_function <- function(aircraft_type) {
  x_char <- unlist(strsplit(as.character(aircraft_type), split="")) %>% ifelse(grepl("^[A-Z]$", .), match(., LETTERS), .) %>% as.numeric()
  x_tabl <- x_char %>% data.table(
    V1 = .[c(T, F, F, F)],
    V2 = .[c(F, T, F, F)],
    V3 = .[c(F, F, T, F)],
    V4 = .[c(F, F, F, T)],
    keep.rownames = F
  )
  x_prod <- paste0(x_tabl$V1, x_tabl$V2, x_tabl$V3, x_tabl$V4) %>% as.numeric()
  x_conv <- floor((x_prod - mean(x_prod)) / sd(x_prod) * 256^3 - 1)
  x_chex <- paste0("#", as.hexmode(x_conv))
  return(x_chex)
}

createBook <- function() {
  createWorkbook(type="xlsx")
}

writeCell <- function(worksheet, row_index, col_index, value) {
  r <- createCell(createRow(worksheet, rowIndex = row_index), colIndex = col_index)
  setCellValue(r[[1,1]], value)
}

writeRow <- function(worksheet, row_index, col_indices, values) {
  r <- createCell(createRow(worksheet, rowIndex = row_index), colIndex = 1:max(col_indices))
  for (i in 1:length(col_indices)) {
    setCellValue(r[[1,col_indices[i]]], values[i])
  }
}

writeTable <- function(worksheet, row_index, col_index, data_table) {
  addDataFrame(data_table, worksheet, startRow = row_index, startColumn = col_index, row.names = F)
}

writeImage <- function(worksheet, row_index, col_index, img) {
  addPicture(img, worksheet, scale = 1, startRow = row_index, startColumn = col_index)
}

saveBook <- function(workbook, filename) {
  saveWorkbook(workbook, filename)
}

# Find area under density curve using trapezoidal rule
density_area <- function(den, min, max, min_include = T, max_include = T) {
  den_x_range <- if (min_include & max_include) {
    den$x[den$x >= min & den$x <= max]
  } else if (min_include & !max_include) {
    den$x[den$x >= min & den$x < max]
  } else if (!min_include & max_include) {
    den$x[den$x > min & den$x <= max]
  } else if (!min_include & !max_include) {
    den$x[den$x > min & den$x < max]
  }
  den_y_range <- den$y[which(den$x %in% den_x_range)]
  if (length(den_y_range) > 1) {
    trapezoidal_areas <- sapply(2:length(den_y_range), function(i) (den_x_range[i] - den_x_range[i-1]) * (den_y_range[i] + den_y_range[i-1]) / 2)
    total_area <- sum(trapezoidal_areas)
    return(total_area)
  } else {
    return(0)
  }
}
