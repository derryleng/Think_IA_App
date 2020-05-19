# ----------------------------------------------------------------------- #
# ORD Calibration Functions -----------------------------------------------
# ----------------------------------------------------------------------- #

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

query_date_ffpids <- function(date, con) {
  # date - must be string in the form YYYY-MM-DD
  x <- sprintf(
    " SET DATEFORMAT dmy
      SELECT DISTINCT Follower_Flight_Plan_ID 
      FROM vw_ORD_Calibration_View
      WHERE CAST(FP_Date AS datetime) = '%s'
    ", as.character(date)
  ) %>% sqlQuery(con,.) %>% unlist() %>% as.vector()
  return(x)
}

query_ffpid_tracks <- function(ffpid, con) {
  x <- sprintf(
    " SELECT * FROM vw_ORD_Calibration_View WHERE Follower_Flight_Plan_ID = '%s'
    ", as.character(ffpid)
  ) %>% sqlQuery(con,.) %>% as.data.table()
  return(x)
}

process_tracks_for_nls <- function(tracks, minRTT = 0, maxRTT = 6, speed_type = "Mode_S_IAS", airport_alt = 0) {
  
  tracks <- tracks[Follower_Range_To_Threshold >= minRTT & Follower_Range_To_Threshold <= maxRTT][order(Track_Time)]
  
  if (nrow(tracks) > 1) {
    
    if (speed_type == "Mode S IAS") {
      
      x <- tracks$Follower_Range_To_Threshold
      y <- tracks$Mode_S_IAS
      
    } else if (speed_type == "Track Speed") {
      
      x <- tracks$Follower_Range_To_Threshold
      y <- (tracks$Track_Speed+tracks$Follower_Threshold_Surface_Headwind)/(1+airport_alt/60000)
      
    } else if (speed_type == "Calculated Speed") {
      
      # Filter track for stationary points
      k <- 2
      while (k <= nrow(tracks)) {
        # same_range <- tracks$Follower_Range_To_Threshold[k] == tracks$Follower_Range_To_Threshold[k-1]
        same_x <- tracks$X_Position[k] == tracks$X_Position[k-1]
        same_y <- tracks$Y_Position[k] == tracks$Y_Position[k-1]
        same_time <- tracks$Track_Time[k] == tracks$Track_Time[k-1]
        if (same_time | (same_x & same_y)) {
          tracks <- tracks[-k]
        } else {
          k <- k + 1
        }
      }
      
      # Calculate speed based on positional difference between timestamps
      tracks_new <- tracks[,c("Track_Time", "Follower_Threshold_Surface_Headwind", "Follower_Range_To_Threshold", "X_Position", "Y_Position", "Altitude", "Track_Speed")]
      tracks_new$Distance_Travelled <- NA
      for (k in 2:nrow(tracks_new)) {
        tracks_new$Distance_Travelled[k] <- sqrt((tracks_new$X_Position[k] - tracks_new$X_Position[k-1])^2 + (tracks_new$Y_Position[k] - tracks_new$Y_Position[k-1])^2 + ((tracks_new$Altitude[k] - tracks_new$Altitude[k-1])/6076.12)^2)
        if (k == 2) {
          tracks_new$Distance_Travelled[k-1] <- tracks_new$Distance_Travelled[k]
        }
      }
      tracks_new$Point_Speed <- NA
      tracks_new$Point_Speed[1] <- tracks_new$Track_Speed[1]
      for (k in 2:nrow(tracks_new)) {
        tracks_new$Point_Speed[k] <- tracks_new$Distance_Travelled[k]/((tracks_new$Track_Time[k]-tracks_new$Track_Time[k-1])/3600)
      }
      
      # Filter strange speeds (x% above or below Track_Speed range)
      tracks_new <- tracks_new[
        Point_Speed >= max(min(tracks_new$Track_Speed, na.rm=T)*(speed_filter_perc/100), speed_filter_limit_low, na.rm=T) &
          Point_Speed <= min(max(tracks_new$Track_Speed, na.rm=T)*(1+speed_filter_perc/100), speed_filter_limit_high, na.rm=T)
        ]
      
      # Get tracks x and y vectors
      x <- tracks_new$Follower_Range_To_Threshold
      y <- (tracks_new$Point_Speed+tracks_new$Follower_Threshold_Surface_Headwind)/(1+airport_alt/60000)
      
      # Catch possible discrepancy in length of x and y
      if (length(x) < length(y)) {
        y <- y[1:length(x)]
      } else if (length(x) > length(y)) {
        x <- x[1:length(y)]
      }
      
    }
    
    return(list(x = x, y = y))
    
  } else {
    return(list(x = NULL, y = NULL))
  }
  
}

generate_nls_model <- function(
  x, y, minRTT = 1, maxRTT = 4, inc_min = T, inc_max = F, start_params = list(a = 140, a1 = 140, b = 160, n1 = 3, n2 = 4)
) {
  
  if (length(x) == 0 | length(y) == 0) {
    return(list(x = x, y = y, model_type = NULL, m = NULL))
  }
  
  if (inc_min == T & min(x) >= minRTT | inc_min == F & min(x) > minRTT | inc_max == T & max(x) <= maxRTT | inc_max == F & max(x) < maxRTT) {
    return(list(x = x, y = y, model_type = NULL, m = NULL))
  }
  
  m <- tryCatch(
    suppressWarnings(nls(
      y ~ airspeed_model_vector_break(x, a, a1, b, n1, n2),
      start = list(a = start_params[["a"]], a1 = start_params[["a1"]], b = start_params[["b"]], n1 = start_params[["n1"]], n2 = start_params[["n2"]]),
      control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
    )),
    error = function(e) NULL
  )
  model_type <- "NLS"
  
  if (is.null(m)) {
    x <- x %>% .[. <= 2]
    y <- y[1:length(x)]
    m <- tryCatch(
      suppressWarnings(nls(
        y ~ airspeed_model_vector_break_simplified(x, a, a1),
        start = list(a = start_params[["a"]], a1 = start_params[["a1"]]),
        control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
      )),
      error = function(e) NULL
    )
    model_type <- "NLS_Simplified"
  }
  
  if (is.null(m)) {
    m <- tryCatch(
      suppressWarnings(nls(
        y ~ a,
        start = list(a = start_params[["a"]]),
        control = nls.control(tol = 0.001, minFactor = 1/ 16384, warnOnly = T)
      )),
      error = function(e) NULL
    )
    model_type <- "NLS_Simplified_2"
  }
  
  if (is.null(m)) {
    return(list(x = x, y = y, model_type = NULL, m = NULL))
  } else {
    return(list(x = x, y = y, model_type = model_type, m = m))
  }
  
}












parameter_summary <- function(dat, sum_var, sum_param) {
  dat[[sum_param]] <- as.numeric(dat[[sum_param]])
  x <- ddply(
    dat, sum_var, summarise,
    N = length(eval(parse(text=sum_param))),
    mean = mean(eval(parse(text=sum_param)), na.rm = T),
    median = median(eval(parse(text=sum_param)), na.rm = T),
    sd = sd(eval(parse(text=sum_param)), na.rm = T),
    p5 = quantile(eval(parse(text=sum_param)), 0.05, na.rm = T),
    p10 = quantile(eval(parse(text=sum_param)), 0.1, na.rm = T)
  )
  x$Type <- sum_param
  return(x)
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

# createBook <- function() {
#   createWorkbook(type="xlsx")
# }
# 
# writeCell <- function(worksheet, row_index, col_index, value) {
#   r <- createCell(createRow(worksheet, rowIndex = row_index), colIndex = col_index)
#   setCellValue(r[[1,1]], value)
# }
# 
# writeRow <- function(worksheet, row_index, col_indices, values) {
#   r <- createCell(createRow(worksheet, rowIndex = row_index), colIndex = 1:max(col_indices))
#   for (i in 1:length(col_indices)) {
#     setCellValue(r[[1,col_indices[i]]], values[i])
#   }
# }
# 
# writeTable <- function(worksheet, row_index, col_index, data_table) {
#   addDataFrame(data_table, worksheet, startRow = row_index, startColumn = col_index, row.names = F)
# }
# 
# writeImage <- function(worksheet, row_index, col_index, img) {
#   addPicture(img, worksheet, scale = 1, startRow = row_index, startColumn = col_index)
# }
# 
# saveBook <- function(workbook, filename) {
#   saveWorkbook(workbook, filename)
# }

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

# ----------------------------------------------------------------------- #
# Server ------------------------------------------------------------------
# ----------------------------------------------------------------------- #

optimised_runway_delivery_server <- function(input, output, session, con) {
  
  ns <- session$ns
  
  # ----------------------------------------------------------------------- #
  # ORD Calibration ---------------------------------------------------------
  # ----------------------------------------------------------------------- #
  
  observeEvent(input$cali_speed_type, {
    if (input$cali_speed_type == "Mode S IAS") {
      output$cali_more_options <- renderUI({})
    } else {
      output$cali_more_options <- renderUI({
        textInput(ns("cali_apt_alt"), "Airport Altitude (ft)", 550, width="160px")
      })
    }
  })
  
  observe({
    req(input$cali_single_run)
    isolate({
      output$cali_single_outputs <- renderUI({
        div(
          hr(),
          plotlyOutput(ns("cali_single_plot")),
          div(style = "height: 5px"),
          verbatimTextOutput(ns("cali_single_params")),
          DT::dataTableOutput(ns("cali_single_table"))
        )
      })
    })
  })
  
  nls <- reactive({
    req(input$cali_single_run)
    isolate({
      tracks <- query_ffpid_tracks(input$cali_single_ffpid, con)
      nls_input <- process_tracks_for_nls(tracks, speed_type = input$cali_speed_type, airport_alt = ifelse(is.null(input$cali_apt_alt), 0, input$cali_apt_alt))
      model <- generate_nls_model(x = nls_input$x, y = nls_input$y)
      return(model)
    })
  })
  
  observeEvent(nls(), {
    output$cali_single_plot <- renderPlotly({
      p <- plot_ly() %>%
        add_markers(
          x = nls()$x,
          y = nls()$y,
          name = "Raw Data",
          marker = list(color = "rgb(128,34,69)")
        ) %>%
        layout(
          hovermode = "compare",
          xaxis = list(title = "Follower Range to Threshold (NM)"),
          yaxis = list(title = paste(ifelse(input$cali_speed_type != "Mode S IAS", "Estimated ", ""), "Mode S IAS (kts)"))
        ) %>%
        config(
          displaylogo = F
        )
      if (!is.null(nls()$m)) {
        p <- p %>% add_lines(
          x = nls()$x,
          y = nls()$m$m$fitted(),
          name = "Fitted Profile",
          line = list(color = "rgb(213,16,103)")
        )
      }
      ggplotly(p, width = session$clientData$output_cali_single_plot_width)
    })
    output$cali_single_params <- renderPrint(nls())
    output$cali_single_table <- DT::renderDataTable({
      datatable_customised_1(query_ffpid_tracks(input$cali_single_ffpid, con))
    }, server = F)
  })
  
}
