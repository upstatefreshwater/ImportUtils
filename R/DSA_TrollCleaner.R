read_datafile <- function(path){

  # Read the file to identify the lines
  file_content <- readLines(path)

  # Find the line number that contains "Date Time"
  start_line <- grep("Date Time", file_content)
  if (length(start_line)==0) {
    stop("Cannot locate 'Date Time' in the datafile./n
        Check that 'Date Time' exists in the raw data.") # Check for Date Time Row Existing
  }
  # Read the CSV starting from the identified line (Date Time)
  data <- readr::read_csv(path,
                          skip = start_line - 1,
                          show_col_types = FALSE)

  # Format the Date Time column
  parsed_dates <- lubridate::parse_date_time(
    data$`Date Time`,
    orders = c('ymd HMS', 'mdy HMS', 'ymd HM', 'mdy HM'),
    quiet = TRUE
  )

  # If the first row is NA, it means none of our orders matched the data
  if (all(is.na(parsed_dates))) {
    stop(paste0("Format check failed: Could not parse 'Date Time' column with provided formats./n
            Formats tried were: ", 'ymd HMS', 'mdy HMS', 'ymd HM', 'mdy HM'))
  } else {
    # Apply the successfully parsed dates back to the dataframe
    data$`Date Time` <- parsed_dates
    message("Successfully parsed 'Date Time' column.")
  }

  if(all(is.na(lubridate::second(data$`Date Time`)))) {
    warning('No seconds were included in the raw data, check the data file formatting for "Date Time" column!')
  }

  return(data)
}

rename_cols <- function(data){
  # name second temperature column - TROLL COM temperature - if included in the spreadsheet
  if ("Temperature (°C) (1153542)" %in% names(data)) {
    data <- data %>% rename(Trollcom_temperature_C = `Temperature (°C) (1153542)`)
  }
  if ("Temperature (°C) (1151975)" %in% names(data)) {
    data <- data %>% rename(Trollcom_temperature_C = `Temperature (°C) (1151975)`)

  }

  # Use gsub to remove undesired (####) in column names
  cleaned_col_names <- gsub("\\s*\\(\\d+\\)","",colnames(data))
  # Set the cleaned names as the column names
  colnames(data) <- cleaned_col_names

  # Checks required Depth column is present
  required <- c("Depth (m)")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Cannot locate 'Depth (m)' in the datafile./n
        Check that 'Depth (m)' exists in the raw data.")

  # Check that there are more columns recorded besides Date Time and Depth
  if(ncol(data) < 3){message("Only parameters are 'Date Time' and 'Depth (m)'")}

  # Selecting ONLY necessary parameters & Renaming them
  param_col <- c("Date Time", "Depth (m)", "Temperature (°C)", "Specific Conductivity (µS/cm)", "pH (pH)", "pH mV (mV)",
                 "RDO Concentration (mg/L)", "RDO Saturation (%Sat)", "Turbidity (NTU)", "ORP (mV)",
                 "Chlorophyll-a Fluorescence (RFU)", "BGA-PC Fluorescence (RFU)", "Pressure (psi)",
                 "Latitude (°)", "Longitude (°)", "Marked")
  param_rename <- c(DateTime = "Date Time", depth_m = "Depth (m)", temperature_C = "Temperature (°C)",
                    sp_conductivity_uScm = "Specific Conductivity (µS/cm)", pH_units = "pH (pH)",
                    pH_mV = "pH mV (mV)", DO_mgL = "RDO Concentration (mg/L)", DO_per = "RDO Saturation (%Sat)",
                    turbidity_NTU = "Turbidity (NTU)", ORP_mV = "ORP (mV)",
                    chlorophyll_RFU = "Chlorophyll-a Fluorescence (RFU)", bga_fluorescence_RFU = "BGA-PC Fluorescence (RFU)",
                    pressure_psi = "Pressure (psi)", latitude_deg = "Latitude (°)", longitude_deg = "Longitude (°)")
  ## ERROR: Check that ^^ are all the important parameters that are wanted!! ##
  data <- data |>
    dplyr::select(any_of(param_col)) |>
    dplyr::rename(any_of(param_rename))

  #List the Columns in the File for the Record
  column_list <- paste(colnames(data))
  message("The CSV has Columns:\n", paste(column_list, collapse = "\n"))
  return(data)
}

remove_bottomup_hitbottom <- function(data, turb_value = 50, stationary_velocity=0.1,
                                      meter_halfm = "whole"){
  data <- data%>%dplyr::mutate(depth_halfm = (round(depth_m/0.5)*0.5),
                               depthwholem = (janitor::round_half_up(depth_m)),
                               # Determine When the Sonde is Moving
                               is_stationary = abs(depth_m - dplyr::lag(depth_m,default = dplyr::first(depth_m)))<stationary_velocity,
                               # Add Index Column
                               id = row_number())

  max_wholem <- max(data$depthwholem) # Find max whole meter
  all_max_wholem <- which(data$depthwholem == max_wholem) # indices of the rows with the max whole m
  last_max_wholem <- tail(all_max_wholem, 1) # highest index # with the max whole m

  max_halfm <- max(data$depth_halfm) # Find max half meter
  all_max_halfm <- which(data$depth_halfm == max_halfm) # indices of the rows with the max half m
  last_max_halfm <- tail(all_max_halfm, 1) # highest index # with the max half m

  # Error if the majority of the dataset was a Bottom-up profile
  if(mean(diff(data$depth_m), na.rm = TRUE) < 0){
    stop("Bottom-up Profile Recorded")
  }

  # Remove Bottom-up from the Max Whole or Half Meter

  depth_lastmaxm <- data[last_max_wholem, "depthwholem"]
  if(meter_halfm == "whole" & any(last_max_wholem < max(data$id))){

    data <- data[1:last_max_wholem, ]

    message(paste("Recorded data after", depth_lastmaxm, "m were removed for going upwards"))

  }

  depth_lastmaxhalfm <- data[last_max_halfm, "depth_halfm"]
  if(meter_halfm == "half" & any(last_max_halfm < max(data$id))){

    data <- data[1:last_max_halfm, ]

    message("Recorded data after", depth_lastmaxhalfm, "m were removed for going upwards")

  }

  # If there is high turbidity that indicates hitting the bottom then remove that meter
  if("turbidity_NTU" %in% names(data)){
    if(meter_halfm == "half" & any(data$turbidity_NTU > turb_value, na.rm = TRUE)){
      # Dataset with only the high turbidity values (hit the bottom - level high)
      hit_bottom <- data%>%dplyr::filter(turbidity_NTU >= turb_value) # turb_value is = 50 unless changed
      hit_bottom_depth <- min(hit_bottom$depth_halfm) # Lowest Depth of Very HIGH Turbidity
      # Remove all the very high turbidity
      data <- data%>%dplyr::filter(depth_halfm < hit_bottom_depth)

      message(paste("Data Removed Below",hit_bottom_depth, "m"))}

    if(meter_halfm == "whole" & any(data$turbidity_NTU > turb_value, na.rm = TRUE)){
      # Dataset with only the high turbidity values (hit the bottom - level high)
      hit_bottom <- data%>%filter(turbidity_NTU >= turb_value) # turb_value is = 50 unless changed
      hit_bottom_depth <- min(hit_bottom$depthwholem) # Lowest Depth of Very HIGH Turbidity
      # Remove all the very high turbidity
      data <- data%>%filter(depthwholem < hit_bottom_depth)

      message(paste("Data Removed Below",hit_bottom_depth, "m for Hitting the Bottom"))}

  }else{

    stop("No Turbidity Column to Indicate Hitting the Bottom")

  }


  return(data)
}
