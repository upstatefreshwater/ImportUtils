library(tidyverse)
library(lubridate)
library(conflicted)
library(cowplot)
library(purrr)
library(rstatix)
library(janitor)
library(dplyr)
library(zoo)

select <- dplyr::select
filter <- dplyr::filter

overwrite.dat <- F
overwrite.plt <- F

TROLL_CSV_cleaner <- function(file_path, turb_value = 50, meter_halfm = "whole") {
  # Read in the file ----

  # Read the file to identify the lines
  file_content <- readLines(file_path)

  # Find the line number that contains "Date Time"
  start_line <- grep("Date Time", file_content)
  if (is.na(start_line)) stop("Header row with 'Date Time' not found.") # Check for Date Time Row Existing

  # Read the CSV starting from the identified line (Date Time)
  data <- read_csv(file_path, skip = start_line - 1)

  # Rename Columns ----
  # name second temperature column - internal temperature - if included in the spreadsheet
  if ("Temperature (°C) (1153542)" %in% names(data)) {
    data <- data %>% rename(Internal_temperature_C = `Temperature (°C) (1153542)`)
  }

  # Use gsub to remove undesired (####) in column names
  cleaned_col_names <- gsub("\\s*\\(\\d+\\)","",colnames(data))
  # Set the cleaned names as the column names
  colnames(data) <- cleaned_col_names

  # Checks required DateTime and Depth columns are present
  ##***************************************************##
  ##*Note: You already checked that "Date Time" exists above
  ##*This doesn't break anything, but it's redundant!
  ##***************************************************##
  required <- c("Date Time", "Depth (m)")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse=", "))

  # Selecting ONLY parameters (+ lat/longs and whether samples were marked) & Renaming them
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

  data <- data%>%select(any_of(param_col))%>%rename(any_of(param_rename))%>%
    # Columns for Depth Rounded to the Half Meter and Whole Meter
    mutate(depth_halfm = (round(depth_m/0.5)*0.5), depthwholem = (round_half_up(depth_m)))





  # Remove Bottom-up data & Hitting the Bottom ----
  max_wholem <- max(data$depthwholem) # Find max whole meter
  all_max_wholem <- which(data$depthwholem == max_wholem) # indices of the rows with the max whole m
  last_max_wholem <- tail(all_max_wholem, 1) # highest index # with the max whole m

  max_halfm <- max(data$depth_halfm) # Find max half meter
  all_max_halfm <- which(data$depth_halfm == max_halfm) # indices of the rows with the max half m
  last_max_halfm <- tail(all_max_halfm, 1) # highest index # with the max half m

  data <- data%>%mutate(id = row_number())

  # Error if the majority of the dataset was a Bottom-up profile
  if(mean(diff(data$depth_m), na.rm = TRUE) < 0){
    stop("Bottom-up Profile Recorded")
  }

  # Remove Bottom-up from the Max Whole or Half Meter

  if(meter_halfm == "whole" & any(last_max_wholem < max(data$id))){

    data <- data[1:last_max_wholem, ]

    message("Recorded data after the max whole meter depth removed")

  }

  if(meter_halfm == "half" & any(last_max_halfm < max(data$id))){

    data <- data[1:last_max_halfm, ]

    message("Recorded data after the max half meter depth removed")

  }

  # If there is high turbidity that indicates hitting the bottom then remove that meter
  if("turbidity_NTU" %in% names(data)){
    if(meter_halfm == "half" & any(data$turbidity_NTU > turb_value, na.rm = TRUE)){
      # Dataset with only the high turbidity values (hit the bottom - level high)
      hit_bottom <- data%>%filter(turbidity_NTU >= turb_value) # turb_value is = 50 unless changed
      hit_bottom_depth <- min(hit_bottom$depth_halfm) # Lowest Depth of Very HIGH Turbidity
      # Remove all the very high turbidity
      data <- data%>%filter(depth_halfm < hit_bottom_depth)

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



  # Check for Depths with too short time period of stopping ----
  if(meter_halfm == "whole"){
    bad_stops <- data%>%group_by(depthwholem)%>%
      summarize(n())%>% rename(depths = "depthwholem",
                               n_value = "n()")%>%filter(n_value < 15)%>%
      ungroup()
  }

  if(meter_halfm == "half"){
    bad_stops <- data%>%group_by(depth_halfm)%>%
      summarize(n())%>% rename(depths = "depth_halfm",
                               n_value = "n()")%>%filter(n_value < 15)%>%
      ungroup()
  }

  if(nrow(bad_stops) > 0){

    bad_stop_list <- paste(bad_stops$depths, collapse = ", ")

    message(paste("Depth(s):",bad_stop_list, "m had less than 15 measurements (less than 30 sec stop)"))
  }

  return(data)

  # Averaging Stabilized Data Code ----
  if(meter_halfm == "whole"){
    data_final <- data%>% group_by(depthwholem)%>%arrange(id)%>%
      mutate( roll_sd = rollapply(DO_mgL, 5, sd, fill = NA),
              roll_mean = rollapply(DO_mgL, 5, mean, fill = NA),
              slope = abs(c(NA, diff(roll_mean))) )%>%
      # Require stability AND low slope
      mutate(stable = roll_sd < 0.20 & slope < 0.10) %>%
      # Keep only stable points AFTER first unstable change
      filter(row_number() > max(which(slope > 0.20), na.rm = TRUE)) %>%
      filter(stable)%>% summarise(across( -c(DateTime, depth_halfm, id),
                                          ~ mean(.x, na.rm = TRUE) ), .groups = "drop")%>%
      select(any_of(c("depthwholem", "depth_m", "temperature_C", "sp_conductivity_uScm",
                      "pH_units", "pH_mV", "DO_mgL", "DO_per", "turbidity_NTU", "ORP_mV",
                      "chlorophyll_RFU", "bga_fluorescence_RFU", "pressure_psi",
                      "latitude_deg", "longitude_deg"))) }

  if(meter_halfm == "half"){
    data_final <- data%>% group_by(depth_halfm)%>%arrange(id)%>%
      mutate( roll_sd = rollapply(DO_mgL, 5, sd, fill = NA),
              roll_mean = rollapply(DO_mgL, 5, mean, fill = NA),
              slope = abs(c(NA, diff(roll_mean))) )%>%
      # Require stability AND low slope
      mutate(stable = roll_sd < 0.20 & slope < 0.10) %>%
      # Keep only stable points AFTER first unstable change
      filter(row_number() > max(which(slope > 0.20), na.rm = TRUE)) %>%
      filter(stable)%>% summarise(across( -c(DateTime, depthwholem, id),
                                          ~ mean(.x, na.rm = TRUE) ), .groups = "drop")%>%
      select(any_of(c("depth_halfm", "depth_m", "temperature_C", "sp_conductivity_uScm",
                      "pH_units", "pH_mV", "DO_mgL", "DO_per", "turbidity_NTU", "ORP_mV",
                      "chlorophyll_RFU", "bga_fluorescence_RFU", "pressure_psi",
                      "latitude_deg", "longitude_deg"))) }

  return(data_final)

}

xx <-
TROLL_CSV_cleaner(file_path = 'inst/extdata/Lake_Tiorati_1.csv')
