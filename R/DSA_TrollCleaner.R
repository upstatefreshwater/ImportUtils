# read_datafile ----
read_datafile <- function(path){
  # Check that file exists at the specified path
  if(!file.exists(path)){
    stop("File does not exist at the specified path:\n", path)
  }
  # Read the file to identify the start line
  file_content <- readLines(path)

  # Find the line number that contains "Date Time"
  start_line <- grep("Date Time", file_content)
  if (length(start_line)==0) {
    stop("Cannot locate 'Date Time' in the datafile.\n
        Check that 'Date Time' exists in the raw data.") # Check for Date Time Row Existing
  }
  if (length(start_line) > 1) {
    stop("Multiple lines containing 'Date Time' were found. Cannot determine which one is the header row.")
  }
  # Read the CSV starting from the identified line (Date Time)
  data <- readr::read_csv(path,
                          skip = start_line - 1,
                          show_col_types = FALSE)

  # Double check (could help with debugging)
  if (!("Date Time" %in% names(data))) {
    stop("Column 'Date Time' was not found after reading the file. Check file structure.")
  }

  raw_dt <- data$`Date Time`

  # Attempt strict parsing WITH seconds
  parsed_hms <- lubridate::parse_date_time(
    raw_dt,
    orders = c("ymd HMS", "mdy HMS"),
    quiet = TRUE
  )

  if (any(is.na(parsed_hms))) { # If any are returned as NA (parsing failed)

    # Try minute-only formats to diagnose cause
    parsed_hm <- lubridate::parse_date_time(
      raw_dt,
      orders = c("ymd HM", "mdy HM"),
      quiet = TRUE
    )

    if (all(!is.na(parsed_hm))) {
      stop("Timestamp format error: seconds are missing from 'Date Time'.\n",
           "Second-level resolution is required. Please check the raw data file.")
    } else {
      stop("Timestamp format error: could not parse 'Date Time' column.\n",
           "Expected formats: ymd HMS or mdy HMS.")
    }
  }

  return(data )
}

# rename_cols ----
rename_cols <- function(data,
                        print_colnames = FALSE){

  # ---- Special-case Troll COM temperature ----
  # Extract columns that rely on the TROLL-comm (Baro press, internal temp)
  comm_cols <- names(data)[stringr::str_detect(
    names(data),
    paste(trollCOMM_serials, collapse = "|")
  )]

  # If no TROLL-comm data columns, just present a message to the console
  if (length(comm_cols) == 0) {

    message("No TROLL-COM data columns detected.")

    # If there are data columns check that there is only a single s/n
  } else {
    # Extract the serial numbers in the data based on the known TROLL-comm serials
    comm_sn <- stringr::str_extract(
      comm_cols,
      paste(trollCOMM_serials, collapse = "|")
    )

    # pull the serial(s)
    comm_sn <- unique(comm_sn[!is.na(comm_sn)])

    # If only one serial detected, rename the temp to avoid conflicts with water temp
    if (length(comm_sn) == 1) {

      comm_tempcol <- paste0("Temperature (°C) (", comm_sn, ")") # Need to apply the correct s/n in column name

      if (comm_tempcol %in% names(data)) {

        message(
          "TROLL-COM temperature detected (serial ", comm_sn, ").\n",
          "Renaming column: ", comm_tempcol
        )

        data <- data |>
          dplyr::rename(
            Trollcom_temperature_C = !!rlang::sym(comm_tempcol) # Needs tidy-eval selection here
          )

      }

    } else {

      warning(
        "Multiple TROLL-COM serials detected: ",
        paste(comm_sn, collapse = ", "),
        ".\nCannot automatically select which temperature column to use."
      )

    }
  }

  # ---- Clean column names ----
  cleaned_col_names <- gsub("\\s*\\(\\d+\\)", "", names(data))
  names(data) <- cleaned_col_names

  # ---- Column selection ----
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
  if(print_colnames){
  column_list <- paste(colnames(data))
  message("The CSV has Columns:\n", paste(column_list, collapse = "\n"))
  }

  return(data |>
           dplyr::arrange(DateTime))
}

# remove_bottomup_hitbottom ----
remove_bottomup_hitbottom <- function(data, turb_value = 50, stationary_velocity=0.1,
                                      meter_halfm = "whole"){
  data <- data|>dplyr::mutate(depth_halfm = (round(depth_m/0.5)*0.5),
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
      hit_bottom <- data|>dplyr::filter(turbidity_NTU >= turb_value) # turb_value is = 50 unless changed
      hit_bottom_depth <- min(hit_bottom$depth_halfm) # Lowest Depth of Very HIGH Turbidity
      # Remove all the very high turbidity
      data <- data|>dplyr::filter(depth_halfm < hit_bottom_depth)

      message(paste("Data Removed Below",hit_bottom_depth, "m"))}

    if(meter_halfm == "whole" & any(data$turbidity_NTU > turb_value, na.rm = TRUE)){
      # Dataset with only the high turbidity values (hit the bottom - level high)
      hit_bottom <- data|>filter(turbidity_NTU >= turb_value) # turb_value is = 50 unless changed
      hit_bottom_depth <- min(hit_bottom$depthwholem) # Lowest Depth of Very HIGH Turbidity
      # Remove all the very high turbidity
      data <- data|>filter(depthwholem < hit_bottom_depth)

      message(paste("Data Removed Below",hit_bottom_depth, "m for Hitting the Bottom"))}

  }else{

    stop("No Turbidity Column to Indicate Hitting the Bottom")

  }


  return(data)
}

# depth_rounder ----
depth_rounder <- function(df,
                          depth_col = depth_m,
                          interval = 1,
                          tolerance = 0.2){

  # Internal helper to extract decimal places
  decimalplaces <- function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }

  tol_dec <- decimalplaces(tolerance)
  if(tol_dec>2) stop(paste0('Depth tolerance value: ',tolerance,' is unrealistically small.'))

  if(!is.null(interval)){ # If regular intervals enter this loop
  int_dec <- decimalplaces(interval)
  if(int_dec>2) stop(paste0('Depth interval: ',interval,' is unrealistically small.'))

  data_out <- df |>
    dplyr::mutate(
      obs_depth = round({{depth_col}}, tol_dec),                        # Only keep precision to tolerance level
      nearest_interval = round(obs_depth / interval) * interval,        # This checks for closest given interval, allowing decimal intervals too
      # Round the difference to 6 decimals to avoid floating-point issues
      flag_depth = dplyr::case_when(
        obs_depth < 0.5 ~ '',
        round(abs(obs_depth - nearest_interval), 6) > tolerance ~ 'flag', # This does the work
        TRUE ~ ''
      ),
      obs_depth = round(obs_depth / interval) * interval
    ) |>
    dplyr::select(-nearest_interval)
  }

  if(is.null(interval)){
    stop('DA needs to update the function to allow irregular intervals. I guess you are on your own!\nIf you meant to use regular depth intervals, check your "target_depths" specification. ')
  }


  return(data_out)
}

# strip_meta -----
strip_meta <- function(df) {
  # 1. Remove the specific metadata columns
  data_out <- df |>
    dplyr::select(-any_of(c('pressure_psi',
                            'latitude_deg',
                            'longitude_deg')))

  # 2. Check if 'Marked' exists and if it consists ONLY of NAs
  if ('Marked' %in% names(data_out)) {
    if (all(is.na(data_out$Marked))) {
      data_out <- data_out |> dplyr::select(-Marked)
    }
  }

  # 3. Crucial: Return the data frame
  return(data_out)
}

# troll_run_stats ----
troll_run_stats <- function(df,
                            datetime_col = DateTime,
                            stationary_col = is_stationary_status,
                            round_depth_col = obs_depth,
                            stationary_secs = 30,
                            target_depths = NULL,
                            target_depth_int = 1) {
  # Pre-allocate to avoid issues within loops
  depthvals_samples <- NA
  n_stationary_depths <- NA
  # Checks on target depths ----
  # Check that target_depths is a numeric vector

  if (!is.null(target_depths)) {
    if (!is.numeric(target_depths) || is.matrix(target_depths)) {
      stop("`target_depths` must be a numeric vector or NULL.", call. = FALSE)
    }

    depth_check <- diff(target_depths)
    depth_check <- depth_check[!is.na(depth_check)]

    tol <- sqrt(.Machine$double.eps) # make robust to floating point errors

    if (!all(abs(depth_check - target_depth_int) < tol)) {
      stop(
        'Provided "target_depths" are not evenly incremented by "target_depth_int".',
        call. = FALSE
      )
    }
  }

  df_stat <- df

  # Calculate sampling interval ----
  times <- df_stat |> dplyr::pull({{ datetime_col }})
  samp_int <-  as.numeric(median(diff(times), na.rm = TRUE), units = 'secs')      # Calculate the sampling interval

  if (!length(unique(na.omit(diff(times)))) == 1) {
    warning('Inconsistent sampling intervals detected.')
  }

  # Calculate cast length ----
  cast_len <- as.numeric(max(times, na.rm = TRUE) - min(times, na.rm = TRUE), units = 'mins')

  # Calculate the max depth

  # Calculate the number of stationary periods in cast ----
  # Check stationary column existence safely
  stationary_name <- rlang::as_name(rlang::ensym(stationary_col))

  if (!stationary_name %in% names(df_stat)) {
    warning(
      "No stationary flag column in input dataframe.\nCould not calculate the number of stationary periods during cast."
    )
  } else{
    stationary_status <- dplyr::pull(df_stat, {{ stationary_col }})               # Pull out the stationary column (reports length of time (s) sonde is stationary)
    stationary_periods <- dplyr::consecutive_id(stationary_status)                # Create an index of the blocks of stationary/moving observations

    stationary_dat <- data.frame(stationary_status, stationary_periods)            # A dataframe to facilitate extracting stationary periods when status meets time criteria
    # Extract the number of depths where sonde was stationary for > stationary_secs
    n_stationary_depths <- length(unique(stationary_dat$stationary_periods[stationary_dat$stationary_status >
                                                                             stationary_secs]))

    # Extract the actual depths of stationary blocks (> stationary_secs) ----
    # Check stationary column existence safely
    depth_name <- rlang::as_name(rlang::ensym(round_depth_col))

    if (!depth_name %in% names(df_stat)) {
      warning(
        "No final depth column in input dataframe.\nThis refers to the rounded target depth column, not the raw depth data."
      )
    } else{
      final_stationary_depths <- dplyr::pull(df_stat, {{ round_depth_col }}) # These are all of the depths including repeats
      stationary_dat <- cbind(stationary_dat, final_stationary_depths)        # Add to the previous dataframe
      # Unique values of the rounded depths during stationary periods
      depthvals_samples <- unique(stationary_dat$final_stationary_depths[stationary_dat$stationary_status > stationary_secs])
    }
  }

  # Compile results as a list ----
  out_list <- list(
    samp_int = samp_int,
    # Sampling interval
    cast_len = cast_len,
    # Cast length
    # stationary_periods = stationary_periods,
    num_stationary_depths = n_stationary_depths,
    # Number of depth where sonde is stationary for at last the "stationary_secs"
    final_depths = depthvals_samples                             # The rounded depths of the stationary sonde periods > "stationary_secs"
  )

  return(out_list)
}
# is_stationary ----
is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          sd_thresh = 0.05,
                          window = 7,
                          stationary_secs = 30,
                          sampling_int = 0,
                          drop_cols = TRUE,
                          plot = FALSE) {

  if (nrow(df) < 2) {
    stop("Data frame must contain at least two observations.")
  }

  # If sampling interval is not provided, calculate it
  if(sampling_int==0){
    # Extract datetime vector safely for interval calculation
    times <- df |> dplyr::pull({{ datetime_col }})

    samp_int <-  as.numeric(median(diff(times), na.rm = TRUE),units = 'secs')   # Calculate the sampling interval

    if(!length(unique(diff(times),na.rm = T))==1){
      warning('Inconsistent sampling intervals detected.')
    }

    if(samp_int > 30){
      message('Sampling interval > 30s detected. Sonde assumed to be fixed in position.')
      return(df |> dplyr::mutate(is_stationary_status = 999))
    }
  } else{
    samp_int <- sampling_int
  }

  # Set the number of obs equal to the min reqd_stationary_obs
  min_obs <- ceiling(stationary_secs / samp_int)

  out <- df |>
    dplyr::mutate(
      depth_sd = zoo::rollapplyr({{ depth_col }},                                # 'right' means from the obs. look backwards n = window
                                 width = window,
                                 FUN = stats::sd,                                # Calculate the rolling SD of the window
                                 na.rm = TRUE,
                                 fill = NA),
      is_stationary_initial = dplyr::if_else(
        is.na(depth_sd),
        FALSE,
        depth_sd < sd_thresh),                                                   # Sets is_stationary_initial based on only SD
      group_id = dplyr::consecutive_id(.data$is_stationary_initial)) |>          # assigns a unique id based on each time is_stationary_initial changes
    dplyr::group_by(.data$group_id) |>
    dplyr::mutate(
      block_duration = dplyr::n(),                                               # counts the number of obs in each stable period (works by group)
      is_stationary_status = dplyr::case_when(
        .data$is_stationary_initial & .data$block_duration >= min_obs ~ 999,     # If there are enough observations after stationary detected, mark as stationary
        .data$is_stationary_initial &
          .data$block_duration < min_obs ~ .data$block_duration * samp_int,      # For fewer than the minimum # of obs, return the number of seconds sonde was stationary
        TRUE ~ 0                                                                 # Mark moving as 0
      )
    ) |>
    dplyr::ungroup()

  if(plot){
    # 1. Get all unique levels
    levels_list <- as.character(unique(out$is_stationary_status))
    ncolors <- length(levels_list)

    # 2. Generate random colors for everything first
    # We name them so ggplot knows exactly which color goes to which level
    mycolors <- setNames(sample(colors(distinct = TRUE), ncolors), levels_list)

    # 3. Explicitly overwrite your "known" values
    mycolors['999'] <- 'darkgreen'
    mycolors['0']   <- 'firebrick'
    # 2. Visualize to verify the threshold
    p1 <- ggplot2::ggplot(out, ggplot2::aes(x = seq_len(nrow(out)), y = {{depth_col}})) +
      ggplot2::geom_line(alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(color = as.factor(is_stationary_status)), size = 1.2) +
      ggplot2::scale_y_reverse() + # Depth plots usually go down
      ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
      ggplot2::scale_color_manual(name = 'Seconds Stationary',
                                  values = mycolors) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(out,
                          ggplot2::aes(x = seq_len(nrow(out)),
                                       y = depth_sd)) +
      ggplot2::geom_line(ggplot2::aes(color = "Rolling SD",
                                      linetype = "Rolling SD")) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = sd_thresh,
                                       color = "SD Threshold",
                                       linetype = "SD Threshold")) +
      ggplot2::scale_color_manual(
        name = "",
        values = c("Rolling SD" = "red",
                   "SD Threshold" = "black")
      ) +
      ggplot2::scale_linetype_manual(
        name = "",
        values = c("Rolling SD" = "solid",
                   "SD Threshold" = "dashed")
      ) +
      ggplot2::labs(title = "Rolling Standard Deviation",
                    y = "SD (m)",
                    x = "Observation Index") +
      ggplot2::theme_minimal()

    # Combine the plots
    print(patchwork::wrap_plots(p1, p2, ncol = 1))

  }

  if (drop_cols) {                                                                # Remove intermediate columns
    out <- dplyr::select(out, -c(depth_sd, is_stationary_initial, group_id, block_duration))
  }

  return(out)
}


# remove_jiggle ----
remove_jiggle <- function(df,
                          sampling_int,
                          jiggle_secs = 15,
                          mode = 'flag',
                          stationary_flag_col = is_stationary_status,
                          stationary_flag_thresh = 998){

  # Check stationary column existence safely
  stationary_name <- rlang::as_name(rlang::ensym(stationary_flag_col))

  if (!stationary_name %in% names(df)) {
    warning(
      "No stationary flag column in input dataframe."
    )
  }

  n_jiggle <- ceiling(jiggle_secs / sampling_int)                      # number of observations to throw out for "jiggle period"

  jiggle_dat <- df |>
    dplyr::mutate(
      stationary_block_id = dplyr::consecutive_id({{ stationary_flag_col }}) # This includes moving periods
    ) |>
    # Group into stationary blocks
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      stationary_block_index = dplyr::row_number(),                    # integer index restarts on each stable block because of data grouping
      post_jiggle = {{stationary_flag_col}} > stationary_flag_thresh &
        stationary_block_index > n_jiggle) |>                 # flag the "jiggle_secs" aka the period after sonde stops moving when the field tech is supposed to slightly move sonde\
    dplyr::group_by(stationary_block_id,post_jiggle) |>
    dplyr::mutate(
      n_obs_post_jig = ifelse(post_jiggle,
                              dplyr::n(),                                      # Number of observation in each stable block
                              0)
    ) |>
    dplyr::ungroup()

  return(jiggle_dat)

}

# troll_rollRange ----
troll_rollRange <- function(df,
                            sampling_int,
                            DO_col = DO_mgL,
                            temp_col = temperature_C,
                            range_window_secs = 10,
                            DO_range_thresh = 0.1,
                            temp_range_thresh = 0.1){
  if(!('stationary_block_id' %in% names(df))){
    stop('"stationary_block_id" column not found. Run `is_stationary()` on raw data first.')
  }

  n_range_window <- ceiling(range_window_secs / sampling_int)              # this is the "width" argument of rollapply in units of "rows"/"observations"

  range_dat <- df |>
    dplyr::group_by(stationary_block_id) |>                            # This groups the data by stationary blocks
    # Within this mutate call operations are performed on grouped data
    dplyr::mutate(
      # Calculate DO range across the window ----
      DO_range = zoo::rollapplyr(                                      # "r" at the end means "right" or "backwards looking" across the width
        ifelse(post_jiggle, {{DO_col}}, NA),                           # Only calculate for the "post_jiggle" period
        width = n_range_window,
        FUN = function(x) if (all(!is.na(x))) max(x) - min(x) else NA,  # Computes the range across the window
        fill = NA                                                       # NOTE** a number of obs are set to NA because of !all(is.na), so when width = 5 obs, 4 obs beyond post_jiggle are NA, and this increases with increasing window size
      ),
      # Flag stationary post-jiggle obs. that are below set DO range threshold ----
      DO_withinthresh = DO_range <= DO_range_thresh,                  # Only need to handle the threshold now, stationary & jiggle handled above
      DO_withinthresh = dplyr::coalesce(DO_withinthresh,FALSE),       # This replaces NAs with FALSE so consecutive_id works

      # Calculate Temp range across the window -----
      temp_range = zoo::rollapplyr(                                      # "r" at the end means "right" or "backwards looking" across the width
        ifelse(post_jiggle, {{temp_col}}, NA),                           # Only calculate for the "post_jiggle" period
        width = n_range_window,
        FUN = function(x) if (all(!is.na(x))) max(x) - min(x) else NA,  # Computes the range across the window
        fill = NA                                                       # NOTE** a number of obs are set to NA because of !all(is.na), so when width = 5 obs, 4 obs beyond post_jiggle are NA, and this increases with increasing window size
      ),
      # Flag stationary post-jiggle obs. that are below set temp range threshold ----
      temp_withinthresh = temp_range <= temp_range_thresh,             # Only need to handle the threshold now, stationary & jiggle handled above
      temp_withinthresh = dplyr::coalesce(temp_withinthresh,FALSE),    # This replaces NAs with FALSE so consecutive_id works

      range_block_id = dplyr::consecutive_id(DO_withinthresh)) |>
    dplyr::ungroup()

  return(range_dat)
}



# pH --------
# For each stationary block, after the jiggle period, repeatedly fit a linear trend of pH vs time, dropping early observations one-by-one, until only min_n points remain.
# Then, based on slope behavior, decide how many of the final points should be kept as “stable”.

pH_stable <- function(df,
                      min_n = 5,               # number of obs required for median calculation (set to 1 if you want to keep as few as just the final obs in each stationary group)
                      sampling_int = 2,        # seconds
                      slope_thresh = 0.01,     # units/minute
                      stationary_thresh = 998){
  # 1. Input checks ----
  # You check required columns. Good.
  #
  # But you should also validate:
  #
  #   min_n >= 1
  #
  # sampling_int > 0
  #
  # slope_thresh >= 0
  #
  # stationary_thresh numeric
  #
  # Package code should fail early and loudly.

req_cols <- c('DateTime','depth_m','obs_depth','stationary_block_id','is_stationary_status','post_jiggle','pH_units')

if(!all(req_cols %in% names(df))){
  missingCols <- req_cols[!req_cols %in% names(df)]
  stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
}
  # 2. Data Manipulation ----

  stable_groups_split <-
    df |>
    dplyr::ungroup() |>
    dplyr::select(DateTime,depth_m,obs_depth,stationary_block_id,is_stationary_status,post_jiggle,pH_units) |>
    # Only keep stationary post jiggle pH data that are non-NA
    dplyr::filter(post_jiggle & is_stationary_status > stationary_thresh & !is.na(pH_units)) |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(t = as.numeric(difftime(DateTime, DateTime[1], units = "mins"))) |>  # scale time and make it into minutes
    dplyr::group_split()

  # 3. Compute slope statistics.
  #    Start with entire post-jiggle period, re-calculate dropping an obs. each time until min_n is reached

  # Iterate across each of the stable groups data
  out <- tibble::tibble()
    # DateTime = df$DateTime,
    #             stationary_block_id = df$stationary_block_id,
    #             stable_pH_flag = FALSE)

  for (i in seq_along(stable_groups_split)) {

    stable_group_dat <- stable_groups_split[[i]]                    # Extract one stable group of data

    # intermediate single group data holder
    group_out <- tibble::tibble(
      stationary_block_id = stable_group_dat$stationary_block_id,
      DateTime = stable_group_dat$DateTime,
      n_used = NA_real_,
      slope = NA_real_,
      n_dropped = NA_real_,
      i = 1:nrow(stable_group_dat),
      meets_thresh = FALSE)

    # pre-allocate iterators
    j = 1
    dropped <- 0

    # Calculate slope starting using all data, then take one away until only min_n rows are left
    while (nrow(stable_group_dat)>=min_n) {
      # fit <- lm(pH_units ~ t, data = stable_group_dat)                            # fit linear regression across entire data
      # slope_fit <- coef(fit)[["t"]]                                # extract the slope

      # Compute slope analytically to speed up processing time
      x_var <- stable_group_dat$pH_units
      y_var <- stable_group_dat$t

      slope_fit <- stats::cov(x_var, y_var) / stats::var(x_var)

      group_out$slope[j] <- slope_fit
      group_out$n_dropped[j] <- dropped
      group_out$n_used[j] <- nrow(stable_group_dat)

      if(abs(slope_fit) <= slope_thresh){
        group_out$meets_thresh[j] <- TRUE
      }

      stable_group_dat <- stable_group_dat[-1,]
      dropped <- dropped + 1
      j = j + 1
    }
# 4. Determine how many rows to keep ----
    # Drop NA tail rows
    # return(group_out)
    group_out <- group_out[!is.na(group_out$slope), ]
    # return(group_out)

    slopes <- group_out$slope

    never_met_thresh <- if(!any(abs(slopes) < slope_thresh)) TRUE else FALSE     # use absolute values to account for negative slopes and positive threshold value

    # If all the slope signs are pointing the same direction, or the slope_thresh was not met, keep only min_n rows
    if(all(slopes >= 0) | all(slopes <=0) | never_met_thresh){
      group_out <- tail(group_out, min_n)
    }

    # Otherwise, keep all rows after the threshold is met
    if(!never_met_thresh){
    first_below_idx <- which(abs(group_out$slope) < slope_thresh)[1]
    r <- nrow(group_out)
    # min_obs_check <- r - first_below_idx + 1
    min_obs_check <- if(!is.na(first_below_idx)) r - first_below_idx + 1 else 0 # Handle case where threshold was never met

    # If first slope threshold met results in < min_n observations, keep min_n
    if(min_obs_check < min_n){ # If min_n = 1, it allows only the final observation be kept
      group_out <- tail(group_out, min_n)
    } else{                    # If slope signal is flat enough and oscillating, keep all rows after first obs < slope_thresh
      group_out <- group_out[first_below_idx:r,]
    }

    }
    out <- dplyr::bind_rows(out, group_out)
    # out <- out |> dplyr::left_join(group_out,by = 'DateTime','stationary_block_id')
  }

  final <- df |>
    dplyr::left_join(out, by = c("DateTime","stationary_block_id"))

  return(final)
}





