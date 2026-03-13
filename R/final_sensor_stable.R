
TROLL_sensor_stable <- function(df,
                                value_col = 'pH_units',
                                min_secs = 5,               # number of obs required for median calculation (set to 1 if you want to keep as few as just the final obs in each stationary group)
                                # sampling_int = 2,        # seconds
                                slope_thresh = 0.05,     # units/minute
                                range_thresh = 0.02,
                                stationary_thresh = 998){# remove_jiggle = TRUE
  # 0. --- Add flag column for stability --- ----
  value_flag_col <- paste0(value_col,"_stable")
  # 1. --- Input/Validation checks --- ----

  # Check required columns exist
  req_cols <- c('DateTime','depth_m','stationary_depth','stationary_block_id','is_stationary_status')

  if(!all(req_cols %in% names(df))){
    missingCols <- req_cols[!req_cols %in% names(df)]

    # Add indentation to each column name and join with newlines
    missing_list <- paste0("  - ", missingCols, collapse = "\n")

    stop(paste0("Missing columns:\n", missing_list,
                '\n\nBe sure to run "is_stationary" first to identify stationary blocks.'))
  }

  # Check min_secs and slope_thresh are positive numeric
  if( !is.numeric(min_secs) || !is.numeric(slope_thresh) ){
    stop('\nBoth "min_secs" and "slope_thresh" must be positive numeric values.')
  }
  if(!(min_secs >= 0 && slope_thresh >= 0)){
    stop('\nBoth "min_secs" and "slope_thresh" must be positive numeric values.')
  }
  # Check that sensor data exist
  # Extract parameter column names in df
  params <- names(df)[which(names(df) %in% troll_column_dictionary$canonical[troll_column_dictionary$core_param])]

  if(length(params) <1 ){
    stop('No sensor data columns identified. Column names must be standardized using the "TROLL_rename_cols" function.')
  }

  # Check that stationary blocks exist above the stationary_thresh
  if(!is.numeric(stationary_thresh) || !stationary_thresh > 0){
    stop('\n "stationary_thresh must be a positive numeric greater than 0')
  }
  z_stationary <- unique(df$stationary_depth[df$is_stationary_status > stationary_thresh])

  if(length(z_stationary) < 1){
    max_stnthresh <- max(df$is_stationary_status,na.rm = T)
    stop(paste0('No stationary data identified using given "stationary_thresh" value.\n',
                'Max "is_stationary_status = ', max_stnthresh,' seconds.'))
  }

  message(paste0('Stationary depths at:\n',
                 paste0(round(z_stationary,2),collapse = '\n'),
                 '\nfound in the data'))

  # 2. --- Calculate min_obs to keep --- ----
  samp_int <- get_sample_interval(datetime_data = df$DateTime, output_units = 'secs',tol_prop = 1)

  min_obs <- ceiling(min_secs / samp_int)
  # Check that enough data points in every stationary block to accomodate min_obs
  blocksizes <- df |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::filter(is_stationary_status > stationary_thresh) |>
    dplyr::mutate(
      block_n = dplyr::n()
    ) |>
    dplyr::pull(block_n) |>
    unique()

  # Strict stop for now, could update to use full block instead with warning later if desired
  if(any(blocksizes < min_obs)){
    stop(paste0('\nToo few observations exist in at least one stationary block to accomodate "min_secs" requirement.\n',
         'Reduce "min_secs" to be below the shortest stationary block: \n',
         min(blocksizes,na.rm = TRUE)*samp_int))
  }

  # 3. --- Filter data into stationary blocks --- ----
  dat_base <- df |>
    dplyr::ungroup() |>
    dplyr::filter(
      is_stationary_status > stationary_thresh,
      !is.na(.data[[value_col]])  # remove rows with NA in the value_col data
    )
  # 4. --- Group data into blocks, and split groups into a list --- ----

  dat_grouped <- dat_base |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")) # Standardize time into minutes from start of data
    ) |>
    dplyr::group_split()
  # 5. --- Compute range and slope over stationary blocks --- ----
  # Pre-allocate output object
  out_list <- vector("list", length(dat_grouped))

  for (i in seq_along(dat_grouped)) { # seq_along gives list length as 1:length(list)

    stable_group_dat <- dat_grouped[[i]]                    # Extract one stable group of data

    n <- nrow(stable_group_dat)                             # length of the stable block (rows)

    # Stop execution if block is empty
    if(n==0){
      msg_depth <- unique(stable_group_dat$stationary_depth)
      stop(paste0('Zero data returned for depth ',msg_depth))
    }

    # Pre-allocate group output dataframe
    group_out <- tibble::tibble(
      stationary_block_id = rep(stable_group_dat$stationary_block_id[1], n),
      DateTime = stable_group_dat$DateTime,
      n_used = NA_integer_,
      slope = NA_real_,
      range = NA_real_,
      n_dropped = NA_integer_,
      slope_ok = FALSE,
      range_ok = FALSE
    )

    # Add sensor flag where both slope/range thresholds are met, This dynamically allows different sensor names
    group_out[[value_flag_col]] <- FALSE

    # **Reset good_flag per group** (marker to keep final "_stable" flat as TRUE once one row meets range + slope thresholds combined)
    good_flag <- FALSE

    # Set min_obs rows slope and range OK flags to keep data if threshold is never met
    if(nrow(group_out) < min_obs){
      msg_depth <- unique(stable_group_dat$stationary_depth)
      warning(paste0('The given "min_obs" is larger than the number of stable data identified for ',value_col,' at',msg_depth,'.'))
    }

    tailstart <- max(1, nrow(group_out) - min_obs + 1) # Number of rows to keep if min_obs only

    group_out$slope_ok[tailstart:nrow(group_out)] <- TRUE
    group_out$range_ok[tailstart:nrow(group_out)] <- TRUE
    group_out[[value_flag_col]][tailstart:nrow(group_out)] <- TRUE

    # assign sensor data and time to variables
    x_all <- stable_group_dat$t
    y_all <- stable_group_dat[[value_col]]

    # Create a safety net in case the # of data rows < min_obs
    if(n < min_obs) {  # If fewer rows of data exist than the min_obs as specified
      n_windows <- n # Use all available data to compute statistics (slope, range)
      msg_depth <- unique(stable_group_dat$stationary_depth)
      warning(paste0('The given "min_obs" is larger than the number of stable data identified for ',value_col,' at',msg_depth,'.'))
    } else {         # Or if more data exist than min_obs rows
      n_windows <- n - min_obs + 1 # Evaluate stats for the number of rows (n) minus the tail (min_obs)
    }

    for(win_start in seq_len(n_windows)) {                  # Because seq_along(0) = 1, this always works
      # Create an index to subset group data on from the start of the window to the end of the stable data block
      idx <- win_start:n

      # Subset data using window index
      x <- x_all[idx]
      y <- y_all[idx]

      # --- Calculate stats ---

      # Slope
      v <- stats::var(x)

      slope_fit <- if(is.na(v) || v == 0) {
        NA_real_
      } else {
        stats::cov(x, y) / v
      }

      if(is.na(slope_fit)){
        warning(paste0("Single datapoint used for ",unique(stable_group_dat$stationary_depth),", slope could not be calculated"))
      }

      # Range
      win_range <- max(y) - min(y)

      # Add stats and metadata to output object
      group_out$slope[win_start] <- slope_fit
      group_out$range[win_start] <- win_range

      group_out$n_dropped[win_start] <- win_start - 1
      group_out$n_used[win_start] <- length(idx)

      # Flag columns that meet slope/range
      slope_good <- abs(slope_fit) <= slope_thresh
      if(slope_good) {
        group_out$slope_ok[win_start] <- TRUE
      }

      range_good <- win_range <= range_thresh
      if(range_good){
        group_out$range_ok[win_start] <- TRUE
      }

      # Combined flag if both conditions are met
      if((slope_good && range_good) || good_flag){
        group_out[[value_flag_col]][win_start] <- TRUE
        good_flag <- TRUE
      }

    }
    out_list[[i]] <- group_out
  }
  # Compile the group_out into a dataframe
  out <- dplyr::bind_rows(out_list)

  # join the out metadata with the input dataframe
  final <- df |>
    dplyr::left_join(out, by = c("DateTime","stationary_block_id"))

  return(final)
}


xx <-
  TROLL_sensor_stable(df = dat_stationary,
                      value_col = 'pH_units',
                      stationary_thresh = 998,
                      min_secs = 5,
                      slope_thresh = 0.05);xx


