sensor_stable <- function(df,
                          value_col = pH_units,
                          min_n = 5,               # number of obs required for median calculation (set to 1 if you want to keep as few as just the final obs in each stationary group)
                          # sampling_int = 2,        # seconds
                          slope_thresh = 0.05,     # units/minute
                          range_thresh = 0.02,
                          stationary_thresh = 998,
                          remove_jiggle = TRUE){

  # 0. Extract value column data and value column name using tidy eval
  value_col <- rlang::ensym(value_col)
  value_name <- rlang::as_name(value_col)
  # 1. Input checks ----

  req_cols <- c('DateTime','depth_m','obs_depth','stationary_block_id','is_stationary_status',value_name) # 'depth_m',

  if(!all(req_cols %in% names(df))){
    missingCols <- req_cols[!req_cols %in% names(df)]
    stop(paste("Missing columns:", paste(missingCols, collapse = ", ")))
  }

  jiggle_check <- 'post_jiggle' %in% names(df)
  if(!remove_jiggle && jiggle_check){
    warning('You have elected to include "jiggle period" data for stability determination, even though "post_jiggle" flags exist in the input data.')
  }

  if(!is.numeric(min_n) || min_n < 1) stop("min_n must be numeric >= 1")
  # if(!is.numeric(sampling_int) || sampling_int <= 0) stop("sampling_int must be > 0")
  if(!is.numeric(slope_thresh) || slope_thresh < 0) stop("slope_thresh must be >= 0")
  if(!is.numeric(stationary_thresh)) stop("stationary_thresh must be numeric")

  # 2. Data Manipulation ----
  # Filter to stationary blocks and remove any NA data values
  dat_base <- df |>
    dplyr::ungroup() |>
    dplyr::filter(
      is_stationary_status > stationary_thresh,
      !is.na(.data[[value_name]])
    )

  # If jiggle period is flagged in the "post_jiggle" column, and user wishes to remove those rows of data
  if (remove_jiggle && jiggle_check) {
    dat_base <- dat_base |>
      dplyr::select(DateTime,depth_m,obs_depth,stationary_block_id,is_stationary_status,post_jiggle,{{value_col}}) |>
      dplyr::filter(post_jiggle)
  } else{
    dat_base <- dat_base |>
      dplyr::select(DateTime,depth_m,obs_depth,stationary_block_id,is_stationary_status,{{value_col}})
  }

  # Group the data into stationary blocks and split them out into a list
  dat_grouped <- dat_base |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      t = as.numeric(difftime(DateTime, DateTime[1], units = "mins")) # Standardize time into minutes from start of data
    ) |>
    dplyr::group_split()
  # return(dat_grouped)

  # 3. Compute slope statistics.
  #    Start with entire post-jiggle period, re-calculate dropping an obs. each time until min_n is reached

  # Iterate across each of the stable groups data
  # out <- tibble::tibble()
  out_list <- vector("list", length(dat_grouped))

  for (i in seq_along(dat_grouped)) { # seq_along gives list length as 1:length(list)

    stable_group_dat <- dat_grouped[[i]]                    # Extract one stable group of data

    n <- nrow(stable_group_dat)                             # length of the stable block (rows)

    # Stop execution if block is empty
    if(n==0){
      msg_depth <- unique(stable_group_dat$obs_depth)
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

    # Set min_n rows slope and ranke OK flags to keep data if threshold is never met
    if(nrow(group_out) < min_n){
      msg_depth <- unique(stable_group_dat$obs_depth)
      warning(paste0('The given "min_n" is larger than the number of stable data identified for ',value_name,' at',msg_depth,'.'))
    }

    tailstart <- max(1, nrow(group_out) - min_n + 1) # Number of rows to keep if min_n only

    group_out$slope_ok[tailstart:nrow(group_out)] <- TRUE
    group_out$range_ok[tailstart:nrow(group_out)] <- TRUE


    # assign sensor data and time to variables
    x_all <- stable_group_dat$t
    y_all <- stable_group_dat[[value_name]]

    # Create a safety net in case the # of data rows < min_n
    if(n < min_n) {  # If fewer rows of data exist than the min_n as specified
      n_windows <- n # Use all available data to compute statistics (slope, range)
      msg_depth <- unique(stable_group_dat$obs_depth)
      warning(paste0('The given "min_n" is larger than the number of stable data identified for ',value_name,' at',msg_depth,'.'))
    } else {         # Or if more data exist than min_n rows
      n_windows <- n - min_n + 1 # Evaluate stats for the number of rows (n) minus the tail (min_n)
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
        warning(paste0("Single datapoint used for ",unique(stable_group_dat$obs_depth),", slope could not be calculated"))
      }

      # Range
      win_range <- max(y) - min(y)

      # Add stats and metadata to output object
      group_out$slope[win_start] <- slope_fit
      group_out$range[win_start] <- win_range

      group_out$n_dropped[win_start] <- win_start - 1
      group_out$n_used[win_start] <- length(idx)

      if(abs(slope_fit) <= slope_thresh) {
        group_out$slope_ok[win_start] <- TRUE
      }

      if(win_range <= range_thresh){
        group_out$range_ok[win_start] <- TRUE
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

  #########################################################
xx <-
  sensor_stable(dat_jiggle);xx
