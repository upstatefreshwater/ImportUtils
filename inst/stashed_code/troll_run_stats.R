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

# dat |> troll_run_stats()

