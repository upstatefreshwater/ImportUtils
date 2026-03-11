
is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          depth_range_threshold = 0.05,
                          stationary_secs = 60,
                          rolling_range_secs = 10,
                          start_trim_secs = 15,
                          # sampling_int = 0,
                          drop_cols = TRUE,
                          plot = FALSE) {
  # 00. --- rlang arguments --- ----
  depth_col <- rlang::enquo(depth_col)
  datetime_col <- rlang::enquo(datetime_col)

  # 0. --- Checks --- ----
  # df columns exist
  missing_cols <- setdiff(c(rlang::as_name(depth_col),
                            rlang::as_name(datetime_col)),
                          names(df))
  if(length(missing_cols) > 0) {
    stop("Required columns missing from input data:\n",
         paste(missing_cols, collapse = ", "))
  }

  # df columns formatting

  if (!is.numeric(df[[rlang::as_name(depth_col)]]) ||
      !inherits(df[[rlang::as_name(datetime_col)]], "POSIXt")) {
    stop('Required columns are incorrectly formatted.\nCheck that "depth_col" is numeric, and "datetime_col" is a POSIXt.')
  }

  # Check on sampling interval
  samp_int <- get_sample_interval(df[[rlang::as_name(datetime_col)]]) # this will warn if multiple sampling intervals exist

    # Check that the start_trim_secs is large enough to

  # Check that user didn't accidentally trim more data than stationary_secs
  if(start_trim_secs >= stationary_secs){
    stop('"start_trim_secs" must be smaller than "stationary_secs"')
  }

  # 0-a. --- If sonde fixed in position, return data with is_stationary_status == 999 for all rows ---
  if(samp_int > 30){
    # Would be good to automate a depth check in here
    message("Sampling interval > 30s detected. Sonde assumed fixed in position.")
    return(df |> dplyr::mutate(is_stationary_status = 999))
  }

  # 1. --- Create objects needed for flagging stationary blocks --- ----
  stationary_n <- ceiling(stationary_secs / samp_int)                           # The number of obs needed to be considered fully stationary
  rolling_n <- ceiling(rolling_range_secs / samp_int)                           # The number of obs needed for the rolling range window
  trim_n <- max(1,ceiling(start_trim_secs / samp_int))                          # The number of obs to trim off the start of a stationary block (force at least 1)
  start_n_trimmed <- rolling_n - trim_n                                         # The number of obs to flag as stationary after trimming at the start of stationary blocks


  # return(start_n_trimmed)

  # If window is more than the number of rows in the dataset
  if(rolling_n > nrow(df)){
    rolling_n <- nrow(df)
    warning(paste('Number of rows in data exceeds the number of observations required for rolling range calculation./n
                  Range calculation will use the maximum # of rows:', rolling_n))
  }

  # 2. --- Compute rolling range --- ----
  # Pull depth values
  depth_vals <- df[[rlang::as_name(depth_col)]]

  # Rolling min and max
  roll_min <- zoo::rollapply(depth_vals, width = rolling_n, FUN = min, fill = NA, align = "right")
  roll_max <- zoo::rollapply(depth_vals, width = rolling_n, FUN = max, fill = NA, align = "right")
  roll_range <- roll_max - roll_min

  # 3. --- Create initial stationary flag and an index for stationary obs based on the rolling window --- ----
  # Initialize stationary flag as all FALSE
  is_stationary_flag <- rep(FALSE, length(depth_vals))

  # Create an index of positions where the rolling range is below the "depth_range_threshold"
  good_rollrange_idx <-  which(!is.na(roll_range) & roll_range < depth_range_threshold) #

  # Update stationary flag with TRUE locations
  # Mark those locations from the rolling range threshold index as TRUE
  is_stationary_flag[good_rollrange_idx] <- TRUE                                 # This has to be done before trimming!!!

  # 4. --- Apply trimming logic --- ----
  # Create a boolean where rolling range below the range threshold is TRUE
  bool_roll <- !is.na(roll_range) & roll_range < depth_range_threshold          # Avoid NA propagation issues

  # This utility function returns indices of stable observations to be trimmed from the start of the stable period
  trim_idx <- trim_stationary_starts(range_met_vector = bool_roll,
                                     rolling_n = rolling_n,
                                     depth_range_threshold = depth_range_threshold,
                                     trim_n = trim_n)

  # Update the stationary_flag to include trimming
  # If the n_obs to be trimmed is less than the rolling window,
  # shift the difference of obs to TRUE from the start_idx backwards
  if(trim_n < rolling_n){
    is_stationary_flag[trim_idx] <- TRUE

    #If there are more n_obs to be trimmed than the size of the rolling window,
    # convert the difference to FALSE between start_idx:trim_n
  } else{
    is_stationary_flag[trim_idx] <- FALSE
  }

  # 5. --- Calculate stationary block duration --- ----
  stationary_block_id <- dplyr::consecutive_id(is_stationary_flag)
  junk <- df |>
    dplyr::mutate(
      is_stationary_initial = is_stationary_flag, #
      stationary_block_id = stationary_block_id   # For grouping (includes both T/F blocks)
    ) |>
    dplyr::group_by(stationary_block_id, is_stationary_initial) |> # Include both so computation is done by T/F grouping on stationary_flag
    dplyr::mutate(
      block_n = dplyr::n(),
      block_secs = as.numeric(max({{datetime_col}}) - min({{datetime_col}}), units = "secs"),
      is_stationary_status = dplyr::case_when(
        is_stationary_initial & block_secs >= stationary_secs ~ 999,
        is_stationary_initial & block_secs < stationary_secs ~ block_secs,
        TRUE ~0
      ))

  return(junk)


  # . --- Compile output Data --- ----
  out <- data.frame(bool_roll,
                    raw = df[[rlang::as_name(depth_col)]],
                    rollrange = roll_range,
                    starts = c(NA,diff(bool_roll)),
                    flag = is_stationary_flag,
                    rolling_n,
                    trim_n,
                    start_n_trimmed)
  return(out)

}


# data.frame(raw = dat_rename$depth_m,flag = is_stationary(dat_rename))
junk <- is_stationary(df = dat_rename,
              depth_range_threshold = 0.1,
              start_trim_secs = 6)













