# Finds the nearest depth from a raw data column to a candidate set of target depths ----
nearest_depth <- function(raw_z,
                          candidate_z,
                          tol = 0.25,
                          surface_tol = tol){

  # Compute differences between raw depths and candidates as absolute distances
  d <- abs(outer(raw_z, candidate_z, "-"))
  # Locate the index of which distance in smallest
  idx <- apply(d, 1, which.min)
  # Pull out the depths
  out <- candidate_z[idx]

  min_d <- apply(d, 1, min)

  out[min_d > tol] <- NA

  # special handling for 0 depth
  if(any(candidate_z == 0)){
    surf <- which(candidate_z == 0)
    surf_diff <- raw_z - candidate_z[surf]
    out[surf_diff >= 0 & surf_diff <= surface_tol] <- 0
  }

  out
}

# Calculates the sampling interval ----
# from a column/vector of datetime data as the mode (if multiple intervals detected)
# If multiple intervals detected, it gives a warning only

get_sample_interval <- function(datetime_data,
                                output_units = "secs",
                                tol_prop = 1,
                                suppress_warning = FALSE) {

  # Calculate sampling intervals
  all_ints <- as.numeric(diff(datetime_data), units = "secs")
  all_ints <- all_ints[!is.na(all_ints)]

  # Count intervals
  counts <- table(all_ints)

  # Most common interval (mode)
  sampling_int <- as.numeric(names(counts)[which.max(counts)])

  # Check consistency
  prop <- max(counts) / sum(counts)

  if (length(counts) > 1 && prop < tol_prop) {

    dist_table <- data.frame(
      sampling_interval = as.numeric(names(counts)),
      n = as.integer(counts)
    )

    if(!suppress_warning){
      warning(
        paste0(
          "Inconsistent sampling intervals detected.\n",
          "Dominant interval: ", sampling_int, " ", output_units,
          " (", round(prop * 100, 1), "% of records) used for `samp_int`.\n",
          "Interval distribution:\n",
          paste(utils::capture.output(print(dist_table, row.names = FALSE)),
                collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }

  sampling_int
}

# (deprecated) ----
# This function is not currently used after the removal of `start_trim_secs` from `is_stationary`

# Trimming utility for is_stationary
# This function is specific to the is_stationary function
# and accepts a Boolean vector where rolling_range < depth_range_threshold == TRUE

trim_stationary_starts <- function(range_met_vector,
                                   # depth_range_threshold,
                                   rolling_n,
                                   trim_n){
  if(!is.logical(range_met_vector)){
    stop('Required "range_met_vector" needs to be a logical (boolean) input.')
  }
  # Identify where stationary blocks begin and backfill them
  # accounting for rolling window delay and start trimming

  # bool_roll = TRUE wherever rolling range < threshold
  bool_roll <- range_met_vector

  # diff(TRUE/FALSE) works because TRUE=1, FALSE=0
  # 1 indicates FALSE -> TRUE transition (entry into stationary block)
  starts_idx <- which(c(NA, diff(bool_roll)) == 1)

  out <- c()

  # Loop through each detected stationary block

  if(trim_n < rolling_n){
    for (s in starts_idx) {

      # Rolling windows detect stability late because they look backwards.
      # The earliest point the window could represent is:
      #
      #   s - rolling_n + 1
      #
      # Example:
      # index:       6 7 8 9 10
      # window:     [-----]
      # detection:              s = 10
      #
      # The stationary behavior could actually start at index 6.

      window_start <- s - rolling_n + 1
      # Apply trimming to remove the beginning of the block
      # (sonde settling / probe movement etc.)

      trimmed_start <- window_start + trim_n

      # Prevent indexing before the dataset start

      start_i <- max(1, window_start) + trim_n

      # The detected index is the end of the block portion we fill

      end_i <- s

      out <- c(out,start_i:end_i)
      # Mark stationary observations

      # is_stationary_flag[start_i:end_i] <- TRUE
      # print('hi')
    }
  }

  # if(trim_n > rolling_n){
  #   for (s in starts_idx) {
  #     trim_diff <- trim_n - rolling_n
  #
  #     start_i <- s
  #
  #     end_i <- s + trim_diff
  #
  #     out <- c(out,start_i:end_i)
  #
  #
  #   }
  # }
  return(out)
}

# Decimal Places ----
# helper to extract decimal places
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

# Check function inputs that should be numeric ----
check_numeric <- function(x, name, allow_zero = FALSE) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop(paste0("`", name, "` must be a single numeric value."))
  }
  if (allow_zero) {
    if (x < 0) stop(paste0("`", name, "` must be >= 0."))
  } else {
    if (x <= 0) stop(paste0("`", name, "` must be > 0."))
  }
}

# Check function inputs that should be logical ----
check_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(paste0("`", name, "` must be TRUE or FALSE."))
  }
}




# Extract TROLL slope and range threshold from internal data ----

get_threshold <- function(param, type){
  val <- stability_ranges[[type]][stability_ranges$param == param]
  if(length(val) != 1){
    stop(paste0("No ", type, " found for ", param))
  }
  val
}

# Calculated slope instead of lm() ----
calc_slope <- function(x, y){
  v <- stats::var(x)
  if(is.na(v) || v == 0) return(NA_real_)
  stats::cov(x, y) / v
}


# Resolve Stability Context ----
resolve_stability_context <- function(df,
                                      value_name,
                                      dict,
                                      stability_ranges) {

  # ---- defaults ----
  source_name <- value_name
  use_fallback <- FALSE

  # ---- lookup in dictionary ----
  dict_row <- dict |>
    dplyr::filter(canonical == value_name)

  if(nrow(dict_row) > 1){
    stop("Duplicate canonical names found in dictionary.")
  }

  if(nrow(dict_row) == 1){

    src <- dict_row$stability_source

    use_fallback <- is.na(src) || !(src %in% names(df))

    if(!use_fallback){
      source_name <- src
    }
  }

  # ---- final calculation target ----
  calc_name <- if(use_fallback) value_name else source_name

  # ---- optical classification ----
  optical_param <- is_optical(calc_name)

  # ---- slope threshold ----
  match_idx <- which(stability_ranges$param == calc_name)

  if(length(match_idx) == 0){
    stop(paste0("No slope/range thresholds found for ", calc_name))
  }

  slope_thresh <- stability_ranges$slope[match_idx[1]]
  range_thresh <- stability_ranges$range[match_idx[1]]

  # ---- return context ----
  list(
    value_name = value_name,
    source_name = source_name,
    calc_name = calc_name,
    use_fallback = use_fallback,
    optical_param = optical_param,
    slope_thresh = slope_thresh,
    range_thresh = range_thresh
  )
}
