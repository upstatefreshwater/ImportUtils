df = dat_rnddepth
depth_col  = rlang::sym('depth_m')
datetime_col  = rlang::sym('DateTime')
depth_range_threshold  = 0.05
stationary_secs  = 30
min_detection_secs  = 10
sampling_int  = 0
drop_cols  = TRUE
plot  = FALSE

# ---- CHECKS --- ----
if (nrow(df) < 2) stop("Data frame must contain at least two observations.")

# Calculate sampling interval if not provided
if (sampling_int == 0) {
  times <- df |> dplyr::pull(!!datetime_col)
  samp_int <- median(as.numeric(diff(times), units = "secs"), na.rm = TRUE)

  if(length(unique(diff(times), na.rm = TRUE)) > 1){
    warning("Inconsistent sampling intervals detected.")
  }
  if(samp_int > 30){
    # Would be good to automate a depth check in here
    message("Sampling interval > 30s detected. Sonde assumed fixed in position.")
    return(df |> dplyr::mutate(is_stationary_status = 999))
  }
} else {
  samp_int <- sampling_int
}

# Ensure we always require at least 2 sampling intervals worth of data,
# but never fewer than 5 observations so the rolling window isn't trivially small
min_detect_secs <- max(min_detection_secs, 2 * samp_int)

# Compute rolling window size (number of observations)
min_detect_obs <- ceiling(min_detect_secs / samp_int)
if(min_detect_obs<5){warning('"min_detect_secs" resulted in too narrow a rolling range window, minimum of 5 observations automatically implemented for range calculation.')}
window <- max(5, min_detect_obs)
if(window > nrow(df)) {
  window <- nrow(df)
  message(paste("Window changed to maximum # of rows:", window))
}

# Pull depth values
depth_vals <- df |> dplyr::pull(!!depth_col)

# Rolling min and max
roll_min <- zoo::rollapply(depth_vals, width = window, FUN = min, fill = NA, align = "right")
roll_max <- zoo::rollapply(depth_vals, width = window, FUN = max, fill = NA, align = "right")
roll_range <- roll_max - roll_min

# Vectorized flagging of stationary points: any point within a window that meets threshold
is_stationary_flag <- rep(FALSE, length(depth_vals))
# Only mark positions where rolling range < threshold
valid_idx <- which(!is.na(roll_range) & roll_range < depth_range_threshold)
# Mark all observations within the rolling window as stationary if range threshold was met
if(length(valid_idx) > 0){
  start_idx <- pmax(1, valid_idx - window + 1)
  end_idx <- valid_idx
  # Build out a list of sequences for stationary block then flatten into a single vector to mark stationary flag as TRUE
  is_stationary_flag[unlist(mapply(seq, start_idx, end_idx))] <- TRUE
}

# --- Jump-based override ---
# Compute instantaneous depth differences
depth_diff <- c(0, abs(diff(depth_vals)))

# Identify jumps exceeding threshold
jump_idx <- which(depth_diff > depth_range_threshold)

# Number of observations to force as non-stationary
cooldown_n <- max(1, ceiling(min_detection_secs / samp_int))

# Initialize override vector
jump_override <- rep(FALSE, length(depth_vals))

# Mark observations following a jump as non-stationary
for (j in jump_idx) {
  end_idx <- min(j + cooldown_n, length(depth_vals))
  jump_override[(j + 1):end_idx] <- TRUE
}

# Apply jump override
is_stationary_flag[jump_override] <- FALSE

# Identify consecutive blocks
stationary_block_id <- dplyr::consecutive_id(is_stationary_flag)

df_out <- df |>
  dplyr::mutate(
    is_stationary_initial = is_stationary_flag, # These are all stationary blocks > min_detect_secs (>5s)
    stationary_block_id = stationary_block_id   # For grouping (includes both T/F blocks)
  ) |>
  dplyr::group_by(stationary_block_id, is_stationary_initial) |> # Include both so computation is done by T/F grouping on stationary_flag
  dplyr::mutate(
    block_duration = dplyr::n(),
    block_secs = block_duration * samp_int,
    is_stationary_status = dplyr::case_when(
      is_stationary_initial & block_secs >= stationary_secs ~ 999,
      is_stationary_initial & block_secs >= min_detect_secs ~ block_secs,
      TRUE ~ 0
    )
  ) |>
  dplyr::ungroup()
