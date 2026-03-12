# Identify where stationary blocks begin and backfill them
# accounting for rolling window delay and start trimming

# bool_roll = TRUE wherever rolling range < threshold
bool_roll <- roll_range < depth_range_threshold

# diff(TRUE/FALSE) works because TRUE=1, FALSE=0
# 1 indicates FALSE -> TRUE transition (entry into stationary block)
starts_idx <- which(c(NA, diff(bool_roll)) == 1)

# Loop through each detected stationary block
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

  start_i <- max(1, trimmed_start)

  # The detected index is the end of the block portion we fill

  end_i <- s

  # Mark stationary observations

  is_stationary_flag[start_i:end_i] <- TRUE
}
