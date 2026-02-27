troll_rollRange <- function(df,
                            sampling_int,
                            DO_col = DO_mgL,
                            temp_col = temperature_C,
                            range_window_secs = 10,
                            DO_range_thresh = 0.1,
                            temp_range_thresh = 0.05){
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

      range_block_id = dplyr::consecutive_id(DO_withinthresh),

      # Calculate Temp range across the window -----
      temp_range = zoo::rollapplyr(                                      # "r" at the end means "right" or "backwards looking" across the width
        ifelse(post_jiggle, {{temp_col}}, NA),                           # Only calculate for the "post_jiggle" period
        width = n_range_window,
        FUN = function(x) if (all(!is.na(x))) max(x) - min(x) else NA,  # Computes the range across the wintempw
        fill = NA                                                       # NOTE** a number of obs are set to NA because of !all(is.na), so when width = 5 obs, 4 obs beyond post_jiggle are NA, and this increases with increasing wintempw size
      ),
      # Flag stationary post-jiggle obs. that are below set temp range threshold ----
      temp_withinthresh = temp_range <= temp_range_thresh,                  # Only need to handle the threshold now, stationary & jiggle handled above
      temp_withinthresh = dplyr::coalesce(temp_withinthresh,FALSE),       # This replaces NAs with FALSE so consecutive_id works

      range_block_id = dplyr::consecutive_id(temp_withinthresh)
      )

}
