stabilize_cast <- function(df,
                           DO_col = DO_mgL,
                           temp_col = temperature_C,
                           median_secs = 10,
                           depth_col = depth_m,
                           datetime_col = DateTime,
                           jiggle_secs = 15,
                           range_window_secs = 10,
                           DO_range_thresh = 0.02,
                           temp_range_thresh = 0.05,
                           ...) {

  # 1) Identify stationary blocks using your existing function
  df_stat <- is_stationary(df,
                           depth_col = {{depth_col}},
                           datetime_col = {{datetime_col}},
                           ...)

  # 2) Compute sampling interval
  times <- df_stat |> dplyr::pull({{ datetime_col }})
  samp_int <- as.numeric(difftime(times[2], times[1], units = "secs")) # calculate the sampling interval
  n_jiggle <<- ceiling(jiggle_secs / samp_int)                          # number of observations to throw out for "jiggle period"
  n_range_window <<- ceiling(range_window_secs / samp_int)              # this is the "width" argument of rollapply
  n_median <<- ceiling(median_secs / samp_int)                           # number of obs to meet minimum

  # 3) Apply range + stability detection within stationary blocks
  df_out <- df_stat |>
    dplyr::mutate(
      # is_stationary_status = is_stationary_status == 999,                    # Flag where sonde is stationary based on is_stationary fxn
      block_id =  dplyr::consecutive_id(is_stationary_status)                # grouping index for stationary observations
    ) |>
    dplyr::group_by(block_id) |>                                       # This groups the data by stationary blocks
    dplyr::mutate(
      idx_in_block = dplyr::row_number(),                              # integer index restarts on each stable block because of data grouping
      post_jiggle = idx_in_block > n_jiggle,                           # flag the "jiggle_secs" aka the period after sonde stops moving when the field tech is supposed to slightly move sonde

      DO_range = zoo::rollapplyr(                                      # "r" at the end means "right" or "backwards looking" across the width
        ifelse(post_jiggle, {{DO_col}}, NA),
        width = n_range_window,
        FUN = function(x) if (all(!is.na(x))) max(x) - min(x) else NA,  # Computes the range across the window
        fill = NA                                                       # NOTE** a number of obs are set to NA because of !all(is.na), so when width = 5 obs, 4 obs beyond post_jiggle are NA, and this increases with increasing window size
      ),

      DO_within_thresh = DO_range <= DO_range_thresh,                  # Only need to handle the threshold now, stationary & jiggle handled above

      DO_run_length = ifelse(                                          # count consecutive TRUE runs
        DO_within_thresh & !is.na(DO_within_thresh),
        stats::ave(DO_within_thresh,        # ave() returns the group average
                   base::cumsum(!(DO_within_thresh %in% TRUE)),        # This resets the group at every FALSE DO_within_thresh
                   FUN = seq_along),
        0                                                              # False rows of the ave() trick get a 0
      ),

      DO_stable_flag = dplyr::case_when(
        DO_run_length >= n_median ~ TRUE,                            # normal case
        DO_run_length > 0 & sum(DO_run_length > 0) < n_median ~ TRUE, # short block: take all available points
        TRUE ~ FALSE                                                 # everything else
      ),
      stable_rank = cumsum(DO_stable_flag),
      stable_total = sum(DO_stable_flag),

      DO_obs_flag =
        DO_stable_flag &
        stable_rank > (stable_total - n_median),

      # DO_obs_flag = DO_stable_flag &
      #   dplyr::row_number() >                     # index within the current group
      #   (dplyr::n() - n_median),                  # total rows in group minus # of rows you want to keep

      DO_median = ifelse(DO_obs_flag,                               # Record the median value for all observations meeting stability criteria
                         median({{DO_col}}[DO_obs_flag], na.rm = TRUE),
                         NA)

    ) |>
    dplyr::ungroup()
  # |>
  #   dplyr::select(-block_id, -idx_in_block, -post_jiggle)

  return(df_out)
}
