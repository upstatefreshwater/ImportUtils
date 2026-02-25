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
      stationary_block_id =  dplyr::consecutive_id(is_stationary_status) # grouping index for stationary observations
    ) |>
    # Group into stationary blocks
    # Within this
    dplyr::group_by(stationary_block_id) |>                            # This groups the data by stationary blocks
    # Within this mutate call operations are performed on grouped data
    dplyr::mutate(
      stationary_block_index = dplyr::row_number(),                    # integer index restarts on each stable block because of data grouping
      post_jiggle = stationary_block_index > n_jiggle,                 # flag the "jiggle_secs" aka the period after sonde stops moving when the field tech is supposed to slightly move sonde
      # Calculate DO range across the window ----
      DO_range = zoo::rollapplyr(                                      # "r" at the end means "right" or "backwards looking" across the width
        ifelse(post_jiggle, {{DO_col}}, NA),
        width = n_range_window,
        FUN = function(x) if (all(!is.na(x))) max(x) - min(x) else NA,  # Computes the range across the window
        fill = NA                                                       # NOTE** a number of obs are set to NA because of !all(is.na), so when width = 5 obs, 4 obs beyond post_jiggle are NA, and this increases with increasing window size
      ),
      # Flag stationary post-jiggle obs. that are below set DO range threshold ----
      DO_withinthresh = DO_range <= DO_range_thresh,                  # Only need to handle the threshold now, stationary & jiggle handled above
      DO_withinthresh = dplyr::coalesce(DO_withinthresh,FALSE),       # This replaces NAs with FALSE so consecutive_id works

      range_block_id = dplyr::consecutive_id(DO_withinthresh)) |>         # grouping index for obs < set DO threshold

      # Add the consecutive range_block_id to grouping ----
  dplyr::group_by(stationary_block_id,range_block_id) |>
      dplyr::mutate(
        # position inside TRUE runs
        DO_run_length =
          if_else(DO_withinthresh,
                  dplyr::row_number(),
                  0L),

        run_total =
          sum(DO_withinthresh),

        # stable definition
        DO_stable_flag =
          DO_run_length >= min(n_median, run_total),

        # select last min(n_median, run_total)
        DO_obs_flag =
          DO_stable_flag & DO_run_length > pmax(run_total - n_median, 0)

      )|>
    dplyr::ungroup() |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(

      DO_median =
        if_else(
          DO_obs_flag,
          median({{DO_col}}[DO_obs_flag], na.rm = TRUE),
          NA_real_
        )

    ) |>
    dplyr::ungroup()
      # )
      # DO_withinthresh_length = ifelse(                                # count consecutive TRUE runs
      #   DO_withinthresh & !is.na(DO_withinthresh),                    # When an observation is under the specified threshold and is not NA
      #   stats::ave(DO_withinthresh,                                   # ave() returns the group average
      #              base::cumsum(!(DO_withinthresh %in% TRUE))-1,       # This resets the group at every FALSE DO_withinthresh
      #              FUN = seq_along),
      #   0                                                              # False rows of the ave() trick get a 0
      # ),
      #
      # # DO_withinthresh_id = dplyr::consecutive_id(DO_withinthresh)
      #
      # DO_stable_flag = dplyr::case_when(
      #   DO_withinthresh_length >= n_median ~ TRUE,                            # normal case
      #   DO_withinthresh_length > 0 & sum(DO_withinthresh_length > 0) < n_median ~ TRUE, # short block: take all available points
      #   TRUE ~ FALSE                                                 # everything else
      # ),
      # # identify consecutive TRUE/FALSE runs
      # stable_run_id = dplyr::consecutive_id(DO_stable_flag),
      #
      # # group within block + run
      # run_rank = ifelse(
      #   DO_stable_flag,
      #   ave(DO_stable_flag,
      #       stable_run_id,
      #       FUN = seq_along),
      #   0
      # ),
      #
      # run_length = ave(
      #   DO_stable_flag,
      #   stable_run_id,
      #   FUN = sum
      # ),
      #
      # DO_obs_flag =
      #   DO_stable_flag &
      #   run_rank > pmax(run_length - n_median, 0),
      #
      # # DO_obs_flag = DO_stable_flag &
      # #   dplyr::row_number() >                     # index within the current group
      # #   (dplyr::n() - n_median),                  # total rows in group minus # of rows you want to keep

    #   DO_median = ifelse(DO_obs_flag,                               # Record the median value for all observations meeting stability criteria
    #                      median({{DO_col}}[DO_obs_flag], na.rm = TRUE),
    #                      NA)
    #
    # ) |>
    # dplyr::ungroup()
  # |>
  #   dplyr::select(-stationary_block_id, -stationary_block_index, -post_jiggle)

  return(df_out)
}
