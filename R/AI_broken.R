

#____________________________
#' Full Pipeline: Stationary Detection + Sensor Stabilization
#'
#' @param df Data frame containing datetime, depth, and sensor columns
#' @param sensor_col Name of sensor column to stabilize (e.g., DO, pH)
#' @param depth_col Name of depth column
#' @param datetime_col Name of datetime column
#' @param jiggle_secs Seconds to discard at start of stationary block
#' @param slope_window_secs Window (seconds) for rolling slope calculation
#' @param slope_thresh Maximum slope magnitude considered stable
#' @param ... Additional arguments passed to is_stationary()
#' @return Data frame with original columns + stable_flag, slope, median_stable
stabilize_cast <- function(df,
                           sensor_col,
                           depth_col = depth_m,
                           datetime_col = DateTime,
                           jiggle_secs = 15,
                           slope_window_secs = 10,
                           slope_thresh = 0.02,
                           ...) {
  # 1) Identify stationary blocks using your existing function
  df_stat <- is_stationary(df,
                           depth_col = {{depth_col}},
                           datetime_col = {{datetime_col}},
                           ...)

  # 2) Compute sampling interval
  times <- df_stat |> dplyr::pull({{ datetime_col }})
  samp_int <- as.numeric(difftime(times[2], times[1], units = "secs")) # calculate the sampling interval
  n_jiggle <- ceiling(jiggle_secs / samp_int)                          # number of observations to throw out for "jiggle period"
  n_slope_window <- ceiling(slope_window_secs / samp_int)              # this is the "width" argument of rollapply

  # 3) Apply slope + stability detection within stationary blocks
  df_out <- df_stat |>
    dplyr::mutate(
      stationary_obs = is_stationary_status == 999,                    # Flag where sonde is stationary based on is_stationary fxn
      block_id = cumsum(!stationary_obs)                               # Cumulative sum adds one per stationary_obs == FALSE, therefore all stationary rows have the same value
    ) |>
    dplyr::group_by(block_id) |>
    dplyr::mutate(
      idx_in_block = dplyr::row_number(),
      post_jiggle = idx_in_block > n_jiggle,                           # toss out the first period after sonde stops moving

      slope = zoo::rollapplyr(                                          # apply "right" so it's backwards looking for slop calc
        .data[[sensor_col]],
        width = n_slope_window,
        FUN = function(x) coef(lm(x ~ seq_along(x)))[2],                # Extract slope of lm()
        fill = NA
      ),

      stable_flag = post_jiggle & stationary_obs & abs(slope) <= slope_thresh, # Flag TRUE if stationary after jiggle and below slope threshold

      median_stable = ifelse(stable_flag,                               # Record the median value for all observations meeting stability criteria
                             median(.data[[sensor_col]][stable_flag], na.rm = TRUE),
                             NA),
      slope_full_stabl_block = ifelse(stable_flag,                      # Record the overall slope of the whole block where stability criteria were met
                                      coef(lm(.data[[sensor_col]] ~ idx_in_block))[2],
                                      NA),
      sd_full_stabl_block = ifelse(stable_flag,                         # Record the SD of the whole block where stability criteria were met
                                   sd(.data[[sensor_col]],na.rm=TRUE),
                                   NA)
    ) |>
    dplyr::ungroup()
  # |>
  #   dplyr::select(-block_id, -idx_in_block, -post_jiggle)

  return(df_out)
}

trash <-
stabilize_cast(try,
               sensor_col = 'DO_mgL',
               slope_thresh = 0.02)

trash <- depth_rounder(trash)
# view(trash)
# trash |> select(depth_m, post_jiggle,)

library(tidyverse)

ggplot(data = trash) +
  geom_point(aes(x = DO_mgL,y = depth_m, color = 'Raw')) +
  geom_point(aes(x = median_stable, y = obs_depth, color = 'Median')) +
  scale_y_reverse()

ggplot(data = trash) +
  geom_point(aes(x = DateTime, y = DO_mgL, color = 'Raw')) + # temperature_C-15 # depth_m
  geom_point(aes(x = DateTime, y = median_stable, color = 'Median')) +
  geom_vline(xintercept = c(ymd_hms('2025/09/16 15:17:55'),
                            ymd_hms('2025/09/16 15:19:23'))) + # Depth jump from 6 to 7 m
  annotate('text', label = '6 -> 7m',
           x = ymd_hms('2025/09/16 15:17:55'),
           y = 7,
           hjust = -0.1,
           angle = 0) +
  annotate('text', label = '7 -> 8m',
           x = ymd_hms('2025/09/16 15:19:23'),
           y = 4,
           hjust = -0.1,
           angle = 0)

  # coord_cartesian(xlim = c(ymd_hms('2025/09/16 15:17:00'),ymd_hms('2025/09/16 15:20:00')))
