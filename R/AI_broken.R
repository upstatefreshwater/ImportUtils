#' Identify Stabilized Sensor Periods
#'
#' Detects periods where a sensor signal has stabilized using a rolling
#' slope (trend) and rolling standard deviation criterion.
#'
#' @param df A data frame containing sensor and datetime columns.
#' @param sensor_col Column containing sensor measurements (numeric).
#' @param datetime_col Column containing datetime values (POSIXct).
#' @param slope_thresh Numeric. Absolute slope threshold (units per second)
#'   below which signal is considered trendless.
#' @param sd_thresh Numeric. Rolling standard deviation threshold.
#' @param window Integer. Rolling window size (number of observations).
#' @param min_obs Integer. Minimum number of consecutive stable observations
#'   required to classify a period as stabilized.
#' @param stationary_col Optional column indicating depth-stationary status
#'   (e.g., from `is_stationary()`). If provided, stabilization is only
#'   evaluated when this equals 999.
#' @param drop_cols Logical. If TRUE, intermediate columns are removed.
#' @param plot Logical. If TRUE, diagnostic plot is produced.
#'
#' @details
#' Stabilization is defined as periods where both:
#' \itemize{
#'   \item Absolute rolling slope < `slope_thresh`
#'   \item Rolling standard deviation < `sd_thresh`
#' }
#'
#' The rolling slope is computed using linear regression of sensor
#' values against time (seconds).
#'
#' @return Data frame with added column `is_sensor_stable`
#'   (999 = stable, 0 = unstable).
#'
#' @export
is_sensor_stable <- function(df,
                             sensor_col,
                             datetime_col,
                             slope_thresh,
                             sd_thresh,
                             window = 10,
                             min_obs = 15,
                             stationary_col = NULL,
                             drop_cols = TRUE,
                             plot = FALSE) {

  if (nrow(df) < window) {
    stop("Not enough observations for selected window.")
  }

  # numeric time in seconds
  time_num <- as.numeric(df |> dplyr::pull({{ datetime_col }}))

  # rolling slope function
  roll_slope <- function(x, t) {
    if (any(is.na(x)) || any(is.na(t))) return(NA_real_)
    coef(stats::lm(x ~ t))[2]
  }

  out <- df |>
    dplyr::mutate(
      .time_num = time_num,
      .sensor = {{ sensor_col }},
      roll_sd = zoo::rollapplyr(.sensor,
                                width = window,
                                FUN = stats::sd,
                                na.rm = TRUE,
                                fill = NA),
      roll_slope = zoo::rollapplyr(
        seq_along(.sensor),
        width = window,
        FUN = function(idx) {
          roll_slope(.sensor[idx], .time_num[idx])
        },
        fill = NA
      )
    )

  # Optional: restrict to depth-stationary only
  if (!rlang::quo_is_null(rlang::enquo(stationary_col))) {
    out <- out |>
      dplyr::mutate(
        .eligible = {{ stationary_col }} == 999
      )
  } else {
    out <- out |>
      dplyr::mutate(.eligible = TRUE)
  }

  out <- out |>
    dplyr::mutate(
      .stable_initial =
        .eligible &
        !is.na(roll_sd) &
        !is.na(roll_slope) &
        abs(roll_slope) < slope_thresh &
        roll_sd < sd_thresh,
      .group_id = dplyr::consecutive_id(.stable_initial)
    ) |>
    dplyr::group_by(.group_id) |>
    dplyr::mutate(
      .block_n = dplyr::n(),
      is_sensor_stable =
        dplyr::case_when(
          .stable_initial & .block_n >= min_obs ~ 999,
          TRUE ~ 0
        )
    ) |>
    dplyr::ungroup()

  if (plot) {

    p <- ggplot2::ggplot(out,
                         ggplot2::aes(x = seq_len(nrow(out)),
                                      y = .sensor)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::geom_point(
        ggplot2::aes(color = factor(is_sensor_stable)),
        size = 1
      ) +
      ggplot2::labs(
        title = "Sensor Stabilization Detection",
        y = deparse(substitute(sensor_col)),
        x = "Observation Index",
        color = "Stable"
      ) +
      ggplot2::scale_color_manual(
        values = c("0" = "firebrick", "999" = "darkgreen")
      ) +
      ggplot2::theme_minimal()

    print(p)
  }

  if (drop_cols) {
    out <- dplyr::select(out,
                         -c(.time_num,
                            .sensor,
                            roll_sd,
                            roll_slope,
                            .eligible,
                            .stable_initial,
                            .group_id,
                            .block_n))
  }

  return(out)
}

junk <-
is_sensor_stable(df = try,
                 datetime_col = DateTime,
                 sensor_col = temperature_C,
                 slope_thresh = 0.001,
                 sd_thresh = 0.001,
                 plot = T)

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

  library(dplyr)
  library(zoo)

  # 1) Identify stationary blocks using your existing function
  df_stat <- is_stationary(df,
                           depth_col = {{depth_col}},
                           datetime_col = {{datetime_col}},
                           ...)

  # 2) Compute sampling interval
  times <- df_stat %>% dplyr::pull({{ datetime_col }})
  samp_int <- as.numeric(difftime(times[2], times[1], units = "secs"))
  n_jiggle <- ceiling(jiggle_secs / samp_int)
  n_slope_window <- ceiling(slope_window_secs / samp_int)

  # 3) Apply slope + stability detection within stationary blocks
  df_out <- df_stat %>%
    dplyr::mutate(
      stationary_obs = is_stationary_status == 999,
      block_id = cumsum(!stationary_obs)
    ) %>%
    dplyr::group_by(block_id) %>%
    dplyr::mutate(
      idx_in_block = row_number(),
      post_jiggle = idx_in_block > n_jiggle,

      slope = zoo::rollapplyr(
        .data[[sensor_col]],
        width = n_slope_window,
        FUN = function(x) coef(lm(x ~ seq_along(x)))[2],
        fill = NA
      ),

      stable_flag = post_jiggle & stationary_obs & abs(slope) <= slope_thresh,

      median_stable = ifelse(stable_flag,
                             median(.data[[sensor_col]][stable_flag], na.rm = TRUE),
                             NA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-block_id, -idx_in_block, -post_jiggle)

  return(df_out)
}
