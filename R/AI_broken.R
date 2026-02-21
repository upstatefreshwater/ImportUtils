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
