#' Identify Stationary Periods in Sonde Data
#'
#' Identifies periods when a profiling sonde is stationary in the water column
#' using a rolling standard deviation of depth measurements.
#'
#' @param df A data frame containing depth and datetime columns.
#' @param depth_col Column containing depth measurements (numeric).
#' @param datetime_col Column containing datetime values (POSIXct or convertible).
#' @param sd_thresh Numeric. Rolling standard deviation threshold (m) below which
#'   the sonde is considered stable.
#' @param window Integer. Rolling window size (number of observations) used
#'   to compute the standard deviation.
#' @param min_obs Integer. Minimum number of consecutive stable observations
#'   required to classify a period as stationary.
#' @param drop_cols Logical. If `TRUE`, intermediate calculation columns
#'   (`depth_sd`, `is_stationary_initial`, `group_id`, `block_duration`)
#'   are removed from the returned data frame.
#' @param plot Logical. If `TRUE`, produces diagnostic plots showing depth
#'   and rolling standard deviation with stability classification.
#'
#' @details
#' A backward-looking rolling standard deviation of depth is computed using
#' `window` (# of observations). Observations with rolling SD below `sd_thresh`
#' are initially classified as stable.
#'
#' Consecutive stable observations are grouped into blocks. If a block contains
#' at least `min_obs` observations, the block is classified as stationary and
#' assigned `is_stationary_status = 999`.
#'
#' Stable blocks shorter than `min_obs` are assigned a numeric value equal to
#' their duration in seconds (`block_duration × sampling interval`). `Sampling interval`
#' is calculated internally as the difference of the first two `datetime_col` values.
#'
#' All non-stable observations (those exceeding `sd_thresh`) are assigned `is_stationary_status = 0`.
#'
#' If the sampling interval exceeds 30 seconds, the sonde is assumed to be
#' fixed in position and all observations are assigned `is_stationary_status = 999`.
#'
#' @return
#' The input data frame with an added column:
#' \describe{
#'   \item{is_stationary_status}{Integer flag indicating stationary classification:
#'     999 = stationary; 0 = not stationary; other positive values represent
#'     short stable periods in seconds.}
#' }
#'
#' @export
is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          sd_thresh = 0.05,
                          window = 7,
                          stationary_secs = 30,
                          sampling_int = 0,
                          drop_cols = TRUE,
                          plot = FALSE) {

  if (nrow(df) < 2) {
    stop("Data frame must contain at least two observations.")
  }

  # If sampling interval is not provided, calculate it
  if(sampling_int==0){
    # Extract datetime vector safely for interval calculation
    times <- df |> dplyr::pull({{ datetime_col }})

    samp_int <-  as.numeric(median(diff(times), na.rm = TRUE),units = 'secs')   # Calculate the sampling interval

    if(!length(unique(diff(times),na.rm = T))==1){
      warning('Inconsistent sampling intervals detected.')
    }

    if(samp_int > 30){
      message('Sampling interval > 30s detected. Sonde assumed to be fixed in position.')
      return(df |> dplyr::mutate(is_stationary_status = 999))
    }
  } else{
    samp_int <- sampling_int
  }

  # Set the number of obs equal to the min reqd_stationary_obs
  min_obs <- ceiling(stationary_secs / samp_int)

  out <- df |>
    dplyr::mutate(
      depth_sd = zoo::rollapplyr({{ depth_col }},                                # 'right' means from the obs. look backwards n = window
                                 width = window,
                                 FUN = stats::sd,                                # Calculate the rolling SD of the window
                                 na.rm = TRUE,
                                 fill = NA),
      is_stationary_initial = dplyr::if_else(
        is.na(depth_sd),
        FALSE,
        depth_sd < sd_thresh),                                                   # Sets is_stationary_initial based on only SD
      group_id = dplyr::consecutive_id(.data$is_stationary_initial)) |>          # assigns a unique id based on each time is_stationary_initial changes
    dplyr::group_by(.data$group_id) |>
    dplyr::mutate(
      block_duration = dplyr::n(),                                               # counts the number of obs in each stable period (works by group)
      is_stationary_status = dplyr::case_when(
        .data$is_stationary_initial & .data$block_duration >= min_obs ~ 999,     # If there are enough observations after stationary detected, mark as stationary
        .data$is_stationary_initial &
          .data$block_duration < min_obs ~ .data$block_duration * samp_int,      # For fewer than the minimum # of obs, return the number of seconds sonde was stationary
        TRUE ~ 0                                                                 # Mark moving as 0
      )
    ) |>
    dplyr::ungroup()

  if(plot){
    # 1. Get all unique levels
    levels_list <- as.character(unique(out$is_stationary_status))
    ncolors <- length(levels_list)

    # 2. Generate random colors for everything first
    # We name them so ggplot knows exactly which color goes to which level
    mycolors <- setNames(sample(colors(distinct = TRUE), ncolors), levels_list)

    # 3. Explicitly overwrite your "known" values
    mycolors['999'] <- 'darkgreen'
    mycolors['0']   <- 'firebrick'
    # 2. Visualize to verify the threshold
    p1 <- ggplot2::ggplot(out, ggplot2::aes(x = seq_len(nrow(out)), y = {{depth_col}})) +
      ggplot2::geom_line(alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(color = as.factor(is_stationary_status)), size = 1.2) +
      ggplot2::scale_y_reverse() + # Depth plots usually go down
      ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
      ggplot2::scale_color_manual(name = 'Seconds Stationary',
                                  values = mycolors) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(out,
                          ggplot2::aes(x = seq_len(nrow(out)),
                                       y = depth_sd)) +
      ggplot2::geom_line(ggplot2::aes(color = "Rolling SD",
                                      linetype = "Rolling SD")) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = sd_thresh,
                                       color = "SD Threshold",
                                       linetype = "SD Threshold")) +
      ggplot2::scale_color_manual(
        name = "",
        values = c("Rolling SD" = "red",
                   "SD Threshold" = "black")
      ) +
      ggplot2::scale_linetype_manual(
        name = "",
        values = c("Rolling SD" = "solid",
                   "SD Threshold" = "dashed")
      ) +
      ggplot2::labs(title = "Rolling Standard Deviation",
                    y = "SD (m)",
                    x = "Observation Index") +
      ggplot2::theme_minimal()

    # Combine the plots
   print(patchwork::wrap_plots(p1, p2, ncol = 1))

  }

  if (drop_cols) {                                                                # Remove intermediate columns
    out <- dplyr::select(out, -c(depth_sd, is_stationary_initial, group_id, block_duration))
  }

  return(out)
}

