# is_stationary ----
#' Identify Stationary Periods in High-Frequency Depth Data
#'
#' Detects stationary periods in high-frequency depth time series data using
#' a rolling standard deviation threshold. Stationary status is determined
#' based on depth variability within a moving window and a minimum required
#' stationary duration.
#'
#' The function:
#' \itemize{
#'   \item Calculates a rolling standard deviation of depth.
#'   \item Flags observations where rolling SD is below a threshold.
#'   \item Groups consecutive stationary observations.
#'   \item Requires a minimum number of consecutive observations
#'         (based on time) to classify a period as fully stationary.
#' }
#'
#' If sampling interval is not supplied, it is estimated from the median
#' difference of the datetime column.
#'
#' @param df A data frame containing at minimum a depth column and a
#' datetime column.
#'
#' @param depth_col Unquoted name of the numeric depth column (meters).
#' Default is \code{depth_m}.
#'
#' @param datetime_col Unquoted name of the POSIXct datetime column.
#' Default is \code{DateTime}.
#'
#' @param depth_range_threshold Numeric. Rolling standard deviation threshold (meters)
#' below which observations are considered stationary. Default = 0.05.
#'
#' @param window Integer. Number of observations in the rolling window
#' used to calculate depth standard deviation. Default = 7.
#'
#' @param stationary_secs Numeric. Minimum number of seconds required for a
#' stationary period to be classified as fully stationary. Default = 30.
#'
#' @param sampling_int Numeric. Sampling interval in seconds. If 0 (default),
#' it is automatically estimated from the datetime column.
#'
#' @param drop_cols Logical. If TRUE (default), intermediate calculation
#' columns are removed before returning the data frame.
#'
#' @param plot Logical. If TRUE, diagnostic plots are generated showing:
#' \itemize{
#'   \item Depth colored by stationary status
#'   \item Rolling standard deviation with threshold
#' }
#'
#' @details
#' The returned column \code{is_stationary_status} is defined as:
#'
#' \itemize{
#'   \item \code{999} — Fully stationary (meets minimum time requirement)
#'   \item \code{0} — Moving
#'   \item Positive numeric value — Number of seconds stationary for
#'         short stationary periods that did not meet the minimum threshold
#' }
#'
#' If the detected sampling interval exceeds 30 seconds, the sonde is assumed
#' to be fixed in position and all observations are marked as stationary
#' (999).
#'
#' @return
#' The original data frame with an added column:
#'
#' \describe{
#'   \item{is_stationary_status}{Numeric flag indicating stationary status.}
#' }
#'
#' If \code{drop_cols = FALSE}, the following additional columns are retained:
#' \itemize{
#'   \item \code{depth_range}
#'   \item \code{is_stationary_initial}
#'   \item \code{stationary_block_id}
#'   \item \code{block_duration}
#' }
#'
#' @examples
#' \dontrun{
#' df_out <- is_stationary(df,
#'                         depth_col = depth_m,
#'                         datetime_col = DateTime,
#'                         depth_range_threshold = 0.03,
#'                         stationary_secs = 20,
#'                         plot = TRUE)
#' }
#'
#' @importFrom dplyr mutate pull group_by ungroup case_when if_else select n
#' @importFrom zoo rollapplyr
#' @importFrom stats sd
#' @export

is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          depth_range_threshold = 0.05,
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

    samp_int <-  median(as.numeric(diff(times), units = "secs"), na.rm = TRUE)   # Calculate the sampling interval

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

  # If window is more than the number of rows in the dataset
  if(window > nrow(df)){
    window <- nrow(df)
    message(paste("Window changed to maximum # of rows:", window))
  }

  # Set the number of obs equal to the min reqd_stationary_obs
  min_obs <- ceiling(stationary_secs / samp_int)

  out <- df |>
    dplyr::mutate(
      depth_range = zoo::rollapplyr({{ depth_col }},                                # 'right' means from the obs. look backwards n = window
                                 width = window,
                                 FUN = function(x) if (all(!is.na(x))) max(x) - min(x) else NA, # Calculate the rolling SD of the window
                                 # na.rm = TRUE,
                                 fill = NA),
      is_stationary_initial = dplyr::if_else(
        is.na(depth_range),
        FALSE,
        depth_range < depth_range_threshold),                                                   # Sets is_stationary_initial based on only SD
      stationary_block_id = dplyr::consecutive_id(.data$is_stationary_initial)) |>          # assigns a unique id based on each time is_stationary_initial changes
    dplyr::group_by(.data$stationary_block_id) |>
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
      ggplot2::geom_point(ggplot2::aes(color = as.factor(is_stationary_status)), size = 1.2, alpha = 0.3) +
      ggplot2::scale_y_reverse() + # Depth plots usually go down
      ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
      ggplot2::scale_color_manual(name = 'Seconds Stationary',
                                  values = mycolors) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(out,
                          ggplot2::aes(x = seq_len(nrow(out)),
                                       y = depth_range)) +
      ggplot2::geom_line(ggplot2::aes(color = "Rolling SD",
                                      linetype = "Rolling SD")) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = depth_range_threshold,
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
    out <- dplyr::select(out, -c(depth_range, is_stationary_initial, stationary_block_id, block_duration))
  }

  return(out)
}
