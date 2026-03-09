#' Flag Stationary Periods in Depth Time Series
#'
#' Identifies periods where a depth sensor (sonde) remains relatively stationary.
#' Stationary periods are defined as times when the range of depth values within a rolling window
#' is below a specified threshold.
#'
#' @param df A data frame containing depth and datetime columns.
#' @param depth_col Unquoted column name containing depth measurements (default: `depth_m`).
#' @param datetime_col Unquoted column name containing datetime values (default: `DateTime`).
#' @param depth_range_threshold Numeric. Maximum allowed depth range within a rolling window
#'        to consider the sonde stationary (default: 0.05 m).
#' @param stationary_secs Numeric. Duration in seconds above which a stationary block is considered fully stationary
#'        and flagged with 999 (default: 30).
#' @param min_detection_secs Numeric. Minimum duration in seconds to consider a block as stationary
#'        for reporting purposes (default: 10). Blocks shorter than this are ignored.
#' @param sampling_int Numeric. Sampling interval in seconds. If 0 (default), the function estimates it from the datetime column.
#' @param drop_cols Logical. If TRUE (default), internal helper columns used for calculations are removed from the output.
#' @param plot Logical. If TRUE, produces a two-panel plot of depth and rolling range, with points colored by stationary status (default: FALSE).
#'
#' @return A data frame identical to `df` with an additional column:
#' \describe{
#'   \item{is_stationary_status}{Numeric:
#'     999 for fully stationary blocks >= `stationary_secs`,
#'     block duration in seconds for blocks >= `min_detection_secs` but < `stationary_secs`,
#'     0 otherwise.}
#' }
#'
#' @details
#' The function first calculates the rolling range of depth values over a window corresponding
#' to `min_detection_secs` (minimum 5 observations). All observations within a window meeting
#' the depth range threshold are flagged as stationary. Consecutive stationary observations are grouped
#' into blocks, and block durations in seconds are calculated. Blocks exceeding `stationary_secs`
#' are flagged as 999; shorter stationary blocks are reported as their actual duration to help users
#' adjust the stationary threshold.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   DateTime = seq.POSIXt(from = Sys.time(), by = "2 sec", length.out = 100),
#'   depth_m = c(rep(1, 10), rep(1.05, 10), rep(1.1, 80))
#' )
#' is_stationary(df, depth_col = depth_m, datetime_col = DateTime, stationary_secs = 10)
#' is_stationary(df, plot = TRUE)
#' }
#'
#' @export
is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          depth_range_threshold = 0.05,
                          stationary_secs = 30,
                          min_detection_secs = 10,
                          sampling_int = 0,
                          drop_cols = TRUE,
                          plot = FALSE) {

  if (nrow(df) < 2) stop("Data frame must contain at least two observations.")

  # Calculate sampling interval if not provided
  if (sampling_int == 0) {
    times <- df |> dplyr::pull({{ datetime_col }})
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
  depth_vals <- df |> dplyr::pull({{ depth_col }})

  # --- NOTE RccpRoll will be more efficient if we run into processing time concerns ---
  # --- but it creates an additional dependency, so for now sticking to zoo::rollApply ---
  # # --- RcppRoll version with NA handling ---
  # # RcppRoll does not have na.rm, so wrap with pmin/pmax to handle NAs
  # roll_min <- RcppRoll::roll_min(depth_vals, n = window, fill = NA, align = "right")
  # roll_max <- RcppRoll::roll_max(depth_vals, n = window, fill = NA, align = "right")
  #
  # # If you want to ignore NAs within the window:
  # # replace NA in rolling min/max with the min/max of non-NA values in that window
  # roll_range <- roll_max - roll_min
  # roll_range[is.na(roll_range)] <- NA  # optional, just ensures downstream logic treats them as not stationary

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
    jump_override[j:end_idx] <- TRUE
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

  if(plot){
    # Line values at depths above the minimum stationary time
    if('obs_depth' %in% names(df_out)){
      stationary_depths <- unique(df_out$obs_depth[df_out$is_stationary_status > 0])
    } else {
      stationary_depths <- unique(round(df_out$depth_m[df_out$is_stationary_status > 0], 1))
    }
    # Plotting as before
    levels_list <- as.character(unique(df_out$is_stationary_status))
    # fixed colors
    mycolors <- c("0" = "red", "999" = "green")

    # intermediate durations (exclude 0 and 999)
    other_vals <- setdiff(levels_list, c("0", "999"))

    if(length(other_vals) > 0){
      # use Dark2 palette and interpolate if more colors are needed
      base_pal <- RColorBrewer::brewer.pal(8, "Dark2")              # base palette
      pal <- grDevices::colorRampPalette(base_pal)(length(other_vals))  # interpolate to needed length
      mycolors <- c(mycolors, setNames(pal, other_vals))
    }

    p1 <- ggplot2::ggplot(df_out, ggplot2::aes(x = seq_len(nrow(df_out)), y = {{depth_col}})) +
      ggplot2::geom_line(alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(color = as.factor(is_stationary_status)), size = 1.2) +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
      ggplot2::scale_color_manual(name = 'Seconds Stationary', values = mycolors) +
      ggplot2::geom_hline(yintercept = stationary_depths, col = 'black',lty = 2) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(df_out, ggplot2::aes(x = seq_len(nrow(df_out)), y = roll_range)) +
      ggplot2::geom_line(ggplot2::aes(color = "Rolling Range", linetype = "Rolling Range")) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = depth_range_threshold, color = "Threshold", linetype = "Threshold")) +
      ggplot2::scale_color_manual(name = "", values = c("Rolling Range" = "red", "Threshold" = "black")) +
      ggplot2::scale_linetype_manual(name = "", values = c("Rolling Range" = "solid", "Threshold" = "dashed")) +
      ggplot2::labs(title = "Rolling Range", y = "Range (m)", x = "Observation Index") +
      ggplot2::theme_minimal()

    print(patchwork::wrap_plots(p1, p2, ncol = 1))
  }

  if(drop_cols){
    df_out <- df_out |> dplyr::select(-c(is_stationary_initial, stationary_block_id, block_duration))
  }

  return(df_out)
}
