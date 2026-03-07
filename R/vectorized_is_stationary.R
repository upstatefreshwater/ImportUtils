# this needs to be tested/vetted/understood
#
# library(dplyr)
# library(zoo)
# library(patchwork)
# library(ggplot2)

is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          depth_range_threshold = 0.05,
                          stationary_secs = 30,
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
      message("Sampling interval > 30s detected. Sonde assumed fixed in position.")
      return(df |> dplyr::mutate(is_stationary_status = 999))
    }
  } else {
    samp_int <- sampling_int
  }

  # Compute window size in # of observations
  window <- ceiling(stationary_secs / samp_int)
  if(window > nrow(df)) {
    window <- nrow(df)
    message(paste("Window changed to maximum # of rows:", window))
  }

  # Minimum number of observations to be considered stationary
  min_obs <- ceiling(stationary_secs / samp_int)

  # Pull depth values
  depth_vals <- df |> dplyr::pull({{ depth_col }})

  # Rolling min and max
  roll_min <- zoo::rollapply(depth_vals, width = window, FUN = min, fill = NA, align = "right")
  roll_max <- zoo::rollapply(depth_vals, width = window, FUN = max, fill = NA, align = "right")
  roll_range <- roll_max - roll_min

  # Vectorized flagging of stationary points: any point within a window that meets threshold
  is_stationary_flag <- rep(FALSE, length(depth_vals))
  # Only mark positions where rolling range < threshold
  valid_idx <- which(!is.na(roll_range) & roll_range < depth_range_threshold)
  if(length(valid_idx) > 0){
    start_idx <- pmax(1, valid_idx - window + 1)
    end_idx <- valid_idx
    # Efficient vectorized assignment
    is_stationary_flag[unlist(mapply(seq, start_idx, end_idx))] <- TRUE
  }

  # Identify consecutive blocks
  stationary_block_id <- dplyr::consecutive_id(is_stationary_flag)

  df_out <- df |>
    dplyr::mutate(
      is_stationary_initial = is_stationary_flag,
      stationary_block_id = stationary_block_id
    ) |>
    dplyr::group_by(stationary_block_id) |>
    dplyr::mutate(
      block_duration = dplyr::n(),
      is_stationary_status = dplyr::case_when(
        is_stationary_initial & block_duration >= min_obs ~ block_duration * samp_int,
        is_stationary_initial & block_duration < min_obs ~ block_duration * samp_int,
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
    ncolors <- length(levels_list)
    mycolors <- setNames(sample(colors(distinct = TRUE), ncolors), levels_list)
    mycolors['999'] <- 'darkgreen'
    mycolors['0']   <- 'firebrick'

    p1 <- ggplot2::ggplot(df_out, ggplot2::aes(x = seq_len(nrow(df_out)), y = {{depth_col}})) +
      ggplot2::geom_line(alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(color = as.factor(is_stationary_status)), size = 1.2) +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
      ggplot2::scale_color_manual(name = 'Seconds Stationary', values = mycolors) +
      ggplot2::geom_hline(yintercept = stationary_depths, col = 'red',lty = 2) +
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
