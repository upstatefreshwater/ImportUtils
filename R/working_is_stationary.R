library(dplyr)
library(zoo)

is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          depth_range_threshold = 0.05,
                          window = 7,
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

  # Adjust window if it's bigger than dataset
  if(window > nrow(df)) {
    window <- nrow(df)
    message(paste("Window changed to maximum # of rows:", window))
  }

  # Minimum number of observations to be considered stationary
  min_obs <- ceiling(stationary_secs / samp_int)

  # Compute rolling depth range
  depth_vals <- df |> dplyr::pull({{ depth_col }})
  depth_range <- zoo::rollapply(
    depth_vals,
    width = window,
    FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    fill = NA,
    align = "right"
  )

  # Initialize flag vector
  is_stationary_flag <- rep(FALSE, length(depth_vals))

  # Flag **all points in the window** if depth range < threshold
  for(i in seq_along(depth_range)) {
    if(!is.na(depth_range[i]) && depth_range[i] < depth_range_threshold){
      start_idx <- max(1, i - window + 1)
      is_stationary_flag[start_idx:i] <- TRUE
    }
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
        is_stationary_initial & block_duration >= min_obs ~ block_duration * samp_int,#999,
        is_stationary_initial & block_duration < min_obs ~ block_duration * samp_int,
        TRUE ~ 0
      )
    ) |>
    dplyr::ungroup()

  if(plot){
    # 1. Get all unique levels
    levels_list <- as.character(unique(df_out$is_stationary_status))
    ncolors <- length(levels_list)

    # 2. Generate random colors for everything first
    # We name them so ggplot knows exactly which color goes to which level
    mycolors <- setNames(sample(colors(distinct = TRUE), ncolors), levels_list)

    # 3. Explicitly overwrite your "known" values
    mycolors['999'] <- 'darkgreen'
    mycolors['0']   <- 'firebrick'
    # 2. Visualize to verify the threshold
    p1 <- ggplot2::ggplot(df_out, ggplot2::aes(x = seq_len(nrow(df_out)), y = {{depth_col}})) +
      ggplot2::geom_line(alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(color = as.factor(is_stationary_status)), size = 1.2, alpha = 0.3) +
      ggplot2::scale_y_reverse() + # Depth plots usually go down
      ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
      ggplot2::scale_color_manual(name = 'Seconds Stationary',
                                  values = mycolors) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(df_out,
                          ggplot2::aes(x = seq_len(nrow(df_out)),
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
    df_out <- dplyr::select(df_out, -c(depth_range, is_stationary_initial, stationary_block_id, block_duration))
  }

  return(df_out)
}
