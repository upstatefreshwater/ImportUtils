#' Identify Stationary Periods in Sonde Data
#'
#' @param df A data frame
#' @param col The column containing depth measurements
#' @param sd_thresh The standard deviation threshold for stability
#' @param window The rolling window size (observations)
#' @param min_obs The minimum number of observations to be considered stationary
#' @param drop_cols Boolean to drop intermediate calculation columns
#'
#' @export
is_stationary <- function(df,
                          depth_col = depth_m,
                          datetime_col = DateTime,
                          sd_thresh = 0.05,
                          window = 7,
                          min_obs = 15,
                          drop_cols = TRUE,
                          plot = FALSE) {

  # Extract datetime vector safely for interval calculation
  times <- df %>% dplyr::pull({{ datetime_col }})

  # Calculate interval in seconds
  samp_int <- as.numeric(difftime(times[2], times[1], units = "secs"))

  if(samp_int > 30){
    message('Sampling interval > 30s detected. Sonde assumed to be fixed in position.')
    return(df %>% dplyr::mutate(is_stationary_status = 999))
  }

  out <- df %>%
    dplyr::mutate(
      # Using your exact names: depth_sd, is_stable_initial, group_id
      depth_sd = zoo::rollapplyr({{ depth_col }},
                                 width = window,
                                 FUN = stats::sd,
                                 na.rm = TRUE,
                                 fill = NA),
      is_stable_initial = .data$depth_sd < sd_thresh,
      group_id = dplyr::consecutive_id(.data$is_stable_initial)
    ) %>%
    dplyr::group_by(.data$group_id) %>%
    dplyr::mutate(
      # Using your exact name: block_duration
      block_duration = dplyr::n(),

      # Final logic - fixed the variable name mismatch
      is_stationary_status = dplyr::case_when(
        .data$is_stable_initial & .data$block_duration >= min_obs ~ 999,
        .data$is_stable_initial & .data$block_duration < min_obs ~ .data$block_duration * samp_int,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup()

  if (drop_cols) {
    out <- dplyr::select(out, -c(depth_sd, is_stable_initial, group_id, block_duration))
  }

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
    p1 <- ggplot2::ggplot(try, aes(x = 1:nrow(try), y = depth_m)) +
      ggplot2::geom_line(alpha = 0.4) +
      ggplot2::geom_point(aes(color = as.factor(is_stationary_status)), size = 0.8) +
      ggplot2::scale_y_reverse() + # Depth plots usually go down
      ggplot2::labs(title = "Sonde Depth (Colored by Stationary Flag)", y = "Depth (m)", x = "Observation Index") +
      ggplot2::scale_color_manual(name = 'Seconds Stationary',
                                  values = mycolors) +
      ggplot2::theme_minimal()

    p2 <- ggplot2::ggplot(try, aes(x = 1:nrow(try), y = depth_sd)) +
      ggplot2::geom_line(aes(color = "Rolling SD")) +  # Map to a name
      ggplot2::geom_hline(aes(yintercept = 0.02,
                              color = "SD Threshold",
                              linetype = "SD Threshold")) +
      # Define the colors explicitly
      ggplot2::scale_color_manual(name = "",
                                  values = c("Rolling SD" = "red",
                                             "SD Threshold" = NULL)) +
      # Define the linetypes (solid for the line, dashed for the hline)
      ggplot2::scale_linetype_manual(name = "",
                                     values = c("Rolling SD" = "solid",
                                                "SD Threshold" = "dashed")) +
      ggplot2::labs(title = "Rolling Standard Deviation",
                    y = "SD (m)",
                    x = "Observation Index") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = 'right')


    # Combine the plots
    print(p1 / p2)

  }
  return(out)
}
