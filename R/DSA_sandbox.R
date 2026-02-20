data_final <- data%>% group_by(depthwholem)%>%arrange(id)%>%
  mutate( roll_sd = rollapply(DO_mgL, 5, sd, fill = NA),
          roll_mean = rollapply(DO_mgL, 5, mean, fill = NA),
          slope = abs(c(NA, diff(roll_mean))) )%>%
  # Require stability AND low slope
  mutate(stable = roll_sd < 0.20 & slope < 0.10) %>%
  # Keep only stable points AFTER first unstable change
  filter(row_number() > max(which(slope > 0.20), na.rm = TRUE)) %>%
  filter(stable)%>% summarise(across( -c(DateTime, depth_halfm, id),
                                      ~ mean(.x, na.rm = TRUE) ), .groups = "drop")%>%
  select(any_of(c("depthwholem", "depth_m", "temperature_C", "sp_conductivity_uScm",
                  "pH_units", "pH_mV", "DO_mgL", "DO_per", "turbidity_NTU", "ORP_mV",
                  "chlorophyll_RFU", "bga_fluorescence_RFU", "pressure_psi",
                  "latitude_deg", "longitude_deg")))

param_col <- c("Date Time", "Depth (m)", "Temperature (°C)", "Specific Conductivity (µS/cm)", "pH (pH)", "pH mV (mV)",
               "RDO Concentration (mg/L)", "RDO Saturation (%Sat)", "Turbidity (NTU)", "ORP (mV)",
               "Chlorophyll-a Fluorescence (RFU)", "BGA-PC Fluorescence (RFU)", "Pressure (psi)",
               "Latitude (°)", "Longitude (°)", "Marked")
param_rename <- c(DateTime = "Date Time", depth_m = "Depth (m)", temperature_C = "Temperature (°C)",
                  sp_conductivity_uScm = "Specific Conductivity (µS/cm)", pH_units = "pH (pH)",
                  pH_mV = "pH mV (mV)", DO_mgL = "RDO Concentration (mg/L)", DO_per = "RDO Saturation (%Sat)",
                  turbidity_NTU = "Turbidity (NTU)", ORP_mV = "ORP (mV)",
                  chlorophyll_RFU = "Chlorophyll-a Fluorescence (RFU)", bga_fluorescence_RFU = "BGA-PC Fluorescence (RFU)",
                  pressure_psi = "Pressure (psi)", latitude_deg = "Latitude (°)", longitude_deg = "Longitude (°)")


dat <- read_datafile('inst/extdata/2025-05-27_LT1.csv')
dat <- read_datafile('inst/extdata/2025-09-16_LT1.csv')
# dat <- read_datafile('inst/extdata/2025-05-13_LW1.csv')
# dat <- read_datafile('inst/extdata/2025-10-01_LW1.csv')


dat <- rename_cols(dat) %>%
  mutate(depthwholem = (round_half_up(depth_m)))

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

try <- is_stationary(dat,drop_cols = F,
                     plot = T)



plotstuff <- function(df,param){
  trash <- df %>% ungroup() %>%
    select(DateTime,depth_m,depthwholem,!!sym(param)) %>%
    mutate(x = c(depthwholem - lag(depthwholem,default = 0)),
           label = ifelse(x==1,depthwholem,NA))

  ggplot(trash) +
    geom_point(aes(x = DateTime, y = !!sym(param))) +
    geom_label(aes(x = DateTime, y = !!sym(param),label = label),
               nudge_y = 0.25)
}

plotstuff(df = dat,'temperature_C')
plotstuff(df = dat,'DO_mgL')
plotstuff(df = dat, 'depth_m')


