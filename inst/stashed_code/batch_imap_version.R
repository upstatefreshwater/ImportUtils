library(dplyr)
library(purrr)
library(rlang)

batch_sensor_stable <- function(dat_stationary, params, ranges,
                                min_secs = 5, stationary_thresh = 10) {

  # Iterate over parameters and return a named list of flag columns
  flag_list <- purrr::imap(params, ~{
    range_val <- ranges$range_thresh[ranges$param == .x]
    value_col <- rlang::sym(.x)

    out <- TROLL_sensor_stable(
      dat_stationary,
      value_col = !!value_col,
      min_secs = min_secs,
      range_thresh = range_val,
      stationary_thresh = stationary_thresh
    )

    # Rename the stable column to include parameter name
    stable_col <- paste0(.x, "_stable")
    dplyr::rename(out, !!stable_col := stable)
  })

  # Bind all the stable flag columns to dat_stationary
  dat_stationary %>% bind_cols(flag_list)
}

batch_sensor_stable(dat_stationary = dat_stationary,
                    params = params,
                    ranges = stability_ranges)
