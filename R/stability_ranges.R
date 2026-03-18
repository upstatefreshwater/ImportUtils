#' Default Range Values For Flagging Sensor Stability
#'
#' A dataset containing the default threshold values used to determine sensor
#' stability within the \code{TROLL_sensor_stable()} function. Stability is
#' typically defined as the maximum allowable range (max - min) over a
#' specific window of time.
#'
#' @format ## `stability_ranges`
#' A tibble with 8 rows and 2 columns:
#' \describe{
#'   \item{param}{Character. The name of the water quality parameter (e.g., temperature, pH).}
#'   \item{range_thresh}{Double. The maximum allowable difference between
#'   the maximum and minimum values in a window for the sensor to be
#'   considered "stable".}
#' }
#'
#' @source Internal calculation based on manufacturer specifications and
#' field testing.
#'
#' @seealso \code{\link{TROLL_sensor_stable}}
"stability_ranges"

stability_ranges <- tibble::tribble(
  ~param,                                  ~range_thresh,

  'sp_conductivity_uScm',                   0.5,
  "temperature_C",                          0.15,
  "pH_units",                               0.1,
  "DO_mgL",                                 0.15,
  "turbidity_NTU",                          0.5,
  "chlorophyll_RFU",                        0.3,
  "bga_fluorescence_RFU",                   0.3,
  "ORP_mv",                                 10

)

# usethis::use_data(stability_ranges,
#                   overwrite = TRUE)
# #
