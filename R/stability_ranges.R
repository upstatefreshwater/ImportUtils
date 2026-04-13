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

