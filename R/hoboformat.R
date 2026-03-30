#' Trim Out-of-Water Data
#'
#' Internal helper used by format_ischua().
#'
#' @keywords internal

# Helper Function
trim_data <- function(df, pressure_jump_thresh = 0.05,
                      trim_start = TRUE, trim_end = TRUE) {

  # --------------------------------------------
  # Helper function to trim out-of-water rows
  # --------------------------------------------

  df$abspress <- as.numeric(df$abspress)
  df$dP <- c(NA, diff(df$abspress))

  ### --- START trimming (beginning of deployment) ---
  if (trim_start && "Coupler Detached" %in% names(df)) {

    # Locate the first recorded out-of-water event at the start of deployment
    start_idx <- which(df$`Coupler Detached` == "Logged")

    if (length(start_idx) > 0) { #in case there is multiple coupler detatched logged points

      # Begin scanning forward from the coupler detachment point
      start_scan_idx<- start_idx[1]

      # Translate a 15-minute search window into number of rows (sampling-rate aware)
      dt_sec <- median(diff(df$datetime), na.rm = TRUE)
      dt_sec <- as.numeric(dt_sec, units = "secs")
      max_steps <- ceiling((15 * 60) / dt_sec)

      # Track scan progress and whether a pressure transition is detected
      steps_taken <- 0
      jump_found  <- FALSE

      # Scan forward to find the first in-water pressure transition
      # (preferred signal when sampling resolution allows)
      while (start_scan_idx < nrow(df) && steps_taken <= max_steps) {
        if (!is.na(df$dP[start_scan_idx + 1]) &&
            abs(df$dP[start_scan_idx + 1]) > pressure_jump_thresh) {
          jump_found <- TRUE
          break
        }
        start_scan_idx <- start_scan_idx + 1
        steps_taken <- steps_taken + 1
      }

      # If no pressure jump is detected, assume coarse sampling and trim on coupler alone
      if (!jump_found) {

        trim_row <- start_idx[1] + 1

        warning(
          paste(
            "No pressure jump >",
            pressure_jump_thresh,
            "found near start of file.",
            "Using coupler detachment to trim data (coarse sampling likely)."
          )
        )

        df <- df[trim_row:nrow(df), ]
        message(paste("Trimmed start at row", trim_row,
                      "based on coupler detachment"))

      } else {

        # If a pressure transition is detected, trim at the first in-water record
        df <- df[start_scan_idx:nrow(df), ]
        message(paste("Trimmed start at row", start_scan_idx,
                      "based on pressure transition"))

      }
    }
  }

  ### --- END trimming (end of deployment) ---
  if (trim_end && "Coupler Attached" %in% names(df)) {

    # Locate the first recorded out-of-water event at the end of deployment
    end_idx <- which(df$`Coupler Attached` == "Logged")

    if (length(end_idx) > 0) {

      # Begin scanning backward from just before the coupler attachment point
      end_scan_idx <- end_idx[1] - 1

      # Translate a 15-minute search window into number of rows (sampling-rate aware)
      dt_sec <- median(diff(df$datetime), na.rm = TRUE)
      dt_sec <- as.numeric(dt_sec, units = "secs")
      max_steps <- ceiling((15 * 60) / dt_sec)

      # Track scan progress and whether a pressure transition is detected
      steps_taken <- 0
      jump_found  <- FALSE

      # Scan backward to find the last in-water pressure transition
      # (preferred signal when sampling resolution allows)
      while (end_scan_idx > 1 && steps_taken <= max_steps) {
        if (!is.na(df$dP[end_scan_idx]) &&
            abs(df$dP[end_scan_idx]) > pressure_jump_thresh) {
          jump_found <- TRUE
          break
        }
        end_scan_idx <- end_scan_idx - 1
        steps_taken <- steps_taken + 1
      }

      # If no pressure jump is detected, assume coarse sampling and trim on coupler alone
      if (!jump_found) {

        trim_row <- end_idx[1] - 1

        warning(
          paste(
            "No pressure jump >",
            pressure_jump_thresh,
            "found near end of file.",
            "Using coupler attachment to trim data"
          )
        )

        df <- df[1:trim_row, ]
        message(paste("Trimmed end at row", trim_row,
                      "based on coupler attachment"))

      } else {

        # If a pressure transition is detected, trim at the last in-water record
        df <- df[1:end_scan_idx, ] #replace i
        message(paste("Trimmed end at row", end_scan_idx,
                      "based on pressure transition"))

      }
    }
  }
  # Remove helper column and return trimmed data
  df$dP <- NULL
  return(df)
}

#' Format and Trim HOBO Logger Files
#'
#' Reads a HOBO logger CSV file, standardizes column names,
#' determines sampling interval, and trims out-of-water
#' periods at the start and end of deployment.
#'
#' @param file_path Character. Path to the CSV file.
#' @param dataset_name Character. Name assigned to the dataset.
#' @param trim_start Logical. Should the start of deployment be trimmed?
#' @param trim_end Logical. Should the end of deployment be trimmed?
#' @param pressure_jump_thresh Numeric. Threshold for detecting pressure transitions.
#'
#' @return A cleaned and trimmed data frame with standardized columns:
#' \describe{
#'   \item{datetime}{POSIXct timestamp}
#'   \item{abspress}{Absolute pressure}
#'   \item{temp}{Temperature}
#'   \item{dataset}{Dataset name}
#' }
#'
#' @examples
#' \dontrun{
#' df <- formatischua("my_file.csv", "site1")
#' }
#'
#' @export

# MAIN FUNCTION <3

hoboformat <- function(
    file_path,
    dataset_name,
    trim_start = TRUE,
    trim_end = TRUE,
    pressure_jump_thresh = 0.05
) {

  df <- read.csv(file_path, skip = 1, check.names = FALSE)

  #validate columns (if any ofther columns say date or time it might be problem)
  has_time  <- any(grepl("Date", names(df)))
  has_press <- any(grepl("Pres", names(df)))
  has_temp  <- any(grepl("Temp", names(df)))

  if (!(has_time && has_press && has_temp)) {
    stop("Invalid logger file: missing Date, Pressure, or Temperature column")
  }

  #rename main columns (getting rid of the parentheses and whats inside them might need to change)
  names(df) <- sub(".*Date.*", "datetime", names(df))
  names(df) <- sub(".*Pres.*", "abspress", names(df))
  names(df) <- sub(".*Temp.*", "temp", names(df))
  names(df) <- sub("\\s*\\(.*\\)", "", names(df))

  #parse datetime
  df$datetime <- as.POSIXct(
    df$datetime,
    format = "%m/%d/%y %I:%M:%S %p",
    tz = "UTC"
  )

  #sampling interval
  samp_diff_num <- as.numeric(df$datetime[2] - df$datetime[1], units = "secs")

  if (samp_diff_num < 60) {
    interval <- samp_diff_num
    units <- "seconds"
  } else if (samp_diff_num < 3600) {
    interval <- samp_diff_num / 60
    units <- "minutes"
  } else {
    interval <- samp_diff_num / 3600
    units <- "hours"
  }

  message(paste0("HOBO sampling interval was: ", interval, " ", units))



  df$dataset <- dataset_name #add dataset name


  #Trim start/end of data (out of water)
  # 5. TRIM DATA using helper function
  df <- trim_data(df,
                  pressure_jump_thresh = pressure_jump_thresh,
                  trim_start = trim_start,
                  trim_end = trim_end)

  return(df)
}

