#' Read TAqua-Troll Data File
#'
#' Reads a raw TROLL data file and automatically locates the header row
#' containing `"Date Time"`. The function validates that the timestamp column
#' exists and ensures that timestamps include **second-level resolution**.
#' Supported timestamp formats are `"ymd HMS"` and `"mdy HMS"`. .
#'
#' @param path Character string. File path to the raw TROLL data file.
#'
#' @return A tibble containing the imported data with the original column
#' structure from the raw file.
#'
#' @details
#' The function performs several validation checks:
#' \itemize{
#'   \item Confirms the file exists at the specified path.
#'   \item Searches the file for the row containing `"Date Time"` to identify
#'   the start of the data table.
#'   \item Ensures exactly one header row containing `"Date Time"` exists.
#'   \item Imports the data using \code{readr::read_csv()} starting from the
#'   detected header row.
#'   \item Verifies that timestamps in the `"Date Time"` column include seconds.
#' }
#'
#' If timestamps contain only minute resolution (e.g., `"ymd HM"` or `"mdy HM"`),
#' the function will error.
#'
#' @examples
#' \dontrun{
#' dat <- TROLL_read_data("data/raw/troll_sensor_data.csv")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom lubridate parse_date_time
#'
#' @export

TROLL_read_data <- function(path){
  # Check that file exists at the specified path
  if(!file.exists(path)){
    stop("File does not exist at the specified path:\n", path)
  }
  # Read the file to identify the start line
  file_content <- readLines(path)

  # Find the line number that contains "Date Time"
  start_line <- grep("Date Time", file_content)
  if (length(start_line)==0) {
    stop("Cannot locate 'Date Time' in the datafile.\n
        Check that 'Date Time' exists in the raw data.") # Check for Date Time Row Existing
  }
  if (length(start_line) > 1) {
    stop("Multiple lines containing 'Date Time' were found. Cannot determine which one is the header row.")
  }
  # Read the CSV starting from the identified line (Date Time)
  data <- readr::read_csv(path,
                          skip = start_line - 1,
                          show_col_types = FALSE)

  # Double check (could help with debugging)
  if (!("Date Time" %in% names(data))) {
    stop("Column 'Date Time' was not found after reading the file. Check file structure.")
  }

  raw_dt <- data$`Date Time`

  # Attempt strict parsing WITH seconds
  parsed_hms <- lubridate::parse_date_time(
    raw_dt,
    orders = c("ymd HMS", "mdy HMS"),
    quiet = TRUE
  )

  if (any(is.na(parsed_hms))) { # If any are returned as NA (parsing failed)

    # Try minute-only formats to diagnose cause
    parsed_hm <- lubridate::parse_date_time(
      raw_dt,
      orders = c("ymd HM", "mdy HM"),
      quiet = TRUE
    )

    if (all(!is.na(parsed_hm))) {
      stop("Timestamp format error: seconds are missing from 'Date Time'.\n",
           "Second-level resolution is required. Please check the raw data file.")
    } else {
      stop("Timestamp format error: could not parse 'Date Time' column.\n",
           "Expected formats: ymd HMS or mdy HMS.")
    }
  }

  return(data )
}
