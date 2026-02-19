read_datafile <- function(path){

  # Read the file to identify the lines
  file_content <- readLines(path)

  # Find the line number that contains "Date Time"
  start_line <- grep("Date Time", file_content)
  if (length(start_line)==0) {
    stop("Cannot locate 'Date Time' in the datafile./n
        Check that 'Date Time' exists in the raw data.") # Check for Date Time Row Existing
  }
  # Read the CSV starting from the identified line (Date Time)
  data <- read_csv(path,
                   skip = start_line - 1,
                   show_col_types = FALSE)

  # Format the Date Time column
  parsed_dates <- lubridate::parse_date_time(
    data$`Date Time`,
    orders = c('ymd HMS', 'mdy HMS', 'ymd HM', 'mdy HM'),
    quiet = TRUE
  )

  # If the first row is NA, it means none of our orders matched the data
  if (all(is.na(parsed_dates))) {
    stop(paste0("Format check failed: Could not parse 'Date Time' column with provided formats./n
            Formats tried were: ", 'ymd HMS', 'mdy HMS', 'ymd HM', 'mdy HM'))
  } else {
    # Apply the successfully parsed dates back to the dataframe
    data$`Date Time` <- parsed_dates
    message("Successfully parsed 'Date Time' column.")
  }

  if(all(is.na(lubridate::second(data$`Date Time`)))) {
    warning('No seconds were included in the raw data, check the data file formatting for "Date Time" column!')
  }

  return(data)
}
