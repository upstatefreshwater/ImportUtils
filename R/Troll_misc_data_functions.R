# To read in CSV file
read_sonde_data <- function(file_path) {
  # Read the file to identify the line with "Date Time"
  file_content <- readLines(file_path)

  # Find the line number that contains "Date Time"
  start_line <- grep("Date Time", file_content)

  # Read the CSV starting from the identified line
  data <- read_csv(file_path, skip = start_line - 1)

  return(data)
}

# Function to read in folder of CSV files
# Recommended to make folders for each lake site
read_files <- function(folder_path) {
  # List all CSV files in the folder
  file_paths <- list.files(folder_path, full.names = TRUE, pattern = "\\.csv$")

  # Apply the dynamic reading function to all files
  data_list <- map(file_paths, read_sonde_data)

  # Combine all data into one dataframe
  combined_data <- bind_rows(data_list, .id = "source") # .id adds a column indicating the source file

  return(combined_data)
}

# Strip numbers from column names
clean_column_names <- function(df) {
  # Use gsub to remove undesired section
  cleaned_col_names <- gsub("\\s*\\(\\d+\\)","",colnames(df))
  # Set the cleaned names as the column names
  colnames(df) <- cleaned_col_names
  return(df)
}

# To trim after max depth is reached
trim_after_max_depth <- function(data, group_col = NULL, depth_col) {

  if (!is.null(group_col)) {
    # Grouped version
    data %>%
      group_by(across(all_of(group_col))) %>%
      group_modify(~ {
        max_idx <- which.max(.x[[depth_col]])
        .x[1:max_idx, ]   # keep rows from start to maximum depth
      }) %>%
      ungroup()
  } else {
    # Ungrouped version
    max_idx <- which.max(data[[depth_col]])
    data[1:max_idx, ]
  }
}

# To trim out the data before stabilization at 0 m
trim_depth_by_group <- function(data, group_col, depth_col, n = 5) {
  data %>%
    group_by(across(all_of(group_col))) %>%            # group by the specified column(s)
    group_modify(~ {
      depth_vec <- .x[[depth_col]]
      exceed_idx <- which(depth_vec > 1)[1]

      if (is.na(exceed_idx)) {
        return(.x)   # no trimming if depth never exceeds 1
      }

      keep_start <- max(1, exceed_idx - n)
      .x[keep_start:nrow(.x), ]  # keep rows from keep_start to end
    }) %>%
    ungroup()
}
