# AquaTROLL DATA PROCESSING----
# Updated 10/24/24, MC
# Added 10/15/24 uploads on 1/17/25

# Load packages----
my_packages <- c("lubridate", "plyr", "dplyr", "naniar", "tidyr","stringr", "stringi", "purrr", "readr", "openxlsx")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data Read In Functions----
# CSV files will have 15-18 lines of information not necessarily needed for analysis. These functions will read in a csv file
# or a folder of csv files to automatically remove those rows.

# Function to read in CSV file
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
read_folder_of_files <- function(folder_path) {
  # List all CSV files in the folder
  file_paths <- list.files(folder_path, full.names = TRUE, pattern = "\\.csv$")

  # Apply the dynamic reading function to all files
  data_list <- map(file_paths, read_sonde_data_dynamic)

  # Combine all data into one dataframe
  combined_data <- bind_rows(data_list, .id = "source") # .id adds a column indicating the source file

  return(combined_data)
}


QL.1 <- read_sonde_data("./Oct-23/CSV Files/VuSitu_LiveReadings_2024-10-23_09-03-06_QL-1.csv")
QL.2 <- read_sonde_data("./Oct-23/CSV Files/VuSitu_LiveReadings_2024-10-23_09-35-41_QL-2.csv")
RH.1 <- read_sonde_data("./Oct-23/CSV Files/VuSitu_LiveReadings_2024-10-23_11-49-05_RH-1.csv")
RH.2 <- read_sonde_data("./Oct-23/CSV Files/VuSitu_LiveReadings_2024-10-23_12-00-54_RH-2.csv")

LW.1 <- read_sonde_data("./Oct-29-30/CSV Files/VuSitu_LiveReadings_2024-10-30_08-32-30_LW-1.csv")
LW.2 <- read_sonde_data("./Oct-29-30/CSV Files/VuSitu_LiveReadings_2024-10-30_09-43-25_LW-2.csv")
LT.1 <- read_sonde_data("./Oct-29-30/CSV Files/VuSitu_LiveReadings_2024-10-29_14-17-36_LT-1.csv")
LT.2 <- read_sonde_data("./Oct-29-30/CSV Files/VuSitu_LiveReadings_2024-10-29_14-03-20_LT-2.csv")

LT.1_comp <- read_sonde_data("./Oct-15/CSV Files/VuSitu_LiveReadings_2024-10-15_15-04-46_Device_Location.csv")
LT.2_comp <- read_sonde_data("./Oct-15/CSV Files/VuSitu_LiveReadings_2024-10-15_15-50-42_Device_Location.csv")

# Dataframe Cleaning Functions----
# Function to remove serial numbers from headers
clean_column_names <- function(df) {
  # Use gsub to remove undesired section
cleaned_col_names <- gsub("\\s*\\(\\d+\\)","",colnames(df))
  # Set the cleaned names as the column names
colnames(df) <- cleaned_col_names
return(df)
}

QL.1 <- clean_column_names(QL.1)
QL.2 <- clean_column_names(QL.2)
RH.1 <- clean_column_names(RH.1)
RH.2 <- clean_column_names(RH.2)

LW.1 <- clean_column_names(LW.1)
LW.2 <- clean_column_names(LW.2)
LT.1 <- clean_column_names(LT.1)
LT.2 <- clean_column_names(LT.2)

LT.1_comp <- clean_column_names(LT.1_comp)
LT.2_comp <- clean_column_names(LT.2_comp)

# Function to find potential bottom hits and remove remaining data from dataframe
bottom_hits <- function(df, turbidity_col, threshold) {
  # Find the first occurrence exceeding the threshold and filter the data
  filtered_data <- df %>%
    mutate(exceeding = !!sym(turbidity_col) > threshold) %>%
    filter(cumsum(exceeding) == 0)  # Keep rows until the first TRUE

  return(filtered_data)
}

# Function for bottom hits daily (from folder of csvs)
daily_bottom_hits <- function(df, turbidity_col, threshold, date_col) {
# Filter the data by date and apply the turbidity threshold
  filtered_data <- df %>%
    group_by(!!sym(date_col)) %>%  # Group by date
    mutate(exceeding = !!sym(turbidity_col) > threshold) %>%
    filter(cumsum(exceeding) == 0) %>%  # Keep rows until the first TRUE
    ungroup() %>%  # Ungroup after filtering
    select(-exceeding)  # Remove the temporary 'exceeding' column

  return(filtered_data)
}

# 100 is a decent threshold, but recommend checking your data  to see if there is a more subtle indicator
cleaned.QL.1 <- bottom_hits(QL.1, "Turbidity (NTU)", 100)
cleaned.QL.2 <- bottom_hits(QL.2, "Turbidity (NTU)", 100)
cleaned.RH.1 <- bottom_hits(RH.1, "Turbidity (NTU)", 100)
cleaned.RH.2 <- bottom_hits(RH.2, "Turbidity (NTU)", 20)

cleaned.LW.1 <- bottom_hits(LW.1, "Turbidity (NTU)", 100)
cleaned.LW.2 <- bottom_hits(LW.2, "Turbidity (NTU)", 50)
cleaned.LT.1 <- bottom_hits(LT.1, "Turbidity (NTU)", 100)
cleaned.LT.2 <- bottom_hits(LT.2, "Turbidity (NTU)", 100)

cleaned.LT.1_comp <- bottom_hits(LT.1_comp, "Turbidity (NTU)", 100)
cleaned.LT.2_comp <- bottom_hits(LT.2_comp, "Turbidity (NTU)", 50)

# Summarize by Depth Function----
# For less detailed contour heat maps or comparison to lab data, it may be essential to round the continuous data to the nearest 1 meter
summarize_by_depth <- function(df, depth_column) {
  # Round the depth to the nearest meter and summarize
  summarized_data <- df %>%
    mutate(rounded_depth = round(!!sym(depth_column))) %>%
    group_by(rounded_depth) %>%
    summarize(across(where(is.numeric),
              mean, na.rm = TRUE)) # Replace with other functions if needed

  return(summarized_data)
}

whole.QL.1 <- summarize_by_depth(cleaned.QL.1, "Depth (m)")
whole.QL.2 <- summarize_by_depth(cleaned.QL.2, "Depth (m)")
whole.RH.1 <- summarize_by_depth(cleaned.RH.1, "Depth (m)")
whole.RH.2 <- summarize_by_depth(cleaned.RH.2, "Depth (m)")

whole.LW.1 <- summarize_by_depth(cleaned.LW.1, "Depth (m)")
whole.LW.2 <- summarize_by_depth(cleaned.LW.2, "Depth (m)")
whole.LT.1 <- summarize_by_depth(cleaned.LT.1, "Depth (m)")
whole.LT.2 <- summarize_by_depth(cleaned.LT.2, "Depth (m)")

whole.LT.1_comp <- summarize_by_depth(cleaned.LT.1_comp, "Depth (m)")
whole.LT.2_comp <- summarize_by_depth(cleaned.LT.2_comp, "Depth (m)")

# Export these files as one Excel Workbook----
to_export <- list("Quaker-1" = whole.QL.1, "Quaker-2" = whole.QL.2,
                  "Red House-1" = whole.RH.1, "Red House-2" = whole.RH.2)
#write.xlsx(to_export,"Allegany_102324_Profiles.xlsx")

to_export <- list("Tiorati-1" = whole.LT.1, "Tiorati-2" = whole.LT.2,
                  "Welch-1" = whole.LW.1, "Welch-2" = whole.LW.2)
#write.xlsx(to_export,"Harriman_103024_Profiles.xlsx")

to_export <- list("Tiorati-1" = whole.LT.1_comp, "Tiorati-2" = whole.LT.2_comp)
#write.xlsx(to_export,"Harriman_101524_Profiles.xlsx")
