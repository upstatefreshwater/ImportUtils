# Format and Trim HOBO Logger Files

Reads a HOBO logger CSV file, standardizes column names, determines
sampling interval, and trims out-of-water periods at the start and end
of deployment.

## Usage

``` r
hoboformat(
  file_path,
  dataset_name,
  trim_start = TRUE,
  trim_end = TRUE,
  pressure_jump_thresh = 0.05
)
```

## Arguments

- file_path:

  Character. Path to the CSV file.

- dataset_name:

  Character. Name assigned to the dataset.

- trim_start:

  Logical. Should the start of deployment be trimmed?

- trim_end:

  Logical. Should the end of deployment be trimmed?

- pressure_jump_thresh:

  Numeric. Threshold for detecting pressure transitions.

## Value

A cleaned and trimmed data frame with standardized columns:

- datetime:

  POSIXct timestamp

- abspress:

  Absolute pressure

- temp:

  Temperature

- dataset:

  Dataset name

## Examples

``` r
if (FALSE) { # \dontrun{
df <- formatischua("my_file.csv", "site1")
} # }
```
