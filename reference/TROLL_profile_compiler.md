# Bulk Process TROLL Sonde Profiles

`TROLL_profile_compiler()` reads raw TROLL sonde data, identifies
stationary periods, evaluates sensor stability for each parameter,
optionally summarizes results, and provides optional plotting of raw and
final summarized data.

## Usage

``` r
TROLL_profile_compiler(
  path,
  depth_col = depth_m,
  datetime_col = DateTime,
  stn_depthrange = 0.1,
  stn_secs = 45,
  stn_rollwindow_secs = 10,
  stbl_settle_secs = 10,
  stbl_min_secs = 5,
  stbl_range_thresholds = NULL,
  summarize_data = TRUE,
  drop_cols = TRUE,
  plot = c(Final = FALSE, Stationary = FALSE),
  debug = FALSE
)
```

## Arguments

- path:

  Character. Path to the raw TROLL CSV file.

- depth_col:

  Unquoted column name for water depth (numeric data).

- datetime_col:

  Unquoted column name for date-time (POSIXct, POSIXt, or Date).

- stn_depthrange:

  Numeric. Depth range threshold for
  [`is_stationary()`](is_stationary.md).

- stn_secs:

  Numeric. Minimum time (seconds) required for a stationary block to be
  considered fully stationary (flagged by:
  `is_stationary_status = 999`).

- stn_rollwindow_secs:

  Numeric. Window size (seconds) used to compute rolling range for
  stationary detection.

- stbl_settle_secs:

  Numeric. Seconds to trim from the start of each stationary block.

- stbl_min_secs:

  Numeric. Minimum seconds used to calculate a summary statistic if
  stability is not reached within a stationary period.

- stbl_range_thresholds:

  Optional named numeric vector. Custom stability thresholds per
  parameter. See [`stability_ranges`](stability_ranges.md) for defaults
  and naming.

- summarize_data:

  Logical. If `TRUE`, returns both \`\$Flagged_Data\` and
  \`\$Summary_Data\` (medians) as a list.

- drop_cols:

  Logical. If `TRUE`, removes intermediate processing columns.

- plot:

  Logical or named logical vector. Controls optional plotting from
  `is_stationary` and `TROLL_stable_summary`. Default is
  `c(Final = FALSE, Stationary = FALSE)`.

- debug:

  Logical. If TRUE, prints the parameters being looped over during
  stability calculation to aid in debugging.

## Value

The output depends on the `summarize_data` argument:

- If `summarize_data = FALSE`:

  Returns a `data.frame` with original sensor data and appended
  `_stable` logical flags.

- If `summarize_data = TRUE`:

  Returns a `list` containing:

  - `Flagged_Data`: The full data frame with stability flags.

  - `Summary_Data`: A summarized tibble of median sensor readings for
    each stable stationary period.

## Details

This function executes the TROLL processing workflow in the following
order:

1.  **Ingestion & Cleanup:** Reads data via
    [`TROLL_read_data()`](TROLL_read_data.md) and standardizes names via
    [`TROLL_rename_cols()`](TROLL_rename_cols.md).

2.  **Stationary Detection:** Uses [`is_stationary()`](is_stationary.md)
    to identify depths where the sonde was held steady. Arguments for
    this step are prefixed with `stn_`.

3.  **Stability Evaluation:** Calls
    [`TROLL_sensor_stable()`](TROLL_sensor_stable.md) for each parameter
    to identify "equilibrated" data points. Arguments for this step are
    prefixed with `stbl_`. Core parameters in data are automatically
    detected from:
    `troll_column_dictionary[troll_column_dictionary$stbl_calc==TRUE,]`.

4.  **Summarization:** If requested, calculates median values for stable
    windows at each depth using
    [`TROLL_stable_summary()`](TROLL_stable_summary.md).

5.  **Visualization:** Optionally generates diagnostic plots comparing
    raw profiles to filtered stationary/stable points.

## See also

[`TROLL_read_data`](TROLL_read_data.md),
[`TROLL_rename_cols`](TROLL_rename_cols.md),
[`is_stationary`](is_stationary.md),
[`TROLL_sensor_stable`](TROLL_sensor_stable.md),
[`TROLL_stable_summary`](TROLL_stable_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard profile compilation
result <- TROLL_profile_compiler(
  path = "extdata/sonde_file.csv",
  depth_col = depth_m,
  datetime_col = DateTime,
  stn_depthrange = 0.1,
  stn_secs = 45,
  stbl_min_secs = 5,
  plot = c(Final = TRUE, Stationary = FALSE)
)

# Access results
head(result$Flagged_Data)
head(result$Summary_Data)
} # }
```
