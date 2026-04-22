# Identify Stable Sensor Values Within Stationary Profiling Blocks

Evaluates whether sensor measurements collected during stationary
profiling periods are stable based on slope and range thresholds.
Stability is assessed within each stationary block identified by
[`is_stationary()`](is_stationary.md), and the function flags
observations that meet both slope and range criteria.

## Usage

``` r
TROLL_sensor_stable(
  df,
  value_col = pH_units,
  min_median_secs = 4,
  slope_thresh = NULL,
  range_thresh = NULL,
  settling_secs = 10,
  drop_cols = TRUE,
  verbose = FALSE,
  plot = FALSE
)
```

## Arguments

- df:

  A data frame containing sensor data and stationary block metadata.

- value_col:

  Unquoted name of the sensor column to evaluate for stability (e.g.,
  `pH_units`, `temp_C`, `sp_conductivity_uScm`).

- min_median_secs:

  Numeric. Minimum duration (seconds) of data to retain at the end of
  each stationary block. If stability criteria are not met, this
  trailing window is still flagged as stable and used for summary
  calculations. Default is `5`.

- slope_thresh:

  Numeric. Maximum allowable absolute slope (units per minute) for
  stable measurements. If `NULL`, a parameter-specific default is
  retrieved from `stability_ranges`.

- range_thresh:

  Numeric. Maximum allowable range (max - min) within the evaluation
  window. If `NULL`, a parameter-specific default is retrieved from
  `stability_ranges`.

- settling_secs:

  Numeric. Number of seconds to remove from the start of each stationary
  block to allow sensor equilibration. These observations are flagged
  with `is_stationary_status = 888`.

- drop_cols:

  Logical. If `TRUE` (default), intermediate diagnostic columns used
  during slope and range calculations are removed from the returned data
  frame.

- verbose:

  Logical. If `TRUE`, prints the depths where stationary blocks were
  identified.

- plot:

  Logical. If `TRUE`, generates a diagnostic plot using
  [`plot_stability()`](plot_stability.md). Intended for tuning and
  debugging, not for final analysis outputs.

## Value

A data frame containing the original input data with an added logical
column `<value_col>_stable`. This column indicates whether each
observation meets the stability criteria.

If `drop_cols = FALSE`, additional diagnostic columns are included:

- `slope`: slope of the evaluation window

- `range`: value range within the window

- `n_used`: number of observations used

- `n_dropped`: number of leading observations removed

- `slope_ok`, `range_ok`: logical threshold checks

## Details

For each stationary block, slope (units per minute) and value range are
calculated iteratively using progressively smaller windows, beginning
with the full stationary block and removing leading observations.

A minimum tail duration defined by `min_median_secs` is always retained.
The function searches for the earliest point in the block where both
`slope_thresh` and `range_thresh` are satisfied, and flags that
observation and all subsequent observations as stable.

For optical parameters (e.g., turbidity, chlorophyll, fluorescence), all
observations within stationary periods (after settling removal) are
flagged as stable. Range-based criteria are not applied, and slope is
used only to generate diagnostic warnings.

For non-optical sensors, the function searches until \`slope_thresh\`
and \`range_thresh\` are both met concurrently, and marks the remainder
of the stationary block at "stable". If neither threshold can be met,
the final `min_median_secs` of data within the stationary block are
retained and flagged as stable.

The input data frame must contain the following columns:

- `DateTime`

- `depth_m`

- `stationary_depth`

- `stationary_block_id`

- `is_stationary_status`

- the column specified by `value_col`

Observations with `NA` values in `value_col` are excluded from stability
calculations.

This function requires that stationary blocks have already been
identified using [`is_stationary`](is_stationary.md).Stability
calculations are performed only on observations where
`is_stationary_status == 999`, i.e., fully stationary data after removal
of the initial settling period. The function will error if any
stationary block does not contain sufficient duration (after settling
removal) to satisfy `min_median_secs`.

## See also

[`is_stationary`](is_stationary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- is_stationary(sensor_data)

dat <- TROLL_sensor_stable(
  df = dat,
  value_col = pH_units,
  min_median_secs = 5,
  slope_thresh = 0.05,
  range_thresh = 0.02
)
} # }
```
