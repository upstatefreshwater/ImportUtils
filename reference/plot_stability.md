# Plot Sensor Stability Diagnostics

Generates a time series plot of a single sensor parameter, highlighting
stationary and stability states as identified by
[`is_stationary()`](is_stationary.md) and
[`TROLL_sensor_stable()`](TROLL_sensor_stable.md).

## Usage

``` r
plot_stability(df, value_col_sym, value_flag_col, range_thresh)
```

## Arguments

- df:

  A data frame containing at minimum:

  - `DateTime`

  - the sensor column specified by `value_col_sym`

  - a logical stability column (`value_flag_col`)

  - `is_stationary_status`

- value_col_sym:

  Unquoted name of the sensor column to plot.

- value_flag_col:

  Character. Name of the logical column indicating stability (typically
  `"<param>_stable"`).

- range_thresh:

  Numeric. Range threshold used for stability detection. If `NA`, no
  threshold lines are drawn.

## Value

The plot is printed.

## Details

Observations are colored by profiling state:

- `"Sonde Moving"`: Sonde is in motion

- `"Unstable Stationary"`: Stationary but stability criteria not met

- `"Stable Stationary"`: Meets stability criteria

- `"Trimmed (Settling)"`: Removed from analysis due to settling time

If `range_thresh` is provided, horizontal dashed lines are drawn around
the median value to visualize the allowable range threshold.

This function is primarily intended for diagnostic visualization during
tuning of stability parameters.

## See also

[`TROLL_sensor_stable`](TROLL_sensor_stable.md),
[`is_stationary`](is_stationary.md)
