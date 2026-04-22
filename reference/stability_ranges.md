# Default Range Values For Flagging Sensor Stability

A dataset containing the default threshold values used to determine
sensor stability within the
[`TROLL_sensor_stable()`](TROLL_sensor_stable.md) function. Stability is
typically defined as the maximum allowable range (max - min) over a
specific window of time.

## Usage

``` r
stability_ranges
```

## Format

\## \`stability_ranges\` A tibble with 8 rows and 2 columns:

- param:

  Character. The name of the water quality parameter (e.g., temperature,
  pH).

- range_thresh:

  Double. The maximum allowable difference between the maximum and
  minimum values in a window for the sensor to be considered "stable".

## Source

Internal calculation based on manufacturer specifications and field
testing.

## See also

[`TROLL_sensor_stable`](TROLL_sensor_stable.md)
