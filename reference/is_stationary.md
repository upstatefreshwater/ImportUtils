# Identify Stationary Periods in Depth Data

This function flags periods where a sonde or depth sensor is effectively
stationary based on a rolling range of depth measurements. It can handle
irregular sampling intervals, optionally trims the start of stationary
blocks, and optionally plots depth and rolling range over time.

## Usage

``` r
is_stationary(
  df,
  depth_col = depth_m,
  datetime_col = DateTime,
  depth_range_threshold = 0.05,
  stationary_secs = 45,
  rolling_range_secs = 10,
  drop_cols = TRUE,
  plot = FALSE
)
```

## Arguments

- df:

  A data frame containing depth and datetime observations. Optionally, a
  path may be passed to the location of a raw TROLL CSV file, in which
  case \`TROLL_read_data()\` and \`TROLL_rename_cols()\` will be run
  with default setting to create \`df\`..

- depth_col:

  Column in \`df\` representing depth measurements (numeric). Default is
  \`depth_m\`.

- datetime_col:

  Column in \`df\` representing observation timestamps (POSIXt). Default
  is \`DateTime\`.

- depth_range_threshold:

  Numeric. Maximum depth range within a rolling window to consider the
  sonde stationary. Default is \`0.05\`.

- stationary_secs:

  Numeric. Minimum duration in seconds for a block to be considered
  stationary. Default is \`60\`.

- rolling_range_secs:

  Numeric. Window size in seconds used to calculate rolling depth range.
  Default is \`10\`.

- drop_cols:

  Logical. If TRUE, intermediate columns used for computation are
  removed from the output. Default is \`TRUE\`.

- plot:

  Logical. If TRUE, produces plots showing depth and rolling range with
  stationary periods highlighted. Default is \`FALSE\`.

## Value

A data frame identical to \`df\` but with additional columns (provided
\`drop_cols\` is `TRUE`:

- is_stationary_status:

  Numeric. 999 = fully stationary block, intermediate values = partial
  stationary duration in seconds, 0 = not stationary.

- stationary_depth:

  Mean depth during stationary blocks (NA if not stationary).

If \`drop_cols\` is `FALSE`, intermediate columns are retained in
addition to `is_stationary_status, stationary_depth`:

- is_stationary_initial:

  Logical. TRUE = met rolling range criteria after \`start_trim_secs\`
  was applied, prior to application of \`stationary_secs\`.

- stationary_block_id:

  Numeric. Consecutive id's of data blocks, including non-stationary
  blocks

- block_n:

  Numeric. The number of observations in each stationary block

- block_secs:

  Numeric. Elapsed time in seconds of each stationary block.

## Details

The function first calculates the sampling interval based on lagged
differences of the \`datetime_col\`. It then computes a rolling range of
depth values across a window size of length = \`rolling_range_secs\`. It
identifies candidate stationary periods where the rolling range is below
\`depth_range_threshold\`. Blocks shorter than \`stationary_secs\` are
flagged with \`is_stationary_status\` equal to seconds stationary.
Blocks longer than \`stationary_secs\` are flagged as 999. If the
sampling interval exceeds 30 seconds, the sonde is assumed to be fixed,
and all rows are returned with \`is_stationary_status = 999\`.

## See also

[`rollapply`](https://rdrr.io/pkg/zoo/man/rollapply.html)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
df <- data.frame(
DateTime = seq.POSIXt(from = as.POSIXct("2026-01-01 00:00"),
                      by = "sec", length.out = 170),
depth_m = c(rep(1.0, 50), rep(1.05, 70), rnorm(50,2,.02))
)
df_flagged <- is_stationary(df, depth_col = depth_m, datetime_col = DateTime,
                            start_trim_secs = 5, stationary_secs = 45,
                            depth_range_threshold = 0.05)
} # }
```
