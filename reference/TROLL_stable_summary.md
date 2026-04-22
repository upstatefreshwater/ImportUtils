# Summarize stable TROLL observations

Summarizes sensor values using only observations flagged as stable
(i.e., where corresponding \`\<sensor\>\_stable\` columns are \`TRUE\`).

## Usage

``` r
TROLL_stable_summary(
  df,
  group_col = stationary_depth,
  summary_fn = median,
  digits = 4
)
```

## Arguments

- df:

  A data frame containing sensor observations and corresponding
  \`\_stable\` flag columns.

- group_col:

  Column used to group observations (e.g., \`stationary_depth\`).

- summary_fn:

  Summary function applied to stable values. Default is \`median\`.

- digits:

  Numeric. Number of decimal places to round result of \`summary_fn\`.

## Value

A data frame with one row per group and summarized sensor values for
each parameter.

## Details

Intended to create final values from the `TROLL_profile_compiler`
function.

The function automatically detects data columns paired with \`\_stable\`
flag columns and computes a summary statistic for each parameter within
the specified grouping variable.
