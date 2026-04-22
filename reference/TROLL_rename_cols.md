# Standardize TROLL Data Column Names

Cleans, validates, and standardizes raw TROLL CSV column names into
names used throughout the package.

## Usage

``` r
TROLL_rename_cols(
  df,
  trollcomm_serials = trollCOMM_serials,
  colname_dictionary = troll_column_dictionary,
  strip_metadata = TRUE,
  verbose = FALSE
)
```

## Arguments

- df:

  A data frame read from a raw TROLL CSV export.

- trollcomm_serials:

  A numeric vector of known TROLL-COM serial numbers. Defaults to the
  internal `trollCOMM_serials` lookup.

- colname_dictionary:

  Dataframe; default = \`troll_column_dictionary\`. Provided regex
  patterns and associated canonical naming schema for standardizing
  column names.

- strip_metadata:

  Logical; if `TRUE`, removes metadata columns specified in the
  \`troll_column_dictionary\`.

- verbose:

  Logical; if `TRUE`, prints the final standardized column names to the
  console.

## Value

A data frame with validated and standardized column names.

## Details

The function performs the following steps:

1.  Detects and renames TROLL-COM temperature columns.

2.  Normalizes raw column names (removes serial numbers).

3.  Applies the internal column names dictionary for validation and
    canonical renaming.

The function will stop execution if:

- Unknown columns are detected,

- Multiple columns map to the same canonical name,

- Required columns are missing.

## See also

[`troll_column_dictionary`](troll_column_dictionary.md)
