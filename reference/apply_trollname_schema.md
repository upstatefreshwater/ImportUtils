# Apply TROLL Column Naming Schema

Matches raw column names against the internal `troll_column_dictionary`
using regular expression patterns, renames columns to their canonical
internal names, and enforces required column presence.

## Usage

``` r
apply_trollname_schema(
  data,
  dictionary = troll_column_dictionary,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame containing raw or partially normalized TROLL data.

- dictionary:

  A tibble containing schema definitions with columns `pattern`,
  `canonical`, and `required`. Defaults to `troll_column_dictionary`.

## Value

A data frame with validated and renamed canonical columns.

## Details

This function will:

- Stop if a column does not match any schema dictonary pattern.

- Stop if multiple columns map to the same canonical name.

- Stop if required columns are missing.
