# Normalize Raw Column Names

Cleans raw TROLL column names by trimming whitespace and removing serial
numbers of the form `(1234567)`.

## Usage

``` r
normalize_raw_names(data)
```

## Arguments

- data:

  A data frame with raw column names.

## Value

A data frame with normalized column names.

## Details

This prepares column names for schema validation.
