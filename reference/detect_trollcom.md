# Detect and Rename TROLL-COM Temperature Column

Identifies TROLL-COM serial numbers in raw column names and, when a
single matching serial is detected, renames the associated internal
temperature column to `Trollcom_temperature_C` to avoid collision with
water temperature sensors.

## Usage

``` r
detect_trollcom(data, trollCOMM_serials, verbose = FALSE)
```

## Arguments

- data:

  A data frame containing raw TROLL column names.

- trollCOMM_serials:

  A numeric vector of known TROLL-COM serial numbers used for detection.

## Value

A data frame with TROLL-COM temperature renamed if applicable.

## Details

If multiple TROLL-COM serial numbers are detected, a warning is issued
and no automatic renaming is performed.
