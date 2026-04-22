# Plot HOBO Logger Data

Creates time series plots of absolute pressure and temperature from one
or more HOBO logger datasets. If multiple datasets are provided, they
are overlaid and colored by dataset name.

## Usage

``` r
plot_hobos(df)
```

## Arguments

- df:

  A data frame containing at least the following columns:

  datetime

  :   POSIXct timestamp

  abspress

  :   Absolute pressure values

  temp

  :   Temperature values

  dataset

  :   Dataset/site name

## Value

A ggplot object showing faceted time series of pressure and temperature.

## Examples

``` r
if (FALSE) { # \dontrun{
combined <- combine_hobos(Ischua_2, Ischua_3)
plot_hobos(combined)
} # }
```
