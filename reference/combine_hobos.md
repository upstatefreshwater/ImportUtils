# Combine HOBO Logger Datasets

Combines multiple processed HOBO datasets into a single data frame. Each
dataset must already be processed using \`formatischua()\` and include a
\`dataset\` column.

## Usage

``` r
combine_hobos(...)
```

## Arguments

- ...:

  Data frames to combine.

## Value

A single data frame containing all input datasets stacked together.

## Examples

``` r
if (FALSE) { # \dontrun{
combined <- combine_hobos(Ischua_2, Ischua_3)
} # }
```
