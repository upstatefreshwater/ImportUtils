# Aqua TROLL Column Mapping and Metadata Dictionary

A data dictionary used to map raw In-Situ Aqua TROLL sensor output
headers to standardized (canonical) variable names. It also contains
metadata flags used by internal functions to identify core parameters,
instrument metadata, and variables eligible for stability calculations.

## Usage

``` r
troll_column_dictionary
```

## Format

\## \`troll_column_dictionary\` A tibble with \`r
nrow(troll_column_dictionary)\` rows and 6 columns:

- pattern:

  Character. Regex pattern used to match raw column headers from TROLL
  CSV/text exports.

- canonical:

  Character. The standardized snake_case name used within the package.

- required:

  Logical. If TRUE, these columns must be present for basic processing.

- meta:

  Logical. Indicates if the column represents instrument diagnostics
  (e.g., battery, voltage) rather than environmental data.

- core_param:

  Logical. Indicates if the column is a primary water quality parameter.

- stbl_calc:

  Logical. Indicates if the parameter is eligible for sensor stability
  flagging in [`TROLL_sensor_stable()`](TROLL_sensor_stable.md).

- derived_param:

  Logical. Indicates if the parameter is derived mathematically from a
  measured parameter.

- stability_source:

  Character. For derived parameters, the measured parameter used to flag
  stable observations.

## See also

\[TROLL_rename_cols()\]

## Examples

``` r
# View all core parameters
subset(troll_column_dictionary, core_param == TRUE)
#> # A tibble: 11 × 8
#>    pattern           canonical required meta  core_param stbl_calc derived_param
#>    <chr>             <chr>     <lgl>    <lgl> <lgl>      <lgl>     <lgl>        
#>  1 "^Specific Condu… sp_condu… FALSE    FALSE TRUE       TRUE      FALSE        
#>  2 "^Temperature \\… temperat… FALSE    FALSE TRUE       TRUE      FALSE        
#>  3 "^pH \\(pH\\)$"   pH_units  FALSE    FALSE TRUE       TRUE      FALSE        
#>  4 "^RDO Concentrat… DO_mgL    FALSE    FALSE TRUE       TRUE      FALSE        
#>  5 "^Turbidity"      turbidit… FALSE    FALSE TRUE       TRUE      FALSE        
#>  6 "^Chlorophyll-a … chloroph… FALSE    FALSE TRUE       TRUE      FALSE        
#>  7 "^BGA-PC Fluores… bga_fluo… FALSE    FALSE TRUE       TRUE      FALSE        
#>  8 "^ORP"            ORP_mv    FALSE    FALSE TRUE       TRUE      FALSE        
#>  9 "^RDO Saturation" DO_per    FALSE    FALSE TRUE       FALSE     TRUE         
#> 10 "^Chlorophyll-a … chloroph… FALSE    FALSE TRUE       FALSE     TRUE         
#> 11 "^BGA-PC Concent… bga_fluo… FALSE    FALSE TRUE       FALSE     TRUE         
#> # ℹ 1 more variable: stability_source <chr>
```
