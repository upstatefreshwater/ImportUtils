# Read Aqua-Troll Data File

Reads a raw TROLL data file and automatically locates the header row
containing \`"Date Time"\`. The function validates that the timestamp
column exists and ensures that timestamps include \*\*second-level
resolution\*\*. Supported timestamp formats are \`"ymd HMS"\` and \`"mdy
HMS"\`.

## Usage

``` r
TROLL_read_data(path)
```

## Arguments

- path:

  Character string. File path to the raw TROLL data file.

## Value

A tibble containing the imported data with the original column structure
from the raw file.

## Details

The function performs several validation checks:

- Confirms the file exists at the specified path.

- Searches the file for the row containing \`"Date Time"\` to identify
  the start of the data table.

- Ensures exactly one header row containing \`"Date Time"\` exists.

- Imports the data using
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
  starting from the detected header row.

- Verifies that timestamps in the \`"Date Time"\` column include
  seconds.

If timestamps contain only minute resolution (e.g., \`"ymd HM"\` or
\`"mdy HM"\`), the function will error.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- TROLL_read_data("data/raw/troll_sensor_data.csv")
} # }

```
