
# SurveySuite

<!-- badges: start -->
<!-- badges: end -->

The original purpose of SurveySuite was to add a crosstab function that can make a table for multiple survey and demographic questions at the same time. It comes from the helpful code in this [Stack Overflow thread](https://stackoverflow.com/questions/71632242/create-multiple-cross-tables-with-one-line-code-function-with-gtsummary) with additional functionality added.I plan to expand this package to include more functions related to survey research that I use in my work. 

## Installation

You can install SurveySuite using the remotes package. 

``` r
remotes::install_github("hunterjohnson11/SurveySuite")
```

## Example

This is a basic example that shows how to use `crosstab()` to create a table
with multiple survey questions and demographic variables. `crosstab()` returns
a gtsummary object; convert it with `gtsummary::as_gt()` if you want to export
it with `gt::gtsave()`.

``` r
library(SurveySuite)

main_vars <- c("CC24_301", "CC24_302", "CC24_303")
sub_vars <- c("gender4", "educ", "race")

# Unweighted
table <- crosstab(CES24_sample, main_vars, sub_vars)
table # For viewing the table in the view pane.

# Weighted -- pass a weight column name, no need to pre-build an srvyr object
weighted_table <- crosstab(CES24_sample, main_vars, sub_vars, weight = "commonweight")

gtsummary::as_gt(table) |> gt::gtsave("surveytable.html") # For exporting.
```

