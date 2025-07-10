
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

This is a basic example that shows how to use the crosstab function to create a table with multiple survey questions and demographic variables. 

``` r
library(SurveySuite)
library(tidyverse)
## basic example code

cols <- c("q1", "q2", "q3")
sub_vars <- c("rel", "partyid", "ideo5")

table <- survey %>% crosstab(cols, sub_vars)
table #For viewing table in view pane. 
gtsave(table, "surveytable.html") #For exporting table. 
```

