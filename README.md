
# Rsurveytools

<!-- badges: start -->
<!-- badges: end -->

The purpose of Rsurveytools is to add a crosstab function that can make a table for multiple survey and demographic questions at the same time. It comes from the helpful code in this [Stack Overflow thread](https://stackoverflow.com/questions/71632242/create-multiple-cross-tables-with-one-line-code-function-with-gtsummary) with additional functionality added.

## Installation

You can install Rsurveytools using the remotes package. 

``` r
remotes::install_github("hunterjohnson11/Rsurveytools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Rsurveytools)
library(tidyverse)
## basic example code

cols <- c("q1", "q2", "q3")
sub_vars <- c("rel", "partyid", "ideo5")

table <- survey %>% crosstab(cols, sub_vars)
table #For viewing table in view pane. 
gtsave(table, "surveytable.html") #For exporting table. 
```

