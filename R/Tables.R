#' @import srvyr
#' @import gtsummary
#' @importFrom dplyr everything
#' @importFrom purrr map
#' @importFrom gt rm_footnotes
NULL

#' Create summary tables with subtables
#'
#' This function creates a main summary table and multiple subtables based on the
#' specified variables, and combines them into a single formatted table.
#'
#' @param data A data frame or survey design object
#' @param main_var The main variable to summarize
#' @param sub_vars A character vector of variable names to create subtables for
#' @param stat_option String specifying the statistics to display: "single", "both", or "custom"
#' @param custom_stat_cat Custom statistic format for categorical variables (when stat_option = "custom")
#' @param custom_stat_cont Custom statistic format for continuous variables (when stat_option = "custom")
#' @param digits Number of digits for primary statistics
#' @param digits_2 Number of digits for secondary statistics
#' @param keep_footnotes Logical; whether to keep footnotes in the final table
#'
#' @return A gt table object
#' @export
#'
#' @examples
#' \dontrun{
#'   library(survey)
#'   data(api)
#'   svy <- svydesign(id=~1, data=apiclus1)
#'   fn_table(svy, main_var = stype, sub_vars = c("both", "awards"))
#' }
fn_table <- function(data, main_var, sub_vars,
                     stat_option = "both",
                     custom_stat_cat = NULL,
                     custom_stat_cont = NULL,
                     digits = 0,
                     digits_2 = 0,
                     keep_footnotes = FALSE) {
  # Function code remains the same
}

#' Create a subtable for use within fn_table
#'
#' Internal function that creates a single subtable for a given variable.
#' This function is not meant to be called directly by users.
#'
#' @param data A data frame or survey design object
#' @param main The main variable to summarize
#' @param sub The variable to stratify by
#' @param stat_option String specifying the statistics to display
#' @param custom_stat_cat Custom statistic format for categorical variables
#' @param custom_stat_cont Custom statistic format for continuous variables
#' @param digits Number of digits for primary statistics
#' @param digits_2 Number of digits for secondary statistics
#'
#' @return A gtsummary table object
#' @keywords internal
fn_subtable <- function(data, main, sub,
                        stat_option = "both",
                        custom_stat_cat = NULL,
                        custom_stat_cont = NULL,
                        digits = 0,
                        digits_2 = 0) {
  # Function code remains the same
}
