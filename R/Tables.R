#' @import srvyr
#' @import gtsummary
#' @import gt
#' @import tidyverse
#' @importFrom purrr map
NULL

#' Create cross-tabulation summary tables
#'
#' This function creates a main summary table and multiple subtables based on the
#' specified variables, and combines them into a single formatted cross-tabulation.
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
#'   crosstab(svy, main_var = stype, sub_vars = c("both", "awards"))
#' }
crosstab <- function(data, main_var, sub_vars,
                     stat_option = "both",
                     custom_stat_cat = NULL,
                     custom_stat_cont = NULL,
                     digits = 0,
                     digits_2 = 0,
                     keep_footnotes = FALSE) {

  # Define subtable function inside crosstab but without default parameters
  subtable <- function(data, main, sub, stat_option, custom_stat_cat, custom_stat_cont, digits, digits_2) {
    if (stat_option == "single") {
      statistic_cat <- "{p}%"
      statistic_cont <- "{mean}"
    } else if (stat_option == "both") {
      statistic_cat <- "{p}% ({n})"
      statistic_cont <- "{mean} ({sd})"
    } else if (stat_option == "custom") {
      statistic_cat <- custom_stat_cat
      statistic_cont <- custom_stat_cont
    } else {
      stop("Invalid stat_option. Choose either 'single', 'both', or 'custom'.")
    }
    message("Creating subtable for ", sub)
    data %>%
      srvyr::select({{main}}, {{sub}}) %>%
      gtsummary::tbl_svysummary(
        by = {{sub}},
        statistic = list(gtsummary::all_categorical() ~ statistic_cat,
                         gtsummary::all_continuous() ~ statistic_cont),
        digits = list(dplyr::everything() ~ c(digits, digits_2))
      )
  }

  # Main function code
  if (stat_option == "single") {
    statistic_cat <- "{p}%"
    statistic_cont <- "{mean}"
  } else if (stat_option == "both") {
    statistic_cat <- "{p}% ({n})"
    statistic_cont <- "{mean} ({sd})"
  } else if (stat_option == "custom") {
    statistic_cat <- custom_stat_cat
    statistic_cont <- custom_stat_cont
  } else {
    stop("Invalid stat_option. Choose either 'single', 'both', or 'custom'.")
  }
  message("Creating main table")
  t0 <- data %>%
    srvyr::select({{main_var}}) %>%
    gtsummary::tbl_svysummary(
      statistic = list(gtsummary::all_categorical() ~ statistic_cat,
                       gtsummary::all_continuous() ~ statistic_cont),
      digits = list(dplyr::everything() ~ c(digits, digits_2))
    ) %>%
    gtsummary::modify_header(label ~ "") %>%
    gtsummary::bold_labels()
  message("Creating subtables")
  sub_tables <- purrr::map(sub_vars, ~{
    message("Processing subtable for ", .x)
    # Pass all parameters explicitly
    subtable(
      data = data,
      main = main_var,
      sub = .x,
      stat_option = stat_option,
      custom_stat_cat = custom_stat_cat,
      custom_stat_cont = custom_stat_cont,
      digits = digits,
      digits_2 = digits_2
    )
  })
  message("Merging tables")
  # MERGE
  tbls <- c(list(t0), sub_tables) %>%
    gtsummary::tbl_merge(tab_spanner = c("**Total**", paste0("**", sub_vars, "**"))) %>%
    gtsummary::as_gt()
  if (!keep_footnotes) {
    message("Removing footnotes")
    tbls <- tbls %>% gt::rm_footnotes()
  }
  message("Table creation complete")
  tbls
}
