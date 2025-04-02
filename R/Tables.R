#' @import srvyr
#' @import gtsummary
#' @import gt
#' @import tidyverse
#' @importFrom purrr map
NULL

#' Create cross-tabulation summary tables for srvyr objects
#'
#' This function creates a main summary table and multiple subtables based on the
#' specified variables, and combines them into a single formatted cross-tabulation.
#' Designed specifically for survey design objects. Useful for weighted crosstabs.
#'
#' @param data A survey design object
#' @param main_vars A character vector of variable names to include in the main table.
#' @param sub_vars A character vector of variable names to create subtables for
#' @param stat_option String specifying the statistics to display: "single", "both", or "custom". Default is both. Single will only show percentage for categorical variables and mean for continuous variables. Both will show percentage and n size for categorical variables and mean and standard deviation for continuous variables (with the latter variable in parentheses). Custom allows you to specify custom configurations as specified in the custom_stat_cat and custom_stat_cont parameters.
#' @param custom_stat_cat Only used when stat_option = "custom". The options follow those available under the "statistic" argument in gtsummary's tbl_svysummary.
#' @param custom_stat_cont Only used when stat_option = "custom". The options follow those available under the "statistic" argument in gtsummary's tbl_svysummary.
#' @param main_digits Number of digits for primary statistics
#' @param sub_digits Number of digits for secondary statistics
#' @param keep_footnotes Logical; whether to keep footnotes in the final table
#'
#' @return A gt table object
#' @export
#'
#' @examples
#' \dontrun{
#'   library(Rsurveytools)
#'   data <- data
#'   svy <- svydesign(data = data, id=~1, weight = wts)
#'   srvy_crosstab(svy, main_vars = c("q1", "q2", "q3"), sub_vars = c("educ", "ideo5"))
#' }
srvy_crosstab <- function(data, main_vars, sub_vars,
                          stat_option = "both",
                          custom_stat_cat = NULL,
                          custom_stat_cont = NULL,
                          main_digits = 0,
                          sub_digits = 0,
                          keep_footnotes = FALSE) {

  # Define subtable function inside crosstab but without default parameters
  subtable <- function(data, main, sub, stat_option, custom_stat_cat, custom_stat_cont, main_digits, sub_digits) {
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
        digits = list(dplyr::everything() ~ c(main_digits, sub_digits))
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
    srvyr::select({{main_vars}}) %>%
    gtsummary::tbl_svysummary(
      statistic = list(gtsummary::all_categorical() ~ statistic_cat,
                       gtsummary::all_continuous() ~ statistic_cont),
      digits = list(dplyr::everything() ~ c(main_digits, sub_digits))
    ) %>%
    gtsummary::modify_header(label ~ "") %>%
    gtsummary::bold_labels()
  message("Creating subtables")
  sub_tables <- purrr::map(sub_vars, ~{
    message("Processing subtable for ", .x)
    # Pass all parameters explicitly
    subtable(
      data = data,
      main = main_vars,
      sub = .x,
      stat_option = stat_option,
      custom_stat_cat = custom_stat_cat,
      custom_stat_cont = custom_stat_cont,
      main_digits = main_digits,
      sub_digits = sub_digits
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

#' Create cross-tabulation summary tables for data frames
#'
#' This function creates a main summary table and multiple subtables based on the
#' specified variables, and combines them into a single formatted cross-tabulation.
#' Designed for standard data frames.
#'
#' @param data A data frame object
#' @param main_vars A character vector of variable names to include in the main table.
#' @param sub_vars A character vector of variable names to create subtables for
#' @param stat_option String specifying the statistics to display: "single", "both", or "custom". Default is both. Single will only show percentage for categorical variables and mean for continuous variables. Both will show percentage and n size for categorical variables and mean and standard deviation for continuous variables (with the latter variable in parentheses). Custom allows you to specify custom configurations as specified in the custom_stat_cat and custom_stat_cont parameters.
#' @param custom_stat_cat Only used when stat_option = "custom". The options follow those available under the "statistic" argument in gtsummary's tbl_summary.
#' @param custom_stat_cont Only used when stat_option = "custom". The options follow those available under the "statistic" argument in gtsummary's tbl_summary.
#' @param main_digits Number of digits for primary statistics
#' @param sub_digits Number of digits for secondary statistics
#' @param keep_footnotes Logical; whether to keep footnotes in the final table
#'
#' @return A gt table object
#' @export
#'
#' @examples
#' \dontrun{
#'   library(Rsurveytools)
#'   data <- data
#'   crosstab(data, main_vars = c("q1", "q2", "q3"), sub_vars = c("educ", "ideo5"))
#' }
crosstab <- function(data, main_vars, sub_vars,
                     stat_option = "both",
                     custom_stat_cat = NULL,
                     custom_stat_cont = NULL,
                     main_digits = 0,
                     sub_digits = 0,
                     keep_footnotes = FALSE) {

  # Define subtable function inside crosstab but without default parameters
  subtable <- function(data, main, sub, stat_option, custom_stat_cat, custom_stat_cont, main_digits, sub_digits) {
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
      dplyr::select({{main}}, {{sub}}) %>%
      gtsummary::tbl_summary(
        by = {{sub}},
        statistic = list(gtsummary::all_categorical() ~ statistic_cat,
                         gtsummary::all_continuous() ~ statistic_cont),
        digits = list(dplyr::everything() ~ c(main_digits, sub_digits))
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
    dplyr::select({{main_vars}}) %>%
    gtsummary::tbl_summary(
      statistic = list(gtsummary::all_categorical() ~ statistic_cat,
                       gtsummary::all_continuous() ~ statistic_cont),
      digits = list(dplyr::everything() ~ c(main_digits, sub_digits))
    ) %>%
    gtsummary::modify_header(label ~ "") %>%
    gtsummary::bold_labels()
  message("Creating subtables")
  sub_tables <- purrr::map(sub_vars, ~{
    message("Processing subtable for ", .x)
    # Pass all parameters explicitly
    subtable(
      data = data,
      main = main_vars,
      sub = .x,
      stat_option = stat_option,
      custom_stat_cat = custom_stat_cat,
      custom_stat_cont = custom_stat_cont,
      main_digits = main_digits,
      sub_digits = sub_digits
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
