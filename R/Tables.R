fn_subtable <- function(data, main, sub,
                        stat_option = "both",
                        custom_stat_cat = NULL,
                        custom_stat_cont = NULL,
                        digits = 0,
                        digits_2 = 0) {

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

  data %>%
    srvyr::select({{main}}, {{sub}}) %>%
    gtsummary::tbl_svysummary(
      by = {{sub}},
      statistic = list(gtsummary::all_categorical() ~ statistic_cat,
                       gtsummary::all_continuous() ~ statistic_cont),
      digits = list(dplyr::everything() ~ c(digits, digits_2))
    )
}

#' Create a Summary Table with Multiple Subtables
#'
#' This function generates a summary table using survey data, with optional subtables
#' for subgroup analysis.
#'
#' @param data A survey dataset.
#' @param main_var The main variable for summarization.
#' @param sub_vars A vector of subgroup variables.
#' @param stat_option The statistic display option: "single", "both", or "custom".
#' @param custom_stat_cat Custom statistic for categorical variables.
#' @param custom_stat_cont Custom statistic for continuous variables.
#' @param digits Number of decimal places for the first statistic.
#' @param digits_2 Number of decimal places for the second statistic.
#' @param keep_footnotes Whether to keep footnotes in the output.
#' @return A formatted summary table.
#' @export
fn_table <- function(data, main_var, sub_vars,
                     stat_option = "both",
                     custom_stat_cat = NULL,
                     custom_stat_cont = NULL,
                     digits = 0,
                     digits_2 = 0,
                     keep_footnotes = FALSE) {

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
    fn_subtable(data = data, main = main_var, sub = .x,
                stat_option = stat_option,
                custom_stat_cat = custom_stat_cat,
                custom_stat_cont = custom_stat_cont,
                digits = digits,
                digits_2 = digits_2)
  })

  message("Merging tables")

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
