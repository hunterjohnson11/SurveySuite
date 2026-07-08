# Build one summary table (main table or a by-subtable), dispatching to the
# weighted (srvyr/tbl_svysummary) or unweighted (tbl_summary) path depending
# on whether `weight` is supplied.
#' @noRd
build_summary_table <- function(
  data,
  vars,
  by = NULL,
  weight = NULL,
  strata = NULL,
  ids = NULL,
  fpc = NULL,
  statistic,
  digits
) {
  select_vars <- c(vars, by)

  if (!is.null(weight)) {
    # `ids`/`strata`/`fpc` are tidyselect args on as_survey_design(); passing a
    # conditional expression (rather than a bare NULL or all_of() call) as the
    # argument confuses its NULL-detection and produces an unselected (empty)
    # design. Build the call explicitly so unused args are true literal NULLs.
    design_args <- list(
      .data = data,
      weights = rlang::expr(dplyr::all_of(!!weight)),
      strata = if (is.null(strata)) {
        NULL
      } else {
        rlang::expr(dplyr::all_of(!!strata))
      },
      ids = if (is.null(ids)) NULL else rlang::expr(dplyr::all_of(!!ids)),
      fpc = if (is.null(fpc)) NULL else rlang::expr(dplyr::all_of(!!fpc))
    )
    design <- rlang::eval_tidy(rlang::call2(
      srvyr::as_survey_design,
      !!!design_args
    ))

    design |>
      srvyr::select(dplyr::all_of(select_vars)) |>
      gtsummary::tbl_svysummary(
        by = if (is.null(by)) NULL else dplyr::all_of(by),
        statistic = statistic,
        digits = digits
      )
  } else {
    data |>
      dplyr::select(dplyr::all_of(select_vars)) |>
      gtsummary::tbl_summary(
        by = if (is.null(by)) NULL else dplyr::all_of(by),
        statistic = statistic,
        digits = digits
      )
  }
}

#' Create cross-tabulation summary tables
#'
#' Creates a main summary table and one subtable per `sub_vars` entry, and
#' combines them into a single formatted cross-tabulation. Works on a plain
#' data frame by default; supplying `weight` (and optionally `strata`/`ids`/
#' `fpc`) switches to a weighted survey design internally, so there is no need
#' to pre-build an `srvyr` object.
#'
#' Variables in `main_vars` whose names start with `"M_"` are treated as
#' multiselect groups (Y2 convention): variables sharing a prefix before a
#' trailing `multiselect_stem` + digits (e.g. `M_TOPIC_r1`, `M_TOPIC_r2`) are
#' collapsed into a single block, dropping "NO TO ..." and missing rows and
#' sorting the remaining levels by descending percentage.
#'
#' @param data A data frame.
#' @param main_vars A character vector of variable names to include in the main table.
#' @param sub_vars A character vector of variable names to create subtables for.
#' @param weight Optional. Name of a numeric weight column in `data`. When
#'   supplied, `crosstab()` builds a weighted survey design internally
#'   (via `srvyr::as_survey_design()`) and uses `gtsummary::tbl_svysummary()`.
#'   When `NULL` (the default), `data` is summarized directly with
#'   `gtsummary::tbl_summary()`.
#' @param strata Optional. Name of a stratification column, used only when `weight` is supplied.
#' @param ids Optional. Name of a cluster/PSU column, used only when `weight` is supplied.
#' @param fpc Optional. Name of a finite population correction column, used only when `weight` is supplied.
#' @param stat_option String specifying the statistics to display: "single", "both", or "custom". Default is "both". "single" shows percentage for categorical variables and mean for continuous variables. "both" shows percentage and n for categorical variables and mean and standard deviation for continuous variables. "custom" uses `custom_stat_cat`/`custom_stat_cont`.
#' @param custom_stat_cat Only used when `stat_option = "custom"`. Follows the "statistic" argument conventions in gtsummary's `tbl_summary()`/`tbl_svysummary()`.
#' @param custom_stat_cont Only used when `stat_option = "custom"`. Follows the "statistic" argument conventions in gtsummary's `tbl_summary()`/`tbl_svysummary()`.
#' @param main_digits Number of digits for primary statistics.
#' @param sub_digits Number of digits for secondary statistics.
#' @param multiselect_stem Trailing stem preceding the digits in multiselect variable names (e.g. `"r"` for `M_TOPIC_r1`).
#'
#' @return A gtsummary object.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(SurveySuite)
#'
#'   # Unweighted
#'   crosstab(CES24_sample, main_vars = c("CC24_301", "CC24_302"), sub_vars = c("gender4", "educ"))
#'
#'   # Weighted
#'   crosstab(
#'     CES24_sample,
#'     main_vars = c("CC24_301", "CC24_302"),
#'     sub_vars = c("gender4", "educ"),
#'     weight = "commonweight"
#'   )
#' }
crosstab <- function(
  data,
  main_vars,
  sub_vars,
  weight = NULL,
  strata = NULL,
  ids = NULL,
  fpc = NULL,
  stat_option = "both",
  custom_stat_cat = NULL,
  custom_stat_cont = NULL,
  main_digits = 0,
  sub_digits = 0,
  multiselect_stem = "r"
) {
  stats <- resolve_stat(stat_option, custom_stat_cat, custom_stat_cont)
  statistic <- list(
    gtsummary::all_categorical() ~ stats$cat,
    gtsummary::all_continuous() ~ stats$cont
  )
  digits <- list(dplyr::everything() ~ c(main_digits, sub_digits))

  message("Creating main table")
  t0 <- build_summary_table(
    data,
    main_vars,
    weight = weight,
    strata = strata,
    ids = ids,
    fpc = fpc,
    statistic = statistic,
    digits = digits
  ) |>
    gtsummary::modify_header(label ~ "") |>
    gtsummary::bold_labels()

  message("Creating subtables")
  sub_tables <- purrr::map(sub_vars, \(sub) {
    message("Creating subtable for ", sub)
    build_summary_table(
      data,
      main_vars,
      by = sub,
      weight = weight,
      strata = strata,
      ids = ids,
      fpc = fpc,
      statistic = statistic,
      digits = digits
    )
  })

  message("Merging tables")
  tbl <- c(list(t0), sub_tables) |>
    gtsummary::tbl_merge(
      tab_spanner = c("**Total**", paste0("**", sub_vars, "**"))
    ) |>
    gtsummary::modify_footnote(gtsummary::all_stat_cols() ~ NA)

  message("Collapsing multiselect groups")
  tbl <- collapse_multiselect(tbl, main_vars, stem = multiselect_stem)

  message("Table creation complete")
  tbl
}
