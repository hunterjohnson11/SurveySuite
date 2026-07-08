# Internal helpers for crosstab() -------------------------------------------

#' @importFrom rlang .data
#' @noRd
resolve_stat <- function(stat_option, custom_stat_cat, custom_stat_cont) {
  switch(
    stat_option,
    single = list(cat = "{p}%", cont = "{mean}"),
    both = list(cat = "{p}% ({n})", cont = "{mean} ({sd})"),
    custom = list(cat = custom_stat_cat, cont = custom_stat_cont),
    stop("Invalid stat_option. Choose 'single', 'both', or 'custom'.")
  )
}

#' Detect multiselect groups from a vector of variable names.
#'
#' Variables starting with "M_" are grouped by shared prefix -- everything
#' before the last occurrence of `stem` + digits.
#'
#' @return A named list: prefix -> character vector of variable names.
#' @noRd
detect_multiselect_groups <- function(vars, stem = "r") {
  m_vars <- vars[startsWith(vars, "M_")]
  if (length(m_vars) == 0) {
    return(list())
  }

  prefixes <- sub(paste0(stem, "\\d+$"), "", m_vars)
  split(m_vars, prefixes)
}

#' Collapse multiselect groups in a tbl_merge/tbl_summary table body.
#'
#' Per group: keeps one label row (first), removes "NO TO" and missing rows,
#' sorts levels descending by %. Non-multiselect rows keep their original order.
#'
#' @param body A gtsummary `table_body` tibble.
#' @noRd
collapse_multiselect_body <- function(body, vars, stem = "r") {
  groups <- detect_multiselect_groups(vars, stem)
  if (length(groups) == 0) {
    return(body)
  }

  m_vars <- unlist(groups, use.names = FALSE)

  non_m <- body |> dplyr::filter(!.data$variable %in% m_vars)

  m_rows <- body |>
    dplyr::filter(.data$variable %in% m_vars) |>
    dplyr::filter(
      .data$row_type != "missing",
      !stringr::str_detect(.data$label, "^NO TO")
    )

  collapsed <- purrr::map(groups, \(group_vars) {
    grp <- m_rows |> dplyr::filter(.data$variable %in% group_vars)

    label_row <- grp |>
      dplyr::filter(.data$row_type == "label") |>
      dplyr::slice(1)
    level_rows <- grp |>
      dplyr::filter(.data$row_type == "level") |>
      dplyr::arrange(dplyr::desc(as.numeric(stringr::str_extract(
        .data$stat_0_1,
        "^[0-9]+"
      ))))

    dplyr::bind_rows(label_row, level_rows)
  }) |>
    dplyr::bind_rows()

  # Re-insert collapsed groups at the position of the first M_ variable,
  # preserving the order of non-M_ variables around them
  first_m_pos <- min(which(body$variable %in% m_vars))
  n_non_m <- nrow(non_m)

  before <- if (first_m_pos > 1) {
    dplyr::slice(non_m, 1:(first_m_pos - 1))
  } else {
    non_m[0, ]
  }
  after <- if (first_m_pos <= n_non_m) {
    dplyr::slice(non_m, first_m_pos:n_non_m)
  } else {
    non_m[0, ]
  }

  dplyr::bind_rows(before, collapsed, after)
}

#' @noRd
collapse_multiselect <- function(tbl, vars, stem = "r") {
  groups <- detect_multiselect_groups(vars, stem)
  if (length(groups) == 0) {
    return(tbl)
  }

  gtsummary::modify_table_body(
    tbl,
    ~ collapse_multiselect_body(.x, vars, stem)
  )
}
