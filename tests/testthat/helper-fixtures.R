# Small synthetic dataset covering: a weight column, a categorical sub_var,
# a continuous variable, and a multiselect group (M_TOPIC_r1..r3) with
# "Selected" / "NO TO ..." factor levels matching the Y2 convention.
make_crosstab_data <- function(n = 40) {
  group <- rep(c("A", "B"), length.out = n)
  data.frame(
    weight = seq(0.5, 1.5, length.out = n),
    group = group,
    age = rep(c(25, 35, 45, 55), length.out = n),
    M_TOPIC_r1 = factor(rep_len(c("Selected", "Selected", "NO TO Topic 1"), n)),
    M_TOPIC_r2 = factor(rep_len(
      c("NO TO Topic 2", "Selected", "NO TO Topic 2"),
      n
    )),
    M_TOPIC_r3 = factor(rep_len(
      c("Selected", "NO TO Topic 3", "NO TO Topic 3"),
      n
    )),
    stringsAsFactors = FALSE
  )
}

# Hand-built table_body tibble matching the columns collapse_multiselect_body()
# operates on, for direct unit tests without a real gtsummary object.
make_table_body <- function(rows) {
  tibble::tibble(
    variable = vapply(rows, `[[`, character(1), "variable"),
    row_type = vapply(rows, `[[`, character(1), "row_type"),
    label = vapply(rows, `[[`, character(1), "label"),
    stat_0_1 = vapply(rows, `[[`, character(1), "stat_0_1")
  )
}
