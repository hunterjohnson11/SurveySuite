# SurveySuite 0.1.0

-   Initial release of SurveySuite (initally called Rsurveytools)

# SurveySuite 0.2.0

## New features

-   Renamed original `crosstab()` function to `srvy_crosstab()` for survey objects
-   Added new `crosstab()` function for standard data frames

# SurveySuite 0.2.1

-   Renamed package and moved to pkgdown

# SurveySuite 0.3.0

## Breaking changes

-   `srvy_crosstab()` has been removed. `crosstab()` now handles both weighted
    and unweighted data: pass a `weight` column name (plus optional `strata`,
    `ids`, `fpc`) to build a weighted survey design internally, or omit it for
    a plain unweighted summary. There is no need to pre-build an `srvyr` design
    object anymore.
-   `crosstab()` now returns a gtsummary object (via `modify_footnote()`)
    instead of a `gt` object (previously produced via `as_gt()` +
    `gt::rm_footnotes()`). Convert with `gtsummary::as_gt()` if you need a
    `gt` object, e.g. for `gt::gtsave()`.
-   The `keep_footnotes` argument has been removed.

## New features

-   `crosstab()` now collapses Y2-style multiselect variable groups (columns
    prefixed `M_` sharing a stem, e.g. `M_TOPIC_r1`/`M_TOPIC_r2`): a single
    label row is kept, "NO TO ..." and missing rows are dropped, and levels
    are sorted by descending percentage. Controlled via the new
    `multiselect_stem` argument.
-   Added a bundled sample dataset, `CES24_sample`, for examples and tests.

## Internal

-   Rewrote the package internals using the base R pipe (`|>`) and fully
    namespaced calls; dropped the `@import tidyverse`/`@import gt` blanket
    imports in favor of explicit `Imports:` (`dplyr`, `gtsummary`, `purrr`,
    `srvyr`, `stringr`).
-   Added a full testthat 3 test suite.
