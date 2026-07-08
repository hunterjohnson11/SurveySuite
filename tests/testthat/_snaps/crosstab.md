# crosstab() errors on an invalid stat_option

    Code
      suppressMessages(crosstab(data, c("age"), c("group"), stat_option = "nope"))
    Condition
      Error in `resolve_stat()`:
      ! Invalid stat_option. Choose 'single', 'both', or 'custom'.

