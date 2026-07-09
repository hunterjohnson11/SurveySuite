library(SurveySuite)
library(tidyverse)

df <- read.csv("data-raw/CES24_sample.csv")

df <- df |>
  mutate(
    CC24_301 = factor(
      CC24_301,
      levels = c(
        "Gotten much better",
        "Gotten somewhat better",
        "Stayed about the same",
        "Gotten somewhat worse",
        "Gotten much worse",
        "Not sure"
      )
    )
  )


df |>
  crosstab(
    main_vars = c("CC24_301", "CC24_302"),
    sub_vars = c("gender4", "educ"),
    weight = "commonweight"
  )
