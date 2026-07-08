## code to prepare `CES24_sample` dataset

CES24_sample <- read.csv("data-raw/CES24_sample.csv")

usethis::use_data(CES24_sample, overwrite = TRUE)
