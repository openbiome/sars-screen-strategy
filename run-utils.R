library(tidyverse)
source("model.R")

set.seed(1234)

named_list <- function(names, values) as.list(set_names(values, names))

# Read parameters -----------------------------------------------------

parameters <- read_tsv("parameters.tsv")

# check that estimate is contained between upper and lower estimates
parameters %>%
  filter(type != "fixed") %>%
  filter(estimate > upper | estimate < lower) %>%
  { if (nrow(.) > 0) {
    stop(str_glue("Bad parameter specification: estimates for {.$name} not contained in range"))
    }
   }

parameters %>%
  filter(!(type %in% c("fixed", "real", "integer"))) %>%
  { if (nrow(.) > 0) {
    stop(str_glue("Bad parameter specification: unknown types {.$type} for {.$name}"))
    }
  }
