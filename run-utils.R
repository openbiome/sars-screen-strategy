library(tidyverse)
source("model.R")

set.seed(1234)

inset2 <- function(lst, name, elt) {
  lst[[name]] <- elt
  lst
}

inset2s <- function(lst, names, elts) {
  stopifnot(length(names) == length(elts))
  for (i in 1:length(names)) lst <- inset2(lst, names[i], elts[i])
  lst
}

named_list <- function(names, values) as.list(set_names(values, names))

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
