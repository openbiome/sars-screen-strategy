#!/usr/bin/env Rscript

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

base_par <- read_tsv("parameters.tsv") %>%
  { as.list(set_names(.$estimate, .$name)) }

base_par$swab_interval <- 14

tic <- Sys.time()

sims <- crossing(
  iter = 1:100,
  stool = c(FALSE, TRUE),
  serology = c(FALSE, TRUE)
) %>%
  mutate(
    battery = pmap(
      list(stool, serology),
      ~ inset2s(base_par, c("stool", "serology"), c(..1, ..2))
    ),
    par = map(battery, ~ inset2(base_par, "battery", .)),
    sim = map(par, model)
  ) %>%
    unnest_wider(sim)

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "sims.rds")
