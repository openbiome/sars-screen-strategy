#!/usr/bin/env Rscript

library(tidyverse)
source("analyze-utils.R")

sims <- read_rds("cache/sims-sens.rds")

tic <- Sys.time()

results <- sims %>%
  crossing(strategies) %>%
  mutate(released = map2(sim, test_suite, get_releases)) %>%
  select(iter, par, strategy, released) %>%
  unnest_wider(released)

toc <- Sys.time()

print(str_glue("Analyzed {nrow(sims)} sims in {toc - tic} sec"))

write_rds(results, "cache/analysis-sens.rds")
