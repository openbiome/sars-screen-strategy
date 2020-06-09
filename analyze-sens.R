#!/usr/bin/env Rscript

library(tidyverse)
source("analyze-utils.R")

sims <- read_rds("cache/sims-sens.rds")

results <- sims %>%
  crossing(strategies) %>%
  mutate(released = map2(sim, test_suite, get_releases)) %>%
  select(iter, par, strategy, released) %>%
  unnest_wider(released)

write_rds(results, "cache/analysis-sens.rds")
