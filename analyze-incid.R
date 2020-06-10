#!/usr/bin/env Rscript

library(tidyverse)
source("analyze-utils.R")

sims <- read_rds("cache/sims-incid.rds")

tic <- Sys.time()

results <- sims %>%
  crossing(strategies) %>%
  mutate(released = map2(sim, test_suite, get_releases)) %>%
  select(iter, daily_inf_prob, strategy, released) %>%
  unnest_wider(released)

toc <- Sys.time()

print(str_glue("Analyzed in {toc - tic} sec"))

write_rds(results, "cache/analysis-incid.rds")
