#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

base_par <- parameters %>%
  { named_list(.$name, .$estimate) }

tic <- Sys.time()

sims <- crossing(
  iter = 1:1e3,
  strategy_i = 1:length(strategies)
) %>%
  mutate(
    strategy_par = map(strategy_i, ~ strategies[[.]]),
    par = map(strategy_par, ~ c(base_par, .)),
    sim = map(par, model)
  ) %>%
  select(par, sim)

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-base.rds")
