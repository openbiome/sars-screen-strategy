#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

base_par <- parameters %>%
  { named_list(.$name, .$estimate) }

tic <- Sys.time()

sims <- crossing(
  iter = 1:1e3,
  daily_inf_prob = c(1e-5, 1e-4, 1e-3)
) %>%
  mutate(
    par = map(daily_inf_prob, ~ assign_in(base_par, "daily_inf_prob", .)),
    sim = map(par, model)
  )

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-incid.rds")
