#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

tic <- Sys.time()

sims <- tibble(iter = 1:base_par$n_iter) %>%
  mutate(sim = map(iter, ~ model(base_par)))

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-base.rds")
