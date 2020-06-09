#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

base_par <- parameters %>%
  { named_list(.$name, .$estimate) }

tic <- Sys.time()

sims <- tibble(iter = 1:1e3) %>%
  mutate(sim = map(iter, ~ model(base_par)))

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-base.rds")
