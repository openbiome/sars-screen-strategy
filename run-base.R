#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

base_par <- parameters %>%
  { named_list(.$name, .$estimate) }

tic <- Sys.time()

sims <- crossing(
  iter = 1:1e3,
  stool = c(FALSE, TRUE),
  serology = c(FALSE, TRUE)
) %>%
  mutate(
    par = pmap(
      list(stool, serology),
      ~ inset2s(base_par, c("use_stool", "use_serology"), c(..1, ..2))
    ),
    sim = map(par, model)
  ) %>%
  select(par, sim)

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-base.rds")
