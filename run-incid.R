#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

incidences <- parameters %>%
  filter(name == "incidence") %>%
  with({ c(lower, estimate, upper) })

tic <- Sys.time()

sims <- crossing(
  iter = 1:base_par$n_iter,
  incidence = incidences
) %>%
  mutate(
    par = map(incidence, ~ assign_in(base_par, "incidence", .)),
    sim = map(par, model)
  )

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-incid.rds")
