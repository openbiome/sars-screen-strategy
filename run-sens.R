#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

# produce a list of parameters values given ranges
par_factory <- function(parameters) {
  fixed_par <- parameters %>%
    filter(type == "fixed") %>%
    { named_list(.$name, .$estimate) }

  real_par <- parameters %>%
    filter(type == "real")

  real_names <- real_par$name
  real_values_f <- with(real_par, {
    function() runif(length(estimate), lower, upper)
  })

  int_par <- parameters %>%
    filter(type == "integer")

  int_names <- int_par$name
  int_values_f <- with(int_par, {
    function() floor(runif(length(estimate), lower, upper + 1))
  })

  c(
    fixed_par,
    named_list(real_names, real_values_f()),
    named_list(int_names, int_values_f())
  )
}

tic <- Sys.time()

sims <- tibble(iter = 1:base_par$n_iter) %>%
  mutate(
    par = map(iter, ~ par_factory(parameters)),
    sim = map(par, model)
  )

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-sens.rds")
