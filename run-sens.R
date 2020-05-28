#!/usr/bin/env Rscript

library(tidyverse)
source("run-utils.R")

# make a function (par_factory_factory) that produces a function
# (par_factory) that produces lists of parameters
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

sims <- crossing(
  iter = 1:1e3,
  stool = c(FALSE, TRUE),
  serology = c(FALSE, TRUE)
) %>%
  mutate(
    base_par = map(iter, ~ par_factory(parameters)),
    par = pmap(
      list(base_par, stool, serology),
      ~ inset2s(..1, c("use_stool", "use_serology"), c(..2, ..3))
    ),
    sim = map(par, model)
  ) %>%
  select(par, sim)

toc <- Sys.time()

print(str_glue("Ran {nrow(sims)} sims in {toc - tic} sec"))

write_rds(sims, "cache/sims-sens.rds")
