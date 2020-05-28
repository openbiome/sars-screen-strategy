#!/usr/bin/env Rscript --vanilla

library(tidyverse)

par <- read_tsv("parameters.tsv") %>%
  { as.list(set_names(.$estimate, .$name)) }

par$swab_interval <- 14

set_status <- function(x, status, starts) {
  stopifnot(dim(status)[1] == length(starts))
  max_j <- dim(status)[2]

  for (i in 1:length(starts)) {
    j <- starts[i]
    if (j <= max_j) {
      x[i, j:max_j] <- status
    }
  }

  x
}

model <- function(par) {
  with(par, {
    # Initialize a matrix of status
    # rows = donors/iterations; columns = days; cells = status on that day
    status <- matrix("u", nrow = n_iter, ncol = max_time)

    # Determine days of status changes
    i1_days <- rnbinom(n_iter, 1, daily_inf_prob)
    i2_days <- i1_days + i1_duration
    r1_days <- i2_days + i2_duration
    r2_days <- r1_days + r1_duration

    # Assign those status changes in the status matrix
    status <- set_status(status, "i1", i1_days)
    status <- set_status(status, "i2", i2_days)
    status <- set_status(status, "r1", r1_days)
    status <- set_status(status, "r2", r2_days)

    # Assign dates of screens, swabs, and donations
    screen_days <- rep(1, max_time, by = screen_interval)
    swab_days <- rep(1, max_time, by = swab_interval)
    donation_days <- rep(1, max_time, by = donation_interval)

    # Simulate results of tests
    swab_results <- 

    # Determine outcomes of donations based on tests

  })
}

model(par)
