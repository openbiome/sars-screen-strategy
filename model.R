library(tidyverse)

run_tests <- function(true_condition, sens, spec) {
  p <- if_else(true_condition, sens, 1 - spec)
  result <- as.logical(rbinom(length(true_condition), 1, p))
  result
}

enforce_tests <- function(days, results) {
  # look for first positive test
  i <- detect_index(results, ~ .)

  if (i == 0) {
    # all tests negative
    deferred <- FALSE
    n_tests <- length(results)
    end_day <- max(days)
  } else if (i == 1) {
    # the first test was positive
    deferred <- TRUE
    n_tests <- 1
    end_day <- 0
  } else {
    deferred <- TRUE
    n_tests <- i
    end_day <- days[i - 1]
  }

  list(
    deferred = deferred,
    n_tests = n_tests,
    end_day = end_day
  )
}

safe_assign <- function(x, start, end, value) {
  if (start < 0 || end < 0) {
    stop(str_glue("Bad start={start} or end={end} value"))
  }
  if (start > length(x) || end < start) return(x)
  if (end > length(x)) end <- length(x)
  x[start:end] <- value
  x
}

model <- function(par) {
  with(par, {
    # Initialize a vector of status; each entry is a day
    # rows = donors/iterations; columns = days; cells = status on that day
    status <- rep(NA, max_time)

    # Determine days of status changes
    # (add 1 to rnbinom for 1-indexing)
    i1_day <- 1 + rnbinom(1, 1, daily_inf_prob)
    i2_day <- i1_day + i1_duration
    r1_day <- i2_day + i2_duration
    r2_day <- r1_day + r1_duration

    # Assign those status changes in the status matrix
    status <- safe_assign(status, 1, i1_day - 1, "u")
    status <- safe_assign(status, i1_day, i2_day - 1, "i1")
    status <- safe_assign(status, i2_day, r1_day - 1, "i2")
    status <- safe_assign(status, r1_day, r2_day - 1, "r1")
    status <- safe_assign(status, r2_day, max_time, "r2")
    if (any(is.na(status))) stop("Missing status")

    # Assign dates of screens, swabs, and donations
    screen_days   <- seq(1, max_time, by = screen_interval)
    swab_days     <- seq(1, max_time, by = swab_interval)
    donation_days <- seq(1, max_time, by = donation_interval)

    # Simulate virus presence in stool
    virus_in_stool <- run_tests(
      (status %in% c("i2", "r1"))[donation_days],
      shed_prob, 1.0
    )

    # Simulate results of tests
    serology_results <- run_tests(
      (status %in% c("r1", "r2"))[screen_days],
      serology_sens, serology_spec
    )
    swab_results <- run_tests(
      (status %in% c("i1", "i2"))[swab_days],
      swab_sens, swab_spec
    )
    stool_results <- run_tests(
      virus_in_stool,
      stool_sens, stool_spec
    )

    # Determine outcomes of donations based on tests
    actions <- list()
    if (use_serology) actions <- c(actions, list(enforce_tests(screen_days, serology_results)))
    if (use_swab) actions <- c(actions, list(enforce_tests(swab_days, swab_results)))
    if (use_stool) actions <- c(actions, list(enforce_tests(donation_days, stool_results)))

    deferred <- any(map_lgl(actions, ~ .$deferred))
    end_day <- min(map_dbl(actions, ~ .$end_day))

    n_released <- sum(donation_days < end_day)
    n_positive_released <- sum(virus_in_stool[donation_days < end_day])
    n_negative_released <- n_released - n_positive_released

    list(
      n_positive_released = n_positive_released,
      n_negative_released = n_negative_released,
      deferred = deferred,
      end_day = end_day
    )
  })
}
