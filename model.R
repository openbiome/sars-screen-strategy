library(tidyverse)

# Utility functions ---------------------------------------------------

# "Run tests" function
# Inputs:
# - true_condition :: logical vector with length equal to number of
#   test days
# - sens :: sensitivity of test
# - spec :: specificity of test
#
# Returns: a list with
# - test_days (int vector) :: days on which the tests occurred
# - results (bool vector) :: test results on test days
# - last_release_day (int) :: last day of released material
run_tests <- function(true_condition, sens, spec, interval, max_days) {
  test_days <- seq(1, max_days, by = interval)
  # probability of positive result on each test day
  probability_positive <- if_else(true_condition[test_days], sens, 1 - spec)
  results <- rbernoulli(length(probability_positive), probability_positive)

  # look for index of first positive test
  i <- detect_index(results, ~ .)

  if (i == 0) {
    # all tests negative; release everything up to last test
    last_release_day <- max(test_days)
  } else if (i == 1) {
    # the first test was positive; release nothing
    last_release_day <- 0
  } else {
    # release everything up to last *negative* test
    last_release_day <- test_days[i - 1]
  }

  list(
    test_days = test_days,
    results = results,
    last_release_day = last_release_day
  )

}

# Do x[start:end] <- value, but being smart about what happens if
# start or end are out of bounds
safe_assign <- function(x, start, end, value) {
  if (start < 0 || end < 0) {
    stop(str_glue("Bad start={start} or end={end} value"))
  }
  if (start > length(x) || end < start) return(x)
  if (end > length(x)) end <- length(x)
  x[start:end] <- value
  x
}


# Main model function -------------------------------------------------
# Simulate the donor's disease course and the results of *all* tests. The
# matter of which tests actually get implemented and how many donations are
# released is determined in the analysis scripts.
#
# Returns: a list with
# - donation_days (int vector) :: days on which donor donated
# - virus_in_stool (bool vector) :: one entry per day
# - tests (list of lists) :: see run_tests function

model <- function(par) {
  with(par, {
    # Initialize a vector of status (i.e., disease state); each entry is a day
    status <- rep(NA, max_days)

    # Determine days of status changes
    # (add 1 to rnbinom for 1-indexing)
    i1_day <- 1 + rnbinom(1, 1, incidence)
    i2_day <- i1_day + i1_duration
    r1_day <- i2_day + i2_duration
    r2_day <- r1_day + r1_duration

    # Assign those status changes in the status vector
    status <- status %>%
      safe_assign(1,      i1_day - 1, "u")  %>%
      safe_assign(i1_day, i2_day - 1, "i1") %>%
      safe_assign(i2_day, r1_day - 1, "i2") %>%
      safe_assign(r1_day, r2_day - 1, "r1") %>%
      safe_assign(r2_day, max_days,   "r2")

    if (any(is.na(status))) stop("Missing status")
    if (length(status) != max_days) stop("Bad status length")

    # Determine if the donor becomes symptomatic. Being symptomatic requires
    # that this donor is "capable" of symptoms and that they become infected
    # during the simulation period
    possibly_symptomatic <- rbernoulli(1, 1 - asymp_prob)
    is_symptomatic <- possibly_symptomatic && i1_day <= max_days

    # Build a test-like results list for symptoms:
    # - The "test" is the first day of infection, or the last day of the simulation
    # - The "result" is positive if infected during the simulation and has
    #   possibility for symptoms
    # - Last release day is: if not symptomatic, end of simulation; if
    #   symptomatic, 14 days before symptoms emerged
    symptoms <- list(
      test_days = min(i1_day, max_days),
      results = is_symptomatic,
      last_release_day = if_else(is_symptomatic, max(i1_day - 14, 0), max_days)
    )

    # Determine presence of virus in stool:
    #  1. Is the donor a shedder? y/n
    #  2. If yes, then there is virus in stool for every day of I2 and R1
    #  3. Simulate the results of a (hypothetical) daily stool test
    is_shedder <- rbernoulli(1, shed_prob)
    virus_in_stool <- is_shedder & status %in% c("i2", "r1")
    daily_stool_test_results <- run_tests(virus_in_stool, stool_sens, stool_spec, 1, max_days)$results

    # Run tests
    tests <- list(
      symptoms    = symptoms,
      serology    = run_tests(status %in% c("r1", "r2"), serology_sens, serology_spec, serology_interval, max_days),
      swab        = run_tests(status %in% c("i1", "i2"), swab_sens,     swab_spec,     swab_interval,     max_days),
      stool14     = run_tests(virus_in_stool,            1.0,           1.0,           14,                max_days),
      stool28     = run_tests(daily_stool_test_results,  1.0,           1.0,           28,                max_days),
      every_stool = run_tests(daily_stool_test_results,  1.0,           1.0,           donation_interval, max_days)
    )

    # Get the first day that at least one of the tests had as its last day
    donation_days <- seq(1, max_days, by = donation_interval)

    list(
      status = status,
      donation_days = donation_days,
      virus_in_stool = virus_in_stool,
      daily_stool_test_results = daily_stool_test_results,
      tests = tests
    )
  })
}
