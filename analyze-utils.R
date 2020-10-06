# Given a "test suite" (vector of names of test list objects), get the first
# (i.e., most conservative) last release day
get_last_day <- function(test_results, test_suite) {
  test_results %>%
    keep(names(.) %in% test_suite) %>%
    map_dbl(~ .$last_release_day) %>%
    min()
}

# Given the simulation results and the suite of tests used, find the last day
# on which material could be released. Then determine how much of the released
# material is virus-positive and -negative
get_releases <- function(sim, test_suite) {
  last_day <- get_last_day(sim$tests, test_suite)
  released_days <- keep(sim$donation_days, ~ . <= last_day)

  n_positive <- sum(sim$virus_in_stool[released_days])
  n_negative <- length(released_days) - n_positive

  list(
    n_positive = n_positive,
    n_negative = n_negative
  )
}

# Strategies and the corresponding testing suites
strategies <- tribble(
  ~strategy,                   ~test_suite,
  "Symptoms only",             c("symptoms"),
  "Stool only (28)",           c("symptoms", "stool28"),
  "Stool only (14)",           c("symptoms", "stool14"),
  "Stool only (every)",        c("symptoms", "every_stool"),
  "Swab only",                 c("symptoms", "swab"),
  "Swab, stool (28)",          c("symptoms", "swab", "stool28"),
  "Swab, stool (14)",          c("symptoms", "swab", "stool14"),
  "Swab, stool (every)",       c("symptoms", "swab", "every_stool"),
  "Swab, serology",            c("symptoms", "swab", "serology"),
  "Swab, ser., stool (every)", c("symptoms", "swab", "serology", "every_stool"),
) %>%
  mutate_at("strategy", fct_inorder)
