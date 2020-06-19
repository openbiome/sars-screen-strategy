get_last_day <- function(test_results, test_suite) {
  test_results %>%
    keep(names(.) %in% test_suite) %>%
    map_dbl(~ .$last_release_day) %>%
    min()
}

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

strategies <- tribble(
  ~strategy, ~test_suite, ~color,
  "Stool only (28)", c("symptoms", "stool28"), "#984ea3", 
  "Stool only (14)", c("symptoms", "stool14"), "#BA89C2",
  "Stool only (every)", c("symptoms", "every_stool"), "#DDC4E0", 
  "Swab only", c("symptoms", "swab"), "#e41a1c",
  "Swab, stool (28)", c("symptoms", "swab", "stool28"), "#377eb8",
  "Swab, stool (14)", c("symptoms", "swab", "stool14"), "#7AA9D0",
  "Swab, stool (every)", c("symptoms", "swab", "every_stool"), "#BCD4E7",
  "Swab, serology", c("symptoms", "swab", "serology"), "#4daf4a",
  "Swab, ser., stool (every)", c("symptoms", "swab", "serology", "every_stool"), "#A6D7A5"
) %>%
  mutate_at("strategy", fct_inorder)
