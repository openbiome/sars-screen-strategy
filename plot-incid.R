#!/usr/bin/env Rscript

library(tidyverse)
library(broom) # for tidy
library(scales) # for math_format
source("plot-utils.R")

results <- read_rds("cache/analysis-incid.rds")

counts <- results %>%
  group_by(incidence, strategy) %>%
  summarize_at(c("n_positive", "n_negative"), sum) %>%
  ungroup() %>%
  arrange(incidence, strategy) %>%
  mutate(
    n_total = n_positive + n_negative,
    binom_test = map2(n_positive, n_total, binom.test),
    data = map(binom_test, tidy)
  ) %>%
  select(incidence, strategy, n_positive, n_negative, n_total, data) %>%
  unnest(data)

# Write tables of proportions -----------------------------------------

# Helper functions

lsignif <- function(x, digits) lapply(x, function(xx) signif(xx, digits))

any_duplicated_nonzero <- function(x) {
  as.numeric(x) %>%
    { duplicated(.)[. != 0] } %>%
    any()
}

min_signif <- function(x) {
  if (any_duplicated_nonzero(x)) stop("Duplicated non-zero values")

  digits <- 1
  while (any_duplicated_nonzero(lsignif(x, digits))) digits <- digits + 1

  lsignif(x, digits)
}

get_range <- function(x, n, digits = 1) {
  test <- binom.test(x, n)

  values <- list(
    estimate = test$estimate,
    lci = test$conf.int[1], uci = test$conf.int[2]
  ) %>%
    min_signif() %>%
    # pcm = per cent mille = 1 per 100,000
    lapply(function(y) str_c(y * 1e5, " pcm"))

  with(values, { str_glue("{estimate} ({lci} to {uci}; {x}/{n})")} )
}

# Make nice tables of proportions, measured as counts and as 1 per X
counts %>%
  mutate(one_per = map_dbl(estimate, ~ signif(1 / ., 1))) %>%
  select(incidence, strategy, one_per) %>%
  pivot_wider(names_from = incidence, values_from = one_per) %>%
  write_tsv("results/results-incid-per.tsv")

counts %>%
  mutate(range = map2_chr(n_positive, n_total, get_range)) %>%
  select(incidence, strategy, range) %>%
  pivot_wider(names_from = incidence, values_from = range) %>%
  write_tsv("results/results-incid.tsv")

# Write a table showing the number of donations released
counts %>%
  select(incidence, strategy, n_total) %>%
  pivot_wider(names_from = incidence, values_from = n_total) %>%
  write_tsv("results/results-incid-n.tsv")


# Plot of proportions -------------------------------------------------

prop_plot <- counts %>%
  # Add a dummy (white) data point
  mutate(color = "black") %>%
  bind_rows(tibble(
      strategy = "Symptoms only", incidence = 1e-4, color = "white",
      estimate = 4e-4, conf.low = 4e-4, conf.high = 4e-4
  )) %>%
  # Recast values as percents
  mutate_at(c("estimate", "conf.low", "conf.high"), ~ . * 100) %>%
  mutate(
    strategy = fct_reorder(factor(strategy), estimate, max),
    label = fct_inorder(factor(as_exponential(incidence)))
  ) %>%
  ggplot(aes(estimate, strategy, color = color)) +
  facet_wrap(vars(label), scales = "free_x", labeller = label_parsed) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), shape = 1) +
  scale_x_continuous(breaks = extended_breaks(3)) +
  scale_color_identity() +
  labs(
    x = "Released donations that are virus-positive (%)",
    y = ""
  ) +
  guides(color = "none") +
  theme_cowplot() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(1, "lines")
  )

ggsave2("results/results-incid", height = 4, width = 8)

# Plot of odds ratios -------------------------------------------------

glm_results <- counts %>%
  # filter(num > 0) %>%
  select(incidence, strategy, n_positive, n_negative) %>%
  nest(data = c(strategy, n_positive, n_negative)) %>%
  mutate(
    model = map(data, ~ glm(cbind(n_positive, n_negative) ~ strategy, family = "binomial", data = .)),
    coef = map(model, ~ tidy(., conf.int = TRUE))
  ) %>%
  select(incidence, coef) %>%
  unnest(coef) %>%
  # get only the strategies
  filter(term != "(Intercept)") %>%
  mutate(strategy = str_replace(term, "^strategy", "")) %>%
  # Recast values as odds ratio (rather than log odds)
  mutate_at(c("estimate", "conf.low", "conf.high"), exp) %>%
  select(incidence, strategy, estimate, conf.low, conf.high)

# Write a table
glm_results %>%
  mutate_at("strategy", ~ fct_reorder2(factor(.), incidence, estimate)) %>%
  arrange(incidence, strategy) %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), ~ signif(., 3)) %>%
  write_tsv("results/results-incid-glm.tsv")

glm_plot <- glm_results %>%
  # throw in a dummy line to ensure we have the refence category
  bind_rows(tibble(strategy = "Symptoms only (ref.)", incidence = 1e-4)) %>%
  mutate(
    incidence_log10 = factor(log10(incidence)),
    strategy = fct_reorder(factor(strategy), estimate)
  ) %>%
  ggplot(aes(estimate, strategy)) +
  geom_vline(xintercept = c(0, 1), linetype = 1) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high, color = incidence_log10),
    shape = 1
  ) +
  geom_point(
    data = tibble(strategy = "Symptoms only (ref.)", estimate = 1),
    size = 3
  ) +
  scale_x_continuous(
    name = "Odds ratio that a released donation is virus-pos.",
    expand = c(0, 0.02)
  ) +
  scale_color_brewer(
    name = "Incidence",
    labels = math_format(),
    palette = "YlOrRd"
  ) +
  labs(y = "") +
  theme_cowplot() +
  theme(
    legend.position = c(0.70, 0.18),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave2("results/results-incid-odds", height = 5)
