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
  mutate(n_total = n_positive + n_negative)


# Plot of proportions -------------------------------------------------

# Get 95% CIs on proportions
counts_prop <- counts %>%
  mutate(
    binom_test = map2(n_positive, n_total, binom.test),
    data = map(binom_test, tidy)
  ) %>%
  select(incidence, strategy, data) %>%
  unnest(data)

prop_plot <- counts_prop %>%
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


# Plot and table of odds ratios ----------------------------------------

counts_glm <- counts %>%
  select(incidence, strategy, n_positive, n_negative) %>%
  nest(data = c(strategy, n_positive, n_negative)) %>%
  mutate(
    model = map(data, ~ glm(cbind(n_positive, n_negative) ~ strategy, family = "binomial", data = .)),
    coef = map(model, ~ tidy(., conf.int = TRUE))
  ) %>%
  unnest(data, coef) %>%
  # Recast values as odds ratio (rather than log odds)
  mutate_at(
    c("estimate", "conf.low", "conf.high"),
    ~ if_else(strategy == "Symptoms only", 1, exp(.), 2)
  )


glm_plot <- counts_glm %>%
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

# Output a table
counts_glm %>%
  mutate_at(c("estimate", "conf.low", "conf.high"), ~ signif(., 2)) %>%
  mutate(range = str_glue("{estimate} ({conf.low} to {conf.high})")) %>%
  select(strategy, incidence, n_positive, n_negative, range) %>%
  arrange(strategy, desc(incidence)) %>%
  write_tsv("results/results-incid.tsv")
