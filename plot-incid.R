#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)
source("analyze-utils.R") # for strategy colors

results <- read_rds("cache/analysis-incid.rds")

plot <- results %>%
  pivot_longer(cols = c(n_positive, n_negative)) %>%
  mutate(
    label = recode(name,
      n_positive = "Virus-positive donations",
      n_negative = "Virus-negative donations"
    )
  ) %>%
  ggplot(aes(value)) +
  facet_grid(
    rows = vars(daily_inf_prob),
    cols = vars(label),
    scales = "free_x"
  ) +
  scale_x_binned(show.limits = TRUE) +
  geom_bar(aes(fill = strategy), position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(
    breaks = strategies$strategy,
    values = strategies$color
  ) +
  labs(
    x = "No. of donations",
    y = "No. of simulations",
    fill = "Testing strategy"
  ) +
  theme_cowplot() +
  theme(
    # legend.position = c(0.75, 0.5)
  )

ggsave("results/results-incid.pdf")
ggsave("results/results-incid.png")
