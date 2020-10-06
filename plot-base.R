#!/usr/bin/env Rscript

library(tidyverse)
source("plot-utils.R") # for ggsave2

results <- read_rds("cache/analysis-base.rds")

plot <- results %>%
  pivot_longer(cols = c(n_positive, n_negative)) %>%
  mutate(
    label = recode(name,
      n_positive = "Virus-positive donations",
      n_negative = "Virus-negative donations"
    )
  ) %>%
  ggplot(aes(value)) +
  facet_wrap(vars(label), scales = "free") +
  scale_x_binned(show.limits = TRUE) +
  geom_bar(aes(fill = strategy), position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "No. of donations",
    y = "No. of simulations",
    fill = "Testing strategy"
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 4)) +
  theme_cowplot() +
  theme(legend.position = "bottom")

ggsave2("results/results-base", width = 7.2)
