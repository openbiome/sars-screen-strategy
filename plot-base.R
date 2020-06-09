#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)

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
  # geom_histogram(aes(fill = strategy), position = "dodge", bins = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "No. of donations",
    y = "No. of simulations",
    fill = "Testing strategy"
  ) +
  theme_cowplot() +
  theme(legend.position = c(0.75, 0.5))

ggsave("results/results-base.pdf")
ggsave("results/results-base.png")
