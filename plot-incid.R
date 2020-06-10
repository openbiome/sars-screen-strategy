#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)
source("analyze-utils.R") # for strategy colors

results <- read_rds("cache/analysis-incid.rds")

plot <- results %>%
  pivot_longer(cols = c(n_positive, n_negative)) %>%
  arrange(incidence) %>%
  mutate(
    row_label = recode(incidence,
      `1e-5` = "10^-5",
      `1e-4` = "10^-4",
      `1e-3` = "10^-3",
    ),
    row_label = fct_inorder(row_label),
    col_label = recode(name,
      n_positive = "'Virus-positive donations'",
      n_negative = "'Virus-negative donations'"
    )
  ) %>%
  ggplot(aes(value)) +
  facet_grid(
    rows = vars(row_label),
    cols = vars(col_label),
    scales = "free_x",
    labeller = label_parsed
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
