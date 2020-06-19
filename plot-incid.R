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
  guides(fill = guide_legend(title.position = "top", nrow = 3)) +
  theme_cowplot() +
  theme(legend.position = "bottom")

ggsave("results/results-incid.pdf", width = 7.2)
ggsave("results/results-incid.png", width = 7.2)


# Make a table of number of simulations with nonzero positive releases
sim_counts <- results %>%
  group_by(incidence, strategy) %>%
  summarize(n_fail = sum(n_positive > 0)) %>%
  pivot_wider(names_from = incidence, values_from = n_fail) %>%
  arrange(strategy)

write_tsv(sim_counts, "results/results-incid-sim-counts.tsv")

# Make a table of the number of positive donations released
donation_counts <- results %>%
  group_by(incidence, strategy) %>%
  summarize_at(c("n_positive", "n_negative"), sum) %>%
  mutate(
    n_total = n_positive + n_negative,
    f_positive = n_positive / n_total
  ) %>%
  arrange(incidence, strategy)

write_tsv(donation_counts, "results/results-incid-donation-counts.tsv")
