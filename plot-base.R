#!/usr/bin/env Rscript

library(tidyverse)
source("plot-utils.R") # for ggsave2

results <- read_rds("cache/analysis-base.rds")

# Strategies, in order, with colors
strategies <- tribble(
  ~strategy,                   ~test_suite,                                      ~color,
  "Symptoms only",             c("symptoms"),                                    "black",
  "Stool only (28)",           c("symptoms", "stool28"),                         "#984ea3",
  "Stool only (14)",           c("symptoms", "stool14"),                         "#ba89c2",
  "Stool only (every)",        c("symptoms", "every_stool"),                     "#ddc4e0",
  "Swab only",                 c("symptoms", "swab"),                            "#e41a1c",
  "Swab, stool (28)",          c("symptoms", "swab", "stool28"),                 "#377eb8",
  "Swab, stool (14)",          c("symptoms", "swab", "stool14"),                 "#7aa9d0",
  "Swab, stool (every)",       c("symptoms", "swab", "every_stool"),             "#bcd4e7",
  "Swab, serology",            c("symptoms", "swab", "serology"),                "#4daf4a",
  "Swab, ser., stool (every)", c("symptoms", "swab", "serology", "every_stool"), "#a6d7a5"
) %>%
  mutate_at("strategy", fct_inorder)

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
  scale_fill_manual(
    breaks = strategies$strategy,
    values = strategies$color
  ) +
  labs(
    x = "No. of donations",
    y = "No. of simulations",
    fill = "Testing strategy"
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 4)) +
  theme_cowplot() +
  theme(legend.position = "bottom")

ggsave2("results/results-base", width = 7.2)
