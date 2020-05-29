#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)

sims <- read_rds("cache/sims-base.rds")

results <- sims %>%
  unnest_wider(par) %>%
  unnest_wider(sim)

results

plot <- results %>%
  mutate(input_key = case_when(
      use_serology & use_stool ~ "NP, serology, stool",
      use_serology & !use_stool ~ "NP, serology",
      !use_serology & use_stool ~ "NP, stool",
      !use_serology & !use_stool ~ "NP"
  )) %>%
  select(input_key, n_positive_released, n_negative_released) %>%
  pivot_longer(cols = c(n_positive_released, n_negative_released), names_to = "output_key") %>%
  mutate_at(
    "output_key",
    ~ recode(.,
        n_positive_released = "Virus-positive donations",
        n_negative_released = "Virus-negative donations"
      )
  ) %>%
  ggplot(aes(value)) +
  facet_wrap(vars(output_key), scales = "free") +
  geom_histogram(aes(fill = input_key), position = "dodge", bins = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "No. of donations",
    y = "No. of simulations",
    fill = "Testing strategy"
  ) +
  theme_cowplot() +
  theme(legend.position = c(0.65, 0.5))

ggsave("results/results-base.pdf")
