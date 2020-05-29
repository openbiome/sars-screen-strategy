#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)

sims <- read_rds("cache/sims-sens.rds")

parameters <- read_tsv("parameters.tsv")

par_names <- parameters %>%
  filter(type != "fixed") %>%
  pull(name)

key_names <- c("use_serology", "use_stool")

out_names <- c("n_negative_released", "n_positive_released")

results <- sims %>%
  unnest_wider(par) %>%
  unnest_wider(sim) %>%
  mutate(key = case_when(
      use_serology & use_stool ~ "NP, serology, stool",
      use_serology & !use_stool ~ "NP, serology",
      !use_serology & use_stool ~ "NP, stool",
      !use_serology & !use_stool ~ "NP"
  )) %>%
  select_at(c("key", par_names, out_names))

plot_data <- results %>%
  # remove fixed columns
  select_if(function(x) length(unique(x)) > 1) %>%
  # intersect here because of columns dropped by select_if
  pivot_longer(cols = intersect(par_names, names(.)), names_to = "par_name", values_to = "par_value") %>%
  pivot_longer(cols = intersect(out_names, names(.)), names_to = "sim_name", values_to = "sim_value")

plot <- plot_data %>%
  ggplot(aes(sim_value, par_value, color = key)) +
  facet_grid(
    rows = vars(par_name),
    cols = vars(sim_name),
    scales = "free"
  ) +
  geom_point(shape = 3) +
  labs(
    x = "Outcome value",
    y = "Parameter value"
  ) +
  theme_cowplot()

ggsave("results/results-sens.pdf", height = 21)
ggsave("results/results-sens.png", height = 21)

# Correlations --------------------------------------------------------

get_corrs <- function(tbl) {
  crossing(par_name = par_names, out_name = out_names) %>%
    mutate(
      test = map2(par_name, out_name, ~ cor.test(tbl[[.x]], tbl[[.y]], method = "spearman")),
      estimate = map_dbl(test, ~ .$estimate),
      p_value = map_dbl(test, ~ .$p.value),
      sig = p.adjust(p_value, "BH") < 0.05
    ) %>%
    select(-test)
}

corr <- results %>%
  group_by(key) %>%
  nest() %>%
  ungroup() %>%
  mutate(corrs = map(data, get_corrs)) %>%
  select(-data) %>%
  unnest(cols = corrs) %>%
  arrange(desc(abs(estimate))) %>%
  mutate_if(is.numeric, ~ signif(., 2))

write_tsv(corr, "results/results-sens-corr.tsv")
