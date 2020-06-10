#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)

parameters <- read_tsv("parameters.tsv")

par_names <- parameters %>%
  filter(type != "fixed") %>%
  pull(name)

out_names <- c("n_negative", "n_positive")

results <- read_rds("cache/analysis-sens.rds") %>%
  unnest_wider(par) %>%
  select_at(c("iter", "strategy", par_names, out_names))

plot_data <- results %>%
  # remove fixed columns
  select_if(function(x) length(unique(x)) > 1) %>%
  # intersect here because of columns dropped by select_if
  pivot_longer(cols = intersect(par_names, names(.)), names_to = "par_name", values_to = "par_value") %>%
  pivot_longer(cols = intersect(out_names, names(.)), names_to = "sim_name", values_to = "sim_value")

plot <- plot_data %>%
  ggplot(aes(sim_value, par_value)) +
  facet_grid(
    rows = vars(par_name),
    cols = vars(strategy, sim_name),
    scales = "free"
  ) +
  geom_point(shape = 3) +
  labs(
    x = "Outcome value",
    y = "Parameter value"
  ) +
  theme_cowplot()

ggsave("results/results-sens.pdf", width = 28, height = 21)
ggsave("results/results-sens.png", width = 28, height = 21)

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
  group_by(strategy) %>%
  nest() %>%
  ungroup() %>%
  mutate(corrs = map(data, get_corrs)) %>%
  select(-data) %>%
  unnest(cols = corrs) %>%
  arrange(desc(abs(estimate))) %>%
  mutate_if(is.numeric, ~ signif(., 2))

write_tsv(corr, "results/results-sens-corr.tsv")
