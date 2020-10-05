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
  filter(
    strategy == "Swab, ser., stool (every)",
    n_positive > 0
  ) %>%
  # remove fixed columns
  select_if(function(x) length(unique(x)) > 1) %>%
  pivot_longer(
    # intersect here because of columns dropped by select_if
    cols = intersect(par_names, names(.)),
    names_to = "par_name", values_to = "par_value"
  )

plot <- plot_data %>%
  ggplot(aes(par_value, n_positive)) +
  facet_wrap(vars(par_name), scales = "free") +
  geom_point(shape = 1, position = position_jitter(height = 0.1)) +
  scale_y_continuous(breaks = 1:3) +
  labs(
    x = "Parameter value",
    y = "No. positive donations released"
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 8))

ggsave("results/results-sens.pdf", width = 9, height = 6.5, useDingbats = FALSE)
ggsave("results/results-sens.png", width = 9, height = 6.5)

# Correlations --------------------------------------------------------

get_corrs <- function(tbl) {
  crossing(par_name = par_names, out_name = out_names) %>%
    mutate(correlation = map2_dbl(
        par_name, out_name,
        ~ cor(tbl[[.x]], tbl[[.y]], method = "spearman")
    ))
}

corr <- results %>%
  group_by(strategy) %>%
  nest() %>%
  ungroup() %>%
  mutate(corrs = map(data, get_corrs)) %>%
  select(strategy, corrs) %>%
  unnest(cols = corrs) %>%
  arrange(desc(abs(correlation))) %>%
  mutate_at("correlation", ~ signif(., 2))

write_tsv(corr, "results/results-sens-corr.tsv")
