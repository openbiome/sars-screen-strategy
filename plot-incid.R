#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)
source("analyze-utils.R") # for strategy colors

results <- read_rds("cache/analysis-incid.rds")
n_positive_max <- 10

# Check that the categories I'll use are good ones
with(results, {
  if (any(n_negative > 99)) stop("over negative limit")
  if (any(n_positive > n_positive_max)) stop("over positive limit")
})

# Transform 1e-4 to "10^`-4`" for nice plotting
as_exponential <- function(x) as.character(math_format()(log10(x)))

plot_data <- results %>%
  mutate(
    n_positive = factor(case_when(
        n_positive == 0 ~ "0",
        n_positive %in% c(1, 2) ~ "1-2",
        n_positive %in% c(3, 4) ~ "3-4",
        n_positive == 5 ~ "5"
      ),
      levels = c("0", "1-2", "3-4", "5")
    ),
    n_negative = factor(
      case_when(
        between(n_negative,  0, 24) ~  "0-24",
        between(n_negative, 25, 49) ~ "25-49",
        between(n_negative, 50, 74) ~ "50-74",
        between(n_negative, 75, 99) ~ "75-99",
      ),
      levels = c("0-24", "25-49", "50-74", "75-99")
    )
  ) %>%
  pivot_longer(cols = c(n_positive, n_negative)) %>%
  arrange(incidence) %>%
  mutate(
    row_label = as_exponential(incidence),
    row_label = fct_inorder(factor(row_label)),
    col_label = recode(name,
      n_positive = "'Virus-positive donations'",
      n_negative = "'Virus-negative donations'"
    )
  ) %>%
  # go from each row being a simulation to each row being a
  # incidence/strategy/no.simulations combo
  count(col_label, row_label, strategy, value) %>%
  # include zero counts (e.g., no positives)
  complete(
    nesting(col_label, row_label, value), strategy,
    fill = list(n = 0)
  )

plot <- plot_data %>%
  ggplot(aes(value, n, fill = strategy)) +
  facet_grid(
    rows = vars(row_label),
    cols = vars(col_label),
    scales = "free_x",
    labeller = label_parsed
  ) +
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 250)) +
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
  theme(
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

ggsave("results/results-incid.pdf", width = 7.2)
ggsave("results/results-incid.png", width = 7.2)
ggsave("results/results-incid.eps", width = 7.2)


# Make a table of the number of positive donations released

lsignif <- function(x, digits) lapply(x, function(xx) signif(xx, digits))

any_duplicated_nonzero <- function(x) {
  as.numeric(x) %>%
    { duplicated(.)[. != 0] } %>%
    any()
}

min_signif <- function(x) {
  if (any_duplicated_nonzero(x)) stop("Duplicated non-zero values")

  digits <- 1
  while (any_duplicated_nonzero(lsignif(x, digits))) digits <- digits + 1

  lsignif(x, digits)
}

get_range <- function(x, n, digits = 1) {
  test <- binom.test(x, n)

  values <- list(
    estimate = test$estimate,
    lci = test$conf.int[1], uci = test$conf.int[2]
  ) %>%
    min_signif() %>%
    # pcm = per cent mille = 1 per 100,000
    lapply(function(y) str_c(y * 1e5, " pcm"))

  with(values, { str_glue("{estimate} ({lci} to {uci}; {x}/{n})")} )
}

donation_counts <- results %>%
  group_by(incidence, strategy) %>%
  summarize_at(c("n_positive", "n_negative"), sum) %>%
  mutate(
    n_total = n_positive + n_negative,
    f_positive = n_positive / n_total,
    one_per = map_dbl(f_positive, ~ signif(1 / ., 1)),
    range = map2_chr(n_positive, n_total, get_range)
  ) %>%
  arrange(incidence, strategy) %>%
  select(incidence, strategy, one_per, range)

donation_counts %>%
  select(incidence, strategy, one_per) %>%
  pivot_wider(names_from = incidence, values_from = one_per) %>%
  write_tsv("results/results-incid-per.tsv")

donation_counts %>%
  select(strategy, incidence, range) %>%
  pivot_wider(names_from = incidence, values_from = range) %>%
  write_tsv("results/results-incid-prop.tsv")
