library(cowplot)

# Transform 1e-4 to "10^`-4`" for nice plotting
as_exponential <- function(x) as.character(math_format()(log10(x)))

# Save the same plot with multiple extensions
ggsave2 <- function(basename, exts = c("pdf", "png", "eps"), ...) {
  for (ext in exts) {
    fn <- str_c(basename, ".", ext)

    if (ext == "pdf") {
      ggsave(fn, useDingbats = FALSE, ...)
    } else {
      ggsave(fn, ...)
    }
  }
}
