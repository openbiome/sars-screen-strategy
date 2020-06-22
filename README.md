# SARS-CoV-2 screening strategies

Code for "Modelling donor screening strategies to reduce the risk of SARS-CoV-2 via
fecal microbiota transplantation"

## Getting started

The code run using R (developed against version 3.6.0) and packages
[tidyverse](https://tidyverse.tidyverse.org/) and
[cowplot](https://wilkelab.org/cowplot/).
[Snakemake](https://snakemake.readthedocs.io/en/stable/) is used for
convenience of executing the scripts but is not strictly necessary.

You can use [Conda](https://docs.conda.io/en/latest/) to create the compute
environment:

```
conda create --name sars_fmt r-base=3.6 python=3.6
conda activate sars_fmt
python3 -m pip install snakemake
R -e "install.packages(c('tidyverse', 'cowplot'))"
snakemake
```

## File structure

`Snakefile` shows the file dependencies. Roughly:

- `run-*.R` scripts depend on `parameters.tsv` and `model.R` and produce cached model outputs.
- `analyze-*.R` scripts depend on cached model outputs and produce cached results.
- `plot-*.R` scripts depend on cached results and produced figures and tables.

There are three workflows, one each for a baseline model `base`, an analysis of
the effect of altering disease incidence `incid`, and a sensitivity analysis
`sens`.

### Results

Running `snakemake` or `snakemake all` should produce this set of files in `results/`:

- `results-base.pdf` and `.png`: Histogram of donations released under point
  estimate parameters
- `results-incid-per.tsv`: Number of virus-positive donations released,
  expressed as donations per *X*, with *X* rounded to 1 significant digit
- `results-incid-prop.tsv`: Proportion of donations released that are
  virus-positive
- `results-incid.pdf` and `.png`: Histogram of donations under difference
  infection incidences
- `results-sens-corr.tsv`: Table of Spearman correlations between model
  parameters and model outcomes in sensitivity analysis
- `results-sens.pdf` and `.png`: Scatterplot of simulations in sensitivity
  analysis

## Author

Scott Olesen <solesen@openbiome.org>
