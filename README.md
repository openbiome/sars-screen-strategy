# SARS-CoV-2 screening strategies

[Model structure](https://docs.google.com/document/d/1k8xJtWVrZhf7a5QZyMc4LVezpczDttjhkRd_S5kXces/edit)

## Structure

- `Snakefile` shows the file dependencies. Roughly, `run-*.R` scripts depend on `parameters.tsv` and `model.R` and produce cached model outputs.
- `analyze-*.R` scripts read the cached model outputs and produce output graphs and tables.

## Author

Scott Olesen <solesen@openbiome.org>
