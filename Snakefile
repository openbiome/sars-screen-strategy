rule top:
    input: expand("results/results-{x}.pdf", x=["base", "sens"])

rule clean:
    shell: "rm -f cache/* results/*"

rule plot:
    output: "results/results-{x}.pdf"
    input: "cache/analysis-{x}.rds", script="plot-{x}.R"
    shell: "./{input.script}"

rule analyze:
    output: "cache/analysis-{x}.rds"
    input: "cache/sims-{x}.rds", "analyze-utils.R", script="analyze-{x}.R"
    shell: "./{input.script}"

rule run:
    output: "cache/sims-{x}.rds"
    input: "model.R", "parameters.tsv", "run-utils.R", script="run-{x}.R"
    shell: "./{input.script}"
