rule top:
    input: expand("results/results-{x}.pdf", x=["base"])

rule clean:
    shell: "rm -f cache/* results/*"

rule analyze:
    output: "results/results-{x}.pdf"
    input: "cache/sims-{x}.rds", script="analyze-{x}.R"
    shell: "./{input.script}"

rule run:
    output: "cache/sims-{x}.rds"
    input: "model.R", "parameters.tsv", script="run-{x}.R"
    shell: "./{input.script}"
