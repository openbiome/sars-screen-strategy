all: results/output.pdf

.PHONY: clean
clean:
	rm -f *.rds

cache/sims.rds: parameters.tsv model.R run.R
	./run.R

results/output.pdf: cache/sims.rds analyze.R
	./analyze.R
