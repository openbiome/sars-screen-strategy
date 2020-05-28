all: results/output-base.pdf

.PHONY: clean
clean:
	rm -f *.rds

cache/sims-base.rds: parameters.tsv model.R run-base.R
	./run-base.R

results/output-base.pdf: cache/sims-base.rds analyze-base.R
	./analyze-base.R
