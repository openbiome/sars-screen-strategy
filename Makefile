all: output.pdf

.PHONY: clean
clean:
	rm -f *.rds

sims.rds: parameters.tsv model.R run.R
	./run.R

output.pdf: sims.rds analyze.R
	./analyze.R
