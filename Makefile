all: sims.rds

.PHONY: clean
clean:
	rm -f *.rds

sims.rds: parameters.tsv model.R run.R
	./run.R
