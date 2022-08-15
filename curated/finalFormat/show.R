args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 1)
    stop("usage Rscript show.R df-xxx")
base.name <- args[1]
f <- sprintf("tsv/%s.tsv", base.name)
stopifnot(file.exists(f))

tbl <- read.table(f, sep="\t", header=TRUE, as.is=TRUE)
print(tbl)

