data.dir <- "tsv"
files <- list.files(data.dir, pattern="tsv$")
tbls <- list()
for(file in files){
    suppressWarnings(
        tbl <- read.table(file.path(data.dir, file), sep="\t", header=TRUE, as.is=TRUE, quote="", fill=TRUE)
        )
    tbls[[file]] <- tbl
    }
length(tbls)
tbl.trees <- do.call(rbind, tbls)
rownames(tbl.trees) <- NULL
write.table(tbl.trees, file="tbl.tsv", sep="\t", quote=FALSE, row.names=FALSE)
