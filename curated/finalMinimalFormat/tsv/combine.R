data.dir <- "./"
files <- list.files(data.dir, pattern="df.*tsv$")
printf("combining %d files", length(files))
tbls <- list()
for(file in files){
    suppressWarnings(
        tbl <- read.table(file.path(data.dir, file), sep="\t", header=TRUE, as.is=TRUE, quote="", fill=TRUE)
        )
    tbl$age <- -1
    if(tbl$yearOfDeath != -1)
        tbl$age <- 2022 - tbl$yearOfDeath
    tbls[[file]] <- tbl
    }
length(tbls)
tbl.trees <- do.call(rbind, tbls)

coi <- c("id",
         "lat",
         "long",
         "dbh",
         "age",
         "directionOfFall",
         "branches.small",
         "branches.large",
         "branches.huge",
         "collapse",
         "bark",
         "yearOfDeath",
         "failureMode",
         "failureHeight",
         "epiphyte.overall",
         "status",
         "datedReferenceTree",
         "date",
         "yearOfDeath.evidence",
         "description",
         "notes")
tbl.trees <- tbl.trees[, coi]

dim(tbl.trees)  # 12 37
rownames(tbl.trees) <- NULL
write.table(tbl.trees, file="tbl.tsv", sep="\t", quote=FALSE, row.names=FALSE)
