data.dir <- "tsv"
data.dir <- "./"
files <- list.files(data.dir, pattern="tsv$")
length(files)
tbls <- list()
for(file in files){
    suppressWarnings(
        tbl <- read.table(file.path(data.dir, file), sep="\t", header=TRUE, as.is=TRUE, quote="", fill=TRUE)
        )
    tbls[[file]] <- tbl
    }
length(tbls)
tbl.trees <- do.call(rbind, tbls)
tbl.trees$age <- 2022 - tbl.trees$yearOfDeath

coi <- c("id",
         "lat",
         "long",
         "dbh",
         "age",
         "directionOfFall",
         "branches.small",
         "branches.large",
         "branches.huge",
         "ground.contact",
         "sag",
         "bark",
         "yearOfDeath",
         "failureMode",
         "failureHeight",
         "epiphyte.overall",
         "status",
         "datedReferenceTree",
         "date",
         "yearOfDeath.evidence",
         "bottom.decay",
         "bottom.decay.notes",
         "mid.decay",
         "mid.decay.notes",
         "top.decay",
         "top.decay.length",
         "top.decay.notes",
         "bottom.cross.section",
         "top.cross.section",
         "epiphytes.meter.1.moss",
         "epiphytes.meter.1.leafCoverage",
         "epiphytes.meter.2.moss",
         "epiphytes.meter.2.leafCoverage",
         "epiphyte.species",
         "epiphytes.overall.moss",
         "description",
         "notes")
tbl.trees <- tbl.trees[, coi]
deferred <- grep("rick", tbl.trees$description)
deferred

if(length(deferred) > 0)
    tbl.trees <- tbl.trees[-deferred,]

dim(tbl.trees)  # 12 37
rownames(tbl.trees) <- NULL
write.table(tbl.trees, file="tbl.tsv", sep="\t", quote=FALSE, row.names=FALSE)
