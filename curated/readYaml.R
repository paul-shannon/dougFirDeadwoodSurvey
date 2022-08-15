library(yaml)
options(digits=9)
f <- "logs.yaml"
f <- "logs43-47.yaml"
f <- "logs-009.yaml"
base.name <- "logs-009-050"
base.name <- "logs-with-deathDates"
f <- sprintf("%s.yaml", base.name)
file.exists(f)
x <- yaml.load(readLines(f, n=-1))
length(x)
head(names(x))
expected.fields <- c("description",
                     "lat",
                     "long",
                     "dbh",
                     "bark",
                     "moss",
                     "epiphytes",
                     "yearOfDeath",
                     "branches.small",
                     "branches.large",
                     "branches.huge",
                     "ground.contact",
                     "top.cross.section",
                     "bottom.cross.section")
for(i in seq_len(length(x))){
    actual <- names(x[[i]])
    printf("checking tree %s", names(x)[i])
    missing <- setdiff(expected.fields, names(x[[i]]))
    if(length(missing) > 0){
        printf("missing: %s", paste(missing, collapse=", "))
        stop()
        } # if missing
    } # for i

extract <- function(el){
    stopifnot(all(expected.fields %in% names(el)))
    data.frame(lat=el$lat,
               long=el$long,
               dbh=el$dbh,
               bark=el$bark,
               moss=el$moss,
               epiphytes=el$epiphytes,
               yearOfDeath=el$yearOfDeath,
               branches.small=el$branches.small,
               branches.large=el$branches.large,
               branches.huge=el$branches.huge,
               ground.contact=el$ground.contact,
               top.xsection=el$top.cross.section,
               bottom.xsection=el$bottom.cross.section,
               description=el$description
               )
    }


rows <- lapply(x, extract)

printf("row count: %d", length(rows))
tbl <- do.call(rbind, rows)
tbl$id <- names(x)
tbl$long <- -1 * tbl$long
print(head(tbl))
dim(tbl)

f.out <- sprintf("%s.tsv", base.name)
write.table(tbl, file=f.out, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
