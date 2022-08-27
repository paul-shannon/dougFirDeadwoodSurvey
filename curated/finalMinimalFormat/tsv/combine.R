data.dir <- "./"
files <- list.files(data.dir, pattern="df.*tsv$")
printf("combining %d files", length(files))
tbls <- list()
for(file in files){
    suppressWarnings(
        tbl <- read.table(file.path(data.dir, file), sep="\t", header=TRUE, as.is=TRUE, quote="", fill=TRUE)
        )
   # tbl$age <- -1
   # if(tbl$yearOfDeath != -1){
   #     tbl$age <- 2022 - tbl$yearOfDeath
   # } else {
   #     tbl$age <- 87.107 *
    if(tbl$status != "complete") next;
    tbls[[file]] <- tbl
    }
length(tbls)
tbl.trees <- do.call(rbind, tbls)

coi <- c("id",
         "lat",
         "long",
         "dbh",
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

dim(tbl.trees)  # 37 21
rownames(tbl.trees) <- NULL

   # build a model of collapse -> age for trees with known fall dates
tbl.known <- subset(tbl.trees, datedReferenceTree==TRUE)
tbl.known$age <- 2022 - tbl.known$yearOfDeath
tbl.predicted <- subset(tbl.trees, datedReferenceTree!=TRUE)
dim(tbl.known)     # 11 21
dim(tbl.predicted)   # 25 21

model <- lm(age ~ 1 + collapse, data=tbl.known)
summary(model)
coefficients <- coef(model)
intercept <- coefficients[["(Intercept)"]]
collapse.beta <- coefficients[["collapse"]]
tbl.predicted$age <- as.integer(intercept + (tbl.predicted$collapse * collapse.beta))

plot(x=NULL, y=NULL, ylim=c(0,220), xlim=c(0,3),
     main="measured (black) & predicted(light blue) age from log collapse\ncallibration: collapse=1 in 1935 tree",
     ylab="years on the ground", xlab="log collapse (1.0 is tree from 1935)")
with(tbl.known, points(collapse, jitter(age, amount=4), col="black", pch=16, cex=1.2))
with(tbl.predicted, points(collapse, jitter(age, amount=4), col="lightblue", pch=16, cex=1))

tbl.trees <- rbind(tbl.known, tbl.predicted)
write.table(tbl.trees, file="tbl.tsv", sep="\t", quote=FALSE, row.names=FALSE)

quartz()

hist(tbl.trees$age, breaks=seq(from=0, to=200, length.out=70),
     main="years before present\nyears on ground modeled from known fall year\nas 6.4 + (75 * collapse)",
     xlab="")

main.title <- paste("Douglas-fir mortality: 10 trees with known fall dates",
                    "21 estimated from log decay",
                    "modeled as 6.4 + (75 * decay)",
                    sep="\n")
hist(tbl.trees$age, breaks=seq(from=0, to=200, length.out=70),
     main= main.title,
     xlab="estimated years before present", xlim=c(200, 0))



