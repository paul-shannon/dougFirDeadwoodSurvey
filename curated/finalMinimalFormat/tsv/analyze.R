f <- "tbl.tsv"
#f <- "logs.tsv"
tbl <- read.table(f, sep="\t", header=TRUE, as.is=TRUE, nrow=-1, quote="")
table(tbl$datedReferenceTree)  # FALSE  TRUE
                               #    29    11

tbl.aged <- subset(tbl, datedReferenceTree==TRUE)
tbl.predice <- subset(tbl, datedReferenceTree==FALSE)
#tbl[, 1:11]
#new.order <- order(tbl$age, decreasing=TRUE)
#tbl <- tbl[new.order,]

summary(lm(age ~ 0 + collapse, data=tbl.aged))
model <- lm(age ~ 0 + sag + epiphyte.overall + bark, data=tbl)
summary(model)


model <- lm(age ~ 0 + sag, data=tbl)
