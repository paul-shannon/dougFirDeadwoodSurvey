f <- "tbl.tsv"
tbl <- read.table(f, sep="\t", header=TRUE, as.is=TRUE, nrow=-1, quote="")

summary(lm(age ~ 0 + sag + (dbh * epiphyte.overall), data=tbl))

model <- lm(age ~ 0 + sag + epiphyte.overall + bark, data=tbl)
summary(model)
predict(model)
plot(tbl$sag, predict(model), pch=8, col="blue")
cor(tbl$sag, predict(model))

