f <- "tbl.tsv"
#f <- "logs.tsv"
tbl <- read.table(f, sep="\t", header=TRUE, as.is=TRUE, nrow=-1, quote="")
tbl[, 1:11]
new.order <- order(tbl$age, decreasing=TRUE)
tbl <- tbl[new.order,]
coi <- c("age", "sag", "bark") #   "branches.huge", "branches.large", "branches.small",
pca <- prcomp(tbl[, coi], scale. = TRUE)
summary(pca)
biplot(pca)

summary(lm(age ~ 0 + sag, data=tbl))
model <- lm(age ~ 0 + sag + epiphyte.overall + bark, data=tbl)
summary(model)


model <- lm(age ~ 0 + sag, data=tbl)

summary(lm(age ~ 0 + sag + epiphyte.overall, data=tbl[-10,]))
#                  Estimate Std. Error t value Pr(>|t|)
# sag                 44.77      16.11    2.78   0.0214 *
# epiphyte.overall    40.28      15.79    2.55   0.0312 *


summary(model)
coef(model)

plot(model)

model <- (lm(age ~ 1 + sag + bark, data=tbl))
model <- lm(age ~ 0 + sag * bark, data=tbl)
model <- lm(age ~ 1 + sag, data=tbl)
model <- lm(age ~ 1 + bark, data=tbl)
model <- lm(age ~ 1 + sag + dbh, data=tbl)
model <- lm(age ~ 1 + sag + epiphytes.overall.moss, data=tbl)
model <- lm(age ~ 1 + sag + directionOfFall, data=tbl)

lm.coi <- c("dbh", "directionOfFall", "branches.small",
            "branches.large", "branches.huge", "sag", "bark",
            "yearOfDeath", " failureHeight", "epiphyte.overall",
            "bottom.decay", "top.decay", "epiphytes.overall.moss")


f.string <- sprintf("age ~ 1 + %s", paste(lm.coi, collapse=" + "))
f <- as.formula(f.string)
model <- lm(f, data=tbl)

#model <- lm(age ~ 0 + sag + bark", data=tbl[-1,])
plot(tbl$age, predict(model))
abline(model, col="red", lwd=5)
summary(model)
    # lm(formula = age ~ 0 + sag + bark, data = tbl)
    #
    # Residuals:
    #       Min        1Q    Median        3Q       Max
    # -12.06757  -4.65307   0.82532   4.66796  11.32532
    #
    # Coefficients:
    #             Estimate Std. Error  t value   Pr(>|t|)
    # (Intercept)  8.47368    9.23125  0.91793    0.38259
    # sag         71.68135   10.14542  7.06539 5.8857e-05 ***
    # bark        -3.47249    9.30937 -0.37301    0.71777
    # ---
    # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plot(tbl$sag, tbl$age, pch=19, main="Association of Log Sag with Years on the Ground")
abline(model, col="red")
model.predictions <- predict(model)
points(tbl$sag, model.predictions, col="red", pch=3, cex=1.5)

length(model.predictions)
dim(tbl)
tbl$modeled <- model.predictions
tbl$sag
tbl$modeled

cor(tbl[-1,]$age, predict(model))

Residual standard error: 7.85888 on 9 degrees of freedom
Multiple R-squared:  0.895055,	Adjusted R-squared:  0.871734
F-statistic: 38.3797 on 2 and 9 DF,  p-value: 3.92937e-05

prcomp(USArrests, scale. = TRUE)
prcomp(~ Murder + Assault + Rape, data = USArrests, scale. = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale. = TRUE))
biplot(prcomp(USArrests, scale. = TRUE))


  set.seed(17)
  adult <- round(unlist(lapply(1:10, function(i) jitter(67, amount=10))), digits=0)
  # 60 76 66 73 65 68 61 61 73 61
  factors <- unlist(lapply(1:10, function(i) jitter(2, amount=0.2)))
  y2h <- round(adult/factors)
  plot(y2h, adult)
  tbl.heights <- data.frame(y2h=y2h, adult=adult)
  fit <- lm(adult ~ y2h + 0, data=tbl.heights)
  abline(fit, col="red")
