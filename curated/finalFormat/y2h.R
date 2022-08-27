set.seed(17)
  adult <- round(unlist(lapply(1:10, function(i) jitter(67, amount=10))), digits=0)
  # 60 76 66 73 65 68 61 61 73 61
  factors <- unlist(lapply(1:10, function(i) jitter(2, amount=0.2)))
  y2h <- round(adult/factors)
  plot(y2h, adult)
  tbl.heights <- data.frame(y2h=y2h, adult=adult)
  fit <- lm(adult ~ y2h + 0, data=tbl.heights)
abline(fit, col="red")


