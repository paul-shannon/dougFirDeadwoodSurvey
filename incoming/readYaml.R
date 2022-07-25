library(yaml)
f <- "logs-10-16-19jul2022.yaml"
file.exists(f)
x <- yaml.load(readLines(f, n=-1))
length(x)
unlist(lapply(x, function(el) el$id), use.names=FALSE)
