library(yaml)
f <- "logs.yaml"
file.exists(f)
x <- yaml.load(readLines(f, n=-1))
length(x)
unlist(lapply(x, function(el) el$id), use.names=FALSE)
