to.binary <- function(n, k = ceiling(logb(n+1,base=2))) {
x1 <- paste(rev(as.integer(intToBits(n))), collapse="")
x2 <- rev(as.integer(unlist(strsplit(sub("^0+", "", x1), NULL))))
c(x2, rep(0, k-length(x2)))
}
