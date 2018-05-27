#Source dgrid.R
#
dgrid <- function(XyDF, span=0.8) {
 stopifnot(is.data.frame(XyDF))
 stopifnot(length(names(XyDF))==ncol(XyDF))
 Xy <- XyDF
 n <- nrow(Xy)
 p <- ncol(Xy)-1
 xvar <- names(Xy)[1:p]
 yvar <- names(Xy)[p+1]
 Xy <- scale(Xy)
 x <- as.vector(Xy[,1:p])
 y <- Xy[,(p+1)]
 whichx <- rep(xvar, rep(n, p))
 df <- data.frame(y=y, x=x, whichx=whichx)
 xyplot(y ~ x | whichx, data=df, panel=function(x,y){
  panel.xyplot(x, y, col=densCols(x, y), pch=20, cex=1)
  panel.loess(x, y, col="black", lwd=2, span=span, degree=1)
 }, xlab="", ylab=yvar, main="Scaled Variables Dependency Plot")
}