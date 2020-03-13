plot1SDRule <- function(ans, main="", sub="", xlab="df", ylab="EPE" ) {
  stopifnot(class(ans)[1]=="train"||
             ((is.matrix(ans)||is.data.frame(ans))&&ncol(ans)==2))
  if (class(ans)[1]=="train") {
    nMin <- unlist(ans$bestTune)
    names(ans$results)[1] <- "tunePar"
    with(ans$results, plot(tunePar, RMSE, xlab=xlab, ylab=ylab))
    RLoHi <- with(ans$results, matrix(c(RMSE-RMSESD,RMSE+RMSESD), ncol=2))
    with(ans$results, segments(x0=tunePar, y0=RLoHi[,1], 
                               x1=tunePar, y1=RLoHi[,2]))
    abline(h=RLoHi[nMin,], col="red")
    cverrs <- with(ans$results, cbind(RMSE, RMSESD))
  } else {
    cverrs <- ans
    plot(1:nrow(cverrs), cverrs[,1], xlab=xlab, ylab=ylab)
  }
  nMin <- which.min(cverrs[,1])
  nOpt <- bestglm::oneSDRule(cverrs)
  points(ans$results$tunePar[nOpt], cverrs[nOpt,1], pch=18, col="blue")
  points(ans$results$tunePar[nMin], cverrs[nMin,1], pch=18, col="red")
  title(main=main, sub=sub)
}