oneSDRule<- function (CVout) 
{
#Changjiang Xu. October 29, 2009
#CVout[,1] - cv estimates
#CVout[,2] - sd of estimates
#extended now to take output from caret::train()
 stopifnot(is.matrix(CVout)||is.data.frame(CVout)||class(CVout)[1]=="train")
 if (class(CVout)[1]=="train") {
  ans <- CVout
  CVout <- with(ans$results, cbind(RMSE, RMSESD))
 } 
    cverrs <- CVout[, 1]
    indMin <- which.min(cverrs)
    fmin <- CVout[indMin, 2]
    cutOff <- fmin + cverrs[indMin]
    min(which(cverrs<cutOff)) 
}