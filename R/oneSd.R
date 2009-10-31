oneSdRule<- function (CVout) 
{
    cverrs <- CVout[, 1]
    indMin <- which.min(cverrs)
    fmin <- CVout[indMin,2]
    cutOff <- fmin + cverrs[indMin]
    min(which(cverrs<cutOff)) 
}
