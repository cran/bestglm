`oneSdRule` <-
function (CVout){
#Rule: find model which has fewest number of parameters and
#      is within one sd of the model with smallest CV error.
cverrs <- CVout[,1]
indMin <- which.min(cverrs)
if (indMin==1)
    indBest <- 1
else {
    ses <- CVout[,2]
    fmin<-ses[indMin]
    cutOff <- fmin/2 + cverrs[indMin]
    indRegion <- cutOff > cverrs
    Offset <- sum(cumprod(as.numeric(!indRegion)))
    TheMins <- (cutOff-cverrs)[indRegion]
    indBest<-Offset + (1:length(TheMins))[(min(TheMins)==TheMins)]
}
indBest
}

