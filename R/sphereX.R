sphereX <- function(X) {
 Z <- scale(X)
 Z%*%solve(chol(var(Z)))
}

NNPredict <- function(XyList, dist=c("L2", "COR", "L1")) {
 dist <- match.arg(dist)
 X <- rbind(XyList$XTr, XyList$XTe)
 p <- ncol(X)
 Z <- sphereX(X)
 ind <- (1:nrow(Z))%in%(1:nrow(XyList$XTr))
 ZTr <- Z[ind,]
 ZTe <- Z[!ind,]
 nTe <- nrow(ZTe)
 nTr <- nrow(ZTr)
 yh <- numeric(nTe)
 for (i in 1:nTe) {
  xi <- ZTe[i,] #given this row in test sample
  edist <- switch(dist,
         L1 = rowSums(abs(ZTr-matrix(xi, byrow=TRUE, ncol=p, nrow=nTr))),
         L2 = rowSums((ZTr-matrix(xi, byrow=TRUE, ncol=p, nrow=nTr))^2),
         CORR = 1-abs(apply(XyList$XTr, 1, function(x) cor(x, xi)))
  )
  yh[i] <- XyList$yTr[which.min(edist)] #prediction 
 }
 yh
}

