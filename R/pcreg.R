#Source: pcreg.R
#
pcreg <- function(Xy, scale=TRUE, method=c("PC", "LV"), ic=c("BIC", "AIC")) {
 ic <- match.arg(ic)
 k <- ifelse(ic=="BIC", log(nrow(Xy)), 2)
 method <- match.arg(method)
 p <- ncol(Xy)-1
 X <- data.matrix(Xy[,-(p+1)])
 y <- Xy[,p+1]
 if (method=="PC") {
  ansPLS <- NULL
  ansPC <- prcomp(X, center=TRUE, scale=scale) #also for scaling
  Z <- ansPC$x
#this is for prediction purposes
  getpcr <- function() {
   #see Advanced R, Wickham, Ch. 3.5
   y <- as.symbol(names(Xy)[p+1])
   substitute(pls::pcr(y ~ ., scale=scale, data=Xy))
  }
  PLSFit <- eval(getpcr())
 } else { #method="PLS"
  getpls <- function() {
   #see Advanced R, Wickham, Ch. 3.5
   y <- as.symbol(names(Xy)[p+1])
   substitute(pls::plsr(y ~ ., scale=scale, data=Xy))
  }
  PLSFit <- eval(getpls())
  Z <- PLSFit$scores
  colnames(Z) <- paste0("LV",1:p)
 }
 Zy <- as.data.frame.matrix(cbind(Z, y))
 colnames(Zy)[p+1] <- colnames(Xy)[p+1] #response variable
 #fit regression
 tbIC <- numeric(p)
 for (i in 1:p) {
  ind <- c(1:i, p+1)
  getReg <- function() {
   #see Advanced R, Wickham, Ch. 3.5
   y <- as.symbol(names(Xy)[p+1])
   substitute(lm(y ~ ., data=Zy[,ind]))
  }
  if (ic=="AIC") {
   tbIC[i] <- AIC(eval(getReg()))
  } else {
   tbIC[i] <- BIC(eval(getReg()))
  }
  mIC <- which.min(tbIC)
 }
 getReg <- function() {
  #see Advanced R, Wickham, Ch. 3.5
  y <- as.symbol(names(Xy)[p+1])
  substitute(lm(y ~ ., data=Zy[,c(1:mIC, p+1)]))
 }
 ansBest <- eval(getReg())
 ans <- list(lmfit=ansBest, PLSFit=PLSFit, Z=Z, method=method)
 class(ans) <- "pcreg"
 ans
}

print.pcreg <- function(x, ...) {
 print(x$lmfit)
}

summary.pcreg <- function(object, ...) {
 summary(object$lmfit)
}

residuals.pcreg <- function(object, ...) {
 resid(object$lmfit)
}

fitted.pcreg <- function(object, ...) {
 fitted(object$lmfit)
}

plot.pcreg <- function(x, ...) {
 layout(matrix(1:4, ncol=2))
 plot(x=x$lmfit)
 layout(1)
}

predict.pcreg <- function(object, newdata, ...) {
 ncomp <- length(coef(object$lmfit))-1
 as.vector(predict(object$PLSFit, newdata=newdata, ncomp=ncomp))
}
