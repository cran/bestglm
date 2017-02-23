trainTestPartition <- function(Xy, trainFrac=2/3) {
 stopifnot(is.data.frame(Xy) && !is.null(names(Xy)))
 n <- nrow(Xy) #total number of observations
 p <- ncol(Xy)-1 #number of inputs
 stopifnot(trainFrac>=0 && trainFrac<=1)
 nTr <- round(trainFrac*n)
 nTe <- n-nTr
 indTr <- sample(c(rep(TRUE, nTr), rep(FALSE, nTe)))
 XyTr <- Xy[indTr, ] #select training data
 XTr <- as.matrix.data.frame(XyTr[,1:p]) 
 yTr <- XyTr[,p+1] #as vector
 XyTe <- Xy[!indTr, ] #select test data
 XTe <- as.matrix.data.frame(XyTe[,1:p])
 yTe <- XyTe[,p+1] #as vector
 list(XyTr=XyTr, XTr=XTr, yTr=yTr, XyTe=XyTe, XTe=XTe, yTe=yTe)
}

glmnetGridTable <- function(XyList, alpha=0, nfolds=10, family="gaussian"){
 #alpha=1 is LASSO, alpha=0 is RR
 #need to adjust cost function for other models!
 rmse <- function(y, yh) sqrt(mean((y-yh)^2))
 stopifnot(nfolds>=3 && nfolds<=10)
 XyTr <- XyList$XyTr
 XTr <- XyList$XTr
 XyTe <- XyList$XyTe
 XTe <- XyList$XTe
 yTr <- XyList$yTr
 yTe <- XyList$yTe
 p <- ncol(XTr)
 bRR <- matrix(0, ncol=4, nrow=p)
 RMSE <- numeric(4)
 layout(matrix(1:4, ncol=2))
 for (i in 1:4) {
  ansCV <- cv.glmnet(x=XTr, y=yTr, alpha=alpha, family=family)
  plot(ansCV)
  ans <- glmnet(x=XTr, y=yTr, alpha=alpha)
  indBest <- which.min(abs(ans$lambda - ansCV$lambda.1se))
  bRR[,i] <- ans$beta[,indBest]
  RMSE[i] <- rmse(yTe, as.vector(predict(ans, newx=XTe, 
                                         s=ansCV$lambda.1se)))
 }
 layout(1)
 if (abs(alpha)<1e-4) {
  abbPen <- "RR"
 } else {
  if (abs(alpha-1)<1e-4) {
   abbPen <- "LASSO"
  } else {
   abbPen <- paste0("EL(", round(alpha,2), ")")
  }
 }
 title(sub=abbPen)
 #compare estimates with OLS and Step/BIC
 # we need to standardize the X matrix as in glmnet()
 XTrS <- scale(XTr)
 XyTrS <- data.frame(XTrS, y=yTr)
 names(XyTrS) <- names(XyTr)
 XTeS <- scale(XTe)
 XyTeS <- data.frame(XTeS, y=yTe)
 names(XyTeS) <- names(XyTe)
 doLM <- function() {
  #see Advanced R, Wickham, Ch. 3.5
  y <- as.symbol(names(XyTrS)[p+1])
  ans <- substitute(lm(y ~ ., data=XyTrS))
  ans
 }
 ansOLS <- eval(doLM())
 rmseOLS <- rmse(yTe, predict(ansOLS, newdata=XyTeS))
 bOLS <- coef(ansOLS)[-1] #Note: shrinkage not applied to intercept
 #Step - AIC
 ansStep <- step(ansOLS, trace=0)
 rmseStepAIC <- rmse(yTe, predict(ansStep, newdata=XyTeS))
 bStep <- coef(ansStep)[-1]
 bStepAIC <- numeric(p)
 names(bStepAIC) <- names(bOLS)
 bStepAIC[names(bStep)] <- bStep
 #Step - BIC
 ansStep <- step(ansOLS, k=log(length(yTr)), trace=0)
 rmseStepBIC <- rmse(yTe, predict(ansStep, newdata=XyTeS))
 bStep <- coef(ansStep)[-1]
 bStepBIC <- numeric(p)
 names(bStepBIC) <- names(bOLS)
 bStepBIC[names(bStep)] <- bStep
 #table
 b <- matrix(c(bOLS, bStepAIC, bStepBIC, bRR), nrow=p)
 bNorm <- apply(b, 2, function(z) sqrt(sum(z^2)))
 b <- rbind(b, bNorm, c(rmseOLS, rmseStepAIC, rmseStepBIC, RMSE))
 dimnames(b) <- list(c(names(bOLS), "NORM", "RMSE"), 
                 c("OLS","StepAIC","StepBIC", paste0(abbPen,1:4)))
 b
}

glmnetPredict <- function(XyList,  NREP=15, alpha=0, nfolds=10,
     family=c("gaussian","binomial","poisson","multinomial"))
     {
 stopifnot(nfolds>=3 && nfolds<=10)
 family <- match.arg(family)
 XyTr <- XyList$XyTr
 XTr <- XyList$XTr
 XTe <- XyList$XTe
 yTr <- XyList$yTr
 p <- ncol(XTr)
 ans <- glmnet(x=XTr, y=yTr, alpha=alpha) #not random
 yHat <- numeric(nrow(XTe))
 for (i in 1:NREP) {
  ansCV <- cv.glmnet(x=XTr, y=yTr, alpha=alpha, nfolds=nfolds, family=family)
  yHat <- yHat + 
          as.vector(predict(ans, newx=XTe, s=ansCV$lambda.1se))
 }
 yHat/NREP 
}


