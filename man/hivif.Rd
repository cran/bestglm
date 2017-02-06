\name{hivif}
\alias{hivif}
\docType{data}
\title{
Simulated Linear Regression (Train) with Nine Highly Correlated Inputs
}
\description{
The script that generated this data is given below.
}
\usage{data("hivif")}
\format{
  A data frame with 1000 observations on the following 10 variables.
  \describe{
    \item{\code{x1}}{a numeric vector}
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
    \item{\code{x4}}{a numeric vector}
    \item{\code{x5}}{a numeric vector}
    \item{\code{x6}}{a numeric vector}
    \item{\code{x7}}{a numeric vector}
    \item{\code{x8}}{a numeric vector}
    \item{\code{x9}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}

\examples{
#Simple example
data(hivif)
lm(y ~ ., data=hivif)
#
#This example shows how the original data was simulated and
#how additional test data may be simulated.
\dontrun{
 set.seed(778851) #needed for original training data
 n <- 100
 p <- 9 #9 covariates plus intercept
 sig <- toeplitz(0.9^(0:(p-1)))
 X <- MASS::mvrnorm(n=n, rep(0, p), Sigma=sig)
 colnames(X) <- paste0("x", 1:p)
 b <- c(0,-0.3,0,0,-0.3,0,0,0.3,0.3) #
 names(b) <- paste0("x", 1:p)
 y <- 1 +  X%*%b + rnorm(n)
 Xy <- cbind(as.data.frame.matrix(X), y=y) #=hivif
#Test data
 nTe <- 10^3
 XTe <- MASS::mvrnorm(n=nTe, rep(0, p), Sigma=sig)
 colnames(XTe) <- paste0("x", 1:p)
  yTe <- 1 +  XTe%*%b + rnorm(nTe)
 XyTe <- cbind(as.data.frame.matrix(XTe), y=yTe) #test data
 ans <- lm(y ~ ., data=Xy) #fit training data
 mean((XyTe$y - predict(ans, newdata=XyTe))^2) #MSE on test data
 }
}

\keyword{datasets}

