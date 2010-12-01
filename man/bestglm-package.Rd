\name{bestglm-package}
\alias{bestglm-package}
\docType{package}
\title{ bestglm: Best Subset GLM}

\description{
Provides new information criterion BICq as well as AIC, BIC and EBIC for selecting the best model. 
Additionally, various CV algorithms are also provided.
}

\details{
\tabular{ll}{
Package: \tab bestglm\cr
Type: \tab Package\cr
Version: \tab 0.31\cr
Date: \tab 2010-12-01\cr
License: \tab GLP 2.0 or greater\cr
LazyData: \tab yes\cr
LazyLoad: \tab yes\cr
}

bestglm is the main function. All other functions are utility functions and are not normally invoked.

Many examples are provided in the vignettes accompanying this package.
The vignettes are produced using the R package \code{Sweave} and so R scripts
can easily be extracted.

The R package \code{xtable} is needed for the vignette in \code{SimExperimentBICq.Rnw}.
}

\author{A.I. McLeod and Changjiang Xu}

\references{
Xu, C. and McLeod, A.I. (2009). 
Bayesian Information Criterion with Bernouilli Prior.

}

\keyword{ package }

\seealso{
\code{\link[leaps]{leaps}}
}

\examples{
data(zprostate)
train<-(zprostate[zprostate[,10],])[,-10]
#Best subset using AIC
bestglm(train, IC="AIC")
#Best subset using BIC
bestglm(train, IC="BIC")
#Best subset using EBIC
bestglm(train, IC="BICg")
#Best subset using BICg with g=0.5 (tuning parameter)
bestglm(train, IC="BICg", t=0.5)
#Best subset using BICq. Note BICq with q=0.25 is default.
bestglm(train, IC="BICq")
#Best subset using BICq with q=0.5 (equivalent to BIC)
bestglm(train, IC="BICq", t=0.5)
#Remark: set seed since CV depends on it
set.seed(123321123)
bestglm(train, IC="CV", t=10)
#using HTF method
bestglm(train, IC="CV", CVArgs=list(Method="HTF", K=10, REP=1))
#Best subset, logistic regression
data(SAheart)
bestglm(SAheart, IC="BIC", family=binomial)
#Best subset, factor variables with more than 2 levels
data(AirQuality)
#subset
bestglm(AirQuality, IC="BICq")

}
