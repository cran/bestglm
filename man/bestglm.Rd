\name{bestglm}
\alias{bestglm}
\title{Best Subset GLM using Information Criterion or Cross-Validation }
\description{
Best subset selection using 'leaps' algorithm  (Furnival and Wilson, 1974)
or complete enumeration (Morgan and Tatar, 1972).
Complete enumeration is used for the non-Gaussian and for the
case where the input matrix contains factor variables with more than 2 levels.
The best fit may be found using
the information criterion IC: AIC, BIC, EBIC, or BICq.
Alternatively, with IC=`CV' various types of cross-validation may be used.
}
\usage{
bestglm(Xy, family = gaussian, IC = "BIC", t = "default", CVArgs = "default", qLevel = 0.99, TopModels = 5, method = "exhaustive", intercept = TRUE, weights = NULL, nvmax = "default", RequireFullEnumerationQ = FALSE, ...)
}
\arguments{
  \item{Xy}{ Dataframe containing the design matrix X and the output variable y. All columns must be named.}
  \item{family}{One of the glm distribution functions. The glm function is not used
  in the Gaussian case. Instead for efficiency either 'leaps' is used or when factor
  variables are present with more than 2 levels, 'lm' may be used. }
  \item{IC}{ Information criteria to use: "AIC", "BIC", "BICg", "BICq", "LOOCV", "CV".}
  \item{t}{ adjustable parameter for BICg, BICq or CV. For BICg, default is g=t=1. 
  For BICq, default is q=t=0.25.  For CV, default the delete-d method with d=ceil(n(1-1/(log n - 1)))
  and REP=t=100. The default value of the parameter may be changed by changing t.}
  \item{CVArgs}{ Used when IC is set to `CV`.  The default is use the delete-d algorithm
  with d=ceil(n(1-1/(log n - 1))) and t=100 repetitions. Note that the number of repetitions
  can be changed using t. 
  More generally, CVArgs is a list with
  3 named components: Method, K, REP, where Method is one of \"HTF\", \"DH\", \"d\" 
  corresponding to using the functions CVHTM (Hastie et al., 2009, K-fold CV), CVDH (adjusted K-fold
  CV, Davison and Hartigan, 1997) and CVd (delete-d CV with random subsamples, Shao, 1997).
  }
  \item{qLevel}{ the alpha level for determining interval for best q. Larger alpha's result in larger intervals. }
  \item{TopModels}{Finds the best \code{TopModels} models.}
  \item{method}{ Method used in leaps algorithm for searching for the best subset.}
  \item{intercept}{Default TRUE means the intercept term is always included. 
  	If set to FALSE, no intercept term is included.
	If you want only include the intercept term when it is signficant then set IncludeInterceptQ=FALSE and include
	a column of 1's in the design matrix.}
  \item{weights}{weights }
  \item{nvmax}{ maximum number of independent variables allowed. By default, all variables}
  \item{RequireFullEnumerationQ}{Use exhaustive search algorithm instead of 'leaps'}
  \item{\dots}{Optional arguments which are passed to \code{lm} or \code{glm}}
}

\details{
In the Gaussian case, 
the loglikelihood may be written \eqn{logL = -(n/2) log (RSS/n)},
where RSS is the residual sum-of-squares and n is the number of observations.
When the function 'glm' is used, the log-likelihood, logL, is obtained using 'logLik'.
The penalty for EBIC and BICq depends on the tuning parameter argument, \code{t}.
The argument \code{t} also controls the number of replications used when
the delete-d CV is used as default. In this case, the parameter d is chosen
using the formula recommended by Shao (1997).
See   \code{\link{CVd}} for more details.

In the binomial GLM, nonlogistic, case the last two columns of Xy
are the counts of 'success' and 'failures'.

Cross-validation may also be used to select the best subset.
When cross-validation is used, the best models of size k according to the
log-likelihood	are compared for k=0,1,...,p, where p is the number of inputs.
Cross-validation is not available when there are categorical variables since
in this case it is likely that the training sample may not contain all levels
and in this case we can't predict the response in the validation sample.
In the case of GLM, the \"DH\" method for CV is not available. 

Usually it is a good idea to keep the intercept term even if it is not
significant. See discussion in vignette.

Cross-validation is not available for models with no intercept term or
when \code{force.in} is non-null or when \code{nvmax} is set
to less than the full number of independent variables.

Please see the package vignette for more details and examples.

}

\value{
A list with class attribute 'bestglm' and named components:
  \item{BestModel }{An lm-object representing the best fitted regression.}
  \item{Title}{A brief title describing the algorithm used: CV(K=K), CVadj(K=K), CVd(d=K).
  The range of q for an equivalent BICq model is given.}
  \item{Subsets }{The best subsets of size, k=0,1,...,p are indicated as well the value of the
  log-likelihood and information criterion for each best subset. 
  In the case of categorical variables with more than 2 levels, the degrees of freedom
  are also shown.
  }
  \item{qTable}{Table showing range of q for choosing each possible subset size. Assuming
  intercept=TRUE, k=1 corresponds to model with only an intercept term and k=p+1,
  where p is the number of input variables, corresponds to including all variables.}
  \item{Bestq}{Optimal q}
  \item{ModelReport}{A list with components: NullModel, LEAPSQ, glmQ, gaussianQ, NumDF, CategoricalQ, Bestk. }
  \item{BestModels}{Variables in the \code{TopModels} best list}
Methods function 'print.bestglm' and 'summary.bestglm' are provided.
}

\references{ 
Xu, C. and McLeod, A.I. (2009). 
Another Extended Bayesian Information Criteria for Model Selection.

Chen, J. and Chen, Z. (2008). Extended Bayesian 
Information Criteria for Model Selection with Large Model Space. Biometrika 2008 95: 759-771.

Furnival, G.M. and Wilson, R. W. (1974).  
Regressions by Leaps and Bounds.  Technometrics, 16, 499--511.

Morgan, J. A.  and  Tatar, J. F. (1972). 
Calculation of the Residual Sum of Squares for All Possible Regressions. 
Technometrics 14, 317-325.

Miller, A. J. (2002), 
Subset Selection in Regression, 2nd Ed. London, Chapman and Hall.

Shao, Jun (1997). An Asymptotic Theory for Linear Model Selection.  Statistica Sinica 7, 221-264.
	}

\author{ C. Xu and A.I. McLeod}


\seealso{ 
\code{\link{glm}},
\code{\link{lm}},
\code{\link[leaps]{leaps}}
\code{\link{CVHTF}},
\code{\link{CVDH}},
\code{\link{CVd}}
}

\examples{
#Example 1. 
#White noise test.
set.seed(123321123)
p<-25   #number of inputs
n<-100  #number of observations
X<-matrix(rnorm(n*p), ncol=p)
y<-rnorm(n)
Xy<-as.data.frame(cbind(X,y))
names(Xy)<-c(paste("X",1:p,sep=""),"y")
bestAIC <- bestglm(Xy, IC="AIC")
bestBIC <- bestglm(Xy, IC="BIC")
bestEBIC <- bestglm(Xy, IC="BICg")
bestBICq <- bestglm(Xy, IC="BICq")
NAIC <- length(coef(bestAIC$BestModel))-1
NBIC <- length(coef(bestBIC$BestModel))-1
NEBIC <- length(coef(bestEBIC$BestModel))-1
NBICq <- length(coef(bestBICq$BestModel))-1
ans<-c(NAIC, NBIC, NEBIC, NBICq)
names(ans)<-c("AIC", "BIC", "BICg", "BICq")
ans
# AIC  BIC EBIC BICq 
#   3    1    0    0 

#Example 2. bestglm with BICq
#Find best model. Default is BICq with q=0.25
data(znuclear) #standardized data. 
#Rest of examples assume this dataset is loaded.
out<-bestglm(znuclear, IC="BICq")
out
#The optimal range for q
out$Bestq
#The possible models that can be chosen
out$qTable
#The best models for each subset size
out$Subsets
#The overall best models
out$BestModels
#
#Example 3. Normal probability plot, residuals, best model
ans<-bestglm(znuclear, IC="BICq")
e<-resid(ans$BestModel)
qqnorm(e, ylab="residuals, best model")
#
#Example 4. bestglm, using EBIC, g=1
bestglm(znuclear, IC="BICg")
#EBIC with g=0.5
bestglm(znuclear, IC="BICg", t=0.5)
#
#Example 5. bestglm, CV
data(zprostate)
train<-(zprostate[zprostate[,10],])[,-10]
#Default method delete-d with 100 replications - takes about 5 seconds
bestglm(train, IC="CV") 
#Using delete-d with 1000 replications takes about 50 seconds but
# the result has converged -- best regression is with lcavol and lweight.
#Uncomment to run next line:
#bestglm(train, IC="CV", t=1000) 
#Other CV methods:
set.seed(23301)
bestglm(train, IC="CV", CVArgs=list(Method="HTF", K=10, REP=1))
#result is different because of random sampling to choose the folds
bestglm(train, IC="CV", CVArgs=list(Method="HTF", K=10, REP=1))
#less variability when we increase REP to 100 but takes more time
#uncomment to run - takes about 50 seconds
#bestglm(train, IC="CV", CVArgs=list(Method="HTF", K=10, REP=100))
#Compare with DH Algorithm
bestglm(train, IC="CV", CVArgs=list(Method="DH", K=10, REP=10))
#Compare LOOCV
bestglm(train, IC="LOOCV")
#
#Example 6. Optimal q for manpower dataset
data(manpower)
out<-bestglm(manpower)
out$Bestq
#
#Example 7. Factors with more than 2 levels
data(AirQuality)
bestglm(AirQuality)
#
#Example 8. Logistic regression
data(SAheart)
bestglm(SAheart, IC="BIC", family=binomial)
#BIC agrees with backward stepwise approach
out<-glm(chd~., data=SAheart, family=binomial)
step(out, k=log(nrow(SAheart)))
#but BICq with q=0.25
bestglm(SAheart, IC="BICq", t=0.25, family=binomial)
#
#Cross-validation with glm
#make reproducible results
set.seed(33997711)
#takes about 15 seconds and selects 5 variables
bestglm(SAheart, IC="CV", family=binomial)
#about 6 seconds and selects 2 variables
bestglm(SAheart, IC="CV", CVArgs=list(Method="HTF", K=10, REP=1), family=binomial)
#Will produce an error -- NA
\dontrun{bestglm(SAheart, IC="CV", CVArgs=list(Method="DH", K=10, REP=1), family=binomial)}
\dontrun{bestglm(SAheart, IC="LOOCV", family=binomial)}
#
#Example 9. Model with no intercept term
X<-matrix(rnorm(200*3), ncol=3)
b<-c(0, 1.5, 0)
y<-X\%*\%b + rnorm(40)
Xy<-data.frame(as.matrix.data.frame(X), y=y)
bestglm(Xy,IncludeInterceptQ=FALSE) 

}

\keyword{ models }
\keyword{ regression }

