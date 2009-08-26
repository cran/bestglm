\name{CVHTF}
\alias{CVHTF}
\title{ K-fold Cross-Validation }
\description{
K-fold cross-validation. 
}
\usage{
CVHTF(X, y, K = 10, REP = 1, family = gaussian, ...)
}

\arguments{
  \item{X}{ training inputs }
  \item{y}{ training output }
  \item{K}{ size of validation sample }
  \item{REP}{ number of replications }
  \item{family}{glm family}
  \item{\dots}{ optional arguments passed to \code{glm} or \code{lm}}
}

\details{
HTF (2009) describe K-fold cross-validation. 
The observations are partitioned into K non-overlapping subsets of approximately
equal size. Each subset is used as the validation sample while the remaining
K-1 subsets are used as training data. When \eqn{K=n}, 
where n is the number of observations
the algorithm is equivalent to leave-one-out CV.
Normally \eqn{K=10} or \eqn{K=5} are used.
When \eqn{K<n-1}, their are may be many possible partitions and so the results of
K-fold CV may vary somewhat depending on the partitions used.
In our implementation, random partitions are used and we allow for many
replications. Note that in the Shao's delete-d method, random samples are
used to select the valiation data whereas in this method the whole partition
is selected as random. This is acomplished using,
\code{fold <- sample(rep(1:K,length=n))}. 
Then \code{fold} indicates each validation sample in the partition.
}

\value{
Vector of two components comprising the cross-validation MSE and its sd based on the MSE in 
each validation sample.
}

\references{
Hastie, T., Tibshirani, R. and Friedman, J. (2009). 
The Elements of Statistical Learning. 2nd Ed. Springer-Verlag.
 }

\author{A.I. McLeod and C. Xu}

\seealso{ 
\code{\link{bestglm}}, 
\code{\link{CVd}}, 
\code{\link{CVDH}}, 
\code{\link{LOOCV}} 
}

\examples{
#Example 1. Variability in 10-fold CV
#Plot the CVs obtained by using 10-fold CV on the best subset
#model of size 2 for the prostate data. We assume the best model is
#the model with the first two inputs and then we compute the CV's
#using 10-fold CV, 100 times. The result is summarized by a boxplot as well 
#as the sd.
NUMSIM<-100
data(zprostate)
train<-(zprostate[zprostate[,10],])[,-10]
X<-train[,1:2]
y<-train[,9]
cvs<-numeric(NUMSIM)
for (isim in 1:NUMSIM)
    cvs[isim]<-CVHTF(X,y,K=10,REP=1)[1]
boxplot(cvs)
sd(cvs)
#The CV MSE is about 61.0 with sd 0.015
#95% c.i. is (60.7, 61.3)


#Example 2. Figure 3.7, HTF
#Using this seed we get similar results to HTF
set.seed(23301)
data(zprostate)
train<-(zprostate[zprostate[,10],])[,-10]
out<-bestglm(train, IC="CV", CVArgs=list(Method="HTF", K=10, REP=1))
CV<-out$Subsets[,"CV"]
sdCV<-out$Subsets[,"sdCV"]
CVLo<-CV-0.5*sdCV
CVHi<-CV+0.5*sdCV
ymax<-max(CVHi)
ymin<-min(CVLo)
k<-0:(length(CV)-1)
plot(k, CV, xlab="Subset Size", ylab="CV Error", ylim=c(ymin,ymax), type="n", yaxt="n")
points(k, CV, cex=2, col="red", pch=16)
lines(k, CV, col="red", lwd=2)
axis(2, yaxp=c(0.6,1.8,6))
segments(k, CVLo, k, CVHi, col="blue", lwd=2)
eps<-0.15
segments(k-eps, CVLo, k+eps, CVLo, col="blue", lwd=2)
segments(k-eps, CVHi, k+eps, CVHi, col="blue", lwd=2)
#cf. oneSDRule
indMin <- which.min(CV)
fmin<-sdCV[indMin]
cutOff <- fmin/2 + CV[indMin]
indRegion <- cutOff > CV
Offset <- sum(cumprod(as.numeric(!indRegion)))
TheMins <- (cutOff-CV)[indRegion]
indBest<-Offset + (1:length(TheMins))[(min(TheMins)==TheMins)]
abline(h=cutOff, lty=2)
abline(v=indBest-1, lty=2)
}

\keyword{ models }
\keyword{ regression }



