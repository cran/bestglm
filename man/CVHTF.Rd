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
When \eqn{K<n-1}, their are may be many possible partitions and so the results 
of K-fold CV may vary somewhat depending on the partitions used.
In our implementation, random partitions are used and we allow for many
replications. Note that in the Shao's delete-d method, random samples are
used to select the valiation data whereas in this method the whole partition
is selected as random. This is acomplished using,
\code{fold <- sample(rep(1:K,length=n))}. 
Then \code{fold} indicates each validation sample in the partition.
}

\value{
Vector of two components comprising the cross-validation MSE and its sd based 
on the MSE in each validation sample.
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
#Example 1. 10-fold CV
data(zprostate)
train<-(zprostate[zprostate[,10],])[,-10]
X<-train[,1:2]
y<-train[,9]
CVHTF(X,y,K=10,REP=1)[1]
}

\keyword{ models }
\keyword{ regression }



