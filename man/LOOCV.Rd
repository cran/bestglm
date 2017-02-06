\name{LOOCV}
\alias{LOOCV}
\title{ Leave-one-out cross-validation }
\description{
An observation is removed and the model is fit the the remaining data and this 
fit used to predict the value of the deleted observation.
This is repeated, n times, for each of the n observations and the mean square 
error is computed.
}
\usage{
LOOCV(X, y)
}

\arguments{
  \item{X}{ training inputs }
  \item{y}{ training output }
}

\details{
LOOCV for linear regression is exactly equivalent to the PRESS method
suggested by Allen (1971) who also provided an efficient algorithm.
}

\value{
Vector of two components comprising the cross-validation MSE and its sd based 
on the MSE in each validation sample.
}

\references{
Hastie, T., Tibshirani, R. and Friedman, J. (2009). 
The Elements of Statistical Learning. 2nd Ed.

Allen, D.M. (1971). Mean Square Error of Prediction as a Criterion 
for Selecting Variables. Technometrics, 13, 469 -475. 
 }

\author{A.I. McLeod and C. Xu}

\seealso{ 
\code{\link{bestglm}}, 
\code{\link{CVd}}, 
\code{\link{CVDH}}, 
\code{\link{CVHTF}} 
}

\examples{
#Example. Compare LOO CV with K-fold CV.
#Find CV MSE's for LOOCV and compare with K=5, 10, 20, 40, 50, 60
#Takes about 30 sec
\dontrun{
 data(zprostate)
 train<-(zprostate[zprostate[,10],])[,-10]
 X<-train[,1:2]
 y<-train[,9]
 CVLOO<-LOOCV(X,y)
 KS<-c(5,10,20,40,50,60)
 nKS<-length(KS)
 cvs<-numeric(nKS)
 set.seed(1233211231)
 for (iK in 1:nKS)
    cvs[iK]<-CVDH(X,y,K=KS[iK],REP=10)[1]
 boxplot(cvs)
 abline(h=CVLOO, lwd=3, col="red")
 title(sub="Boxplot of CV's with K=5,10,20,40,50,60 and LOO CV in red")
 }
}

\keyword{ models }
\keyword{ regression }


