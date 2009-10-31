\name{CVDH}
\alias{CVDH}
\title{ Adjusted K-fold Cross-Validation }
\description{
An adjustment to K-fold cross-validation is made to reduce bias.
}
\usage{
CVDH(X, y, K = 10, REP = 1)
}

\arguments{
  \item{X}{ training inputs }
  \item{y}{ training output }
  \item{K}{ size of validation sample }
  \item{REP}{ number of replications }
}

\details{
Algorithm 6.5 (Davison and Hinkley, p.295) is implemented.
}

\value{
Vector of two components comprising the cross-validation MSE and its sd based on the MSE in 
each validation sample.
}

\references{
Davison, A.C. and Hinkley, D.V. (1997). Bootstrap Methods and their Application. Cambridge University Press.
}

\author{A.I. McLeod and C. Xu}

\seealso{ 
\code{\link{bestglm}}, 
\code{\link{CVHTF}}, 
\code{\link{CVd}}, 
\code{\link{LOOCV}} 
}

\examples{
#Example 1. Variability in 10-fold CV with Davison-Hartigan Algorithm.
#Plot the CVs obtained by using 10-fold CV on the best subset
#model of size 2 for the prostate data. We assume the best model is
#the model with the first two inputs and then we compute the CV's
#using 10-fold CV, 100 times. The result is summarized by a boxplot as well 
#as the sd.
NUMSIM<-10
data(zprostate)
train<-(zprostate[zprostate[,10],])[,-10]
X<-train[,1:2]
y<-train[,9]
cvs<-numeric(NUMSIM)
set.seed(123321123)
for (isim in 1:NUMSIM)
    cvs[isim]<-CVDH(X,y,K=10,REP=1)[1]
summary(cvs)
}

\keyword{ models }
\keyword{ regression }



