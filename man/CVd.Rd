\name{CVd}
\alias{CVd}
\title{ Cross-validation using delete-d method.}
\description{
The delete-d method for cross-validation uses a random sample of d observations 
as the validation sample.  This is repeated many times.
}
\usage{
CVd(X, y, d = ceiling(n * (1 - 1/(log(n) - 1))), REP = 100, family = gaussian, ...)
}
\arguments{
  \item{X}{ training inputs }
  \item{y}{ training output }
  \item{d}{ size of validation sample }
  \item{REP}{ number of replications }
  \item{family}{glm family}
  \item{\dots}{ optional arguments passed to \code{glm} or \code{lm}}
}

\details{
Shao (1993, 1997) suggested the delete-d algorithm implemented in this function. 
In this algorithm, a random sample of d observations are taken as the validation
sample.
This random sampling is repeated \code{REP} times.
Shao (1997, p.234, eqn. 4.5 and p.236) suggests \eqn{d= n(1-1/(log n - 1))},
This is obtained by taking \eqn{\lambda_n = log n} on page 236 (Shao, 1997).
As shown in the table Shao's recommended choice of the d parameter corresponds
to validation samples that are typically much larger that used in 10-fold or 
5-fold
cross-validation.  LOOCV corresponds to d=1 only!
\tabular{llll}{
   n  \tab  d   \tab  K=10   \tab  K=5   	\cr
   50 \tab  33  \tab  5      \tab  10    	\cr
  100 \tab  73  \tab  10     \tab  20   	\cr
  200 \tab 154  \tab  20     \tab  40   	\cr
  500 \tab 405  \tab  50     \tab  100   	\cr
 1000 \tab 831  \tab  100    \tab  200  	\cr
       }
}

\value{
Vector of two components comprising the cross-validation MSE and its sd based 
on the MSE in each validation sample.
}

\references{
Shao, Jun (1993). Linear Model Selection by Cross-Validation. 
Journal of the American Statistical Assocation 88, 486-494.

Shao, Jun (1997). An Asymptotic Theory for Linear Model Selection.  
Statistica Sinica 7, 221-264.
 }

\author{A.I. McLeod and C. Xu}

\seealso{ 
\code{\link{bestglm}}, 
\code{\link{CVHTF}}, 
\code{\link{CVDH}}, 
\code{\link{LOOCV}} 
}

\examples{
#Example 1. delete-d method
#For the training set, n=67. So 10-fold CV is like using delete-d
#with d=7, approximately.
data(zprostate)
train<-(zprostate[zprostate[,10],])[,-10]
X<-train[,1:2]
y<-train[,9]
set.seed(123321123)
CVd(X, y, d=7, REP=10)
#should set to 1000. Used 10 to save time in example.
}

\keyword{ models }
\keyword{ regression }


