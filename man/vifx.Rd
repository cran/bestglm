\name{vifx}
\alias{vifx}

\title{
Variance Inflation Factor for a Design Matrix
}
\description{
Barplot of the VIF is produced
}
\usage{
vifx(X)
}
\arguments{
  \item{X}{
A design matrix
}
}
\details{
The VIF are the diagonal elements in the inverse 
\eqn{t(X*) X*}, where X* is the rescaled design matrix.
}
\value{
vector with VIF's
}
\references{
Marquardt, D. W. (1970). 
Generalized Inverses, Ridge Regression, Biased Linear Estimation, and Nonlinear 
Estimation. Technometrics 12(3), 591-612. 
}
\author{
A. I. McLeod
}

\examples{
data(mcdonald)
vifx(mcdonald[, -ncol(mcdonald)])
}

\keyword{ ts }
