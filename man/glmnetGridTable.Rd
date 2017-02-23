\name{glmnetGridTable}
\alias{glmnetGridTable}

\title{
Multipanel Display and Table Glmnet CV Output.
}
\description{
Four panels.
}
\usage{
glmnetGridTable(XyList, alpha = 0, nfolds=10, family = "gaussian")
}

\arguments{
  \item{XyList}{
input
}
  \item{alpha}{
elastic net parameter
}
  \item{nfolds}{
Number of folds, K, in regularized K-fold CV, must be >3 and <=10.
}
  \item{family}{
distribution
}
}
\details{
tba
}
\value{
plot produced by side-effect.
Table.
}


\author{
A. I. McLeod
}
\note{
Set random seed beforehand if you want reproducibility.
}

\seealso{
\code{\link{trainTestPartition}},
\code{\link[glmnet]{cv.glmnet}},
\code{\link[glmnet]{glmnet}},
\code{\link[glmnet]{predict.glmnet}}

}
\examples{
set.seed(7733551)
out <- trainTestPartition(mcdonald)
round(glmnetGridTable(out),4)
}

\keyword{ models }
\keyword{ regression }
