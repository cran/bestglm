\name{glmnetPredict}
\alias{glmnetPredict}

\title{Glmnet Prediction Using CVAV.}

\description{
Predict by averaging the predictions from cv.glmnet().
}
\usage{
glmnetPredict(XyList, NREP = 15, alpha = 0, nfolds=10, 
 family = c("gaussian", "binomial", "poisson", "multinomial"))
}

\arguments{
  \item{XyList}{
list with components XyTr, XTr, yTr, XTe.
}
  \item{NREP}{
number of replications to use in average
}
  \item{alpha}{
elastic net parameter
}
  \item{nfolds}{
Number of folds, K, in regularized K-fold CV, must be >3 and <=10.
}
  \item{family}{
model
}
}

\value{
vector with predictions
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{trainTestPartition}},
\code{\link{glmnetGridTable}},
\code{\link[glmnet]{glmnet}},
\code{\link[glmnet]{cv.glmnet}},
\code{\link[glmnet]{predict.glmnet}}
}
\examples{
set.seed(7733551)
out <- trainTestPartition(mcdonald)
round(glmnetGridTable(out),4)
yh <- glmnetPredict(out, NREP=5)
sqrt(mean((out$yTe - yh)^2))
}

\keyword{ models }
\keyword{ regression }
