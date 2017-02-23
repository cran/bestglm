\name{grpregPredict}
\alias{grpregPredict}

\title{
Predictions on Test Data with Grpreg
}
\description{
A dataframe is partitioned randomly into training and test samples.
The function grpreg::grpreg() is used to fit the training data
using Lasso, SCAD and MCP penalty functions. The BIC criterion is used
to selecting the penalty parameter lambda.
}
\usage{
grpregPredict(Xy, trainFrac = 2/3, XyList=NULL)
}

\arguments{
  \item{Xy}{
a dataframe that may contain factor variables
}
  \item{trainFrac}{
the fraction of data to be used for training
}
  \item{XyList}{
instead of supplying Xy you can provide XyList.
}
}

\value{
vector of RMSEs
}


\seealso{
\code{\link{glmnetPredict}},
\code{\link{glmnetGridTable}},
\code{\link{trainTestPartition}},
\code{\link[grpreg]{grpreg}}

}
\examples{
grpregPredict(mcdonald)
}

\keyword{ models }
\keyword{ regression }