\name{NNPredict}
\alias{NNPredict}

\title{
Nearest Neighbour Regression Prediction
}
\description{
Given training/test data in the predictions on the test data computed.
L1, L2 and correlation distances may be used.
The data is sphered prior to making the NN predictions.
}
\usage{
NNPredict(XyList, dist = c("L2", "COR", "L1"))
}

\arguments{
  \item{XyList}{
list with six elements
}
  \item{dist}{
distance used
}
}

\value{
vector of predictions
}
\author{
A. I. McLeod
}


\seealso{
\code{\link{sphereX}}
}
\examples{
AQ <- airquality[complete.cases(airquality),c(2,3,4,1)]
XyList <- trainTestPartition(AQ)
NNPredict(XyList) 
}

\keyword{ prediction }
