\name{predict.pcreg}
\alias{predict.pcreg}

\title{
Predict Method for Pcreg.
}
\description{
Prediction for models fit using \code{pcreg()}.
}
\usage{
\method{predict}{pcreg}(object, newdata, ...)
}

\arguments{
  \item{object}{
the S3 class object produced as output from the function pcreg()
}
  \item{newdata}{
dataframe with new data and with same column names as used in the original 
argument to pcreg.
}
  \item{\dots}{
additional arguments
}
}
\details{
The prediction method, \code{predict.mvr()}, which is available in the pls 
package is used.
We take advantage of this since it avoids fussing with scaling issues 
since it is automatically handled
for us by \code{predict.mvr()}
}
\value{
the predicted values
}
\author{
A. I. McLeod
}

\seealso{
\code{\link{predict.pcreg}},
\code{\link{summary.pcreg}},
\code{\link{plot.pcreg}},
\code{\link{fitted.pcreg}},
\code{\link{residuals.pcreg}}
}

\examples{
XyList <- trainTestPartition(mcdonald)
XyTr <- XyList$XyTr
XyTe <- XyList$XyTe
ans <- pcreg(XyTr, scale=TRUE)
predict(ans, newdata=XyTe)
}

\keyword{ models }
\keyword{ regression }


