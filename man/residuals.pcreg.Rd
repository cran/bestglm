\name{residuals.pcreg}
\alias{residuals.pcreg}
\alias{resid.pcreg}

\title{
Residuals Fitted PCR or PLS  
}
\description{
The residuals from a model fitted using pcreg are returned.
}
\usage{
\method{residuals}{pcreg}(object, ...)
}

\arguments{
  \item{object}{
 \code{object} output
}
  \item{\dots}{
additional parameters
}
}
\details{
Method function for pcreg.
}
\value{residuals}

\author{
A. I. McLeod
}

\seealso{
\code{\link{pcreg}},
\code{\link{fitted}},
\code{\link{plot}}
}

\examples{
resid(pcreg(mcdonald, scale=TRUE))
}

\keyword{ models }
\keyword{ regression }

