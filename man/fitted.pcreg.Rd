\name{fitted.pcreg}
\alias{fitted.pcreg}

\title{
Fitted values in PCR and PLS.
}
\description{
The fitted values are returned given the output from \code{pcreg}.
}
\usage{
\method{fitted}{pcreg}(object, ...)
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
\code{\link{residuals.pcreg}},
\code{\link{plot.pcreg}}
}

\examples{
fitted(pcreg(mcdonald, scale=TRUE))
}

\keyword{ models }
\keyword{ regression }


