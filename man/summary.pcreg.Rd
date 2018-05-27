\name{summary.pcreg}
\alias{summary.pcreg}

\title{
Summary Method for Pcreg.
}
\description{
The summary is based on the summary method for S3 class 'lm'.
}
\usage{
\method{summary}{pcreg}(object, ...)
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

\note{
The standard errors and p-values are wrong due to selection bias.
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

