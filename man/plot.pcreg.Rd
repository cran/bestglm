\name{plot.pcreg}
\alias{plot.pcreg}

\title{
Diagnostic plots for PCR and PLS
}
\description{
Diagnostic plots available with lm-objects are provided.
}
\usage{
\method{plot}{pcreg}(x, ...)
}

\arguments{
  \item{x}{
 \code{x} output from pcreg(). It has S3 class 'pcreg'.
}
  \item{\dots}{
additional parameters
}
}
\details{
See plot method for S3 class 'lm'.
}
\value{
Nothing. The plot is produced.
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{pcreg}},
\code{\link{fitted.pcreg}},
\code{\link{residuals.pcreg}}
}

\examples{
ans <- pcreg(mcdonald, scale=TRUE)
plot(ans)
}

\keyword{ models }
\keyword{ regression }


