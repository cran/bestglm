\name{print.pcreg}
\alias{print.pcreg}
\title{Print method for `pcreg` object }
\description{A brief description of the best fit is given.}
\usage{
\method{print}{pcreg}(x, ...)
}

\arguments{
  \item{x}{ Output from the pcreg function }
  \item{\dots}{ optional arguments }
}

\value{
No value. Output to terminal only.
}

\author{A.I. McLeod and C. Xu}

\seealso{ 
\code{\link{pcreg}},
\code{\link{summary.pcreg}}
}

\examples{
pcreg(znuclear, scale=TRUE)
}

\keyword{ models }
\keyword{ regression }


