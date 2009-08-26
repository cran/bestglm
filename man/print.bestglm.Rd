\name{print.bestglm}
\alias{print.bestglm}
\title{Print method for `bestglm` object }
\description{A brief description of the best fit is given.}
\usage{
\method{print}{bestglm}(x, ...)
}

\arguments{
  \item{x}{ Output from the bestglm function }
  \item{\dots}{ optional arguments }
}

\value{
No value. Output to terminal only.
}

\author{A.I. McLeod and C. Xu}

\seealso{ 
\code{\link{bestglm}},
\code{\link{summary.bestglm}}
}

\examples{
data(znuclear)
bestglm(znuclear)
}

\keyword{ models }
\keyword{ regression }


