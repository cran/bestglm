\name{summary.bestglm}
\alias{summary.bestglm}
\title{ summary of  `bestglm` object }

\description{
An analysis of deviance and a likelihood-ratio test
with p-value. 
The p-value is greatly exagerated due to selection.
}

\usage{
\method{summary}{bestglm}(object, SubsetsQ=FALSE, ...)
}

\arguments{
  \item{object}{ Output from the bestglm function }
  \item{SubsetsQ}{ List best subsets of each size}
  \item{\dots}{ optional arguments }
}


\value{
No value. Output to terminal only.
}

\author{A.I. McLeod and C. Xu}

\seealso{ 
\code{\link{bestglm}},
\code{\link{print.bestglm}}
}

\examples{
data(znuclear)
summary(bestglm(znuclear))
#
#find statistical signficance of overall regression
data(Fires)
summary(bestglm(Fires, IC="BICq", t=1))
}

\keyword{ models }
\keyword{ regression }



