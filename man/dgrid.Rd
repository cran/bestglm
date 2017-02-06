\name{dgrid}
\alias{dgrid}
\title{
Dependency Plots of Output vs Each Input
}
\description{
A lattice grid plot is produced for the output vs. each input.
}
\usage{
dgrid(XyDF, span=0.8)
}

\arguments{
  \item{XyDF}{
Must be a dataframe with the last column corresponding to the output
}
  \item{span}{
smoothing parameter for loess
}
}

\value{
a lattice plot
}
\author{
A. I. McLeod
}

\seealso{
\code{\link{pairs}},
\code{\link[lattice]{splom}},
}
\examples{
data(mcdonald)
dgrid(mcdonald)
}

\keyword{ ts }
