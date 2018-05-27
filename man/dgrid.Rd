\name{dgrid}
\alias{dgrid}
\title{
Scaled Variables Dependency Plots: Output vs Inputs
}
\description{
A lattice grid plot is produced for the output vs. each input.
The variables are scaled to have mean zero and variance one.
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
