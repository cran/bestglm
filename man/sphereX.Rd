\name{sphereX}
\alias{sphereX}

\title{
Sphere Data Matrix
}
\description{
The data matrix is scaled and sphered so it is orthonormal.
The Cholesky decomposition is used.
}
\usage{
sphereX(X)
}

\arguments{
  \item{X}{
 \code{X} rectangular data matrix
}
}
\value{
sphered matrix
}
\author{
A. I. McLeod
}


\seealso{
 \code{\link{scale}}, 
 \code{\link{NNPredict}}
}
\examples{
data(longley)
longley.x <- data.matrix(longley[, 1:6])
sphereX(longley.x)
}

\keyword{ matrix }
