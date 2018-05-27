\name{plot1SDRule}
\alias{plot1SDRule}

\title{
Plot Regularization Path and One Standard Deviation Rule
}
\description{
Takes input either matrix with 2 columns or output from caret::train()
and produces a plot showing the best model selected using the 1 SD rule.
}
\usage{
plot1SDRule(ans, main = "", sub = "", xlab = "df", ylab = "EPE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ans}{
matrix or output from train
}
  \item{main}{
optional plot title
}
  \item{sub}{
optional plot subtitle
}
  \item{xlab}{
optional x-axis label
}
  \item{ylab}{
optional y-axis label
}

}

\value{
tuning parameter value for best model
}
\references{
Hastie, Tibsharani and Friedman, "Elements of Statistical Learning".
}
\author{
A. I. McLeod
}

\seealso{
\code{\link{oneSDRule}}
}

\examples{
CV<-c(1.4637799,0.7036285,0.6242480,0.6069406,0.6006877,0.6005472,0.5707958,
      0.5907897,0.5895489)
CVsd<-c(0.24878992,0.14160499,0.08714908,0.11376041,0.08522291,
 0.11897327,0.07960879,0.09235052,0.12860983)
CVout <- matrix(c(CV,CVsd), ncol=2)
oneSDRule(CVout)
}

\keyword{models}
