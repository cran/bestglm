\name{oneSdRule}
\alias{oneSdRule}
\title{ Utility function. Implements the 1-sd rule. }
\description{
The CV and its standard devation are provided for a range of models
ordered by the number of parameters estimated.
}

\usage{
oneSdRule(CVout)
}

\arguments{
  \item{CVout}{ A matrix with two columns. First column is the CV and second, its sd.
  Row ordering is from fewest parameter to most. }
}

\value{
The row corresponding to the best model.
}

\references{ 
Hastie, T., Tibshirani, R. and Friedman, J. (2009). 
The Elements of Statistical Learning. 2nd Ed. Springer-Verlag.
}

\author{A.I. McLeod and C. Xu}

\examples{
CV<-c(1.4637799,0.7036285,0.6242480,0.6069406,0.6006877,0.6005472,0.5707958,
      0.5907897,0.5895489)
CVsd<-c(0.7867428,0.4477943,0.2755896,0.3597420,0.2694985,0.3762265,
     0.2517451,0.2920380,0.4067000)
CVout <- matrix(c(CV,CVsd), ncol=2)
oneSdRule(CVout)
}

