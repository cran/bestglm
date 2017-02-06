\name{znuclear}
\alias{znuclear}
\docType{data}
\title{ Nuclear plant data. Quantitative inputs logged and standardized.}

\description{
Data on 32 nuclear power plants.
The response variable is cost and there are ten covariates.
}

\usage{data(znuclear)}

\format{
  A data frame with 32 observations on the following 12 variables. 
  All quantitative variables, except date, have been logged and standardized 
  to have mean 0 and variance 1.
  \describe{
    \item{\code{date}}{Quantitative covariate. The date on which the 
    construction permit was issued. The data are measured in years since 
    January 1 1990 to the nearest month.}
    \item{\code{T1}}{Quantitative covariate. The time between application for 
    and issue of the construction permit.}
    \item{\code{T2}}{Quantitative covariate. The time between issue of 
    operating license and construction permit.}
    \item{\code{capacity}}{Quantitative covariate. The net capacity of the 
    power plant (MWe).}
    \item{\code{PR}}{Binary covariate. Value 1, indicates the prior existence 
    of a LWR plant at the same site.}
    \item{\code{NE}}{Binary covariate, located in North-East USA}
    \item{\code{CT}}{Binary covariate, presence of cooling tower}
    \item{\code{BW}}{Binary covariate, where 1 indicates that the nuclear 
    steam supply system was manufactured by Babcock-Wilcox.}
    \item{\code{N}}{Quantitative covariate. The cumulative number of power 
    plants constructed by each architect-engineer.}
    \item{\code{PT}}{Binary covariate, partial turnkey guarantee.}
    \item{\code{cost}}{Outcome. The capital cost of construction in millions 
    of dollars adjusted to 1976 base. 
}
  }
}

\details{
Davison (2003) explores fitting models to this data using forward
and backward stepwise regression. In this modelling logs of quantiative 
variablesare used. We have also standardized this data to facilitate comparison 
with other techniques such as LARS and principal component regression.

Davison and Hinkley (1997, Example 6.8, 6.10, 6.12) use this data in
a series of examples.
Example 6.8: estimation of prediction error.
Example 6.10: prediction error using cross-validation and bootstrapping.
Example 6.12: subset model selection using cross-validation.
}

\source{
Obtained from the CRAN package boot.
}

\references{
Davison, A. C. (2003). Statistical Models. Cambridge: Cambridge University Press. 

Davison, A.C. and Hinkley, D.V. (1997). Bootstrap Methods and their Application.
Cambridge University Press.
}

\examples{
data(znuclear)
bestglm(znuclear, IC="BICq")
}
\keyword{datasets}
