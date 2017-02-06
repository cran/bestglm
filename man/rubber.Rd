\name{rubber}
\alias{rubber}
\docType{data}
\title{
Abrasion loss for various hardness and tensile strength
}
\description{
The data come from an experiment to investigate how the resistance of rubber 
to abrasion is affected by the hardness of the rubber and its tensile strength.
}
\usage{data(rubber)}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{hardness}}{hardness in degree Shore}
    \item{\code{tensile.strength}}{tensile strength in kg per square meter}
    \item{\code{abrasion.loss}}{abrasion loss in gram per hour}
    \item{\code{ts.low}}{tensile strength minus the breakpoint 180 km/m^2}
    \item{\code{ts.high}}{tensile strength minus the breakpoint 180 km/m^2}
  }
}
\source{
Hand, D.J., Daly, F., Lunn, A.D., McConway, K.J. and Ostrowski, E. (1993). 
A Handbook of Small Datasets. Chapman and Hall.	
}
\references{
Cleveland, W. S. (1993). Visualizing data. Hobart Press, Summit: New Jersey. 

Davies, O.L. and Goldsmith, P.L.(1972) 
Statistical methods in Research and Production.
}
\examples{
data(rubber)
ans <- lm(abrasion.loss~hardness+tensile.strength, data=rubber)
}
\keyword{datasets}
