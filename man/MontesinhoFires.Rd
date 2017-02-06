\name{MontesinhoFires}
\Rdversion{1.1}
\alias{MontesinhoFires}
\docType{data}
\title{Forest fires in Montesinho natural park}
\description{
The forest fire data were collected during January 2000 to December 2003 
for fires in the 
Montesinho natural park located in the northeast region of Portugal.  
The response variable of interest was area burned in ha.  
When the area burned as less than one-tenth of a hectare, the response variable 
as set to zero.  
In all there were 517 fires and 247 of them recorded as zero.  
The region was divided into a 10-by-10 grid with coordinates X and Y 
running from 1 to 9.  
}

\usage{data(MontesinhoFires)}
\format{
  A data frame with 517 observations on the following 13 variables.
  \describe{
    \item{\code{X}}{X coordinate for region, 0-10}
    \item{\code{Y}}{X coordinate for region, 0-10}
    \item{\code{month}}{an ordered factor with 12 levels}
    \item{\code{day}}{an ordered factor with 7 levels }
    \item{\code{FFMC}}{fine fuel moisture code}
    \item{\code{DMC}}{Duff moisture code}
    \item{\code{DC}}{drought code}
    \item{\code{ISI}}{initial spread index}
    \item{\code{temp}}{average ambient temperature}
    \item{\code{RH}}{a numeric vector}
    \item{\code{wind}}{wind speed}
    \item{\code{rain}}{rainfall}
    \item{\code{burned}}{area burned in hectares}
  }
}

\details{
This is the original data taken from the website below.
}

\source{
\url{http://archive.ics.uci.edu/ml/datasets/Forest+Fires}
}

\seealso{ 
\code{\link{Fires}}
}

\references{
P. Cortez and A. Morais, 2007. 
A Data Mining Approach to Predict Forest Fires using Meteorological Data. 
In J. Neves, M. F. Santos and J. Machado Eds., 
New Trends in Artificial Intelligence, 
Proceedings of the 13th EPIA 2007 - 
Portuguese Conference on Artificial Intelligence, 
December, Guimaraes, Portugal, pp. 512-523, 2007. 
}

\examples{
data(MontesinhoFires)
names(MontesinhoFires)
data(Fires)
names(Fires)
#Anova for month
summary(aov(burned~month, data=MontesinhoFires))
}

\keyword{datasets}


