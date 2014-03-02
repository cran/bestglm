\name{Fires}
\Rdversion{1.1}
\alias{Fires}
\docType{data}
\title{Forest fires in Montesinho natural park. Standardized inputs.}

\description{
The forest fire data were collected during January 2000 to December 2003 for fires in the 
Montesinho natural park located in the northeast region of Portugal.  
The response variable of interest was area burned in ha.  
When the area burned as less than one-tenth of a hectare, the response variable as set to zero.  
In all there were 517 fires and 247 of them recorded as zero.  
The region was divided into a 10-by-10 grid with coordinates X and Y running from 1 to 9.  
The categorical variable xyarea indicates the region in this grid for the fire.
}

\usage{data(Fires)}
\format{
  A data frame with 517 observations on the following 12 variables.
  All quantitative variables have been standardized.
  \describe{
    \item{\code{xyarea}}{a factor with 36 levels }
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
    \item{\code{lburned}}{log(x+1), x is burned area with x=0 for small fires}
  }
}

\details{
The original data may be found at the website below as well
as an analysis.
The  quantitative variables in this dataset have been standardized.
For convenience, the original data is provided in  \code{\link{MontesinhoFires}}.
}

\source{
\url{http://archive.ics.uci.edu/ml/datasets/Forest+Fires}
}

\seealso{ 
\code{\link{MontesinhoFires}}
}

\references{
P. Cortez and A. Morais, 2007. 
A Data Mining Approach to Predict Forest Fires using Meteorological Data. 
In J. Neves, M. F. Santos and J. Machado Eds., New Trends in Artificial Intelligence, 
Proceedings of the 13th EPIA 2007 - Portuguese Conference on Artificial Intelligence, 
December, Guimaraes, Portugal, pp. 512-523, 2007. 
}

\examples{
data(Fires)
names(Fires)
#ANOVA for xyarea is significant at 1.1%.
summary(aov(lburned~xyarea, data=Fires))
}

\keyword{datasets}
